## -------------------------------------------------------
##
## File: strategies.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------


## -----------
## helper function replace pipe at 'idx'
## in the inventory with a new one
replace.pipe <- function(idx, inv, time){

  ## get new ID
  if(nrow(inv)>0){
    id <- max(inv$ID) + 1
  } else {
    id <- 1
  }

  ## retire old pipe
  inv$time.end.of.service[idx] <- time  

  ## add new pipe
  new.pipe <- data.frame(ID=id,
                         time.construction=time,
                         replacement.value=inv$replacement.value[idx],
                         diameter=inv$diameter[idx],
                         n.failure=0,
                         time.last.failure=NA,
                         time.end.of.service=NA,
                         in.service=TRUE)

  inv <- rbind(inv, new.pipe)
  class(inv) <- c("data.frame", "inventory")
  inv
}


## -----------
## helper function replace all pipes at index 'idx'
## if the budget allows for it
replace.pipes <- function(state, idx){
  inv <- state$inventory
  budget <- state$budget
  time <- state$time
  
  for(i in idx) {
    if(budget < inv$replacement.value[i]){
      break
    } else {
      budget <- budget - inv$replacement.value[i] # pay for new pipe
      inv <- replace.pipe(i, inv, time)           # get new pipe
    }
  }
  return(list(inventory=inv, budget=budget, time=time))
}



##' Dummy strategy to model no rehabilitation at all.
##' 
##' @title Rehabilitation strategy: no rehabilitation
##' @param state a state list
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate():
##' mystrategy <- . %>% do.nothing
##'
##' @export
do.nothing <- function(state){
  return(state)
}



##' Strategy to replace pipes over a given age. Pipes are only
##' replaced if the budget remains positive.
##'
##' .. content for \details{} ..
##' @title Rehabilitation strategy: replace pipes over \code{max.age}
##' @param state a state list
##' @param max.age pipes older than max.age are replaced
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate():
##' mystrategy <- . %>% replace.older.than(max.age=85)
##'
##' ## or combine multiple strategies to define more complex strategy:
##' mystrategy <- . %>%
##' replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##' 
##' @export
replace.older.than <- function(state, max.age){
  inv <- state$inventory
  
  ## find the index of the pipes older than max.age that are *in use* 
  age <- state$time - inv$time.construction
  idx <- which(age > max.age & inv$in.service)

  ## build new pipes and update budget
  state <- replace.pipes(state, idx)
  
  return(state)
}



##' Replace pipes with a high number of failures. Pipes are only
##' replaced if the budget remains positive.
##'
##' @title Rehabilitation strategy: replace pipes with too many failures
##' @param state a state list
##' @param max.failures maximal allowed number of failures
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate():
##' mystrategy <- . %>% replace.more.failures.than(max.failure=3)
##'
##' ## or combine multiple strategies to define more complex strategy:
##' mystrategy <- . %>%
##' replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##' 
##' @export
replace.more.failures.than <- function(state, max.failures){
  inv <- state$inventory

  ## find the index of the pipes older >max.age that are *in use* 
  idx <- which(inv$n.failure > max.failures & inv$in.service)

  ## build new pipes and update budget
  state <- replace.pipes(state, idx)
  
  return(state)
}



##' Prioritize old pipes for replacement. Pipes are only
##' replaced if the budget remains positive.
##'
##' @title Rehabilitation strategy: replace the n oldest pipes
##' @param state a state list
##' @param n.max maximal number of pipes to replace
##' @return a state list
##' @author Andreas Scheidegger
##' 
##' @examples
##' ## define a strategy function that can be passed to simulate():
##' mystrategy <- . %>% replace.n.oldest(n=10)
##'
##' ## or combine multiple strategies to define more complex strategy:
##' mystrategy <- . %>%
##' replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##' 
##' @export
replace.n.oldest <- function(state, n){
  inv <- state$inventory
  
  ## find the index of the n oldest pipes *in use*
  n.in.service <- sum(inv$in.service)
  idx <- order(inv$time.construction + as.numeric(!inv$in.service)*1E10)[1:(min(n, n.in.service))]

  ## build new pipes and update budget
  state <- replace.pipes(state, idx)
  
  return(state)
}


##' Replace a certain number of randomly chosen pipes. Pipes are only
##' replaced if the budget remains positive.
##'
##' @title Rehabilitation strategy: replace \code{n} randomly selected pipes
##' @param state a state list
##' @param n maximal number of pipes to replace
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate():
##' mystrategy <- . %>% replace.n.random(n=10)
##'
##' ## or combine multiple strategies to define more complex strategy:
##' mystrategy <- . %>%
##' replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##'
##' @export
replace.n.random <- function(state, n){
  inv <- state$inventory
  
  ## sample randomly the index of pipes that are *in use* 
  idx <- sample(which(inv$in.service), min(sum(inv$in.service), n))

  ## build new pipes and update budget
  state <- replace.pipes(state, idx)
  
  return(state)
}

##' Strategy to prioritize pipes with high risk. Pipes are only
##' replaced if the budget remains positive.
##'
##' The Risk is defined as the product of the probability to fail next year
##' and the expected failure costs.
##' 
##' @title Rehabilitation strategy: replace the \code{n} pipes with the highest risk
##' @param state a state list
##' @param n maximal number of pipes to replace
##' @param failure.rate failur rate function. Typically the same as passed to \code{\link(simulate)}.
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate():
##' mystrategy <- . %>% replace.n.highest.risk(n=2, failure.rate=f.rate)
##'
##' ## or combine multiple strategies to define more complex strategy:
##' mystrategy <- . %>%
##' replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##'
##' @export
replace.n.highest.risk <- function(state, n, failure.rate){
  inv <- state$inventory

  ## calculate risk
  risk <- rep(NA, nrow(inv))
  for(i in which(inv$in.service)){
    
    Prob.fail <- failure.rate(age=time-inventory$time.construction[i],
                              inventory$time.last.failure[i],
                              inventory$n.failure[i])
    
    expected.failure.cost <- failure.cost(inv$diameter[i], mean=TRUE)
    
    risk[i] <- Prob.fail * expected.failure.cost
  } 

  idx <- order(risk, decreasing=TRUE)[1:min(n,sum(inv$in.service))]
 
  ## build new pipes and update budget
  state <- replace.pipes(state, idx)
  
  return(state)
}
