## -------------------------------------------------------
##
## File: strategies.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------


## -----------
## helper function to replace pipe at 'idx'
## in the inventory with a new one
replace.pipe <- function(idx, inv, time){

  ## get new pipe ID
  if(nrow(inv)>0){
    id <- max(inv$ID) + 1
  } else {
    id <- 1
  }

  ## retire old pipe
  inv$time.end.of.service[idx] <- time
  inv$in.service[idx] <- FALSE

  ## add new pipe to inventory
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
## helper function to replace all pipes at index 'idx'
## if the budget allows for it
replace.pipes <- function(state, idx, max.costs){
  inv <- state$inventory
  time <- state$time
  budget.intern <- min(state$budget, max.costs) # money that can be spent

  for(i in idx) {
    if(budget.intern < inv$replacement.value[i]){
      break
    } else {
      budget.intern <- budget.intern - inv$replacement.value[i] # pay for new pipe
      inv <- replace.pipe(i, inv, time)                         # get new pipe
    }
  }

  ## calculate remaining money
  new.budget <- state$budget - min(state$budget, max.costs) + budget.intern
  return(list(inventory=inv, budget=new.budget, time=time))
}



##' Dummy strategy to model no rehabilitation at all.
##' 
##' @title Rehabilitation strategy: no pipe replacement, repairs only
##' @param state a state list
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate_network():
##' mystrategy <- . %>% do.nothing
##'
##' @seealso  \code{\link{replace.n.highest.risk}},
##' \code{\link{replace.n.oldest}}, \code{\link{replace.n.random}}, \code{\link{replace.older.than}},
##' \code{\link{replace.more.failures.than}}
##' @export
do.nothing <- function(state){
  return(state)
}



##' Strategy to replace pipes older than a given age. Pipes are only
##' replaced if the budget remains positive.
##'
##' @title Rehabilitation strategy: replace pipes older than \code{max.age}
##' @param state a state list
##' @param max.age pipes older than max.age are replaced
##' @param max.costs maximal amount of money allowed to be spent on this strategy
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate_network():
##' mystrategy <- . %>% replace.older.than(max.age=85, max.costs=20000)
##'
##' ## or define a more complex strategy by combining multiple strategies
##' ## into a prioritized sequence:
##' mystrategy <- . %>%
##'   replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##'
##'
##' @seealso  \code{\link{replace.n.highest.risk}},
##' \code{\link{replace.n.oldest}}, \code{\link{replace.n.random}},
##' \code{\link{replace.more.failures.than}}, \code{\link{do.nothing}}
##' @export
replace.older.than <- function(state, max.age, max.costs=Inf){
  inv <- state$inventory
  
  ## find the index of the pipes older than max.age and that are in use
  age <- state$time - inv$time.construction
  idx <- which((age > max.age) & inv$in.service)

  ## build new pipes and update budget
  state <- replace.pipes(state, idx, max.costs)
  
  return(state)
}



##' Replace pipes with a high number of failures. Pipes are only
##' replaced if the budget remains positive.
##'
##' @title Rehabilitation strategy: replace pipes with too many failures
##' @param state a state list
##' @param max.failures maximal allowed number of failures
##' @param max.costs maximal amount of money allowed to be spent on this strategy
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate_network():
##' mystrategy <- . %>% replace.more.failures.than(max.failure=3, max.costs=20000)
##'
##' ## or define a more complex strategy by combining multiple strategies
##' ## into a prioritized sequence:
##' mystrategy <- . %>%
##'   replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##'
##'
##' @seealso  \code{\link{replace.n.highest.risk}},
##' \code{\link{replace.n.oldest}}, \code{\link{replace.n.random}}, \code{\link{replace.older.than}},
##' \code{\link{do.nothing}}
##' @export
replace.more.failures.than <- function(state, max.failures, max.costs=Inf){
  inv <- state$inventory

  ## find the index of the pipes with more failures than max.failures and that are in use
  idx <- which(inv$n.failure > max.failures & inv$in.service)

  ## build new pipes and update budget
  state <- replace.pipes(state, idx, max.costs)
  
  return(state)
}



##' Prioritize the oldest pipes for replacement. Pipes are only
##' replaced if the budget remains positive.
##'
##' @title Rehabilitation strategy: replace the \code{n} oldest pipes
##' @param state a state list
##' @param n number of oldest pipes to replace
##' @param max.costs maximal amount of money allowed to be spent on this strategy
##' @return a state list
##' @author Andreas Scheidegger
##' 
##' @examples
##' ## define a strategy function that can be passed to simulate_network():
##' mystrategy <- . %>% replace.n.oldest(n=10)
##'
##' ## or define a more complex strategy by combining multiple strategies
##' ## into a prioritized sequence:
##' mystrategy <- . %>%
##'   replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##'
##' @seealso  \code{\link{replace.n.highest.risk}},
##' \code{\link{replace.n.random}}, \code{\link{replace.older.than}},
##' \code{\link{replace.more.failures.than}}, \code{\link{do.nothing}}
##' @export
replace.n.oldest <- function(state, n, max.costs=Inf){
  inv <- state$inventory
  
  ## find the index of the n oldest pipes that are in use
  n.in.service <- sum(inv$in.service)
  idx <- order(inv$time.construction + as.numeric(!inv$in.service)*1E10)[1:(min(n, n.in.service))]

  ## build new pipes and update budget
  state <- replace.pipes(state, idx, max.costs)
  
  return(state)
}


##' Replace a certain number of randomly chosen pipes. Pipes are only
##' replaced if the budget remains positive.
##'
##' @title Rehabilitation strategy: replace \code{n} randomly selected pipes
##' @param state a state list
##' @param n number of random pipes to replace
##' @param max.costs maximal amount of money allowed to be spent on this strategy
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate_network():
##' mystrategy <- . %>% replace.n.random(n=10)
##'
##' ## or define a more complex strategy by combining multiple strategies
##' ## into a prioritized sequence:
##' mystrategy <- . %>%
##'   replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##'
##' @seealso  \code{\link{replace.n.highest.risk}},
##' \code{\link{replace.n.oldest}}, \code{\link{replace.older.than}},
##' \code{\link{replace.more.failures.than}}, \code{\link{do.nothing}}
##' @export
replace.n.random <- function(state, n, max.costs=Inf){
  inv <- state$inventory
  
  ## randomly sample the index of pipes (from those that are in use)
  idx <- sample(which(inv$in.service), min(sum(inv$in.service), n))

  ## build new pipes and update budget
  state <- replace.pipes(state, idx, max.costs)
  
  return(state)
}

##' Strategy to prioritize pipes with the highest risk. Pipes are only
##' replaced if the budget remains positive.
##'
##' The risk is defined as the product of the failure probability in the next year
##' and the expected failure costs.
##' 
##' @title Rehabilitation strategy: replace the \code{n} pipes with the highest risk
##' @param state a state list
##' @param n number of highest risk pipes to replace
##' @param failure.rate failure rate function. Typically the same as passed to \code{\link{simulate_network}}.
##' @param max.costs maximal amount of money allowed to be spent on this strategy
##' @return a state list
##' @author Andreas Scheidegger
##'
##' @examples
##' ## define a strategy function that can be passed to simulate_network():
##' mystrategy <- . %>% replace.n.highest.risk(n=2, failure.rate=f.rate, max.costs=30000)
##'
##' ## or define a more complex strategy by combining multiple strategies
##' ## into a prioritized sequence:
##' mystrategy <- . %>%
##'   replace.more.failures.than(max.failures=2) %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.older.than(max.age=8) %>%
##'   replace.n.random(n=4)
##'
##' @seealso \code{\link{replace.n.oldest}}, \code{\link{replace.n.random}},
##' \code{\link{replace.older.than}},
##' \code{\link{replace.more.failures.than}}, \code{\link{do.nothing}}
##' @export
replace.n.highest.risk <- function(state, n, failure.rate, max.costs=Inf){
  inv <- state$inventory

  ## calculate risk (for the pipes in use)
  risk <- rep(NA, nrow(inv))
  for(i in which(inv$time.construction<state$time & inv$in.service)){
    
    Prob.fail <- failure.rate(age=state$time-inv$time.construction[i],
                              inv$time.last.failure[i]-inv$time.construction[i],
                              inv$n.failure[i])

    expected.failure.cost <- failure.cost(inv$diameter[i], mean=TRUE)
    
    risk[i] <- Prob.fail * expected.failure.cost
  } 

  idx <- order(risk, decreasing=TRUE)[1:min(n,sum(inv$in.service))]
 
  ## build new pipes and update budget
  state <- replace.pipes(state, idx, max.costs)
  
  return(state)
}
