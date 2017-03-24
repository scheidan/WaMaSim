## -------------------------------------------------------
##
## File: strategies.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------


## [x] 1.1 No rehabilitation

## [x] 1.2 Replacement age

## [x] 1.3 Number of failures

## [x] 1.4 Fixed budget or fixed capacity -> every strategy has a budget

## [ ] 1.5 Risk based prioritisation -> needs first risk calcualtion

## [x]     Random replacement

## [x] 1.6 Combination of different factors




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
                         damage.potential=inv$damage.potential[idx],
                         n.failure=0,
                         time.last.failure=NA,
                         time.end.of.service=NA)

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



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title rehabilitation strategy: no rehabilitation
##' @param state 
##' @return a state list
##' @author Andreas Scheidegger
do.nothing <- function(state){
  return(state)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title rehabilitation strategy: replace pipes over max.age
##' @param state a state list
##' @param max.age pipes older than max.age are replaced
##' @return a state list
##' @author Andreas Scheidegger
replace.older.than <- function(state, max.age){
  inv <- state$inventory
  
  ## find the index of the pipes older than max.age that are *in use*
  is.active <- is.na(inv$time.end.of.service)
  age <- state$time - inv$time.construction
  idx <- which(age > max.age & is.active)

  ## build new pipes and update budget
  state <- replace.pipes(state, idx)
  
  return(state)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title rehabilitation strategy: replace pipes with too many failures
##' @param state a state list
##' @param max.failures maximal allowed number of failures
##' @return a state list
##' @author Andreas Scheidegger
replace.more.failures.than <- function(state, max.failures){
  inv <- state$inventory

  ## find the index of the pipes older >max.age that are *in use*
  is.active <- is.na(inv$time.end.of.service)
  idx <- which(inv$n.failure > max.failures & is.active)

  ## build new pipes and update budget
  state <- replace.pipes(state, idx)
  
  return(state)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title rehabilitation strategy: replace the n oldest pipes
##' @param state 
##' @param n.max maximal number of pipes to replace
##' @return a state list
##' @author Andreas Scheidegger
replace.n.oldest <- function(state, n){
  inv <- state$inventory
  
  ## find the index of the n oldest pipes *in use*
  is.active <- is.na(inv$time.end.of.service)
  idx <- order(inv$time.construction + as.numeric(!is.active)*1E10)[1:(min(n, nrow(inv)))]

  ## build new pipes and update budget
  state <- replace.pipes(state, idx)
  
  return(state)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title rehabilitation strategy: replace the n randomly selected pipes
##' @param state 
##' @param n maximal number of pipes to replace
##' @return a state list
##' @author Andreas Scheidegger
replace.n.random <- function(state, n){
  inv <- state$inventory
  
  ## sample randomly the index of pipes that are *in use*
  is.active <- is.na(inv$time.end.of.service)
  idx <- sample(which(is.active), min(nrow(inv), n))

  ## build new pipes and update budget
  state <- replace.pipes(state, idx)
  
  return(state)
}

