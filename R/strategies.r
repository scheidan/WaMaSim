## -------------------------------------------------------
##
## File: strategies.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------


## 1.1 No rehabilitation
## This is the simplest strategy and very often
## applied in practice. It means that broken pipes will be repaired,
## but no pipes replaced with a new one (rehabilitated).

## 1.2 Replacement age
## As soon as a pipe reaches a specific age, it
## will be replaced with a new one. Typically, the technical
## guidelines give an estimated lifespan of a water main, e.g. 50
## years. This then will be the replacement age.

## 1.3 Number of breaks
## Often pipes are replaced after a number of
## breaks. This reflects the often observed fact that the time between
## the breaks of a single pipe becomes shorter with increasing number
## of breaks.

## 1.4 Fixed budget or fixed capacity
## Often the utility has a fixed
## annual budget to operate with. Either in terms of money or then in
## terms of manpower that can be mustered to manage all the repair and
## rehabilitation.

## 1.5 Risk based prioritisation
## This is a very modern approach, where
## pipes with a high potential for damage will be given priority. This
## is usually combined with another strategy measure, e.g. replacement
## age. In this exercise the damage potential is proportional to the
## pipe diameter.

## 1.6 Combination of different factors
## A typical combination is a
## fixed budget with a replacement age, where the remaining budget
## after all repairs are used to rehabilitate the oldest pipes.

## -----------
## replace pipe at 'idx' in the inventory with a new one
replace.pipe <- function(idx, inv, time){

  ## get new ID
  if(nrow(inv)>0){
    id <- max(inv$ID) + 1
  } else {
    id <- 1
  }

                                        # retire old pipe
  inv$time.end.of.service[idx] <- time  

                                        # add new pipe
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
##' @title rehabilitation strategy: replace the oldest pipes
##' @param state 
##' @param n.max maximal number of pipes to replace
##' @return a state list
##' @author Andreas Scheidegger
replace.oldest <- function(state, n.max, time){
  inv = state$inventory
  budget = state$budget
  
  ## find the index of the n oldest pipes *in use*
  is.active <- is.na(inv$time.end.of.service)
  idx <- order(inv$time.construction + as.numeric(!is.active)*1E10)[1:(min(n.max, nrow(inv)))]

  ## build new pipes
  for(i in idx) {
    if(budget < inv$replacement.value[i]){
      break
    } else {
      budget <- budget - inv$replacement.value[i] # pay for new pipe
      inv <- replace.pipe(i, inv, time)       # get new pipe
    }
  }
  
  return(list(inventory=inv, budget=budget))
}
