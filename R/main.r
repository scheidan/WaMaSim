## -------------------------------------------------------
##
## File: main.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------

library(magrittr)

simulate <- function(t.sim,
                     expansion,   
                     rehabilitation,
                     failure.rate,
                     initial.budget=Inf,
                     initial.inventory=NULL) {

  if(!("function" %in% class(rehabilitation))){
    error("argument 'rehabilitation' must be a function!")
  }
  if(!("function" %in% class(failure.rate))){
    error("argument 'failure.rate' must be a function!")
  }

  if(length(expansion)==1) expansion <- rep(expansion, t.sim)
  
  ## build initial state
  if(is.null(initial.inventory)) inv <- make.empty.inventory()
  state <- list(inventory=inv, budget=initial.budget, time=0)

  ## list to keep results
  result <- vector("list", t.sim+1) 
  names(result) <- paste0("time.", 1:(t.sim+1))
  result[[1]] <- state
  
  ## loop over time
  for(t in 1:t.sim){
    print(paste("Simulate step", t))
    
    ## 0) update time
    state$time = state$time + 1
    
    ## 1) expand system
    state <- expand(state, expansion[t], replacement.value=1000,
                    separat.budget=FALSE) 
    
    ## 2) collect fees
    state$budget <- state$budget + 4000

    print(state$budget)
    
    ## 3) simulate failures
    state <- fail(state, failure.rate)

    ## 4) rehabilitate pipes
    state <- rehabilitation(state)

    result[[t+1]] <- state    
  }

  return(result)
}


