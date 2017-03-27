## -------------------------------------------------------
##
## File: main.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------

##' WaMaSim is a package to simulate the effect of different
##' rehabiliation strategies for water distribution systems.
##' @name WaMaSim
##' @author Andreas Scheidegger
##' @docType package



library(magrittr)

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Function to run a simulation
##' @param t.sim number of years to simulate
##' @param expansion a scalar the descriving the number of pipes added
##' annualy or a evctor of length \code{t.sim}
##' @param rehabilitation a (combination of) rehabiltation strategies functions
##' @param failure.rate a function screibing the failure rate
##' @param income.per.pipe anual income per pipe
##' @param initial.budget initial budget
##' @param initial.inventory if \code{NULL} the simulation starts without pipes,
##' or an integer can be given specifying the numbe rof initial pipes,
##' or an initial inventory can be specified.
##' @return A list of length \code{t.sim+1} conting all model states.
##' @author Andreas Scheidegger
##' @export
simulate <- function(t.sim,
                     expansion,   
                     rehabilitation,
                     failure.rate,
                     income.per.pipe,
                     initial.budget=Inf,
                     initial.inventory=NULL) {

  if(!("function" %in% class(rehabilitation))){
    stop("argument 'rehabilitation' must be a function!")
  }
  if(!("function" %in% class(failure.rate))){
    stop("argument 'failure.rate' must be a function!")
  }

  if(length(expansion)==1) expansion <- rep(expansion, t.sim)
  
  ## build initial state
  if(is.null(initial.inventory) | is.numeric(initial.inventory)){
    inv  <- make.empty.inventory()
  }

  state <- list(inventory=inv, budget=initial.budget, time=0)

  if(is.numeric(initial.inventory)){
    state <- expand(state, initial.inventory,
                    replacement.value=1000,
                    separat.budget=TRUE)
  }


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
                    separat.budget=TRUE) 
    
    ## 2) collect fees
    state$budget <- state$budget + income.per.pipe*sum(state$inventory$in.service)
    
    ## 3) simulate failures
    state <- fail(state, failure.rate)

    ## ## 4) rehabilitate pipes
    state <- rehabilitation(state)

    result[[t+1]] <- state    
  }

  return(result)
}


