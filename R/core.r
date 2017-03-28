## -------------------------------------------------------
##
## File: core.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------


## Initialize empty inventory
make.empty.inventory <- function() {

  inventory <- data.frame(ID=integer(),
                          time.construction=double(),
                          replacement.value=double(),
                          diameter=double(),
                          n.failure=integer(),
                          time.last.failure=double(),
                          time.end.of.service=double(),
                          in.service=logical())

  class(inventory) <- c("data.frame", "inventory")
  return(inventory) 
}


##' The diameter distribution is based on a real data set from ???
##' @title Sample a diameter of a new pipe.
##' @param n number of samples
##' @return a vector of diameters
##' @author Andreas Scheidegger
sample.diameter <- function(n=1){
  diam <- c(60, 70, 80, 100, 110, 120, 125, 130, 150, 175,
            180, 200, 250, 300, 350, 400, 500)
  freq <- c(4, 4, 285, 503, 1, 34, 625, 2, 2431,
            4, 13, 698, 84, 265, 2, 22, 23)

  sample(x=diam, n, replace=TRUE, prob=freq)
}


##' Based on eq(1) of "The water Network Management Chalange, Maurer (2017)",
##' assuming a pipe length of 100m
##' 
##' @title Calculate replacement value  
##' @param diameter diameter of the pipe [mm]
##' @return replacement value in CHF
##' @author Andreas Scheidegger
replacement.value <- function(diameter){
  100*(1.9*diameter + 540)              
}

##' Expand the network with additional pipes. The diameter of these pipes is sampled.
##' 
##' @title Model expansion of the network
##' @param state a state object
##' @param n.new \code{n.new} number of new pipes
##' @param separat.budget Boolan, if \code{TRUE} expansion cost are
##' not counted on the normal budget
##' @return the expanded inventory
##' @author Andreas Scheidegger
expand <- function(state, n.new, separat.budget=FALSE){

  inventory <- state$inventory
  budget <- state$budget
  time <- state$time
  
  if(nrow(inventory)>0){
    idmin <- max(inventory$ID) + 1
  } else {
    idmin <- 1
  }

  ## sample diameter and costs
  diameters <- sample.diameter(n.new)
  values <- replacement.value(diameters)

  ## check budget 
  if(!separat.budget){
    ## check how many pipes can be build with the budget?
    n.new <- max(which(cumsum(values)<state$budget), 0) 
    budget <- budget - ifelse(n.new>0, sum(values[1:n.new]), 0)
  }


  if(n.new>0){
    inventory.add <- data.frame(ID=idmin:(idmin+n.new-1),
                                time.construction=time,
                                replacement.value=values[1:n.new],
                                diameter=diameters[1:n.new],
                                n.failure=0,
                                time.last.failure=NA,
                                time.end.of.service=NA,
                                in.service=TRUE)
  
    inventory <- rbind(inventory, inventory.add)
    class(inventory) <- c("data.frame", "inventory")
  }
  


  return(list(inventory=inventory, budget=budget, time=time))
  
}


##' Calulate the costs caused by a failure according to sectio 5.1 in
##' "The Water Network Management Challenge", Max Maurer 2017.
##' 
##' @title Calculate (random) costs of a failure
##' @param diameter diameter [mm]
##' @param mean boolan. Should the expected cost be returned? Random otherwise.
##' @return if \code{mean=FALSE}, the cost of failure [CHF] are sampled
##' randomly. If \code{mean=TRUE}, the expected average costs are returned.
##' @author Andreas Scheidegger
failure.cost <- function(diameter, mean=FALSE){
  n <- length(diameter)
  
  mean.fix <- 6500
  sd.fix <- 1500
  mean.damage <- 7+sqrt(diameter/200)
  sd.damage <- sqrt(diameter/200)

  ## reparamerisation for log-normal 
  meanlog <- log(mean.damage) - 0.5*log(1 + (sd.damage/mean.damage)^2)
  sdlog <- sqrt(log(1 + sd.damage^2/(mean.damage^2)))

  if(mean){
    return(mean.fix + mean.damage)
  } else {
    ##            repair costs               +       damage costs
    return(max(0, rnorm(n, mean.fix, sd.fix)) + rlnorm(n, meanlog, sdlog))
  }
}



##' Lets pipe randomly fail. If a failure happened, the damage costs are
##' calculated and subtracted from the budget. Note, that this may result in a negative budget.
##'
##' @title Model failures of the network
##' @param state a state object
##' @param failure.rate function returning the failure rate.
##' \code{failure.rate} must take \code{age, time.last.failure, n.failure} as arguments.
##' @return inventory with new failures
##' @author Andreas Scheidegger
fail <- function(state, failure.rate){

  inventory <- state$inventory
  time <- state$time
  budget <- state$budget
  
  for(i in seq_len(nrow(inventory))){
    if(is.na(inventory$time.end.of.service[i])){
      
      Prob.fail <- failure.rate(age=time-inventory$time.construction[i],
                                inventory$time.last.failure[i],
                                inventory$n.failure[i])
      ## add failure, calculate costs
      if(runif(1) < Prob.fail){
        inventory$time.last.failure[i] <- time
        inventory$n.failure[i] <- inventory$n.failure[i] + 1
        budget <- budget - failure.cost(inventory$diameter[i]) # keep track of costs
      }
    } 
  }

  return(list(inventory=inventory, budget=budget, time=time))
}
