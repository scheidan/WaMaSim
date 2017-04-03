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
  diam <- c(80, 100, 125, 150, 180,
            200, 250, 300, 400, 500)
  freq <- c(293, 503, 661, 2431, 17,
            698, 84, 265, 24, 23)

  sample(x=diam, n, replace=TRUE, prob=freq)
}


##' Based on Eq(1) of "The Water Network Management Challenge, Max Maurer 2017",
##' assuming a pipe length of 100m.
##' 
##' @title Calculate replacement value  
##' @param diameter diameter of the pipe [mm]
##' @return replacement value [CHF]
##' @author Andreas Scheidegger
replacement.value <- function(diameter){
  100*(1.9*diameter + 540)              
}

##' Expand the network with additional pipes. The diameter of these pipes is sampled.
##' 
##' @title Model expansion of the network
##' @param state a state object
##' @param n.new \code{n.new} number of new pipes
##' @param separate.budget boolean, if \code{TRUE} then expansion costs 
##' are not deducted from the normal budget
##' @return the expanded inventory
##' @author Andreas Scheidegger
expand <- function(state, n.new, separate.budget=FALSE){

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
  if(!separate.budget){
    ## check how many pipes can be built with the budget
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


##' Calculate the costs caused by a failure according to Section 5.1 in
##' "The Water Network Management Challenge", Max Maurer 2017.
##' 
##' @title Calculate the (random) cost of a failure
##' @param diameter diameter [mm]
##' @param mean boolean. Should the expected cost be returned? Random otherwise.
##' @return if \code{mean=FALSE}, the failure costs [CHF] are sampled
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



##' Allows pipes to randomly fail. If a failure occurs, the failure costs (repair + damage) are
##' calculated and subtracted from the budget. Note, that this may result in a negative budget.
##'
##' @title Model failures of the network
##' @param state a state object
##' @param failure.rate function returning the annual failure rate; i.e. the probability of a 
##' pipe failing in the current year of simulation.
##' \code{failure.rate} must take \code{age, age.last.failure, n.failure} as input arguments.
##' @return inventory with new failures
##' @author Andreas Scheidegger
fail <- function(state, failure.rate){

  inventory <- state$inventory
  time <- state$time
  budget <- state$budget
  
  for(i in seq_len(nrow(inventory))){
    if(is.na(inventory$time.end.of.service[i])){
      
      Prob.fail <- failure.rate(age=time-inventory$time.construction[i],
                                time-inventory$age.last.failure[i],
                                inventory$n.failure[i])
      ## add failure, calculate costs, update budget
      if(runif(1) < Prob.fail){
        inventory$time.last.failure[i] <- time
        inventory$n.failure[i] <- inventory$n.failure[i] + 1
        budget <- budget - failure.cost(inventory$diameter[i]) # update budget
      }
    } 
  }

  return(list(inventory=inventory, budget=budget, time=time))
}



##' The number of failures per year is calculated
##' from a state list produced by \code{\link{simulate_network}}.
##'
##' @title Calculate number of failures per year
##' @param statelist a state list
##' @return vector containing the number of failures per year
##' @author Andreas Scheidegger
##' @export
failures.per.year <- function(statelist) {
  sapply(statelist, function(x) sum(x$inventory$time.last.failure == x$time, na.rm=TRUE))
}


##' The number of newly built pipes per year is calculated
##' from a state list produced by \code{\link{simulate_network}}.
##'
##' @title Calculate number of newly built pipes for each year
##' @param statelist a state list
##' @return vector containing the number  of newly built pipes for each year
##' @author Andreas Scheidegger
##' @export
pipes.built.per.year <- function(statelist) {
  sapply(statelist, function(x) sum(x$inventory$time.construction == x$time, na.rm=TRUE))
}


##' The annual total costs are calculated. The total costs consist
##' of damage, failure and rehabilitation costs.
##'
##' @title Calculate the total costs per year
##' @param statelist a state list
##' @param income the same values as passed to
##' \code{simulate_network}. Either a scalar or vector.
##' @return a vector of the total cost per year
##' @author Andreas Scheidegger
##' @export
costs.per.year <- function(statelist, income) {
  if(length(income)==1) income <- rep(income, length(statelist)-1)
  c(0, income - diff(statelist$budget))
}

