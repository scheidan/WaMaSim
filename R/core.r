## -------------------------------------------------------
##
## File: core.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
##
##   This program is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##
##   This program is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
##   You should have received a copy of the GNU General Public License
##   along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
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


##' The diameter distribution is based on a real data set from Lisa Scholten 
##' (pipe_data.csv). Refer to the old exercise on watermain break modelling
##' in the ETH Infrastructure Systems course by Max Maurer. 
##' @title Sample a diameter of a new pipe.
##' @param n number of samples
##' @return a vector of diameters
##' @author Andreas Scheidegger
sample.diameter <- function(n=1){
  diam <- c(80, 100, 125, 150, 180, 200, 250, 300, 400, 500)
  freq <- c(293, 503, 661, 2431, 17, 698, 84, 265, 24, 23)

  sample(x=diam, n, replace=TRUE, prob=freq)
}


##' Based on Eq(14) of "The Water Network Management Challenge, Max Maurer 2017",
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


##' Calculate the costs caused by a failure according to Section 7.1 in
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
  mean.log.damage <- 7+sqrt(diameter/200)
  sd.log.damage <- sqrt(diameter/200)
  mean.damage <- exp(mean.log.damage + sd.log.damage^2/2) 

  if(mean){
    return(mean.fix + mean.damage)
  } else {
    ##            repair costs                +       damage costs
    return(max(0, rnorm(n, mean.fix, sd.fix)) + rlnorm(n, mean.log.damage, sd.log.damage))
  }
}



##' Allows pipes to randomly fail. If a failure occurs, the failure costs (repair + damage) are
##' calculated and subtracted from the budget. Note, that this may result in a negative budget.
##'
##' @title Model failures of the network
##' @param state a state object
##' @param prob.failure function returning the annual failure rate; i.e. the probability of a 
##' pipe failing in the current year of simulation.
##' \code{prob.failure} must take \code{age, age.last.failure, n.failure} as input arguments.
##' Note that in the case of a pipe with zero previous failures (i.e. \code{n.failure}=0), 
##' \code{age.last.failure}=NA.
##' @return inventory with new failures
##' @author Andreas Scheidegger
fail <- function(state, prob.failure){

  inventory <- state$inventory
  time <- state$time
  budget <- state$budget
  
  for(i in seq_len(nrow(inventory))){
    if(is.na(inventory$time.end.of.service[i])){
      
      Prob.fail <- prob.failure(age=time-inventory$time.construction[i],
                                inventory$time.last.failure[i]-inventory$time.construction[i],
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
failures.per.year <- function(statelist){
  sapply(statelist, function(x) sum(x$inventory$time.last.failure == x$time, na.rm=TRUE))
}


##' The number of newly built pipes per year is calculated
##' from a state list produced by \code{\link{simulate_network}}.
##'
##' @title Calculate number of newly built pipes for each year
##' @param statelist a state list
##' @return vector containing the number of newly built pipes for each year
##' @author Andreas Scheidegger
##' @export
pipes.built.per.year <- function(statelist){
  sapply(statelist, function(x) sum(x$inventory$time.construction == x$time, na.rm=TRUE))
}


##' The number of pipes in service is calculated for every year
##' based on a state list produced by \code{\link{simulate_network}}.
##' 
##' @title Returns the number of pipes in service for each year
##' @param statelist a state list
##' @return vector containing the number of pipes in service
##' @author Andreas Scheidegger
##' @export
pipes.inservice.per.year <- function(statelist){
  sapply(statelist, function(x) sum(x$inventory$in.service))
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
costs.per.year <- function(statelist, income){
  if(length(income)==1) income <- rep(income, length(statelist)-1)
  c(0, income - diff(statelist$budget))
}

