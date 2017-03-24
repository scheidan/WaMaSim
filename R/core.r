## -------------------------------------------------------
##
## File: core.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Initialize empty inventory
##' @return an inventory object
##' @author Andreas Scheidegger
make.empty.inventory <- function() {

  inventory <- data.frame(ID=integer(),
                          time.construction=double(),
                          replacement.value=double(),
                          damage.potential=double(),
                          n.failure=integer(),
                          time.last.failure=double(),
                          time.end.of.service=double())

  class(inventory) <- c("data.frame", "inventory")
  return(inventory) 
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Model failures of the network
##' @param inventory inventory object
##' @param failure.rate function returning the failure rate (age, time.last.failure, n.failure)
##' @param time point in time
##' @return inventory with new failures
##' @author Andreas Scheidegger
fail <- function(inventory, failure.rate, time){

  for(i in 1:nrow(inventory)){
    if(is.na(inventory$time.end.of.service[i])){
      
      Prob.fail <- failure.rate(age=time-inventory$time.construction[i],
                                inventory$time.last.failure[i],
                                inventory$n.failure[i])
      ## add failure
      if(runif(1) < Prob.fail){
        inventory$time.last.failure[i] <- time
        inventory$n.failure[i] <- inventory$n.failure[i] + 1
      }
    } 
  }

  return(inventory)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Model expansion of the network
##' @param inventory inventory object
##' @param n.new number of new pipes
##' @param replacement.value replacement.value of new pipes
##' @param time point in time of expansion
##' @return the expanded inventory
##' @author Andreas Scheidegger
expand <- function(inventory, n.new, time, replacement.value=1000){

  if(nrow(inventory)>0){
    id.min <- max(inventory$ID) + 1
  } else {
    idmin <- 1
  }

  inventory.add <- data.frame(ID=idmin:(idmin+n.new-1),
                              time.construction=time,
                              replacement.value=replacement.value,
                              damage.potential=NA,
                              n.failure=0,
                              time.last.failure=NA,
                              time.end.of.service=NA)
  
  inventory <- rbind(inventory, inventory.add)
  class(inventory) <- c("data.frame", "inventory") 
  return(inventory)
}
