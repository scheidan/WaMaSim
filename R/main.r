## -------------------------------------------------------
##
## File: main.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------

##' @import stats
##' @import magrittr
NULL

##' WaMaSim - Water Management Simulator
##'
##' WaMaSim is a package that simulates the effect of different
##' rehabilitation strategies for water distribution systems. It is an education tool
##' used for the Water Infrastructure Experimental and Computer Laboratory at ETH Zurich, Switzerland.
##' See the documentation for \code{\link{simulate.network}} to get started.
##' @name WaMaSim
##' @author Andreas Scheidegger
##' @docType package
NULL


##' Creates the initial network that can be used with \code{\link{simulate.network}}.
##'
##' @title creates the initial network 
##' @param budget initial budget
##' @param inventory if \code{inventory} is an integer it specifies the
##' number of initial pipes, or alternatively it can be a \code{data.frame}
##' containing the initial inventory of pipes.
##' @return a state list
##' @author Andreas Scheidegger
##' @export
initiate.network <- function(inventory=0,
                             budget=Inf) {


  ## build initial state
  if(!is.numeric(inventory) &
     !("data.frame" %in% class(inventory))) {
    stop("Argument 'inventory' must a positive integer, or a data.frame!")
  }

  if(is.numeric(inventory)){
    state <- list(inventory=make.empty.inventory(), budget=budget, time=0)
    state <- expand(state, n.new=inventory, separate.budget=TRUE)
  }
  if("data.frame" %in% class(inventory)){
    if(!all(colnames(inventory) == c("ID","time.construction", "replacement.value",
                                     "diameter", "n.failure", "time.last.failure",
                                     "time.end.of.service", "in.service"))){
      stop("incorrect column names of 'inventory'!")
    }
    class(inventory) <- c("data.frame", "inventory")
    state <- list(inventory=inventory, budget=budget, time=0)
  }

  ## list to keep results
  result <- list()
  result$time.0 <- state
  
  return(result)

}


##' Simulates failures, expansion, rehabilitation, and costs of a water supply pipe network.
##' The simulation is stochastic.
##'
##' The rehabilitation is defined by combining different simple replacement strategies.
##' See the example for how this can be done using the \code{mystrategy} function input.
##'
##' The failure behavior is defined by the function \code{failure.rate}.
##' It calculates the probability of a pipe failing within the next year based on pipe age,
##' pipe age at the last failure, and the number of failures. Note, the model
##' makes the assumption that a pipe cannot fail more than once per year.
##'
##' The costs are calculated as a function of the pipe diameter, assuming all pipes have a
##' length of 100 meters.
##'
##' @title Simulate the network for a period of time
##' @param statelist a state list
##' @param n.years number of years to simulate
##' @param expansion either a scalar describing the number of pipes added
##' every year to expand the pipe network, or a vector of length \code{n.years}.
##' @param rehabilitation a (combination of) rehabilitation strategy function(s). See details below.
##' @param failure.rate a function describing the probability of a pipe failing in the next year
##' given its age, number of previous failures, and the age at the last failure (if any).
##' @param income either a scalar describing the annual income, or a vector of length \code{n.years}.
##' @return an updated state list
##' @seealso \code{\link{simulate.network}} provides a slightly more convenient interface.
##' @author Andreas Scheidegger
##' @export
simulate.network.period <- function(statelist,
                                    n.years,
                                    expansion,
                                    rehabilitation,
                                    failure.rate,
                                    income=0) {

  last.year <- statelist[[length(statelist)]]$time
  state <- statelist[[length(statelist)]]

  if(length(expansion)==1) expansion <- rep(expansion, n.years)
  if(length(income)==1) income <- rep(income, n.years)
  if(length(expansion)!=n.years) stop("`expansion` must be of length one or `n.years`!")
  if(length(income)!=n.years) stop("`income` must be of length one or `n.years`!")
  
  ## loop over time
  i <- 1
  for(t in (last.year+1):(last.year+n.years)){
    if(t%%10==0){
      print(paste("Simulate year", t))
    }
    
    ## 0) update time
    state$time <- state$time + 1

    ## 1) expand system
    state <- expand(state, expansion[i],
                    separate.budget=TRUE)

    ## 2) collect fees
    state$budget <- state$budget + income[i]

    ## 3) simulate failures
    state <- fail(state, failure.rate)

    ## 4) rehabilitate pipes
    state <- rehabilitation(state)

    statelist[[t+1]] <- state
    names(statelist[t+1]) <- paste0("time.", t)

    i <- i+1
  }

  class(statelist) <- "statelist"
  return(statelist)

}

##' Simulates failures, expansion, rehabilitation, and costs of a water supply pipe network.
##' The simulation is stochastic.
##'
##' The rehabilitation is defined by combining different simple replacement strategies.
##' See the example for how this can be done using the \code{mystrategy} function input.
##' If the strategies vary over time, see \code{\link{initiate.network}} and
##' \code{\link{simulate.network.period}}.
##'
##' The failure behavior is defined by the function \code{failure.rate}.
##' It calculates the probability of a pipe failing within the next year based on pipe age,
##' pipe age at the last failure, and the number of failures. Note, the model
##' makes the assumption that a pipe cannot fail more than once per year.
##'
##' The costs are calculated as a function of the pipe diameter, assuming all pipes have a
##' length of 100 meters.
##'
##' @title Simulate the failures, expansion, rehabilitation, and costs of a network
##' @param n.years number of years to simulate
##' @param expansion either a scalar describing the number of pipes added
##' every year to expand the pipe network, or a vector of length \code{n.years}.
##' @param rehabilitation a (combination of) rehabilitation strategy function(s). See details below.
##' @param failure.rate a function describing the probability of a pipe failing in the next year
##' given its age, number of previous failures, and the age at the last failure (if any).
##' @param income either a scalar describing the annual income, or a vector of length \code{n.years}.
##' @param initial.budget initial budget
##' @param initial.inventory if it is an integer it specifies the
##' number of initial pipes, or alternatively it can be a \code{data.frame}
##' containing the initial inventory of pipes.
##' 
##' @return an updated state list
##' 
##' @seealso For more fine-grained control see \code{\link{initiate.network}}
##' and \code{\link{simulate.network.period}}. Different replacement strategies
##' are implemented: \code{\link{replace.n.highest.risk}},
##' \code{\link{replace.n.oldest}}, \code{\link{replace.n.random}}, \code{\link{replace.older.than}},
##' \code{\link{replace.more.failures.than}}, \code{\link{do.nothing}}.
##'
##' @author Andreas Scheidegger
##' @export
simulate.network <- function(n.years,
                             expansion,
                             rehabilitation,
                             failure.rate,
                             income=0,
                             initial.budget=Inf,
                             initial.inventory=NULL) {

  statelist <- initiate.network(initial.inventory, initial.budget)
  statelist <- simulate.network.period(statelist,
                                       n.years = n.years, 
                                       expansion = expansion, 
                                       rehabilitation = rehabilitation,
                                       failure.rate=failure.rate, 
                                       income = income)
  return(statelist)

}

##' Convenient functions to extract the time or budget.
##'
##' @title Extract time or budget as vectors
##' @param x a state list
##' @param name name of the element to extract
##' @return a vector of the time or budget, or a state
##' @author Andreas Scheidegger
##'
##' @examples
##' \dontrun{
##' str(result)      # result is a 'statelist' returned from simulate.network
##' result$budget    # vector of budget
##' result$time      # vector of time
##' result$time.22   # state list of time 22
##' }
##' @export
`$.statelist` <- function(x, name){
  if(name %in% c("budget", "time")) {
    ret <- sapply(x, function(y) getElement(y, name))
    names(ret) <- paste0("time.", sapply(x, function(y) getElement(y, "time")))
  } else {
    ret <- getElement(x, name)
  }
  return(ret)
}

