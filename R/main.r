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
##' WaMaSim is a package to simulate the effect of different
##' rehabiliation strategies for water distribution systems. It is an education tool
##' used for the Water Infrastructure Experimental and Computer Laboratory at ETH Zurich, Switzerland.
##' See the documentation for \code{\link{simulate}} to get started.
##' @name WaMaSim
##' @author Andreas Scheidegger
##' @docType package
NULL



##' Simulates failures, expansion, rehabilitation, and costs of a water supply pipe network.
##' The simulation is stochastic.
##'
##' The rehabilitation is defined by combining different simple replacement strategies.
##' See the example for how this can be done using the \code{mystrategy} function input.
##'
##' The \code{failure.rate} is a function that must take \code{age, time.last.failure, n.failure}
##' as arguments.
##'
##' The costs are calculated as a function of the pipe diameter, assuming all pipes have a
##' length of 100 meters.
##'
##' @title Run a simulation
##'
##' @param t.sim number of years to simulate
##' @param expansion either a scalar describing the number of pipes added
##' every year to expand the pipe network, or a vector of length \code{t.sim}.
##' @param rehabilitation a (combination of) rehabilitation strategy function(s). See details below.
##' @param failure.rate a function describing the probability of a pipe failing in the next year
##' given its age, number of previous failures, and the point in time of the last failure (if any).
##' @param income either a scalar describing the annual income, or a vector of length \code{t.sim}.
##' @param initial.budget initial budget
##' @param initial.inventory if \code{initial.inventory} equals \code{NULL} the simulation starts 
##' without pipes, if it is an integer it specifies the number of initial pipes,
##' or alternatively it can be a \code{data.frame} containing the initial inventory of pipes.
##'
##' @return A list of length \code{t.sim+1} containing all modeled states. A state
##' is a list consisting of the time, the budget, and the inventory at a given point in time.
##'
##' @seealso Different replacement strategies are implemented: \code{\link{replace.n.highest.risk}},
##' \code{\link{replace.n.oldest}}, \code{\link{replace.n.random}}, \code{\link{replace.older.than}},
##' \code{\link{replace.more.failures.than}}, \code{\link{do.nothing}}
##'
##' @examples
##
##' # Define failure rate
##' f.rate <- function(age, time.last.failure, n.failure) {
##'   if(n.failure==0){
##'     return(1/30)
##'   } else {
##'     return(1/10)
##'   }
##' }
##'
##'
##' # Define a complicated (and probably useless) rehabilitation strategy
##' mystrategy <- . %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate, max.costs=100e3) %>%
##'   replace.more.failures.than(max.failures=5) %>%
##'   replace.older.than(max.age=100, max.cost=2e6) %>%
##'   replace.n.oldest(n=2) %>%
##'   replace.n.random(n=2)
##' # This defines a prioritized sequence of annual rehabilitation steps as follows: 
##' # each year, and as long as we have enough budget, replace first the 2 pipes 
##' # with the highest risk of failure, then all pipes with more than 5 failures,
##' # then all pipes older then 100 years, then the 3 oldest remaining pipes, and
##' # finally replace 4 randomly selected pipes. Additionally, spendings on the first 
##' # rehabilitation strategy, \code{replace.n.highest.risk}, can not exceed a maximum 
##' # budget of 100,000 CHF.
##'
##' # Define a "do nothing" rehabilitation strategy (i.e. repairs only, no pipe replacement)
##' mystrategy <- . %>% do.nothing
##' 
##'
##' # Run the simulation
##' result <- simulate(t.sim=100,                  # run it for 100 years
##'                    expansion=0,                # do not expand the system
##'                    rehabilitation=mystrategy,  # use the strategy defined above
##'                    failure.rate=f.rate,        # use the failure rate defined above
##'                    income = 1e6,               # the annual income
##'                    initial.budget=30e6,        # the initial budget
##'                    initial.inventory=500)      # start the simulation with 500 new pipes
##'
##' str(result)    # just a long list of states
##' 
##' ## Convenience functions to extract budget or time are available
##' result$time  
##' result$budget
##'
##' @author Andreas Scheidegger
##' @export
simulate <- function(t.sim,
                     expansion,
                     rehabilitation,
                     failure.rate,
                     income=0,
                     initial.budget=Inf,
                     initial.inventory=NULL) {

  if(!("function" %in% class(rehabilitation))){
    stop("argument 'rehabilitation' must be a function!")
  }
  if(!("function" %in% class(failure.rate))){
    stop("argument 'failure.rate' must be a function!")
  }

  if(length(expansion)==1) expansion <- rep(expansion, t.sim)
  if(length(income)==1) income <- rep(income, t.sim)


  ## build initial state
  if(!is.null(initial.inventory) &
     !is.numeric(initial.inventory) &
     !("data.frame" %in% class(initial.inventory))) {
    stop("Argument 'initial.inventory' must be NULL, an integer, or a data.frame!")
  }
  if(is.null(initial.inventory)){
    state <- list(inventory=make.empty.inventory(), budget=initial.budget, time=0)
  }
  if(is.numeric(initial.inventory)){
    state <- list(inventory=make.empty.inventory(), budget=initial.budget, time=0)
    state <- expand(state, initial.inventory, separate.budget=TRUE)
  }
  if("data.frame" %in% class(initial.inventory)){
    if(!all(colnames(initial.inventory) == c("ID","time.construction", "replacement.value",
                                        "diameter", "n.failure", "time.last.failure",
                                        "time.end.of.service", "in.service"))){
      stop("incorrect column names of 'initial.inventory'!")
    }
    class(initial.inventory) <- c("data.frame", "inventory")
    state <- list(inventory=initial.inventory, budget=initial.budget, time=0)
  }




  ## list to keep results
  result <- vector("list", t.sim+1)
  names(result) <- paste0("time.", 0:(t.sim))
  result[[1]] <- state

  ## loop over time
  for(t in 1:t.sim){
    if(t%%10==0){
      print(paste("Simulate year", t))
    }

    ## 0) update time
    state$time = state$time + 1

    ## 1) expand system
    state <- expand(state, expansion[t],
                    separate.budget=TRUE)

    ## 2) collect fees
    state$budget <- state$budget + income[t]

    ## 3) simulate failures
    state <- fail(state, failure.rate)

    ## # 4) rehabilitate pipes
    state <- rehabilitation(state)

    result[[t+1]] <- state
  }

  class(result) <- "statelist"
  return(result)
}

##' Convenience functions to extract the time or budget.
##'
##' @title Extract time or budget as vectors
##' @param x a state list
##' @param name either \code{budget} or \code{time}
##' @return a vector of the time or budget
##' @author Andreas Scheidegger
##'
##' @examples
##' \dontrun{
##' result$budget     # result is a 'statelist' returned from simulate
##' result$time
##' }
##' @export
`$.statelist` <- function(x, name){
  if(name %in% c("budget", "time")) {
    sapply(x, function(y) getElement(y, name)) 
  }
}

