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



##' Simulates failures expansion, rehabilitation and cost of a pipe network.
##' The simulation is stochastic.
##'
##' The rehabilitation is defined by combining different simple replacement strategies.
##' See the example how the strategies are can be linked with the pipe operator.
##'
##' The \code{failure.rate} is a function that must take \code{age, time.last.failure, n.failure}
##' as arguments.
##'
##' The cost are calcualted as a finction of the diameter, assuming all pipes have a
##' length of 100 meters.
##'
##' @title Run a simulation
##'
##' @param t.sim number of years to simulate
##' @param expansion either a scalar the describing the number of pipes added
##' every year, or a vector of length \code{t.sim}.
##' @param rehabilitation a (combination of) rehabilitation strategies functions. See details below.
##' @param failure.rate a function describing the probability for a pipe to fail in the next
##' year given the pipe age, number of failures, and the point in time of the last failure (if any).
##' @param income either a scalar describing the annual income, or a vector of length \code{t.sim}.
##' @param initial.budget initial budget
##' @param initial.inventory If equals \code{NULL} the simulation starts without pipes,
##' if \code{initial.inventory} is integer it specifies the number of initial pipes,
##' or it can be a \code{data.frame} containing the initial inventory.
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
##' # define a complicated (and probably useless) rehabilitation strategy
##' mystrategy <- . %>%
##'   replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
##'   replace.more.failures.than(max.failures=5) %>%
##'   replace.older.than(max.age=100, max.cost=2e6) %>%
##'   replace.n.oldest(n=2) %>%
##'   replace.n.random(n=2)
##' # This means: every year (if we have enough budget!), replace first the 2 pipes
##' # with the highest risk, then all pipes with more than 5 failures,
##' # then all pipes older then 100 years, then the 3 oldest pipes remaining, and
##' # finally replace 4 randomly selected pipes.
##'
##'
##' # run the simulation
##' result <- simulate(t.sim=100,                  # run it for 100 years
##'                    expansion=0,                # do not expand the system
##'                    rehabilitation=mystrategy,  # use the strategy defined above
##'                    failure.rate=f.rate,        # use the failure rate defined above
##'                    income = 1e6,               # the annual income
##'                    initial.budget=30e6,
##'                    initial.inventory=500)      # start the simulation with 50 new pipes
##'
##' str(result)    # just a long list of states
##' 
##' ## convinience functions extract budget or time are available
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
     !(class(initial.inventory)=="data.frame")) {
    stop("Argument 'initial.inventory' must be NULL, an integer or a data.frame!")
  }
  if(is.null(initial.inventory)){
    state <- list(inventory=make.empty.inventory(), budget=initial.budget, time=0)
  }
  if(is.numeric(initial.inventory)){
    state <- list(inventory=make.empty.inventory(), budget=initial.budget, time=0)
    state <- expand(state, initial.inventory, separat.budget=TRUE)
  }
  if(class(initial.inventory)=="data.frame"){
    if(colnames(initial.inventory) != c("ID","time.construction", "replacement.value",
                                        "diameter", "n.failure", "time.last.failure",
                                        "time.end.of.service", "in.service")){
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
                    separat.budget=TRUE)

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

##' Convinience function to extract the time and budget.
##'
##' @title Extract time and budget as vector
##' @param x a state list
##' @param name either \code{budget} or \code{time}
##' @return a vector of the time or budgets
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

