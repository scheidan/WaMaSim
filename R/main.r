## -------------------------------------------------------
##
## File: main.r
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

##' @import stats
##' @import magrittr
NULL

##' WaMaSim - Water Management Simulator
##'
##' WaMaSim is a package that simulates the effect of different
##' rehabilitation strategies for water distribution systems. It is an education tool
##' used for the Water Infrastructure Experimental and Computer Laboratory at ETH Zurich, Switzerland.
##' See the documentation for \code{\link{simulate_network}} to get started.
##' @name WaMaSim
##' @author Andreas Scheidegger
##' @docType package
NULL


##' Creates the initial network that can be used with \code{\link{simulate_network}}.
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
##' The failure behavior is defined by the function \code{prob.failure}.
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
##' Negative values are not allowed.
##' @param rehabilitation a (combination of) rehabilitation strategy function(s). See details below.
##' @param prob.failure a function describing the probability of a pipe failing in the next year
##' given its age, number of previous failures, and the age at the last failure (if any).
##' @param income either a scalar describing the annual income, or a vector of length \code{n.years}.
##' @param free.expansion if \code{TRUE} costs for network expansion are not deducted from the budget.
##' @return an updated state list
##' @seealso \code{\link{simulate_network}} provides a slightly more convenient interface.
##' @author Andreas Scheidegger
##'
##' @examples
##' ## -----------
##' ## define model parameters
##' 
##' ## Define the annual probability of a failure
##' prob.failure.exp <- function(age, age.last.failure, n.failure) {
##'   if(n.failure==0){
##'     return(1/30)
##'   } else {
##'     return(1/10)
##'   }
##' }
##' 
##' ## define a complicated (and probably useless) rehabilitation strategy
##' mystrategy <- . %>%
##'   replace.n.highest.risk(n=2, prob.failure=prob.failure.exp) %>%
##'   replace.more.failures.than(failures=5) %>%
##'   replace.older.than(age=70, max.cost=2e6)  %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.random(n=2)
##' ## This means: every year (if we have enough budget!), replace first the 2 pipes
##' ## with the highest risk, then all pipes with more than 5 failures,
##' ## then all pipes older then 70 years (up to costs of 2e6), then the 3
##' ## oldest pipes remaining, and finally replace 2 randomly selected pipes. 
##' 
##' 
##' ## -----------
##' ## run the simulation in steps
##' 
##' statelist <- initiate.network(inventory = 50, budget = 1e7)
##' 
##' statelist <- simulate_network.period(
##' 
##'    statelist,                       # state list to update
##'    n.years = 20,                    # run it for 20 years
##'    expansion = 10,                  # build 10 pipes per year (if money is available)
##'    rehabilitation = mystrategy,     # use the strategy defined above
##'    prob.failure = prob.failure.exp, # use the probability function defined above
##'    income = 1e6                     # the annual income
##'                  
##'    )                    
##' 
##' statelist <- simulate_network.period(
##'    
##'    statelist,                       # state list to update
##'    n.years = 10,                    # run it for 10 more years
##'    expansion = 2,                   # now, build only 2 pipes per year (if money is available)
##'    rehabilitation = mystrategy,     # use the strategy defined above
##'    prob.failure = prob.failure.exp, # use the probability function defined above
##'    income = 1e6                     # the annual income
##'                  
##'    )     
##' 
##' 
##' ## look at some results
##' ## str(statelist)
##' ## str(statelist$time.30)
##' 
##' @export
simulate_network.period <- function(statelist,
                                    n.years,
                                    expansion,
                                    rehabilitation,
                                    prob.failure,
                                    income=0,
                                    free.expansion=TRUE) {

  last.year <- statelist[[length(statelist)]]$time
  state <- statelist[[length(statelist)]]

    if(any(expansion<0)) stop("Negative values for `expansion` are not allowed!")
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
                        separate.budget=free.expansion)

        ## 2) collect fees
        state$budget <- state$budget + income[i]

        ## 3) simulate failures
        state <- fail(state, prob.failure)

        ## 4) rehabilitate pipes
        state <- rehabilitation(state)

        statelist[[t+1]] <- state
        names(statelist)[t+1] <- paste0("time.", t)

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
##' \code{\link{simulate_network.period}}.
##'
##' The failure behavior is defined by the function \code{prob.failure}.
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
##' Negative values are not allowed.
##' @param rehabilitation a (combination of) rehabilitation strategy function(s). See details below.
##' @param prob.failure a function describing the probability of a pipe failing in the next year
##' given its age, number of previous failures, and the age at the last failure (if any).
##' @param income either a scalar describing the annual income, or a vector of length \code{n.years}.
##' @param initial.budget initial budget
##' @param initial.inventory if it is an integer it specifies the
##' number of initial pipes, or alternatively it can be a \code{data.frame}
##' containing the initial inventory of pipes.
##' @param free.expansion if \code{TRUE} costs for network expansion are not deducted from the budget.
##' 
##' @return an updated state list
##' 
##' @seealso For more fine-grained control see \code{\link{initiate.network}}
##' and \code{\link{simulate_network.period}}. Different replacement strategies
##' are implemented: \code{\link{replace.n.highest.risk}},
##' \code{\link{replace.n.oldest}}, \code{\link{replace.n.random}}, \code{\link{replace.older.than}},
##' \code{\link{replace.more.failures.than}}, \code{\link{do.nothing}}.
##'
##' @examples
##' ## -----------
##' ## define model parameters
##' 
##' ## Define the annual probability of a failure
##' prob.failure.exp <- function(age, age.last.failure, n.failure) {
##'   if(n.failure==0){
##'     return(1/30)
##'   } else {
##'     return(1/10)
##'   }
##' }
##' 
##' ## define a complicated (and probably useless) rehabilitation strategy
##' mystrategy <- . %>%
##'   replace.n.highest.risk(n=2, prob.failure=prob.failure.exp) %>%
##'   replace.more.failures.than(failures=5) %>%
##'   replace.older.than(age=70, max.cost=2e6)  %>%
##'   replace.n.oldest(n=3) %>%
##'   replace.n.random(n=2)
##' ## This means: every year (if we have enough budget!), replace first the 2 pipes
##' ## with the highest risk, then all pipes with more than 5 failures,
##' ## then all pipes older then 70 years (up to costs of 2e6), then the 3
##' ## oldest pipes remaining, and finally replace 2 randomly selected pipes. 
##' 
##' 
##' ## -----------
##' ## run the simulation
##' 
##' result <- simulate_network(
##' 
##'     n.years = 100,                   # run it for 100 years
##'     expansion = 10,                  # build 10 pipes per year (if money is available)
##'     rehabilitation = mystrategy,     # use the strategy defined above
##'     prob.failure = prob.failure.exp, # use the probability function defined above
##'     income = 1e6,                    # the annual income
##'     initial.budget = 1e7,   
##'     initial.inventory = 50,          # start the simulation with 50 new pipes
##'     free.expansion = FALSE
##'      
##'      )          
##' 
##' ## look at some results
##' ## str(result)
##' ## str(result$time.100)
##' 
##' @author Andreas Scheidegger
##' @export
simulate_network <- function(n.years,
                             expansion,
                             rehabilitation,
                             prob.failure,
                             income=0,
                             initial.budget=Inf,
                             initial.inventory=NULL,
                             free.expansion=TRUE) {

  statelist <- initiate.network(initial.inventory, initial.budget)
  statelist <- simulate_network.period(statelist,
                                       n.years = n.years, 
                                       expansion = expansion, 
                                       rehabilitation = rehabilitation,
                                       prob.failure=prob.failure, 
                                       income = income,
                                       free.expansion = free.expansion)
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
##' str(result)      # result is a 'statelist' returned from simulate_network
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

