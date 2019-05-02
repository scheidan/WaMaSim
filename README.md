WaMaSim - Water Management Simulator
====================================

_WaMaSim_ is an R package that simulates the effect of different
rehabilitation strategies for water distribution systems. It is an
education tool used for the Water Infrastructure Experimental and
Computer Laboratory at ETH Zurich, Switzerland.


## Installation

1. Install [R](https://cran.r-project.org) and [R-Studio](https://www.rstudio.com/products/RStudio/) or any other editor.

2. Install `devtools` (type in the R command line)
```
install.packages("devtools")
```

3. Install WaMaSim (type in the R command line)
```
library(devtools)
install_github("scheidan/WaMaSim")
```


## Usage

This is a minimal example of how you can run the simulation:
```R
library(WaMaSim)

## -----------
## define model parameters

## Define the annual probability of a failure
prob.failure.exp <- function(age, age.last.failure, n.failure) {
  if(n.failure==0){
    return(1/30)
  } else {
    return(1/10)
  }
}

## define a complicated (and probably useless) rehabilitation strategy
mystrategy <- . %>%
  replace.n.highest.risk(n=2, prob.failure=prob.failure.exp) %>%
  replace.more.failures.than(failures=5) %>%
  replace.older.than(age=70, max.cost=2e6)  %>%
  replace.n.oldest(n=3) %>%
  replace.n.random(n=2)
## This means: every year (if we have enough budget!), replace first the 2 pipes
## with the highest risk, then all pipes with more than 5 failures,
## then all pipes older then 70 years (up to costs of 2e6), then the 3
## oldest pipes remaining, and finally replace 2 randomly selected pipes.


## -----------
## run the simulation

result <- simulate_network(n.years = 100,                   # run it for 100 years
                           expansion = 10,                  # build 10 pipes per year (if money is available)
                           rehabilitation = mystrategy,     # use the strategy defined above
                           prob.failure = prob.failure.exp, # use the probability function defined above
                           income = 1e6,                    # the annual income
                           initial.budget = 1e7,
                           initial.inventory = 50,          # start the simulation with 50 new pipes
                           free.expansion = FALSE)          #

## look at some results
str(result)
str(result$time.100)


## -----------
## plots

par(mfrow=c(3,1))

## budget
plot(result$time, result$budget, type="l")
abline(h=0, col=2)

## pipes built per year
plot(result$time, pipes.built.per.year(result))

## annual costs
plot(result$time, costs.per.year(result, income=1e6))
```

See the package help for more information.



## Package development

The packages `devtools`, `testthat`, and `roxygen2` are required.
To build and test this package use the following workflow:
```R
library(devtools)

package.path = "WaMaSim/"     # path must point to the folder containing the WaMaSim files

## simulate a new package installation
load_all(package.path)

## run tests
test(package.path)            # this runs the tests in the `test` folder of the package
test_coverage(package.path)       # needs package 'covr' to be installed

## build documentation (uses Roxygen2)
document(package.path)

## run R CMD checks
check(package.path, cran=TRUE, manual=TRUE)

## build package for CRAN submission
build(pkg=package.path)

```
