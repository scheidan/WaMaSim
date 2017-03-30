WaMaSim - Water Management Simulator
====================================

_WaMaSim_ is an R package to simulate the effect of different
rehabiliation strategies for water distribution systems. It is an
education tool used for the Water Infrastructure Experimental and
Computer Laboratory at ETH Zurich, Switzerland.


## Installation

1. Install [R](https://cloud.r-project.org/) and [R-Studio](https://www.rstudio.com/products/RStudio/) or any other editor.

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

This is a minimal example how you can run the simulation:
```R
library(WaMaSim)

## 1) Define failure rate
f.rate <- function(age, time.last.failure, n.failure) {
  if(n.failure==0){
    return(1/30)
  } else {
    return(1/10)
  }
}


## define a complicated (and probably useless) rehabilitation strategy
mystrategy <- . %>%
  replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
  replace.more.failures.than(max.failures=5) %>%
  replace.older.than(max.age=70, max.cost=2e6)  %>%
  replace.n.oldest(n=2) %>%
  replace.n.random(n=2)
## This means: every year (if we have enough budget!), replace first the 2 pipes
## with the highest risk, then all pipes with more than 5 failures,
## then all pipes older then 100 years (up to costs of 2e6), then the 3
## oldest pipes remaining, and finally replace 4 randomly selected pipes.


## run the simulation
result <- simulate(t.sim=100,                  # run it for 100 years
                   expansion=0,                # do not expand the system
                   rehabilitation=mystrategy,  # use the strategy defined above
                   failure.rate=f.rate,        # use the failure rate defined above
                   income = 1e6,               # the annual income
                   initial.budget=30e6,
                   initial.inventory=500)      # start the simulation with 50 new pipes

str(result)                                    # a list of model states
```

See the package help for more information.



## Package development

The packages `devtools`, `testthat`, and `roxygen2` are required.
To build and test this package use the follwing workflow:
```R
library(devtools)

package.path = "WaMaSim/"     # path must point to the folder containing the WaMaSim files

## simulate a new package installation
load_all(package.path)

## run tests
test(package.path)            # this runs the tests in the `test` folder of the package

## build documentation (uses Roxygen2)
document(package.path)

## run R CMD check
check(package.path)

# build_win(package.path)     # optional, test build on a online windows instance

```