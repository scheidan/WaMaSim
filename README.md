WaMaSim - Water Management Simulator
====================================

_WaMaSim_ is an R package that simulates the effect of different
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

This is a minimal example of how you can run the simulation:
```R
library(WaMaSim)

## 1) Define the failure rate
f.rate <- function(age, time.last.failure, n.failure) {
  if(n.failure==0){
    return(1/30)
  } else {
    return(1/10)
  }
}


## 2) Define a complicated (and probably useless) rehabilitation strategy
mystrategy <- . %>%
  replace.n.highest.risk(n=2, failure.rate=f.rate) %>%
  replace.more.failures.than(max.failures=5) %>%
  replace.older.than(max.age=100, max.cost=2e6)  %>%
  replace.n.oldest(n=3) %>%
  replace.n.random(n=4)
## This defines a prioritized sequence of annual rehabilitation steps as follows: 
## each year, and as long as there is enough budget, replace first the 2 pipes 
## with the highest risk of failure, then all pipes with more than 5 failures,
## then all pipes more than 100 years old, then the 3 oldest remaining pipes, and
## finally replace 4 randomly selected pipes. Additionally, spendings on the 
## rehabilitation strategy replace.older.than can not exceed a maximum 
## budget of 2,000,000 CHF.

## Or, define a "do nothing" rehabilitation strategy (i.e. repairs only, no pipe replacement)
# mystrategy <- . %>% do.nothing


## 3) Run the simulation
result <- simulate(t.sim=100,                  # run it for 100 years
                   expansion=0,                # do not expand the system
                   rehabilitation=mystrategy,  # use the strategy defined above
                   failure.rate=f.rate,        # use the failure rate defined above
                   income=1e6,                 # the annual income
                   initial.budget=30e6,        # the initial budget
                   initial.inventory=500)      # start the simulation with 500 new pipes

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

# build_win(package.path)     # optional, test build on an online windows instance

```