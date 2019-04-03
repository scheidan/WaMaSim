## -------------------------------------------------------
##
## File: test_failures.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------

library(WaMaSim)

## ---------------------------------
context("core functionality")

empty.inv <- make.empty.inventory()
test_that("empty inventory is correct", {
  expect_true( all(c("data.frame", "inventory") %in% class(empty.inv)) )
})

state <- list(inventory=empty.inv, budget=3000000, time=0)

state.exp <- expand(state, 10)
test_that("expanded inventory is correct", {
  expect_true( all(c("data.frame", "inventory") %in% class(state.exp$inventory)) )
  expect_equal(nrow(state.exp$inventory), 10)
})

state.exp$time <- state.exp$time + 1

prob.failure.test <- function(age, age.last.failure, n.failure) 1 # fails always

state.failed <- fail(state.exp, prob.failure.test)
test_that("failed inventory is correct", {
  expect_true( all(c("data.frame", "inventory") %in% class(state.failed$inventory)) )
  expect_false( any(is.na(state.failed$inventory$time.last.failure)) )
  expect_true( all(state.failed$inventory$n.failure > 0) )
})

## ---------------------------------
context("strategies")

inv.failed <- state.failed$inventory
inv.replaced <- replace.pipe(1, inv.failed, 3)
nn <- nrow(inv.replaced)

test_that("replace pipe", {
  expect_true( all(c("data.frame", "inventory") %in% class(inv.replaced)) )
  expect_equal(inv.replaced$replacement.value[1], inv.replaced$replacement.value[nn])
  expect_equal(inv.replaced$damage.potentia[1], inv.replaced$damage.potential[nn])
  expect_equal(inv.replaced$time.end.of.service[1], 3)
  expect_equal(inv.replaced$n.failure[nn], 0)
})


nn <- nrow(inv.failed)
state1 <- list(inventory=inv.failed, budget=Inf, time=10)
state2 <- list(inventory=inv.failed, budget=0, time=10)

test_that("replace older than", {

  s1 <- replace.older.than(state=state1, age=20)$inventory
  expect_equal(nrow(s1), nn)
  s2 <- replace.older.than(state=state1, age=5)$inventory
  expect_equal(nrow(s2), nn+nn)
  s3 <- replace.older.than(state=state2, age=5)$inventory
  expect_equal(nrow(s3), nn)
  budget <- replace.older.than(state=state2, age=5)$budget
  expect_equal(budget, 0)

})



nn <- nrow(inv.failed)
state1 <- list(inventory=inv.failed, budget=Inf, time=10)
state2 <- list(inventory=inv.failed, budget=0, time=10)

test_that("replace.more.failures.than", {

  s1 <- replace.more.failures.than(state=state1, failures=2)$inventory
  expect_equal(nrow(s1), nn)
  s2 <- replace.more.failures.than(state=state1, failures=0)$inventory
  expect_equal(nrow(s2), nn+nn)
  s3 <- replace.more.failures.than(state=state2, failures=0)$inventory
  expect_equal(nrow(s3), nn)
  budget <- replace.more.failures.than(state=state2, failures=0)$budget
  expect_equal(budget, 0)

})


nn <- nrow(inv.failed)
state1 <- list(inventory=inv.failed, budget=Inf, time=10)
state2 <- list(inventory=inv.failed, budget=0, time=10)

test_that("replace oldest", {

  s1 <- replace.n.oldest(state=state1, n=Inf)$inventory
  expect_equal(nrow(s1), nn+nn)
  s2 <- replace.n.oldest(state=state1, n=3)$inventory
  expect_equal(nrow(s2), nn+3)
  s3 <- replace.n.oldest(state=state2, n=Inf)$inventory
  expect_equal(nrow(s3), nn)
  s4 <- replace.n.oldest(state=state1, n=0)$inventory
  expect_equal(nrow(s4), nn)
  budget <- replace.n.oldest(state=state2, n=Inf)$budget
  expect_equal(budget, 0)

})


nn <- nrow(inv.failed)
state1 <- list(inventory=inv.failed, budget=Inf, time=10)
state2 <- list(inventory=inv.failed, budget=0, time=10)

test_that("replace randomly", {

  s1 <- replace.n.random(state=state1, n=Inf)$inventory
  expect_equal(nrow(s1), nn+nn)
  s2 <- replace.n.random(state=state1, n=3)$inventory
  expect_equal(nrow(s2), nn+3)
  s3 <- replace.n.random(state=state2, n=Inf)$inventory
  expect_equal(nrow(s3), nn)
  s4 <- replace.n.random(state=state1, n=0)$inventory
  expect_equal(nrow(s4), nn)
  budget <- replace.n.random(state=state2, n=Inf)$budget
  expect_equal(budget, 0)

})


nn <- nrow(inv.failed)
state1 <- list(inventory=inv.failed, budget=Inf, time=10)
state2 <- list(inventory=inv.failed, budget=0, time=10)

test_that("replace highest risk", {

  s1 <- replace.n.highest.risk(state=state1, n=Inf, prob.failure.test)$inventory
  expect_equal(nrow(s1), nn+nn)
  s2 <- replace.n.highest.risk(state=state1, n=3, prob.failure.test)$inventory
  expect_equal(nrow(s2), nn+3)
  s3 <- replace.n.highest.risk(state=state2, n=Inf, prob.failure.test)$inventory
  expect_equal(nrow(s3), nn)
  s4 <- replace.n.highest.risk(state=state1, n=0)$inventory
  expect_equal(nrow(s4), nn)
  budget <- replace.n.highest.risk(state=state2, n=Inf, prob.failure.test)$budget
  expect_equal(budget, 0)

})


## ---------------------------------
context("main")

prob.failure <- function(age, age.last.failure, n.failure) 0 # no failures

strategy <- . %>% do.nothing()

test_that("Interface", {
    expect_error(simulate_network(n.years=10,
                                  expansion=-1,# negative expansion
                                  rehabilitation = strategy,
                                  prob.failure=prob.failure,
                                  income=0,
                                  initial.budget = 1e7,
                                  initial.inventory = 20,
                                  free.expansion = TRUE))

    expect_error(simulate_network(n.years=10,
                                  expansion=c(rep(2, 9), -1), # negative expansion
                                  rehabilitation = strategy,
                                  prob.failure=prob.failure,
                                  income=0,
                                  initial.budget = 1e7,
                                  initial.inventory = 20,
                                  free.expansion = TRUE))

})
