## -------------------------------------------------------
##
## File: test_failures.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------

library(WaMaSim)


context("failures model")

empty.inv <- make.empty.inventory()
test_that("empty inventory is correct", {
  expect_true( all(c("data.frame", "inventory") %in% class(empty.inv)) )
})


inv.exp <- expand(empty.inv, 10, 1)
test_that("expanded inventory is correct", {
  expect_true( all(c("data.frame", "inventory") %in% class(inv.exp)) )
  expect_equal(nrow(inv.exp), 10)
})


f.rate.test <- function(age, time.last.failure, n.failure) 1   # fails always
inv.failed <- fail(inv.exp, f.rate, 2)
test_that("failed inventory is correct", {
  expect_true( all(c("data.frame", "inventory") %in% class(inv.failed)) )
  expect_false( any(is.na(inv.failed$time.last.failure)) )
  expect_true( all(inv.failed$n.failure > 0) )
})
