## -------------------------------------------------------
##
## File: test_failures.r
##
## March 24, 2017 -- Andreas Scheidegger
## andreas.scheidegger@eawag.ch
## -------------------------------------------------------

library(WaMaSim)


context("core functionality")

empty.inv <- make.empty.inventory()
test_that("empty inventory is correct", {
  expect_true( all(c("data.frame", "inventory") %in% class(empty.inv)) )
})


inv.exp <- expand(empty.inv, 10, 1)
test_that("expanded inventory is correct", {
  expect_true( all(c("data.frame", "inventory") %in% class(inv.exp)) )
  expect_equal(nrow(inv.exp), 10)
})


f.rate.test <- function(age, time.last.failure, n.failure) 1 # fails always

inv.failed <- fail(inv.exp, f.rate.test, 2)
test_that("failed inventory is correct", {
  expect_true( all(c("data.frame", "inventory") %in% class(inv.failed)) )
  expect_false( any(is.na(inv.failed$time.last.failure)) )
  expect_true( all(inv.failed$n.failure > 0) )
})


context("strategies")

inv.replaced <- replace.pipe(1, inv.failed, 3)
nn <- nrow(inv.replaced)

test_that("replace pipe", {
  expect_true( all(c("data.frame", "inventory") %in% class(inv.replaced)) )
  expect_equal(inv.replaced$replacement.value[1], inv.replaced$replacement.value[nn])
  expect_equal(inv.replaced$damage.potentia[1], inv.replaced$damage.potential[nn])
  expect_equal(inv.replaced$time.end.of.service[1], 3)
  expect_equal(inv.replaced$n.failure[nn], 0)
})
