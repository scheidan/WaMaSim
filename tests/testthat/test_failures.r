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


nn <- nrow(inv.failed)
state1 <- list(inventory=inv.failed, budget=30000, time=10)
state2 <- list(inventory=inv.failed, budget=2000, time=10)

test_that("replace older than", {

  s1 <- replace.older.than(state=state1, max.age=20)$inventory
  expect_equal(nrow(s1), nn)
  s2 <- replace.older.than(state=state1, max.age=5)$inventory
  expect_equal(nrow(s2), nn+nn)
  s3 <- replace.older.than(state=state2, max.age=5)$inventory
  expect_equal(nrow(s3), nn+2)
  budget <- replace.older.than(state=state2, max.age=5)$budget
  expect_equal(budget, 0)

})



nn <- nrow(inv.failed)
state1 <- list(inventory=inv.failed, budget=30000, time=10)
state2 <- list(inventory=inv.failed, budget=2000, time=10)

test_that("replace.more.failures.than", {

  s1 <- replace.more.failures.than(state=state1, max.failures=2)$inventory
  expect_equal(nrow(s1), nn)
  s2 <- replace.more.failures.than(state=state1, max.failures=0)$inventory
  expect_equal(nrow(s2), nn+nn)
  s3 <- replace.more.failures.than(state=state2, max.failures=0)$inventory
  expect_equal(nrow(s3), nn+2)
  budget <- replace.more.failures.than(state=state2, max.failures=0)$budget
  expect_equal(budget, 0)

})



nn <- nrow(inv.failed)
state1 <- list(inventory=inv.failed, budget=30000, time=10)
state2 <- list(inventory=inv.failed, budget=2000, time=10)

test_that("replace oldest", {

  s1 <- replace.n.oldest(state=state1, n=Inf)$inventory
  expect_equal(nrow(s1), nn+nn)
  s2 <- replace.n.oldest(state=state1, n=3)$inventory
  expect_equal(nrow(s2), nn+3)
  s3 <- replace.n.oldest(state=state2, n=Inf)$inventory
  expect_equal(nrow(s3), nn+2)
  budget <- replace.n.oldest(state=state2, n=Inf)$budget
  expect_equal(budget, 0)

})


nn <- nrow(inv.failed)
state1 <- list(inventory=inv.failed, budget=30000, time=10)
state2 <- list(inventory=inv.failed, budget=2000, time=10)

test_that("replace randomly", {

  s1 <- replace.n.random(state=state1, n=Inf)$inventory
  expect_equal(nrow(s1), nn+nn)
  s2 <- replace.n.random(state=state1, n=3)$inventory
  expect_equal(nrow(s2), nn+3)
  s3 <- replace.n.random(state=state2, n=Inf)$inventory
  expect_equal(nrow(s3), nn+2)
  budget <- replace.n.random(state=state2, n=Inf)$budget
  expect_equal(budget, 0)

})
