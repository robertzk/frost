context("freeze")

test_that("it correct computes the cross-reference of two simple lists with out = 1", {
  one <- list(x = 1, b = ref(two))
  two <- list(x = 2, b = 2)
  expect_identical(freeze(one, two), list(x = 1, b = 2))
})

test_that("it correct computes the cross-reference of two simple cross-referenced lists with out = 1", {
  one <- list(x = 1, b = ref(two))
  two <- list(x = ref(one), b = 2)
  expect_identical(freeze(one, two), list(x = 1, b = 2))
})

test_that("it errors when cross-references collide", {
  one <- list(x = ref(two))
  two <- list(x = ref(one))
  expect_error(freeze(one, two), 'points to another reference')
})

test_that("it can compute nested cross-references", {
})
