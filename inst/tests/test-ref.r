context("ref")

test_that("it can retrieve the character representation of a reference", {
  expect_equal(as.character(ref(test)), "test")
})

test_that("the is.ref function works", {
  expect_true(is.ref(ref(test)))
})

