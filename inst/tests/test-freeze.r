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

test_that("it can compute nested references", {
  one <- list(x = 1, y = list(z = list(w = ref(two)), v = 4), s = 3)
  two <- list(y = list(z = list(w = 1)))
  expect_identical(freeze(one, two),
    list(x = 1, y = list(z = list(w = 1), v = 4), s = 3))
  
  # test that function objects can be referenced as well
  one <- list(x = 1, y = list(z = list(w = ref(two)), v = 4), s = 3)
  two <- list(y = list(z = list(w = tmp <- function(x) x)))
  expect_identical(freeze(one, two),
    list(x = 1, y = list(z = list(w = tmp), v = 4), s = 3))
})

test_that("it can compute nested cross-references", {
  one <- list(x = list(1, 2), y = list(z = list(w = ref(two)), v = ref(two)), s = 3)
  two <- list(x = ref(one), y = list(z = list(w = 5), v = 3), s = ref(one))
  expect_identical(freeze(one, two), 
    out <- list(x = list(1, 2), y = list(z = list(w = 5), v = 3), s = 3))
  expect_identical(freeze(two, one), out)
})

test_that("it can compute references from multiple reference lists", {
  one <- list(x = ref(two), y = ref(three), z = ref(four))
  two <- list(x = 1); three <- list(y = 2); four <- list(z = 3)
  expect_identical(freeze(one, two, three, four), list(x = 1, y = 2, z = 3))
})

test_that("it can compute cross-references from multiple reference lists", {
  one <- list(x = ref(two), y = 2, z = ref(four))
  two <- list(x = 1); three <- list(y = ref(one)); four <- list(z = 3)
  frozens <- freeze(one, two, three, four, out = c(1, 3))
  expect_identical(frozens$one, list(x = 1, y = 2, z = 3))
  expect_identical(frozens$three, list(y = 2))
})

test_that("it errors on circular references", {
  one <- list(x = ref(two))
  two <- list(x = ref(one))
  expect_error(freeze(one, two), 'circularly points to another reference')
})

test_that("it can resolve a simple reference chain", {
  one <- list(x = ref(two))
  two <- list(x = ref(three))
  three <- list(x = 1)
  expect_identical(freeze(one, two, three), three)
})

test_that("it can resolve a complicated reference chain", {
  one <- list(a = ref(two), b = 2, y = list(ref(three), 1), c = list(a = ref(two)))
  two <- list(a = 3, y = list(ref(three), 2), c = list(b = 1, a = ref(three)))
  three <- list(z = 5, y = list(2), a = ref(one), c = list(c = 1, a = 3))
  frozens <- freeze(one, two, three, out = seq_len(3))
  expect_identical(frozens$one, list(a = 3, b = 2, y = list(2, 1), c = list(a =  3)))
  expect_identical(frozens$two, list(a = 3, y = list(2, 2), c = list(b = 1, a = 3)))
  expect_identical(frozens$three, list(z = 5, y = list(2), a = 3, c = list(c = 1, a = 3)))
})

test_that("it can detect a complicated circular reference chain", {
  one <- list(a = 1, b = list(ref(two), 2), c = 3)
  two <- list(a = 5, b = list(ref(three), 2), c = 3)
  three <- list(a = 5, b =  list(ref(one)), c = 4)
  expect_error(freeze(one, two, three), 'circularly points to another reference')
})

