test_that("single element works", {
  expect_equal(dir_prob(factor(c(1), levels = 1:3)), 1/3)
  expect_equal(dir_prob(factor(c(1), levels = 1:4)), 1/4)
})
test_that("probabilities sum to 1", {
  expect_equal(
    sum(sapply(1:3, \(x) dir_prob(factor(c(x), levels = 1:3)))),
    1)
  expect_equal(
    sum(sapply(1:4, \(x) dir_prob(factor(c(x), levels = 1:4)))),
    1)
})
