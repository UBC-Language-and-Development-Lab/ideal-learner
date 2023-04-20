test_that("surprise equals negative log2", {
  expect_equal(surprise(factor(c(1), levels = 1:3)), -log2(1/3))
  expect_equal(surprise(factor(c(1), levels = 1:2)), -log2(1/2))
})
