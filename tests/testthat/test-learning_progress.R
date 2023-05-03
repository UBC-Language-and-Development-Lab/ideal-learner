test_that("sapply works", {
  expect_equal(
    slearning_progress(factor(c(1,1), levels = 1:3)),
    c(learning_progress(factor(c(1), levels = 1:3)), learning_progress(factor(c(1,1), levels = 1:3)))
  )
})
