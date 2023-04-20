test_that("easy case works", {
  expect_equal(
    predictability(factor(c(1), levels = 1:3)),
    (1/2*log2(1/2) + 1/4*log2(1/4) + 1/4*log2(1/4))
    )
})
