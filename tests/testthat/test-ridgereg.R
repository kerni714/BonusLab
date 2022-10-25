data(iris)
data <- iris
formula <- Sepal.Length ~ Petal.Length + Petal.Width

lambda <- 2
ridgereg_res <- ridgereg(formula, data, lambda)
lmridge_res <- MASS::lm.ridge(formula, data, lambda=lambda)

names(ridgereg_res$coefficients)[1] <- ""

test_that("ridgereg works", {
  expect_equal(round(ridgereg_res$coefficients,2),round(coef(lmridge_res),2))
})
