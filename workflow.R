library(devtools)

#create_package("C:/Users/kerstin/Documents/LiU/R/Labs/BonusLab")
load_all()
document()

use_test("ridgereg")

usethis::use_vignette("BostonHousing")
devtools::build_rmd("C:/Users/kerstin/Documents/LiU/R/Labs/BonusLab/vignettes/BostonHousing.Rmd")

library(MASS)
data(iris)

#data <- iris[,c("Sepal.Length","Petal.Length","Petal.Width")]
#formula <- Sepal.Length ~ Petal.Length + Petal.Width

data <- iris
formula <- Petal.Length ~ Species

lambda <- 2
print("lambda: ");print(lambda)
#- Ridgereg
rr_res <- ridgereg(formula, data, lambda)
rr_res$coefficients
#rr_res

lmr_res <- lm.ridge(formula, data, lambda=lambda)
lmr_res

#- Predict
x <- iris[1:2,c("Petal.Length","Petal.Width")]

object <- rr_res
#y_pred <- predict.ridgereg(rr_res,x)
y_pred <- predict(rr_res,x)

# lambda=4 :
# [1] "coef:"
# [,1]
# [1,] 0.698508082
# [2,] 0.002414204
# > r_res
# Petal.Length  Petal.Width
# 4.34754332   0.39701402   0.00317787
#
#n <- nrow(data)


