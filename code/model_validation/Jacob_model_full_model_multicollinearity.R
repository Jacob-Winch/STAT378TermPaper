library(olsrr)
library(lmtest)
library(MASS)
library(car)
data <- read.table("data/data_estimation_random.txt", header = FALSE)

y <- data$V11
x1 <- data$V2
x2 <- data$V3
x3 <- data$V4
x4 <- data$V5
x5 <- data$V6
x6 <- data$V7 
x7 <- data$V8
x8 <- data$V9
x9 <- data$V10
x10 <- ifelse(data$V12 == 1, 1, 0)
x11 <- ifelse(data$V12 == 2, 1, 0)
x12 <- ifelse(data$V12 == 3, 1, 0)

transformed_y <- 1/log(y)
transformed_x1 <- 1/sqrt(x1)
transformed_x2 <- sqrt(x2)
transformed_x3 <- x3
transformed_x4 <- log(x4)
transformed_x5 <- log(x5)
transformed_x6 <- sqrt(x6)
transformed_x7 <- x7
transformed_x8 <- 1/log(x8)
transformed_x9 <- sqrt(x9)
transformed_x10 <- x10
transformed_x11 <- x11
transformed_x12 <- x12

final_transformed_model = lm(transformed_y ~ transformed_x1 + transformed_x2 +
                               transformed_x4 + transformed_x5 + 
                               transformed_x7 + transformed_x8 +
                               transformed_x10+ transformed_x9 +
                               transformed_x11 + 
                               transformed_x10 * transformed_x2 +
                               transformed_x10 * transformed_x1 +
                               transformed_x11 * transformed_x7 +
                               transformed_x10 * transformed_x5 +
                               transformed_x10 * transformed_x4)

ols_coll_diag(final_transformed_model)

# Q-Q Plot to test for Normality
par(mfrow=c(1,1))
qqnorm(final_transformed_model$resid, col="blue", main = "Normal Q-Q Plot for Transformed Model")
qqline(final_transformed_model$resid, col=2)

# Plot of Residuals vs fitted values to test for Equal Variance
plot(final_transformed_model$fitted, final_transformed_model$resid, col="blue", xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values for Transformed Model")
abline(h=0, col="red")
