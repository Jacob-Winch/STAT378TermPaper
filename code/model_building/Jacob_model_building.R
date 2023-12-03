library(car)

data <- read.table("data/data_estimation_random.txt", header = FALSE)

y <- data$V11
X1 <- data$V2
X2 <- data$V3
X3 <- data$V4
X4 <- data$V5
X5 <- data$V6
X6 <- data$V7
X7 <- data$V8
X8 <- data$V9
X9 <- data$V10
X10 <- ifelse(data$V12 == 1, 1, 0)
X11 <- ifelse(data$V12 == 2, 1, 0)
X12 <- ifelse(data$V12 == 3, 1, 0)

model <- lm(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12)

t <- rstudent(model)
yhat <- model$fit

transformed_y <- log(y)^2

transformed_X1 <- log(X1)
transformed_X2 <- log(X2)
transformed_X3 <- X3
transformed_X4 <- X4
transformed_X5 <- X5
transformed_X6 <- log(X6)
transformed_X7 <- X7
transformed_X8 <- log(X8)
transformed_X9 <- log(X9)
transformed_X10 <- X10
transformed_X11 <- X11
transformed_X12 <- X12

transformed_model <- lm(transformed_y ~ transformed_X1 + transformed_X2 +
                               transformed_X3  + transformed_X4 +
                               transformed_X5 + transformed_X6 +
                               transformed_X7 + transformed_X8 +
                               transformed_X9 + transformed_X10+
                               transformed_X11 + transformed_X12)

t <- rstudent(transformed_model)
yhat <- transformed_model$fit

par(mfrow=c(1,1))
qqnorm(transformed_model$resid, col="blue", main = "Normal Q-Q Plot for Transformed Model")
qqline(transformed_model$resid, col=2)

plot(transformed_model$fitted, transformed_model$resid, col="blue", xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values for Transformed Model")
abline(h=0, col="red")



