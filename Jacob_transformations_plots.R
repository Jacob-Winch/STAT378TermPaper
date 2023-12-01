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


# qqnorm(t, col="blue")
# qqline(t, col=2)
# 
# plot(yhat, t, col="blue", xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")
# abline(h=0, col="red")
# 
# # Plots of Residuals versus the Regressors
# 
# plot(X1, t, col = "blue", xlab = "x1", ylab = "Residuals", main = "Residuals vs x1")
# abline(h=0, col = "red")
# plot(X2, t, col = "blue", xlab = "x2", ylab = "Residuals", main = "Residuals vs x2")
# abline(h=0, col = "red")
# plot(X3, t, col = "blue", xlab = "x3", ylab = "Residuals", main = "Residuals vs x3")
# abline(h=0, col = "red")
# plot(X4, t, col = "blue", xlab = "x4", ylab = "Residuals", main = "Residuals vs x4")
# abline(h=0, col = "red")
# plot(X5, t, col = "blue", xlab = "x5", ylab = "Residuals", main = "Residuals vs x5")
# abline(h=0, col = "red")
# plot(X6, t, col = "blue", xlab = "x6", ylab = "Residuals", main = "Residuals vs x6")
# abline(h=0, col = "red")
# plot(X7, t, col = "blue", xlab = "x7", ylab = "Residuals", main = "Residuals vs x7")
# abline(h=0, col = "red")
# plot(X8, t, col = "blue", xlab = "x8", ylab = "Residuals", main = "Residuals vs x8")
# abline(h=0, col = "red")
# plot(X9, t, col = "blue", xlab = "x9", ylab = "Residuals", main = "Residuals vs x9")
# abline(h=0, col = "red")
# plot(X10, t, col = "blue", xlab = "x10", ylab = "Residuals", main = "Residuals vs x10")
# abline(h=0, col = "red")
# plot(X11, t, col = "blue", xlab = "x11", ylab = "Residuals", main = "Residuals vs x11")
# abline(h=0, col = "red")
# plot(X12, t, col = "blue", xlab = "x12", ylab = "Residuals", main = "Residuals vs x12")
# abline(h=0, col = "red")

# Plots of Residuals versus the Transformed Regressors

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

# Plot of Residuals vs Regressors

# plot(transformed_X1, t, col = "blue", xlab = "Transformed x1", ylab = "Residuals", main = "Residuals vs Transformed x1")
# abline(h=0, col = "red")
# 
# plot(transformed_X2, t, col = "blue", xlab = "Transformed x2", ylab = "Residuals", main = "Residuals vs Transformed x2")
# abline(h=0, col = "red")
# 
# plot(transformed_X3, t, col = "blue", xlab = "Transformed x3", ylab = "Residuals", main = "Residuals vs Transformed x3")
# abline(h=0, col = "red")
# 
# plot(transformed_X4, t, col = "blue", xlab = "Transformed x4", ylab = "Residuals", main = "Residuals vs Transformed x4")
# abline(h=0, col = "red")
# 
# plot(transformed_X6, t, col = "blue", xlab = "Transformed x6", ylab = "Residuals", main = "Residuals vs Transformed x6")
# abline(h=0, col = "red")
# 
# plot(transformed_X7, t, col = "blue", xlab = "Transformed x7", ylab = "Residuals", main = "Residuals vs Transformed x7")
# abline(h=0, col = "red")
# 
# plot(transformed_X8, t, col = "blue", xlab = "Transformed x8", ylab = "Residuals", main = "Residuals vs Transformed x8")
# abline(h=0, col = "red")
# 
# plot(transformed_X9, t, col = "blue", xlab = "Transformed x9", ylab = "Residuals", main = "Residuals vs Transformed x9")
# abline(h=0, col = "red")
# 
# plot(transformed_X10, t, col = "blue", xlab = "Transformed x10", ylab = "Residuals", main = "Residuals vs Transformed x10")
# abline(h=0, col = "red")
# 
# plot(transformed_X11, t, col = "blue", xlab = "Transformed x11", ylab = "Residuals", main = "Residuals vs Transformed x11")
# abline(h=0, col = "red")
# 
# plot(transformed_X12, t, col = "blue", xlab = "Transformed x12", ylab = "Residuals", main = "Residuals vs Transformed x12")
# abline(h=0, col = "red")
# 
# #avPlots(transformed_model)
# crPlots(transformed_model)
# layout(matrix(1:4,2,2))
# plot(transformed_model)

