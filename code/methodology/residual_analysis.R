library(car)

data <- read.table("data/data_estimation_duplex.txt", header = TRUE, sep = ",")

y <- data$y
X1 <- data$x1
X2 <- data$x2
X3 <- data$x3
X4 <- data$x4
X5 <- data$x5
X6 <- data$x6
X7 <- data$x7
X8 <- data$x8
X9 <- data$x9
X10 <- data$x10
X11 <- data$x11
X12 <- data$x12

model <- lm(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12)

summary(model)

t <- rstudent(model)
yhat <- model$fit



# qqnorm(t, col="blue")
# qqline(t, col=2)

# plot(yhat, t, col="blue", xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")
# abline(h=0, col="red")
 
#windows()
par(mfrow=c(2,2))

# Plots of Residuals versus the Regressors
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

# Partial Regression Plots 
avPlots(model)
# 
# # Partial Residual Plots
# crPlots(model)




