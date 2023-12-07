library(lmtest)
library(MASS)
library(car)

data <- read.table("data/data_estimation_duplex.txt", header = TRUE, sep = ",")

y <- data$y
x1 <- data$x1
x2 <- data$x2
x3 <- data$x3
x4 <- data$x4
x5 <- data$x5
x6 <- data$x6
x7 <- data$x7
x8 <- data$x8
x9 <- data$x9
x10 <- data$x10
x11 <- data$x11
x12 <- data$x12

model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12)

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

transformed_model = lm(transformed_y ~ transformed_x1 + transformed_x2 +
                         transformed_x3  + transformed_x4 +
                         transformed_x5 + transformed_x6 +
                         transformed_x7 + transformed_x8 +
                         transformed_x9 + transformed_x10+
                         transformed_x11 + transformed_x12)

summary(transformed_model)

t <- rstudent(transformed_model)
yhat <- transformed_model$fit

# Q-Q Plot to test for Normality
par(mfrow=c(1,1))
windows()
# qqnorm(transformed_model$resid, col="blue", main = "Normal Q-Q Plot for Transformed Model")
# qqline(transformed_model$resid, col=2)

# Plot of Residuals vs fitted values to test for Equal Variance
plot(transformed_model$fitted, transformed_model$resid, col="blue", xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values for Transformed Model")
abline(h=0, col="red")

# Plot of Residuals vs Regressors

# plot(transformed_x1, t, col = "blue", xlab = "Transformed x1", ylab = "Residuals", main = "Residuals vs Transformed x1")
# abline(h=0, col = "red")
# 
# plot(transformed_x2, t, col = "blue", xlab = "Transformed x2", ylab = "Residuals", main = "Residuals vs Transformed x2")
# abline(h=0, col = "red")
# 
#plot(transformed_x3, t, col = "blue", xlab = "Transformed x3", ylab = "Residuals", main = "Residuals vs Transformed x3")
#abline(h=0, col = "red")
# 
# plot(transformed_x4, t, col = "blue", xlab = "Transformed x4", ylab = "Residuals", main = "Residuals vs Transformed x4")
# abline(h=0, col = "red")
# 
# plot(transformed_x5, t, col = "blue", xlab = "Transformed x6", ylab = "Residuals", main = "Residuals vs Transformed x5")
# abline(h=0, col = "red")
# 
# plot(transformed_x6, t, col = "blue", xlab = "Transformed x6", ylab = "Residuals", main = "Residuals vs Transformed x6")
# abline(h=0, col = "red")
# 
# plot(transformed_x7, t, col = "blue", xlab = "Transformed x7", ylab = "Residuals", main = "Residuals vs Transformed x7")
# abline(h=0, col = "red")
# 
# plot(transformed_x8, t, col = "blue", xlab = "Transformed x8", ylab = "Residuals", main = "Residuals vs Transformed x8")
# abline(h=0, col = "red")
# 
# plot(transformed_x9, t, col = "blue", xlab = "Transformed x9", ylab = "Residuals", main = "Residuals vs Transformed x9")
# abline(h=0, col = "red")
# 
# plot(transformed_x10, t, col = "blue", xlab = "Transformed x10", ylab = "Residuals", main = "Residuals vs Transformed x10")
# abline(h=0, col = "red")
# 
# plot(transformed_x11, t, col = "blue", xlab = "Transformed x11", ylab = "Residuals", main = "Residuals vs Transformed x11")
# abline(h=0, col = "red")
# 
# plot(transformed_x12, t, col = "blue", xlab = "Transformed x12", ylab = "Residuals", main = "Residuals vs Transformed x12")
# abline(h=0, col = "red")
# 
#avPlots(transformed_model)
#crPlots(transformed_model)
#layout(matrix(1:4,2,2))
# plot(transformed_model)

#Shapiro-Wilk test for normality assumption 
shapiro.test(transformed_y)

# Breusch-Pagan test for constant variance assumption. 
bptest(transformed_model)


