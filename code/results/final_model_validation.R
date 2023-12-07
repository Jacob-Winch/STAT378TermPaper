library(glmnet)
library(lmtest)
library(MASS)
library(car)

data <- read.table("data/Term Paper -  Fall 2023 - Data.txt", header = FALSE)

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

model = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12)
bc = boxcox(model)
alpha = bc$x[which.max(bc$y)]

transformed_y <- (y^(alpha - 1)/alpha)

transformed_x1 <- log(x1)
transformed_x2 <- log(x2)
transformed_x3 <- x3
transformed_x4 <- x4
transformed_x5 <- x5
transformed_x6 <- log(x6)
transformed_x7 <- x7
transformed_x8 <- log(x8)
transformed_x9 <- log(x9)
transformed_x10 <- x10
transformed_x11 <- x11
transformed_x12 <- x12

full_final_transformed_model = lm(transformed_y ~ transformed_x2 + transformed_x7 +
                                    transformed_x11 + transformed_x9 + transformed_x10 +
                                    transformed_x6 + transformed_x2 * transformed_x6)

t <- rstudent(full_final_transformed_model)
yhat <- full_final_transformed_model$fit


qqnorm(t, col="blue")
qqline(t, col=2)
# 
# plot(yhat, t, col="blue", xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")
# abline(h=0, col="red")
# 
# plot(transformed_x2, t, col = "blue", xlab = "Transformed x2", ylab = "Residuals", main = "Residuals vs Transformed x2")
# abline(h=0, col = "red")
# 
# plot(transformed_x6, t, col = "blue", xlab = "Transformed x6", ylab = "Residuals", main = "Residuals vs Transformed x6")
# abline(h=0, col = "red")
# 
# plot(transformed_x7, t, col = "blue", xlab = "Transformed x7", ylab = "Residuals", main = "Residuals vs Transformed x7")
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
# 
# # Breusch-Pagan test for constant variance assumption. 
# bptest(full_final_transformed_model)

shapiro.test(transformed_y)
shapiro.test(1/log(y))
shapiro.test(1/sqrt(y))





