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

model = lm(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12)

par(mfrow=c(4,2))

qqnorm(model$resid, col="blue")
qqline(model$resid, col=2)
plot(model$fitted, model$resid, col="blue")
abline(h=0, col="red") 

new_y = log(y)

new_X1 = log(X1)
new_X1_square = new_X1^2
new_X2 = log(X2)
new_X3 = sqrt(X3)
new_X3_square = X3^2
new_X4 = X4
new_X4_square = X4^2
new_X5 = sqrt(X5)
new_X6 = log(X6)
new_x6_square = new_X6^2
new_X7 = X7
new_X7_square = X7^2
new_X8 = log(X8)
new_X9 = log(X9)
new_X10 = X10
new_X11 = X11
new_X12 = X12

transformed_model = lm(new_y ~ new_X1 + new_X1_square + new_X2 + 
                               new_X3 + new_X3_square + new_X4 + new_X4_square +
                               new_X5 + new_X6 + 
                               new_X7 + new_X7_square + new_X8 + 
                               new_X9 + new_X10+ new_x6_square +
                               new_X11 + new_X12)

qqnorm(transformed_model$resid, col="blue")
qqline(transformed_model$resid, col=2)
plot(transformed_model$fitted, transformed_model$resid, col="blue")
abline(h=0, col="red")

crPlots(transformed_model)



