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

p1 = predict(model)
r1 = rstudent(model)
plot(p1, r1, main = "Scatterplot of R-student residuals vs predicted values", col = 2)

bins = cut(r1, seq(-7.5,6.75, 0.5), right=FALSE)
r1.mean = tapply(r1, bins, mean)
r1.var  = tapply(r1, bins, var)

plot(r1.mean, r1.var, col = "blue", pch = 16)
