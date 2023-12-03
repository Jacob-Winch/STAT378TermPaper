library(car)
library(agricolae)

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


# Checking Equal Variance Assumption via Bartlett's Test
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X2)
bartlett.test(transformed_model$resid, transformed_X3)
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X1)
bartlett.test(transformed_model$resid, transformed_X1)


