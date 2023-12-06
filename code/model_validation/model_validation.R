library(olsrr)
library(qpcR)

data_e <- read.table("data/data_estimation_duplex.txt", header = TRUE, sep = ",")

data_p <- read.table("data/data_prediction_duplex.txt", header = TRUE, sep = ",")

y <- data_e$y
x1 <- data_e$x1
x2 <- data_e$x2
x3 <- data_e$x3
x4 <- data_e$x4
x5 <- data_e$x5
x6 <- data_e$x6
x7 <- data_e$x7
x8 <- data_e$x8
x9 <- data_e$x9
x10 <- data_e$x10
x11 <- data_e$x11
x12 <- data_e$x12

yp <- data_p$y
xp1 <- data_p$x1
xp2 <- data_p$x2
xp3 <- data_p$x3
xp4 <- data_p$x4
xp5 <- data_p$x5
xp6 <- data_p$x6
xp7 <- data_p$x7
xp8 <- data_p$x8
xp9 <- data_p$x9
xp10 <- data_p$x10
xp11 <- data_p$x11
xp12 <- data_p$x12

model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12)

summary(model)

# Plots of Residuals versus the Transformed Regressors
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

transformed_yp <- 1/log(yp)
transformed_xp1 <- 1/sqrt(xp1)
transformed_xp2 <- sqrt(xp2)
transformed_xp3 <- xp3
transformed_xp4 <- log(xp4)
transformed_xp5 <- log(xp5)
transformed_xp6 <- sqrt(xp6)
transformed_xp7 <- xp7
transformed_xp8 <- 1/log(xp8)
transformed_xp9 <- sqrt(xp9)
transformed_xp10 <- xp10
transformed_xp11 <- xp11
transformed_xp12 <- xp12



transformed_model = lm(transformed_y ~ transformed_x1 + transformed_x2 +
                         transformed_x3  + transformed_x4 +
                         transformed_x5 + transformed_x6 +
                         transformed_x7 + transformed_x8 +
                         transformed_x9 + transformed_x10+
                         transformed_x11 + transformed_x12)

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


PRESS(final_transformed_model)

data_P = data.frame(transformed_yp, transformed_xp1, transformed_xp2, transformed_xp10, 
                    transformed_xp11, transformed_xp7, transformed_xp9,
                     transformed_xp4, transformed_xp5)
data_P
final_transformed_model
p = predict(final_transformed_model, data_P)
p
Rsquared = (cor(transformed_y,p))^2

SSe = sum((transformed_y - p)^2)
SSt = sum((transformed_y - mean(transformed_y))^2)


Rsquared_prediction = 1 - (SSe/SSt)
Rsquared_prediction
