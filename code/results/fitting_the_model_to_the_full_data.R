library(olsrr)
library(glmnet)

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

final_transformed_model