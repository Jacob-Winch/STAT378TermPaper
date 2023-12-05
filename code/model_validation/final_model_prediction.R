
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


transformed_y <- log(y)^4

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


final_transformed_model = lm(transformed_y ~
                               transformed_X2 + transformed_X5 +
                               transformed_X10 + transformed_X11 +
                               transformed_X7 + transformed_X9 +
                               transformed_X6 + transformed_X8 +
                               transformed_X10 * transformed_X6)


summary(final_transformed_model)

PRESS(final_transformed_model)

transformed_y <- log(data_P$V11)^4
transformed_X1 <- log(data_P$V2)
transformed_X2 <- log(data_P$V3)
transformed_X3 <- data_P$V4
transformed_X4 <- data_P$V5
transformed_X5 <- data_P$V6
transformed_X6 <- log(data_P$V7)
transformed_X7 <- data_P$V8
transformed_X8 <- log(data_P$V9)
transformed_X9 <- log(data_P$V10)
transformed_X10 <- ifelse(data_P$V12 == 1, 1, 0)
transformed_X11 <- ifelse(data_P$V12 == 2, 1, 0)
transformed_X12 <- ifelse(data_P$V12 == 3, 1, 0)


data_P = data.frame(transformed_y, transformed_X2, transformed_X5, transformed_X10, 
                    transformed_X11, transformed_X7, transformed_X9,
                    transformed_X6, transformed_X8)
data_P
final_transformed_model
p = predict(final_transformed_model, data_P)
p
Rsquared = (cor(transformed_y,p))^2

SSe = sum((transformed_y - p)^2)
SSt = sum((transformed_y - mean(transformed_y))^2)


Rsquared_prediction = 1 - (SSe/SSt)
Rsquared_prediction
