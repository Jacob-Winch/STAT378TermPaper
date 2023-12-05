
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
X10 <- ifelse(data$V12 == 1, 1, 0)
X11 <- ifelse(data$V12 == 2, 1, 0)
X12 <- ifelse(data$V12 == 3, 1, 0)

model <- lm(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12)

summary(model)


