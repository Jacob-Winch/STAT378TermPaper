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


transformed_model_interactions_x10 = lm(transformed_y ~ (transformed_x1 + transformed_x2 +
                         transformed_x3  + transformed_x4 +
                         transformed_x5 + transformed_x6 +
                         transformed_x7 + transformed_x8 +
                         transformed_x9 + transformed_x11+
                         transformed_x12)*transformed_x10)

transformed_model_interactions_x11 = lm(transformed_y ~ (transformed_x1 + transformed_x2 +
                                                           transformed_x3  + transformed_x4 +
                                                           transformed_x5 + transformed_x6 +
                                                           transformed_x7 + transformed_x8 +
                                                           transformed_x9 + transformed_x10+
                                                           transformed_x12)*transformed_x11)


transformed_model_interactions_x12 = lm(transformed_y ~ (transformed_x1 + transformed_x2 +
                                                           transformed_x3  + transformed_x4 +
                                                           transformed_x5 + transformed_x6 +
                                                           transformed_x7 + transformed_x8 +
                                                           transformed_x9 + transformed_x10+
                                                           transformed_x11)*transformed_x12)


anova(transformed_model_interactions_x10)
anova(transformed_model_interactions_x11)
anova(transformed_model_interactions_x12)

# full_transformed_model = lm(transformed_y ~ transformed_x1 + transformed_x2 +
#                          transformed_x3  + transformed_x4 +
#                          transformed_x5 + transformed_x6 +
#                          transformed_x7 + transformed_x8 +
#                          transformed_x9 + transformed_x10+
#                          transformed_x11 + transformed_x12+
#                            transformed_x5*transformed_x12 +
#                            transformed_x7*transformed_x11 +
#                            transformed_x1*transformed_x10 +
#                            transformed_x2*transformed_x10 + 
#                            transformed_x3*transformed_x10 +
#                            transformed_x4*transformed_x10 +
#                            transformed_x5*transformed_x10
#                            )