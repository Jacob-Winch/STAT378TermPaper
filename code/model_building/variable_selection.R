library(olsrr)

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

summary(model)

# Plots of Residuals versus the Transformed Regressors
transformed_y <- log(y)^4

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



transformed_model = lm(transformed_y ~ transformed_x1 + transformed_x2 +
                         transformed_x3  + transformed_x4 +
                         transformed_x5 + transformed_x6 +
                         transformed_x7 + transformed_x8 +
                         transformed_x9 + transformed_x10+
                         transformed_x11 + transformed_x12)

summary(transformed_model)


full_transformed_model = lm(transformed_y ~ transformed_x1 + transformed_x2 +
                              transformed_x3  + transformed_x4 +
                              transformed_x5 + transformed_x6 +
                              transformed_x7 + transformed_x8 +
                              transformed_x9 + transformed_x10+
                              transformed_x11 + transformed_x12 + 
                              transformed_x1 * transformed_x2 + 
                              transformed_x1 * transformed_x10 +
                              transformed_x3 * transformed_x10 +
                              transformed_x4 * transformed_x10 +
                              transformed_x1 * transformed_x9 + 
                              transformed_x6 * transformed_x10 + 
                              transformed_x1 * transformed_x7 + 
                              transformed_x4 * transformed_x7 +
                              transformed_x9 * transformed_x7 +
                              transformed_x10 * transformed_x7 +
                              transformed_x12 * transformed_x7 +
                              transformed_x1 * transformed_x8 +
                              transformed_x7 * transformed_x9 +
                              transformed_x7 * transformed_x5 +
                              transformed_x2 * transformed_x6 + 
                              transformed_x1 * transformed_x6
                              )
summary(full_transformed_model)

# summary(full_transformed_model)
# ols_step_forward_p(full_transformed_model)
# ols_step_backward_p(full_transformed_model)
# ols_step_both_p(full_transformed_model)
# 
# m0=lm(transformed_y~1)
# forward=step(m0, direction='forward', scope=formula(full_transformed_model), trace=1)
# forward
# anova(forward)
# summary(forward)
# 
# backward=step(full_transformed_model, direction='backward', scope=formula(full_transformed_model), trace=1)
# backward
# anova(backward)
# summary(backward)
# 
# stepwise=step(m0, direction='both', scope=formula(full_transformed_model), trace=1)
# stepwise
# anova(stepwise)
# summary(stepwise)

final_transformed_model = lm(transformed_y ~ transformed_x2 + transformed_x7 +
                               transformed_x11 + transformed_x9 + transformed_x10 +
                               transformed_x6 + transformed_x2 * transformed_x6)

anova(final_transformed_model)


                             