library(olsrr)


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

model <- lm(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12)
# summary(model)
t <- rstudent(model)
yhat <- model$fit

# Plots of Residuals versus the Transformed Regressors
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



transformed_model = lm(transformed_y ~ transformed_X1 + transformed_X2 +
                                          transformed_X3  + transformed_X4 +
                                          transformed_X5 + transformed_X6 +
                                          transformed_X7 + transformed_X8 +
                                          transformed_X9 + transformed_X10+
                                          transformed_X11 + transformed_X12)
# summary(transformed_model)

full_transformed_model = lm(transformed_y ~ transformed_X1 + transformed_X2 +
                              transformed_X3  + transformed_X4 +
                              transformed_X5 + transformed_X6 +
                              transformed_X7 + transformed_X8 +
                              transformed_X9 + transformed_X10+
                              transformed_X11 + transformed_X12 + 
                              transformed_X1 * transformed_X2 + 
                              transformed_X1 * transformed_X10 +
                              transformed_X3 * transformed_X10 +
                              transformed_X4 * transformed_X10 +
                              transformed_X1 * transformed_X9 + 
                              transformed_X6 * transformed_X10 + 
                              transformed_X1 * transformed_X7 + 
                              transformed_X6 * transformed_X7 +
                              transformed_X9 * transformed_X7 +
                              transformed_X10 * transformed_X7 +
                              transformed_X12 * transformed_X7 +
                              transformed_X1 * transformed_X8)
# summary(full_transformed_model)
# ols_step_forward_p(full_transformed_model)
# ols_step_backward_p(full_transformed_model)
# ols_step_both_p(full_transformed_model)

# m0=lm(transformed_y~1)
# forward=step(m0, direction='forward', scope=formula(full_transformed_model), trace=1)
# forward
# anova(forward)
# summary(forward)

# backward=step(full_transformed_model, direction='backward', scope=formula(full_transformed_model), trace=1)
# backward
# anova(backward)
# summary(backward)

# stepwise=step(m0, direction='both', scope=formula(full_transformed_model), trace=1)
# stepwise
# anova(stepwise)
# summary(stepwise)

k <- ols_step_all_possible(full_transformed_model)
k

final_transformed_model = lm(transformed_y ~
                               transformed_X2 + transformed_X5 +
                               transformed_X10 + transformed_X11 +
                               transformed_X7 + transformed_X9 +
                               transformed_X6 + transformed_X8 +
                               transformed_X10 * transformed_X6)
anova(final_transformed_model)
summary(final_transformed_model)


