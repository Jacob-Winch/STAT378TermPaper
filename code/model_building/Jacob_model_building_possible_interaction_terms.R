library(lmtest)

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

transformed_model <- lm(transformed_y ~ transformed_X1 + transformed_X2 +
                          transformed_X3  + transformed_X4 +
                          transformed_X5 + transformed_X6 +
                          transformed_X7 + transformed_X8 +
                          transformed_X9 + transformed_X10+
                          transformed_X11 + transformed_X12)

#Let's consider possible interaction terms
# Sparsity of effects principle dictates that most likely higher order
# Interactions are not typically significant 
transformed_model = lm(transformed_y ~ transformed_X1 + transformed_X2 +
                                          transformed_X3  + transformed_X4 +
                                          transformed_X5 + transformed_X6 +
                                          transformed_X7 + transformed_X8 +
                                          transformed_X9 + transformed_X10+
                                          transformed_X11 + transformed_X12)
                       

# transformed_model_geographic_interactions = lm(transformed_y ~
#                      (transformed_X1 + transformed_X2 +
#                      transformed_X3  + transformed_X4 +
#                      transformed_X5 + transformed_X6 +
#                      transformed_X7 + transformed_X8 +
#                      transformed_X9) * transformed_X10
#                      +
#                        (transformed_X1 + transformed_X2 +
#                           transformed_X3  + transformed_X4 +
#                           transformed_X5 + transformed_X6 +
#                           transformed_X7 + transformed_X8 +
#                           transformed_X9) * transformed_X11 +
#                      (transformed_X1 + transformed_X2 +
#                          transformed_X3  + transformed_X4 +
#                          transformed_X5 + transformed_X6 +
#                          transformed_X7 + transformed_X8 +
#                          transformed_X9) * transformed_X12)
# anova(transformed_model_geographic_interactions)
# summary(transformed_model_geographic_interactions)

# transformed_model_physician_interactions = lm(transformed_y ~ (transformed_X1 + transformed_X2 +
#                                                 transformed_X3  + transformed_X4 +
#                                                 transformed_X5 + transformed_X6 +
#                                                 transformed_X7 + transformed_X8 +
#                                                 transformed_X9 + transformed_X10+
#                                                 transformed_X11 + transformed_X12) * transformed_X5)
#                                                  
# anova(transformed_model_physician_interactions)

# transformed_model_hospital_beds = lm(transformed_y ~ (transformed_X1 + transformed_X2 +
#                          transformed_X3  + transformed_X4 +
#                          transformed_X5 + transformed_X6 +
#                          transformed_X7 + transformed_X8 +
#                          transformed_X9 + transformed_X10+
#                          transformed_X11 + transformed_X12) * transformed_X6)
# anova(transformed_model_hospital_beds)

# transformed_model_highschool =lm(transformed_y ~ (transformed_X1 + transformed_X2 +
#                                                         transformed_X3  + transformed_X4 +
#                                                         transformed_X5 + transformed_X6 +
#                                                         transformed_X7 + transformed_X8 +
#                                                         transformed_X9 + transformed_X10+
#                                                         transformed_X11 + transformed_X12) * transformed_X7)
# anova(transformed_model_highschool)

# transformed_model_labor =lm(transformed_y ~ (transformed_X1 + transformed_X2 +
#                                                     transformed_X3  + transformed_X4 +
#                                                     transformed_X5 + transformed_X6 +
#                                                     transformed_X7 + transformed_X8 +
#                                                     transformed_X9 + transformed_X10+
#                                                     transformed_X11 + transformed_X12) * transformed_X8)
# anova(transformed_model_labor)

