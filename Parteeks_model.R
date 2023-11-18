library(MASS)
library("car")
data <- read.table("data_estimation.txt", header = FALSE)
summary(data)

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



m1 <- lm(y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12)

layout(matrix(1:4,2,2))
plot(m1)
qqnorm(m1$resid, col="blue")
qqline(m1$resid, col=2) 
plot(m1$fitted, m1$resid, col="blue")
abline(h=0, col="red")


p1=predict(m1)
r1= rstudent(m1)
r1
summary(r1)
bins=cut(r1, seq(-7.5,7,0.5), right=FALSE) 
r1.mean = tapply(r1,bins,mean)
r1.var =tapply(r1,bins, var)
plot(r1.mean, r1.var, col="blue", pch=16) 


ln_y = log(y)

ln_x1 = log(X1)
ln_x3 = sqrt(X3)
ln_x8 = log(X8)

sqrt_x5 = sqrt(X5)
sqrt_x6 = 1/sqrt(X6)
m2 <- lm(ln_y~ln_x1+X2+X3+X4+sqrt_x5+sqrt_x6+X7+ln_x8+X9+X10+X11+X12+I(X7^(2))+I(X3^(2))+I(ln_x1^(2))+I(X4^(2))+I(ln_x8^(2)))
crPlots(m2)

plot(m2)
qqnorm(m2$resid, col="blue")
qqline(m2$resid, col=2)
plot(m2$fitted, m2$resid, col="blue")
abline(h=0, col="red")

avPlots(m2)

crPlots(m2)

m0=lm(y~1)
forward=step(m0, direction='forward', scope=formula(m2), trace=1)

modelfinal = lm(ln_y ~ X9 + X10 + X11 + I(ln_x8^(2)) + ln_x8 + sqrt_x5 + X12)
plot(modelfinal)
qqnorm(modelfinal$resid, col="blue")
qqline(modelfinal$resid, col=2)
plot(modelfinal$fitted, modelfinal$resid, col="blue")
abline(h=0, col="red")

anova(modelfinal)
