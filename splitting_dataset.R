data <- read.table("/Users/winch/STAT378/STAT378TermPaper/Term Paper -  Fall 2023 - Data.txt", header = FALSE)

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

s1=(length(X1)-1)*var(X1)
s2=(length(X2)-1)*var(X2)
s3=(length(X3)-1)*var(X3)
s4=(length(X4)-1)*var(X4)
s5=(length(X5)-1)*var(X5)
s6=(length(X6)-1)*var(X6)
s7=(length(X7)-1)*var(X7)
s8=(length(X8)-1)*var(X8)
s9=(length(X9)-1)*var(X9)
s10=(length(X10)-1)*var(X10)
s11=(length(X11)-1)*var(X11)
s12=(length(X12)-1)*var(X12)

Z=cbind(X1-mean(X1)/sqrt(s1), 
        X2-mean(X2)/sqrt(s2),
        X3-mean(X3)/sqrt(s3), 
        X4-mean(X4)/sqrt(s4), 
        X5-mean(X5)/sqrt(s5),
        X6-mean(X6)/sqrt(s6), 
        X7-mean(X7)/sqrt(s7),
        X8-mean(X8)/sqrt(s8),
        X9-mean(X9)/sqrt(s9),
        X10-mean(X10)/sqrt(s10),
        X11-mean(X11)/sqrt(s11),
        X12-mean(X12)/sqrt(s12)
        )

T = chol(t(Z)%*%Z)

W = Z%*%solve(T)

distances = dist(W)

sorted_distances = sort(distances, decreasing = TRUE)







