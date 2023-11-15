data <- read.table("/Users/winch/STAT378/STAT378TermPaper/Term Paper -  Fall 2023 - Data.txt", header = FALSE)

y <- data$V11
x1 <- data$V2
x2 <- data$V3
x3 <- data$V4
x4 <- data$V5
x5 <- data$V6
x6 <- data$V7
x7 <- data$v8
x8 <- data$V9
x9 <- data$v10
x10 <- data$v12

s1=(length(x1)-1)*var(x1)
s2=(length(x2)-1)*var(x2)
s3=(length(x3)-1)*var(x3)
s4=(length(x4)-1)*var(x4)
s5=(length(x5)-1)*var(x5)
s6=(length(x6)-1)*var(x6)
s7=(length(x7)-1)*var(x7)
s8=(length(x8)-1)*var(x8)
s9=(length(x9)-1)*var(x9)
s10=(length(x10)-1)*var(x10)

Z=cbind(x1-mean(x1)/sqrt(s1), 
        x2-mean(x2)/sqrt(s2),
        x2-mean(x3)/sqrt(s3), 
        x4-mean(x4)/sqrt(s4), 
        x5-mean(x5)/sqrt(s5),
        x6-mean(x6)/sqrt(s6), 
        x7-mean(x7)/sqrt(s7),
        x8-mean(x8)/sqrt(s8),
        x9-mean(x9)/sqrt(s9),
        x10-mean(x10)/sqrt(s10)
        )

T = chol(t(Z)%*%Z)

W = Z%*%solve(T)

distances = dist(W)

sorted_distances = sort(distances, decreasing = TRUE)