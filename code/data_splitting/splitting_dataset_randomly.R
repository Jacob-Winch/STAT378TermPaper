data <- read.table("/Users/winch/STAT378/STAT378TermPaper/data/Term Paper -  Fall 2023 - Data.txt", header = FALSE)

set.seed(10)
row = sample(nrow(data), size = 25)
data_p = data[row,]
data_e = data[-row,]

write.table(data_e, file = "data_estimation.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
write.table(data_p, file = "data_prediction.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

