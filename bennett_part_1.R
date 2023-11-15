# read data
file_name <- "Term Paper -  Fall 2023 - Data.txt"
data <- read.table(file_name, header = FALSE)
data

# add column names
col_names <- c("Identification number", "Land Area", "Total population", "Percent of population in central cities", 
               "Percent of population 65 or older", "Number of active physicians", "Number of hospital beds",
               "Percent high school graduates", "Civilian labor force", "Total personal income", "Total serious crimes",
               "Geographic region")
colnames(data) <- col_names
data


# correct geographic region
data$"Geographic region" <- ifelse(data$"Geographic region" == "1", "NE",
                                ifelse(data$"Geographic region" == "2", "NC",
                                       ifelse(data$"Geographic region" == "3", "S",
                                              ifelse(data$"Geographic region" == "4", "W", 
                                                     NA))))
data


# split into train/test (placeholder for duplex algorithm)
set.seed(10)
test_indices <- sample(nrow(data), 25)
test_data <- data[test_indices, ]
train_data <- data[-test_indices, ]
train_data
