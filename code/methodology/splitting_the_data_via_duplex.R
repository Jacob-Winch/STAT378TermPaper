


remove_elements_from_vector <- function(elements_to_remove, vector){
  
  for (i in 1:length(elements_to_remove)) {
    vector <- vector[vector != elements_to_remove[i]]
  }
  return(vector)
}

add_point_to_points <- function(point, points){
  return(c(points, point))
}

update_rounded_sorted_distance <- function(rounded_sorted_distance, point_str, rounded_distance_matrix){
  return(remove_elements_from_vector(rounded_distance_matrix[colnames(rounded_distance_matrix) == point_str], rounded_sorted_distance))
}

update_rounded_distance_matrix <- function(rounded_distance_matrix, point_str){
  return(rounded_distance_matrix[, colnames(rounded_distance_matrix) != point_str])
}


data <- read.table("data/Term Paper -  Fall 2023 - Data.txt", header = FALSE)


y <- data$V11
x1 <- data$V2
x2 <- data$V3
x3 <- data$V4
x4 <- data$V5
x5 <- data$V6
x6 <- data$V7
x7 <- data$V8
x8 <- data$V9
x9 <- data$V10
x10 <- ifelse(data$V12 == 1, 1, 0)
x11 <- ifelse(data$V12 == 2, 1, 0)
x12 <- ifelse(data$V12 == 3, 1, 0)

s11 = (length(x1)-1)*var(x1)
s22 = (length(x2)-1)*var(x2)
s33 = (length(x3)-1)*var(x3)
s44 = (length(x4)-1)*var(x4)
s55 = (length(x5)-1)*var(x5)
s66 = (length(x6)-1)*var(x6)
s77 = (length(x7)-1)*var(x7)
s88 = (length(x8)-1)*var(x8)
s99 = (length(x9)-1)*var(x9)
s1010 = (length(x10)-1)*var(x10)
s1111 = (length(x11)-1)*var(x11)
s1212 = (length(x12)-1)*var(x12)

Z = cbind((x1-mean(x1))/sqrt(s11), 
          (x2-mean(x2))/sqrt(s22),
          (x3-mean(x3))/sqrt(s33),
          (x4-mean(x4))/sqrt(s44),
          (x5-mean(x5))/sqrt(s55),
          (x6-mean(x6))/sqrt(s66),
          (x7-mean(x7))/sqrt(s77),
          (x8-mean(x8))/sqrt(s88),
          (x9-mean(x9))/sqrt(s99),
          (x10-mean(x10))/sqrt(s1010),
          (x11-mean(x11))/sqrt(s1111),
          (x12-mean(x12))/sqrt(s1212)
)


T = chol(t(Z)%*%Z)

W = Z%*%solve(T)

distance = dist(W)

sorted_distance = sort(distance, decreasing = TRUE)

distance_matrix = as.matrix(distance)

rounded_distance_matrix = apply(distance_matrix, MARGIN = c(1, 2), FUN = function(x) round(x, digits = 6))

rounded_sorted_distance = round(sorted_distance, digits = 6)

estimation_points = c()

prediction_points = c()

# Add the two points with the largest distance to the estimation points
for(i in 1:2){
  point = which(rounded_distance_matrix == rounded_sorted_distance[1],
                arr.ind = TRUE)[i]
  
  point_str = as.character(point)
  
  estimation_points = add_point_to_points(point, estimation_points)
  
}

# Update the distance and sorted distance matrix
for(i in 1:length(estimation_points)){
  
  point_str = as.character(estimation_points[i])
  
  rounded_sorted_distance = update_rounded_sorted_distance(rounded_sorted_distance, point_str, rounded_distance_matrix)
  
  rounded_distance_matrix = update_rounded_distance_matrix(rounded_distance_matrix, point_str)
  
}


# Add the next two points to the prediction points
for(i in 1:2){
  point = which(rounded_distance_matrix == rounded_sorted_distance[1],
                arr.ind = TRUE)[i]
  
  point_str = as.character(point)
  
  prediction_points = add_point_to_points(point, prediction_points)
  
}

# Update the distance and sorted distance matrix
for(i in 1:length(prediction_points)){
  
  point_str = as.character(prediction_points[i])
  
  rounded_sorted_distance = update_rounded_sorted_distance(rounded_sorted_distance, point_str, rounded_distance_matrix)
  
  rounded_distance_matrix = update_rounded_distance_matrix(rounded_distance_matrix, point_str)
  
}


# Add the remaining points
# Alternating between adding to estimation and to prediction
remaining_points <- colnames(rounded_distance_matrix)

i = 1
while (length(prediction_points) < 25){
  
  if ((i %% 2) == 1){
    total_distances_from_point <- matrix(0, nrow = 1, ncol = length(remaining_points))
    
    colnames(total_distances_from_point) <- remaining_points
    
    for (j in 1:length(estimation_points)){
      distances_from_point <- rounded_distance_matrix[estimation_points[j],]
      total_distances_from_point <- total_distances_from_point + distances_from_point
    }
    
    point <- as.integer(colnames(total_distances_from_point)[which(total_distances_from_point == max(total_distances_from_point), arr.ind = TRUE)[2]])
    
    point_str <- as.character(point)
    
    estimation_points <- add_point_to_points(point, estimation_points)
    
    if(length(remaining_points) == 2){
      prediction_points <- add_point_to_points(as.integer(remaining_points[remaining_points != point_str]), prediction_points)
      break
    }
    
    rounded_distance_matrix <- update_rounded_distance_matrix(rounded_distance_matrix, point_str)
    
    remaining_points <- colnames(rounded_distance_matrix)
    
    i = i + 1
  }else {
    total_distances_from_point <- matrix(0, nrow = 1, ncol = length(remaining_points))
    
    colnames(total_distances_from_point) <- remaining_points
    
    for (j in 1:length(prediction_points)){
      distances_from_point <- rounded_distance_matrix[prediction_points[j],]
      total_distances_from_point <- total_distances_from_point + distances_from_point
    }
    
    point <- as.integer(colnames(total_distances_from_point)[which(total_distances_from_point == max(total_distances_from_point), arr.ind = TRUE)[2]])
    
    point_str <- as.character(point)
    
    prediction_points <- add_point_to_points(point, prediction_points)
    
    if(length(remaining_points) == 2){
      estimation_points_points <- add_point_to_points(as.integer(remaining_points[remaining_points != point_str]), estimation_points)
      break
    }
    rounded_distance_matrix <- update_rounded_distance_matrix(rounded_distance_matrix, point_str)
    
    remaining_points <- colnames(rounded_distance_matrix)
    
    i = i + 1
  }
}

estimation_points = c(estimation_points, as.integer(remaining_points))


estimation_points 

prediction_points

data = data.frame(y, x)

E = data[-prediction_points,]

P = data[prediction_points,]

write.csv(E, "data_estimation_duplex.txt", row.names = FALSE)

write.csv(P, "data_prediction_duplex.txt", row.names = FALSE)


