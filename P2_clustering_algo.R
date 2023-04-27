# Installation of necessary libraries and packages
library(cluster)
library(reticulate)
library(jpeg)
library(tidyverse)
library(keras)
library(tensorflow)

install_keras()
install_tensorflow(extra_packages="pillow")

# Set working directory to the project file
setwd("C:\\Users\\shams\\Desktop\\Academic Courses\\Spring 23\\IE 332\\Assignments & Projects\\Project 2")
getwd()

# Load model
model <- load_model_tf("./dandelion_model")
summary(model)

#set target size
target_size <- c(224, 224)

# Function to apply k-means clustering and modify the image
modify_image_kmeans <- function(image_array, num_clusters) {
  num_rows <- dim(image_array)[1]
  num_cols <- dim(image_array)[2]
  pixel_matrix <- array_reshape(image_array, c(num_rows * num_cols, 3))
  
  kmeans_result <- kmeans(pixel_matrix, centers = num_clusters)
  modified_centroids <- kmeans_result$centers
  modified_centroids[, 1] <- 1
  modified_centroids[, 2] <- 1
  modified_centroids[, 3] <- 0
  
  modified_pixel_matrix <- modified_centroids[kmeans_result$cluster, ]
  modified_image_array <- array_reshape(modified_pixel_matrix, dim(image_array))
  
  return(modified_image_array)
}

# Write and display the modified image
write_modified_image <- function(image_array, file_name) {
  writeJPEG(image_array, file_name)
  modified_image <- jpeg::readJPEG(file_name)
  graphics::plot(1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="", ylab="")
  graphics::rasterImage(modified_image, 0, 0, 1, 1)
}

# Parameters
target_size <- c(224, 224)
num_clusters <- 5
grass_folder <- "./grass"

# Process and modify images
file_list <- list.files(grass_folder)
for (file_name in file_list) {
  image <- image_load(paste(grass_folder, "/", file_name, sep=""), target_size = target_size)
  image_array <- image_to_array(image) / 255
  
  modified_image_array <- modify_image_kmeans(image_array, num_clusters)
  
  modified_file_name <- paste(grass_folder, "/modified_", file_name, sep="")
  write_modified_image(modified_image_array, modified_file_name)
}

# Test the model
result <- c("","")
modified_file_list <- list.files(grass_folder, pattern = "^modified_")
for (file_name in modified_file_list) {
  test_image <- image_load(paste(grass_folder, "/", file_name, sep=""), target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x / 255
  
  pred <- model %>% predict(x)
  print(pred)
}
