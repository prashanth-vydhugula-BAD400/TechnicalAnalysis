# Student Name: Prashanth Vydhugula
# Course: BDA400
# Assignment 5 - Standard Deviation

stdev <- function(data) {

  # Check if data is empty
  if (length(data) == 0) {
    stop("Data cannot be empty")
  }

  # Calculate the mean
  mean_value <- sum(data) / length(data)

  # Calculate the differences
  diff_values <- data - mean_value

  # Calculate squared differences
  squared_diff <- diff_values^2

  # Calculate the variance
  variance <- sum(squared_diff) / length(data)

  # Calculate standard deviation
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
