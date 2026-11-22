# Student Name: Prashanth Vydhugula
# Course: BDA400
# Assignment 5 - Linear Regression

linreg <- function(regressionSource, regressionLength, regressionOffset) {

  # Calculate the total number of elements
  n <- length(regressionSource)

  # Validate regression length
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  # Validate regression offset
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  if (regressionLength < 2 || regressionOffset < 0) {
    stop("Invalid regression length or offset")
  }

  # Calculate starting and ending indices
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)

  if (end_index <= start_index) {
    stop("Insufficient data for regression")
  }

  # Extract the relevant data
  source_subset <- regressionSource[start_index:end_index]

  # Calculate index values
  index_values <- seq_along(source_subset)

  # Calculate mean values
  mean_index <- mean(index_values)
  mean_source <- mean(source_subset)

  # Calculate numerator and denominator
  numerator <- sum((index_values - mean_index) *
                   (source_subset - mean_source))

  denominator <- sum((index_values - mean_index)^2)

  # Calculate slope and intercept
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate predicted values
  predicted_values <- slope * index_values + intercept

  # Return results
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )

  return(result)
}
