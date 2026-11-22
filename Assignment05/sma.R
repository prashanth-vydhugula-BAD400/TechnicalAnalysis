# Student Name: Prashanth Vydhugula
# Course: BDA400
# Assignment 5 - Simple Moving Average

sma <- function(data, period) {

  # Validate the period
  if (period <= 0 || period != floor(period)) {
    stop("Period must be a positive integer")
  }

  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize the SMA vector
  sma_values <- numeric(length(data) - period + 1)

  # Calculate SMA for each window
  for (i in seq_along(sma_values)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }

  return(sma_values)
}
