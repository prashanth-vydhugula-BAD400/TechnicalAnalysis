# Student Name: Prashanth Vydhugula
# Course: BDA400
# Assignment 5 - Exponential Moving Average

ema <- function(data, period) {

  # Validate inputs
  if (length(data) == 0) {
    stop("Data cannot be empty")
  }

  if (period <= 0 || period != floor(period)) {
    stop("Period must be a positive integer")
  }

  # Calculate the multiplier
  multiplier <- 2 / (period + 1)

  # Initialize EMA vector
  ema_values <- numeric(length(data))

  # Calculate EMA
  for (i in seq_along(data)) {
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      ema_values[i] <- (data[i] - ema_values[i - 1]) *
        multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}
