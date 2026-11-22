# Student Name: Prashanth Vydhugula
# Course: BDA400
# Assignment 5 - Relative Strength Index

rsi <- function(data, period) {

  # Validate inputs
  if (period <= 0 || period != floor(period)) {
    stop("Period must be a positive integer")
  }

  if (length(data) <= period) {
    stop("Insufficient data for RSI calculation")
  }

  # Calculate consecutive price differences
  diff_values <- diff(data)

  # Calculate gains and losses
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)

  # Calculate initial average gains and losses
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])

  # Initialize RSI vector
  rsi_values <- rep(NA_real_, length(data))

  # Calculate RSI using Wilder's smoothing
  for (i in (period + 1):length(data)) {

    avg_gain <- (avg_gain * (period - 1) +
                 gains[i - 1]) / period

    avg_loss <- (avg_loss * (period - 1) +
                 losses[i - 1]) / period

    if (avg_loss == 0 && avg_gain == 0) {
      rsi_values[i] <- 50
    } else if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}
