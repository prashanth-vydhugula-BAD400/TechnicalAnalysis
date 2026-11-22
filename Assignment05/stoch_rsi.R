# Student Name: Prashanth Vydhugula
# Course: BDA400
# Assignment 5 - Stochastic RSI

stoch_rsi <- function(data, period, k_period, d_period) {

  # Calculate RSI
  rsi_values <- rsi(data, period)

  # Remove initial missing values
  rsi_values <- rsi_values[!is.na(rsi_values)]

  # Validate sufficient data
  if (length(rsi_values) < k_period + d_period - 1) {
    stop("Insufficient data for Stochastic RSI")
  }

  # Calculate minimum and maximum RSI
  min_rsi <- min(rsi_values)
  max_rsi <- max(rsi_values)

  # Calculate normalized Stochastic RSI
  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(rsi_values))
  } else {
    k_values <- (rsi_values - min_rsi) /
                (max_rsi - min_rsi)
  }

  # Calculate K line
  k_line <- sma(k_values, k_period)

  # Calculate D line
  d_line <- sma(k_line, d_period)

  # Return results
  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}
