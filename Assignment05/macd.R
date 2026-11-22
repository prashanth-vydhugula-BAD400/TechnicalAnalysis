# Student Name: Prashanth Vydhugula
# Course: BDA400
# Assignment 5 - MACD

macd <- function(data, short_period, long_period, signal_period) {

  # Calculate short-term and long-term EMA
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)

  # Calculate MACD line
  macd_line <- short_ema - long_ema

  # Calculate signal line
  signal_line <- ema(macd_line, signal_period)

  # Calculate histogram
  histogram <- macd_line - signal_line

  # Store results
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )

  return(result)
}
