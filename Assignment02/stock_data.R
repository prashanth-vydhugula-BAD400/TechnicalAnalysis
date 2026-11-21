library(quantmod)

# Function to import stock data
load_stock_data <- function(file_path) {

  symbols <- readLines(file_path)
  stock_data <- list()

  for (symbol in symbols) {

    data <- getSymbols(
      symbol,
      src = "yahoo",
      from = "2026-01-01",
      auto.assign = FALSE
    )

    stock_data[[symbol]] <- data.frame(
      Date = index(data),
      coredata(data)
    )
  }

  return(stock_data)
}

# Import stock data
stock_data <- load_stock_data(file.choose())

# Display imported stock symbols
print(names(stock_data))

# Display first six rows of each stock
lapply(stock_data, head)

library(TTR)

# Function to calculate statistical mode
calculate_mode <- function(x) {
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Function to calculate stock statistics
calculate_statistics <- function(data) {
  
  close_price <- data[[grep("\\.Close$", names(data))]]
  
  moving_average <- SMA(close_price, n = 20)
  
  data.frame(
    Mean = mean(close_price),
    Mode = calculate_mode(close_price),
    Median = median(close_price),
    Standard_Deviation = sd(close_price),
    Moving_Average_20 = tail(moving_average, 1)
  )
}

# Calculate statistics for all stocks
stock_statistics <- lapply(
  stock_data, calculate_statistics
)

# Combine results into a data frame
statistics_table <- do.call(rbind, stock_statistics)

# Display statistics
print(round(statistics_table, 2))