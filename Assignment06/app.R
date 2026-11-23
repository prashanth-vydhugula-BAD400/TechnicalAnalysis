
# ==================================================
# BDA400 - Assignment 6
# Technical Analysis using R - Visualization Phase
# Student Name: Prashanth Vydhugula
# ==================================================

# STEP 1: Load required libraries

library(shiny)
library(ggplot2)
library(quantmod)

# ==================================================
# STEP 2: Create User Interface
# ==================================================

ui <- fluidPage(
  
  titlePanel("Stock Portfolio Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(
        "stock_symbol",
        "Enter Stock Symbol:",
        value = "AAPL"
      ),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = Sys.Date()
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Line", "Area", "Candlestick")
      ),
      
      checkboxGroupInput(
        "technical_indicators",
        "Select Technical Indicators:",
        choices = c(
          "Moving Averages",
          "RSI",
          "MACD"
        ),
        selected = "Moving Averages"
      ),
      
      checkboxInput(
        "show_signals",
        "Show Buy/Sell Signals",
        value = TRUE
      )
      
    ),
    
    mainPanel(
      
      h3("Stock Price Analysis"),
      
      plotOutput(
        "stock_chart",
        height = "400px"
      ),
      
      h3("Technical Indicators"),
      
      plotOutput(
        "indicator_chart",
        height = "350px"
      ),
      
      h3("Trading Signal"),
      
      verbatimTextOutput("trading_signal")
      
    )
  )
)

# ==================================================
# STEP 3: Server Logic
# ==================================================

server <- function(input, output, session) {
  
  # STEP 3.1: Fetch data from Yahoo Finance
  
  stock_data <- reactive({
    
    req(input$stock_symbol, input$date_range)
    
    validate(
      need(
        input$date_range[1] < input$date_range[2],
        "Please select a valid date range."
      )
    )
    
    # Fetch extra history for indicator calculations.
    # This gives monthly charts enough observations
    # for the 50-period moving average.
    
    start_date <- as.Date(input$date_range[1]) - 365 * 5
    end_date <- as.Date(input$date_range[2]) + 1
    
    symbol <- toupper(trimws(input$stock_symbol))
    
    validate(
      need(
        nzchar(symbol),
        "Please enter a stock symbol."
      )
    )
    
    data <- tryCatch({
      
      getSymbols(
        symbol,
        src = "yahoo",
        from = start_date,
        to = end_date,
        auto.assign = FALSE,
        warnings = FALSE
      )
      
    }, error = function(e) {
      
      NULL
      
    })
    
    validate(
      need(
        !is.null(data),
        "Unable to fetch stock data. Check the symbol or internet connection."
      )
    )
    
    validate(
      need(
        NROW(data) > 50,
        "Not enough historical stock data."
      )
    )
    
    data
    
  })
  
  # ==================================================
  # STEP 3.2: Convert stock data to data frame
  # ==================================================
  
  daily_data <- reactive({
    
    data <- stock_data()
    
    df <- data.frame(
      
      Date = as.Date(index(data)),
      
      Open = as.numeric(Op(data)),
      
      High = as.numeric(Hi(data)),
      
      Low = as.numeric(Lo(data)),
      
      Close = as.numeric(Cl(data))
      
    )
    
    df <- df[complete.cases(df), ]
    
    df[order(df$Date), ]
    
  })
  
  # ==================================================
  # STEP 3.3: Daily, Weekly and Monthly aggregation
  # ==================================================
  
  filtered_data <- reactive({
    
    df <- daily_data()
    
    frame <- input$time_frame
    
    if (frame == "Weekly") {
      
      # Identify Monday of each week
      
      week_start <- df$Date -
        as.integer(format(df$Date, "%u")) + 1
      
      groups <- as.character(week_start)
      
    } else if (frame == "Monthly") {
      
      groups <- format(df$Date, "%Y-%m")
      
    } else {
      
      return(df)
      
    }
    
    # Aggregate OHLC values by period
    
    group_rows <- split(
      seq_len(nrow(df)),
      factor(groups, levels = unique(groups))
    )
    
    combined <- lapply(group_rows, function(idx) {
      
      x <- df[idx, , drop = FALSE]
      
      data.frame(
        Date = max(x$Date),
        Open = x$Open[1],
        High = max(x$High),
        Low = min(x$Low),
        Close = x$Close[nrow(x)]
      )
      
    })
    
    result <- do.call(rbind, combined)
    
    rownames(result) <- NULL
    
    result
    
  })
  
  # ==================================================
  # STEP 3.4: Calculate Technical Indicators
  # ==================================================
  
  analysis_data <- reactive({
    
    df <- filtered_data()
    
    validate(
      need(
        nrow(df) >= 51,
        "Not enough data to calculate technical indicators."
      )
    )
    
    # Use numeric vectors to avoid xts conversion errors
    
    close_prices <- as.numeric(df$Close)
    
    # Moving Averages
    
    df$SMA20 <- as.numeric(
      TTR::SMA(close_prices, n = 20)
    )
    
    df$SMA50 <- as.numeric(
      TTR::SMA(close_prices, n = 50)
    )
    
    # Relative Strength Index
    
    df$RSI <- as.numeric(
      TTR::RSI(close_prices, n = 14)
    )
    
    # MACD and signal line
    
    macd_result <- TTR::MACD(
      close_prices,
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      maType = "EMA"
    )
    
    df$MACD <- as.numeric(
      macd_result[, "macd"]
    )
    
    df$MACDSignal <- as.numeric(
      macd_result[, "signal"]
    )
    
    # ==================================================
    # STEP 3.5: Buy / Sell / Hold Rules
    # ==================================================
    
    # Compare current and previous moving averages
    
    previous_short <- c(
      NA_real_,
      head(df$SMA20, -1)
    )
    
    previous_long <- c(
      NA_real_,
      head(df$SMA50, -1)
    )
    
    # Buy when SMA20 crosses above SMA50
    
    buy <- (
      previous_short <= previous_long &
        df$SMA20 > df$SMA50
    )
    
    # Sell when SMA20 crosses below SMA50
    
    sell <- (
      previous_short >= previous_long &
        df$SMA20 < df$SMA50
    )
    
    # Handle unavailable initial values
    
    buy[is.na(buy)] <- FALSE
    sell[is.na(sell)] <- FALSE
    
    df$Signal <- "Hold"
    
    df$Signal[buy] <- "Buy"
    df$Signal[sell] <- "Sell"
    
    # Only show selected date range
    
    start <- as.Date(input$date_range[1])
    end <- as.Date(input$date_range[2])
    
    df <- df[
      df$Date >= start & df$Date <= end,
      ,
      drop = FALSE
    ]
    
    validate(
      need(
        nrow(df) > 0,
        "No stock data available for this date range."
      )
    )
    
    df
    
  })
  
  # ==================================================
  # STEP 4: Stock Price Visualization
  # ==================================================
  
  output$stock_chart <- renderPlot({
    
    df <- analysis_data()
    
    p <- ggplot(df, aes(x = Date))
    
    # Line Chart
    
    if (input$chart_type == "Line") {
      
      p <- p +
        geom_line(
          aes(y = Close),
          color = "blue",
          linewidth = 0.7
        )
      
    }
    
    # Area Chart
    
    else if (input$chart_type == "Area") {
      
      p <- p +
        geom_area(
          aes(y = Close),
          fill = "skyblue",
          alpha = 0.5
        )
      
    }
    
    # Candlestick Chart
    
    else if (input$chart_type == "Candlestick") {
      
      candle_width <- switch(
        input$time_frame,
        "Daily" = 0.35,
        "Weekly" = 2,
        "Monthly" = 8
      )
      
      df$CandleColor <- ifelse(
        df$Close >= df$Open,
        "Up",
        "Down"
      )
      
      p <- ggplot(df, aes(x = Date)) +
        
        geom_segment(
          aes(
            xend = Date,
            y = Low,
            yend = High
          ),
          color = "black"
        ) +
        
        geom_rect(
          aes(
            xmin = Date - candle_width,
            xmax = Date + candle_width,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = CandleColor
          ),
          color = "black",
          linewidth = 0.15
        ) +
        
        scale_fill_manual(
          values = c(
            "Up" = "green",
            "Down" = "red"
          ),
          guide = "none"
        )
      
    }
    
    # ==================================================
    # STEP 4.1: Moving Average Overlay
    # ==================================================
    
    if (
      "Moving Averages" %in%
      input$technical_indicators
    ) {
      
      p <- p +
        
        geom_line(
          aes(y = SMA20),
          color = "orange",
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        
        geom_line(
          aes(y = SMA50),
          color = "purple",
          linewidth = 0.8,
          na.rm = TRUE
        )
      
    }
    
    # ==================================================
    # STEP 4.2: Buy/Sell Annotations
    # ==================================================
    
    if (isTRUE(input$show_signals)) {
      
      signal_rows <- df[
        df$Signal %in% c("Buy", "Sell"),
        ,
        drop = FALSE
      ]
      
      p <- p +
        
        geom_point(
          data = signal_rows,
          aes(
            y = Close,
            color = Signal
          ),
          size = 3
        ) +
        
        geom_text(
          data = signal_rows,
          aes(
            y = Close,
            label = Signal,
            color = Signal
          ),
          vjust = -1,
          size = 3,
          show.legend = FALSE
        ) +
        
        scale_color_manual(
          values = c(
            "Buy" = "darkgreen",
            "Sell" = "red"
          )
        )
      
    }
    
    p +
      
      labs(
        title = paste(
          toupper(input$stock_symbol),
          "-",
          input$time_frame,
          "Stock Price"
        ),
        
        x = "Date",
        y = "Stock Price"
      ) +
      
      theme_minimal()
    
  })
  
  # ==================================================
  # STEP 5: RSI and MACD Charts
  # ==================================================
  
  output$indicator_chart <- renderPlot({
    
    df <- analysis_data()
    
    selected <- input$technical_indicators
    
    show_rsi <- "RSI" %in% selected
    
    show_macd <- "MACD" %in% selected
    
    validate(
      need(
        show_rsi || show_macd,
        "Select RSI or MACD to display an indicator."
      )
    )
    
    charts <- list()
    
    # RSI Chart
    
    if (show_rsi) {
      
      charts[["RSI"]] <- ggplot(
        df,
        aes(x = Date, y = RSI)
      ) +
        
        geom_line(
          color = "darkblue",
          na.rm = TRUE
        ) +
        
        geom_hline(
          yintercept = 70,
          linetype = "dashed"
        ) +
        
        geom_hline(
          yintercept = 30,
          linetype = "dashed"
        ) +
        
        labs(
          title = "Relative Strength Index (RSI)",
          x = "Date",
          y = "RSI"
        ) +
        
        theme_minimal()
      
    }
    
    # MACD Chart
    
    if (show_macd) {
      
      charts[["MACD"]] <- ggplot(
        df,
        aes(x = Date)
      ) +
        
        geom_line(
          aes(y = MACD),
          color = "blue",
          na.rm = TRUE
        ) +
        
        geom_line(
          aes(y = MACDSignal),
          color = "red",
          na.rm = TRUE
        ) +
        
        geom_hline(
          yintercept = 0,
          linetype = "dashed",
          color = "gray"
        ) +
        
        labs(
          title = "MACD Indicator",
          x = "Date",
          y = "MACD"
        ) +
        
        theme_minimal()
      
    }
    
    # Display selected indicators
    
    if (length(charts) == 1) {
      
      print(charts[[1]])
      
    } else {
      
      grid::grid.newpage()
      
      grid::pushViewport(
        grid::viewport(
          layout = grid::grid.layout(2, 1)
        )
      )
      
      for (i in seq_along(charts)) {
        
        print(
          charts[[i]],
          vp = grid::viewport(
            layout.pos.row = i,
            layout.pos.col = 1
          ),
          newpage = FALSE
        )
        
      }
      
    }
    
  })
  
  # ==================================================
  # STEP 6: Display Latest Trading Signal
  # ==================================================
  
  output$trading_signal <- renderPrint({
    
    df <- analysis_data()
    
    latest <- tail(df, 1)
    
    cat(
      "Stock:",
      toupper(input$stock_symbol),
      "\n"
    )
    
    cat(
      "Latest Close:",
      round(latest$Close, 2),
      "\n"
    )
    
    cat(
      "Latest Crossover Signal:",
      latest$Signal,
      "\n"
    )
    
    cat("\nTrading Rules:\n")
    
    cat(
      "Buy: SMA20 crosses above SMA50\n"
    )
    
    cat(
      "Sell: SMA20 crosses below SMA50\n"
    )
    
    cat(
      "Hold: No crossover detected\n"
    )
    
    cat(
      "\nSelected Time Frame:",
      input$time_frame,
      "\n"
    )
    
  })
  
}

# ==================================================
# STEP 7: Launch Shiny Application
# ==================================================

shinyApp(
  ui = ui,
  server = server
)
