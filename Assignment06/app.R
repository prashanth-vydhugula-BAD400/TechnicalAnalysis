
# BDA400 - Assignment 6
# Technical Analysis using R - Visualization Phase
# Student Name: Prashanth Vydhugula

# Step 1: Load libraries
library(shiny)
library(ggplot2)
library(quantmod)

# Step 2: Create User Interface
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
      plotOutput("stock_chart", height = "400px"),

      h3("Technical Indicators"),
      plotOutput("indicator_chart", height = "300px"),

      h3("Trading Signal"),
      verbatimTextOutput("trading_signal")
    )
  )
)

# Step 3: Server Logic
server <- function(input, output, session) {

  # Fetch stock data from Yahoo Finance
  stock_data <- reactive({

    req(input$stock_symbol, input$date_range)

    tryCatch({

      validate(
        need(
          input$date_range[1] < input$date_range[2],
          "Choose a valid date range."
        )
      )

      getSymbols(
        toupper(trimws(input$stock_symbol)),
        src = "yahoo",
        from = input$date_range[1],
        to = input$date_range[2] + 1,
        auto.assign = FALSE
      )

    }, error = function(e) {

      validate(
        need(FALSE, paste("Data error:", e$message))
      )

    })

  })

  # Convert data to selected time frame
  filtered_data <- reactive({

    data <- stock_data()

    if (input$time_frame == "Weekly") {
      data <- to.weekly(data, indexAt = "lastof")
    }

    if (input$time_frame == "Monthly") {
      data <- to.monthly(data, indexAt = "lastof")
    }

    validate(
      need(NROW(data) >= 50,
           "At least 50 data points are required.")
    )

    data

  })

  # Calculate technical indicators
  analysis_data <- reactive({

    data <- filtered_data()
    close <- Cl(data)

    df <- data.frame(
      Date = as.Date(index(data)),
      Open = as.numeric(Op(data)),
      High = as.numeric(Hi(data)),
      Low = as.numeric(Lo(data)),
      Close = as.numeric(close),
      SMA20 = as.numeric(SMA(close, n = 20)),
      SMA50 = as.numeric(SMA(close, n = 50)),
      RSI = as.numeric(RSI(close, n = 14)),
      MACD = as.numeric(
        MACD(close, nFast = 12,
             nSlow = 26, nSig = 9)$macd
      ),
      MACDSignal = as.numeric(
        MACD(close, nFast = 12,
             nSlow = 26, nSig = 9)$signal
      )
    )

    # Moving average crossover strategy
    previous_short <- c(NA, head(df$SMA20, -1))
    previous_long <- c(NA, head(df$SMA50, -1))

    df$Signal <- "Hold"

    buy <- !is.na(previous_short) &
      !is.na(previous_long) &
      previous_short <= previous_long &
      df$SMA20 > df$SMA50

    sell <- !is.na(previous_short) &
      !is.na(previous_long) &
      previous_short >= previous_long &
      df$SMA20 < df$SMA50

    df$Signal[buy] <- "Buy"
    df$Signal[sell] <- "Sell"

    df

  })

  # Step 4: Stock price chart
  output$stock_chart <- renderPlot({

    df <- analysis_data()

    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Line") {

      p <- p +
        geom_line(aes(y = Close), color = "blue")

    } else if (input$chart_type == "Area") {

      p <- p +
        geom_area(
          aes(y = Close),
          fill = "skyblue",
          alpha = 0.5
        )

    } else {

      p <- p +
        geom_segment(
          aes(xend = Date, y = Low, yend = High)
        ) +
        geom_rect(
          aes(
            xmin = Date - 0.3,
            xmax = Date + 0.3,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Close >= Open
          )
        ) +
        scale_fill_manual(
          values = c("TRUE" = "green", "FALSE" = "red"),
          guide = "none"
        )

    }

    # Add moving averages
    if ("Moving Averages" %in% input$technical_indicators) {

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

    # Add Buy/Sell annotations
    if (input$show_signals) {

      signal_rows <- df[df$Signal != "Hold", ]

      p <- p +
        geom_point(
          data = signal_rows,
          aes(y = Close, color = Signal),
          size = 3
        ) +
        geom_text(
          data = signal_rows,
          aes(y = Close, label = Signal,
              color = Signal),
          vjust = -1,
          show.legend = FALSE
        ) +
        scale_color_manual(
          values = c("Buy" = "darkgreen",
                     "Sell" = "red")
        )

    }

    p +
      labs(
        title = paste(
          toupper(input$stock_symbol),
          "Stock Price"
        ),
        x = "Date",
        y = "Stock Price"
      ) +
      theme_minimal()

  })

  # Step 5: RSI and MACD indicators
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

    if (show_rsi) {

      charts[["RSI"]] <- ggplot(df, aes(Date, RSI)) +
        geom_line(color = "darkblue", na.rm = TRUE) +
        geom_hline(yintercept = 70, linetype = "dashed") +
        geom_hline(yintercept = 30, linetype = "dashed") +
        labs(title = "Relative Strength Index (RSI)",
             x = "Date", y = "RSI") +
        theme_minimal()

    }

    if (show_macd) {

      charts[["MACD"]] <- ggplot(df, aes(Date)) +
        geom_line(aes(y = MACD),
                  color = "blue", na.rm = TRUE) +
        geom_line(aes(y = MACDSignal),
                  color = "red", na.rm = TRUE) +
        labs(title = "MACD Indicator",
             x = "Date", y = "MACD") +
        theme_minimal()

    }

    if (length(charts) == 1) {
      print(charts[[1]])
    } else {
      # Display both indicator plots in one output
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

  # Step 6: Display latest trading signal
  output$trading_signal <- renderPrint({

    df <- analysis_data()

    latest <- tail(df, 1)

    cat("Stock:", toupper(input$stock_symbol), "\n")
    cat("Latest Close:", round(latest$Close, 2), "\n")
    cat("Latest Crossover Signal:", latest$Signal, "\n")

    cat("\nTrading Rule:\n")
    cat("Buy: SMA20 crosses above SMA50\n")
    cat("Sell: SMA20 crosses below SMA50\n")
    cat("Hold: No crossover detected\n")

  })

}

# Step 7: Run Shiny Application
shinyApp(ui = ui, server = server)
