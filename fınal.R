library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(ggpubr)
library(zoo)

# Function to calculate rounded mean
mean_form <- function(dt, n = 3) round(mean(dt, n))

# Function to calculate rounded variance
var_form <- function(dt, n = 3) round(var(dt), n)

# Function to calculate rounded standard deviation
std_form <- function(dt, n = 3)  round(sqrt(var_form(dt)), n)


# Function to calculate t-score
t_score_calc <- function(x, y, n = 3) {
  x <- as.double(x)
  y <- as.double(y)
  if (length(y) == 1) {
    y <- rep(y, length(x))
  }
  result <- abs((mean(x) - mean(y)) / sqrt(var(x) / length(x) + var(y) / length(y)))
  round(result, n)
}

# Function to calculate paired t-score
paired_t_score_calc <- function(x, y, n = 3) {
  x <- as.double(x)
  y <- as.double(y)
  d <- (x - y)
  result <- abs(mean(d) * sqrt(length(d) / var(d)))
  round(result, n)
}

# Function to calculate p-value
p_value_cal <- function(t_score, df, two_tail) {
  if (two_tail) {
    (1 - pt(q = t_score, df = df)) * 2
  } else {
    1 - pt(q = t_score, df = df)
  }
}

# Function to calculate t-critical value
t_cric_calc <- function(alpha, df) {
  qt(alpha / 2, df, lower.tail = FALSE)
}


average_funcs <-
  list(
    "roll_mean" = zoo::rollmean,
    "roll_max" = zoo::rollmax,
    "roll_med" = zoo::rollmedian  )

# Function to calculate rolling average
rollers <- function(series, param, k = 5) {
  if (!is.null(param)){k=1}
  geom_line(aes(y = average_funcs[[param[[1]]]](
    series,
    k = k,
    na.pad = TRUE,
    align = "center"
  )))
}

analyze_graph <- function(y1, y2, colnamey1, colnamey2, ...) {
  data <- data.frame(first = y1, second = y2)
  data <- data[data$first != 0 & data$second != 0,]
  ovar <- list(...)
  print(data)
  month <- seq_along(data$first)  # Assuming month is a sequential numeric vector
  
  p <- ggplot(data, aes(x = month, y = first)) + geom_line() +
    ylab(colnamey1)
  
  
  if (!is.null(ovar[["wroller"]])) {
    p <- p + rollers(data$first, ovar[["wroller"]], ovar[["k"]])
  }
  if (colnamey1 != colnamey2) {
    q <- ggplot(data, aes(x = month, y = second)) + geom_line() +
      ylab(paste(colnamey2))
    if (!is.null(ovar[["wroller"]])) {
      q <- q + rollers(data$second, ovar[["wroller"]], ovar[["k"]])
    }
    p<- ggarrange(p, q, ncol = 2, nrow = 1)
  }
  p +ggtitle("Export Performance between 2019-January to 2023-March") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}


# Get province names
path<-"C:\\Users\\muham\\Desktop\\clean-dot.csv"
df <-read.csv(path, sep = ";", encoding = "utf-8")
sector <- unique(df[["sector"]])
province <- unique(df[["province"]])
month <-c("2019-01","2019-03","2019-05","2019-07","2019-08","2019-10","2019-12","2020-01","2020-02","2020-03","2020-04","2020-05","2020-06","2020-07","2020-08","2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04","2021-05","2021-06","2021-07","2021-08","2021-09","2021-10","2021-11","2021-12","2022-01","2022-02","2022-03","2022-04","2022-05","2022-06","2022-08","2022-09","2022-10","2022-11","2022-12","2023-01","2023-02","2023-03")



# Define UI for the application
ui <- fluidPage(
  withMathJax(),
  
  titlePanel("Export Numbers by Provinces"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("digit", "Significance", min = -3, max = 10, value = 3),
      selectInput("first_sector", "First Sector", choices = sector),
      selectInput("first_pro", "First Province", choices = province),
      selectInput("second_sector", "Second Sector", choices = sector),
      selectInput("second_pro", "Second Province", choices = province),
      numericInput("comp_mean", "Mean to Compare", value = 3),
      radioButtons(
        "wroller",
        h3("Moving Averages"),
        choices = c("None" ="NULL","Moving Mean" = "roll_mean", "Moving Median" = "roll_med", "Moving Max" = "roll_max"),
        selected = "roll_mean"
      ),
      numericInput("k", "Moving Coefficent", min = 1, max = 15, value = 5)
    ),
    
    mainPanel(fluidPage(fluidRow(
      plotOutput("distPlot"),
      htmlOutput("calculations")
    ))))
)

# Define server logic
server <- function(input, output) {
  column1 <- reactive({
    filtered_df <- subset(df, sector == input$first_sector & province == input$first_pro)
    as.double(t(filtered_df[,3:length(filtered_df)]))
    
  })
  column2 <- reactive({
    filtered_df2 <- subset(df, sector == input$second_sector & province == input$second_pro)
    as.double(t(filtered_df2[,3:length(filtered_df2)]))
    
  })
  name1 <- reactive({
    paste(input$first_sector,input$first_pro,sep="-")
  })
  name2 <- reactive({
    paste(input$second_sector,input$second_pro,sep="-")
  })
  output$distPlot <- renderPlot({
    analyze_graph(column1(), column2(),name1(),name2(), "wroller" = input$wroller, "k" = input$k)
  })
  
  output$calculations <- renderUI(withMathJax({
    calculations <- c(
      paste("\\(\\mu_{", 1, "} =", mean_form(column1(), input$digit), "\\)"),
      paste("\\(\\sigma_{", 1, "} =", std_form(column1(), input$digit), "\\)")
    )
    
    if (name1() != name2()) {
      calculations <- c(
        calculations,
        "<br>",
        paste("\\(\\mu_{", 2, "} =", mean_form(column2(), input$digit), "\\)"),
        paste("\\(\\sigma_{", 2, "} =", std_form(column2(), input$digit), "\\)"),
        "<br>",
        "<br>",
        paste("\\(t_{score} =", t_score_calc(column1(), column2()), "\\)"),
        paste("\\(\\text{paired-}t_{score} =", paired_t_score_calc(column1(), column2()), "\\)")
      )
    }
    
    if (input$first_pro == input$second_pro & input$comp_mean > 0) {
      calculations <- c(
        calculations,
        "<br>",
        paste("\\(t_{score} =", t_score_calc(column1(), input$comp_mean), "\\)")
      )
    }
    
    HTML(paste(calculations))
  }))
  
}

# Run the application
shinyApp(ui = ui, server = server)

