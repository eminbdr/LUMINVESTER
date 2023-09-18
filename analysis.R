analysisUi <- {fluidPage(
  tags$head(
    tags$style(HTML("body {
      }.well{padding:5px}
      .shiny-input-container {margin:1px;padding:0px;
      }.col-sm-6 {padding:0px;margin:0px}"))
  ),
  titlePanel(lang$Ex_Nu),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      column(
        width = 6,
        radioButtons(
          "option_num",
          lang$slctns,
          choices = c(1, 2),
          selected = "1",
          inline = TRUE
        )
      ),
      column(
        width = 6,
        numericInput(
          "digit",
          lang$Si_Fi,
          min = -3,
          max = 10,
          value = 3
        )
      ),
      selectInput("first_sector", lang$fi_se, choices = sector),
      selectInput("first_pro",  lang$fi_pr, choices = province),
      conditionalPanel(
        condition = "input.option_num == 2",
        selectInput("second_sector", lang$se_pr, choices = sector),
        selectInput("second_pro", lang$se_se, choices = province, selected = province[2])
      ),
      conditionalPanel(condition = "input.option_num == 1",
                       numericInput("comp_mean", lang$mean_comp, value = 0)),
      radioButtons(
        "wroller",
        h3(lang$mov_avg),
        choiceNames = c(lang$mov_none,lang$mov_mean,lang$mov_med,lang$mov_max),
        choiceValues = c(0,"roll_mean","roll_med","roll_max"),
        selected = "0"
      ),
      uiOutput("k_input"),

    ),
    
    mainPanel(fluidRow(
      plotOutput("distPlot"),
      verbatimTextOutput("calculations"),
      p(lang$main_desc),
    ))
  ))}


