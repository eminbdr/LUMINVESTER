source("functions.R")
source("global.R")
server <- function(input, output) {
  column1 <- reactive({
    filtered_df <-subset(df,
                         sector == input$first_sector &
                           province == input$first_pro)
    filtered_df<-filtered_df[3:length(filtered_df)]
    filtered_df<-unlist(filtered_df)
    filtered_df
    
  })
  name1 <- reactive({
    paste(input$first_sector, input$first_pro, sep = "-")
  })
  
  column2 <- reactive({
    if (input$option_num == "1") {
      return(column1())
    }
    
    filtered_df2 <-
      subset(
        df,
        sector == input$second_sector &
          province == input$second_pro
      )
    as.double(t(filtered_df2[, 3:length(filtered_df2)]))
  })
  
  name2 <- reactive({
    if (input$option_num == "1") {
      return(name1())
    }
    
    paste(input$second_sector, input$second_pro, sep = "-")
  })
  
  output$distPlot <- renderPlot({
    print(c(name1(),name2()))
    analyze_graph(
      column1(),
      column2(),
      name1(),
      name2(),
      "wroller" = input$wroller,
      "k" = input$k
    )
  })
  
  output$calculations <- renderPrint({

    ratio <-round((column1()[[length(column(1))]]/(column1()[[length(column(1))-1]])-1)*100,input$digit)
    collapser <- ", "
    calculations <- c(
      paste(
        "Mean (",
        input$first_sector,
        "-",
        input$first_pro,
        "): ",
        mean_form(column1(), input$digit)
      ),
      paste("Standard Deviation: ", std_form(column1(), input$digit)),
      
      paste("Ratio to the Last Year:",ratio,"%",sep="")
    )
    
    if (all(name1() != name2())) {
      calculations <- c(
        calculations,
        "",
        paste(
          "Mean (",
          input$second_sector,
          "-",
          input$second_pro,
          "): ",
          mean_form(column2(), input$digit),"\nStandard Deviation: ", std_form(column2(), input$digit)),
        paste("Ratio to the Last Year:",round((column2()[[length(column2())]]/(column2()[[length(column2())-1]]+.000001)-1)*100,input$digit),"%",sep=""),
        "",
        paste(
          "Paired t-score: ",
          paired_t_score_calc(column1(), column2())
        ),
        paste(
          "P-value: ",
          p_value_cal(paired_t_score_calc(column1(), column2()),df=length(column1())-1,two_tail=TRUE)
        ))
    }
    
    if (input$option_num == 1) {
      calculations <- c(
        calculations,
        "",
        paste(
          "t-score (comparing mean with ",
          input$comp_mean,
          "): ",
          t_score_calc(column1(), input$comp_mean)
        ),
        paste(
          "P-value: ",
          p_value_cal(t_score_calc(column1(), input$comp_mean),df=length(column1())-1,two_tail=TRUE))
      )
    }
    
    cat(calculations, sep = "\n")
    
    if((input$first_pro == input$second_pro)&(input$option_num != 1)) {
      linreg <- lm(column2()~column1())
      cat(paste("\np-value of LR between sector= ",summary(linreg)$coefficients[,4][2],"\nR^2-Squared:",summary(linreg)$r.squared,"\nIntercept =",summary(linreg)$coefficients[,1][1],
                "\nCoefficent =",summary(linreg)$coefficients[,1][2],"\n",collapse  = ""))
    }
    
    
    median_mean <-
      desc_finder(input$first_sector, which.median, digits = input$digit)
    cat(
      lang$pwmedm,
      input$first_sector,
      "):",
      paste(median_mean, collapse = collapser)
    )
    
    max_mean_provinces <-
      desc_sorter(input$first_sector, digits = input$digit)
    cat(
      lang$pwmaxm,
      input$first_sector,
      "):",
      "\n",
      first_kth(max_mean_provinces[2:nrow(max_mean_provinces), ,drop=FALSE],collapser,5)
    )
    
    min_mean_provinces <-
      desc_sorter(input$first_sector, FALSE, digits = input$digit)
    cat(
      lang$pwminm,
      input$first_sector,
      "):",
      "\n",
      first_kth(min_mean_provinces,collapser,5)
    )
    
    
  })
  
  
  
  observe({
    if (input$wroller != 0) {
      output$k_input <- renderUI({
        numericInput(
          "k",
          lang$mov_coef,
          min = 1,
          max = 15,
          value = 5
        )
      })
    } else {
      output$k_input <- renderUI({
        
      })
    }
  })
  
  
  
}
