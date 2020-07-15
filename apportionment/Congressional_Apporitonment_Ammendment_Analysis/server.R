#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
shinyServer(function(input, output) {
    source("../apportionment_functions.R")
    df <- buildDataframe()
    
    output$seatsPlot <- renderPlot ({
        radio_input <- as.integer(input$radio)
        
        y_col <- switch(radio_input, "seats_limit", "seats_unlim")
        order_col <- switch(radio_input, "seats_limit", "seats_unlim")
        
        generatePlot(df, y_col, order_col)
    })
        
    output$deltaPlot <- renderPlot ({
        generatePlot(df, y_col = "influence_delta(%)", order_col = "influence_delta(%)")
    })
    
    output$table <- renderDataTable(df, options = list(paging = FALSE, searching = FALSE))
  
})
