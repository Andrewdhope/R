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
        
        generatePlot(df, y_col, order_col) + ylab("Seats")
    })
        
    output$deltaPlot <- renderPlot ({
        generatePlot(df, y_col = "influence_delta(%)", order_col = "influence_delta(%)") + ylab("Change in Relative Influence (%)")
    })
    
    output$table <- renderDataTable(df, options = list(
        pageLength = 5,
        lengthMenu = list(c(5, -1), c("5", "50")),
        searching = FALSE))
  
})
