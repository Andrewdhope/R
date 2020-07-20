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
        
        y_col <- switch(radio_input, "seats_lim", "seats_unlim")
        order_col <- switch(radio_input, "seats_lim", "seats_unlim")
        
        # get column name from dataframe
        y_col_name <- names(df[y_col])
        order_col_name <- names(df[order_col])
        
        g <- ggplot(data = df)
        g <- g + geom_col(mapping = aes(x = reorder(state_name, !!ensym(order_col_name)), y = !!ensym(y_col_name)))  
        g <- g + xlab("50 States")
        g <- g + coord_flip()
        
        g
    })
        
    output$deltaPlot <- renderPlot ({
        
        y_col <- "inf_delta(%)"
        order_col <- "inf_delta(%)"
        
        # get column name from dataframe
        y_col_name <- names(df[y_col])
        order_col_name <- names(df[order_col])
        
        g <- ggplot(data = df)
        g <- g + geom_col(mapping = aes(x = reorder(state_name, !!ensym(order_col_name)), y = !!ensym(y_col_name)))  
        g <- g + xlab("50 States")
        g <- g + coord_flip()
        
        g
    })
    
    output$averagesPlot <- renderPlot ({
        
        g <- ggplot(data = df)
        g <- g + geom_col(mapping = aes(x = reorder(state_name, avg_seat_size_lim), y = avg_seat_size_lim, fill = "#6C5B7B"))  
        g <- g + geom_col(mapping = aes(x = reorder(state_name, avg_seat_size_lim), y = avg_seat_size_unlim, fill = "#C06C84"))
        
        # axis, labels, and legend
        g <- g + scale_y_continuous(breaks=seq(0, 1000000,125000))
        g <- g + labs(fill = "") + xlab("50 States") + ylab("Average Constituents per Seat") 
        g <- g + scale_fill_discrete(name = "Apportionment Method", labels = c("Capped at 435", "Uncapped"))
        g <- g + theme(legend.position=c(0.9, 0.15)) # axis.text.y = element_text(size=16)
        
        g <- g + coord_flip()
        
        g
    })
    
    output$table <- renderDataTable(df, options = list(
        pageLength = 5,
        lengthMenu = list(c(5, -1), c("5", "50")),
        searching = FALSE))
  
})
