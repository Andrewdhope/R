#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  
  # TITLE
  titlePanel("Conressional Apportionment Amendment Analysis"),
  
  # INTRODUCTION
  fluidRow(helpText("Intro to apportionment and article the first...")),
  
  # ANALYSIS
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("seatsPlot"),
        plotOutput("deltaPlot"),
        dataTableOutput('table')
    ),
    sidebarPanel(
        radioButtons("radio", 
                     "Seat Limit", 
                     choices = list("Capped (435 total seats)" = 1, "Uncapped (>10,000 total seats)" = 2), 
                     selected = 1, 
                     inline = FALSE),
        helpText("Explain the plots and the capped/uncapped options.")
    )
  ),
  
  # DOCUMENTATION
  fluidRow(helpText("Detailed objective, background, and methods...",
                    br(),br(),
                    "Paragraphs spaced with br(), br()"))
))
