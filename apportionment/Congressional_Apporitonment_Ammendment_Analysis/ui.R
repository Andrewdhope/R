#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#   https://shiny.rstudio.com/gallery/
#   https://bookdown.org/weicheng/shinyTutorial/ui.html
#

library(shiny)

shinyUI(fluidPage(
  
  # TITLE
  titlePanel("Conressional Apportionment Amendment Analysis"),
  
  fluidRow(
  # INTRODUCTION
  column(12, helpText("Intro to apportionment and article the first...")),
  column(10,plotOutput("averagesPlot"))
  ),
  
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
                     choices = list("Capped (435 total seats)" = 1, "Uncapped (>2,000 total seats)" = 2), 
                     selected = 1, 
                     inline = FALSE),
        helpText("Explain the plots and the capped/uncapped options.")
    )
  ),
  
  # DOCUMENTATION
  fluidRow(column(12, 
                  helpText("Detailed objective, background, and methods...",
                  br(),br(),
                  "Paragraphs spaced with br(), br()"
                  )))
))
