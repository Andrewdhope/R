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
  titlePanel("Artificial Scarcity in Congressional Representation"),
  
  fluidRow(
  # INTRODUCTION
  column(12, helpText("Intro to apportionment and article the first...")), 
  column(12,plotOutput("averagesPlot"))
  ),
  
  tabsetPanel(type = "tabs", 
              tabPanel("Problem Summary", helpText('mean size per seat (y) and state-level range-band (y) over time (x)')),
              tabPanel("Next Tab", 
                       fluidRow(column(6, helpText('seats per state')), column(6, helpText('intro and controls'))),
                       fluidRow(column(6, helpText('seat size w/ mean and variance')), column(6, helpText('delta influence')))
                       )
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
