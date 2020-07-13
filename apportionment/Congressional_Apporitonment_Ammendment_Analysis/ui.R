#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
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
        plotOutput("distPlot")
    ),
    sidebarPanel(
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
    )
  ),
  
  # OPTIONS
  # need to style the radio buttons to list horizontally
  fluidRow(radioButtons("radio", 
                        "Seat Limit", 
                        choices = list("Capped (435)" = 1, "Uncapped (>10,000)" = 2), selected = 1, inline = TRUE)),
  
  # DOCUMENTATION
  fluidRow(helpText("Detailed objective, background, and methods...",
                    br(),br(),
                    "Paragraphs spaced with br(), br()"))
))
