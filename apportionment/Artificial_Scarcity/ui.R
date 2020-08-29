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
library(markdown)

shinyUI(fluidPage(
  
  # TITLE
  titlePanel("Artificial Scarcity in Congressional Representation"),
  fluidRow(
  # INTRODUCTION
  column(12,includeMarkdown("./markdown/subtitle.md")),
  column(12,plotOutput("historicalPlot")),
  # column(12,tableOutput("summaryTable")),
  column(12,includeMarkdown("./markdown/problem.md")),
  column(12,includeMarkdown("./markdown/introduction.md")), 
  column(12,includeMarkdown("./markdown/proposal.md")),
  column(12,plotOutput("averagesPlot"))
  ),

  # ANALYSIS
  sidebarLayout(
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("seatsPlot"),
        plotOutput("deltaPlot")
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
                  includeMarkdown("./markdown/conclusion.md"),
                  dataTableOutput('table')
                  ))
))