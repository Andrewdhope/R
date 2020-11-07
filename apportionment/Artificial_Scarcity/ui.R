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
  titlePanel("Artificial Scarcity in Congressional Representation (draft)"),
    fluidRow(
        # INTRODUCTION
        column(12,includeMarkdown("./markdown/subtitle.md")),
        column(12,plotOutput("historicalPlot")),
        # PROBLEM SUMMARY
        column(12,includeMarkdown("./markdown/problem.md")),
        
        # HISTORICAL CONTEXT - plenty here, keep it brief, find sources
        column(12,includeMarkdown("./markdown/background.md")), 
        
        # PROPOSAL - visual design w/ width, color, shading
        column(12,includeMarkdown("./markdown/proposal.md")),
        column(12,includeMarkdown("./markdown/results.md")),
        column(12,includeMarkdown("./markdown/results-variance.md")),
        column(12,plotOutput("averagesPlot"))
        ),
  
    fluidRow(
        column(12,includeMarkdown("./markdown/results-rankings-title.md"))
    ),
    sidebarLayout(
        mainPanel(
            plotOutput("seatsPlot")
            ),
        sidebarPanel(
            radioButtons("radio", 
                "Seat Limit", 
                choices = list("Capped (435 total seats)" = 1, "Uncapped (2,245 total seats)" = 2), 
                selected = 1, 
                inline = FALSE
                ),
            includeMarkdown("./markdown/results-rankings-text.md")
            )
        ),
    fluidRow(
      column(12,includeMarkdown("./markdown/results-influence-title.md"))
    ),
    sidebarLayout(
      mainPanel(
          plotOutput("deltaPlot")
      ),
      sidebarPanel(
          includeMarkdown("./markdown/results-influence-text.md")
      )
    ),  
  
    fluidRow(
        # CONCLUSION - write the supplemental
        column(12,includeMarkdown("./markdown/conclusion.md")),
        column(12,includeMarkdown("./markdown/supplement.md")),
        column(12,includeMarkdown("./markdown/references.md")),
        
        # DATA
        column(12,includeMarkdown("./markdown/data.md")),
        column(12,includeMarkdown("./markdown/title-currentData.md"),dataTableOutput('currentData')),
        column(12,includeMarkdown("./markdown/title-historicalData.md"),dataTableOutput('historicalData'))
    )
))