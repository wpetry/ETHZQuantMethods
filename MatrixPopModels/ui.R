# User-interface definition for Matrix Population Model app

library(shiny)
library(rhandsontable)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Modeling structure populations"),
  
  # Show a plot of the generated distribution
  sidebarLayout(
    sidebarPanel(
      helpText("Specify the structure of the matrix model using the options below."),
      h3("Set model options"),
      sliderInput(
        inputId="nbins",
        label="Number of life cycle classes",
        min=1L,max=10L,value=6L,step=1L,ticks=FALSE
      ),
      numericInput(
        inputId="ts",
        label="Projection interval (time)",
        value=250,
        min=2,
        max=5000
      )
    ),

    mainPanel(
      fluidRow(
        h3("Transition matrix")
      ),
      fluidRow(
        column(width=8,
               rHandsontableOutput("hot")
        ),
        column(width=4,
               rHandsontableOutput("hotvec")
        )
      ),
      fluidRow(
        plotOutput("lambda")
      )  
    )
  )
))
