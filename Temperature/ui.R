# User-interface definition for Temperature Model Shiny app

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Temperature effects on insect population dynamics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Move the slider to adjust the temperature of the environment. The model will use the parameters from the vital rate distributions (top panels) and project the population dynamics (lower figure)."),
      sliderInput("Temp",
                  "Temperature (°C):",
                  min = 0,
                  max = 40,
                  value = 20,
                  step = 0.1),
      h4("Starting population densities"),
      numericInput("startA",
                   "Adults:",
                   min = 0,
                   max = 1,
                   value = 0.01,
                   step = 0.01),
      numericInput("startJ",
                   "Juveniles:",
                   min = 0,
                   max = 1,
                   value = 0.01,
                   step = 0.01),
      h4("Plot options"),
      numericInput("xmax",
                   "X-axis maximum:",
                   min = 0,
                   max = 10e6,
                   value = 100,
                   step = 1),
      numericInput("ymax",
                   "Y-axis maximum:",
                   min = 0.1,
                   max = 3,
                   value = 1.5,
                   step = 0.1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Model dynamics",
                 fluidRow(
                   column(4, plotOutput("vital_repro")),
                   column(4, plotOutput("vital_devel")),
                   column(4, plotOutput("vital_mort"))
                 ),
                 fluidRow(
                   column(12, plotOutput("POPDYN"))
                 )
        ),
        tabPanel("Model description",
                 withMathJax(includeMarkdown("ModelDescription.md")))
      )
    )
  )
))
