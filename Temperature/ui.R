# User-interface definition for Temperature Model Shiny app

library(shiny)
library(rhandsontable)
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
                   value = 19.85,
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
      numericInput("ymax",
                   "Y-axis maximum:",
                   min = 0.1,
                   max = 3,
                   value = 1.5,
                   step = 0.1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("POPDYN")
    )
  )
))
