# User-interface definition for Matrix Population Model app

library(shiny)

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
        min=1L,max=10L,value=3L,step=1L,ticks=FALSE
      ),
      radioButtons("matType", "Matrix type", c("Leslie (age/stage)", "Lefkovitch (size)"))
    ),
    
    mainPanel(
      h3("Transition matrix"),
      rHandsontableOutput("hot")
    )
  )
))
