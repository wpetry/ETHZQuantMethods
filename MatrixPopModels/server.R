# Server logic for Matrix Population Model app

library(shiny)

# ( DF <- data.frame(matrix(0,nrow=values[["nbins"]],ncol=values[["nbins"]])))

# Define server logic to take user input matrix model and project population dynamics
shinyServer(function(input, output) {
   
  values <- reactiveValues()
  
  values$DF <- data.frame(matrix(0,nrow=3,ncol=3))
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, stretchH = "none")
  })
  
})
