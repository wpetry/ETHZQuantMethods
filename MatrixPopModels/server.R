# Server logic for Matrix Population Model app

library(shiny)
library(rhandsontable)

# ( DF <- data.frame(matrix(0,nrow=values[["nbins"]],ncol=values[["nbins"]])))

DF <- data.frame(matrix(0,nrow=10,ncol=10))


# Define server logic to take user input matrix model and project population dynamics
shinyServer(function(input, output) {
   
  values <- reactiveValues()
  
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
      if (input$nbins>1){
        rhandsontable(DF[1:input$nbins,1:input$nbins], useTypes = F, stretchH = "none", digits = 10,
                      colHeaders=paste0("class",1:input$nbins), rowHeaders=paste0("class",1:input$nbins)) 
      }else{
        rhandsontable(data.frame(X1=DF[1,1]), useTypes = F, stretchH = "none", digits = 10,
                      colHeaders="class1", rowHeaders="class1")
      }
  })
  
})
