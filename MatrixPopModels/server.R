# Server logic for Matrix Population Model app

library(shiny)
library(rhandsontable)

# DF is the user interface, hideDF will stay in background to populate DF when
# matrix dimensions change
set.seed(314)
hideDF <- DF <- data.frame(matrix(round(runif(100,0,1),digits=3),nrow=10,ncol=10))


# Define server logic to take user input matrix model and project population dynamics
shinyServer(function(input, output) {
   
  values <- reactiveValues()
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)[1:input$nbins,1:input$nbins]
      hideDF[1:input$nbins,1:input$nbins] = DF
    } else {
      if (is.null(values[["DF"]])){
        DF <- DF
        hideDF <- hideDF
      }else{
        DF <- values[["DF"]]
      }
    }
    values[["DF"]] <- DF
  })
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      if (input$nbins>1){
        rhandsontable(DF, useTypes = F, stretchH = "none",
                      colHeaders=paste0("class",1:input$nbins), rowHeaders=paste0("class",1:input$nbins)) 
      }else{
        rhandsontable(data.frame(X1=DF[1,1]), useTypes = F, stretchH = "none",
                      colHeaders="class1", rowHeaders="class1")
      }
  })
  
})
