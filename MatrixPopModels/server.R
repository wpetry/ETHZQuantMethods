# Server logic for Matrix Population Model app

library(shiny)
library(rhandsontable)
library(popbio)
library(jsonlite)

# DF is the user interface, hideDF will stay in background to populate DF when
# matrix dimensions change
data(tortoise)
hideDF <- DF <- tortoise$med.high


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
        rhandsontable(DF, useTypes = T, stretchH = "none", digits = 6,
                      colHeaders=paste0("class",1:input$nbins),
                      rowHeaders=paste0("class",1:input$nbins)) %>%
          hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
      }else{
        rhandsontable(data.frame(X1=DF[1,1]), useTypes = F, stretchH = "none",
                      colHeaders="class1", rowHeaders="class1")
      }
  })
  
})
