# Server logic for Matrix Population Model app

library(shiny)
library(rhandsontable)
library(popbio)
library(ggplot2)
library(dplyr)

# DF is the user interface, hideDF will stay in background to populate DF when
# matrix dimensions change
data(tortoise)
hideDF <- DF <- hudsonia$A88
N<-1000
ts<-250

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
  
  output$lambda <- renderPlot({
    lammat<-matrix(c(N,rep(0,times=input$nbins-1),
                     rep(0,times=input$nbins*ts)),
                   ncol=ts+1)
    # lammat<-matrix(c(N,rep(0,times=bins-1),
    #                  rep(0,times=bins*ts)),
    #                ncol=ts+1)
    for(i in 1:ts){
      lammat[,i+1]<-DF %*% lammat[,i]
    }
    n<-colSums(lammat)
    simpop<-data.frame(year=0:ts,lam=n/lag(n))
    
    ggplot(data=simpop,aes(x=year,y=lam))+
      geom_line(size=2,color="dodgerblue3")+
      geom_hline(yintercept=1,linetype="dashed")+
      scale_x_continuous(name="Year")+
      scale_y_continuous(name="Population growth rate")+
      theme_bw()
  })
})
