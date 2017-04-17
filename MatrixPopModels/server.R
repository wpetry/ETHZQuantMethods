# Server logic for Matrix Population Model app

library(shiny)
library(rhandsontable)
library(popbio)
library(ggplot2)
library(dplyr)
library(htmlwidgets)

# DF is the user interface, hideDF will stay in background to populate DF when
# matrix dimensions change
data(hudsonia)
hideDF <- DF <- hudsonia$A88
n<-1000
#ts<-250

VEC<-matrix(c(n,0,0,0,0,0),ncol=1,dimnames=list(paste0("c",1:6),"n"))

# Define server logic to take user input matrix model and project population dynamics
shinyServer(function(input, output) {
   
  values <- reactiveValues()  # is this doing anything?
  
  ## Handsontable transition matrix
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
    if (!is.null(DF)){
      if (input$nbins>1){
        rhandsontable(DF, useTypes = T, stretchH = "none", digits = 6,
                      colHeaders=paste0("c",1:input$nbins),
                      rowHeaders=paste0("c",1:input$nbins)) %>%
          hot_cols(renderer=JS("safeHtmlRenderer"))
      }else{
        rhandsontable(DF[1,1], useTypes = T, stretchH = "none", digits = 6,
                      colHeaders="c1", rowHeaders="c1") %>%
          hot_cols(renderer=JS("safeHtmlRenderer"))
      }
    }
  })
  
  ## Handsontable population vector
  observe({
    if (!is.null(input$hotvec)) {
      #VEC = hot_to_r(input$hotvec)
      
      VEC = matrix(matrix(hot_to_r(input$hotvec))[1:input$nbins,],dimnames=list(paste0("c",1:input$nbins),"n"))
    } else {
      if (is.null(values[["VEC"]])){
        VEC <- VEC
      }else{
        VEC <- values[["VEC"]]
      }
    }
    values[["VEC"]] <- VEC
  })
  
  output$hotvec <- renderRHandsontable({
    VEC <- values[["VEC"]]
    if (!is.null(VEC)){
      rhandsontable(VEC, useTypes = T, stretchH = "none", digits = 6,
                    colHeaders = "n", rowHeaders = "") %>%
        hot_cols(renderer=JS("safeHtmlRenderer"))
    }
  })
  
  
  output$lambda <- renderPlot({
    lammat<-matrix(c(values[["VEC"]],
                     rep(0,times=input$nbins*input$ts)),
                   ncol=input$ts+1)
    for(i in 1:input$ts){
      lammat[,i+1]<-values[["DF"]] %*% lammat[,i]
    }
    n<-colSums(lammat)
    simpop<-data.frame(year=0:input$ts,lam=n/lag(n)) %>% na.omit(.)
    
    suppressWarnings(ggplot(data=simpop,aes(x=year,y=lam))+
                       geom_hline(yintercept=1,color="grey",size=1)+
                       geom_line(size=3,color="dodgerblue3")+
                       scale_x_continuous(name="Time steps",expand=c(0,0))+
                       scale_y_continuous(name="Population growth rate (λ)",limits=c(0,NA),expand=c(0.01,0))+
                       theme_bw()+
                       theme(aspect.ratio=1,
                             panel.grid=element_blank(),
                             axis.text=element_text(size=18),
                             axis.title.x=element_text(size=26,vjust=-1),
                             axis.title.y=element_text(size=26,vjust=2)))
  })
})
