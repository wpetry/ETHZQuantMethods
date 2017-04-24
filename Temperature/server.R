# Server logic for Temperature Model Shiny app

library(shiny)
library(deSolve)
library(tidyverse)

# define temperature conversion functions
C2K <- function(x){
  x+273.15
}
K2C <- function(x){
  x-273.15
}

# define model parameters
params <- matrix(c(1,293,5,0.1,283,15000,5000,70000,278,295,0.1,5000,0.1,2500),
                 dimnames=list(c("r_Topt","T_opt","s","m_R","T_R","A_m","A_L","A_H","T_L","T_H","d_JR","A_dJ","d_AR","A_dA"),
                               c("value")))

# calculate vital rate plot data
vitalrates <- data.frame(TempK = C2K(seq(1, 40, by = 0.1))) %>%
  mutate(Repro = params["r_Topt",]*exp(-((TempK-params["T_opt",])^2)/(2*(params["s",])^2)),
         Devel_J = params["d_JR",]*exp(params["A_dJ",]*((1/params["T_R",])-(1/TempK))),
         Devel_A = params["d_AR",]*exp(params["A_dA",]*((1/params["T_R",])-(1/TempK))),
         Mort = params["m_R",]*
           (TempK/params["T_R",])*
           ((exp(params["A_m",]*
                   ((1/params["T_R",])-(1/TempK))))/
              (1+exp(params["A_L",]*((1/params["T_L",])-(1/TempK)))+exp(params["A_H",]*((1/params["T_H",])-(1/TempK))))))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$POPDYN <- renderPlot({
    TempK <- C2K(input$Temp)
    
    # calculate population parameters
    Tparams <- c(r = params["r_Topt",]*exp(-((TempK-params["T_opt",])^2)/(2*(params["s",])^2)),
                 m = params["m_R",]*
                   (TempK/params["T_R",])*
                   ((exp(params["A_m",]*
                           ((1/params["T_R",])-(1/TempK))))/
                      (1+exp(params["A_L",]*((1/params["T_L",])-(1/TempK)))+exp(params["A_H",]*((1/params["T_H",])-(1/TempK))))),
                 d_J = params["d_JR",]*exp(params["A_dJ",]*((1/params["T_R",])-(1/TempK))),
                 d_A = params["d_AR",]*exp(params["A_dA",]*((1/params["T_R",])-(1/TempK))))
    state <- c(J = input$startJ, A = input$startA)
    time <- seq(0, 100, by = 1)
    
    # project population dynamics
    popdyn <- as.data.frame(ode(func = TempMod, y = state, parms = Tparams, times = time)) %>%
      gather("stage","popdens",2:3)
    
    # draw the population dynamics figure
    ggplot(popdyn, aes(x = time, y = popdens, group = stage, color = stage))+
      geom_line(size = 3)+
      scale_x_continuous(name = "Time")+
      scale_y_continuous(name = "Population Density", limits = c(0, input$ymax))+
      scale_color_discrete(name = "Stage", breaks = c("J", "A"), labels = c("Juveniles", "Adults"))+
      theme_bw()+
      theme(axis.title = element_text(size = 24, face = "bold"),
            axis.text = element_text(size = 18),
            legend.title = element_text(size = 18, face = "bold"),
            legend.text = element_text(size = 16),
            panel.grid = element_blank())
    
  })
  
})
