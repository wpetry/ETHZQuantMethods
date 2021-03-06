# Server logic for Temperature Model Shiny app

library(shiny)
library(deSolve)
library(dplyr)
library(tidyr)

# define temperature conversion functions
C2K <- function(x){
  x+273.15
}
K2C <- function(x){
  x-273.15
}

# set colorscheme
J_color <- "dodgerblue2"
A_color <- "firebrick2"

# define fixed model parameters
params <- matrix(c(1,293,5,0.1,283,15000,5000,70000,278,295,0.1,5000,0.1,2500),
                 dimnames=list(c("r_Topt","T_opt","s","m_R","T_R","A_m","A_L","A_H","T_L","T_H","d_JR","A_dJ","d_AR","A_dA"),
                               c("value")))

# calculate vital rate plot data, make plot templates
vitalrates <- data.frame(TempC = seq(0, 40, by = 0.1)) %>%
  mutate(TempK = C2K(TempC)) %>%
  mutate(Repro = params["r_Topt",]*exp(-((TempK-params["T_opt",])^2)/(2*(params["s",])^2)),
         Devel = params["m_R",]*
           (TempK/params["T_R",])*
           ((exp(params["A_m",]*
                   ((1/params["T_R",])-(1/TempK))))/
              (1+exp(params["A_L",]*((1/params["T_L",])-(1/TempK)))+exp(params["A_H",]*((1/params["T_H",])-(1/TempK))))),
         Mort_J = params["d_JR",]*exp(params["A_dJ",]*((1/params["T_R",])-(1/TempK))),
         Mort_A = params["d_AR",]*exp(params["A_dA",]*((1/params["T_R",])-(1/TempK))))

t_vital_repro <- ggplot(vitalrates, aes(x = TempC, y = Repro))+
  geom_line(size = 2, color = J_color)+
  scale_x_continuous(name = "Temperature (°C)", limits = c(0, 40))+
  scale_y_continuous(name = "Reproduction", limits = c(0, 1))+
  scale_color_hue()+
  theme_bw()+
  theme(axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        panel.grid = element_blank())
t_vital_devel <- ggplot(vitalrates, aes(x = TempC, y = Devel))+
  geom_line(size = 2, color = J_color)+
  scale_x_continuous(name = "Temperature (°C)", limits = c(0, 40))+
  scale_y_continuous(name = "Development", limits = c(0, 0.2))+
  scale_color_hue()+
  theme_bw()+
  theme(axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        panel.grid = element_blank())
t_vital_mort <- ggplot(vitalrates, aes(x = TempC))+
  geom_line(aes(y = Mort_J), size = 2, color = J_color)+
  geom_line(aes(y = Mort_A), size = 2, color = A_color)+
  scale_x_continuous(name = "Temperature (°C)", limits = c(0, 40))+
  scale_y_continuous(name = "Mortality", limits = c(0, 0.6))+
  scale_color_hue()+
  theme_bw()+
  theme(axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        panel.grid = element_blank())

# define ODE model
TempMod <- function (time, state, Tparams) {
  with(as.list(c(state, Tparams)), {
    dJ = r*A*exp(-A)-m*J-d_J*J
    dA = m*J-d_A*A
    return(list(c(dJ, dA)))
  })
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$vital_repro <- renderPlot({
    t_vital_repro+
      geom_vline(xintercept = input$Temp)
  })
  output$vital_devel <- renderPlot({
    t_vital_devel+
      geom_vline(xintercept = input$Temp)
  })
  output$vital_mort <- renderPlot({
    t_vital_mort+
      geom_vline(xintercept = input$Temp)
  })
  
  output$POPDYN <- renderPlot({
    TempK <- C2K(input$Temp)
    
    # calculate dynamic population parameters
    Tparams <- c(r = params["r_Topt",]*exp(-((TempK-params["T_opt",])^2)/(2*(params["s",])^2)),
                 m = params["m_R",]*
                   (TempK/params["T_R",])*
                   ((exp(params["A_m",]*
                           ((1/params["T_R",])-(1/TempK))))/
                      (1+exp(params["A_L",]*((1/params["T_L",])-(1/TempK)))+exp(params["A_H",]*((1/params["T_H",])-(1/TempK))))),
                 d_J = params["d_JR",]*exp(params["A_dJ",]*((1/params["T_R",])-(1/TempK))),
                 d_A = params["d_AR",]*exp(params["A_dA",]*((1/params["T_R",])-(1/TempK))))
    state <- c(J = input$startJ, A = input$startA)
    time <- seq(0, input$xmax, by = 1)
    
    # project population dynamics
    popdyn <- as.data.frame(ode(func = TempMod, y = state, parms = Tparams, times = time)) %>%
      gather("stage","popdens",2:3)
    
    # draw the population dynamics figure
    ggplot(popdyn, aes(x = time, y = popdens, group = stage, color = stage))+
      geom_line(size = 3)+
      scale_x_continuous(name = "Time", limits = c(0, input$xmax))+
      scale_y_continuous(name = "Population Density", limits = c(0, input$ymax))+
      scale_color_manual(name = "Stage", breaks = c("J", "A"), labels = c("Juveniles", "Adults"), values = c(J_color, A_color))+
      theme_bw()+
      theme(axis.title = element_text(size = 24, face = "bold"),
            axis.text = element_text(size = 18),
            legend.title = element_text(size = 18, face = "bold"),
            legend.text = element_text(size = 16),
            legend.position = "top",
            panel.grid = element_blank())
    
  })
  
})
