#################################################
## Frequency-dependent matrix models: Two-sexes
## W. Petry
#################################################
## 0. Preliminaries
#################################################
library(popbio)
library(dplyr)
library(tidyr)
library(ggplot2)

## define simulation functions
# mating functions
B.fem <- function(nFM){
  return(1)
}
B.logis <- function(nFM){
  min(c(1, 1.1/(1+0.1*exp(-8*(qlogis(nFM[2]/sum(nFM)))))))
}

# simulate (non)linear population dynamics
simpops<-function(U_mat, F_mat, startpop = matrix(100, nrow = nrow(U_mat), ncol = 1),
                  Bfxn = B.fem, iterate = 100){
  # initialize variables
  dynamic.mod <- linear.mod <- U_mat + F_mat
  popvecs <- matrix(0, nrow = nrow(U_mat), ncol = iterate+1)
  popvecs[, 1] <- startpop
  cutoff <- nrow(startpop)/2
  # iterate
  for(i in 1:iterate){
    # modify projection matrix based on mating function
    nFM <- c(popvecs[cutoff, i], popvecs[cutoff*2, i])
    B <- Bfxn(nFM)
    dynamic.mod <- U_mat + F_mat*B
    popvecs[, i+1] <- dynamic.mod %*% popvecs[, i]
  }
  return(popvecs)
}

# function to calculate lambda from simulated populations
Lag <- function(x, shift = 1){
    xLen <- length(x)
    if (shift == 0) 
      return(x)
    ret <- as.vector(character(xLen), mode = storage.mode(x))
    attrib <- attributes(x)
    if (length(attrib$label)) 
      attrib$label <- paste(attrib$label, "lagged", shift, 
                            "observations")
    if (abs(shift) < xLen) {
      if (shift > 0) 
        ret[-(1:shift)] <- x[1:(xLen - shift)]
      else ret[1:(xLen + shift)] <- x[(1 - shift):xLen]
    }
    attributes(ret) <- attrib
    return(ret)
}

lambda.ts <- function(x){
  n.t1 <- colSums(x)
  n.t <- Hmisc::Lag(n.t1)
  return(n.t1/n.t)
}

# color options
palette <- c("#00A08A", "#5BBCD6", "#F2AD00")

#################################################
## 1. A paradoxical example
#################################################
## Set up matrices with different survival scenarios
alleq_U <- matrix(c(0, 0, 0, 0, 0, 0, 
                    0.3, 0, 0, 0, 0, 0, 
                    0, 0.3, 0.5, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0.3, 0, 0, 
                    0, 0, 0, 0, 0.3, 0.5), nrow = 6, byrow = TRUE)
alleq_F <- matrix(c(0, 0, 10, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 
                    0, 0, 10, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0), nrow = 6, byrow = TRUE)
alleq_A <- alleq_U+alleq_F

# increase survival of FEMALES by 50% (=average survival rate increase of 25%)
finc_U <- matrix(c(0, 0, 0, 0, 0, 0, 
                    0.45, 0, 0, 0, 0, 0, 
                    0, 0.45, 0.75, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0.3, 0, 0, 
                    0, 0, 0, 0, 0.3, 0.5), nrow = 6, byrow = TRUE)
finc_A <- finc_U+alleq_F

# increase survival of BOTH SEXES by 25% (=average survival rate increase of 25%)
bothinc_U <- matrix(c(0, 0, 0, 0, 0, 0, 
                   0.45, 0, 0, 0, 0, 0, 
                   0, 0.45, 0.75, 0, 0, 0, 
                   0, 0, 0, 0, 0, 0, 
                   0, 0, 0, 0.45, 0, 0, 
                   0, 0, 0, 0, 0.45, 0.75), nrow = 6, byrow = TRUE)
bothinc_A <- bothinc_U+alleq_F

# define initial population vector
pop0 <- matrix(c(0.3, 0.1, 0.1, 0.3, 0.1, 0.1))

# project models
pops_alleq <- simpops(U_mat = alleq_U, F_mat = alleq_F, startpop = pop0, Bfxn = B.logis)
ts_alleq <- lambda.ts(pops_alleq)

pops_finc <- simpops(U_mat = finc_U, F_mat = alleq_F, startpop = pop0, Bfxn = B.logis)
ts_finc <- lambda.ts(pops_finc)

pops_bothinc <- simpops(U_mat = bothinc_U, F_mat = alleq_F, startpop = pop0, Bfxn = B.logis)
ts_bothinc <- lambda.ts(pops_bothinc)

# make plots
# combine lambda time series into dataframe for plotting
plotdat <- data.frame(time = 1:length(ts_alleq),
                    alleq = ts_alleq, finc = ts_finc, bothinc = ts_bothinc) %>%
  na.omit(.) %>%
  gather("survival", "value", -time)

# plot all equal alone
ggplot(plotdat %>% filter(survival == "alleq"),
       aes(x = time, y = value, group = survival, color = survival))+
  geom_hline(yintercept = 1, size = 0.5)+
  geom_line(size = 3, color = palette[1])+
  scale_x_continuous(name = "Time", limits = c(0, 31), expand = c(0, 0))+
  scale_y_continuous(name = expression(bold(paste("Population growth rate ",
                                                  bold(lambda)))), limits = c(0, 2.5),
                     expand = c(0, 0))+
  scale_color_manual(name = "", breaks = c("alleq"), labels = c("Control"), values = palette)+
  theme_classic()+
  theme(legend.position = c(0.75, 0.85), 
        legend.text = element_text(size = 18), 
        axis.title = element_text(face = "bold", size = 20), 
        axis.text = element_text(size = 18), 
        panel.grid = element_blank())
# plot all equal + female increase
ggplot(plotdat %>% filter(survival %in% c("alleq", "finc")), 
       aes(x = time, y = value, group = survival, color = survival))+
  geom_hline(yintercept = 1, size = 0.5)+
  geom_line(size = 3)+
  scale_x_continuous(name = "Time", limits = c(0, 31), expand = c(0, 0))+
  scale_y_continuous(name = expression(bold(paste("Population growth rate ",
                                                  bold(lambda)))), limits = c(0, 2.5),
                     expand = c(0, 0))+
  scale_color_manual(name = "", breaks = c("alleq", "finc"), 
                     labels = c("Control", "+25% average survival"), values = palette)+
  theme_classic()+
  theme(legend.position = c(0.75, 0.85), 
        legend.text = element_text(size = 18), 
        axis.title = element_text(face = "bold", size = 20), 
        axis.text = element_text(size = 18), 
        panel.grid = element_blank())
# plot all equal + female + both increase
ggplot(plotdat, aes(x = time, y = value, group = survival, color = survival))+
  geom_hline(yintercept = 1, size = 0.5)+
  geom_line(size = 3)+
  scale_x_continuous(name = "Time", limits = c(0, 31), expand = c(0, 0))+
  scale_y_continuous(name = expression(bold(paste("Population growth rate ",
                                                  bold(lambda)))), limits = c(0, 2.5),
                     expand = c(0, 0))+
  scale_color_manual(name = "", breaks = as.ordered(c("alleq", "finc", "bothinc")),
                     labels = c("Control", "+50% survival, females",
                                "+25% survival, both sexes"), values = palette[c(1, 3, 2)])+
  theme_classic()+
  theme(legend.position = c(0.75, 0.85), 
        legend.text = element_text(size = 18), 
        axis.title = element_text(face = "bold", size = 20), 
        axis.text = element_text(size = 18), 
        panel.grid = element_blank())

#################################################
## 2. Simulation vs. analytical population dynamics
#################################################
image2(alleq_A)
image2(bothinc_A)
image2(finc_A)

# sexes are demographically identical (i.e., no biased sex ratio):
lambda(alleq_A)  # eigenvalue of A matrix
ts_alleq[length(ts_alleq)]  # simulation-based

lambda(bothinc_A)
ts_bothinc[length(ts_bothinc)]

# sexes have different demographic rates:
lambda(finc_A)
ts_finc[length(ts_finc)]

## simulate effect of biased sex ratio in starting population
biasedpop0 <- matrix(c(0, 0, 17, 0, 0, 3))
pops_biased_alleq <- simpops(U_mat = alleq_U, F_mat = alleq_F, startpop = biasedpop0,
                             Bfxn = B.logis)
ts_biased_alleq <- lambda.ts(pops_biased_alleq)
lambda(alleq_A)  # eigenvalue of A matrix
ts_biased_alleq[length(ts_biased_alleq)]  # simulation-based

