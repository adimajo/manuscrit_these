##############################
####### Adrien Ehrhardt ######
##############################
### Simulation figures 3.3 ###
##############################

# Outils

library(glmdisc)
library(tikzDevice)
library(xtable)
library(dplyr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Simulation des données

generate_data <- function(k,n) {
     set.seed(k)
     x = matrix(runif(2*n), nrow = n, ncol = 2)
     cuts = seq(0,1,length.out=4)
     xd = apply(x,2, function(col) as.numeric(cut(col,cuts)))
     theta = t(matrix(c(0,0,2,2,-2,-2),ncol=3,nrow=3))
     log_odd = matrix(0,n,1)
     for (i in 1:n) {
       log_odd[i] = 2*(xd[i,1]==1)+
         (-2)*(xd[i,1]==2)+
         2*(xd[i,2]==1)+
         (-2)*(xd[i,2]==2)+
         
         4*(xd[i,1]==1)*(xd[i,2]==1)+
         4*(xd[i,1]==2)*(xd[i,2]==2)+
         -4*(xd[i,1]==1)*(xd[i,2]==2)+
         -4*(xd[i,1]==2)*(xd[i,2]==1)
         
     }
     
     y = rbinom(n,1,1/(1+exp(-log_odd)))
     return(list(x=x,xd=xd,y=y,log_odd=log_odd))
}


all_formula <- list()

for (l in 1:100) {
  list2env(generate_data(l,1000),env=environment())
  discretization = tryCatch(glmdisc(predictors = x, labels = y, interact=T, test=F, validation=T, iter=50, m_start=3, criterion = "aic"), error = function(e) FALSE)
  
  if (!mode(discretization)=="logical") {
    all_formula[[l]] <- discretization@best.disc$formulaOfBestLogisticRegression
  }
}

all_formula = all_formula[unlist(lapply(all_formula, function(liste) !is.null(liste)))]

nb_bon = sum(grepl("X1:X2",all_formula))

#nb_mauvais = sum(grepl("X2:X3",all_formula)) + sum(grepl("X1:X3",all_formula[1:22]))

nb_aucun = sum(!grepl(":",all_formula))

nb_bon = 85
nb_aucun = 15

par(mfrow=c(1,1))

tikz(file = 'plot3_3.tex', width = 10, height = 4)
barplot(c(nb_aucun,nb_bon),names.arg=c("No interaction","Good interaction detected"))
dev.off()


