##############################
####### Adrien Ehrhardt ######
##############################
### Simulation figures 2.4 ###
##############################

# Outils

library(glmdisc)
library(tikzDevice)
library(xtable)
library(dplyr)
setwd("~/overleaf/13215824whymrvkrsdsv/figures")

# Simulation des données

generate_data <- function(k,n) {
  set.seed(k)
  x = matrix(runif(2*n), nrow = n, ncol = 2)
  cuts = seq(0,1,length.out= 4)
  xd = apply(x,2, function(col) as.numeric(cut(col,cuts)))
  theta = t(matrix(c(0,0,2,2,-2,-2),ncol=3,nrow=2))
  log_odd = rowSums(t(sapply(seq_along(xd[,1]), function(row_id) sapply(seq_along(xd[row_id,]),function(element) theta[xd[row_id,element],element]))))
  y = rbinom(n,1,1/(1+exp(-log_odd)))
  return(list(x=x,xd=xd,y=y,log_odd=log_odd))
}

##############################
### Elimination variables ####
##############################

# 100 simulations consécutives à 3 discrétisations pour n=100 - 3 variables inutiles
nb_intervalles_big = list()
intervalles_big = list()

for (l in 1:200) {
     x_test = matrix(runif(2*10000), nrow = 10000, ncol = 2)
     
     list2env(generate_data(l,100),env=environment())
     #x = cbind(x,matrix(runif(3*100), nrow = 100, ncol = 3))
     
     MCMC = tryCatch(glmdisc(predictors = x, labels = y, interact=F, test=F, validation=F, iter=500, m_start=10, criterion = "bic"), error = function(e) FALSE)
     
     if (!mode(MCMC)=="logical") {
          discrete_test = discretize(MCMC,x_test)
          
          nb_intervalles = list()
          intervalles = list()
          
          for (j in 1:2) {
               ordonne = cbind(x_test[,j],as.numeric(discrete_test[,j]))
               colnames(ordonne) = list("cont","disc")
               ordonne = data.frame(ordonne)
               tab_summary = summarise(group_by(ordonne,disc),minimum=min(cont),maximum=max(cont))
               tab_cut = sort(c(tab_summary$minimum,tab_summary$maximum))[2:(length(c(tab_summary$minimum,tab_summary$maximum))-1)]
               
               nb_intervalles[[j]] = length(tab_cut)/2 + 1
               
               intervalles[[j]] = (tab_cut[seq(1,length(tab_cut),by=2)]+tab_cut[seq(1,length(tab_cut),by=2)+1])/2
          }
          
          nb_intervalles_big[[l]] = nb_intervalles
          intervalles_big[[l]] = intervalles
     } else {
          nb_intervalles_big[[l]] = 0
          intervalles_big[[l]] = list()
     }
}

tikz(file = 'plot2_4.tex', width = 10, height = 4)
first_attribute = na.omit(unlist(lapply(nb_intervalles_big, function(liste) ifelse(liste[[1]]==0,NA,liste[[1]]))))[1:100]
second_attribute = na.omit(unlist(lapply(nb_intervalles_big, function(liste) ifelse(liste[[1]]==0,NA,liste[[2]]))))[1:100]
par(mfrow=c(1,2))
plot(factor(first_attribute), ylim = c(0,60), xlab = "First attribute", ylab = "Freq")
plot(factor(second_attribute), ylim = c(0,60), xlab = "Second attribute", ylab = "Freq")
dev.off()

