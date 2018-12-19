##############################
####### Adrien Ehrhardt ######
##############################
### Simulation figures 2.3 ###
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

# 1ers résultats avec un jeu de données

list2env(generate_data(1,100),env=environment())

# Représentation des données en 1D

hist(x[,1],xlab="First coordinate",main=NULL,breaks = c(0,1/3,2/3,1))
abline(v=1/3)
abline(v=2/3)

# Représentation des données en 2D

tikz(file = '2D_discretization_plot.tex')
plot(x[,1],x[,2],pch=2+y,xlab="First coordinate x[,1]",ylab="Second coordinate x[,2]")
legend(0,1,c("Class Y = 0", "Class Y = 1","p(1 | x)"), pch = c(2,3,-1))
abline(v=1/3)
abline(v=2/3)
abline(h=1/3)
abline(h=2/3)
for (j in (1:3)) {
  for(k in (1:3)) {
    prob = round(1/(1+exp(-log_odd[which(xd[,1]==j&xd[,2]==k)])),3)
    text(j/3-1/6,k/3-1/6,prob)
  }
}
dev.off()

##############################
### Précision d'estimation ###
##############################

# 100 simulations consécutives à 3 discrétisations pour n=100 et 3 MCMC
nb_intervalles_big = list()
intervalles_big = list()

for (l in 1:100) {
  x_test = matrix(runif(2*10000), nrow = 10000, ncol = 2)
  
  list2env(generate_data(l,100),env=environment())
  
  MCMC = tryCatch(glmdisc(predictors = x, labels = y, interact=F, test=F, validation=F, iter=20, m_start=3, criterion = "bic"), error = function(e) FALSE)

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


# 100 simulations consécutives à 3 discrétisations pour n=1000

nb_intervalles_big = list()
intervalles_big = list()

for (l in 1:100) {
  x_test = matrix(runif(2*10000), nrow = 10000, ncol = 2)
  
  list2env(generate_data(l,1000),env=environment())
  
  MCMC = tryCatch(glmdisc(predictors = x, labels = y, interact=F, test=F, validation=F, iter=200, m_start=3, criterion = "bic"), error = function(e) FALSE)
  
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


first_cut_first_features = list()
second_cut_first_features = list()
first_cut_second_features = list()
second_cut_second_features = list()

for (l in 1:100) {
     if (!(nb_intervalles_big[[l]])==0) {
          if (nb_intervalles_big[[l]][[1]]==3 & nb_intervalles_big[[l]][[2]]==3) {
               first_cut_first_features[[l]] = intervalles_big[[l]][[1]][1]
               second_cut_first_features[[l]] = intervalles_big[[l]][[1]][2]
               first_cut_second_features[[l]] = intervalles_big[[l]][[2]][1]
               second_cut_second_features[[l]] = intervalles_big[[l]][[2]][2]
          }
     }
}

summary(unlist(first_cut_first_features))
summary(unlist(second_cut_first_features))
summary(unlist(first_cut_second_features))
summary(unlist(second_cut_second_features))

tableau = matrix(0,ncol=2,nrow=2)

tableau[1,1] = paste0("[",round(t.test(unlist(first_cut_first_features))$conf.int[1],digits=3),",",round(t.test(unlist(first_cut_first_features))$conf.int[2],digits=3),"]")
tableau[1,2] = paste0("[",round(t.test(unlist(second_cut_first_features))$conf.int[1],digits=3),",",round(t.test(unlist(second_cut_first_features))$conf.int[2],digits=3),"]")
tableau[2,1] = paste0("[",round(t.test(unlist(first_cut_second_features))$conf.int[1],digits=3),",",round(t.test(unlist(first_cut_second_features))$conf.int[2],digits=3),"]")
tableau[2,2] = paste0("[",round(t.test(unlist(second_cut_second_features))$conf.int[1],digits=3),",",round(t.test(unlist(second_cut_second_features))$conf.int[2],digits=3),"]")

dimnames(tableau) = list(c("$\\hat{s}_{1,1}$","$\\hat{s}_{1,2}$"),c("$\\hat{s}_{2,1}$","$\\hat{s}_{2,2}$"))

print(xtable(tableau,caption="95 \\% confidence intervals around the estimated cut-offs $s_{1,j} = 1/3$ and $s_{2,j} = 2/3$ for $n=1000$",digits = c(0,3,3)),sanitize.text.function=function(x){x}, type="latex", file="table_2.3.tex")

##############################
##### Nombre intervalles #####
##############################

# 100 simulations consécutives à 3 discrétisations pour n=100 - m_start = 10
nb_intervalles_big = list()
intervalles_big = list()

for (l in 1:100) {
     x_test = matrix(runif(2*10000), nrow = 10000, ncol = 2)
     
     list2env(generate_data(l,100),env=environment())
     x = cbind(x,matrix(runif(3*100), nrow = 100, ncol = 3))
     
     MCMC = tryCatch(glmdisc(predictors = x, labels = y, interact=F, test=F, validation=F, iter=100, m_start=10, criterion = "bic"), error = function(e) FALSE)
     
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


# 100 simulations consécutives à 3 discrétisations pour n=1000 - m_start = 10


##############################
### Elimination variables ####
##############################

# 100 simulations consécutives à 3 discrétisations pour n=100 - 3 variables inutiles
nb_intervalles_big = list()
intervalles_big = list()

for (l in 1:100) {
     x_test = matrix(runif(5*10000), nrow = 10000, ncol = 5)
     
     list2env(generate_data(l,100),env=environment())
     x = cbind(x,matrix(runif(3*100), nrow = 100, ncol = 3))
     
     MCMC = tryCatch(glmdisc(predictors = x, labels = y, interact=F, test=F, validation=F, iter=60, m_start=3, criterion = "bic"), error = function(e) FALSE)
     
     if (!mode(MCMC)=="logical") {
          discrete_test = discretize(MCMC,x_test)
          
          nb_intervalles = list()
          intervalles = list()
          
          for (j in 1:5) {
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


# 100 simulations consécutives à 3 discrétisations pour n=1000 - 3 variables inutiles

nb_intervalles_big = list()
intervalles_big = list()

for (l in 1:100) {
     x_test = matrix(runif(5*10000), nrow = 10000, ncol = 5)
     
     list2env(generate_data(l,1000),env=environment())
     set.seed(l*l)
     x = cbind(x,matrix(runif(3*1000), nrow = 1000, ncol = 3))
     
     MCMC = tryCatch(glmdisc(predictors = x, labels = y, interact=F, test=F, validation=F, iter=300, m_start=3, criterion = "bic"), error = function(e) FALSE)
     
     if (!mode(MCMC)=="logical") {
          discrete_test = discretize(MCMC,x_test)
          
          nb_intervalles = list()
          intervalles = list()
          
          for (j in 1:5) {
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


