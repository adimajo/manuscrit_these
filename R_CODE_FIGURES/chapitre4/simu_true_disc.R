##############################
####### Adrien Ehrhardt ######
##############################
#### Simulations COMPSTAT ####
##############################

# Outils

library(glmdisc)
library(tikzDevice)
library(xtable)
library(dplyr)
setwd("C:/Users/s36612/Documents/13206528qssppdptvshn/R_CODE_FIGURES/chapitre4")

# Simulation des données

generate_data <- function(k,n) {
  set.seed(k)
  x = matrix(runif(2*n), nrow = n, ncol = 2)

  log_odd = sin((x[,1]-0.7)*7)
  y = rbinom(n,1,1/(1+exp(-log_odd)))
  return(list(x=x,y=y,log_odd=log_odd))
}

# Figure vraie distribution

list2env(generate_data(1,5000),env=environment())

tikz(file = 'true_data_plot.tex', width = 8, height = 3.2)
plot(sort(x[,1]),sin((sort(x[,1])-0.7)*7), type="l", xlab = "x", ylab = "logit(p(1|x))", col = "green", lwd=2)
legend(0.3,1,c("True distribution"), lty=1,col = "green", lwd=2)
dev.off()

# Figure régression logistique linéaire

coeff_x_1 = glm(y~V1, data=data.frame(cbind(x,y)),family=binomial(link="logit"))$coefficients[c("(Intercept)","V1")]

tikz(file = 'linear_plot.tex', width = 8, height = 3.2, engine="pdftex")
plot(sort(x[,1]),sin((sort(x[,1])-0.7)*7), type="l", xlab = "x", ylab = "logit(p(1|x))", col = "green", lwd=2)
lines(sort(x[,1]),(coeff_x_1["(Intercept)"]+coeff_x_1["V1"]*x[,1])[order(x[,1])], type="l", col = "red", lwd=2)
legend(0.3,1,c("True distribution","Linear logistic regression"), lty=1,col = c("green","red"), lwd=2)
dev.off()

# Figure régression logistique discrète mal choisie

library(infotheo)

l = generate_data(2,5000)

x_test = l[[1]]
y_test = l[[2]]
log_odd_test = l[[3]]

bic_list = NULL

for (k in 2:100) {
  x_disc = discretize(c(x[,1],x_test[,1]), disc="equalfreq", nbins=k)
  x_disc_test = x_disc[5001:10000,]
  x_disc = x_disc[1:5000,]
  
  coeff_x_disc = glm(y~., data=data.frame(x=factor(x_disc),y),family=binomial(link="logit"))
  intervals = confint(coeff_x_disc)
  coeff_x_disc_low = summary(coeff_x_disc)$coefficients[,1]+1.96*summary(coeff_x_disc)$coefficients[,2]
  coeff_x_disc_high = summary(coeff_x_disc)$coefficients[,1]-1.96*summary(coeff_x_disc)$coefficients[,2]
  bic_list = c(bic_list,normalizedGini(y_test,predict(coeff_x_disc,data.frame(x=factor(x_disc_test),y_test))))
  coeff_x_disc = coeff_x_disc$coefficients
  
  pred = matrix(NA,5000)
  predlow = matrix(NA,5000)
  predhigh = matrix(NA,5000)
  
  for (k in (1:nlevels(factor(x_disc)))) {
    pred[factor(x_disc) == levels(factor(x_disc))[k],1] = coeff_x_disc[k]
    predlow[factor(x_disc) == levels(factor(x_disc))[k],1] = coeff_x_disc_low[k]
    predhigh[factor(x_disc) == levels(factor(x_disc))[k],1] = coeff_x_disc_high[k]
  }
  
  redtrans <- rgb(255, 0, 0, 127, maxColorValue=255) 
  
  tikz(file = paste0('disc_plot',k,'.tex'), width = 8, height = 3.2, engine="pdftex")
  plot(sort(x[,1]),sin((sort(x[,1])-0.7)*7), type="l", xlab = "x", ylab = "logit(p(1|x))", col = "green", lwd=2)
  lines(sort(x[,1]), (coeff_x_disc["(Intercept)"] + pred)[order(x[,1])], col = "red", lwd=2)
  polygon(c(sort(x[,1]), rev(sort(x[,1]))), c((coeff_x_disc_low["(Intercept)"] + predlow)[order(x[,1])], (coeff_x_disc_high["(Intercept)"] + predhigh)[order(x[,1])]),
          col=redtrans, border = NA)
  legend(0.3,1,c("True distribution","Bad discretization"), lty=c(1,1),col =c("green","red"), lwd=2)
  dev.off()

}

