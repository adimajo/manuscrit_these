

library(tidyverse)
library(party)
library(partykit)
library(RWeka)

# Génération des données

####### "Mélange" de régression logistiques

###### Génération des données

# Simulation of a discretized logit model

n_clusters = 3
n = 100000
d = 3

K_depart = 6
max_iter = 1000

data = matrix(0,nrow = n*n_clusters,ncol = d+2)
vraisemblance_generation = vector("numeric", length = n_clusters)

theta = list()
theta[[1]] = c(0,-1,0.5)
theta[[2]] = c(0,-0.5,1.5)
theta[[3]] = c(0,1,-0.5)

for (c in seq(2,2*n_clusters,2)) {
  set.seed(c/2)
  x_cont = matrix(rnorm(n*(d-1),mean = 0, sd = 1.5), nrow = n, ncol = d-1)
  x_cat = replicate(n,sample(c(c-1,c),1,c(0.5,0.5),replace=TRUE))
  x = cbind(x_cat,x_cont)
  #theta = c(0,runif(1,min = -0.8, max = -0.2),runif(1,min = 0.2, max = 0.6))
  log_odd = apply(x_cont, 1, function(row) theta[[c/2]][1] + t(theta[[c/2]][2:(d)]) %*% row)
  y = rbinom(n,1,1/(1+exp(-log_odd)))
  data[(1+(c/2-1)*n):(c/2*n),] <- cbind(x,y,rep(c/2,n))
  vraisemblance_generation[c/2] <- sum(log_odd)
}

data <- data.frame(data)
colnames(data) <- c("x1","x2","x3","y","c")
data$x1 <- as.factor(data$x1)

int_train = sample.int(n = 3*n, size = 0.5*n_clusters*n)

test = data[-int_train,]
data = data[int_train,]

criterion_iter=list()

par(mfrow=c(2,2))
for (c in 1:n_clusters) {
  plot(data[data$c==c,2],data[data$c==c,3],col=data[data$c==c,4]+1,xlab="First coordinate",ylab="Second coordinate")
}

# MOB

mob_data = glmtree(y ~ x1 + x2 + x3 | x1 + x2 + x3, data = data, family = binomial(link="logit"))
plot(mob_data)
print(mob_data)


# LMT

data$y = factor(data$y)
lmt_data = LMT(y ~ ., data = data[,c("y","x1","x2","x3")])
plot(lmt_data)
print(lmt_data)

