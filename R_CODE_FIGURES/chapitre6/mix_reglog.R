

####### "Mélange" de régression logistiques

###### Génération des données

# Simulation of a discretized logit model

n_clusters = 3
n = 1000
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
  vraisemblance_generation[c] <- sum(log_odd)
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


table(data$c,data$y)

# Initialisation al??atoire de c dans 1:K_depart
data$c_map <- data$c_hat <- as.factor(sample(K_depart, size=0.5*n*n_clusters, replace = TRUE))

current_best = 1
library(rpart)
library(nnet)

for (i in 1:max_iter) {
     
     # Initialisations
     reglogs_c_hat = list()
     reglogs_c_map = list()
     predictions_log <- matrix(0,nrow=0.5*n*n_clusters,ncol=nlevels(data$c_hat))
     
     # Apprentissage des régressions p(y | x, c_hat) et remplissage des probabilités prédites
     for (c_iter in 1:nlevels(data$c_hat)) {
          reglogs_c_hat[[c_iter]] <- glm(y ~ ., data = data[data$c_hat==levels(data$c_hat)[c_iter],c("y","x2","x3")], family=binomial(link="logit"))
          predictions_log[,c_iter] <- predict(reglogs_c_hat[[c_iter]],data,type="response")
     }
     
     # Apprentissage des régressions p(y | x, c_map) et calcul de l'AIC "cumulé"
     criterion_iter[[i]] = 0
     
     for (c_iter in 1:nlevels(data$c_map)) {
          reglogs_c_map[[c_iter]] <- glm(y ~ ., data = data[data$c_map==levels(data$c_map)[c_iter],c("y","x2","x3")], family=binomial(link="logit"))
          criterion_iter[[i]] = criterion_iter[[i]] - reglogs_c_map[[c_iter]]$aic
     }
     
     # Mise à jour éventuelle de la meilleure solution actuelle
     if (i>=100 & criterion_iter[[i]] >= criterion_iter[[current_best]]) {
          best_reglogs = reglogs_c_map
          best_link = tryCatch(link,error=function(cond) list())
          current_best = i
     }
     
     # Apprentissage du lien p(c_hat | x)
     link = rpart(c_hat ~ ., data = data[,c("c_hat","x1","x2","x3")])
     link = prune(link,cp= link$cptable[which.min(link$cptable[,"xerror"]),"CP"])
     #link=multinom(c_hat ~ ., data = data[,c("c_hat","x1")])
     
     # Calcul de c_map
     data$c_map <- factor(apply(predict(link,data),1,function(p) names(which.max(p))))
     
     # Tirage du nouveau c_hat
     p <- prop.table(predict(link,data)*(data$y*predictions_log + (1-data$y)*(1-predictions_log)),1) 
     data$c_hat <- factor(apply(p,1,function(row) sample(levels(data$c_hat),1,prob = row)))
     
}

par(mfrow=c(1,1))

plot(unlist(criterion_iter), type="l")

data$c_map <- factor(apply(predict(best_link,data),1,function(p) names(which.max(p))))
test$c_map <- factor(apply(predict(best_link,test),1,function(p) names(which.max(p))))


table(data$c,data$c_map)

plot(data[,1],data[,2],pch=2+data[,3],col=as.numeric(data$c_map),xlab="First coordinate",ylab="Second coordinate")

plot(best_link)
text(best_link, all = TRUE, fancy=TRUE)

library(rattle)
fancyRpartPlot(best_link)


###################################################################
# Résultat de l'approche SEM

pred = matrix(0, nrow = 0.5*n*n_clusters, ncol = 1)

for (j in levels(data$c_map)) {
  modele = glm(y ~ x1 + x2 + x3, data = data[data$c_map==j,], family=binomial(link = "logit"))
  pred[test$c_map==j] = predict(modele, test[test$c_map==j,], type="response")
}

glmdisc::normalizedGini(test$y,pred)


###################################################################
# Résultat de l'approche naïve


modele = glm(y ~ x1 + x2 + x3, data = data, family=binomial(link = "logit"))
pred_tot = predict(modele, test, type="response")
glmdisc::normalizedGini(test$y,pred_tot)



###################################################################
# Résultat de l'approche PLS



library(pls)

target = plsr(y~x1+x2+x3, data=data)

data$pls = 0.0516507841*(data$x1=="2") -0.0536112378*(data$x1=="3") + 0.0144308523*(data$x1=="4") + 0.0002582061*(data$x1=="5") -0.0151194019*(data$x1=="6") -0.3045379821*data$x2 + 0.9493564855*data$x3 < 0
test$pls = 0.0516507841*(test$x1=="2") -0.0536112378*(test$x1=="3") + 0.0144308523*(test$x1=="4") + 0.0002582061*(test$x1=="5") -0.0151194019*(test$x1=="6") -0.3045379821*test$x2 + 0.9493564855*test$x3 < 0

pred = matrix(0, nrow = 0.5*n*n_clusters, ncol = 1)

for (j in c(TRUE,FALSE)) {
  modele = glm(y ~ x1 + x2 + x3, data = data[data$pls==j,], family=binomial(link = "logit"))
  pred[test$pls==j] = predict(modele, test[test$pls==j,], type="response")
}

glmdisc::normalizedGini(test$y,pred)




###################################################################
# Résultat de l'approche FAMD



library(FactoMineR)


mixed = FAMD(data[,c("x1","x2","x3")])

dim_famd = 0.705*test$x2+0.343*test$x3 < 0

pred = matrix(0, nrow = 0.5*n*n_clusters, ncol = 1)

for (j in c(TRUE,FALSE)) {
  modele = glm(y ~ x1 + x2 + x3, data = data[dim_famd==j,], family=binomial(link = "logit"))
  pred[dim_famd==j] = predict(modele, test[dim_famd==j,], type="response")
}

glmdisc::normalizedGini(test$y,pred)




###################################################################
# Résultat de l'oracle


data$c = factor(data$c)
test$c = factor(test$c)

for (j in levels(data$c)) {
  modele = glm(y ~ x2 + x3, data = data[data$c==j,], family=binomial(link = "logit"))
  pred[test$c==j] = predict(modele, test[test$c==j,], type="response")
}

glmdisc::normalizedGini(test$y,pred)






