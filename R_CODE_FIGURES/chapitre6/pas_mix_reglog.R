setwd(dirname(rstudioapi::getSourceEditorContext()$path))


####### "Mélange" de régression logistiques

###### Génération des données

# Simulation of a discretized logit model

n_clusters = 3
n = 1000
d = 2

max_iter = 1000

data = matrix(0,nrow = n*n_clusters,ncol = d+1)
vraisemblance_generation = vector("numeric", length = n_clusters)

theta = c(3,0.5,-1)

for (c in 1:n_clusters) {
  set.seed(c)
  x = matrix(rnorm(n*d,mean = 3*c, sd = 1), nrow = n, ncol = d)
  log_odd = apply(x, 1, function(row) theta[1] + t(theta[2:(d+1)]) %*% row)
  y = rbinom(n,1,1/(1+exp(-log_odd)))
  data[(1+(c-1)*n):(c*n),] <- cbind(x,y)
  vraisemblance_generation[c] <- sum(log_odd)
}

data <- data.frame(data)
colnames(data) <- c("x1","x2","y")


int_train = sample.int(n = 3*n, size = 0.5*n_clusters*n)

test = data[-int_train,]
data = data[int_train,]

criterion_iter=list()

tikz("graph_pas_mix_reglog.tex", standAlone=FALSE, width = 12, height = 6, fg = "black", sanitize = FALSE)
plot(data$x1,data$x2, col = data$y+1,xlab="First coordinate",ylab="Second coordinate")
dev.off()

K_depart = 4

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
    reglogs_c_hat[[c_iter]] <- glm(y ~ ., data = data[data$c_hat==levels(data$c_hat)[c_iter],c("y","x1","x2")], family=binomial(link="logit"))
    predictions_log[,c_iter] <- predict(reglogs_c_hat[[c_iter]],data,type="response")
  }
  
  # Apprentissage des régressions p(y | x, c_map) et calcul de l'AIC "cumulé"
  criterion_iter[[i]] = 0
  
  for (c_iter in 1:nlevels(data$c_map)) {
    reglogs_c_map[[c_iter]] <- glm(y ~ ., data = data[data$c_map==levels(data$c_map)[c_iter],c("y","x1","x2")], family=binomial(link="logit"))
    criterion_iter[[i]] = criterion_iter[[i]] - reglogs_c_map[[c_iter]]$aic
  }
  
  # Mise à jour éventuelle de la meilleure solution actuelle
  if (i>=100 & criterion_iter[[i]] >= criterion_iter[[current_best]]) {
    best_reglogs = reglogs_c_map
    best_link = tryCatch(link,error=function(cond) list())
    current_best = i
  }
  
  # Apprentissage du lien p(c_hat | x)
  link = rpart(c_hat ~ ., data = data[,c("c_hat","x1","x2")], method = "class")
  #link = prune(link,cp= link$cptable[which.min(link$cptable[,"xerror"]),"CP"])
  link = prune(link,cp= 0.1)
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


table(data$c_map)

plot(data[,1],data[,2],pch=2+data[,3],col=as.numeric(data$c_map),xlab="First coordinate",ylab="Second coordinate")

plot(best_link)
text(best_link, all = TRUE, fancy=TRUE)

library(rattle)
fancyRpartPlot(best_link)


###################################################################
# Résultat de l'approche SEM

pred = matrix(0, nrow = 0.5*n*n_clusters, ncol = 1)

for (j in levels(data$c_map)) {
  modele = glm(y ~ x1 + x2, data = data[data$c_map==j,], family=binomial(link = "logit"))
  pred[test$c_map==j] = predict(modele, test[test$c_map==j,], type="response")
}

glmdisc::normalizedGini(test$y,pred)


###################################################################
# Résultat de l'approche naïve


modele = glm(y ~ x1 + x2, data = data, family=binomial(link = "logit"))
pred_tot = predict(modele, test, type="response")
glmdisc::normalizedGini(test$y,pred_tot)



###################################################################
# Résultat de l'approche PLS



library(pls)

target = plsr(y~x1+x2, data=data)

data$pls1 = predict(target, data, type = "scores", comps = 1:2)[,1]
data$pls2 = predict(target, data, type = "scores", comps = 1:2)[,2]
test$pls1 = predict(target, test, type = "scores", comps = 1:2)[,1]
test$pls2 = predict(target, test, type = "scores", comps = 1:2)[,2]

tikz("graph_pas_mix_reglog_pls.tex", standAlone=FALSE, width = 12, height = 6, fg = "black", sanitize = FALSE)
plot(data$pls1,data$pls2, col = data$y+1, xlab = "First coordinate", ylab = "Second coordinate")
dev.off()

data$cluster = ifelse(data$pls1 >2, 1, ifelse(data$pls1 > -2, 2, 3))
test$cluster = ifelse(test$pls1 >2, 1, ifelse(test$pls1 > -2, 2, 3))

pred = matrix(0, nrow = 0.5*n*n_clusters, ncol = 1)

for (j in 1:3) {
  modele = glm(y ~ x1 + x2, data = data[data$cluster==j,], family=binomial(link = "logit"))
  pred[test$pls==j] = predict(modele, test[test$cluster==j,], type="response")
}

glmdisc::normalizedGini(test$y,pred)




###################################################################
# Résultat de l'approche FAMD



library(FactoMineR)


mixed = PCA(data[,c("x1","x2")])

data$pca1 = predict(mixed, data)$coord[,1]
data$pca2 = predict(mixed, data)$coord[,2]
test$pca1 = predict(mixed, test)$coord[,1]
test$pca2 = predict(mixed, test)$coord[,2]

data$cluster = ifelse(data$pca1 > 1, 1, ifelse(data$pca1 > 0, 2, 3))
test$cluster = ifelse(test$pca1 > 1, 1, ifelse(test$pca1 > 0, 2, 3))

pred = matrix(0, nrow = 0.5*n*n_clusters, ncol = 1)

for (j in 1:3) {
  modele = glm(y ~ x1 + x2, data = data[data$cluster==j,], family=binomial(link = "logit"))
  pred[test$cluster==j] = predict(modele, test[test$cluster==j,], type="response")
}

glmdisc::normalizedGini(test$y,pred)

tikz("graph_pas_mix_reglog_pca.tex", standAlone=FALSE, width = 12, height = 6, lwdUnit = 1.3, fg = "black", sanitize = TRUE)
plot(mixed, choix = 'ind', label = "none")
dev.off()



###################################################################
# Résultat de l'approche LMT


library(RWeka)

data$y = factor(data$y)
lmt_data = LMT(y ~ x1 + x2, data = data)

tikz("graph_lmt.tex", standAlone=FALSE, width = 12, height = 6, fg = "black", sanitize = FALSE)
plot(lmt_data)
dev.off()






###################################################################
# Résultat de l'approche MOB


library(party)
library(partykit)

mob_data = glmtree(formula = y ~ x1 + x2 | x1 + x2, data = data, family = binomial)
tikz("graph_mob.tex", standAlone=FALSE, width = 12, height = 6, fg = "black", sanitize = FALSE)
plot(mob_data)
dev.off()

glmdisc::normalizedGini(test$y, predict(mob_data,test))
