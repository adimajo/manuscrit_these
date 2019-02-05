
####### Chargement des packages #######
library(mvtnorm)
library(discretization)
library(MASS)
# library(hydroGOF)

####### Création du fichier simulé #######

n_total <- 5000
p0 = 0.5
y <- rbinom(n_total,1,p0)

data_tot = data.frame(y=as.factor(y))

data_tot[y==0,"x"] = mvrnorm(n = sum(y==0), 0, 0.1)
data_tot[y==1,"x"] = mvrnorm(n = sum(y==1), 1, 0.1)
data_tot <- data_tot[,c(2,1)]

####### Séparation train / test #######

repartition <- 0.8*nrow(data_tot)
train_ind <- sample(seq_len(nrow(data_tot)), size = repartition)

data_train <- data_tot[train_ind,]
data_test <- data_tot[-train_ind,]


####### Apprentissage chiM : boucle sur alpha #######

mse_test_chiM <- array(1,20)

for (alpha_chiM in seq(0.01,0.2,0.01)) {
  # Discrétisation
  chiM_train <- chiM(data_train, alpha = alpha_chiM)
  data_train_disc <- chiM_train[["Disc.data"]]
  data_train_disc$x <- as.factor(data_train_disc$x)
  
  cutoffvalues <- chiM_train[["cutp"]][[1]]
  cutoffvalues[length(cutoffvalues)+1] <- -Inf
  cutoffvalues[length(cutoffvalues)+1] <- Inf
  
  data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
  data_test_disc$x <- data_test_disc$x_disc
  data_test_disc$x_disc <- NULL
  
  
  # Apprentissage du modèle sur données discrètes
  data_train_disc_lm <- lda(y ~., data_train_disc)
  

  # Prédiction et erreur sur l'ensemble test
  data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
  mse_test_chiM[alpha_chiM*100] <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
}



####### Apprentissage classification linéaire sur variable continue #######

data_train_lm <- lda(y ~ x, data_train)
data_test_predict <- predict(data_train_lm,data_test,type='response')$class
mse_test <- sum((as.numeric(paste0(data_test_predict)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)



