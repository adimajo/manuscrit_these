
####### Chargement des packages #######
library(mvtnorm)
library(discretization)
library(MASS)

####### Création du fichier simulé #######

n_total <- 500
y <- seq(1:n_total)
data_tot <- data.frame(x = y + mvrnorm(n=n_total, 0,0.3), y=y)

####### Séparation train / test #######

repartition <- 0.8*nrow(data_tot)
train_ind <- sample(seq_len(nrow(data_tot)), size = repartition)

data_train <- data_tot[train_ind,]
data_test <- data_tot[-train_ind,]


####### Apprentissage chiM : boucle sur alpha #######

chiM_train <- chiM(data_train)
