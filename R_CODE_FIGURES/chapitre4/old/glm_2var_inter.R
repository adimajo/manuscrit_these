library(Hmisc)
library(mvtnorm)
library(discretization)
library(MASS)
library(infotheo)
library(foreach)
library(doSNOW)
library(sqldf)

cl <- makeCluster(8, outfile='test')

registerDoSNOW(cl)



gini_test_inter <- array(0,100)
gini_test <- array(0,100)


for (random in 1:100) {
  set.seed(random)
  
  n_total <- 10000
  p0 = 0.5
  y <- rbinom(n_total,1,p0)
  
  data_tot = data.frame(y=as.factor(y))
  data_tot$x1 <- rep(0,n_total)
  data_tot$x2 <- rep(0,n_total)
  
  data_tot[y=="0",2:3] = mvrnorm(n = sum(y=="0"), c(0,0), rbind(c(0.8,0.4),c(0.4,0.8)))
  data_tot[y=="1",2:3] = mvrnorm(n = sum(y=="1"), c(2,2), rbind(c(2.2,-0.4),c(-0.4,2.2)))
  data_tot <- data_tot[,c(2,3,1)]
  
  
  repartition <- 0.8*nrow(data_tot)
  train_ind <- sample(seq_len(nrow(data_tot)), size = repartition)
  
  data_train <- data_tot[train_ind,]
  data_test <- data_tot[-train_ind,]
  
  data_train_lm <- glm(y ~., family=binomial(link='logit'), data_train)
  data_test_predict <- predict(data_train_lm,data_test)
  gini_test[random] <- rcorr.cens(data_test_predict,data_test[,"y"])[[2]]
  
  
  data_train_inter <- data_train
  data_train_inter$x3 <- data_train_inter$x1^2
  data_train_inter$x4 <- data_train_inter$x2^2
  
  data_train_lm_inter <- glm(y ~ . + x1:x2 + x3:x4, family=binomial(link='logit'), data_train_inter)
  
  
  data_test_inter <- data_test
  data_test_inter$x3 <- data_test_inter$x1^2
  data_test_inter$x4 <- data_test_inter$x2^2
  
  data_test_predict_inter <- predict(data_train_lm_inter,data_test_inter)
  gini_test_inter[random] <- rcorr.cens(data_test_predict_inter,data_test_inter[,"y"])[[2]]
}


