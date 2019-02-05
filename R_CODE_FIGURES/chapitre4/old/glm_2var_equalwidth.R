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

gini_test_equalwidth <- array(0,c(48,100))
gini_test <- array(0,100)

for (random in 1:100) {
  
  set.seed(random)
  
  n_total <- 10000
  p0 = 0.5
  y <- rbinom(n_total,1,p0)
  
  data_tot = data.frame(y=as.factor(y))
  data_tot$x1 <- rep(0,n_total)
  data_tot$x2 <- rep(0,n_total)
  
  data_tot[y=="0",2:3] = mvrnorm(n = sum(y=="0"), c(0,0), rbind(c(1.2,0.4),c(0.4,1.2)))
  data_tot[y=="1",2:3] = mvrnorm(n = sum(y=="1"), c(1,1), rbind(c(1,-0.1),c(-0.10,1)))
  data_tot <- data_tot[,c(2,3,1)]
  
  
  repartition <- 0.8*nrow(data_tot)
  train_ind <- sample(seq_len(nrow(data_tot)), size = repartition)
  
  data_train <- data_tot[train_ind,]
  data_test <- data_tot[-train_ind,]
  
  
  gini_test_equalwidth[,random] <- foreach (n = 3:50, .combine='rbind', .packages = c("infotheo","Hmisc","sqldf")) %dopar% {
    
    equalfreq_train <- discretize(data_train[,c("x1","x2")], disc = "equalwidth", nbins=n)
    colnames(equalfreq_train) <- c("x1_disc","x2_disc")
    data_train_disc <- cbind(equalfreq_train, data_train)
    
    cut_x1 <- sqldf(
      'select x1_disc, max(x1)
      from data_train_disc
      group by x1_disc
      '
    )
    
    cut_x2 <- sqldf(
      'select x2_disc, max(x2)
      from data_train_disc
      group by x2_disc
      '
    )
    
    data_train_disc[,3] <- NULL
    data_train_disc[,3] <- NULL
    
    colnames(data_train_disc) <- c("x1","x2","y")
    
    data_train_disc[,1:2] <- lapply(data_train_disc[,1:2], function(x) as.factor(x))
    
    cutoffvalues <- data.frame(cbind(cut_x1[,c("max(x1)")], cut_x2[,c("max(x2)")]))
    
    cutoffvalues[length(cutoffvalues[,1]),] <- Inf
    cutoffvalues[length(cutoffvalues[,1])+1,] <- -Inf
    
    data_test_disc <- data_test
    data_test_disc$x1_disc <- cut(data_test_disc$x1,cutoffvalues[,1], include.lowest = FALSE, labels = seq(1:(length(cutoffvalues[,1])-1)))
    data_test_disc$x1 <- data_test_disc$x1_disc
    data_test_disc$x1_disc <- NULL
    
    data_test_disc$x2_disc <- cut(data_test_disc$x2,cutoffvalues[,2], include.lowest = FALSE, labels = seq(1:(length(cutoffvalues[,2])-1)))
    data_test_disc$x2 <- data_test_disc$x2_disc
    data_test_disc$x2_disc <- NULL
    
    
    data_train_disc_lm <- glm(y ~., family=binomial(link='logit'), data_train_disc)
    
    
    data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc)
    # mse_test_equalfreq[n] <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
    rcorr.cens(data_test_predict_disc,data_test_disc[,"y"])[[2]]
  }
  
  
  
  data_train_lm <- glm(y ~., family=binomial(link='logit'), data_train)
  data_test_predict <- predict(data_train_lm,data_test)
  gini_test[random] <- rcorr.cens(data_test_predict,data_test[,"y"])[[2]]
  
  data_train_lm_inter <- glm(y ~x1+x2+x1:x2, family=binomial(link='logit'), data_train)
  data_test_predict_inter <- predict(data_train_lm_inter,data_test)
  gini_test_inter[random] <- rcorr.cens(data_test_predict_inter,data_test[,"y"])[[2]]
  
  
}



