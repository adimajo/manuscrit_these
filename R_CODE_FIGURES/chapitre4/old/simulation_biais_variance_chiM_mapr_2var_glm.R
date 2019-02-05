library(Hmisc)
library(mvtnorm)
library(discretization)
library(MASS)
library(infotheo)
library(foreach)
library(doSNOW)
cl <- makeCluster(8, outfile='test')

registerDoSNOW(cl)

n_total <- 5000
p0 = 0.5
y <- rbinom(n_total,1,p0)

data_tot = data.frame(y=as.factor(y))
data_tot$x1 <- rep(0,n_total)
data_tot$x2 <- rep(0,n_total)

data_tot[y=="0",2:3] = mvrnorm(n = sum(y=="0"), c(0,0), rbind(c(1,0),c(0,1)))
data_tot[y=="1",2:3] = mvrnorm(n = sum(y=="1"), c(1,1), rbind(c(1,0),c(0,1)))
data_tot <- data_tot[,c(2,3,1)]


repartition <- 0.8*nrow(data_tot)
train_ind <- sample(seq_len(nrow(data_tot)), size = repartition)

data_train <- data_tot[train_ind,]
data_test <- data_tot[-train_ind,]



gini_test_chiM <- array(0,c(2,60))
# nb_var_chiM <- array(1,20)
alpha_chiM = c(0.0000001,0.000001,0.00001,0.00005,0.0001,0.00015,0.0002,0.0003,0.0004,0.0005,0.0006,0.0007,0.0008,0.0009,0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008,0.009,0.01,0.015,0.02,0.03,0.04,0.05,0.07,0.1,0.2,0.3,0.4)

gini_test_chiM <- foreach (i = 1:34, .combine='rbind', .packages = c("discretization","Hmisc")) %dopar% {

  chiM_train <- chiM(data_train, alpha = alpha_chiM[i])
  data_train_disc <- chiM_train[["Disc.data"]]
  data_train_disc$x1 <- as.factor(data_train_disc$x1)
  data_train_disc$x2 <- as.factor(data_train_disc$x2)
  
  cutoffvalues <- chiM_train[["cutp"]][[1]]
  cutoffvalues[length(cutoffvalues)+1] <- -Inf
  cutoffvalues[length(cutoffvalues)+1] <- Inf
  
  data_test_disc <- data_test
  data_test_disc$x1_disc <- cut(data_test_disc$x1,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
  data_test_disc$x1 <- data_test_disc$x1_disc
  data_test_disc$x1_disc <- NULL
  
  cutoffvalues <- chiM_train[["cutp"]][[2]]
  cutoffvalues[length(cutoffvalues)+1] <- -Inf
  cutoffvalues[length(cutoffvalues)+1] <- Inf
  
  data_test_disc$x2_disc <- cut(data_test_disc$x2,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
  data_test_disc$x2 <- data_test_disc$x2_disc
  data_test_disc$x2_disc <- NULL
  
  
  data_train_disc_lm <- glm(y ~., family = binomial(link="logit"), data_train_disc)
  

  data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc)

  c(rcorr.cens(data_test_predict_disc,data_test_disc[,"y"])[[2]],length(cutoffvalues))
}



gini_test_chi2 <- array(0,c(2,60))
# nb_var_chi2 <- array(1,20)
alpha_chi2 = c(0.0000001,0.000001,0.00001,0.00005,0.0001,0.00015,0.0002,0.0003,0.0004,0.0005,0.0006,0.0007,0.0008,0.0009,0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008,0.009,0.01,0.015,0.02,0.03,0.04,0.05,0.07,0.1,0.2,0.3,0.4)


gini_test_chi2 <- foreach (i = 1:34, .combine='rbind', .packages = c("discretization","Hmisc")) %dopar% {
  
  chi2_train <- chi2(data_train, del = alpha_chi2[i])
  data_train_disc <- chi2_train[["Disc.data"]]
  data_train_disc$x <- as.factor(data_train_disc$x)
  
  cutoffvalues <- chi2_train[["cutp"]][[1]]
  cutoffvalues[length(cutoffvalues)+1] <- -Inf
  cutoffvalues[length(cutoffvalues)+1] <- Inf
  
  data_test_disc <- data_test
  data_test_disc$x1_disc <- cut(data_test_disc$x1,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
  data_test_disc$x1 <- data_test_disc$x1_disc
  data_test_disc$x1_disc <- NULL
  
  cutoffvalues <- chi2_train[["cutp"]][[2]]
  cutoffvalues[length(cutoffvalues)+1] <- -Inf
  cutoffvalues[length(cutoffvalues)+1] <- Inf
  
  data_test_disc$x2_disc <- cut(data_test_disc$x2,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
  data_test_disc$x2 <- data_test_disc$x2_disc
  data_test_disc$x2_disc <- NULL
  
  
  data_train_disc_lm <- lda(y ~., data_train_disc)
  

  data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
  # mse_test_chi2[alpha_chi2*100] <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
  # nb_var_chi2[alpha_chi2*100] <- length(cutoffvalues)
  c(rcorr.cens(data_test_predict_disc,data_test_disc[,"y"])[[2]],length(cutoffvalues))
}




mse_test_equalfreq <- array(1,50)

for (n in 3:100) {

  equalfreq_train <- discretize(data_train[,c("x1","x2")], disc = "equalfreq", nbins=n)
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
  
  
  data_train_disc_lm <- lda(y ~., data_train_disc)
  

  data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
  mse_test_equalfreq[n] <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
}






topdown_train <- disc.Topdown(data_train, method = 1)
data_train_disc <- topdown_train[["Disc.data"]]
data_train_disc$x <- as.factor(data_train_disc$x)
cutoffvalues <- topdown_train[["cutp"]][[1]]
cutoffvalues[length(cutoffvalues)+1] <- -Inf
cutoffvalues[length(cutoffvalues)+1] <- Inf
data_test_disc <- data_test
data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
data_test_disc$x <- data_test_disc$x_disc
data_test_disc$x_disc <- NULL
data_train_disc_lm <- lda(y ~., data_train_disc) 
data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
mse_test_topdown1 <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
nb_var_topdown1 <- length(cutoffvalues)


topdown_train <- disc.Topdown(data_train, method = 2)
data_train_disc <- topdown_train[["Disc.data"]]
data_train_disc$x <- as.factor(data_train_disc$x)
cutoffvalues <- topdown_train[["cutp"]][[1]]
cutoffvalues[length(cutoffvalues)+1] <- -Inf
cutoffvalues[length(cutoffvalues)+1] <- Inf
data_test_disc <- data_test
data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
data_test_disc$x <- data_test_disc$x_disc
data_test_disc$x_disc <- NULL
data_train_disc_lm <- lda(y ~., data_train_disc) 
data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
mse_test_topdown2 <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
nb_var_topdown2 <- length(cutoffvalues)


topdown_train <- disc.Topdown(data_train, method = 3)
data_train_disc <- topdown_train[["Disc.data"]]
data_train_disc$x <- as.factor(data_train_disc$x)
cutoffvalues <- topdown_train[["cutp"]][[1]]
cutoffvalues[length(cutoffvalues)+1] <- -Inf
cutoffvalues[length(cutoffvalues)+1] <- Inf
data_test_disc <- data_test
data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
data_test_disc$x <- data_test_disc$x_disc
data_test_disc$x_disc <- NULL
data_train_disc_lm <- lda(y ~., data_train_disc) 
data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
mse_test_topdown3 <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
nb_var_topdown3 <- length(cutoffvalues)


echi2_train <- extendChi2(data_train)
data_train_disc <- echi2_train[["Disc.data"]]
data_train_disc$x <- as.factor(data_train_disc$x)
cutoffvalues <- echi2_train[["cutp"]][[1]]
cutoffvalues[length(cutoffvalues)+1] <- -Inf
cutoffvalues[length(cutoffvalues)+1] <- Inf
data_test_disc <- data_test
data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
data_test_disc$x <- data_test_disc$x_disc
data_test_disc$x_disc <- NULL
data_train_disc_lm <- lda(y ~., data_train_disc) 
data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
mse_test_echi2 <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
nb_var_echi2 <- length(cutoffvalues)


modchi2_train <- modChi2(data_train)
data_train_disc <- modchi2_train[["Disc.data"]]
data_train_disc$x <- as.factor(data_train_disc$x)
cutoffvalues <- modchi2_train[["cutp"]][[1]]
cutoffvalues[length(cutoffvalues)+1] <- -Inf
cutoffvalues[length(cutoffvalues)+1] <- Inf
data_test_disc <- data_test
data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
data_test_disc$x <- data_test_disc$x_disc
data_test_disc$x_disc <- NULL
data_train_disc_lm <- lda(y ~., data_train_disc) 
data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
mse_test_modchi2 <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
nb_var_modchi2 <- length(cutoffvalues)


mdlp_train <- mdlp(data_train)
data_train_disc <- mdlp_train[["Disc.data"]]
data_train_disc$x <- as.factor(data_train_disc$x)
cutoffvalues <- mdlp_train[["cutp"]][[1]]
cutoffvalues[length(cutoffvalues)+1] <- -Inf
cutoffvalues[length(cutoffvalues)+1] <- Inf
data_test_disc <- data_test
data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
data_test_disc$x <- data_test_disc$x_disc
data_test_disc$x_disc <- NULL
data_train_disc_lm <- lda(y ~., data_train_disc) 
data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
mse_test_mdlp <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
nb_var_mdlp <- length(cutoffvalues)



data_train_lm <- glm(y ~ x1 + x2, family = binomial(link='logit'), data_train)
data_test_predict <- predict(data_train_lm,data_test)
gini_test <- rcorr.cens(data_test_predict,data_test[,"y"])[[2]]



