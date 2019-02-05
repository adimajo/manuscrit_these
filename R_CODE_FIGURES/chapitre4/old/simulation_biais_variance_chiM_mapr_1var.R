
library(mvtnorm)
library(discretization)
library(MASS)
library(infotheo)


ptm <- proc.time()

n_total <- 5000
p0 = 0.5
y <- rbinom(n_total,1,p0)

data_tot = data.frame(y=as.factor(y))

data_tot[y==0,"x"] = mvrnorm(n = sum(y==0), 0, 0.4)
data_tot[y==1,"x"] = mvrnorm(n = sum(y==1), 1, 0.4)
data_tot <- data_tot[,c(2,1)]


repartition <- 0.8*nrow(data_tot)
train_ind <- sample(seq_len(nrow(data_tot)), size = repartition)

data_train <- data_tot[train_ind,]
data_test <- data_tot[-train_ind,]



mse_test_chiM <- array(1,20)
nb_var_chiM <- array(1,20)

for (alpha_chiM in seq(0.01,0.2,0.01)) {

  chiM_train <- chiM(data_train, alpha = alpha_chiM)
  data_train_disc <- chiM_train[["Disc.data"]]
  data_train_disc$x <- as.factor(data_train_disc$x)
  
  cutoffvalues <- chiM_train[["cutp"]][[1]]
  cutoffvalues[length(cutoffvalues)+1] <- -Inf
  cutoffvalues[length(cutoffvalues)+1] <- Inf
  
  data_test_disc <- data_test
  data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
  data_test_disc$x <- data_test_disc$x_disc
  data_test_disc$x_disc <- NULL
  
  
  data_train_disc_lm <- lda(y ~., data_train_disc)
  

  data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
  mse_test_chiM[alpha_chiM*100] <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
  nb_var_chiM[alpha_chiM*100] <- length(cutoffvalues)
}





total <- ptm - proc.time()






mse_test_chi2 <- array(1,20)
nb_var_chi2 <- array(1,20)

for (alpha_chi2 in seq(0.01,0.2,0.01)) {

  chi2_train <- chi2(data_train, del = alpha_chi2)
  data_train_disc <- chi2_train[["Disc.data"]]
  data_train_disc$x <- as.factor(data_train_disc$x)
  
  cutoffvalues <- chi2_train[["cutp"]][[1]]
  cutoffvalues[length(cutoffvalues)+1] <- -Inf
  cutoffvalues[length(cutoffvalues)+1] <- Inf
  
  data_test_disc <- data_test
  data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
  data_test_disc$x <- data_test_disc$x_disc
  data_test_disc$x_disc <- NULL
  
  
  data_train_disc_lm <- lda(y ~., data_train_disc)
  

  data_test_predict_disc <- predict(data_train_disc_lm,data_test_disc,type='response')$class
  mse_test_chi2[alpha_chi2*100] <- sum((as.numeric(paste0(data_test_predict_disc)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)
  nb_var_chi2[alpha_chi2*100] <- length(cutoffvalues)
}




mse_test_equalfreq <- array(1,50)

for (n in 3:100) {

  equalfreq_train <- discretize(data_train$x, disc = "equalfreq", nbins=n)
  data_train_disc <- cbind(equalfreq_train, data_train)

  cut <- sqldf(
      'select V1, max(x), min(x)
      from data_train_disc
      group by V1
      '
  )

  data_train_disc[,2] <- NULL
  colnames(data_train_disc) <- c("x","y")

  data_train_disc[,1:2] <- lapply(data_train_disc[,1:2], function(x) as.factor(x))

  cutoffvalues <- cut[,"max(x)"]

  cutoffvalues[length(cutoffvalues)] <- Inf
  cutoffvalues[length(cutoffvalues)+1] <- -Inf
  
  data_test_disc <- data_test
  data_test_disc$x_disc <- cut(data_test_disc$x,cutoffvalues, include.lowest = FALSE, labels = seq(1:(length(cutoffvalues)-1)))
  data_test_disc$x <- data_test_disc$x_disc
  data_test_disc$x_disc <- NULL
  
  
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



data_train_lm <- lda(y ~ x, data_train)
data_test_predict <- predict(data_train_lm,data_test,type='response')$class
mse_test <- sum((as.numeric(paste0(data_test_predict)) - as.numeric(paste0(data_test$y)))^2)/nrow(data_test)



