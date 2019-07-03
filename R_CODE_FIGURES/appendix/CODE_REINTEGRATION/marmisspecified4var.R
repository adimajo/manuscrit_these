############ Chargement des librairies ############ 

library(MASS)
library(Rmixmod)
library(mvtnorm)
library(sqldf)
set.seed(123)

############ Calcul du temps de calcul ############ 

ptm <- proc.time()

############ Vecteur moyenne et matrice de variance ############ 

mu0 = array(0,c(8,1))
mu1 = array(1,c(8,1))
# sigma0 = diag(8)
# sigma1 = 2*diag(8)

Posdef <- function (n, ev = runif(n, 0, 3)) 
{
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp) 
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}

sigma0 <- Posdef(n=8)
sigma1 <- Posdef(n=8)

############ Initialisation du fichier test ############ 

m_test = 100000
m=10000
nbvar = c(4,8,15,30)
p0 = 0.5
set.seed(21)
y = rbinom(m_test,1,p0)
data_test_pred = array(0,c(4,m_test,51))
data_test_gen = array(0,c(4,m_test,51))
data_test_bayes = array(0,c(4,3))

for (n in 2:2) {
  x = array(0,c(m_test,nbvar[n]))
  x[y==0,] = mvrnorm(n = sum(y==0), mu0[1:nbvar[n]], sigma0[1:nbvar[n],1:nbvar[n]])
  x[y==1,] = mvrnorm(n = sum(y==1), mu1[1:nbvar[n]], sigma1[1:nbvar[n],1:nbvar[n]])
  data_test_pred[n,,1:(nbvar[n]+1)] = as.matrix(cbind.data.frame(y = y,x = x))
  data_test_gen[n,,] <- data_test_pred[n,,]
  data_test_gen[n,,1] <- ifelse(data_test_pred[n,,1] == 0, 2, 1)
  
  e0 <- log(p0) + dmvnorm(x[y==0,],mu0[1:nbvar[n]],sigma0[1:nbvar[n],1:nbvar[n]],log = TRUE) < log(1-p0) + dmvnorm(x[y==0,],mu1[1:nbvar[n]],sigma1[1:nbvar[n],1:nbvar[n]],log = TRUE)
  e1 <- log(p0) + dmvnorm(x[y==1,],mu0[1:nbvar[n]],sigma0[1:nbvar[n],1:nbvar[n]],log = TRUE) > log(1-p0) + dmvnorm(x[y==1,],mu1[1:nbvar[n]],sigma1[1:nbvar[n],1:nbvar[n]],log = TRUE)
  eb <-  mean(c(e0,e1))
  ebconf <- prop.test(sum(c(e0,e1)),m_test)$conf.int[1:2]
  data_test_bayes[n,] <- c(eb,ebconf[1],ebconf[2])
  rm(x)
}

rm(y)

cut = seq(0,.9,by = 0.02)
tx_erreur = array(NA,c(4,length(cut),5,20))

rm(ebconf)
rm(eb)
rm(e0)
rm(e1)
gc()

############ Calcul des mod?les ############ 

#### Intialisation du fichier d'apprentissage ####

for (random in 1:20) {
  set.seed(random)
  y = rbinom(m,1,p0)
  data_learn_pred = array(0,c(4,m,51))
  data_learn_gen = array(0,c(4,m,51))
  
  for (n in 2:2) {
    x = array(0,c(m,nbvar[n]))
    x[y==0,] = mvrnorm(n = sum(y==0), mu0[1:nbvar[n]], sigma0[1:nbvar[n],1:nbvar[n]])
    x[y==1,] = mvrnorm(n = sum(y==1), mu1[1:nbvar[n]], sigma1[1:nbvar[n],1:nbvar[n]])
    data_learn_pred[n,,1:(nbvar[n]+1)] = as.matrix(cbind.data.frame(y = y,x = x))
    data_learn_gen[n,,] <- data_learn_pred[n,,]
    data_learn_gen[n,,1] <- ifelse(data_learn_gen[n,,1] == 0, 2, 1)
    rm(x)
  }
  
  rm(y)
  gc()
  
  #### Boucle sur nombre de variables dans le mod?le ####
  
  for (j in 2:2) {
    # On convertit l'ensemble d'apprentissage en dataframe
    learn_pred = data.frame(data_learn_pred[j,,1:(nbvar[j]+1)])
    learn_gen = data.frame(data_learn_gen[j,,1:(nbvar[j]+1)])
    
    # On construit la formule appliqu?e ? glm (elle d?pend du nombre de variables dans le mod?le)
    PredictorVariables <- paste("X", 2:(nbvar[j]+1), sep="")
    Formula <- formula(paste("X1 ~ ", paste(PredictorVariables, collapse=" + ")))
    rm(PredictorVariables)
    gc()
    
    # On entra?ne un mod?le glm sur toutes les donn?es pour simuler les refus?s
    model_pred_tot = glm(Formula,"binomial",learn_pred)
    
    #### Boucle sur le cut ####
    
    for (i in 1:length(cut)){
      
      # On calcule les accept?s et rejet?s en fonction du cut et du premier mod?le
      accepte <- (model_pred_tot$fitted.values > cut[i])
      rejete <- (model_pred_tot$fitted.values <= cut[i])
      
      # On contr?le qu'il reste des accept?s et des mauvais payeurs parmi les accept?s
      if (sum(accepte) > 0 & !(sum(learn_pred[accepte,1])==nrow(learn_pred[accepte,])) ) {
        
        # On construit l'ensemble d'apprentissage partiel
        learn_pred_part = learn_pred[accepte,]
        learn_gen_part = learn_gen
        learn_gen_part[rejete,1] = NA
        
        # On entra?ne les mod?les
        model_pred_part = glm(Formula, family = binomial(link='logit'), learn_pred_part)
        model_gen_part = mixmodCluster(data=learn_gen_part[,2:(nbvar[j]+1)], knownLabels = learn_gen_part[,1], nbCluster=2)
        
        # On pr?dit leur r?sultat sur l'ensemble de test et on enregistre le taux d'erreur
        model_pred_part.pred <- predict(model_pred_part, data.frame(data_test_pred[j,,]), type='response') >= 0.5
        model_pred_part.erreur <- sum(abs(data_test_pred[j,,1]-model_pred_part.pred))/m_test
        
        model_gen_part.pred <- mixmodPredict(data.frame(data_test_gen[j,,2:(nbvar[j]+1)]), model_gen_part@bestResult)@partition
        model_gen_part.erreur <- sum(abs(data_test_gen[j,,1]-model_gen_part.pred))/m_test
        
        
        
        
        
        
        
        
        ## Augmentation ##
        
        learn_pred_augmente <- learn_pred
        learn_pred_augmente$classe_SCORE <- round(model_pred_tot$fitted.values, digits=1)
        learn_pred_augmente$acc[accepte] <- 1
        learn_pred_augmente$acc[rejete] <- 0
        poids2 <- sqldf(
          'select distinct count(*) as count, classe_SCORE, acc
          from learn_pred_augmente
          group by classe_SCORE, acc
          '
        )
        poids_acceptes <- poids2[poids2$acc==1,]
        poids_rejetes <- poids2[poids2$acc==0,]
        
        poids_acceptes$count_acc <- poids_acceptes$count
        poids_acceptes$count <- NULL
        poids_acceptes$acc <- NULL
        
        poids_rejetes$count_rej <- poids_rejetes$count
        poids_rejetes$count <- NULL
        poids_rejetes$acc <- NULL
        
        poids_tot <- merge(poids_acceptes, poids_rejetes, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
        poids_tot$poidsfinal <- ifelse(is.na(poids_tot$count_acc),0,ifelse(is.na(poids_tot$count_rej),1,1+poids_tot$count_rej/poids_tot$count_acc))
        poids_tot$count_acc <- NULL
        poids_tot$count_rej <- NULL
        
        learn_pred_augmente <- merge(learn_pred_augmente, poids_tot, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
        model_pred_augmente = glm(Formula, family = binomial(link='logit'), learn_pred_augmente[learn_pred_augmente$acc==1,], weights = learn_pred_augmente$poidsfinal[learn_pred_augmente$acc==1])
        model_pred_augmente.pred <- predict(model_pred_augmente, data.frame(data_test_pred[j,,]), type='response') >= 0.5
        model_pred_augmente.erreur <- sum(abs(data_test_pred[j,,1]-model_pred_augmente.pred))/m_test
        
        
        
        
        
        
        
        
        
        
        ## Parcelling ##
        
        model_pred_train <- predict(model_pred_part, learn_pred, type='response')
        learn_pred_parceling <- learn_pred
        learn_pred_parceling$classe_SCORE <- round(model_pred_train, digits=1)
        learn_pred_part$classe_SCORE <- round(model_pred_train[accepte], digits=1)
        learn_pred_refus <- learn_pred[rejete,]
        learn_pred_refus$classe_SCORE <- round(model_pred_train[rejete], digits=1)
        
        poids_part <- sqldf(
          'select distinct count(X1) as count, classe_SCORE, X1
          from learn_pred_part
          group by classe_SCORE, X1
          '
        )
        
        poids_bon <- poids_part[poids_part$X1==1,]
        poids_mauvais <- poids_part[poids_part$X1==0,]
        poids_bon$X1 <- NULL
        poids_mauvais$X1 <- NULL
        poids <- merge(poids_bon, poids_mauvais, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
        poids$poids_final <- 0.85*poids$count.y/(poids$count.x+poids$count.y)
        poids$count.x <- NULL
        poids$count.y <- NULL
        
        learn_pred_parceling <- merge(learn_pred_parceling, poids, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
        learn_pred_parceling$poids_final <- ifelse(is.na(learn_pred_parceling$poids_final), 1, learn_pred_parceling$poids_final)
        
        fun_binom <- function(x) {
          return(rbinom(1,1,1-x))
        }
        learn_pred_parceling[rejete,"X1"] <- sapply(learn_pred_parceling[rejete,"poids_final"],fun_binom)
        
        model_pred_parcelling = glm(Formula, family = binomial(link='logit'), learn_pred_parceling)
        model_pred_parcelling.pred <- predict(model_pred_parcelling, data.frame(data_test_pred[j,,]), type='response') >= 0.5
        model_pred_parcelling.erreur <- sum(abs(data_test_pred[j,,1]-model_pred_parcelling.pred))/m_test
        
        
        
        
        
        
        
        
        ## Reclassification forc?e ##
        
        if (sum(rejete)>0) {
          if (exists("model_pred_part_reclass")) rm(model_pred_part_reclass)
          if (exists("learn_pred_part_reclass")) rm(learn_pred_part_reclass)
          learn_pred_part_reclass <- learn_pred_part
          reintegres1 <- learn_pred[rejete,]
          reintegres1$X1 <- as.integer(round(predict(model_pred_part, reintegres1, type="response"),digits = 0))
          learn_pred_part_reclass <- rbind(learn_pred_part_reclass[,1:(nbvar[j]+1)], reintegres1)
          
          model_pred_part_reclass = glm(Formula, family = binomial(link='logit'), learn_pred_part_reclass)
          model_pred_part_reclass.pred <- predict(model_pred_part_reclass, data.frame(data_test_pred[j,,]), type='response') >= 0.5
          model_pred_part_reclass.erreur <- sum(abs(data_test_pred[j,,1]-model_pred_part_reclass.pred))/m_test
          
          gc()
        }
        
        # rm(learn_pred_part)
        # rm(learn_gen_part)
        # rm(learn_pred_part_reclass)
        # rm(learn_pred_parceling)
        # rm(learn_pred_refus)
        # 
        # rm(model_pred_part)
        # rm(model_gen_part)
        # rm(model_pred_augmente)
        # rm(model_pred_parcelling)
        # rm(model_pred_part_reclass)
        # 
        # rm(model_pred_part.pred)
        # rm(model_gen_part.pred)
        # rm(model_pred_augmente.pred)
        # rm(model_pred_parcelling.pred)
        # rm(model_pred_part_reclass.pred)
        # 
        # rm(poids_bon)
        # rm(poids_mauvais)
        # rm(poids_tot)
        # rm(reintegres1)
        # rm(poids_acceptes)
        # rm(poids_rejetes)
        # rm(poids)
        
        
        # rm(accepte)
        # for (k in 1:10) {gc()}
      }
      
      
      # On renvoie le taux d'erreur des mod?les
      if (sum(rejete)>0) {
        tx_erreur[j,i,,random] <- c(model_pred_part.erreur, model_pred_augmente.erreur, model_pred_part_reclass.erreur, model_pred_parcelling.erreur, model_gen_part.erreur)
      } else {
        tx_erreur[j,i,,random] <- c(model_pred_part.erreur, model_pred_augmente.erreur, model_pred_part.erreur, model_pred_parcelling.erreur, model_gen_part.erreur)
      }
      
      # tx_erreur[j,i,,random] <- c(model_pred_part.erreur, model_gen_part.erreur)
      
      gc()
    }
    
    # On fait le m?nage pour la RAM
    # rm(learn_pred)
    # rm(learn_gen)
    # rm(taux)
    # rm(PredictorVariables)
    # rm(Formula)
    
    gc()
  }
}

ptm_total <- proc.time() - ptm
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tikzDevice)
tikz('reintegration_simu_4var.tex', width=8, height = 3.2, engine = "pdftex")
plot(x = cut, y = tx_erreur[2,,1,1], xlim=c(0,.9), ylim=c(.1,.3), ylab = 'Error rate on test set', xlab = 'Cut-off value', pch=15, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = cut, y = tx_erreur[2,,2,1], xlim=c(0,.9), ylim=c(.1,.3), ylab = 'Error rate on test set', xlab = 'Cut-off value', pch=17, col='green', xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = cut, y = tx_erreur[2,,3,1], xlim=c(0,.9), ylim=c(.1,.3), ylab = 'Error rate on test set', xlab = 'Cut-off value', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = cut, y = tx_erreur[2,,4,1], xlim=c(0,.9), ylim=c(.1,.3), ylab = 'Error rate on test set', xlab = 'Cut-off value', pch=8, col=590, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = cut, y = tx_erreur[2,,5,1], xlim=c(0,.9), ylim=c(.1,.3), ylab = 'Error rate on test set', xlab = 'Cut-off value', pch=9, col=376, xaxt = "n", yaxt="n", type='o')

axis(1, at=pretty(cut), lab=pretty(cut), las=TRUE)
axis(2, at=0.05, lab="0.05", las=TRUE)
axis(2, at=pretty(seq(0.1,0.3,0.05), n=10), lab=pretty(seq(0.1,0.3,0.05), n=10), las=TRUE)

legend(0,0.3, 
       pch = c(15,17,7,8,9), 
       col=c(1,"green",653,590,376),legend= c("Logistic regression","Augmentation","Reclassification","Parcelling","Gaussian mixture"),cex=0.7)
dev.off()