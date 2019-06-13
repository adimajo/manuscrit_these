library(Hmisc)
library(sqldf)
# library(h2o)
library(MASS)
# library(neuralnet)
library(e1071)
library(nnet)
library(rpart)
library(randomForest)
library(Rmixmod)
# h2o.init()

########### M?thodes de r?int?gration de refus?s : donn?es r?elles ########### 

###### Chargement des donn?es ###### 
setwd("Z:/6D1/329/Thèse CIFRE SCORING/Code R/Simulation Réintégration/real_data")
set.seed(10)

#### Darty ####
darty_tot <- read.csv(file = "DARTY_SO14011412.csv",sep=";", fileEncoding="latin1")
darty_tot <- darty_tot[,c(1,12:39)]
darty_tot$MOIS <- NULL
darty_tot$TYPE <- NULL
darty_tot$cat <- NULL
darty_tot$TOP_PIC <- ifelse(darty_tot$TOP_PIC=="1", 1, ifelse(darty_tot$TOP_PIC=="0",0,NA))

darty <- subset(darty_tot, !is.na(darty_tot$TOP_PIC))

darty$csp_emb <- ifelse(darty$csp_emb_1==1,'1',ifelse(darty$csp_emb_2==1,'2',ifelse(darty$csp_emb_3==1,'3',ifelse(darty$csp_emb_4==1,'4',ifelse(darty$csp_emb_5==1,'5','6')))))
darty$habit_loyer <- ifelse(darty$habit_loyer_1==1,'1',ifelse(darty$habit_loyer_2==1,'2',ifelse(darty$habit_loyer_3==1,'3', ifelse(darty$habit_loyer_4==1,'4', ifelse(darty$habit_loyer_5==1,'5', ifelse(darty$habit_loyer_6==1,'6','7'))))))
darty$cspcj <- ifelse(darty$cspcj_1==1,'1',ifelse(darty$cspcj_2==1,'2',ifelse(darty$cspcj_3==1,'3','4')))
darty$anc_dclem <- ifelse(darty$anc_dclem_1==1,'1',ifelse(darty$anc_dclem_2==1,'2','3'))
darty$nb_enf <- ifelse(darty$nbenf_1==1,'1',ifelse(darty$nbenf_2==1,'2',ifelse(darty$nbenf_3==1,'3','4')))

darty <- darty[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC","SCORE")]

# Randomization
darty <- darty[sample(nrow(darty)),]
darty[,1:5] <- lapply(darty[,1:5], function(x) as.factor(x))
rownames(darty) <- seq(length=nrow(darty))




###### Cr?ation du fichier test et des fichiers d'apprentissage ######

# Cr?ation du fichier test
row_darty <- nrow(darty)
# 
# darty_test <- darty[1:row_darty,]
# darty_test <- darty_test[!(is.na(darty_test$TOP_PIC)),]

# darty_test <- darty[1:round(row_darty*0.2),]
# darty_test$poids <- 1

# Cr?ation des fichiers d'apprentissage

# darty_learn <- darty[(round(row_darty*0.2)+1):row_darty,]
# darty_learn <- darty[100000:109999,]

cut=c(210,215,220,225,230,240,250,260,270,280)
cost_fp = 1
cost_fn = 1

taux <- array(0,c(10,7,length(cut)))

###### Apprentissage des mod?les ######

# Boucle sur les ensembles d'apprentissage
for(b in 1:5) {  
  # set.seed(b)
  # darty_learn_dataset <- darty_learn[sample(length(darty_learn[,1]),replace=TRUE),]
  
  # darty_learn_dataset <- darty_learn[(round(b*row_darty/10):round((b+1)*row_darty/10)),]
  darty_test <- darty[(round((b-1)*row_darty/5)+1):(round(b*row_darty/5)),]
  darty_learn_dataset <- darty[-(((b-1)*round(row_darty/5)+1):(b*round(row_darty/5))),]
  
  darty_test$poids <- 1
  darty_learn_dataset$poids <- 1
  
  colnames(darty_learn_dataset) <- colnames(darty)
  
  darty_learn_dataset$poids <- 1
  
  # Boucle sur le cut
  
  for (j in 1:length(cut)) {
    
    
      
      
    # Apprentissage logistique sur accept?s
    darty_learn_acc_dataset <- darty_learn_dataset[darty_learn_dataset$SCORE>=cut[j],]
    darty_learn_acc_dataset <- darty_learn_acc_dataset[!(is.na(darty_learn_acc_dataset$TOP_PIC)),]
    darty_learn_acc_dataset$poids[darty_learn_acc_dataset$TOP_PIC==1] <- cost_fp
    darty_learn_acc_dataset$poids[darty_learn_acc_dataset$TOP_PIC==0] <- cost_fn
    
    darty_model <- glm(TOP_PIC ~ ., "binomial",darty_learn_acc_dataset[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")],weights=darty_learn_acc_dataset$poids)
    
    # darty_learn_acc_dataset_h2o <- darty_learn_acc_dataset
    # darty_learn_acc_dataset_h2o$TOP_PIC <- as.factor(darty_learn_acc_dataset_h2o$TOP_PIC)
    # darty_learn_acc_dataset_h2o <- as.h2o(darty_learn_acc_dataset_h2o[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")])
    # darty_test_h2o <- darty_test[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf")]
    # darty_test_h2o <- data.frame(lapply(darty_test_h2o, function(x) as.factor(x)))
    # darty_test_h2o <- as.h2o(darty_test_h2o)
    # darty_model_h2o_deep <- h2o.deeplearning(x=1:5, y="TOP_PIC",training_frame=darty_learn_acc_dataset_h2o)
    # 
    # darty_model_h2o_gbm <- h2o.gbm(x=1:5, y="TOP_PIC",training_frame=darty_learn_acc_dataset_h2o)
    
    ## darty_learn_acc_dataset_neural <- data.frame(model.matrix(~ TOP_PIC+csp_emb+habit_loyer+cspcj+anc_dclem+nb_enf,data = darty_learn_acc_dataset))
    ## darty_learn_acc_dataset_neural$TOP_PIC <- as.factor(darty_learn_acc_dataset_neural$TOP_PIC)
    ## darty_learn_acc_dataset_neural$X.Intercept. <- NULL
    ## n <- names(darty_learn_acc_dataset_neural)
    ## f <- as.formula(paste("TOP_PIC ~", paste(n[!n %in% "medv"], collapse = " + ")))
    # darty_model_nnet <- nnet(as.factor(TOP_PIC) ~ .,darty_learn_acc_dataset[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")], size = 10, maxiter = 50)
    # 
    # darty_model_rpart <- rpart(TOP_PIC ~ .,darty_learn_acc_dataset[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")],weights=darty_learn_acc_dataset$poids)
    # darty_model_rforest <- randomForest(as.factor(TOP_PIC) ~ .,darty_learn_acc_dataset[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")])
    # 
    # darty_learn_acc_dataset_svm <- darty_learn_acc_dataset
    # darty_learn_acc_dataset_svm$TOP_PIC <- ifelse(darty_learn_acc_dataset_svm$TOP_PIC==1,1,-1)
    # darty_model_svm <- svm(as.factor(TOP_PIC) ~ .,darty_learn_acc_dataset_svm[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")],weights=darty_learn_acc_dataset$poids, probability = TRUE)
    
    
    darty_learn_gen <- darty_learn_dataset[darty_learn_dataset$SCORE<cut[j],]
    try(darty_learn_gen$TOP_PIC <- NA, silent=TRUE)
    darty_learn_gen <- rbind(darty_learn_gen, darty_learn_acc_dataset)
    darty_learn_gen$TOP_PIC <- ifelse(darty_learn_gen$TOP_PIC == 0, 2, 1)
    
    darty_model_gen <- mixmodCluster(data=darty_learn_gen[,1:5], knownLabels = darty_learn_gen[,6], nbCluster=2)
    
    

    
    
    if (length(darty_learn_dataset[darty_learn_dataset$SCORE<cut[j],1]) > 0) {

    
    # Apprentissage logistique repond?r?e
    
    # darty_learn_ref_dataset <- darty_learn_dataset[is.na(darty_learn_dataset$TOP_PIC),]
    # darty_learn_ref_dataset <- rbind(darty_learn_ref_dataset, darty_learn_dataset[darty_learn_dataset$SCORE<cut[j],])
    darty_learn_ref_dataset <- darty_learn_dataset[darty_learn_dataset$SCORE<cut[j],]
    
    darty_learn_ref_dataset1 <- darty_learn_ref_dataset
    try(darty_learn_ref_dataset1$poids <- predict(darty_model, darty_learn_ref_dataset1, type='response'), silent = TRUE)
    darty_learn_ref_dataset1$TOP_PIC <- 1
    
    darty_learn_ref_dataset2 <- darty_learn_ref_dataset
    darty_learn_ref_dataset2$poids <- 1-darty_learn_ref_dataset1$poids
    darty_learn_ref_dataset2$TOP_PIC <- 0
    
    darty_learn_pond <- rbind(darty_learn_acc_dataset,darty_learn_ref_dataset1,darty_learn_ref_dataset2)
    darty_model_pond <- glm(TOP_PIC ~ ., "binomial",darty_learn_pond[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")], weights = darty_learn_pond$poids)
    try(darty_model_pond.pred <- predict(darty_model_pond, darty_test[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")]), silent = TRUE)
    # darty_model_pond.erreur <- sum(abs(darty_test[,"TOP_PIC"]-darty_model_pond.pred))/nrow(darty_test)
    darty_model_pond.gini <- rcorr.cens(darty_model_pond.pred,darty_test[,"TOP_PIC"])[[2]]
    
    
    
    
    # Apprentissage logistique reclass?e
    darty_learn_ref_reclass <- darty_learn_ref_dataset
    try(darty_learn_ref_reclass$TOP_PIC <- as.integer(round(predict(darty_model, darty_learn_ref_reclass, type="response"),digits = 0)), silent = TRUE)
    darty_learn_ref_reclass <- rbind(darty_learn_ref_reclass,darty_learn_acc_dataset)
    
    darty_model_reclass <- glm(TOP_PIC ~ ., "binomial",darty_learn_acc_dataset[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")], weights = darty_learn_acc_dataset$poids)
    
    try(darty_model_reclass.pred <- predict(darty_model_reclass, darty_test[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")]), silent = TRUE)
    # darty_model_reclass.erreur <- sum(abs(darty_test[,"TOP_PIC"]-darty_model_reclass.pred))/nrow(darty_test)
    darty_model_reclass.gini <- rcorr.cens(darty_model_reclass.pred,darty_test[,"TOP_PIC"])[[2]]
    
    
    
    
    
    # Apprentissage augmentation
    darty_learn_augmentation <- darty_learn_dataset
    darty_learn_augmentation$classe_SCORE <- cut2(darty_learn_augmentation$SCORE, g=10)
    darty_learn_augmentation$acc <- ifelse(((darty_learn_augmentation$SCORE>=cut[j])&(!(is.na(darty_learn_augmentation$TOP_PIC)))),1,0)
    
    poids2 <- sqldf(
      'select distinct count(*) as count, classe_SCORE, acc
      from darty_learn_augmentation
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
    
    darty_learn_augmentation <- merge(darty_learn_augmentation, poids_tot, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
    darty_model_augmentation <- glm(TOP_PIC ~ ., "binomial", darty_learn_augmentation[((darty_learn_augmentation$SCORE>=cut[j])&(!(is.na(darty_learn_augmentation$TOP_PIC)))),c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")], weights = darty_learn_augmentation[((darty_learn_augmentation$SCORE>=cut[j])&(!(is.na(darty_learn_augmentation$TOP_PIC)))),"poidsfinal"])
    try(darty_model_aumgentation.pred <- predict(darty_model_augmentation, darty_test[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")]), silent = TRUE)
    # darty_model_augmentation.erreur <- sum(abs(data_test_pred[j,,1]-model_pred_augmente.pred))/m_test
    darty_model_aumgentation.gini <- rcorr.cens(darty_model_aumgentation.pred,darty_test[,"TOP_PIC"])[[2]]
    
    
    
    
    
    
    
    
    
    
    # Apprentissage parceling
    darty_learn_parceling <- darty_learn_dataset
    darty_learn_parceling$classe_SCORE <- cut2(darty_learn_parceling$SCORE, g=10)
    darty_learn_parceling_part <- darty_learn_parceling[darty_learn_dataset$SCORE>=cut[j],]
    darty_learn_parceling_part <- darty_learn_parceling_part[!(is.na(darty_learn_parceling_part$TOP_PIC)),]
    
    poids_part <- sqldf(
      'select distinct count(TOP_PIC) as count, classe_SCORE, TOP_PIC
      from darty_learn_parceling_part
      group by classe_SCORE, TOP_PIC
      '
    )
    
    poids_bon <- poids_part[as.numeric(poids_part$TOP_PIC)==0,]
    poids_mauvais <- poids_part[as.numeric(poids_part$TOP_PIC)==1,]
    poids_bon$TOP_PIC <- NULL
    poids_mauvais$TOP_PIC <- NULL
    poids <- merge(poids_bon, poids_mauvais, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
    poids$poids_final <- poids$count.y/(poids$count.x+poids$count.y)
    poids$count.x <- NULL
    poids$count.y <- NULL
    
    darty_learn_parceling <- merge(darty_learn_parceling, poids, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
    darty_learn_parceling$poids_final.x <- NULL
    darty_learn_parceling$poids_final.y <- NULL
    # darty_learn_parceling$poids_final <- ifelse(is.na(darty_learn_parceling$poids_final), 1, darty_learn_parceling$poids_final)
    
    fun_binom <- function(x) {
      return(rbinom(1,1,x))
    }
    darty_learn_parceling[(is.na(darty_learn_parceling$TOP_PIC)|(darty_learn_parceling$SCORE < cut[j])),"ESSAI"] <- sapply(darty_learn_parceling[(is.na(darty_learn_parceling$TOP_PIC)|(darty_learn_parceling$SCORE < cut[j])),"poids_final"],fun_binom)
    darty_learn_parceling[(is.na(darty_learn_parceling$TOP_PIC)|(darty_learn_parceling$SCORE < cut[j])),"TOP_PIC"] <- ifelse(darty_learn_parceling[(is.na(darty_learn_parceling$TOP_PIC)|(darty_learn_parceling$SCORE < cut[j])),"ESSAI"]==0,0,1)
    darty_learn_parceling$ESSAI <- NULL
    darty_learn_parceling$poids[darty_learn_parceling$TOP_PIC==1] <- cost_fp
    darty_learn_parceling$poids[darty_learn_parceling$TOP_PIC==0] <- cost_fn
    
    darty_model_parcelling <- glm(TOP_PIC ~ ., family = binomial(link='logit'), darty_learn_parceling[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")], weights = darty_learn_parceling$poids)
    try(darty_model_parcelling.pred <- predict(darty_model_parcelling, darty_test[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")]), silent = TRUE)
    # darty_model_parcelling.erreur <- sum(abs(darty_test[,"TOP_PIC"]-darty_model_parcelling.pred))/nrow(darty_test)
    darty_model_parcelling.gini <- rcorr.cens(darty_model_parcelling.pred,darty_test[,"TOP_PIC"])[[2]]
    
    
    
    
    
    
    
    
    
    
    
    # Apprentissage twins
    darty_learn_twins <- darty_learn_dataset
    darty_learn_twins$acc <- ifelse(((darty_learn_twins$SCORE>=cut[j])&(!(is.na(darty_learn_twins$TOP_PIC)))),1,0)
    
    darty_model_acc <- glm(acc ~ ., family = binomial(link='logit'), darty_learn_twins[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","acc")],weights = darty_learn_twins$poids)
    
    try(darty_learn_twins$score_acc <- predict(darty_model_acc,darty_learn_twins), silent = TRUE)
    try(darty_learn_twins$score_def <- predict(darty_model,darty_learn_twins), silent = TRUE)
    
    try(darty_model_twins <- glm(TOP_PIC ~ score_acc + score_def, family = binomial(link='logit'), darty_learn_twins[darty_learn_twins$acc==1,],weights=darty_learn_twins$poids[darty_learn_twins$acc==1]), silent = TRUE)
    
    darty_learn_twins_ref1 <- darty_learn_twins[darty_learn_twins$acc==0,]
    darty_learn_twins_ref1$TOP_PIC <- 1
    try(darty_learn_twins_ref1$poids <- predict(darty_model_twins,darty_learn_twins_ref1, type = 'response'), silent = TRUE)
    
    darty_learn_twins_ref2 <- darty_learn_twins[darty_learn_twins$acc==0,]
    darty_learn_twins_ref2$TOP_PIC <- 0
    try(darty_learn_twins_ref2$poids <- predict(darty_model_twins,darty_learn_twins_ref1, type = 'response'), silent = TRUE)
    
    darty_learn_twins_final <- rbind(darty_learn_twins[darty_learn_twins$acc==1,],darty_learn_twins_ref1,darty_learn_twins_ref2)
    darty_model_twins_final <- glm(TOP_PIC ~., family = binomial(link='logit'), darty_learn_twins_final[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")], weights=darty_learn_twins_final$poids)
    
    try(darty_model_twins_final.pred <- predict(darty_model_twins_final, darty_test[,c("csp_emb","habit_loyer","cspcj","anc_dclem","nb_enf","TOP_PIC")]), silent = TRUE)
    darty_model_twins_final.gini <- rcorr.cens(darty_model_twins_final.pred,darty_test[,"TOP_PIC"])[[2]]
    
    
    
    }
    darty_model.pred <- predict(darty_model, darty_test)
    # darty_model_nnet.pred <- predict(darty_model_nnet, darty_test)
    
    darty_model.gini <- rcorr.cens(darty_model.pred,darty_test[,"TOP_PIC"])[[2]]
    # darty_model_nnet.gini <- rcorr.cens(darty_model_nnet.pred,darty_test[,"TOP_PIC"])[[2]]
    
    # darty_model_rpart.pred <- predict(darty_model_rpart, darty_test)
    # darty_model_rpart.gini <-  rcorr.cens(darty_model_rpart.pred,darty_test[,"TOP_PIC"])[[2]]
    
    # darty_model_rforest.pred <- predict(darty_model_rforest, darty_test, type="prob")[,2]
    # darty_model_rforest.gini <- rcorr.cens(darty_model_rforest.pred,darty_test[,"TOP_PIC"])[[2]]
    # 
    # darty_model_deep.pred <- as.data.frame(h2o.predict(darty_model_h2o_deep, darty_test_h2o))
    # darty_model_deep.gini <- rcorr.cens(darty_model_deep.pred[,3],darty_test[,"TOP_PIC"])[[2]]
    # 
    # darty_model_gbm.pred <- as.data.frame(h2o.predict(darty_model_h2o_gbm, darty_test_h2o))
    # darty_model_gbm.gini <- rcorr.cens(darty_model_gbm.pred[,3],darty_test[,"TOP_PIC"])[[2]]
    # 
    # darty_model_svm.pred <- predict(darty_model_svm, darty_test, probability = TRUE)
    # darty_model_svm.gini <- rcorr.cens(attr(darty_model_svm.pred,"probabilities")[,2],darty_test[,"TOP_PIC"])[[2]]
    
    darty_model_gen.pred <- mixmodPredict(darty_test[,1:5], darty_model_gen@bestResult)@proba[,1]
    darty_model_gen.gini <- rcorr.cens(darty_model_gen.pred,darty_test[,"TOP_PIC"])[[2]]
    
    # if (length(darty_learn_dataset[darty_learn_dataset$SCORE<cut[j],1]) > 0) {
    # taux[b,,j] <- c(darty_model.gini,darty_model_nnet.gini,darty_model_rpart.gini,darty_model_rforest.gini, darty_model_deep.gini, darty_model_gbm.gini, darty_model_svm.gini, darty_model_gen.gini, darty_model_aumgentation.gini,darty_model_pond.gini,darty_model_reclass.gini,darty_model_parcelling.gini,darty_model_twins_final.gini)
    # }else {
    # taux[b,,j] <- c(darty_model.gini,darty_model_nnet.gini,darty_model_rpart.gini,darty_model_rforest.gini, darty_model_deep.gini, darty_model_gbm.gini, darty_model_svm.gini, darty_model_gen.gini, darty_model.gini,darty_model.gini,darty_model.gini,darty_model.gini,darty_model.gini)
    if (length(darty_learn_dataset[darty_learn_dataset$SCORE<cut[j],1]) > 0) {
      taux[b,,j] <- c(darty_model.gini,darty_model_gen.gini, darty_model_aumgentation.gini,darty_model_pond.gini,darty_model_reclass.gini,darty_model_parcelling.gini,darty_model_twins_final.gini)
    }else {
      taux[b,,j] <- c(darty_model.gini,darty_model_gen.gini, darty_model.gini,darty_model.gini,darty_model.gini,darty_model.gini,darty_model.gini)
      
    }
  }
}




#### Calcul de la moyenne et de la variance du gini ####

# acceptance <- matrix(0,length(cut))
# 
# for (i in 1:length(cut)) {
# acceptance[i] <- length(darty_tot[((darty_tot$SCORE>=cut[i])&(!is.na(darty_tot$TOP_PIC))),1]) / length(darty_tot[,1])
# }
# 
# plot(x = acceptance[1:12], y = taux[1,1:12], xlim=c(1,0.45), ylim=c(-0.15,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=15, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[2,1:12], xlim=c(1,0.45), ylim=c(-0.15,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=16, xaxt = "n", col=34, yaxt="n", type='o')
# # par(new=TRUE)
# # plot(x = acceptance[1:12], y = taux[3,1:12], xlim=c(1,0.45), ylim=c(-0.15,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=17, col=49, xaxt = "n", yaxt="n", type='o')
# # par(new=TRUE)
# # plot(x = acceptance[1:12], y = taux[4,1:12], xlim=c(1,0.45), ylim=c(-0.15,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=18, col=97, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[5,1:12], xlim=c(1,0.45), ylim=c(-0.15,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[6,1:12], xlim=c(1,0.45), ylim=c(-0.15,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=8, col=590, xaxt = "n", yaxt="n", type='o')
# # par(new=TRUE)
# # plot(x = acceptance[1:12], y = taux[7,1:12], xlim=c(1,0.45), ylim=c(-0.15,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, col=376, xaxt = "n", yaxt="n", type='o')
# 
# axis(1, at=pretty(acceptance[1:12]), lab=paste0(pretty(acceptance[1:12]) * 100, " %"), las=TRUE)
# axis(2, at=pretty(taux[1,1:12]), lab=pretty(taux[1,1:12] * 100), las=TRUE)
# 
# legend(1,0.2, 
#        pch = c(15,16,7,8), 
#        col=c(1,34,653,590),legend= c("Logistic regression","Neural network","Deep Learning H2O","Gradient Boosting"),cex=1.5)
# 
# 
# 
# plot(x = acceptance[1:12], y = taux[1,1:12], xlim=c(1,0.45), ylim=c(0.565,0.582), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=15, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[8,1:12], xlim=c(1,0.45), ylim=c(0.565,0.582), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=16, xaxt = "n", col=34, yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[9,1:12], xlim=c(1,0.45), ylim=c(0.565,0.582), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=17, col='green', xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[10,1:12], xlim=c(1,0.45), ylim=c(0.565,0.582), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=18, col='orange', xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[11,1:12], xlim=c(1,0.45), ylim=c(0.565,0.582), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[12,1:12], xlim=c(1,0.45), ylim=c(0.565,0.582), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=8, col=590, xaxt = "n", yaxt="n", type='o')
# 
# axis(1, at=pretty(acceptance[1:12]), lab=paste0(pretty(acceptance[1:12]) * 100, " %"), las=TRUE)
# axis(2, at=pretty(taux[1,1:12], n=50), lab=pretty(taux[1,1:12] * 100, n=50), las=TRUE)
# 
# legend(1,0.55, 
#        pch = c(15,16,17,18,7,8), 
#        col=c(1,34,'green', 'orange', 653,590),legend= c("Logistic regression","Augmentation","Reweighting","Reclassification","Parceling","Twins"),cex=1.5)


load("taux_darty.RData")

acceptance <- matrix(0,length(cut))

for (i in 1:length(cut)) {
  acceptance[i] <- length(darty[((darty_tot$SCORE>=cut[i])&(!is.na(darty_tot$TOP_PIC))),1]) / length(darty[,1])
}


myMoyenne <- function(x){
  y <- array(NA,c(10,7))
    for (k in 1:10) {
      for (j in 1:7) {
        y[k,j] <- mean(x[1:5,j,k])
      }
    }
  y
}

myVariance <- function(x){
  y <- array(NA,c(10,7))
    for (k in 1:10) {
      for (j in 1:7) {
        y[k,j] <- var(x[1:5,j,k])
      }
    }
  y
}

moyenne <- myMoyenne(taux)
variance <- myVariance(taux)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tikzDevice)

tikz(file="electronics.tex", width=8, height=3.2, engine="pdftex")
plot(x = acceptance[1:10], y = moyenne[1:10,1], xlim=c(1,.6), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=15, xaxt = "n", yaxt="n", type='o', lty=1)
# lines(x = acceptance[1:10], y = moyenne[1:10,1] + 2*sqrt(variance[1:10,1]))
# par(new=TRUE)
# plot(x = acceptance[1:10], y = moyenne[1:10,2], xlim=c(1,.6), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=16, xaxt = "n", col=34, yaxt="n", type='o',lty=2)
par(new=TRUE)
plot(x = acceptance[1:10], y = moyenne[1:10,3], xlim=c(1,.6), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=17, col=300, xaxt = "n", yaxt="n", type='o',lty=3)
par(new=TRUE)
# plot(x = acceptance[1:10], y = moyenne[1:10,4], xlim=c(1,.6), ylim=c(.56,0.574), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=18, col=97, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:10], y = moyenne[1:10,5], xlim=c(1,.6), ylim=c(.56,0.574), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
plot(x = acceptance[1:10], y = moyenne[1:10,6], xlim=c(1,.6), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=8, col=590, xaxt = "n", yaxt="n", type='o',lty=4)
# par(new=TRUE)
# plot(x = acceptance[1:10], y = moyenne[1:10,7], xlim=c(1,.6), ylim=c(.56,0.574), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, col='green', xaxt = "n", yaxt="n", type='o')

axis(1, at=pretty(acceptance[1:10]), lab=paste0(pretty(acceptance[1:10]) * 100, " %"), las=TRUE)
axis(2, at=pretty(quantile(moyenne, probs = seq(0,1,0.1))), lab=pretty(quantile(moyenne, probs = seq(0,1,0.1)) * 100), las=TRUE)

legend(1,0.33,
       pch = c(15,17,8),
       lty=c(1,3,4),
       col=c(1,300,590),legend= c("Financed","Augmentation","Parcelling"),cex=0.75)

dev.off()

# gini.darty_part <- data.frame(moyenne=moyenne[,1], variance1=moyenne[,1] + sqrt(variance[,1]), variance2=moyenne[,1] - sqrt(variance[,1]))
# gini.darty_nnet <- data.frame(moyenne=moyenne[,2], variance1=moyenne[,2] + sqrt(variance[,2]), variance2=moyenne[,2] - sqrt(variance[,2]))
# gini.darty_augmentation <- data.frame(moyenne=moyenne[,3], variance1=moyenne[,3] + sqrt(variance[,3]), variance2=moyenne[,3] - sqrt(variance[,3]))
# gini.darty_pond <- data.frame(moyenne=moyenne[,4], variance1=moyenne[,4] + sqrt(variance[,4]), variance2=moyenne[,4] - sqrt(variance[,4]))
# gini.darty_reclass <- data.frame(moyenne=moyenne[,5], variance1=moyenne[,5] + sqrt(variance[,5]), variance2=moyenne[,5] - sqrt(variance[,5]))
# gini.darty_parceling <- data.frame(moyenne=moyenne[,6], variance1=moyenne[,6] + sqrt(variance[,6]), variance2=moyenne[,6] - sqrt(variance[,6]))
# gini.darty_twins <- data.frame(moyenne=moyenne[,7], variance1=moyenne[,7] + sqrt(variance[,7]), variance2=moyenne[,7] - sqrt(variance[,7]))
# 
# plot(x = cut, y = gini.darty_part$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Taux d\'erreur estim? pour 10 000 observations (train/test)')
# lines(x = cut, y = gini.darty_part$variance1, col='red')
# lines(x = cut, y = gini.darty_part$variance2, col='red')
# par(new=TRUE)
# plot(x = cut, y = gini.darty_nnet$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Taux d\'erreur estim? pour 10 000 observations (train/test)', col='orange')
# lines(x = cut, y = gini.darty_nnet$variance1)
# lines(x = cut, y = gini.darty_nnet$variance2)
# par(new=TRUE)
# plot(x = cut, y = gini.darty_augmentation$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Taux d\'erreur estim? pour 10 000 observations (train/test)', col='blue')
# lines(x = cut, y = gini.darty_augmentation$variance1, col='green')
# lines(x = cut, y = gini.darty_augmentation$variance2, col='green')
# par(new=TRUE)
# plot(x = cut, y = gini.darty_pond$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Taux d\'erreur estim? pour 10 000 observations (train/test)', col='red')
# lines(x = cut, y = gini.darty_pond$variance1, col='green')
# lines(x = cut, y = gini.darty_pond$variance2, col='green')
# par(new=TRUE)
# plot(x = cut, y = gini.darty_reclass$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Taux d\'erreur estim? pour 10 000 observations (train/test)', col='red')
# lines(x = cut, y = gini.darty_reclass$variance1, col='green')
# lines(x = cut, y = gini.darty_reclass$variance2, col='green')
# par(new=TRUE)
# plot(x = cut, y = gini.darty_parceling$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Taux d\'erreur estim? pour 10 000 observations (train/test)', col='red')
# lines(x = cut, y = gini.darty_parceling$variance1, col='green')
# lines(x = cut, y = gini.darty_parceling$variance2, col='green')
# par(new=TRUE)
# plot(x = cut, y = gini.darty_twins$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Taux d\'erreur estim? pour 10 000 observations (train/test)', col='red')
# lines(x = cut, y = gini.darty_twins$variance1, col='green')
# lines(x = cut, y = gini.darty_twins$variance2, col='green')





