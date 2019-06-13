library(Hmisc)
library(sqldf)
library(h2o)
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
# setwd("O:/DC/PRI/STAGE/Th?se CIFRE SCORING/Code R/Simulation R?int?gration/real_data")
set.seed(10)


#### D?cathlon #### 
decathlon_tot <- read.csv(file = "DECAT_SO14011412.csv",sep=";", fileEncoding="latin1")
decathlon_tot <- decathlon_tot[,c(2,15:58)]
decathlon_tot$MOIS <- NULL
decathlon_tot$TYPE <- NULL
decathlon_tot$cat <- NULL
decathlon_tot$TOP_PIC <- ifelse(decathlon_tot$TOP_PIC=="1", 1, ifelse(decathlon_tot$TOP_PIC=="0",0,NA))

decathlon <- subset(decathlon_tot, !is.na(decathlon_tot$TOP_PIC))

decathlon$habit <- ifelse(decathlon$habit_1==1,'1',ifelse(decathlon$habit_2==1,'2','3'))
decathlon$sitfam <- ifelse(decathlon$sitfam_1==1,'1',ifelse(decathlon$sitfam_2==1,'2',ifelse(decathlon$sitfam_3==1,'3','4')))
decathlon$nb_enf <- ifelse(decathlon$nb_enf_1==1,'1',ifelse(decathlon$nb_enf_2==1,'2',ifelse(decathlon$nb_enf_3==1,'3',ifelse(decathlon$nb_enf_4==1,'4','5'))))
decathlon$anc_dnaiss <- ifelse(decathlon$anc_dnaiss_1==1,'1',ifelse(decathlon$anc_dnaiss_2==1,'2',ifelse(decathlon$anc_dnaiss_3==1,'3','4')))
decathlon$habit_anc_dclem <- ifelse(decathlon$habit_anc_dclem_1==1,'1',ifelse(decathlon$habit_anc_dclem_2==1,'2',ifelse(decathlon$habit_anc_dclem_3==1,'3',ifelse(decathlon$habit_anc_dclem_4==1,'4',ifelse(decathlon$habit_anc_dclem_5==1,'5','6')))))
decathlon$csp_anc_demba <- ifelse(decathlon$csp_anc_demba_1==1,'1',ifelse(decathlon$csp_anc_demba_2==1,'2',ifelse(decathlon$csp_anc_demba_3==1,'3',ifelse(decathlon$csp_anc_demba_4==4,'4',ifelse(decathlon$csp_anc_demba_5==1,'5',ifelse(decathlon$csp_anc_demba_6==1,'6','7'))))))
decathlon$revnu <- ifelse(decathlon$revnu_1==1,'1',ifelse(decathlon$revnu_2==1,'2',ifelse(decathlon$revnu_3==1,'3',ifelse(decathlon$revnu_4==1,'4',ifelse(decathlon$revnu_5==1,'5',6)))))
decathlon$sitfam_cspcj_amembc <- ifelse(decathlon$sitfam_cspcj_amembc_1==1,'1',ifelse(decathlon$sitfam_cspcj_amembc_2==1,'2',ifelse(decathlon$sitfam_cspcj_amembc_3==1,'3',ifelse(decathlon$sitfam_cspcj_amembc_4==1,'4','5'))))

decathlon <- decathlon[!(decathlon$revnu=="6"),]

decathlon <- decathlon[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC","SCORE")]

# Randomization
decathlon <- decathlon[sample(nrow(decathlon)),]
decathlon[,1:8] <- lapply(decathlon[,1:8], function(x) as.factor(x))
rownames(decathlon) <- seq(length=nrow(decathlon))

row_decathlon <- nrow(decathlon)

# decathlon_test <- decathlon[1:round(row_decathlon*0.2),]
# decathlon_test$poids <- 1

# decathlon_learn <- decathlon[(round(row_decathlon*0.2)+1):row_decathlon,]

cut=c(230,235,240,245,250,255,260,265,270,280,285,290)
cost_fp = 1
cost_fn = 1

taux <- array(0,c(10,7,length(cut)))


for(b in 1:5) {  

  decathlon_test <- decathlon[(round((b-1)*row_decathlon/5)+1):(round(b*row_decathlon/5)),]
  decathlon_learn_dataset <- decathlon[-(((b-1)*round(row_decathlon/5)+1):(b*round(row_decathlon/5))),]
  
  decathlon_test$poids <- 1
  decathlon_learn_dataset$poids <- 1
  

  for (j in 1:length(cut)) {
      
    decathlon_learn_acc_dataset <- decathlon_learn_dataset[decathlon_learn_dataset$SCORE>=cut[j],]
    decathlon_learn_acc_dataset <- decathlon_learn_acc_dataset[!(is.na(decathlon_learn_acc_dataset$TOP_PIC)),]
    decathlon_learn_acc_dataset$poids[decathlon_learn_acc_dataset$TOP_PIC==' 1'] <- cost_fp
    decathlon_learn_acc_dataset$poids[decathlon_learn_acc_dataset$TOP_PIC==' 0'] <- cost_fn
    
    decathlon_model <- glm(TOP_PIC ~ ., "binomial",decathlon_learn_acc_dataset[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")], weights = decathlon_learn_acc_dataset$poids)
  
    
    # decathlon_learn_acc_dataset_h2o <- decathlon_learn_acc_dataset
    # decathlon_learn_acc_dataset_h2o$TOP_PIC <- as.factor(decathlon_learn_acc_dataset_h2o$TOP_PIC)
    # decathlon_learn_acc_dataset_h2o <- as.h2o(decathlon_learn_acc_dataset_h2o[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")])
    # decathlon_test_h2o <- decathlon_test[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc")]
    # decathlon_test_h2o <- data.frame(lapply(decathlon_test_h2o, function(x) as.factor(x)))
    # decathlon_test_h2o <- as.h2o(decathlon_test_h2o)
    # decathlon_model_h2o_deep <- h2o.deeplearning(x=1:8, y="TOP_PIC",training_frame=decathlon_learn_acc_dataset_h2o)
    # 
    # decathlon_model_h2o_gbm <- h2o.gbm(x=1:8, y="TOP_PIC",training_frame=decathlon_learn_acc_dataset_h2o)
    # 
    # decathlon_model_nnet <- nnet(as.factor(TOP_PIC) ~ .,decathlon_learn_acc_dataset[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")], size = 10, maxiter = 50)
    # 
    # decathlon_model_rpart <- rpart(TOP_PIC ~ .,decathlon_learn_acc_dataset[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")],weights=decathlon_learn_acc_dataset$poids)
    # decathlon_model_rforest <- randomForest(as.factor(TOP_PIC) ~ .,decathlon_learn_acc_dataset[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")])
    # 
    # decathlon_learn_acc_dataset_svm <- decathlon_learn_acc_dataset
    # decathlon_learn_acc_dataset_svm$TOP_PIC <- ifelse(decathlon_learn_acc_dataset_svm$TOP_PIC==1,1,-1)
    # decathlon_model_svm <- svm(as.factor(TOP_PIC) ~ .,decathlon_learn_acc_dataset_svm[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")],weights=decathlon_learn_acc_dataset$poids, probability = TRUE)
    
    decathlon_learn_gen <- decathlon_learn_dataset[decathlon_learn_dataset$SCORE<cut[j],]
    decathlon_learn_gen$TOP_PIC <- NA
    decathlon_learn_gen <- rbind(decathlon_learn_gen, decathlon_learn_acc_dataset)
    decathlon_learn_gen$TOP_PIC <- ifelse(decathlon_learn_gen$TOP_PIC == 0, 2, 1)
    
    decathlon_model_gen <- mixmodCluster(data=decathlon_learn_gen[,1:8], knownLabels = decathlon_learn_gen[,9], nbCluster=2)
    
    
    if (length(decathlon_learn_dataset[decathlon_learn_dataset$SCORE<cut[j],1]) > 0) {
        
      
      # decathlon_learn_ref_dataset <- decathlon_learn_dataset[is.na(decathlon_learn_dataset$TOP_PIC),]
      # decathlon_learn_ref_dataset <- rbind(decathlon_learn_ref_dataset, decathlon_learn_dataset[decathlon_learn_dataset$SCORE<cut[j],])
      decathlon_learn_ref_dataset <- decathlon_learn_dataset[decathlon_learn_dataset$SCORE<cut[j],]
      
      decathlon_learn_ref_dataset1 <- decathlon_learn_ref_dataset
      try(decathlon_learn_ref_dataset1$poids <- predict(decathlon_model, decathlon_learn_ref_dataset, type='response'), silent = TRUE)
      decathlon_learn_ref_dataset1$TOP_PIC <- 1
      
      decathlon_learn_ref_dataset2 <- decathlon_learn_ref_dataset
      decathlon_learn_ref_dataset2$poids <- 1-decathlon_learn_ref_dataset1$poids
      decathlon_learn_ref_dataset2$TOP_PIC <- 0
      
      decathlon_learn_pond <- rbind(decathlon_learn_acc_dataset,decathlon_learn_ref_dataset1,decathlon_learn_ref_dataset2)
      decathlon_model_pond <- glm(TOP_PIC ~ ., "binomial",decathlon_learn_pond[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")], weights = decathlon_learn_pond$poids)
      try(decathlon_model_pond.pred <- predict(decathlon_model_pond, decathlon_test[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")]), silent = TRUE)
      # decathlon_model_pond.erreur <- sum(abs(decathlon_test[,"TOP_PIC"]-decathlon_model_pond.pred))/nrow(decathlon_test)
      decathlon_model_pond.gini <- rcorr.cens(decathlon_model_pond.pred,decathlon_test[,"TOP_PIC"])[[2]]
      
      
      
      decathlon_learn_ref_reclass <- decathlon_learn_ref_dataset
      try(decathlon_learn_ref_reclass$TOP_PIC <- as.integer(round(predict(decathlon_model, decathlon_learn_ref_reclass, type="response"),digits = 0)), silent = TRUE)
      decathlon_learn_ref_reclass <- rbind(decathlon_learn_ref_reclass,decathlon_learn_acc_dataset)
      
      decathlon_model_reclass <- glm(TOP_PIC ~ ., "binomial",decathlon_learn_acc_dataset[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")], weights = decathlon_learn_acc_dataset$poids)
      
      try(decathlon_model_reclass.pred <- predict(decathlon_model_reclass, decathlon_test[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")]), silent = TRUE)
      # decathlon_model_reclass.erreur <- sum(abs(decathlon_test[,"TOP_PIC"]-decathlon_model_reclass.pred))/nrow(decathlon_test)
      decathlon_model_reclass.gini <- rcorr.cens(decathlon_model_reclass.pred,decathlon_test[,"TOP_PIC"])[[2]]
      
      
      decathlon_learn_augmentation <- decathlon_learn_dataset
      decathlon_learn_augmentation$classe_SCORE <- cut2(decathlon_learn_augmentation$SCORE, g=10)
      decathlon_learn_augmentation$acc <- ifelse(((decathlon_learn_augmentation$SCORE>=cut[j])&(!(is.na(decathlon_learn_augmentation$TOP_PIC)))),1,0)
      
      poids2 <- sqldf(
        'select distinct count(*) as count, classe_SCORE, acc
      from decathlon_learn_augmentation
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
      
      decathlon_learn_augmentation <- merge(decathlon_learn_augmentation, poids_tot, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
      decathlon_model_augmentation <- glm(TOP_PIC ~ ., "binomial", decathlon_learn_augmentation[((decathlon_learn_augmentation$SCORE>=cut[j])&(!(is.na(decathlon_learn_augmentation$TOP_PIC)))),c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")], weights = decathlon_learn_augmentation[((decathlon_learn_augmentation$SCORE>=cut[j])&(!(is.na(decathlon_learn_augmentation$TOP_PIC)))),"poidsfinal"])
      try(decathlon_model_aumgentation.pred <- predict(decathlon_model_augmentation, decathlon_test[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")]), silent = TRUE)
      # decathlon_model_augmentation.erreur <- sum(abs(data_test_pred[j,,1]-model_pred_augmente.pred))/m_test
      decathlon_model_aumgentation.gini <- rcorr.cens(decathlon_model_aumgentation.pred,decathlon_test[,"TOP_PIC"])[[2]]
      
      
      decathlon_learn_parceling <- decathlon_learn_dataset
      decathlon_learn_parceling$classe_SCORE <- cut2(decathlon_learn_parceling$SCORE, g=10)
      # decathlon_learn_parceling_part <- decathlon_learn_parceling[decathlon_learn_dataset$SCORE>=cut[j],]
      # decathlon_learn_parceling_part <- decathlon_learn_parceling_part[!(is.na(decathlon_learn_parceling_part$TOP_PIC)),]
      # decathlon_learn_parceling_part$classe_SCORE <- cut2(decathlon_learn_parceling_part$SCORE, g=10)
      decathlon_learn_parceling_part <- decathlon_learn_parceling[decathlon_learn_parceling$SCORE>=cut[j],]
      decathlon_learn_parceling_part <- decathlon_learn_parceling_part[!(is.na(decathlon_learn_parceling_part$TOP_PIC)),]
      
      poids_part <- sqldf(
        'select distinct count(TOP_PIC) as count, classe_SCORE, TOP_PIC
      from decathlon_learn_parceling_part
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
      
      decathlon_learn_parceling <- merge(decathlon_learn_parceling, poids, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
      decathlon_learn_parceling$poids_final.x <- NULL
      decathlon_learn_parceling$poids_final.y <- NULL
      # decathlon_learn_parceling$poids_final <- ifelse(is.na(decathlon_learn_parceling$poids_final), 1, decathlon_learn_parceling$poids_final)
      
      fun_binom <- function(x) {
        return(rbinom(1,1,x))
      }
      decathlon_learn_parceling[(is.na(decathlon_learn_parceling$TOP_PIC)|(decathlon_learn_parceling$SCORE < cut[j])),"ESSAI"] <- sapply(decathlon_learn_parceling[(is.na(decathlon_learn_parceling$TOP_PIC)|(decathlon_learn_parceling$SCORE < cut[j])),"poids_final"],fun_binom)
      decathlon_learn_parceling[(is.na(decathlon_learn_parceling$TOP_PIC)|(decathlon_learn_parceling$SCORE < cut[j])),"TOP_PIC"] <- ifelse(decathlon_learn_parceling[(is.na(decathlon_learn_parceling$TOP_PIC)|(decathlon_learn_parceling$SCORE < cut[j])),"ESSAI"]==0,0,1)
      decathlon_learn_parceling$ESSAI <- NULL
      decathlon_learn_parceling$poids[decathlon_learn_parceling$TOP_PIC==1] <- cost_fp
      decathlon_learn_parceling$poids[decathlon_learn_parceling$TOP_PIC==0] <- cost_fn
      
      decathlon_model_parcelling <- glm(TOP_PIC ~ ., family = binomial(link='logit'), decathlon_learn_parceling[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")], weights = decathlon_learn_parceling$poids)
      try(decathlon_model_parcelling.pred <- predict(decathlon_model_parcelling, decathlon_test[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")]), silent = TRUE)
      # decathlon_model_parcelling.erreur <- sum(abs(decathlon_test[,"TOP_PIC"]-decathlon_model_parcelling.pred))/nrow(decathlon_test)
      decathlon_model_parcelling.gini <- rcorr.cens(decathlon_model_parcelling.pred,decathlon_test[,"TOP_PIC"])[[2]]
      
      
      
      decathlon_learn_twins <- decathlon_learn_dataset
      decathlon_learn_twins$acc <- ifelse(((decathlon_learn_twins$SCORE>=cut[j])&(!(is.na(decathlon_learn_twins$TOP_PIC)))),1,0)
      
      decathlon_model_acc <- glm(acc ~ ., family = binomial(link='logit'), decathlon_learn_twins[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "acc")],weights = decathlon_learn_twins$poids)
      
      try(decathlon_learn_twins$score_acc <- predict(decathlon_model_acc,decathlon_learn_twins), silent = TRUE)
      try(decathlon_learn_twins$score_def <- predict(decathlon_model,decathlon_learn_twins), silent = TRUE)
      
      try(decathlon_model_twins <- glm(TOP_PIC ~ score_acc + score_def, family = binomial(link='logit'), decathlon_learn_twins[decathlon_learn_twins$acc==1,],weights=decathlon_learn_twins$poids[decathlon_learn_twins$acc==1]), silent = TRUE)
      
      decathlon_learn_twins_ref1 <- decathlon_learn_twins[decathlon_learn_twins$acc==0,]
      decathlon_learn_twins_ref1$TOP_PIC <- 1
      try(decathlon_learn_twins_ref1$poids <- predict(decathlon_model_twins,decathlon_learn_twins_ref1, type = 'response'), silent = TRUE)
      
      decathlon_learn_twins_ref2 <- decathlon_learn_twins[decathlon_learn_twins$acc==0,]
      decathlon_learn_twins_ref2$TOP_PIC <- 0
      try(decathlon_learn_twins_ref2$poids <- 1-predict(decathlon_model_twins,decathlon_learn_twins_ref2, type = 'response'), silent = TRUE)
      
      decathlon_learn_twins_final <- rbind(decathlon_learn_twins[decathlon_learn_twins$acc==1,],decathlon_learn_twins_ref1,decathlon_learn_twins_ref2)
      decathlon_model_twins_final <- glm(TOP_PIC ~., family = binomial(link='logit'), decathlon_learn_twins_final[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")], weights=decathlon_learn_twins_final$poids)
      
      try(decathlon_model_twins_final.pred <- predict(decathlon_model_twins_final, decathlon_test[,c("habit","sitfam","nb_enf","anc_dnaiss","habit_anc_dclem","csp_anc_demba","revnu","sitfam_cspcj_amembc", "TOP_PIC")]), silent = TRUE)
      decathlon_model_twins_final.gini <- rcorr.cens(decathlon_model_twins_final.pred,decathlon_test[,"TOP_PIC"])[[2]]
      
      
      
      try(decathlon_model.pred <- predict(decathlon_model, decathlon_test), silent = TRUE)
      try(decathlon_model_nnet.pred <- predict(decathlon_model_nnet, decathlon_test), silent = TRUE)
      
      
  
    }
    
    
    decathlon_model.pred <- predict(decathlon_model, decathlon_test)
    # decathlon_model_nnet.pred <- predict(decathlon_model_nnet, decathlon_test)
    
    decathlon_model.gini <- rcorr.cens(decathlon_model.pred,decathlon_test[,"TOP_PIC"])[[2]]
    # decathlon_model_nnet.gini <- rcorr.cens(decathlon_model_nnet.pred,decathlon_test[,"TOP_PIC"])[[2]]
    
    # decathlon_model_rpart.pred <- predict(decathlon_model_rpart, decathlon_test)
    # decathlon_model_rpart.gini <-  rcorr.cens(decathlon_model_rpart.pred,decathlon_test[,"TOP_PIC"])[[2]]
    # 
    # decathlon_model_rforest.pred <- predict(decathlon_model_rforest, decathlon_test, type="prob")[,2]
    # decathlon_model_rforest.gini <- rcorr.cens(decathlon_model_rforest.pred,decathlon_test[,"TOP_PIC"])[[2]]
    # 
    # decathlon_model_deep.pred <- as.data.frame(h2o.predict(decathlon_model_h2o_deep, decathlon_test_h2o))
    # decathlon_model_deep.gini <- rcorr.cens(decathlon_model_deep.pred[,3],decathlon_test[,"TOP_PIC"])[[2]]
    # 
    # decathlon_model_gbm.pred <- as.data.frame(h2o.predict(decathlon_model_h2o_gbm, decathlon_test_h2o))
    # decathlon_model_gbm.gini <- rcorr.cens(decathlon_model_gbm.pred[,3],decathlon_test[,"TOP_PIC"])[[2]]
    # 
    # decathlon_model_svm.pred <- predict(decathlon_model_svm, decathlon_test, probability = TRUE)
    # decathlon_model_svm.gini <- rcorr.cens(attr(decathlon_model_svm.pred,"probabilities")[,2],decathlon_test[,"TOP_PIC"])[[2]]
    
    decathlon_model_gen.pred <- mixmodPredict(decathlon_test[,1:8], decathlon_model_gen@bestResult)@proba[,1]
    decathlon_model_gen.gini <- rcorr.cens(decathlon_model_gen.pred,decathlon_test[,"TOP_PIC"])[[2]]
    
    
    # if (length(decathlon_learn_dataset[decathlon_learn_dataset$SCORE<cut[j],1]) > 0) {
    #   taux[b,,j] <- c(decathlon_model.gini,decathlon_model_nnet.gini,decathlon_model_rpart.gini,decathlon_model_rforest.gini, decathlon_model_deep.gini, decathlon_model_gbm.gini, decathlon_model_svm.gini, decathlon_model_gen.gini, decathlon_model_aumgentation.gini,decathlon_model_pond.gini,decathlon_model_reclass.gini,decathlon_model_parcelling.gini,decathlon_model_twins_final.gini)
    # }else {
    #   taux[b,,j] <- c(decathlon_model.gini,decathlon_model_nnet.gini,decathlon_model_rpart.gini,decathlon_model_rforest.gini, decathlon_model_deep.gini, decathlon_model_gbm.gini, decathlon_model_svm.gini, decathlon_model_gen.gini, decathlon_model.gini,decathlon_model.gini,decathlon_model.gini,decathlon_model.gini,decathlon_model.gini)
    # }
    if (length(decathlon_learn_dataset[decathlon_learn_dataset$SCORE<cut[j],1]) > 0) {
      taux[b,,j] <- c(decathlon_model.gini, decathlon_model_gen.gini, decathlon_model_aumgentation.gini,decathlon_model_pond.gini,decathlon_model_reclass.gini,decathlon_model_parcelling.gini,decathlon_model_twins_final.gini)
    }else {
      taux[b,,j] <- c(decathlon_model.gini, decathlon_model_gen.gini, decathlon_model.gini,decathlon_model.gini,decathlon_model.gini,decathlon_model.gini,decathlon_model.gini)
    }
    
  }
}





acceptance <- matrix(0,length(cut))

# for (i in 1:length(cut)) {
#   acceptance[i] <- length(decathlon_tot[decathlon_tot$SCORE>=cut[i],1]) / length(decathlon_tot[,1])
# }




for (i in 1:length(cut)) {
  acceptance[i] <- length(decathlon[((decathlon_tot$SCORE>=cut[i])&(!is.na(decathlon_tot$TOP_PIC))),1]) / length(decathlon[,1])
}



load("taux_decathlon.RData")


myMoyenne <- function(x){
  y <- array(NA,c(12,7))
  for (k in 1:12) {
    for (j in 1:7) {
      y[k,j] <- mean(x[1:5,j,k])
    }
  }
  y
}

myVariance <- function(x){
  y <- array(NA,c(12,7))
  for (k in 1:12) {
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

tikz(file="sports.tex", width=8, height=3.2, engine="pdftex")
plot(x = acceptance[1:11], y = moyenne[1:11,1], xlim=c(1,.6), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=15, xaxt = "n", yaxt="n", type='o', lty = 1)
# par(new=TRUE)
# plot(x = acceptance[1:11], y = moyenne[1:11,2], xlim=c(1,.6), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=16, xaxt = "n", col=34, yaxt="n", type='o', lty = 2)
par(new=TRUE)
plot(x = acceptance[1:11], y = moyenne[1:11,3], xlim=c(1,.6), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=17, col=300, xaxt = "n", yaxt="n", type='o', lty = 3)
# par(new=TRUE)
# plot(x = acceptance[1:11], y = moyenne[1:11,4], xlim=c(1,.6), ylim=c(0.42,0.5), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=18, col=97, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:11], y = moyenne[1:11,5], xlim=c(1,.6), ylim=c(0.42,0.5), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:11], y = moyenne[1:11,6], xlim=c(1,.6), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=8, col=590, xaxt = "n", yaxt="n", type='o', lty = 4)
par(new=TRUE)
# plot(x = acceptance[1:11], y = moyenne[1:11,7], xlim=c(1,.6), ylim=c(0.42,0.5), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, col='green', xaxt = "n", yaxt="n", type='o')

axis(1, at=pretty(acceptance[1:12]), lab=paste0(pretty(acceptance[1:12]) * 100, " %"), las=TRUE)
axis(2, at=pretty(quantile(moyenne, probs = seq(0,1,0.1))), lab=pretty(quantile(moyenne, probs = seq(0,1,0.1)) * 100), las=TRUE)

legend(1,0.36,
       pch = c(15,17,8),
       lty = c(1,3,4),
       col=c(1,300,590),legend=c("Financed","Augmentation","Parcelling"),cex=0.75)
dev.off()












plot(x = acceptance[1:12], y = taux[1,1:12], xlim=c(1,0.45), ylim=c(0.1,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=15, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[2,1:12], xlim=c(1,0.45), ylim=c(0.1,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=16, xaxt = "n", col=34, yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[3,1:12], xlim=c(1,0.45), ylim=c(0.1,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=17, col=49, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[4,1:12], xlim=c(1,0.45), ylim=c(0.1,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=18, col='orange', xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[5,1:12], xlim=c(1,0.45), ylim=c(0.1,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[6,1:12], xlim=c(1,0.45), ylim=c(0.1,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=8, col=590, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:12], y = taux[7,1:12], xlim=c(1,0.45), ylim=c(0.1,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, col=376, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[8,1:12], xlim=c(1,0.45), ylim=c(0.1,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, col=376, xaxt = "n", yaxt="n", type='o')

axis(1, at=pretty(acceptance[1:12]), lab=paste0(pretty(acceptance[1:12]) * 100, " %"), las=TRUE)
axis(2, at=pretty(taux[2,1:12]), lab=pretty(taux[2,1:12] * 100), las=TRUE)

legend(1,0.44, 
       pch = c(15,18,7,8,9), 
       col=c(1,"orange",653,590,376),legend= c("Logistic regression","Random Forest","Deep Learning H2O","Gradient Boosting","Multinomial model"),cex=1.5)



plot(x = acceptance[1:12], y = taux[1,1:12], xlim=c(1,0.45), ylim=c(0.49,0.59), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=15, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[8,1:12], xlim=c(1,0.45), ylim=c(0.49,0.59), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, xaxt = "n", col=376, yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[9,1:12], xlim=c(1,0.45), ylim=c(0.49,0.59), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=17, col='green', xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[10,1:12], xlim=c(1,0.45), ylim=c(0.49,0.59), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=18, col='orange', xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[11,1:12], xlim=c(1,0.45), ylim=c(0.49,0.59), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[12,1:12], xlim=c(1,0.45), ylim=c(0.49,0.59), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=8, col=590, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:12], y = taux[13,1:12], xlim=c(1,0.45), ylim=c(0.49,0.59), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=16, col=34, xaxt = "n", yaxt="n", type='o')

axis(1, at=pretty(acceptance[1:12]), lab=paste0(pretty(acceptance[1:12]) * 100, " %"), las=TRUE)
axis(2, at=pretty(taux[1,1:12], n=20), lab=pretty(taux[1,1:12] * 100, n=20), las=TRUE)

legend(0.94,0.55, 
       pch = c(15,9,17,18,7,8,16), 
       col=c(1,376,'green', 'orange', 653,590, 34),legend= c("Logistic regression","Multinomial model","Augmentation", "Reweighting","Reclassification","Parceling","Twins"),cex=1.2)







# gini.decathlon_part <- data.frame(moyenne=moyenne[,1], variance1=moyenne[,1] + sqrt(variance[,1]), variance2=moyenne[,1] - sqrt(variance[,1]))
# gini.decathlon_nnet <- data.frame(moyenne=moyenne[,2], variance1=moyenne[,2] + sqrt(variance[,2]), variance2=moyenne[,2] - sqrt(variance[,2]))
# gini.decathlon_augmentation <- data.frame(moyenne=moyenne[,3], variance1=moyenne[,3] + sqrt(variance[,3]), variance2=moyenne[,3] - sqrt(variance[,3]))
# gini.decathlon_pond <- data.frame(moyenne=moyenne[,4], variance1=moyenne[,4] + sqrt(variance[,4]), variance2=moyenne[,4] - sqrt(variance[,4]))
# gini.decathlon_reclass <- data.frame(moyenne=moyenne[,5], variance1=moyenne[,5] + sqrt(variance[,5]), variance2=moyenne[,5] - sqrt(variance[,5]))
# gini.decathlon_parceling <- data.frame(moyenne=moyenne[,6], variance1=moyenne[,6] + sqrt(variance[,6]), variance2=moyenne[,6] - sqrt(variance[,6]))
# gini.decathlon_twins <- data.frame(moyenne=moyenne[,7], variance1=moyenne[,7] + sqrt(variance[,7]), variance2=moyenne[,7] - sqrt(variance[,7]))
# 
# plot(x = cut, y = gini.decathlon_part$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Gini estim?')
# lines(x = cut, y = gini.decathlon_part$variance1, col='red')
# lines(x = cut, y = gini.decathlon_part$variance2, col='red')
# par(new=TRUE)
# plot(x = cut, y = gini.decathlon_nnet$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Gini estim?', col='orange')
# lines(x = cut, y = gini.decathlon_nnet$variance1)
# lines(x = cut, y = gini.decathlon_nnet$variance2)
# par(new=TRUE)
# plot(x = cut, y = gini.decathlon_augmentation$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Gini estim?', col='blue')
# lines(x = cut, y = gini.decathlon_augmentation$variance1, col='green')
# lines(x = cut, y = gini.decathlon_augmentation$variance2, col='green')
# par(new=TRUE)
# plot(x = cut, y = gini.decathlon_pond$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Gini estim?', col='red')
# lines(x = cut, y = gini.decathlon_pond$variance1, col='green')
# lines(x = cut, y = gini.decathlon_pond$variance2, col='green')
# par(new=TRUE)
# plot(x = cut, y = gini.decathlon_reclass$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Gini estim?', col='red')
# lines(x = cut, y = gini.decathlon_reclass$variance1, col='green')
# lines(x = cut, y = gini.decathlon_reclass$variance2, col='green')
# par(new=TRUE)
# plot(x = cut, y = gini.decathlon_parceling$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Gini estim?', col='red')
# lines(x = cut, y = gini.decathlon_parceling$variance1, col='green')
# lines(x = cut, y = gini.decathlon_parceling$variance2, col='green')
# par(new=TRUE)
# plot(x = cut, y = gini.decathlon_twins$moyenne, ylim=c(-0.3,0.7), ylab = 'Valeur moyenne du Gini', xlab = 'cut', main='Gini estim?', col='red')
# lines(x = cut, y = gini.decathlon_twins$variance1, col='green')
# lines(x = cut, y = gini.decathlon_twins$variance2, col='green')
