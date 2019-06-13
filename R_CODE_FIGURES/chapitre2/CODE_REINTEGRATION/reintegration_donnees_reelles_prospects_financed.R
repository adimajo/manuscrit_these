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
h2o.init()
library(tidyverse)
########### M?thodes de r?int?gration de refus?s : donn?es r?elles ########### 

###### Chargement des donn?es ###### 
setwd("Z:/6D1/329/Thèse CIFRE SCORING/Code R/Simulation Réintégration/real_data")
set.seed(1)


#### Prospects jeunes #### 
jeunesM3_tot <- read.csv(file = "JEUNES_SOCC14011412.csv",sep=";")
jeunesM3_tot$TOP_PIC <- ifelse(jeunesM3_tot$TOP_PIC=="1", 1, ifelse(jeunesM3_tot$TOP_PIC=="0",0,NA))
jeunesM3_tot$IDDOS <- NULL
jeunesM3 <- subset(jeunesM3_tot, !is.na(jeunesM3_tot$TOP_PIC))

jeunesM3[,2:7] <- lapply(jeunesM3[,2:7], function(x) as.factor(x))
jeunesM3 <- subset(jeunesM3, !((jeunesM3$revtot_r=="21") | (jeunesM3$nb_imp_max_r=="21") | (jeunesM3$anempcj_cspcj_r=="35")))


# Randomization
jeunesM3 <- jeunesM3[sample(nrow(jeunesM3)),]
rownames(jeunesM3) <- seq(length=nrow(jeunesM3))
row_jeunesM3 <- nrow(jeunesM3)
# jeunesM3[,1:5] <- lapply(jeunesM3[,1:5], function(x) as.factor(x))


###### Cr?ation du fichier test et des fichiers d'apprentissage ######

# Cr?ation du fichier test
row_jeunesM3 <- nrow(jeunesM3)
# 
# jeunesM3_test <- jeunesM3[1:row_jeunesM3,]
# jeunesM3_test <- jeunesM3_test[!(is.na(jeunesM3_test$TOP_PIC)),]

# jeunesM3_test <- jeunesM3[1:round(row_jeunesM3*0.2),]
# jeunesM3_test$poids <- 1

# Cr?ation des fichiers d'apprentissage

# jeunesM3_learn <- jeunesM3[(round(row_jeunesM3*0.2)+1):row_jeunesM3,]

cut=c(190,200,210,215,220,225,230,240,250)
cost_fp = 1
cost_fn = 1

taux <- array(0,c(5,7,length(cut)))

###### Apprentissage des mod?les ######

# Boucle sur les ensembles d'apprentissage
for(b in 1:5) {

  jeunesM3_test <- jeunesM3[(round((b-1)*row_jeunesM3/5)+1):(round(b*row_jeunesM3/5)),]
  jeunesM3_learn_dataset <- jeunesM3[-(((b-1)*round(row_jeunesM3/5)+1):(b*round(row_jeunesM3/5))),]
  
  jeunesM3_test$poids <- 1
  jeunesM3_learn_dataset$poids <- 1
  
  # colnames(jeunesM3_learn_dataset) <- colnames(jeunesM3)
  
  jeunesM3_learn_dataset$poids <- 1
  
  
# Boucle sur le cut

for (j in 1:length(cut)) {
  
  # Apprentissage logistique sur accept?s
  jeunesM3_learn_acc_dataset <- jeunesM3_learn_dataset[jeunesM3_learn_dataset$SCORE>=cut[j],]
  jeunesM3_learn_acc_dataset <- jeunesM3_learn_acc_dataset[!(is.na(jeunesM3_learn_acc_dataset$TOP_PIC)),]
  jeunesM3_learn_acc_dataset$poids[jeunesM3_learn_acc_dataset$TOP_PIC==1] <- cost_fp
  jeunesM3_learn_acc_dataset$poids[jeunesM3_learn_acc_dataset$TOP_PIC==0] <- cost_fn
  
  jeunesM3_model <- glm(TOP_PIC ~ ., "binomial",jeunesM3_learn_acc_dataset[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")], weights=jeunesM3_learn_acc_dataset$poids)
  
  # jeunesM3_learn_acc_dataset_h2o <- jeunesM3_learn_acc_dataset
  # jeunesM3_learn_acc_dataset_h2o$TOP_PIC <- as.factor(jeunesM3_learn_acc_dataset_h2o$TOP_PIC)
  # jeunesM3_learn_acc_dataset_h2o <- as.h2o(jeunesM3_learn_acc_dataset_h2o[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")])
  # jeunesM3_test_h2o <- jeunesM3_test[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r")]
  # jeunesM3_test_h2o <- data.frame(lapply(jeunesM3_test_h2o, function(x) as.factor(x)))
  # jeunesM3_test_h2o <- as.h2o(jeunesM3_test_h2o)
  # jeunesM3_model_h2o_deep <- h2o.deeplearning(x=1:6, y="TOP_PIC",training_frame=jeunesM3_learn_acc_dataset_h2o)
  # 
  # jeunesM3_model_h2o_gbm <- h2o.gbm(x=1:6, y="TOP_PIC",training_frame=jeunesM3_learn_acc_dataset_h2o)
  
  # jeunesM3_learn_acc_dataset_neural <- data.frame(model.matrix(~ TOP_PIC+csp_emb+habit_loyer+cspcj+anc_dclem+nb_enf,data = jeunesM3_learn_acc_dataset))
  # jeunesM3_learn_acc_dataset_neural$TOP_PIC <- as.factor(jeunesM3_learn_acc_dataset_neural$TOP_PIC)
  # jeunesM3_learn_acc_dataset_neural$X.Intercept. <- NULL
  # n <- names(jeunesM3_learn_acc_dataset_neural)
  # f <- as.formula(paste("TOP_PIC ~", paste(n[!n %in% "medv"], collapse = " + ")))
  # jeunesM3_model_nnet <- nnet(as.factor(TOP_PIC) ~ .,jeunesM3_learn_acc_dataset[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")], size = 10, maxiter = 50)
  # 
  # jeunesM3_model_rpart <- rpart(TOP_PIC ~ .,jeunesM3_learn_acc_dataset[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")],weights=jeunesM3_learn_acc_dataset$poids)
  # jeunesM3_model_rforest <- randomForest(as.factor(TOP_PIC) ~ .,jeunesM3_learn_acc_dataset[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")])
  # 
  # jeunesM3_learn_acc_dataset_svm <- jeunesM3_learn_acc_dataset
  # jeunesM3_learn_acc_dataset_svm$TOP_PIC <- ifelse(jeunesM3_learn_acc_dataset_svm$TOP_PIC==1,1,-1)
  # jeunesM3_model_svm <- svm(as.factor(TOP_PIC) ~ .,jeunesM3_learn_acc_dataset_svm[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")],weights=jeunesM3_learn_acc_dataset$poids, probability = TRUE)
  
  
  jeunesM3_learn_gen <- jeunesM3_learn_dataset[jeunesM3_learn_dataset$SCORE<cut[j],]
  try(jeunesM3_learn_gen$TOP_PIC <- NA, silent=TRUE)
  jeunesM3_learn_gen <- rbind(jeunesM3_learn_gen, jeunesM3_learn_acc_dataset)
  jeunesM3_learn_gen$TOP_PIC <- ifelse(jeunesM3_learn_gen$TOP_PIC == 0, 2, 1)
  
  jeunesM3_model_gen <- mixmodCluster(data=jeunesM3_learn_gen[,2:7], knownLabels = jeunesM3_learn_gen[,8], nbCluster=2)
  
  
  
  
  
  if (length(jeunesM3_learn_dataset[jeunesM3_learn_dataset$SCORE<cut[j],1]) > 0) {
    
    
    # Apprentissage logistique repond?r?e
    
    # jeunesM3_learn_ref_dataset <- jeunesM3_learn_dataset[is.na(jeunesM3_learn_dataset$TOP_PIC),]
    # jeunesM3_learn_ref_dataset <- rbind(jeunesM3_learn_ref_dataset, jeunesM3_learn_dataset[jeunesM3_learn_dataset$SCORE<cut[j],])
    jeunesM3_learn_ref_dataset <- jeunesM3_learn_dataset[jeunesM3_learn_dataset$SCORE<cut[j],]
    
    jeunesM3_learn_ref_dataset1 <- jeunesM3_learn_ref_dataset
    try(jeunesM3_learn_ref_dataset1$poids <- predict(jeunesM3_model, jeunesM3_learn_ref_dataset1, type='response'), silent = TRUE)
    jeunesM3_learn_ref_dataset1$TOP_PIC <- 1
    
    jeunesM3_learn_ref_dataset2 <- jeunesM3_learn_ref_dataset
    jeunesM3_learn_ref_dataset2$poids <- 1-jeunesM3_learn_ref_dataset1$poids
    jeunesM3_learn_ref_dataset2$TOP_PIC <- 0
    
    jeunesM3_learn_pond <- rbind(jeunesM3_learn_acc_dataset,jeunesM3_learn_ref_dataset1,jeunesM3_learn_ref_dataset2)
    jeunesM3_model_pond <- glm(TOP_PIC ~ ., "binomial",jeunesM3_learn_pond[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")], weights = jeunesM3_learn_pond$poids)
    try(jeunesM3_model_pond.pred <- predict(jeunesM3_model_pond, jeunesM3_test[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")]), silent = TRUE)
    # jeunesM3_model_pond.erreur <- sum(abs(jeunesM3_test[,"TOP_PIC"]-jeunesM3_model_pond.pred))/nrow(jeunesM3_test)
    jeunesM3_model_pond.gini <- rcorr.cens(jeunesM3_model_pond.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
    
    
    
    
    # Apprentissage logistique reclass?e
    jeunesM3_learn_ref_reclass <- jeunesM3_learn_ref_dataset
    try(jeunesM3_learn_ref_reclass$TOP_PIC <- as.integer(round(predict(jeunesM3_model, jeunesM3_learn_ref_reclass, type="response"),digits = 0)), silent = TRUE)
    jeunesM3_learn_ref_reclass <- rbind(jeunesM3_learn_ref_reclass,jeunesM3_learn_acc_dataset)
    
    jeunesM3_model_reclass <- glm(TOP_PIC ~ ., "binomial",jeunesM3_learn_acc_dataset[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")], weights = jeunesM3_learn_acc_dataset$poids)
    
    try(jeunesM3_model_reclass.pred <- predict(jeunesM3_model_reclass, jeunesM3_test[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")]), silent = TRUE)
    # jeunesM3_model_reclass.erreur <- sum(abs(jeunesM3_test[,"TOP_PIC"]-jeunesM3_model_reclass.pred))/nrow(jeunesM3_test)
    jeunesM3_model_reclass.gini <- rcorr.cens(jeunesM3_model_reclass.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
    
    
    
    
    
    # Apprentissage augmentation
    jeunesM3_learn_augmentation <- jeunesM3_learn_dataset
    jeunesM3_learn_augmentation$classe_SCORE <- cut2(jeunesM3_learn_augmentation$SCORE, g=10)
    jeunesM3_learn_augmentation$acc <- ifelse(((jeunesM3_learn_augmentation$SCORE>=cut[j])&(!(is.na(jeunesM3_learn_augmentation$TOP_PIC)))),1,0)
    
    poids2 <- sqldf(
      'select distinct count(*) as count, classe_SCORE, acc
      from jeunesM3_learn_augmentation
      group by classe_SCORE, acc
      '
    )
    
    # poids2 <- jeunesM3_learn_augmentation %>% group_by(classe_SCORE, acc) %>% add_tally(name = "count") %>% summarise(count = n())
    
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
    
    jeunesM3_learn_augmentation <- merge(jeunesM3_learn_augmentation, poids_tot, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
    jeunesM3_model_augmentation <- glm(TOP_PIC ~ ., "binomial", jeunesM3_learn_augmentation[((jeunesM3_learn_augmentation$SCORE>=cut[j])&(!(is.na(jeunesM3_learn_augmentation$TOP_PIC)))),c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")], weights = jeunesM3_learn_augmentation[((jeunesM3_learn_augmentation$SCORE>=cut[j])&(!(is.na(jeunesM3_learn_augmentation$TOP_PIC)))),"poidsfinal"])
    try(jeunesM3_model_aumgentation.pred <- predict(jeunesM3_model_augmentation, jeunesM3_test[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")]), silent = TRUE)
    # jeunesM3_model_augmentation.erreur <- sum(abs(data_test_pred[j,,1]-model_pred_augmente.pred))/m_test
    jeunesM3_model_aumgentation.gini <- rcorr.cens(jeunesM3_model_aumgentation.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
    

    
    
    
    # Apprentissage parceling
    jeunesM3_learn_parceling <- jeunesM3_learn_dataset
    jeunesM3_learn_parceling$classe_SCORE <- cut2(jeunesM3_learn_parceling$SCORE, g=10)
    jeunesM3_learn_parceling_part <- jeunesM3_learn_parceling[jeunesM3_learn_dataset$SCORE>=cut[j],]
    jeunesM3_learn_parceling_part <- jeunesM3_learn_parceling_part[!(is.na(jeunesM3_learn_parceling_part$TOP_PIC)),]
    
    poids_part <- sqldf(
      'select count(TOP_PIC) as count, classe_SCORE, TOP_PIC
      from jeunesM3_learn_parceling_part
      group by classe_SCORE, TOP_PIC
      '
    )
    
    # poids_part <- jeunesM3_learn_parceling_part %>% 
    
    poids_bon <- poids_part[as.numeric(poids_part$TOP_PIC)==0,]
    poids_mauvais <- poids_part[as.numeric(poids_part$TOP_PIC)==1,]
    poids_bon$TOP_PIC <- NULL
    poids_mauvais$TOP_PIC <- NULL
    poids <- merge(poids_bon, poids_mauvais, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
    poids$poids_final <- poids$count.y/(poids$count.x+poids$count.y)
    poids$count.x <- NULL
    poids$count.y <- NULL
    
    jeunesM3_learn_parceling <- merge(jeunesM3_learn_parceling, poids, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
    jeunesM3_learn_parceling$poids_final.x <- NULL
    jeunesM3_learn_parceling$poids_final.y <- NULL
    # jeunesM3_learn_parceling$poids_final <- ifelse(is.na(jeunesM3_learn_parceling$poids_final), 1, jeunesM3_learn_parceling$poids_final)
    
    fun_binom <- function(x) {
      return(rbinom(1,1,x))
    }
    jeunesM3_learn_parceling[(is.na(jeunesM3_learn_parceling$TOP_PIC)|(jeunesM3_learn_parceling$SCORE < cut[j])),"ESSAI"] <- sapply(jeunesM3_learn_parceling[(is.na(jeunesM3_learn_parceling$TOP_PIC)|(jeunesM3_learn_parceling$SCORE < cut[j])),"poids_final"],fun_binom)
    jeunesM3_learn_parceling[(is.na(jeunesM3_learn_parceling$TOP_PIC)|(jeunesM3_learn_parceling$SCORE < cut[j])),"TOP_PIC"] <- ifelse(jeunesM3_learn_parceling[(is.na(jeunesM3_learn_parceling$TOP_PIC)|(jeunesM3_learn_parceling$SCORE < cut[j])),"ESSAI"]==0,0,1)
    jeunesM3_learn_parceling$ESSAI <- NULL
    jeunesM3_learn_parceling$poids[jeunesM3_learn_parceling$TOP_PIC==1] <- cost_fp
    jeunesM3_learn_parceling$poids[jeunesM3_learn_parceling$TOP_PIC==0] <- cost_fn
    
    jeunesM3_model_parcelling <- glm(TOP_PIC ~ ., family = binomial(link='logit'), jeunesM3_learn_parceling[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")], weights = jeunesM3_learn_parceling$poids)
    try(jeunesM3_model_parcelling.pred <- predict(jeunesM3_model_parcelling, jeunesM3_test[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")]), silent = TRUE)
    # jeunesM3_model_parcelling.erreur <- sum(abs(jeunesM3_test[,"TOP_PIC"]-jeunesM3_model_parcelling.pred))/nrow(jeunesM3_test)
    jeunesM3_model_parcelling.gini <- rcorr.cens(jeunesM3_model_parcelling.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
    

    
    
    # Apprentissage twins
    jeunesM3_learn_twins <- jeunesM3_learn_dataset
    jeunesM3_learn_twins$acc <- ifelse(((jeunesM3_learn_twins$SCORE>=cut[j])&(!(is.na(jeunesM3_learn_twins$TOP_PIC)))),1,0)
    
    jeunesM3_model_acc <- glm(acc ~ ., family = binomial(link='logit'), jeunesM3_learn_twins[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","acc")],weights = jeunesM3_learn_twins$poids)
    
    try(jeunesM3_learn_twins$score_acc <- predict(jeunesM3_model_acc,jeunesM3_learn_twins), silent = TRUE)
    try(jeunesM3_learn_twins$score_def <- predict(jeunesM3_model,jeunesM3_learn_twins), silent = TRUE)
    
    try(jeunesM3_model_twins <- glm(TOP_PIC ~ score_acc + score_def, family = binomial(link='logit'), jeunesM3_learn_twins[jeunesM3_learn_twins$acc==1,],weights=jeunesM3_learn_twins$poids[jeunesM3_learn_twins$acc==1]), silent = TRUE)
    
    jeunesM3_learn_twins_ref1 <- jeunesM3_learn_twins[jeunesM3_learn_twins$acc==0,]
    jeunesM3_learn_twins_ref1$TOP_PIC <- 1
    try(jeunesM3_learn_twins_ref1$poids <- predict(jeunesM3_model_twins,jeunesM3_learn_twins_ref1, type = 'response'), silent = TRUE)
    
    jeunesM3_learn_twins_ref2 <- jeunesM3_learn_twins[jeunesM3_learn_twins$acc==0,]
    jeunesM3_learn_twins_ref2$TOP_PIC <- 0
    try(jeunesM3_learn_twins_ref2$poids <- predict(jeunesM3_model_twins,jeunesM3_learn_twins_ref1, type = 'response'), silent = TRUE)
    
    jeunesM3_learn_twins_final <- rbind(jeunesM3_learn_twins[jeunesM3_learn_twins$acc==1,],jeunesM3_learn_twins_ref1,jeunesM3_learn_twins_ref2)
    jeunesM3_model_twins_final <- glm(TOP_PIC ~., family = binomial(link='logit'), jeunesM3_learn_twins_final[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")], weights=jeunesM3_learn_twins_final$poids)
    
    try(jeunesM3_model_twins_final.pred <- predict(jeunesM3_model_twins_final, jeunesM3_test[,c("age_csp_r","anc_dclem_habit_r","anempcj_cspcj_r","revtot_r","sitfam_r","nb_imp_max_r","TOP_PIC")]), silent = TRUE)
    jeunesM3_model_twins_final.gini <- rcorr.cens(jeunesM3_model_twins_final.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
    
    
    
  }
  jeunesM3_model.pred <- predict(jeunesM3_model, jeunesM3_test)
  # jeunesM3_model_nnet.pred <- predict(jeunesM3_model_nnet, jeunesM3_test)
  
  jeunesM3_model.gini <- rcorr.cens(jeunesM3_model.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
  # jeunesM3_model_nnet.gini <- rcorr.cens(jeunesM3_model_nnet.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
  
  # jeunesM3_model_rpart.pred <- predict(jeunesM3_model_rpart, jeunesM3_test)
  # jeunesM3_model_rpart.gini <-  rcorr.cens(jeunesM3_model_rpart.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
  # 
  # jeunesM3_model_rforest.pred <- predict(jeunesM3_model_rforest, jeunesM3_test, type="prob")[,2]
  # jeunesM3_model_rforest.gini <- rcorr.cens(jeunesM3_model_rforest.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
  # 
  # jeunesM3_model_deep.pred <- as.data.frame(h2o.predict(jeunesM3_model_h2o_deep, jeunesM3_test_h2o))
  # jeunesM3_model_deep.gini <- rcorr.cens(jeunesM3_model_deep.pred[,3],jeunesM3_test[,"TOP_PIC"])[[2]]
  # 
  # jeunesM3_model_gbm.pred <- as.data.frame(h2o.predict(jeunesM3_model_h2o_gbm, jeunesM3_test_h2o))
  # jeunesM3_model_gbm.gini <- rcorr.cens(jeunesM3_model_gbm.pred[,3],jeunesM3_test[,"TOP_PIC"])[[2]]
  # 
  # jeunesM3_model_svm.pred <- predict(jeunesM3_model_svm, jeunesM3_test, probability = TRUE)
  # jeunesM3_model_svm.gini <- rcorr.cens(attr(jeunesM3_model_svm.pred,"probabilities")[,2],jeunesM3_test[,"TOP_PIC"])[[2]]
  # 
  jeunesM3_model_gen.pred <- mixmodPredict(jeunesM3_test[,2:7], jeunesM3_model_gen@bestResult)@proba[,1]
  jeunesM3_model_gen.gini <- rcorr.cens(jeunesM3_model_gen.pred,jeunesM3_test[,"TOP_PIC"])[[2]]
  
  # if (length(jeunesM3_learn_dataset[jeunesM3_learn_dataset$SCORE<cut[j],1]) > 0) {
  #   taux[,j] <- c(jeunesM3_model.gini,jeunesM3_model_nnet.gini,jeunesM3_model_rpart.gini,jeunesM3_model_rforest.gini, jeunesM3_model_deep.gini, jeunesM3_model_gbm.gini, jeunesM3_model_svm.gini, jeunesM3_model_gen.gini, jeunesM3_model_aumgentation.gini,jeunesM3_model_pond.gini,jeunesM3_model_reclass.gini,jeunesM3_model_parcelling.gini,jeunesM3_model_twins_final.gini)
  # }else {
  #   taux[,j] <- c(jeunesM3_model.gini,jeunesM3_model_nnet.gini,jeunesM3_model_rpart.gini,jeunesM3_model_rforest.gini, jeunesM3_model_deep.gini, jeunesM3_model_gbm.gini, jeunesM3_model_svm.gini, jeunesM3_model_gen.gini, jeunesM3_model.gini,jeunesM3_model.gini,jeunesM3_model.gini,jeunesM3_model.gini,jeunesM3_model.gini)
  # }
  if (length(jeunesM3_learn_dataset[jeunesM3_learn_dataset$SCORE<cut[j],1]) > 0) {
    taux[b,,j] <- c(jeunesM3_model.gini, jeunesM3_model_gen.gini, jeunesM3_model_aumgentation.gini,jeunesM3_model_pond.gini,jeunesM3_model_reclass.gini,jeunesM3_model_parcelling.gini,jeunesM3_model_twins_final.gini)
  }else {
    taux[b,,j] <- c(jeunesM3_model.gini, jeunesM3_model_gen.gini, jeunesM3_model.gini,jeunesM3_model.gini,jeunesM3_model.gini,jeunesM3_model.gini,jeunesM3_model.gini)
  }
}
}

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tikzDevice)

save(taux, file = "taux_prospects.RData")

load("taux_prospects.RData")

acceptance <- matrix(0,length(cut))

for (i in 1:length(cut)) {
  acceptance[i] <- length(jeunesM3[((jeunesM3$SCORE>=cut[i])&(!is.na(jeunesM3$TOP_PIC))),1]) / length(jeunesM3[,1])
}


myMoyenne <- function(x){
  y <- array(NA,c(9,7))
  for (k in 1:9) {
    for (j in 1:7) {
      y[k,j] <- mean(x[1:5,j,k])
    }
  }
  y
}

myVariance <- function(x){
  y <- array(NA,c(9,7))
  for (k in 1:9) {
    for (j in 1:7) {
      y[k,j] <- var(x[1:5,j,k])
    }
  }
  y
}

moyenne <- myMoyenne(taux)
variance <- myVariance(taux)



tikz(file="standard.tex", width=8, height=3.2, engine="pdftex")
plot(x = acceptance[1:9], y = moyenne[1:9,1], xlim=c(1,.4), ylim=c(0.29,0.36), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=15, xaxt = "n", yaxt="n", type='o', lty = 1)
# par(new=TRUE)
# plot(x = acceptance[1:9], y = moyenne[1:9,2], xlim=c(1,.4), ylim=c(0.29,0.36), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=16, xaxt = "n", col=34, yaxt="n", type='o', lty = 2)
par(new=TRUE)
plot(x = acceptance[1:9], y = moyenne[1:9,3], xlim=c(1,.4), ylim=c(0.29,0.36), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=17, col=300, xaxt = "n", yaxt="n", type='o', lty = 3)
# par(new=TRUE)
# plot(x = acceptance[1:9], y = moyenne[1:9,4], xlim=c(1,.4), ylim=c(0.29,0.36), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=18, col=97, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:9], y = moyenne[1:9,5], xlim=c(1,.4), ylim=c(0.29,0.36), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = moyenne[1:9,6], xlim=c(1,.4), ylim=c(0.29,0.36), ylab = 'Gini on test set', xlab = 'Acceptance rate', pch=8, col=590, xaxt = "n", yaxt="n", type='o', lty = 4)
# par(new=TRUE)
# plot(x = acceptance[1:9], y = moyenne[1:9,7], xlim=c(1,.4), ylim=c(0.29,0.36), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, col='green', xaxt = "n", yaxt="n", type='o')

axis(1, at=pretty(acceptance[1:9]), lab=paste0(pretty(acceptance[1:9]) * 100, " %"), las=TRUE)
axis(2, at=pretty(quantile(moyenne, probs = seq(0,1,0.1))), lab=pretty(quantile(moyenne, probs = seq(0,1,0.1)) * 100), las=TRUE)

legend(1,0.33,
       pch = c(15,17,8),
       lty = c(1,3,4),
       col=c(1,300,590),legend=c("Financed","Augmentation","Parcelling"),cex=0.75)
dev.off()





















plot(x = acceptance[1:9], y = taux[1,1:9], xlim=c(0.7,0.1), ylim=c(0.15,0.4), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=15, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[2,1:9], xlim=c(0.7,0.1), ylim=c(0.15,0.4), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=16, xaxt = "n", col=34, yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[3,1:9], xlim=c(0.7,0.1), ylim=c(0.15,0.4), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=17, col=49, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[4,1:9], xlim=c(0.7,0.1), ylim=c(0.15,0.4), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=18, col=97, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[5,1:9], xlim=c(0.7,0.1), ylim=c(0.15,0.4), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[6,1:9], xlim=c(0.7,0.1), ylim=c(0.15,0.4), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=8, col=590, xaxt = "n", yaxt="n", type='o')
# par(new=TRUE)
# plot(x = acceptance[1:9], y = taux[7,1:9], xlim=c(1,0), ylim=c(-0.15,0.6), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, col=376, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[8,1:9], xlim=c(0.7,0.1), ylim=c(0.15,0.4), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, col=376, xaxt = "n", yaxt="n", type='o')

axis(1, at=pretty(acceptance[1:9]), lab=paste0(pretty(acceptance[1:9]) * 100, " %"), las=TRUE)
axis(2, at=pretty(taux[1,1:9], n=20), lab=pretty(taux[1,1:9] * 100, n=20), las=TRUE)

legend(0.71,0.26, 
       pch = c(15,16,17,18,7,8,9), 
       col=c(1,34,49,97,653,590,376),legend= c("Logistic regression","Neural network","Decision tree","Random Forest","Deep Learning H2O","Gradient Boosting", "Multinomial model"),cex=1)


plot(x = acceptance[1:9], y = taux[1,1:9], xlim=c(0.7,0.1), ylim=c(0.33,0.39), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=15, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[8,1:9], xlim=c(0.7,0.1), ylim=c(0.33,0.39), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=16, xaxt = "n", col=34, yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[9,1:9], xlim=c(0.7,0.1), ylim=c(0.33,0.39), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=17, col='green', xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[10,1:9], xlim=c(0.7,0.1), ylim=c(0.33,0.39), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=18, col='orange', xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[11,1:9], xlim=c(0.7,0.1), ylim=c(0.33,0.39), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=7, col=653, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[12,1:9], xlim=c(0.7,0.1), ylim=c(0.33,0.39), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=8, col=590, xaxt = "n", yaxt="n", type='o')
par(new=TRUE)
plot(x = acceptance[1:9], y = taux[13,1:9], xlim=c(0.7,0.1), ylim=c(0.33,0.39), ylab = 'Gini coefficient on test set', xlab = 'Acceptance rate', main='Gini coefficient w.r.t. the acceptance rate of the previous scoring model', pch=9, col=376, xaxt = "n", yaxt="n", type='o')

axis(1, at=pretty(acceptance[1:9]), lab=paste0(pretty(acceptance[1:9]) * 100, " %"), las=TRUE)
axis(2, at=pretty(taux[1,1:9], n=50), lab=pretty(taux[1,1:9] * 100, n=50), las=TRUE)

legend(0.71,0.368, 
       pch = c(15,16,17,18,7,8,9), 
       col=c(1,34,'green', 'orange',653,590,376),legend= c("Logistic regression","Multinomial model","Augmentation","Reweighting","Reclassification","Parceling","Twins"),cex=1.2)
