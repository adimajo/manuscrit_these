
library(FactoMineR)
library(tidyverse)
library(tikzDevice)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Chargement données
AUTOS = read_delim('~/Google Drive/Discrétisation ICLR19/data_iclr19/AUTOS.csv', 
                   delim = ';', 
                   na = c('.','','NA'),
                   col_types = cols(CDENERG = col_factor(), 
                                    CVFISC = col_integer(),
                                    HABIT = col_factor(),
                                    LIBMARQ = col_factor(),
                                    POIDSVH = col_integer(),
                                    SITFAM = col_factor(),
                                    CSP = col_factor(),
                                    CSPCJ = col_factor(),
                                    E_CLI_ACTIF = col_factor()))


## PCA
res_pca_autos = PCA(X = AUTOS %>% select(-c(CDENERG, CSP, CSPCJ, E_CLI_ACTIF, SITFAM, HABIT, LIBMARQ, SCORE, top_perf)) %>% drop_na())

tikz("pca_points.tex", standAlone=FALSE, width = 7, height = 7, lwdUnit = 1.3, fg = "black", sanitize=TRUE, verbose=TRUE)
plot(res_pca_autos, choix = 'ind', label="none")
dev.off()

tikz("pca_axes_definition.tex", standAlone=FALSE, width = 7, height = 7, lwdUnit = 1.3, fg = "black", sanitize=TRUE, verbose=TRUE)
plot(res_pca_autos, choix = 'var', autoLab = 'yes')
dev.off()

## MCA
res_mca_autos = MCA(X = AUTOS %>% select(c(CDENERG, CSP, CSPCJ, E_CLI_ACTIF, SITFAM, HABIT, LIBMARQ)) %>% drop_na())

tikz("mca_levels.tex", standAlone=FALSE, width = 7, height = 7, lwdUnit = 1.3, fg = "black", sanitize=TRUE, verbose=TRUE)
plot(res_mca_autos, invisible = 'ind', choix = 'ind', autoLab = 'yes')
dev.off()

tikz("mca_features.tex", standAlone=FALSE, width = 7, height = 7, lwdUnit = 1.3, fg = "black", sanitize=TRUE, verbose=TRUE)
plot(res_mca_autos, choix = 'var', autoLab = 'yes')
dev.off()

## FAMD
res_famd_autos = FAMD(base = AUTOS %>% select(-c(SCORE, top_perf)) %>% drop_na())

tikz("famd_levels.tex", standAlone=FALSE, width = 7, height = 7, lwdUnit = 1.3, fg = "black", sanitize=TRUE, verbose=TRUE)
plot(res_famd_autos, invisible = 'ind', choix = 'ind', autoLab = 'yes')
dev.off()

tikz("famd_features.tex", standAlone=FALSE, width = 7, height = 7, lwdUnit = 1.3, fg = "black", sanitize=TRUE, verbose=TRUE)
plot(res_famd_autos, choix = 'var', autoLab = 'yes')
dev.off()

## PLS

library(pls)
AUTOS$top_perf <- as.numeric(as.character(AUTOS$top_perf))
pls_auto = plsr(top_perf ~ ., data = AUTOS %>% select(-c(SCORE, CSPCJ, anc_AMEMBC, anc_DNACJ, anc_DT1FIN)) %>% impute_at(.na = na.mean, .vars = c("CVFISC", "CYLVH", "POIDSVH", "CVFISC")))
ind_graph = sample.int(nrow(AUTOS), size = 1000)
prediction_pls = predict(pls_auto,AUTOS[ind_graph,] %>% select(-c(SCORE, CSPCJ, anc_AMEMBC, anc_DNACJ, anc_DT1FIN)) %>% impute_at(.na = na.mean, .vars = c("CVFISC", "CYLVH", "POIDSVH", "CVFISC")), comps = 1:2, type = "scores")

tikz("graph_pls.tex", standAlone=FALSE, width = 12, height = 6, fg = "black", sanitize = FALSE)
plot(prediction_pls, col = (as.data.frame(AUTOS[ind_graph,"top_perf"]+1))$top_perf)
dev.off()

## LMT

library(RWeka)

AUTOS$top_perf = factor(AUTOS$top_perf)

### en laissant les valeurs manquantes

lmt_autos = LMT(top_perf ~ ., data = AUTOS %>% select(-SCORE))
plot(lmt_autos)
print(lmt_autos)

glmdisc::normalizedGini(as.numeric(as.character(AUTOS$top_perf)), predict(lmt_autos, AUTOS, type = "probability")[,2])

### en supprimant les variables incriminées

lmt_autos = LMT(top_perf ~ ., data = AUTOS %>% select(-c(SCORE, CSPCJ, anc_AMEMBC, anc_DNACJ, anc_DT1FIN)))
plot(lmt_autos)
print(lmt_autos)

glmdisc::normalizedGini(as.numeric(as.character(AUTOS$top_perf)), predict(lmt_autos, AUTOS, type = "probability")[,2])

### en imputant par la moyenne
library(tidyimpute)
library(na.tools)

levels(AUTOS$LIBMARQ) <- c(levels(AUTOS$LIBMARQ), "manquant")
levels(AUTOS$E_CLI_ACTIF) <- c(levels(AUTOS$E_CLI_ACTIF), "manquant")

AUTOS$LIBMARQ[is.na(AUTOS$LIBMARQ)] = "manquant"
AUTOS$E_CLI_ACTIF[is.na(AUTOS$E_CLI_ACTIF)] = "manquant"

lmt_autos = LMT(top_perf ~ ., data = AUTOS %>% select(-c(SCORE, CSPCJ, anc_AMEMBC, anc_DNACJ, anc_DT1FIN)) %>% impute_at(.na = na.mean, .vars = c("CVFISC", "CYLVH", "POIDSVH", "CVFISC")))
plot(lmt_autos)
print(lmt_autos)

glmdisc::normalizedGini(as.numeric(as.character(AUTOS$top_perf)), predict(lmt_autos, AUTOS, type = "probability")[,2])


## MOB
library(party)
library(partykit)

### RECHARGER LES DONNEES !!!

### en laissant les valeurs manquantes

mob_data_autos = as.data.frame(AUTOS[AUTOS %>% select(-SCORE) %>% complete.cases(),] %>% select(c(top_perf, CSP, anc_DNAISS, MLOYER)))

mob_data = glmtree(top_perf ~ . | ., data = mob_data_autos, family = binomial(link="logit"))
plot(mob_data)
print(mob_data)

glmdisc::normalizedGini(as.numeric(as.character(AUTOS$top_perf)), predict(mob_data, AUTOS, type = "probability")[,2])

### en supprimant les variables incriminées

mob_data = glmtree(top_perf ~ . | ., data = AUTOS %>% select(-c(SCORE, CSPCJ, anc_AMEMBC, anc_DNACJ, anc_DT1FIN)), family = binomial(link="logit"))
plot(mob_data)
print(mob_data)

glmdisc::normalizedGini(as.numeric(as.character(AUTOS$top_perf)), predict(mob_data, AUTOS, type = "probability")[,2])

### en imputant par la moyenne

library(tidyimpute)
library(na.tools)

levels(AUTOS$LIBMARQ) <- c(levels(AUTOS$LIBMARQ), "manquant")
levels(AUTOS$E_CLI_ACTIF) <- c(levels(AUTOS$E_CLI_ACTIF), "manquant")

AUTOS$LIBMARQ[is.na(AUTOS$LIBMARQ)] = "manquant"
AUTOS$E_CLI_ACTIF[is.na(AUTOS$E_CLI_ACTIF)] = "manquant"

mob_data = glmtree(top_perf ~ . | ., data = AUTOS %>% select(-c(SCORE, CSPCJ, anc_AMEMBC, anc_DNACJ, anc_DT1FIN)) %>% impute_at(.na = na.mean, .vars = c("CVFISC", "CYLVH", "POIDSVH", "CVFISC")), family = binomial(link="logit"))
plot(mob_data)
print(mob_data)

glmdisc::normalizedGini(as.numeric(as.character(AUTOS$top_perf)), predict(mob_data, AUTOS, type = "probability")[,2])




# Visualisation coefficients CSP

standard_glm = glm(top_perf ~ . , data = AUTOS %>% select(-c(SCORE, CSPCJ, anc_AMEMBC, anc_DNACJ, anc_DT1FIN)) %>% impute_at(.na = na.mean, .vars = c("CVFISC", "CYLVH", "POIDSVH", "CVFISC")), family = binomial(link="logit"))

confidence_intervals = confint(standard_glm, parms = names(standard_glm$coefficients[substr(names(standard_glm$coefficients),1,3) == 'CSP']))

df = data.frame(confidence_intervals[names(standard_glm$coefficients[substr(names(standard_glm$coefficients),1,3)=="CSP"]),])
df$mean = rowMeans(df)

library(ggplot2)

tikz("csp_estim.tex", standAlone=FALSE, width = 7, height = 7, lwdUnit = 1.3, fg = "black", sanitize=TRUE, verbose=TRUE)
ggplot(df, aes(x= rownames(df), y=mean)) + 
  geom_errorbar(aes(ymin=X2.5.., ymax=X97.5..), width=.1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(name ="CSP levels") +
  scale_y_continuous(name ="Associated LR coefficient") +
  geom_hline(yintercept = 0, color = 'red')
dev.off()



# avec glmdisc

df_autos = AUTOS %>% select(-c(SCORE, CSPCJ, anc_AMEMBC, anc_DNACJ, anc_DT1FIN)) %>% impute_at(.na = na.mean, .vars = c("CVFISC", "CYLVH", "POIDSVH", "CVFISC"))

labels = df_autos$top_perf
df_autos$top_perf = NULL

library(glmdisc)
df_autos = data.frame(df_autos)
df_autos$CVFISC = as.numeric(df_autos$CVFISC)
df_autos$POIDSVH = as.numeric(df_autos$POIDSVH)

disc_glm = glmdisc(df_autos, labels, interact = FALSE, validation = FALSE, criterion = 'bic', m_start = 4, iter = 100)
df_autos_disc = disc_glm@disc.data

disc_glm_autos = glm(labels~., data = Filter(function(x) (length(unique(x)) > 1), df_autos_disc), family = binomial(link='logit'))

confidence_intervals = confint(disc_glm_autos)

df = data.frame(confidence_intervals[names(disc_glm_autos$coefficients[substr(names(disc_glm_autos$coefficients),1,3)=="CSP"]),])
df$mean = rowMeans(df)


tikz("csp_estim_disc.tex", standAlone=FALSE, width = 7, height = 7, lwdUnit = 1.3, fg = "black", sanitize=TRUE, verbose=TRUE)
ggplot(df, aes(x= rownames(df), y=mean)) + 
  geom_errorbar(aes(ymin=X2.5.., ymax=X97.5..), width=.1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(name ="CSP levels") +
  #  ylim(-5,5) +
  scale_y_continuous(name ="Associated LR coefficient") +
  geom_hline(yintercept = 0, color = 'red')
dev.off()







####### glmdisc tree

AUTOS$E_CLI_ACTIF = factor(AUTOS$E_CLI_ACTIF)
levels(AUTOS$E_CLI_ACTIF) = c(levels(AUTOS$E_CLI_ACTIF), "manquant")
AUTOS$E_CLI_ACTIF[is.na(AUTOS$E_CLI_ACTIF)] = "manquant"

AUTOS$CSPCJ = factor(AUTOS$CSPCJ)
levels(AUTOS$CSPCJ) = c(levels(AUTOS$CSPCJ), "manquant")
AUTOS$CSPCJ[is.na(AUTOS$CSPCJ)] = "manquant"

AUTOS$CDENERG = factor(AUTOS$CDENERG)
levels(AUTOS$CDENERG) = c(levels(AUTOS$CDENERG), "manquant")
AUTOS$CDENERG[is.na(AUTOS$CDENERG)] = "manquant"

AUTOS$anc_AMEMBC[!is.na(AUTOS$anc_AMEMBC)] = floor(AUTOS$anc_AMEMBC[!is.na(AUTOS$anc_AMEMBC)] / (12))
AUTOS$anc_AMEMBC = factor(AUTOS$anc_AMEMBC)
levels(AUTOS$anc_AMEMBC) = c(levels(AUTOS$anc_AMEMBC), "manquant")
AUTOS$anc_AMEMBC[is.na(AUTOS$anc_AMEMBC)] = "manquant"

AUTOS$anc_DNACJ = NULL
AUTOS$anc_DT1FIN = NULL
AUTOS$POIDSVH = NULL
AUTOS$CYLVH = NULL

AUTOS$LIBMARQ = factor(AUTOS$LIBMARQ)
levels(AUTOS$LIBMARQ) = c(levels(AUTOS$LIBMARQ), "manquant")
AUTOS$LIBMARQ[is.na(AUTOS$LIBMARQ)] = "manquant"

AUTOS = AUTOS %>% drop_na()


ind_train = rbinom(nrow(AUTOS),1, c(0.3,0.7))
ind_test = 1-ind_train

data = AUTOS[as.logical(ind_train),]

n = nrow(data)
K_depart = 6
max_iter = 100
data$c_map <- data$c_hat <- as.factor(sample(K_depart, size=n, replace = TRUE))

var_explicatives = c("APPORT","CDENERG","COTARGUS","CREDAC","CSP","CSPCJ","CVFISC","DUR_CRED","ENDEXT","E_CLI_ACTIF","HABIT","LIBMARQ","MCDE","MLOYER","NBENF","SITFAM","anc_AMCIRC","anc_AMEMBC","anc_DCLEM","anc_DNAISS","revtot") 
current_best = 1
library(rpart)
criterion_iter = list()

for (i in 1:max_iter) {
  print(paste('iter',i))
  
  # Initialisations
  reglogs_c_hat = list()
  reglogs_c_map = list()
  predictions_log <- matrix(0,nrow=n,ncol=nlevels(data$c_hat))
  
  # Apprentissage des régressions p(y | x, c_hat) et remplissage des probabilités prédites
  for (c_iter in 1:nlevels(data$c_hat)) {
    print(paste('c_iter',c_iter))
    reglogs_c_hat[[c_iter]] <- glm(top_perf ~ ., data = data[data$c_hat==levels(data$c_hat)[c_iter],c(var_explicatives,"top_perf")][, sapply(data[data$c_hat==levels(data$c_hat)[c_iter],c(var_explicatives,"top_perf")], function(col) ifelse(is.factor(col), nlevels(factor(col)), 2)) > 1], family=binomial(link="logit"))
    predictions_log[data$c_hat==levels(data$c_hat)[c_iter],c_iter] <- predict(reglogs_c_hat[[c_iter]],data[data$c_hat==levels(data$c_hat)[c_iter],c(var_explicatives,"top_perf")],type="response")
  }
  
  # Apprentissage des régressions p(y | x, c_map) et calcul de l'AIC "cumulé"
  criterion_iter[[i]] = 0
  
  for (c_iter in 1:nlevels(data$c_map)) {
    print(paste('c_iter2',c_iter))
    reglogs_c_map[[c_iter]] <- glm(top_perf ~ ., data = data[data$c_map==levels(data$c_map)[c_iter],c(var_explicatives,"top_perf")][, sapply(data[data$c_map==levels(data$c_map)[c_iter],c(var_explicatives,"top_perf")], function(col) ifelse(is.factor(col), nlevels(factor(col)), 2)) > 1], family=binomial(link="logit"))
    criterion_iter[[i]] = criterion_iter[[i]] - reglogs_c_map[[c_iter]]$aic
  }
  
  # Mise à jour éventuelle de la meilleure solution actuelle
  if (i>=5 & criterion_iter[[i]] >= criterion_iter[[current_best]]) {
    best_reglogs = reglogs_c_map
    best_link = tryCatch(link,error=function(cond) list())
    current_best = i
  }
  
  # Apprentissage du lien p(c_hat | x)
  print('link')
  link = rpart(c_hat ~ ., data = data[,c("c_hat",var_explicatives)], method = "class", control = rpart.control(cp=0.0001), weights = ifelse(data$top_perf == 0 , 10, 1))
  link = prune(link,cp= link$cptable[which(link$cptable[,"nsplit"] > 0)[1],"CP"])
  # link = ctree(c_hat ~ ., data = data[,c("c_hat",var_explicatives,"top_perf")])
  
  # Calcul de c_map
  data$c_map <- factor(apply(predict(link,data),1,function(p) names(which.max(p))))
  # data$c_map <- factor(apply(predict(link,data, type = "prob"),1,function(p) names(which.max(p))))
  
  # Tirage du nouveau c_hat
  p <- prop.table(predict(link,data)*(data$top_perf*predictions_log + (1-data$top_perf)*(1-predictions_log)),1)
  # p <- prop.table(predict(link,data,type="prob")*(data$top_perf*predictions_log + (1-data$top_perf)*(1-predictions_log)),1)
  data$c_hat <- factor(apply(p,1,function(row) sample(levels(data$c_hat),1,prob = row)))
  
}
