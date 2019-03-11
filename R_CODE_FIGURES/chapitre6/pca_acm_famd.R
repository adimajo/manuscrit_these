
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

mob_data = glmtree(top_perf ~ . | ., data = AUTOS %>% select(-SCORE), family = binomial(link="logit"))
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



