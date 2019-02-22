
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
Sys.sleep(10)

tikz("pca_points.tex", standAlone=FALSE, width = 4, height = 3, fg = "black")
plot(res_pca_autos, choix = 'ind', label="none")
Sys.sleep(10)
dev.off()

tikz("pca_axes_definition.tex", standAlone=FALSE, width = 4, height = 3, fg = "black")
plot(res_pca_autos, choix = 'var', autoLab = 'yes')
Sys.sleep(10)
dev.off()

## MCA
res_mca_autos = MCA(X = AUTOS %>% select(c(CDENERG, CSP, CSPCJ, E_CLI_ACTIF, SITFAM, HABIT, LIBMARQ)) %>% drop_na())
Sys.sleep(10)

tikz("mca_levels.tex", standAlone=FALSE, width = 4, height = 3, fg = "black")
plot(res_mca_autos, invisible = 'ind', choix = 'ind', autoLab = 'yes')
Sys.sleep(10)
dev.off()

tikz("mca_features.tex", standAlone=FALSE, width = 4, height = 3, fg = "black")
plot(res_mca_autos, choix = 'var', autoLab = 'yes')
Sys.sleep(10)
dev.off()

## FAMD
res_famd_autos = FAMD(base = AUTOS %>% select(-c(SCORE, top_perf)) %>% drop_na())
Sys.sleep(10)

tikz("famd_levels.tex", standAlone=FALSE, width = 4, height = 3, fg = "black")
plot(res_famd_autos, invisible = 'ind', choix = 'ind', autoLab = 'yes')
Sys.sleep(10)
dev.off()

tikz("famd_features.tex", standAlone=FALSE, width = 4, height = 3, fg = "black")
plot(res_famd_autos, choix = 'var', autoLab = 'yes')
Sys.sleep(10)
dev.off()