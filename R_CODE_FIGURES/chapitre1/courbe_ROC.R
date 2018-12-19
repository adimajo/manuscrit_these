
install.packages('pROC')
library(pROC)
library(tikzDevice)

set.seed(1)
x = matrix(runif(300), nrow = 100, ncol = 3)
cuts = seq(0,1,length.out= 4)
xd = apply(x,2, function(col) as.numeric(cut(col,cuts)))
theta = t(matrix(c(0,0,0,2,2,2,-2,-2,-2),ncol=3,nrow=3))
log_odd = rowSums(t(sapply(seq_along(xd[,1]), function(row_id) sapply(seq_along(xd[row_id,]),
                                                                      function(element) theta[xd[row_id,element],element]))))
y = rbinom(100,1,1/(1+exp(-log_odd)))

glm_mod = glm(y~x,family=binomial(link="logit"))


tikz(file="Documents/LaTeX/13206528qssppdptvshn/figures/chapitre1/courbe_roc.tex", width = 4, height = 4)
plot(roc(y,glm_mod$fitted.values), print.auc = TRUE)
dev.off()
