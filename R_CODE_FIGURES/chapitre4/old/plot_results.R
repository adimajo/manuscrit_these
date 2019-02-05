


load("gini_test_equalfreq_mis.RData")

load("gini_test_mis.RData")

load("gini_test_inter.RData")

rowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}


moyenne_continu <- mean(gini_test)
std_continu <- sqrt(var(gini_test))


moyenne_discret <- rowMeans(gini_test_equalfreq)
std_discret <- rowVar(gini_test_equalfreq)


moyenne_inter <- mean(gini_test_inter)
std_inter <- sqrt(gini_test_inter)


setwd("C:/Users/s36612/Documents/13206528qssppdptvshn/R_CODE_FIGURES/chapitre4")


tikz(file = 'gini_well.tex', width = 8, height = 3.2, engine="pdftex")
plot(x = seq(1,98,1), y = rep(moyenne_continu, 98), xlim=c(0,100), ylim=c(0.87,0.94), ylab = 'Gini', xlab = 'Nombre de bins', xaxt = "n", yaxt="n", pch=8, type='o', col='red')
par(new=TRUE)
plot(x = seq(1,98,1), y = moyenne_discret, xlim=c(0,100), ylim=c(0.87,0.94), ylab = 'Gini', xlab = 'Nombre de bins', xaxt = "n", yaxt="n", type='o', pch=9, col='green')
par(new=TRUE)
plot(x = seq(1,98,1), y = rep(moyenne_inter, 98), xlim=c(0,100), ylim=c(0.87,0.94), ylab = 'Gini', xlab = 'Nombre de bins', xaxt = "n", yaxt="n", type='o', pch=1, col='blue')

axis(1, at=pretty(seq(1,98,1)), lab=pretty(seq(1,98,1)), las=TRUE)
axis(2, at=pretty(seq(0.87,0.94,0.01)), lab=pretty(seq(0.87,0.94,0.01)), las=TRUE)

legend(4,0.89,
       pch = c(1,9,8),
       col=c("blue","green","red"),legend= c("Oracle","Quantized data LR","Linear LR"),cex=0.8)
dev.off()


