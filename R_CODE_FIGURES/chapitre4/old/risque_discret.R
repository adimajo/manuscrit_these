
library(sqldf)



##### Risque discret modèle continu #####


revenus <- seq(200,5000)

cutpoints <- c(-Inf,400,1000,1500,2000,3000,4500,Inf)

risque_discret <- data.frame(revenus_cont = as.numeric(revenus))

risque_discret$revenus_disc <- cut(risque_discret$revenus_cont,cutpoints, include.lowest = FALSE, labels = seq(1:(length(cutpoints)-1)))

risque_discret$TOP_PIC[risque_discret$revenus_disc=='1'] <- rbinom(length(risque_discret[risque_discret$revenus_disc=='1',1]),1,prob=0.2)
risque_discret$TOP_PIC[risque_discret$revenus_disc=='2'] <- rbinom(length(risque_discret[risque_discret$revenus_disc=='2',1]),1,prob=0.1)
risque_discret$TOP_PIC[risque_discret$revenus_disc=='3'] <- rbinom(length(risque_discret[risque_discret$revenus_disc=='3',1]),1,prob=0.07)
risque_discret$TOP_PIC[risque_discret$revenus_disc=='4'] <- rbinom(length(risque_discret[risque_discret$revenus_disc=='4',1]),1,prob=0.05)
risque_discret$TOP_PIC[risque_discret$revenus_disc=='5'] <- rbinom(length(risque_discret[risque_discret$revenus_disc=='5',1]),1,prob=0.03)
risque_discret$TOP_PIC[risque_discret$revenus_disc=='6'] <- rbinom(length(risque_discret[risque_discret$revenus_disc=='6',1]),1,prob=0.02)
risque_discret$TOP_PIC[risque_discret$revenus_disc=='7'] <- rbinom(length(risque_discret[risque_discret$revenus_disc=='7',1]),1,prob=0.005)
  

proportion <- sqldf(
  'select distinct count(*) as count, revenus_disc, TOP_PIC
  from risque_discret
  group by revenus_disc, TOP_PIC
  '
)


proportion_bons <- proportion[proportion$TOP_PIC==0,]
proportion_mauvais <- proportion[proportion$TOP_PIC==1,]

proportion_bons$count_bons <- proportion_bons$count
proportion_bons$count <- NULL

proportion_mauvais$count_mauvais <- proportion_mauvais$count
proportion_mauvais$count <- NULL

proportion_tot <- merge(proportion_bons, proportion_mauvais, by="revenus_disc")
proportion_tot$poidsfinal <- proportion_tot$count_mauvais/(proportion_tot$count_mauvais + proportion_tot$count_bons)
proportion_tot$count_bons <- NULL
proportion_tot$count_mauvais <- NULL
proportion_tot$TOP_PIC.x <- NULL
proportion_tot$TOP_PIC.y <- NULL


risque_discret <- merge(risque_discret,proportion_tot,by='revenus_disc')


modele_cont <- glm(TOP_PIC ~ revenus_cont, family=binomial(link='logit'), risque_discret)
modele_disc <- glm(TOP_PIC ~ revenus_disc, family=binomial(link='logit'), risque_discret)

risque_discret$modele_disc <- modele_disc$fitted.values
risque_discret$modele_cont <- modele_cont$fitted.values

plot(risque_discret$revenus_cont,risque_discret$poidsfinal, xlim=c(-200,5200), ylim=c(0,0.25), ylab = 'Probabilité de défaut', xlab = 'Revenus', xaxt = "n", yaxt="n", pch=8, type='o', col='green')
par(new=TRUE)
plot(risque_discret$revenus_cont,risque_discret$modele_disc, xlim=c(-200,5200), ylim=c(0,0.25), ylab = 'Probabilité de défaut', xlab = 'Revenus', xaxt = "n", yaxt="n", pch=9, type='o', col='red')
par(new=TRUE)
plot(risque_discret$revenus_cont,risque_discret$modele_cont, xlim=c(-200,5200), ylim=c(0,0.25), ylab = 'Probabilité de défaut', xlab = 'Revenus', xaxt = "n", yaxt="n", pch=7, type='o', col='blue')


axis(1, at=pretty(risque_discret$revenus_cont), lab=pretty(risque_discret$revenus_cont), las=TRUE)
axis(2, at=pretty(risque_discret$modele_cont), lab=pretty(risque_discret$modele_cont), las=TRUE)






##### Risque continu modèle discret #####

proba <- function(x) {
  a <- -0.295/4800
  b <- 0.5*(0.305-5200*a)
  return(a*x+b)
}

risque_discret$modele_cont <- NULL
risque_discret$modele_disc <- NULL
risque_discret$poidsfinal <- NULL


risque_discret[,"TOP_PIC"] <- rbinom(n=4801,1,prob=proba(risque_discret$revenus_cont))


modele_cont <- glm(TOP_PIC ~ revenus_cont, family=binomial(link='logit'), risque_discret)
modele_disc <- glm(TOP_PIC ~ revenus_disc, family=binomial(link='logit'), risque_discret)


risque_discret$modele_disc <- modele_disc$fitted.values
risque_discret$modele_cont <- modele_cont$fitted.values



plot(risque_discret$revenus_cont,risque_discret$poidsfinal, xlim=c(-200,5200), ylim=c(0,0.4), ylab = 'Probabilité de défaut', xlab = 'Revenus', xaxt = "n", yaxt="n", pch=8, type='o', col='green')
par(new=TRUE)
plot(risque_discret$revenus_cont,risque_discret$modele_disc, xlim=c(-200,5200), ylim=c(0,0.4), ylab = 'Probabilité de défaut', xlab = 'Revenus', xaxt = "n", yaxt="n", pch=9, type='o', col='red')
par(new=TRUE)
plot(risque_discret$revenus_cont,risque_discret$modele_cont, xlim=c(-200,5200), ylim=c(0,0.4), ylab = 'Probabilité de défaut', xlab = 'Revenus', xaxt = "n", yaxt="n", pch=7, type='o', col='blue')


axis(1, at=pretty(risque_discret$revenus_cont), lab=pretty(risque_discret$revenus_cont), las=TRUE)
axis(2, at=pretty(risque_discret$modele_cont), lab=pretty(risque_discret$modele_cont), las=TRUE)

