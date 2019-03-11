library(glmdisc)

d=2

generate_data <- function(k,n) {
    set.seed(k)
    x = matrix(runif(d*n), nrow = n, ncol = d)
    cuts = seq(0,1,length.out= 4)
    xd = apply(x,2, function(col) as.numeric(cut(col,cuts)))
    theta = t(matrix(c(0,0,0,2,2,2,-2,-2,-2),ncol=3,nrow=3))
    log_odd = rowSums(t(sapply(seq_along(xd[,1]), function(row_id) sapply(seq_along(xd[row_id,]),
                                                                          function(element) theta[xd[row_id,element],element]))))
    y = rbinom(n,1,1/(1+exp(-log_odd)))
    return(list(x=x,y=y,log_odd=log_odd))
}

list_levels = array(0,dim=100)
list_c1 = array(0,dim=100)
list_c2 = array(0,dim=100)

for (b in 1:100) {
    list2env(generate_data(b,1000),env=environment())
    
    sem_disc = glmdisc(x,y,iter=200,m_start=3,test=FALSE,validation=FALSE,criterion="bic",interact=FALSE)
    
    list_levels[b] = nlevels(factor(sem_disc@disc.data[,1]))
    
    if (list_levels[b]==3) {
      val1 = sem_disc@cont.data[sem_disc@disc.data[,1] == levels(sem_disc@disc.data[,1])[1],1]
      val2 = sem_disc@cont.data[sem_disc@disc.data[,1] == levels(sem_disc@disc.data[,1])[2],1]
      val3 = sem_disc@cont.data[sem_disc@disc.data[,1] == levels(sem_disc@disc.data[,1])[3],1]
      list_c1[b] = mean(sort(c(min(val1),max(val1),min(val2),max(val2),min(val3),max(val3)))[2:3])
      list_c2[b] = mean(sort(c(min(val1),max(val1),min(val2),max(val2),min(val3),max(val3)))[4:5])
    }
}

summary(list_c1)
summary(list_c2)

# 2 : 3 // 3 : 53 // 4 : 44
