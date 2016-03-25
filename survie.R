rm(list=ls())

library(survival)

data = cbind(c(12.5,12.9,14.1,17.8,19.7,25.8,33.7,34.4,37.4,45.6),c(0,1,0,0,1,1,0,0,0,1))
surv <- survfit(Surv(data[,1], data[,2])~ 1, conf.type="none")
summary(surv)

plot(surv, xlab="Time", ylab="Survival Probability")





#############################"
    # Exercice 2
#############################

data = read.table("Freireich.txt", h=T)
surv = survfit(Surv(data[,1], data[,2])~1)
plot(surv)
summary(surv)
