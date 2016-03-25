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


surv2 = survfit(Surv(T, delta)~X6.MP, data=data, type="kaplan-meier")
plot(surv2)

survdiff(Surv(T,delta)~X6.MP, data=data)
# difference entre les deux groupe est significatif

surv3 = coxph(Surv(T, delta)~X6.MP, data=data)
summary(surv3)




#############################
    # Exercice 3
#############################

data = read.table("breast.txt", h=T, sep="\t")
grpGang = vector()
data1 = na.omit(data)
for (i in 1:length(data1$nbGanglion)){
  if (data1$nbGanglion[i]==0) {grpGang[i] = 0}
  if ((data1$nbGanglion[i]>=1)&&(data1$nbGanglion[i]<=4)) {grpGang[i] = 1}
  if ((data1$nbGanglion[i]>=5)&&(data1$nbGanglion[i]<=9)) {grpGang[i] = 2}
  if (data1$nbGanglion[i]>=10) {grpGang[i] = 3}
}


plot(survfit(Surv(data$time, data$delta)~grpGang, conf.int=0))


surv1 = coxph(Surv(data$time, data$delta)~as.factor(grpGang))
summary(surv1)
surv.5 = coxph(Surv(data$time, data$delta)~data$nbGanglion)
anova(surv1, surv.5)

surv2 = coxph(Surv(data$time, data$delta)~data$tailleTumeur)
summary(surv2)

surv3 = coxph(Surv(data$time, data$delta)~log2(data$tailleTumeur))
summary(surv3)

surv4 = coxph(Surv(data$time, data$delta)~log2(data$tailleTumeur)+data$nbGanglion)
summary(surv4)