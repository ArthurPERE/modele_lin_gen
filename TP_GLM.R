rm(list=ls())

tab = read.table('ILLE.txt', h=T)

malade = abs(tab$malade-2)

alcool = vector()
tabac = vector()
age = vector()

for (i in 1:length(tab[,2])){
  if (tab[i,1]<=3){ age[i] = 0 }
  if (tab[i,1]>3){ age[i] = 1 }
  
  if (tab[i,2]<=2){ alcool[i] = 0 }
  if (tab[i,2]>2){ alcool[i] = 1 }
  
  if (tab[i,3]<=2){ tabac[i] = 0 }
  if (tab[i,3]>2){ tabac[i] = 1 }
  
}

tab2 = as.data.frame(cbind(age, alcool, tabac, malade))

# logit(P(M|alcool)) = b0 + b1*alcool
glm1 = glm(malade~alcool, data=tab2, family='binomial')
summary(glm1)
# estimate alcool est >1 donc cette variable a un effet deletere 

OR = exp(1.73)
# b1 est distribuer normalement donc IC = b1 ± sqrt(var(b1)), sqrt(var(b1)) = std.error

exp(1.73 + 1.96 * 0.1752)
exp(1.73 - 1.96 * 0.1752)
