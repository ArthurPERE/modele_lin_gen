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
