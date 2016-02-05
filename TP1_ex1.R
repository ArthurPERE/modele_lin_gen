rm(list=ls())

data = matrix(c(20,10,245,290), nrow=2,ncol=2)
rownames(data) = c('Placebo', 'Acide Folique')
colnames(data) = c('Anomalie', 'pas d anomalie')

######### Question 1
# Prevenir les anomalies, voire si l'acide folique est efficace contre l'anomalie

######### Question 2
# H0 : independance entre anomalie et acide folique


######## Question 3
Rb = 10/300
Ra = 20/265
# test de Ficher 'a la main'