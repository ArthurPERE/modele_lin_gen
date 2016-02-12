rm(list=ls())

data = matrix(c(29,70,19,109), ncol=2)
colnames(data) = c('IDM', 'nIDM')
rownames(data) = c('Co', 'nCo')

chisq.test(data, correct = F)
# il y a une association statistiquement significative entre les donnees

OR = 29*109/(70*19)
# OR = 2.38 [1.24 4.66]

# on a une augmentation des risques quand on prend la pillule contraceptive