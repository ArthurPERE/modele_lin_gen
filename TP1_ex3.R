rm(list=ls())

data = matrix(c(29,70,19,109), ncol=2)
colnames(data) = c('IDM', 'nIDM')
rownames(data) = c('Co', 'nCo')

chisq.test(data, correct = F)
# il y a une association statistiquement significative entre les donnees

OR = 29*109/(70*19)
# OR = 2.38 [1.24 4.66]

# on a une augmentation des risques quand on prend la pillule contraceptive
# ainsi que l'age

# mais plus l'age augmente moins les femmes prennent la pillule

tab = array(c(8,44,2,50,21,26,17,59), dim=c(2,2,2))

mantelhaen.test(tab)

###### Question 4

# log odds = b0 + b1 co + b2 age
# log P(IDM=1|CO, age) = b0 + b1 co + b2 age
# log P(IDM=1|CO=0, age=0) = b0


# le modele 2 inclu un autre terme qui est l'interaction