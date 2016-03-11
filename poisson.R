rm(list=ls())

tab = read.table('poisson.txt', h=T)
View(tab)

# nombre de deces en tout
n = sum(tab$Deaths.1977.85)

# nombre estimer d'enfant année a risque 9 = 1986 - 1977
pa = sum(tab$Under.5.1981) * 9

lambda = n/pa # 2.63 pour 1000 pa

# intervalle de confiance
# var(lambda) = var(N * 1/pa) = 1/pa^2 * var(N) = n/pa^2
lambda + 1.96 * sqrt(n)/pa
lambda - 1.96 * sqrt(n)/pa


# construction modele R

glm1 = glm(tab$Deaths.1977.85~offset(log(9*tab$Under.5.1981)), family='poisson')
summary(glm1)
# ln(y) = b0 + offset(ln(pa))
exp(glm1$coefficients[1])
# b0 = ln(lanbda = y/pa)
# donc on a le taux de mortalite qui est egale a exp(b0)

# h0 : savoir si le taux de mortalite est egale a 0

# Intervalle de confiance a 95% = exp(b0+1.96*Std. Error)
exp(glm1$coefficients[1]+1.96*summary(glm1)$coefficients[1,2])
exp(glm1$coefficients[1]-1.96*summary(glm1)$coefficients[1,2])
# donc on n'a pas le taux de mortalite qui est egale a 0

# le nombre de deces attendut pour chaque region est yi = exp(b0)*PAi
plot(exp(glm1$coefficients[1])*(9*tab$Under.5.1981))
plot(tab$Deaths.1977.85,exp(glm1$coefficients[1])*(9*tab$Under.5.1981), 
     xlab='observer',
     ylab='attendue',
     main='attendue vs observer')

# plus le nombre de deces est grand plus on commet d'erreur





############################### Partie 2
# ln(y) = b0 + offset(ln(PA)) + b1*Easting
glm2 = glm(Deaths.1977.85~offset(log(9*Under.5.1981))+Easting, data=tab, family='poisson')
summary(glm2)

# coefficient
exp(glm2$coefficients)

# Intervalle de confiance
exp(glm2$coefficients + 1.96*summary(glm2)$coefficients[,2])
exp(glm2$coefficients - 1.96*summary(glm2)$coefficients[,2])

# on est a la limite de la significativite pour b1 


# ln(y) = b0 + offset(ln(PA)) + b1*Northing
glm3 = glm(Deaths.1977.85~offset(log(9*Under.5.1981))+Northing, data=tab, family='poisson')
summary(glm3)

# coefficient
exp(glm3$coefficients)

# Intervalle de confiance
IC = matrix(nrow=2, ncol=length(glm3$coefficients))
IC[1,] = exp(glm3$coefficients + 1.96*summary(glm3)$coefficients[,2])
IC[2,] = exp(glm3$coefficients - 1.96*summary(glm3)$coefficients[,2])
colnames(IC) = names(glm3$coefficients)
IC

# on a la significativite pour b1 pour le nord sud

# on diminue par 0.994 pour le taux de mortalite quand on ce deplace du sud vers le nord


# ln(y) = b0 + offset(ln(PA)) + b1*Northing + b2*Easting
glm4 = glm(Deaths.1977.85~offset(log(9*Under.5.1981))+Northing+Easting,
           data=tab, family='poisson')
summary(glm4)

# coefficient
exp(glm4$coefficients)

# Intervalle de confiance
IC = matrix(nrow=2, ncol=length(glm4$coefficients))
IC[1,] = exp(glm4$coefficients + 1.96*summary(glm4)$coefficients[,2])
IC[2,] = exp(glm4$coefficients - 1.96*summary(glm4)$coefficients[,2])
colnames(IC) = names(glm4$coefficients)
IC

