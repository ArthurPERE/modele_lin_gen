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
# b1 est distribuer normalement donc IC = exp(b1 ± 1.96*sqrt(var(b1)))
# sqrt(var(b1)) = std.error

exp(1.73 + 1.96 * 0.1752)
exp(1.73 - 1.96 * 0.1752)

# pour le tabac
glm2 = glm(malade~tabac, data=tab2, family='binomial')

exp(0.67334 + 1.96 * 0.17676)
exp(0.67334 - 1.96 * 0.17676)

# pour l'age
glm3 = glm(malade~age, data=tab2, family='binomial')
exp(1.3877 + 1.96 * 0.1738)
exp(1.3877 - 1.96 * 0.1738)





# ====================================================================
##### Question 2

# ========================================
# question 2.1
# ========================================
glm4 = glm(malade~tab$alcool, family='binomial')
summary(glm4)
# logit(P(M|X)) = b0 + b1 X ; X apparenant à {1,2,3,4}
# X = 1 ; logit(P(M|X=1)) = b0 + b1
# X = 2 ; logit(P(M|X=2)) = b0 + 2b1

# OR2/1 = exp(b1) 
# ln(oddsM|2) - ln(oddsM|1) = b0 + 2b1 - b0 - b1 = b1

# OR3|1 = exp(2b1)
# OR4|1 = exp(3b1)

# donc a chaque fois qu'on augment X d'une unitee on aura le ln(odds) qui augmentera de b1

# OR2/1 = 2.83 [2.37 ; 3.40]

# OR3/2 = exp(b1)
exp(glm4$coefficients[2])


# pour calculer la var(b1) pour IC pour les OR3/1 ou le reste, il faut avoir a l'esprit que :
# var(aX) = a^2 var(X) donc sqrt(var(aX)) = a sqrt(var(X))


# ========================================
# question 2.2
# ========================================
glm5 = glm(malade~as.factor(tab$alcool), family='binomial')
summary(glm5)

# ln(oddsM|1) = b0
# ln(oddsM|2) = b0 + b1
# ln(oddsM|3) = b0 + b2
# ln(oddsM|4) = b0 + b3

# le modele qui fait le moins d'hypothese est emboite dans celui qui fait le plus d'hypothese
# ici le modele qui fait le plus d'hypthese est le premier car il fait l'hypothese que
# l'effet de l'alcool est lineaire

# pour montre que les modele sont emboite, il faut montrer que ces coefficients sont dans des
# cas particuliers egaux a ceux de l'autre modele

# ici 
# Y = g0 + g1 X
# Y = b0 + b1 I[X = 2] + b2 I[X = 3] + b3 I[X = 4]
# 1->2   g1 = b1
# 1->3   2g1 = b2
# 1->4   3g1 = b3

# OR2/1
exp(1.2712)
exp(1.2712 + 1.96 * 0.2323)
exp(1.2712 - 1.96 * 0.2323)

# OR3/1
exp(2.0545)
exp(2.0545 + 1.96 * 0.2611)
exp(2.0545 - 1.96 * 0.2611)

# OR4/1
exp(3.3042)
exp(3.3042 + 1.96*0.3237)
exp(3.3042 - 1.96*0.3237)


anova(glm4, glm5, test='Chisq')
# donc ils ne sont pas statistiquement significatif

# OR2/1 = b2 - b1

# var(X-Y) = var(X) + var(Y) - 2cov(X,Y)
summary(glm5)$cov.unscaled

exp(2.0545 - 1.2712 + 1.96*sqrt((0.05397805 + 0.06817539 - 2 * 0.03707329)))


# graphe

ORmodlin <- exp(c(0,glm4$coef[2],2*glm4$coef[2],3*glm4$coef[2]))
BImodlin <- exp(c(0,(glm4$coef[2]-1.96*sqrt(summary(glm4)$cov.unscaled[4])),
                  2*(glm4$coef[2]-1.96*sqrt(summary(glm4)$cov.unscaled[4])),
                  3*(glm4$coef[2]-1.96*sqrt(summary(glm4)$cov.unscaled[4]))))
BSmodlin <- exp(c(0,(glm4$coef[2]+1.96*sqrt(summary(glm4)$cov.unscaled[4])),
                  2*(glm4$coef[2]+1.96*sqrt(summary(glm4)$cov.unscaled[4])),
                  3*(glm4$coef[2]+1.96*sqrt(summary(glm4)$cov.unscaled[4]))))


plot(c(1,2,3,4),ORmodlin,ylim=c(0,55),xlim=c(1,4.5),xlab="Alcool")
segments(c(1,2,3,4),BSmodlin,c(1,2,3,4),BImodlin)


# ================================================================================
####### Question 3
# ================================================================================

# X = {0,1}
# b0 = -1.86 ; b1 = 1.73

# X = {1,2}
# b0 = -3.58 ; b1 = 1.73

# X = {0,2}
# b0 = -1.86 ; b1 = 0.86

rm(glm1, glm2, glm3, glm4, glm5, BImodlin, BSmodlin, OR, ORmodlin, i)
# ================================================================================
####### Question 4
# ================================================================================

glm6 = glm(malade~age+alcool+tabac, data=tab2, family='binomial')
summary(glm6)
# ln(P(M|Age, Alcool, Tabac)/1-P(M|Age, Alcool, Tabac)) = b0 + b1*age + b2*alcool + b3*tabac

# 100 : age = 1 ; alcool=0; tabac=0;
# OR100/000 = exp(b1)

# ORage = 4.31 ; ORalcool = 5.67 ; ORtabac = 2.23










