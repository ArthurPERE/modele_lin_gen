rm(list=ls())
data = matrix(c(96,104,109,666), nrow=2, ncol=2)
##### Caracteristique de l'etude
# C'est une etude cas temoins


##### H0?

# H0 : P(OH|K) = P(OH|nK) K = maladie OH = exposition

# odds ratio = P(OH|K)/P(nOH|K)
#              ----------------
#              P(OH|nK)/P(nOH|nK)

OR = (96/200) / (104/200)   /(    (109/775) / (666/775)        )
OR = 96*666/(104*109)

# OR = 5.64

# IC = ln(OR) ± 1.96*sqrt(var(ln(OR))) avec var(ln(OR)) = somme des inverse des termes du
# tableau

var = 1/96+1/104+1/109+1/666

exp(log2(OR)+1.96*sqrt(var))
exp(log2(OR)-1.96*sqrt(var))

chisq.test(data, correct = F)

# on une valeur de OR statistiquement different de 1 donc on a 5.64 fois plus de chance
# d'attraper un cancer quand on boit plus de 80g par jour que ce qui en a moins


###### Question 3
chisq.test(matrix(c(19,15,47,324), nrow=2, ncol=2), correct = F) # sans tabac et jeune
chisq.test(matrix(c(11,11,17,84), nrow=2, ncol=2), correct = F) # avec tabac et jeune
chisq.test(matrix(c(47,55,39,215), nrow=2, ncol=2), correct = F) # sans tabac et agee
chisq.test(matrix(c(19,23,6,43), nrow=2, ncol=2), correct = F) # avec tabac et agee


###### Question 4
# ORmh = sum(ai*di/ni)/sum(bi*ci/ni) i = 1..4
# les ai, bi, ci, di sont les coefficients dans les tableaux

ORmh = ( (19*324/405)+(11*84/123)+(47*215/356)+(19*43/91) )/
  ( (15*47/405)+(11*17/123)+(55*39/356)+(23*6/91) )


# ORmh = 5.56[3.85 8.03]
# chisq mh = 93.69 (voire cours pour la formule modele lineaire generalises partie 1
# 26eme diapositive)