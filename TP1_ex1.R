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
# test de Ficher 'a la main' sur un tableau moindre

#     M   nM
# A   4   1
# B   0   4

# On fait toutes les combinaison tel que toutes sommes des colones et des lignes soit toujours
# egale a la somme des lignes et colones pour le tableau initial

# 4   1       3   2       2   3       1   4       0   5
# 0   4       1   3       2   2       3   1       4   0

# on compte le nobre de taleau que l'on pourrait observer (on peut le prendre en colonnes 
# ou en ligne), on raisonne en colonnes ici
# [1,1] parmie la somme de la premiere colonne * [1,2] parmie la somme de la 2nd colonne

# n1 = 4 parmie 4 * 1 parmie 5 = 5
# n2 = 3 parmie 4 * 2 parmie 5 = 40
# n3 = 2 parmie 4 * 3 parmie 5 = 60
# n4 = 1 parmie 4 * 4 parmie 5 = 20
# n5 = 0 parmie 4 * 5 parmie 5 = 1

N=choose(4,4)*choose(5,1) + choose(4,3)*choose(5,2)+choose(4,2)*choose(5,3)+choose(4,1)*
  choose(5,4)+choose(4,0)*choose(5,5)

# notre tableau 1 est le tableau initial, on peut calculer la proba que l'on observe ce tab
p1 = 5/126
# environ 4% 

# H0 : P(M|A) = P(M|B)    H1 : P(M|A) > P(M|B)


# On calcul la distance des tab avec un cal du determinant, on prend la distance du tab
# qu'on a au debut, ensuite on additionne le nombre de talbeau dont la distance est >= 
# a la distance du tab d'origine et on divise le tout par N (voire ligne 39)


fisher.test(matrix(c(4,0,1,4), nrow=2, ncol=2))


### Reour au data
fisher.test(t(data))

RR = Rb/Ra
V = 290/(10*300)+245/(20*265) # Var(ln(RR))
log(RR) + 1.96 * sqrt(V)
log(RR) - 1.96 * sqrt(V)

Dr = abs(Rb-Ra)
1/Dr # NNT qui est le nombre de patient qu'il faut traiter pour eviter la foramtion d'une
# anomalie