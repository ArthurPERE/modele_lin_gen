rm(list=ls())

tab = read.table('poisson.txt', h=T)
View(tab)

# nombre de deces en tout
n = sum(tab$Deaths.1977.85)

# nombre estimer d'enfant année a risque 9 = 1986 - 1977
pa = sum(tab$Under.5.1981) * 9

lambda = n/pa # 2.63 pour 1000 pa

# intervalle de confiance

lambda + 1.96 * 