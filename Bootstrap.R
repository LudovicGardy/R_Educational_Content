# By Ludovic G.

###=========================================###
###             BOOTSTRAP                   ###
###=========================================###

# Creation d'une matrice
matrix <- matrix(1,100,nrow=20,ncol=5)

# Mettre la matrice au cube
for (auCube in 1 : 100) {
  matrix[auCube] = vecteur[auCube]^3
}

# Tirage de valeurs au hasard dans la colonne 1 de la matrice
BootStrap = sample(matrix[,1])

# Moyenne 
Moyenne300Boot = rep(0,300)

for (experience in 1 : 300) {
  BootStrap = sample(matrix[,1],replace=T)
  Moyenne300Boot[experience] = mean(BootStrap)
}

ma_Moyenne = mean(Moyenne300Boot)