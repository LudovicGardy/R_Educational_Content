# By Ludovic G.
# Petit script pour faciliter les calculs d'aire sous la courbe.

# Pour une fonction strictement positive
#---------------------------------------------------------------
# Generation des donnees
#---------------------------------------------------------------

x = seq(1,6)
y = sqrt(x-1)

x_courbe = seq(1,6, by = 0.01)
y_courbe = sqrt(x_courbe-1)

#---------------------------------------------------------------
# Representation graphique
#---------------------------------------------------------------

par(mfrow = c(1,1))

plot(x,y,type = "l", lwd = 0.1, col = "darkgray", main = "Approximaition aire sous la courbe: Aires des trapezes", ylab = "f(x) = sqrt(x-1)", xlab = "x = 1:6")
lines(x_courbe,y_courbe, lwd = 3, col = "darkgray")

lines(c(2,2),c(0,y[x==2]), col = "red", lty = 2)
lines(c(3,3),c(0,y[x==3]), col = "red", lty = 2)
lines(c(4,4),c(0,y[x==4]), col = "red", lty = 2)
lines(c(5,5),c(0,y[x==5]), col = "red", lty = 2)
lines(c(6,6),c(0,y[x==6]), col = "red", lty = 2)

lines(c(1,2),c(y[x==1], y[x==2]), col = "red", lty = 2)
lines(c(2,3),c(y[x==2], y[x==3]), col = "red", lty = 2)
lines(c(3,4),c(y[x==3], y[x==4]), col = "red", lty = 2)
lines(c(4,5),c(y[x==4], y[x==5]), col = "red", lty = 2)
lines(c(5,6),c(y[x==5], y[x==6]), col = "red", lty = 2)

text(1.6,0.15, "Trapeze 1", col = "red")
text(2.5,0.7, "Trapeze 2", col = "red")
text(3.5,0.7, "Trapeze 3", col = "red")
text(4.5,0.7, "Trapeze 4", col = "red")
text(5.5,0.7, "Trapeze 5", col = "red")

#---------------------------------------------------------------
# Calcul des aires des trapezes :
#---------------------------------------------------------------
# Note 1 : peut aussi se faire avec des rectangles. 
# Note 2 : Aire du trapeze = demi-somme de ses 2 bases * sa hauteur. soit :
# f(x0) + f(x1) / 2

# Aire totale = 
# ( f(1) + f(2) ) / 2 * delta_x +
# ( f(2) + f(3) ) / 2 * delta_x + 
# ( f(3) + f(4) ) / 2 * delta_x + 
# ( f(4) + f(5) ) / 2 * delta_x + 
# ( f(5) + f(6) ) / 2 * delta_x
# delta_x = hauteur du trapeze, c'est a dire la valeur séparant 2 lignes verticales soit 2 valeurs de x.

# Pour simplifier on factorise :
# Aire totale = 
# (delta_x / 2) * (1 * f(1) + 2 * f(2) + 2 * f(3) + 2 * f(4) + 2 * f(5) + 1 * f(6) )
Aire_sous_la_courbe_approximative = (1 / 2) * (
(1 * y[x==1]) +
(2 * y[x==2]) +
(2 * y[x==3]) +
(2 * y[x==4]) +
(2 * y[x==5]) +
(1 * y[x==6]) 
)

print(Aire_sous_la_courbe_approximative)
text(2,2, paste("AUC =", round(Aire_sous_la_courbe_approximative,2), "unites d'aire"))

# Note on perd un peu d'aire sur le trapeze 1 mais ca reste une bonne estimation.
# Pour + de precision j'aurai pu raccourcir l'intervalle des x, en utilisant x_courbe et y_courbe pour calculer mes trapezes par exemple, ce que je n'ai pas fait ici pour simplifier.

arrows(x0 = 2, x1 = 3, y0 = 0.1, y1 = 0.1, col = "purple", length = 0.1, lwd = 2)
arrows(x0 = 3, x1 = 2, y0 = 0.1, y1 = 0.1, col = "purple", length = 0.1, lwd = 2)
text(2.5,0, "dx ou delta_x", col = "purple", cex = 0.6)

#===============================================================
# Pour une fonction positive & negative
#---------------------------------------------------------------
# Generation des donnees
#---------------------------------------------------------------

# Note : quand on veut calculer l'aire d'une fonction qui n'est pas strictement positive, on parle : 
# d'integrale.
