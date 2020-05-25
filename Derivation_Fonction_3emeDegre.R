# By Ludovic G.

#--------------------------------------------------------------
# Theorie
#--------------------------------------------------------------
# Une TANGENTE a la courbe est representative d'une fonction en un point particulier: il s'agit d'une droite qui passe par ce poiint et qui "ressemble" le mieux possible a la courbe aux environs de ce point. Cette droite est la representation d'une fonctione affine.

# Supposons que notre point ait pour abscisse a. On appelle nombre derive de la fonction f en a, note f'(a), le coefficient directeur de la tangente au point d'abscisse a.

# On peut definir graphiquement ce coefficient grace a la formule : Coef = (Yb - Ya) / (Xb - Xa). Mais comment le determiner mathematiquement?

# Chaque type de fonction utilise une formule differente. Voyons celle de la fonction polynomiale du second degre (ax^2 + bx + c). Une fonction du second degre est derivable en tout point. La derivee s'ecrit sous la forme f'(x) = 2ax + b. C'est donc une fonction affine, voire lineaire si b = 0. Ainsi le carre est remplace par une multiplication par 2, le x qui n'est pas au carre disparait (ne subsiste que son coefficient) ainsi que la constante.

# Pour la determination de l'equation de la tangente en un point d'abscisse a, il faut connaitre la formule :
# y = f(a) + f'(a)(x-a)
# L'operation consiste donc d'une part a trouver l'expression de la derivee et d'autre part calculer f(a) et f'(a).

#--------------------------------------------------------------
# Derivation d'une fonction polynomiale du troisieme degre
#--------------------------------------------------------------
par(mfrow = c(2,3))
valeurs_abscisses = c(-20,-15,-10,-5,0,10)

for(draw_tangente in valeurs_abscisses) {
	
	# Valeur a modifier
	choix_valeur_absc = draw_tangente
	
# Definition de l'axe des x
x = seq(-100,100, by = 0.1)

# Polynome du 2nd degré : f(x) = ax^2 + bx + c
a = 0.2 ; b = 4 ; c = 0.3 ; d = 0.6
#y = a * x^2 + (b * x) - c
y = a * x^3 + b * x^2 + c * x + d

# Calcul de la derivee de ma fonction quadratique pour le point d'abscisse desire
fprim_a = (3 * a * x[which(x == choix_valeur_absc)]^2) + 
	2 * b * x[which(x == choix_valeur_absc)] +
	 c 

# Quel valeur de y quand x = choix_valeur_absc
f_a = y[which(x == choix_valeur_absc)]

#--------------------------------------------------------------
# Graphique
#--------------------------------------------------------------

plot(y~x, type = "l", xlim = c(-50,50), ylim = c(-1000,1000), lwd = 3, main = paste("Abscisse (x) = ", choix_valeur_absc), ylab = "f(x) = ax^3 + bx^2 + cx + d")
abline(v = 0, col = "gray", lty = 2)
abline(h = 0, col = "gray", lty = 2)
text(30,150,paste("f'(",choix_valeur_absc,") = ", fprim_a), col = "red")

# Trace la tangente en un point
vec = f_a
vec2 = f_a
i = 1

for(k in -1000:(1000-choix_valeur_absc)) {
	vec[i+1] = vec[i] + fprim_a
	vec2[i+1] = vec2[i] - fprim_a
	i = i + 1
}

x3 = c(choix_valeur_absc:(-2001 + 2 * choix_valeur_absc))
x2 = c(choix_valeur_absc:2001)
lines(x2, vec, col = "red", lwd = 2)
lines(x3, vec2, col = "red", lwd = 2)
points(choix_valeur_absc, f_a, pch = 21, bg = "orange", cex = 1.5, col = "red")
legend("bottomright", legend = c("f(x)","tangeante"), col = c("black","red"), lwd = c(3,2))

vals_grille = seq(-1000,1000,by = 100)
for(grille in vals_grille) {
	abline(h = grille, col = "lightgray", lty = "dotted", lwd = 0.5)
	abline(v = grille/10, col = "lightgray", lty = "dotted", lwd = 0.5)

}

}