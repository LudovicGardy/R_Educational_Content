# By Ludovic G.

###=========================================###
### Techniques de normaliastion de donnees  ###
###=========================================###

#--------------------------------------------------------------
# 1. Simulation de donnees
#--------------------------------------------------------------
set.seed(3)
a = round(runif(30,min = -15, max = 20),4)

# Min & max
min_a = min(a)
max_a = max(a)

print(a)
print(min_a)
print(max_a)

#--------------------------------------------------------------
# 2.1 Normalisation entre 0 et +1
#--------------------------------------------------------------
a_normalized = NA

for(position in 1 : length(a)) {
	
	a_normalized[position] = (a[position] - min_a) / (max_a - min_a)
	
}

#--------------------------------------------------------------
# 2.2 Normalisation entre -1 et +1
#--------------------------------------------------------------
a_normalized_v2 = NA

for(position in 1 : length(a)) {
	
	a_normalized_v2[position] = ( (2*(a[position] - min_a)) / (max_a - min(a))) - 1
	
}

which(a_normalized_v2 == - 1)
which(a_normalized_v2 == 1)
plot(a_normalized_v2, pch = 19,ylab = "y", xlab = "x")

#--------------------------------------------------------------
# 2.3 Noramlisation entre deux bornes que je defini
#--------------------------------------------------------------
borne_inf = -10
borne_sup = +10

min_vecteur = min(a)
max_vecteur = max(a)

fun_normalize = function(input, min_vecteur, borne_inf, max_vecteur, borne_sup) {
	
	if(max_vecteur == min_vecteur) {
		return(borne_inf)
	}
	
	a = (borne_sup - borne_inf) / (max_vecteur - min_vecteur)
	b = borne_inf - a * min_vecteur
	
	valeur_finale = (a * input + b)
	
	return(valeur_finale)
	
}

a_normalized_v3 = NA

for(position in 1 : length(a)) {
	a_normalized_v3[position] = fun_normalize(a[position], min_vecteur, borne_inf, max_vecteur,borne_sup)
}

#--------------------------------------------------------------
# 2.4 Normalisation par Z-score
#--------------------------------------------------------------
# Note : la je fais par rapport a une population de reference
# Mais on peut le faire sur le vecteur etudie directement... Ici, je regarde
# a quel point chacune de mes mesures s'eloigne de ce qui est observe dans la
# population de reference.
# Si je fais un z-score sur un seul vecteur alors c'est juste une maniere de 
# normaliser les donnees.

set.seed(10)
population_reference = round(runif(30,min = -10, max = 15),4)

moyenne_ref = mean(population_reference, na.rm = T)
stdDev_ref = sd(population_reference)

a_normalized_v4 = NA

for(position in 1 : length(a)) {
	a_normalized_v4[position] = (a[position] - moyenne_ref) / (stdDev_ref)
}

#--------------------------------------------------------------
# 3. Resume
#--------------------------------------------------------------
print(a)
print(a_normalized_v2)
print(a_normalized_v3)
print(a_normalized_v4)

# Exploration 1
par(mfrow = c(2,3))
plot(a, pch = 19, main = "Donnees brutes (points)")
plot(a, type = "l", main = "Donnees brutes (lignes)")
plot(a_normalized, pch = 19, type = "l", main = "Normalisation entre 0 et 1")
plot(a_normalized_v2, pch = 19, type = "l", main = "Normalisation entre -1 et 1")
plot(a_normalized_v3, pch = 19, type = "l", main = paste("Normalisation entre", borne_inf," et ", borne_sup))
plot(a_normalized_v4, pch = 19, type = "l", main = "Normalisation par Z-score")

# Exploration 2
par(mfrow = c(2,4))
hist(a, main = "Donnees brutes", col = "lightgray")
hist(log(abs(a)), main = "log(abs(a))", col = "lightgray")
hist(exp(abs(a)), main = "exp(abs(a))", col = "lightgray")
hist(a_normalized, main = "Normalisation entre 0 et 1", col = "lightgray")
hist(a_normalized_v2, main = "Normalisation entre -1 et 1", col = "lightgray")
hist(a_normalized_v3, main = paste("Normalisation entre", borne_inf," et ", borne_sup), col = "lightgray")
hist(a_normalized_v4, main = "Normalisation par Z-score", col = "lightgray")

# Exploration 3
longueur = 1:length(a)
par(mfrow = c(2,4))
hist(lm(a ~ longueur)$residuals, main = "Donnees brutes", col = "lightgray")
hist(lm(log(abs(a))~longueur)$residuals, main = "log(abs(a))", col = "lightgray")
hist(lm(exp(abs(a))~longueur)$residuals, main = "exp(abs(a))", col = "lightgray")
hist(lm(a_normalized ~ longueur)$residuals, main = "Normalisation entre 0 et 1", col = "lightgray")
hist(lm(a_normalized_v2 ~ longueur)$residuals, main = "Normalisation entre -1 et 1", col = "lightgray")
hist(lm(a_normalized_v3 ~ longueur)$residuals, main = paste("Normalisation entre", borne_inf," et ", borne_sup), col = "lightgray")
hist(lm(a_normalized_v4 ~ longueur)$residuals, main = "Normalisation par Z-score", col = "lightgray")

# Exploration 4
longueur = 1:length(a)
par(mfrow = c(2,4))
hist(lm(a ~ longueur)$residuals, main = "Donnees brutes", col = "lightgray")
hist(lm(log(abs(a))~log(longueur))$residuals, main = "log(abs(a))", col = "lightgray")
hist(lm(exp(abs(a))~exp(longueur))$residuals, main = "exp(abs(a))", col = "lightgray")
hist(lm(a_normalized ~ longueur)$residuals, main = "Normalisation entre 0 et 1", col = "lightgray")
hist(lm(a_normalized_v2 ~ longueur)$residuals, main = "Normalisation entre -1 et 1", col = "lightgray")
hist(lm(a_normalized_v3 ~ longueur)$residuals, main = paste("Normalisation entre", borne_inf," et ", borne_sup), col = "lightgray")
hist(lm(a_normalized_v4 ~ longueur)$residuals, main = "Normalisation par Z-score", col = "lightgray")