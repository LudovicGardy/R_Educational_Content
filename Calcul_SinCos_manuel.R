# By Ludovic G, with the participation of Martin D.
# Calcul manuel d'un cosinus et d'un sinus. Imitation des fonctions internes deja
# existantes dans R sin() et cos().

par(mfrow = c(1,2))

#--------------------------------------------------------------
# Sin / Cos avec les fonctions de R
#--------------------------------------------------------------

sinus = sin(seq(1,5000, by = 0.01))
plot(sinus[1:1000],type ="l", lwd = 2)

cosinus = cos(seq(1,5000, by = 0.01))
lines(cosinus[1:1000],type ="l", col = "red", lwd = 2)

legend("topright", legend = c("sinus","cosinus"), col = c("black","red"), lwd = 2, lty = 1)

#--------------------------------------------------------------
# Radians
#--------------------------------------------------------------

cercle = c(0, pi/12, pi/8, pi/6, pi/4, pi/3, 3*pi/8, 5*pi/12, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi)

plot(0,0)

for(i in cercle) {
	
	abline(a = 0, b = i)
	
}

for(i in cercle) {
	
	abline(a = 0, b = -i, col = "red")
	
}

# Avec Normalisation
#-----------------------------------------------------------
# DEVELOPPEMENT EN SERIE DE MACLAURIN DES FONCTIONS USUELLES
#-----------------------------------------------------------
# Calcul cosinus a la main 
#-----------------------------------------------------------

calcul_cos = function(alpha,N) {
	
	pair_ou_impair = floor(alpha/pi) %% 2
	if(pair_ou_impair == 0) {
		pair = TRUE
	} else {
		pair = FALSE
	}
		
	alpha = ( alpha/pi - floor(alpha/pi) ) * pi

	n = 0:N
	
	cosinus = sum(( (-1)^n / factorial(2 * n) ) * alpha^(2*n))
		
	if(pair == FALSE) {
		cosinus = - cosinus
	}
	
	return(cosinus)
	
}

calcul_cos(36,10)
cos(36)

#--------------------------------------------------------------
# Calcul sinus a la main
#--------------------------------------------------------------

calcul_sin = function(alpha,N) {
	
	pair_ou_impair = floor(alpha/pi) %% 2
	if(pair_ou_impair == 0) {
		pair = TRUE
	} else {
		pair = FALSE
	}
	
	alpha = ( alpha/pi - floor(alpha/pi) ) * pi
	
	#sinus = ( (8*(1-sqrt(2))) / pi^2) * alpha^2 + (2*(2*sqrt(2) - 1) / pi) * alpha 
	
	n = 0:N
	
	sinus = sum(( (-1)^n / factorial(2 * n  + 1) ) * alpha^(2*n+1))
	
	if(pair == FALSE) {
	sinus = - sinus
	}
	
	return(sinus)
	
}

calcul_sin(50,10)
sin(50)