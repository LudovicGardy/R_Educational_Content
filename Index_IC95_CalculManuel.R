# By Ludovic G.

#===================================#
# Intervalles de confiance : calcul #
#===================================#

# Courbe
plot(qnorm, lwd =3, xlab = "Valeur souhaitee de l'intervalle de confiance (0% a 100%)", main = "Trace des valeurs de z en fonction de l'IC desire : exemple IC a + ou - 95% en orange", ylab = "qnorm (z)", sub = "Selon une loi normale centree reduite")

abline(v = 0.025,col = "red", lwd = 2)
abline(v = 0.975,col = "red", lwd = 2)

abline(h = qnorm(0.025),col = "red", lwd = 2, lty = 3)
abline(h = qnorm(0.975),col = "red", lwd = 2, lty = 3)

points(0.975,qnorm(0.975), pch = 19, col = "orange", cex = 2)
text(0.78,1.75, "x = 0.975 ; y = qnorm(0.975) = 1.96", col = "orange")

points(0.025,qnorm(0.025), pch = 19, col = "orange", cex = 2)
text(0.22,-1.75, "x = 0.025 ; y = qnorm(0.025) = -1.96", col = "orange")

# Histogramme
library(plotrix)

a = rnorm(1000,10,10)
hist(a, col = "lightgray", main = "Histogramme de rnorm(n = 1000, mean = 10, sd = 10)", xlab = "Valeurs possibles pour une moyenne 10 de std dev ± 10", ylab = "Fréquence sur 1000 essais")

abline(v = mean(a), col = "red", lwd = 3)
abline(v =  mean(a) - std.error(a) * 1.96, col = "red", lwd = 2, lty = 3)
abline(v =  mean(a) + std.error(a)* 1.96, col = "red", lwd = 2, lty = 3)

legend("topright", col = "red", lty = c(1,2),lwd = c(3,2), legend = c("Mean","std.err * 1.96 = IC 95%"))
