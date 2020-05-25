# By Ludovic G.

data(iris)
str(iris)

library(ggplot2)

#--------------#
# Model simple #
#--------------#
model = lm(iris$Sepal.Length ~ iris$Petal.Length)
par(mfrow = c(2,2))
plot(model)
summary(model)

par(mfrow = c(1,1))
plot(iris$Sepal.Length ~ iris$Petal.Length, pch = 19, col = "blue", cex = 1, xlim = c(-1,8))
ma_legend = as.factor(iris$Species)
grid(col="lightgray", lty = "dotted")

abline(model, lwd = 2, col ="blue")

abline(h = model$coefficients[1], lty = 2, col = "red")
abline(v = 0, lty = 2, col = "red")

#-----------------------------#
# Model 2 VI sans interaction #
#-----------------------------#
model2 = lm(iris$Sepal.Length ~ iris$Petal.Length + iris$Species) # Est ce que la longueur du petale et l'espece ont une influence sur la longueur du sepal?
# Intercept : valeur de sepal_length setosa quand petal_length = 0
summary(model2)

# Calcul intercepts
intercept_setosa = 3.68353
intercept_versicolor = 3.68353 + (- 1.60097)
intercept_virginica = 3.68533 + (- 2.11767)

# Calcul pente
pente = 0.90456

# Representation graphique des droites de regression
abline(a = intercept_setosa, b = pente, col = "black", lwd = 2, lty = 2)
abline(a = intercept_versicolor, b = pente, col = "red", lwd = 2, lty = 2)
abline(a = intercept_virginica, b = pente, col = "green", lwd = 2, lty = 2)

points(iris$Sepal.Length ~ iris$Petal.Length, pch = 19, col = iris$Species, cex = 1)
legend("topleft", legend = levels(ma_legend), col =  c("black","green","red"), pch = 19, cex = 1)

#-----------------------------#
# Model 2 VI avec interaction #
#-----------------------------#
model3 = lm(iris$Sepal.Length ~ iris$Petal.Length * iris$Species) # * on demande l'interaction 
summary(model3)

# Calcul intercepts
intercept_setosa = 4.2132
intercept_versicolor = 4.2132 + (- 1.8056)
intercept_virginica = 4.2132 + (- 3.1535)

# Calcul pente
pente_setosa = 0.5423
pente_versicolor = 0.5423 + 0.2860
pente_virginica = 0.5423 + 0.4534

# Representation graphique des droties de regression
plot(iris$Sepal.Length ~ iris$Petal.Length, pch = 19, col = "blue", cex = 1, xlim = c(0,10), ylim = c(0,10))

points(iris$Sepal.Length ~ iris$Petal.Length, pch = 19, col = iris$Species, cex = 1)
legend("topleft", legend = levels(ma_legend), col =  c("black","green","red"), pch = 19, cex = 1)

abline(a = intercept_setosa, b = pente_setosa, col = "black", lwd = 2, lty = 2)
abline(a = intercept_versicolor, b = pente_versicolor, col = "red", lwd = 2, lty = 2)
abline(a = intercept_virginica, b = pente_virginica, col = "green", lwd = 2, lty = 2)

# Changement niveau de reference du facteur species:
# iris$Species = relevel(iris$Factor,"virginica")


# Representation differente de la meme chose
ggplot(data = iris, aes(x = iris$Petal.Length, y = iris$Sepal.Length, colour = iris$Species)) +
	geom_smooth(method = "lm")
