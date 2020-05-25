# By Ludovic G.

###=========================================###
###           Anova + Tukey                 ###
###=========================================###

# Chargement et mise en forme des donnes
data(iris)
attach(iris)
library(ggplot2)

my_names = c("Species","Part","Measure","Value")
iris.tidy = data.frame(matrix(NA,600,4))
colnames(iris.tidy) = my_names
iris.tidy$Value = c(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width )
iris.tidy$Part = c(rep("Sepal", 300), rep("Petal", 300))
iris.tidy$Measure = c(rep("Length", 150), rep("Width", 150), rep("Length", 150), rep("Width", 150))
iris.tidy$Species = rep(iris$Species, 4)

str(iris.tidy)
head(iris.tidy)

# Value en fonction de chaque groupe et condition pour chaque measure
# Visualisation : 1. points
ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() +
  facet_grid(. ~ Measure)
  
# 2. boxplots  
ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_boxplot(notch = TRUE) +
  facet_grid(. ~ Measure)
        
# Modeles : y'a til une difference de longueur de petal en fonction de l'espece de fleur?
# A FAIRE :
model1 = aov(Value ~ Species, data = iris.tidy[iris.tidy$Measure == "Length" & iris.tidy$Part == "Petal",])
summary(model1)    
par(mfrow = c(2,2))
plot(model1)

par(mfrow = c(1,1))
TukeyHSD(model1)
plot(TukeyHSD(model1))

# NOTE :
# A NE PAS FAIRE : Le format .tidy peut etre un piege pour l'ecriture des modeles donc attention.
# model2 = aov(Value ~ Measure * Part * Species, data = iris.tidy) # WRONG
# summary(model1) # WRONG
# par(mfrow = c(2,2)) # WRONG
# plot(model1) # WRONG
# library(MuMIn) # Du coup pas la peine de faire ca...
# model.sel(model1,model2) # Idem.
# TukeyHSD(model1, which = "Species") # Idem.
# plot(TukeyHSD(model1, which = "Species")) # Idem.

