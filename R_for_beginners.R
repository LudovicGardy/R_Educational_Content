# By Ludovic G.

rm(list = ls()) # clean the workspace
clc = function() {cat(rep("\n",100))} # clean la console (creation fonction)
clc() # clean la console (utilisation fonction)


###=================================================###
###                  1. LOAD DATA                   ###
###=================================================###

# Lire un .csv
tr = read.csv("/here/is/the/path/FileName.csv",h=T,sep=";")

# Lire un .txt
DataImportTXT = read.table("/here/is/the/path/FileName.txt", sep="\t", header=TRUE)  

# Lire un .xls
library(Rcmdr)
DataPatients <- readXL("/here/is/the/path/FileName.xlsx",
                       rownames=FALSE, header=TRUE, na="", sheet="SheetName", 
                       stringsAsFactors=TRUE)

# Rassembler les 2 dataframes
merge(x = DataImportCSV, y = DataImportTXT)

###=================================================###
###                  2. WRITE DATA                  ###
###=================================================###

# Ficher excel
library("xlsx")

write.xlsx(distMoyAD0, file = "/here/is/the/path/FileName.xlsx",
           sheetName = "blabla", append = FALSE)

# Fichier texte
write.table(frameCompT0AD, file = "/here/is/the/path/FileName.txt", sep = ";",
            dec = ".", col.names = TRUE, row.names = FALSE )


###==========================================###
###               3. VECTEURS                ###
###==========================================###

vecteur  = c(1:100)
vecteur2 = rep(0:100)

# Sort : classe les valeurs par ordre croissant ou decroissant
sort(vecteur2)        
sort(vecteur2,decr=T)  

# Donner un nom aux valeurs d'un vecteur 
ranks = 1 : 3
  # J'attribue les noms
  names(ranks) = c("first", "second", "third") 
  # Les noms apparaissent sur le plot
  barplot(ranks)                                

# Transformer un vecteur en matrice 
plank = 1 : 8
dim(plank) = c(2,4)


###=======================###
###    4. SEGMENTATION    ###
###=======================###

limiteSup = which(vecteur2 > 57) ## Me donne les positions du vecteur2 contenant des valeurs
                                 #- superieures a 57.

limiteInf = which(vecteur2 < 90)  ## Meme chose dans l'autre sens.

vecteurNouveau = vecteur2[57:90] # Cree un vecteur avec l'intervalle demande de l'ancien vecteur.

vecteur>50 & vecteur <70         # Endroits du vecteurs rÃ©pondant a ces 2 criteres.

range(vecteur2)                  ## Donne la plus petite et la plus grande valeur contenue dans 
                                 #- le vecteur.

which(vecteur2==max(vecteur2))   # Donne la position du vecteur contenant la valeur maximale.
which(vecteur2==min(vecteur2))   # Pareil dans l'autre sens.

max(vecteur2[60:80])  # Donne la valeur maximale contenue dans l'espace 60 a 80 du vecteur. Nous donne 6400.
which(vecteur2==6400) # Donne l'emplacement dans le vecteur oÃ¹ cette valeur est atteinte.


###======================================###
###              5. TABLE                ###
###======================================###

# Creer un tableau rapidement.Attention, ceci n'est pas un dataframe.
M <- as.table(rbind(c(76, 32, 46), c(48,23,47), c(45,34,78)))
dimnames(M) <- list(sex=c("Male","Female","Juv"),loc=c("Lower","Middle","Upper"))
print(M)

###=====================================###
###             6. MATRICES             ###
###=====================================###

# Creer une matrice
matrix <- matrix(1,100,nrow=20,ncol=5)

# Mettre la matrice au cube
for (auCube in 1 : 100) {
  matrix[auCube] = vecteur[auCube]^3
}

# Connaitre les dimensions d'une matrice
dim(matrix)

# Noms lignes/col
rownames(matrix) = letters[1:20] # Donne un nom aux lignes de ma matrice.
colnames(matrix) = LETTERS[1:5]  # Donne un nom aux colonnes.

# Moyennes ligne/col au choix
mean(matrix[,1])                 # Donne la moyenne de la premiee colonne.
mean(matrix[1,])                 # Donne la moyenne de la premiee ligne.

# Moyennes toutes lignes/col
apply(matrix,1,mean)             # Moyenne pour chaque ligne.
apply(matrix,2,mean)             # Moyenne pour chaque colonne.

# Copie la premiere matrice dans une seconde
matrix2 = matrix 

# Transpose les lignes en colonnes
matrix3 = t(matrix2)             

# Supprime la 3eme colonne de la matrice
matrix3 = matrix[, -3]          

## Order : Classe par ordre croissant la colonne 2 en gardant la correspondance 
#- avec les autres colonnes.
matrix3 = matrix3[order(matrix3[,2],decreasing=F), ]   

# Inserer/remplacer des elements dans une matrice (ici je remplace des valeurs par 0)
matrix3[c(1,3,5,7,9),c(3,4)] = 0

# Remplace tous les 0 par NA 
matrix3[matrix3==0] = NA     

# Remplace tous les NA par 0
matrix3 = replace(matrix3,is.na(matrix3),0)


###======================================###
###             7. Factors               ###
###======================================###

# Importe le jeu de donnees Iris disponible de base dans R.
data(iris)

# Connaitre les niveaux d'un facteur, ici le facteur "Species"
print(levels(iris$Species))

# Modifier les niveaux du facteur
iris$Species <- relevel(iris$Species, "versicolor")
print(levels(iris$Species))

# Creer une chaine de caractere puis la transformer en facteurs
chests <- c('gold', 'silver', 'gems', 'gold', 'gems')
types <- factor(chests)
print(chests)
print(types)
# """Printed at the bottom, you'll see the factor's "levels" - groups of unique values. 
# Notice also that there are no quotes around the values. That's because they're not 
# strings; they're actually integer references to one of the factor's levels."""

# Changer les facteurs de type "caractères" en nombres sans changer les niveaux du facteur
as.integer(types)
levels(types)

# Tracer un graph prenant en compte les facteurs pour differencier les points sur le graph
weights <- c(300, 200, 100, 250, 150)
prices <- c(9000, 5000, 12000, 7500, 18000)
plot(weights, prices)
plot(weights, prices, pch=1:length(levels(types)), cex = 1.5, col = as.integer(types))
legend("topright", levels(types), pch=1:length(levels(types)),col = as.integer(types))

###======================================###
###            8. DATA.FRAME             ###
###======================================###

# Creer un dataframe
df <- data.frame(matrix(6,100,nrow=20,ncol=5))

# Cree un nouveau dataframe qui correspond au premier au cube
df2 = df

for (auCube in 1 : length(df)) {
  df2[,auCube] = df[,auCube]^3
}


# Fusionner des data frames
# Par lignes
newFrame = rbind(df,df2)

# Par colonnes
newFrame = cbind(df,df2)

# Supprimer plusieurs lignes
newFrame = newFrame[-c(2,4,5,12),]

# Passe toutes les lignes d'une colonne en caractere
str(newFrame[,1])
newFrame[,1] = sapply(newFrame[,1],as.character)  
str(newFrame[,1])

# Changer les noms de colonnes / lignes
colnames(newFrame)[c(1,2)] = c("MyColName1","MyColName2")
rownames(newFrame)[c(1,2)] = c("MyRowName1","MyRowName2")

# Changer des valeurs dans une colonne
newFrame$MyColName1[5:15] = 300
newFrame$MyColName2[2:6] = 4


# Creer un subset de mon tableau
my_subset = subset(newFrame,newFrame$MyColName1 == 300)

# Subset a plusieurs conditons
my_subset = newFrame[(newFrame$MyColName1 == 300) & (newFrame$MyColName2 <5),]

# Acces aux donnees dans le frame 
print(newFrame[,2])                                              # On accede a la colonne 2.
print(newFrame[["MyColName2"]])                                     # Idem.
print(newFrame$MyColName2)                                          # Idem.

# Creations de vecteurs puis assemblement en dataframe
vecteurExTable = c("homme","femme","homme","homme","femme","femme","femme","femme")
vecteurExTable2 = c("ouvrier","ouvrier","directeur","cadre","ouvrier","ouvrier","chef","ouvrier")
vecteurExTable3 = c("roux","blond","blond","brun","brun","blond","brun","brun")

# Creation du data frame
frameExTable = data.frame(vecteurExTable,vecteurExTable2,vecteurExTable3)
  
# Resume simplement le data frame
table(frameExTable[,1])

# Combien de valeurs sont vraies pour "femme" ?
table(frameExTable[,1] == "femme")          # (TRUE/FALSE)
summary(frameExTable[,1] == "femme")        # (TRUE/FALSE/NA)
  
# Combien de femmes sont ouvrier et/ou blond ? (TRUE/FALSE)
table(frameExTable[,1] == "femme" & (frameExTable[,2] == "ouvrier" | frameExTable[,3] == "blond"))
  
## Si femme + ouvrier = TRUE ; si femme + cadre = TRUE ; autre = FALSE.
#- L'operateur %in% teste si une valeur fait partie des elements dans un vecteur
frameExTable[,1] == "femme" & frameExTable[,2] %in% c("ouvrier", "cadre")

# Combien de femmes sont ouvrier ET blond ? (TRUE/FALSE)
table(frameExTable[,1] == "femme" & (frameExTable[,2] == "ouvrier" & frameExTable[,3] == "blond"))
  
# Creation d'nouveau frame qui est comme le precedent mais qui ne contient que les femmes
monNouveauExTable = frameExTable[frameExTable[,1] == "femme" & frameExTable[,3] == "blond",]
print(monNouveauExTable)

## Creation d'un nouveau frame qui est comme le precedent mais qui ne contient que les 
#- femmes blondes.
monNouveauExTable = frameExTable[frameExTable[,1] == "femme" & frameExTable[,3] == "blond",]
print(monNouveauExTable)
  
## Creation d'un nouveau frame qui est comme le precedent mais qui ne contient que les 
#- femmes blondes et ne conserve que les colonnes 1 et 2 (sexe et qualification).
monNouveauExTable = frameExTable[frameExTable[,1] == "femme" & frameExTable[,3] == "blond", c("vecteurExTable","vecteurExTable2")]
print(monNouveauExTable)

###=================================================###
###            9. FONCTIONS IMPORTANTES             ###
###=================================================###

# Installer des packages/libraries
install.packages( c("ggplot2" ,"lme4") , dependencies = TRUE )

# Importer des packages libraries
library("ggplot2")

summary(vecteur)

table(vecteur[,colonne])

sort(vecteur)

order(dataFrame)

set.seed(2) # fix the chance

~. # means all other data ; ex : model = lm

# Normaliser les valeurs de chaque colonne d'une matrice avec la fonction scale.
test = matrix(1:20,5,4)
print(test)
test_scale = scale(test)
print(test_scale)  
  
###====================###
###   10. Les Images   ###
###====================###

## Generer une image
help("image")
nbLignes = 10
nbCol = 10
matriceTest = matrix(0,nbLignes,nbCol)

for(i in 1:nbLignes) {
  for(j in 1:nbCol) {
    matriceTest[i,j] = runif(1) * 30
      
  }
}
  
image(matriceTest,col = heat.colors(12),useRaster=T)

setup.image.smooth(nrow = nbLignes, ncol = nbCol, dx = 1, dy = 1,
                   kernel.function = double.exp,
                   theta = 1, xwidth = nrow * dx, ywidth = ncol * dx, lambda=NULL, ...)

## La 3D
# Je dessine une plage en 2D dans laquelle je creuse un trou pour cacher un tresor
elevation = matrix(1,10,10)
elevation[4,6] = 0
contour(elevation) # Contour de la plage
persp(elevation)   # Perspective en 3D
persp(elevation,expand = 0.2)   # Visualisation en 3D

# Map d'un volcan, deja contenue dans R, matrice de 87*61
contour(volcano)
persp(volcano,expand = 0.2)
image(volcano)
  
###=====================###
###   11. Les graphes   ###
###=====================###
## lty = le type de ligne. Plusieurs lty on peut faire lty 1:2 ou 1:3..
#- lwd = taille ligne,
#- pwd = la taille de la ligne, 
#- cex = taille du texte ou plot.
#- cex.lab = taille des labels x et y.
#- cex.axis = taille de l'ecriture sur les axes x et y.
#- cex.main
#- cex.sub
#- pch = type de points .
#- \n sauter une ligne dans le titre
#- title(main = "main title", sub = "subtitle")

#------------------------------------------------------------------------------------#
# Polygon function (pour tracer un intervalle transparant sur une courbe apr exemple #
#------------------------------------------------------------------------------------#
x_axis = c(1,2,3,4,5)
Limite_Sup = c(50,55,60,75,100)
Limite_Inf = c(30,35,40,55,80)
LigneCentrale = c(40,45,50,65,90)

plot(x_axis,Limite_Inf,type = "l",ylim = c(0,105))
lines(LigneCentrale,lwd = 3,col = "pink3")
lines(x_axis, Limite_Sup)

macouleur = adjustcolor("deeppink",alpha.f=0.3)

polygon(c(x_axis,rev(x_axis)),c(Limite_Sup,rev(Limite_Inf)),    
        col=macouleur, border=NA)

#-----------------------------------------------------------------#
# Application au cas d'un modèle linéaire et droite de regression #
#-----------------------------------------------------------------#
x_model = 1:1500
y_model = x_model^2
plot(y_model ~ x_model, type = "l", lwd = 2, col = "orange")

# Definition du modele
model_centre = lm(y_model ~ x_model)

# Ligne centrale du modele (classique)
abline(model_centre, col = "red", lty = 2)
summary(model_centre)
confint(model_centre)
# IC 95% superieur
IC_sup = abline(confint(model_centre)[,2],col = "indianred", lty = 6)
# IC 95% inferieur
IC_inf = abline(confint(model_centre)[,1],col = "indianred", lty = 6)

# Polygone de l'IC 95%
x_axis = c(x_model, seq(max(x_model), max(x_model) *2), 1  ) 

# Limite superieure : calcul
Limite_Sup = confint(model_centre)[1,2]
for(k in 2:length(x_axis)) {
	
	Limite_Sup[k] = Limite_Sup[k-1] + confint(model_centre)[2,2]
	
}

# Limite inferieure : calcul
Limite_Inf = confint(model_centre)[1,1]

for(k in 2:length(x_axis)) {
	
	Limite_Inf[k] = Limite_Inf[k-1] + confint(model_centre)[2,1]
	
}

# Trace et rempli le polygone
macouleur = adjustcolor("indianred",alpha.f=0.3)

polygon(c(x_axis,rev(x_axis)),c(Limite_Sup,rev(Limite_Inf)),    
        col=macouleur, border=NA)

#-----------------------------#
# Les palettes de Couleurs... #
#-----------------------------#
# Chaleur
palette = heat.colors(12)
x = c(1:12)
plot(x, pch = 19,cex = 2,col = palette)

# Rainbow
palette = rainbow(12)
x = c(1:12)
plot(x, pch = 19,cex = 2,col = palette)

# Terre
palette = terrain.colors(12)
x = c(1:12)
plot(x, pch = 19,cex = 2,col = palette)

#----------------------------------------------------------#
# Ajouter une droite de regression (pour les correlations) #
#----------------------------------------------------------#
x = c(1,1,1,20,20,20,30,30,30,40,40,40)/25
y = c(1,3,25,10,24,30,20,140,160,70,210,250)/100
cor.test(x,y)  
plot(x,y)
droite_regression = lm(x~y)
abline(droite_regression,col = "red")

#----------------------------------------------------------------#
# Ajouter du bruitage pour pallier a la superposition des points #
#----------------------------------------------------------------#
x = c(1,1,1,2,2,2,3,3,3,4,4,4)
y = c(1,1,1,2,2,2,3,3,3,4,4,4)
plot(x, y, main = "Sans bruitage")
plot(jitter(x), jitter(y), main = "Avec bruitage")

#--------------------------------------------------#
# Controler/Modifier le nom de l'axe des abscisses #
#--------------------------------------------------#
x = c(1,1,1,2,2,2,3,3,3,4,4,4)
y = c(1,1,1,2,2,2,3,3,3,4,4,4)
plot(jitter(x), jitter(y), main = "Avec bruitage",
     xaxt = "n")
axis(1,at=c(1:5), labels=c("Lvl1","Lvl2","Lvl3","Lvl4","Lvl5"))

#---------------------------#
# Gestion d'un second axe y #
#---------------------------#
# je gere la marge pour afficher le nom du 2eme axe
par(mar=c(5,6,4,4) + 0.1)
# nouvelle fenetre par dessus l'autre
plot.window(xlim = c(0,6),ylim = c(330,500))
# 2eme axe
axis(4,col="red")
# nom de l'axe
mtext("Mon_Axe_2", side = 4, line =3, cex = par("cex.lab"))

#--------------------------#
# Visu de plusieurs graphs #
#--------------------------#
# 
nombreLignes = 1
nombresCol = 2
par(mfrow=c(nombreLignes,nombresCol)) # ou
par(mfrow=c(1,2))

plot(x, y, main = "Sans bruitage")
plot(jitter(x), jitter(y), main = "Avec bruitage")

# revenir a 1 graph
par(mfrow = c(1,1))
plot(jitter(x), jitter(y), main = "Avec bruitage")

#---------------------#
# Ajout d'une legende #
#---------------------#
legend(3, 4, legend=c("Patients Alzheimers", "Sujets controles"),    
      col=c("red", "blue"), lty=1, cex=0.8)

# Legende avec pointeur souris
#legend(locator(1),c("mean Diff +/- 95%CI"), col = 1, pch = 1, cex = 1.2)

# Legende plus en details 
curve( x^2/2
       , from = 0
       , to = 100
       , n = 30
       , type = "p"
       , pch = 21 # alternatively pch=15 is a solid symbol
       , col = "red" # colors the outline of hollow symbol pch=21
       , bg = "red" # fills hollow symbol pch=21 with color
       , xlab = "x"
       , ylab = "y"
)
curve( (100^2-x^2)/2
       , from = 0
       , to = 100
       , n = 30
       , type = "p"
       , pch = 22  # alternative pch=16
       , col = "blue"
       , bg = "blue"
       , add = TRUE
)
legend( "topleft"
        , inset = c(0,0.4), 
        , cex = 1.5, 
        , bty = "n", 
        , legend = c("A", "B"), 
        , text.col = c("red", "blue"),
        , col = c("red", "blue"), 
        , pt.bg = c("red","blue")
        , pch = c(21,22)
)

#----------#
# Le qplot #
#----------#
library(ggplot2)
weights <- c(300, 200, 100, 250, 150)
prices <- c(9000, 5000, 12000, 7500, 18000)
chests <- c('gold', 'silver', 'gems', 'gold', 'gems')
types <- factor(chests)

qplot(weights, prices, color=types)

#------------#
# Le Vioplot #
#------------#
#resume l'histogramme et le boxplot - Apprendre a l'utiliser
library(vioplot)
vioplot(x, y,names=c('x', 'y'), col='yellow')

#-------------------------------------------------#
# Plot intervals des confiance ou erreur standard #
#-------------------------------------------------#
library(plotrix)
data(iris)

#== Exemple 1 ==#
x = c(1,2)        # arbitrary position of the two boxplots on the x-axis

plotCI(x, c(mean(iris$Sepal.Length), mean(iris$Sepal.Width)), 
       uiw=c(std.error(iris$Sepal.Length),std.error(iris$Sepal.Width)),    # width of the whiskers
       xlim = c(0.5,2.5), #ylim = c(12,15),
       sfrac = 0.15,                          # half width of the error-bar
       cex = 1.5,                             # scaling factor of the point
       cex.axis = 1.2,
       cex.lab = 1.5,
       xlab='lab x', ylab = 'lab y',
       xaxt = "n")                 

#== Exemple 2:  a partir d'un modele lineaire a 2VI sans interaction ==#
model = lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)

plotCI(-(model$coefficients[3]), 
       ui = -(confint(model)[3,1]),    
       li = -(confint(model)[3,2]),
       #xlim = c(0.5,2.5), #ylim = c(12,15),
       ylim = c(-1,1),
       sfrac = 0.10,                          
       cex = 1.5,                             
       cex.axis = 1.2,
       cex.lab = 1.5,
       xlab='group', ylab = 'My Measure',
       xaxt = "n",
       pch=16,
       col="orange",
       lwd=2)          

abline(h = 0, lty = 2,col = "red")    

#---------------------------------------------------------#
# Dotchart (exemple) Pour identifier des valeurs extremes #
#---------------------------------------------------------#
dotchart(iris$Sepal.Length, groups= iris$Species, 
         xlab= "My measure VD",
         pch= 16, #Choose symbol for the points
         col= ifelse(iris$Species == "versicolor", "red", "darkblue")) # L'espece de la fleur determine la couleur

#--------#
# Coplot #
#--------#
coplot(Sepal.Width  ~ Sepal.Length|Species, data= iris)

#----------------------------------#
# Line plots avec barres d'erreurs #
#----------------------------------#
# Solution 1
library(plotrix)
x = c(1,2,3,4); y = c(4,5,6,7); err = c(0.4,1,0.8,0.3)
plotCI(x, y, uiw=err,gap=0) # je peux aussi utiliser ui et li comme limites sup et inf des barres d'erreur (au lieu de uiw)
#Ceci dit, le uiw fait mean Â± s.e.

# Solution 2
x = c(1,2,3,4); y = c(4,5,6,7); err = c(0.4,1,0.8,0.3)
plot(x, y, type="l", lty=3,ylim=c(0,10))
plotCI(x,y, uiw=err, lwd = 2,gap=0,pch=16,add=T)

#------------------------------------------------------------#
# Boxplot avec valeurs individuelles par dessus : Stripchart #
#------------------------------------------------------------#
x = 1:100
boxplot(x , outpch = NA, notch = T, col = "yellow")
stripchart(x ,
           vertical = TRUE, method = "jitter",
           pch = 21, col = "red", bg = "green",
           add = TRUE, cex = 2)

#--------------------#
# Gestion des marges #
#--------------------#
#par(oma = rep(3,4) + 0.1, bg = "white") # rep(3,3,3,3) correspond a marges bas, gauche, haut, droit
#plot.new()
#
#for(i in 1:4) {
#  
#  mtext(paste("Marge exterieure",i), side = i,
#        line = 1, adj = 0.5, outer = T)
#  
#  for(j in 0:2) {
#    mtext(paste("ligne",i), side = i, line = j + 0.1,
#          adj = 0, cex = 0.5, outer = T)
#    
#  }
#  
#}
#
## Pour reinitialiser les parametres
#op = par(no.readonly = T)
#par(op)

###============================================================###
###  12. UTILISER UN VECTEUR CREE DANS UNE FONCTION (return)   ###
###============================================================###

mafonction = function(monvecteur,x,y) {
  monvecteur = rep(0,2)
  monvecteur[1] = 5
  
  for (k in x:y) {
    monvecteur[k+1] = monvecteur[k]^2
  }
  
  return(monvecteur)
  
}

a = mafonction(a,1,50)


###========================###
###    13. STATISTIQUES    ###
###========================###
data(iris)

# Egalite des variances
var.test(iris$Sepal.Length,iris$Petal.Length)

# Moyenne d'une colonne d'un dataframe sans prendre en compte les NA
with(iris,mean(Sepal.Length,na.rm=TRUE))

# Standard deviation
maStandardDeviationR = sd(iris$Sepal.Length)               

# Standard Deviation A la main. (= ECART TYPE)
maStandardDeviationManuelle = sqrt(var(iris$Sepal.Length)) 

# Erreur standard (SEM) - Solution 1
maStandardError1 = sqrt(var(iris$Sepal.Length)/length(iris$Sepal.Length))   
# Erreur standard (SEM) - Solution 2
maStandardError2 = (sd(iris$Sepal.Length)/sqrt(length(iris$Sepal.Length)))  

# Pour un IC a 95% : # On pourra alors noter : Intervalle de confiance = Moyenne ± SEM.IC95
# SOlution 1
IC95_inf = qt(.025,length(iris$Sepal.Length)-1)*sqrt(var(iris$Sepal.Length)/length(iris$Sepal.Length)) # SEM IC95%
IC95_sup = qt(.975,length(iris$Sepal.Length)-1)*sqrt(var(iris$Sepal.Length)/length(iris$Sepal.Length)) # SEM IC95%

# Solution 2
ValeurTableZ = qnorm(0.975) # soit environ 1.96 pour un IC95.
SEM.IC95aLaMain = ValeurTableZ * (maStandardDeviationR/sqrt(length(iris$Sepal.Length))) # soit 1.96 * std.err

# Tester la correlation entre 2 vecteurs (voir p.value)
# C'est un peu comme un test de chi2. Ca nous dit si les donnees sont independantes.
# Avec cette fonction on obtient aussi l'intervalle de confiance 95% du r.
r = cor.test(iris$Sepal.Length, iris$Petal.Length)

# Obtenir l'IC 95% d'un t.test
mont_test = t.test(iris$Sepal.Length)
mon_IC = mont_test$conf.int

# erreur standard
library(plotrix)
std.error(vecteur)

###==================================================###
###                  14. PROGRAMMATION               ###
###==================================================###
# <= inf ou egal
# >= sup ou egal
# != different
# == egal
# %>% pipe (library(magrittr))

# 1000 nombre aleatoires entre 0 et 100
x = runif(1000, min = 0, max = 100)

# Meme chose avec arrondi des valeurs
b = round(runif(1000,min=0,max=100),digits=2)

# Fixation du hasard pour resultats reproductibles ##
# Note : on peut mettre le chiffre que l'on veut entre parentheses
sed.seed(2) ; sample(1:10)
sed.seed(2) ; sample(1:10)

#--------------#
# Condition if #
#--------------#
if(0 != "NA"){ 
  print("yes 0 is different from NA")
  }
  
# Double Condition
if((0 != "NA") && ("a" == "a")) {
  print("This is true again !")
}

#--------------#
# Boucles for  #
#--------------#
# Assigner nouveaux noms a des donnees dans boucle for ##
#-> Voici un exemple, il faut utiliser la fonction "assign", qui peut aussi faire appel a un vecteur contenant les noms au lieu d'une generation aleatoire.
for(i in 1:10) { 
  variable_name <- paste("Example_", i, sep = "") # (A) Cree une chaine de caracteres 
  nombre_hasard = rnorm(3) + 5 # (B) Genere un nombre au hasard
  assign(variable_name, nombre_hasard) # (C) assigne (B) a (A)
}  
  
# Le break permet d'arreter la boucle, exemple ici des que j'atteints
# la condition else, je commande d'arreter la boucle.
a = c(1,2,3,4,10,4,5,6,2,1)  
for(k in 1 : length(a)) {
  if(a[k]<10) { 
    a[k] = 0 
  } else {
    break()}
}
print(a)

# Tirer des nombres dans une loi gaussienne
# Je tire 10 valeurs dans une gaussienne, centrees autour d'une moyenne de 450 avec un ecart type de 20
rnorm(10, 450, 20)

###==============================================###
### Restore Default graphical parameters (reset) ###
###==============================================###
# reset_par <- function(){
#     op <- structure(list(xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE,
#                          ask = FALSE, bg = "transparent", bty = "o", cex = 1, cex.axis = 1,
#                          cex.lab = 1, cex.main = 1.2, cex.sub = 1, col = "black",
#                          col.axis = "black", col.lab = "black", col.main = "black",
#                          col.sub = "black", crt = 0, err = 0L, family = "", fg = "black",
#                          fig = c(0, 1, 0, 1), fin = c(6.99999895833333, 6.99999895833333
#                          ), font = 1L, font.axis = 1L, font.lab = 1L, font.main = 2L,
#                          font.sub = 1L, lab = c(5L, 5L, 7L), las = 0L, lend = "round",
#                          lheight = 1, ljoin = "round", lmitre = 10, lty = "solid",
#                          lwd = 1, mai = c(1.02, 0.82, 0.82, 0.42), mar = c(5.1, 4.1,
#                                                                            4.1, 2.1), mex = 1, mfcol = c(1L, 1L), mfg = c(1L, 1L, 1L,
#                                                                                                                           1L), mfrow = c(1L, 1L), mgp = c(3, 1, 0), mkh = 0.001, new = FALSE,
#                          oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), omi = c(0, 0, 0,
#                                                                            0), pch = 1L, pin = c(5.75999895833333, 5.15999895833333),
#                          plt = c(0.117142874574832, 0.939999991071427, 0.145714307397962,
#                                  0.882857125425167), ps = 12L, pty = "m", smo = 1, srt = 0,
#                          tck = NA_real_, tcl = -0.5, usr = c(0.568, 1.432, 0.568,
#                                                              1.432), xaxp = c(0.6, 1.4, 4), xaxs = "r", xaxt = "s", xpd = FALSE,
#                          yaxp = c(0.6, 1.4, 4), yaxs = "r", yaxt = "s", ylbias = 0.2), .Names = c("xlog",
#                                                                                                   "ylog", "adj", "ann", "ask", "bg", "bty", "cex", "cex.axis",
#                                                                                                   "cex.lab", "cex.main", "cex.sub", "col", "col.axis", "col.lab",
#                                                                                                   "col.main", "col.sub", "crt", "err", "family", "fg", "fig", "fin",
#                                                                                                   "font", "font.axis", "font.lab", "font.main", "font.sub", "lab",
#                                                                                                   "las", "lend", "lheight", "ljoin", "lmitre", "lty", "lwd", "mai",
#                                                                                                   "mar", "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh", "new", "oma",
#                                                                                                   "omd", "omi", "pch", "pin", "plt", "ps", "pty", "smo", "srt",
#                                                                                                   "tck", "tcl", "usr", "xaxp", "xaxs", "xaxt", "xpd", "yaxp", "yaxs",
#                                                                                                   "yaxt", "ylbias"))
#     par(op)
#   }
#   
# reset_par()
