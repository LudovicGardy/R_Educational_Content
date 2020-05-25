# By Ludovic G.

###=========================================###
###          Centrer un modele              ###
###=========================================###

par(mfrow = c(1,2))

#=========#
# Model 1 #
#=========#
y = 1:20
x = y^2 + 25

model = lm(y ~ x)
summary(model)

plot(y ~ x,xlim = c(0,400),col = "yellow",pch = 16, main = "Model non-centré")
abline(model,col = "red")
abline(h = coef(model)[1], col = "black",lty =3)
abline(v = 0, col = "black",lty =3)
abline(v = mean(x), col = "darkgray", lty = 3)
abline(h = 10.5, col = "darkgray", lty = 3) # valeur de y quand x est a sa moyenne.

## Quand x = 0 ; y = 2.9. Or il se peut que x ne soit jamais = a 0, et donc cette valeur de y n'a aucun sens. C'est
#- le cas ici, dans les valeurs que j'ai simulees, x ne sera jamais egal a 0, il est au minimum = a 25. Il me 
#- faut donc recentrer le modele pour avoir des comparaisons coherantes.

#=========#
# Model 2 #
#=========#
# Pour centrer le modele, je soustrait la moyenne de x a chaque valeur de x.
x.c = x - mean(x)

model2 = lm(y ~ x.c)
summary(model2)

plot(y ~ x.c,xlim = c(-200,200),ylim = c(0,30),col = "green",pch = 16, main = "Model centré")
abline(model2,col = "red")
abline(h = coef(model2)[1], col = "black",lty =3)
abline(v = 0, col = "black",lty =3)
text(100,8,"Intercept = valeur y \n quand x = mean(x)",col  = "black",cex = 0.8)

## Quand x est a sa valeur moyenne soit mean(x) soit dans ce cas 168.5; y = 10.5. Ce qui a potentiellement 
#- plus de sens. Attention, sur le graph il est marque y = 10.5 pour x = 0 mais c'est normal, c'est juste que
#- le model ne sait pas que l'on a centre les valeurs ; donc pour lui c'est x = 0, mais comme on a centre les valeurs
#- autour de la moyenne, on sait que l'intercept = moyenne de x.


# Note generale: on remarque que l'intercept change, mais bien sur la pente, elle, reste identique.