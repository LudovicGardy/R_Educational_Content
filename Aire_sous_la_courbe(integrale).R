# Graph
#---------
plot_aire_graph = function(a, b, xlimit, ylimit, integralvalue = NA) {
  
  # Je definis une fonction f(x) = x + 1 
  x = xlimit
  fx = x + 1
  
  # Je definis l'aire qui m'interesse
  xa = a
  xb = b
  
  ya = xa + 1
  yb = xb + 1
  
  # Je fais un graphe
  #plot(x,fx, type = "l", ylim = ylimit, lwd = 3, col = "red", main = paste("Calcul d'integrale = ", integralvalue))
  par(mar=c(5,5,6,3))
  
  plot(x,fx, type = "l", ylim = ylimit, lwd = 3, col = "red", ylab = "f(x)") 

  title2<-paste("Resultat = ",integralvalue)
  title2<-list(title2,col="darkolivegreen4",cex=1.2)
  title(main=title2,line=2)
  title1<-expression(paste("f(x) = x + 1                        ",integral()["a"]^"b", "(",italic("x"), " + 1) ", italic("dx"), ".                        F(x) = 1/2 x"^2," + x. ",sep=""))
  title1<-list(title1,col="black",cex=1)
  title(main=title1,line=4)

  abline(h = 0, lwd = 2)
  abline(v = 0, lwd = 2)
  abline(v = xa, lty = 2, col = "purple")
  abline(v = xb, lty = 2, col = "purple")
  
  # Je represente l'aire a calculer sur le graphe
  x_axis = xa:xb
  Limit_Sup = x_axis + 1
  Limit_Inf = rep(0, length(Limit_Sup))
  mycolor = adjustcolor("darkolivegreen4",alpha.f=0.5)
  polygon(c(x_axis,rev(x_axis)),c(Limit_Sup,rev(Limit_Inf)),    
          col=mycolor, border=NA)

  text(xa - 8,ya - 15 , paste("a = ", a), col = "purple")
  text(xb + 8,yb - 6, paste("b = ", b), col = "purple")
  text(xb - 9, yb - 20, expression(paste(integral()["a"]^"b", italic("f(x)dx"),sep="")), col = "black")
  
}

# Algebere
#-----------

# Calcul de la fonction primitive
fonction_primitive = function(x) {
  
  # L'integrale entre -2 et 4 de f(x) = x + 1 dx est egale a F(4) - F(-2)
  # Cette integrale se note S [indice -2, exposant 4]
  
  # Nous devons trouver la primitive de chacun des termes de 
  # la fonction dont nous voulons connaitre l'integrale.
  # Dans le cas present nous avons 2 termes : "x" et "1".
  
  # Nous savons grace au tableau des primitives que la primitive
  # de x^2 est egale a 2 * x. Donc la primitive de x est...
  primitive_x = 1/2 * x^2 # ou = x^2 / 2
  
  # Nous savons grace au tableau des primitives que la primitive
  # de n est egale a n * x, ici 1 * x, soit x.
  primitive_1 = x
  
  fonction_primitive = primitive_x + primitive_1
  
  return(fonction_primitive)
  
}

# Calcul de l'integrale
calcul_integrale = function(primitive_a, primitive_b) {
  
  integrale = Fx_b - Fx_a
  
  return(integrale)
  
}

# Run script
#------------
a = -15
b = 30

Fx_a = fonction_primitive(a)
Fx_b = fonction_primitive(b)
integrale = calcul_integrale(Fx_a,Fx_b)
plot_aire_graph(a, b, xlimit = c(-50,50), ylimit = c(-50,50), integralvalue = integrale)
print(integrale)

