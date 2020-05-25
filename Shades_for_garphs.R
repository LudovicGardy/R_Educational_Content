# By Ludovic G.

#================================#
# Exemple principale : Exemple 1 #
#================================#
#= Creation des donnees =#
x_axis = c(1,2,3,4,5)
Limite_Sup = c(50,55,60,75,100)
Limite_Inf = c(30,35,40,55,80)
LigneCentrale = c(40,45,50,65,90)

#= Plot shades uniquement =#
plot ( Limite_Inf~ x_axis, type="n", axes=FALSE, ann=FALSE, ylim = c(0,105))
#lines(LigneCentrale,lwd = 3,col = "darkgreen")

macouleur = adjustcolor("green3",alpha.f=0.3)
polygon(c(x_axis,rev(x_axis)),c(Limite_Sup,rev(Limite_Inf)),    
        col=macouleur, border=NA)

#= Plot 2 sans shades =#
plot(x_axis,Limite_Inf,type = "l",ylim = c(0,105))
lines(LigneCentrale,lwd = 3,col = "pink3")
lines(x_axis, Limite_Sup)
#= Ajoute shades =#
macouleur = adjustcolor("deeppink",alpha.f=0.3)

polygon(c(x_axis,rev(x_axis)),c(Limite_Sup,rev(Limite_Inf)),    
        col=macouleur, border=NA)


#=================#
# Autres exemples #
#=================#
#-------------#
#  Exemple 2  #
#-------------#
cord.x <- c(-3,seq(-3,-2,0.01),-2) 
cord.y <- c(0,dnorm(seq(-3,-2,0.01)),0) 
curve(dnorm(x,0,1),xlim=c(-3,3),main='Standard Normal')
polygon(cord.x,cord.y,col='skyblue')

#-------------#
#  Exemple 3  #
#-------------#
x=seq(0,30)
y1=exp(-0.1*x)
plot(x,y1,type="l",lwd=2,col="chocolate1")
# Sur la courbe
polygon(x,y1, angle = 45,col="red")
# Sous la courbe
polygon(c(min(x),x),c(min(y1),y1), density = 5, angle = 45,col="chocolate1")

#-------------#
#  Exemple 4  #
#-------------#
x=seq(-7,10,length=200)
y1=dnorm(x,mean=0,sd=1)
plot(x,y1,type="l",lwd=2,col="red")
y2=dnorm(x,mean=3,sd=2)
lines(x,y2,type="l",lwd=2,col="blue")
polygon(x,pmin(y1,y2),col="gray")

#-----------------------#
#  Exemple 5 (ggplot2)  #
#-----------------------#
library(ggplot2)
x  = seq(-7, 10, length = 200)
y1 = dnorm(x, mean = 0,sd = 1)
y2 = dnorm(x, mean = 3,sd = 2)

mydf = data.frame(x, y1, y2)

p0 = ggplot(mydf, aes(x = x)) +                         
  geom_line(aes(y = y1), colour = 'blue') +
  geom_line(aes(y = y2), colour = 'red') +
  geom_area(aes(y = pmin(y1, y2)), fill = 'gray60')

p0

#-------------#
#  Exemple 6  #
#-------------#
# Create data for the area to shade
cord.x <- c(-3,seq(-3,-1,0.01),-1) 
cord.y <- c(0,dnorm(seq(-3,-1,0.01)),0) 
# Make a curve
curve(dnorm(x,0,1), xlim=c(-3,3), main='Standard Normal') 
# Add the shaded area.
polygon(cord.x,cord.y,col='skyblue')

#-------------#
#  Exemple 7  #
#-------------#
x <- seq(-3,3,0.01)
y1 <- dnorm(x,0,1)
y2 <- 0.5*dnorm(x,0,1)

plot(x,y1,type="l",bty="L",xlab="X",ylab="dnorm(X)")
points(x,y2,type="l",col="red")
polygon(c(x,rev(x)),c(y2,rev(y1)),col="skyblue")

#-------------#
#  Exemple 8  #
#-------------#
x <- seq(-3,3,0.01)
y1 <- dnorm(x,0,1)
y2 <- 0.5*dnorm(x,0,1)
x.shade <- seq(-2,1,0.01)

polygon(c(x.shade,rev(x.shade)),c(dnorm(x.shade,0,1),0.5*dnorm(rev(x.shade),0,1)),col="yellow")

#-------------#
#  Exemple 9  #
#-------------#
x <- seq(-3,3,0.01)
y1 <- dnorm(x,0,1)
y2 <- 0.5*dnorm(x,0,1)
x.shade <- seq(-2,1,0.01)
par(oma=c(1,1,1,1),cex=0.7)

plot(x,y1,type="l",bty="L",xlab="X",ylab="dnorm(X)")
points(x,y2,type="l",col="gray")
l <- length(x.shade)
color <- heat.colors(l)
for (i in 1:l)
{
  polygon(c(x.shade[i],rev(x.shade[i])),c(dnorm(x.shade[i],0,1),
                                          0.5*dnorm(rev(x.shade[i]),0,1)),border=color[i],col=NA)
}

#--------------#
#  Exemple 10  #
#--------------#
xx <- c(1:50)
yy <- rnorm(50)
n <- 50
hline <- 0
plot (yy ~ xx, type="n", axes=FALSE, ann=FALSE)
text(x=xx,y=min(yy)+max(yy),labels='a')
polygon(c(xx[1], xx, xx[n]), c(min(yy), yy, min(yy)),    
        col=rgb(1, 0, 0,0.5), border=NA)
# another convenient possibility is something like adjustcolor("red",alpha.f=0.5)
