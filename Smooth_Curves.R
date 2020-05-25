# By Ludovic G.

###=========================================###
###            Make smooth curves           ###
###=========================================###

#===========#
# Exemple 1 #
#===========#
x <- 1:10
y <- c(2,4,6,8,7,12,14,16,18,20)
lo <- loess(y~x)
plot(x,y)
lines(predict(lo), col='red', lwd=2)

#===========#
# Exemple 2 #
#===========#
smoothingSpline = smooth.spline(x, y, spar=0.35)
plot(x,y)
lines(smoothingSpline)

#===========#
# Exemple 3 #
#===========#
## This style interpolates lots of extra points and gets you a curve that is very smooth. It also appears 
#- to be the the approach that ggplot takes. If the standard level of smoothness is fine you can just use.
x <- 1:10
y <- c(2,4,6,8,7,8,14,16,18,20)
lo <- loess(y~x)
plot(x,y)
xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
lines(xl, predict(lo,xl), col='red', lwd=2)
#scatter.smooth(x, y)

#===========#
# Exemple 4 #
#===========#
cr <- colorRampPalette(col=c("red", "red", "red", "red"), bias=1)
linecols <- cr(3)
x<-c(-1000.000000,-900.000000,-800.000000,-700.000000,-600.000000,-500.000000,-400.000000,-300.000000,-200.000000,-100.000000,0.000000,100.000000,200.000000,300.000000,400.000000,500.000000,600.000000,700.000000,800.000000,900.000000,1000.000000)
y<-c(0.809524,1.000000,1.333333,1.333333,3.285714,7.761905,13.619048,7.571429,14.809524,3.904762,1.857143,2.285714,4.857143,8.571429,2.000000,1.523810,2.714286,0.857143,1.285714,0.857143,1.380952)
plot(x, y,type="l",main="Average",ylab="Average Profile",col=linecols[1],ylim=c(0.809524,14.809524),xaxt="s",yaxt="s",lwd=2)
lines(x, smooth(y))
lines(supsmu(x, y))
