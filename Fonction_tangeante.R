x <- seq(-3, 7, by = 1/80)
tx <- cbind(x, cos(pi*x), cospi(x), sin(pi*x), sinpi(x),
               tan(pi*x), tanpi(x), deparse.level=2)
op <- options(digits = 4, width = 90) # for nice formatting
head(tx)
tx[ (x %% 1) %in% c(0, 0.5) ,]
options(op)

a = -5
b = 5

x = seq(a,b, by = 0.01)
y = tanpi(x)
plot(y~x, lwd = 2, type = "l")




