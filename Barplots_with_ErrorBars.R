# By Ludovic G.

#####################################################
#                                                   #
#        BARPLOT + BARRES ERREURS EXEMPLE           #
#                                                   #
#####################################################

A = c(1,3,4,5) ; B = c(3,5,6,3,4) ; C = c(7,6,4,7,5,4,4,4,4)
moyennes = c(mean(A),mean(B),mean(C))
#intervalles = c(((t.test(A)$conf.int[2]-t.test(A)$conf.int[1])/2),((t.test(B)$conf.int[2]-t.test(B)$conf.int[1])/2),((t.test(C)$conf.int[2]-t.test(C)$conf.int[1])/2))
intervalles2 = c((sd(A)/sqrt(length(A))),(sd(B)/sqrt(length(B))),(sd(C)/sqrt(length(C))))
bp = barplot(moyennes,ylim=c(0,10),col=c(2,3,4)); box()
arrows(bp,moyennes-intervalles2,bp, moyennes+intervalles2, lwd=1.5, angle=90,length=0.1,code=3) 

#####################################################
#                                                   #
#             BARPLOTS COTE A COTE                  #
#                                                   #
#####################################################

# Hist de gauche
colleBPEndroit = c(1.414,1.907,1.103)
# Hist de droite apparie
colleBPEnvers = c(0.604,0.844,0.617)

type     = c("Amis","Apparente","Celebrite")
couleurs = c("seagreen3","seagreen2","salmon2", "salmon1","royalblue3","royalblue2")

moyennesColleBP = c(colleBPEndroit,colleBPEnvers)
moyennesColleBP = matrix(moyennesColleBP,nc=3, nr=2, byrow=T)
colnames(moyennesColleBP) = type

# Mes Moyennes
vecMoyennesBP = c(1.414,0.604,1.907,0.844,1.103,0.617)

# Mes erreurs standard
intervallesBP = c(0.211,0.146,0.150,0.194,0.168,0.140)

## Je plot
bp = barplot(moyennesColleBP,beside=T,ylim=c(0,2.5),col = couleurs) ; box()
arrows(bp,vecMoyennesBP-intervallesBP,bp, vecMoyennesBP+intervallesBP, lwd=1.5, angle=90,length=0.1,code=3) 

#####################################################
#                                                   #
#            BARPLOT A DOUBLE ECHELLE               #
#                                                   #
#####################################################
seBPEndroit = c(0.146,0.194,0.140)

# Je reduit la marge de droite pour pouvoir afficher le label de mon 2eme axe
par(mar=c(5,6,4,4) + 0.1)

# Je plot
bp = barplot(colleBPEndroit,ylim = c(0,2.5),
             names = c("Amis","Apparente","Celeb"),
             col=c("seagreen3","salmon2","royalblue3"),
             ylab = "Performance (d')", xlab = "Condition",
             main = "Comparaisons visages a l'endroit"); box()

# Mes barres d'erreurs standards
arrows(bp,colleBPEndroit-seBPEndroit,bp, colleBPEndroit+seBPEndroit, lwd=1.5, angle=90,length=0.1,code=3) 

# Je cree un 2eme axe en ordonnees
plot.window(xlim = c(0,6),ylim = c(330,500))
axis(4,col="red")
# nom de l'axe
mtext("TRmin", side = 4, line =3, cex = par("cex.lab"))

# Je plot mes TRmin pour les conditions endroit
mesXlim = c(0.9,3,5.15)
mesMoyTR = c(360,344,363)
mesTRse  = c(12.47,7.35,14.09)

arrows(mesXlim,mesMoyTR-mesTRse,mesXlim, mesMoyTR+mesTRse, lwd=1, angle=90,length=0.05,code=3,col="gray15") 
points(mesXlim,mesMoyTR,pch=15,cex=1,col="red")

