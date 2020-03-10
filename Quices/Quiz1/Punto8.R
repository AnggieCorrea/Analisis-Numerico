Tk<-c(100,200,300,400,450,500,600)
B<-c(-160,-35,-4.2,9.0,0,16.9,21.3)

install.packages("PolynomF")#instalar paquete 
library(PolynomF)

plot(Tk,B, pch=19, col = "red") #ver grafica 
#plot(x0,y0, pch=19, col = "red", asp=1) #utilizar escala igual
p <- poly_calc(Tk, B)        ## conduce a una falla numérica catastrófica! al utilizar Poly
p                             ## imprime el polinomio
pk<-p(Tk)
#par(new=T)
plot(Tk,pk,col="blue")
