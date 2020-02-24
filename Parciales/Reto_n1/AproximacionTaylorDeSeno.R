factorial <-function (valor){
  factor<-1
  a<-1
  for(a in 1:valor){
    factor<-factor*a
  }
  return(factor)
}

function(valor){
  resul<-valor
  precision <-10 #Cantidad de ciclos
  a<-1
  posicion<-0
  
  for(a in 1:precision){
    posicion<-a*2+1
    if(a%%2 ==0){
      resul=resul+((valor^posicion)/(factorial(posicion)))
    }
    else
      resul=resul-(valor^posicion/factorial(posicion))
  }
  return(resul)
}

angulox<-c()
anguloy<-c()
it<--pi/64

for(it in 0.01:pi/64){
  angulox=c(angulox,it)
  y<-seno(it)
  anguloy=c(anguloy,y)
}

cat(angulox,"\n---------------------\n",anguloy)

plot(angulox,anguloy,type="l", main="Aproximación de Taylor para Seno", xlab="x", ylab="y")