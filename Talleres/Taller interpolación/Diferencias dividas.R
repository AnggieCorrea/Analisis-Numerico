f<-function(x) log(x)
x<-c(1,2)
y<-f(x)

#Diferencias divididas. diff.dv

plot(x,y,type="o",col="blue")
n<-length(y)
nombres<-c("f",paste("D",1:(n-1),sep="") )
diff.dv <-rep(NA,n*n)
dim(diff.dv)<-c(n,n)
diff.dv[,1]<-y
dimnames(diff.dv)<-list(0:(n-1),nombres)
for (j in 2:n) {
  for (i in 1:(n-j+1)) {
    k<-j+i-1
    print(c(i,j,k))
    diff.dv[i,j] <- (diff.dv[i+1,j-1] - diff.dv[i,j-1])/(x[k]-x[i])
  }
}
tabla<-as.matrix(data.frame(x=x,diff.dv))
print(tabla,na.print = "")
plot(x,y, type="o", col="red")

ErrA=y[1]-tabla[1,2]
ErrB=y[2]-tabla[1,3]

#print("Error para x0=1")
#print(ErrA)
#print("Error para x0=2")
#print(ErrB)