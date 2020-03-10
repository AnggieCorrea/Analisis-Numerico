#Diferencias divididas. diff.dv
x<-c(1,2)
f<-c(2,6)
plot(x,f)
n<-length(f)
nombres<-c("f",paste("D",1:(n-1),sep="") )
diff.dv <-rep(NA,n*n)
dim(diff.dv)<-c(n,n)
diff.dv[,1]<-f
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
plot(x,f, col="red", xlim=c(0,10),ylim=c(0,10))
