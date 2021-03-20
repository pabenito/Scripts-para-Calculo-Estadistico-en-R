# Autor: Pedro Antonio Benito Rojano

{r}
abc<-function(X,Y,Z,t.indep=T){
  
  N<-length(X)
  
  if(t.indep){
    Mt<- matrix( c( ( (1:N)*0+1 ) , X, Y), nrow=3, byrow=T)
    M<- t(Mt)
    
  }else{
    Mt<- matrix( c( ((1:N)*0) , X, Y), nrow=3, byrow=T)
    M<- t(Mt)
  }
  
  MtM<- Mt %*% M
  MtY<- Mt %*% Y
  
  if(t.indep){
    A<-solve(MtM,MtY)
    A
    
  }else{
    A<-solve(MtM,MtY)
    A[c(2,3)]
  }
}

c.det<-function(X,Y,Z,A,t.indep=T){
  N<- length(X)
  if(t.indep){
    e<- Z-(A[1]+A[2]*X+A[3]*Y)
  }else{
    e<- Z-(A[1]*X+A[2]*Y)
  }
  MSE<- sum(e^2)/N
  varZ<- (N-1)*var(Z)/N
  R2<- 1-MSE/varZ
  R2
}

#Ejecución Z=a+bX+cY

X<-c(1,1.9,4,9,15)
Y<-c(0,1,2,3,4)
Z<-c(0.5,0.95,1,5,10)

A<-abc(X,Y,Z)
A

R2<-c.det(X,Y,Z,A)
R2

estimado<-function(xi,yi,A){
  Z<-A[1]+A[2]*xi+A[3]*Y
  Z
}

xi<-c()
yi<-c()

Zest<-estimado(xi,yi,A)

#Ejecución Z=aX+bY

X<-c(1,1.9,4,9,15)
Y<-c(0,1,2,3,4)
Z<-c(0.5,0.95,1,5,10)

A<-abc(X,Y,Z,F)
A

R2<-c.det(X,Y,Z,A)
R2

estimado<-function(xi,yi,A){
  Z<-A[1]*xi+A[2]*Y
  Z
}

xi<-c()
yi<-c()

Zest<-estimado(xi,yi,A)


