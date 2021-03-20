# Autor: Pedro Antonio Benito Rojano

{r}
ab<-function(X,Y,t.indep=T){
  
  N<-length(X)
  
  if(t.indep){
    Mt<- matrix( c( ( (1:N)*0+1 ) , X), nrow=2, byrow=T)
    M<- t(Mt)
    
  }else{
    Mt<- matrix( c( ((1:N)*0) , X), nrow=2, byrow=T)
    M<- t(Mt)
  }
  
  MtM<- Mt %*% M
  MtY<- Mt %*% Y
  
  if(t.indep){
    A<-solve(MtM,MtY)
    A
    
  }else{
    A<-solve(MtM,MtY)
    A[2]
  }
}

c.c.lineal<-function(X,Y){
  N<-length(X)
  cov<-cov(X,Y)*(N-1)/N
  dtx<-sqrt(var(X)*(N-1)/N)
  dty<-sqrt(var(Y)*(N-1)/N)
  r2<-cov/(dtx*dty)
  r2
}

#Ejecución Y=a+bX

X<-c()
Y<-c()

A<-ab(X,Y)
A

r2<-c.c.lineal(X,Y)
r2

estimado<-function(xi, A){
  Y<-A[1]+A[2]*xi
  Y
}

xi<-c()

Yest<-estimado(xi, A)

#Ejecución Y=a*e^(bX)

X<-c()
Y<-c()

A0<-ab(X,log(Y))
A<-A0
A[2]<-exp(A[2])
A

R2<-c.c.lineal(X,log(Y))
R2

estimado<-function(xi, A){
  Y<-A[1]+exp(A[2]*xi)
  Y
}

xi<-c()

Yest<-estimado(xi, A)

#Ejecución Y=a*b^X

X<-c()
Y<-c()

A0<-ab(X,log(Y))
A<-exp^(A0)
A

R2<-c.c.lineal(X,log(Y))
R2

estimado<-function(xi, A){
  Y<-A[1]+A[2]^(xi)
  Y
}

xi<-c()

Yest<-estimado(xi, A)

#Ejecución Y=1/(a+bX)

X<-c()
Y<-c()

A<-ab(X,1/Y)
A

R2<-c.c.lineal(X,1/Y)
R2

estimado<-function(xi, A){
  Y<-1/(A[1]+exp(A[2]*xi))
  Y
}

xi<-c()

Yest<-estimado(xi, A)

#Ejecución Y=aX

X<-c()
Y<-c()

A<-ab(X,Y,F)
A

r2<-c.c.lineal(X,Y)
r2

estimado<-function(xi, A){
  Y<-A*xi
  Y
}

xi<-c()

Yest<-estimado(xi, A)

#Ejecución Y=1/bX

X<-c()
Y<-c()

A<-ab(X,1/Y,F)
A

R2<-c.c.lineal(X,1/Y)
R2

estimado<-function(xi, A){
  Y<-1/(A*xi)
  Y
}

xi<-c()

Yest<-estimado(xi, A)

