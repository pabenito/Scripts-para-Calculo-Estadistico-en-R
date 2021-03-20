# Autor: Pedro Antonio Benito Rojano

{r}
mu<-function(X,Y,r,s){
  N<-length(X)
  mu<-(sum((X-mean(X))^(r)*(Y-mean(Y))^(s)))/N
}

m<-function(X,Y,r,s){
  N<-length(X)
  mu<-(sum(X^(r)*Y^(s)))/N
}

# Devuelve a partir de un vector de entrada
# el vector de outliers calculado como:
# X0 es outlier si x0<Q1-1.5*(Q3-Q1) o x0>Q3+1.5*(Q3-Q1).

outliers<-function(x){
  q<-quantile(x) # Calcula cuartiles.
  rangoInter<-q[4]-q[2] # Calcula el rango intercuartílico.
  limInf<-q[2]-1.5*rangoInter # Calcula el lim. Inferior.
  limSup<-q[4]+1.5*rangoInter # Calcula el lim. superior.
  x1<-x[x<limInf] # Guarda los menores que el lim. inferior.
  x2<-x[x>limSup] # Guarda los mayores que el lim. superior.
  c(x1,x2) # Devuelve el vector con los valores fuera de rango.
}

X<-c()
Y<-c()


# Devuelve a partir de un vector de entrada
# el vector de outliers calculado como:
# X0 es outlier si x0<Q1-1.5*(Q3-Q1) o x0>Q3+1.5*(Q3-Q1).

outliers<-function(x){
  q<-quantile(x) # Calcula cuartiles.
  rangoInter<-q[4]-q[2] # Calcula el rango intercuartílico.
  limInf<-q[2]-1.5*rangoInter # Calcula el lim. Inferior.
  limSup<-q[4]+1.5*rangoInter # Calcula el lim. superior.
  x1<-x[x<limInf] # Guarda los menores que el lim. inferior.
  x2<-x[x>limSup] # Guarda los mayores que el lim. superior.
  c(x1,x2) # Devuelve el vector con los valores fuera de rango.
}

x<-c(30, 25, 20, 52, 45, 20, 10, 1)
outliers(x)


