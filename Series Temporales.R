# Autor: Pedro Antonio Benito Rojano

{r}
library(tidyverse)

mmp1<-function(vector, orden, pos, primero=T){
  mm<-0
  if(primero==T){
    for(i in (pos-orden%/%2):(pos-orden/2+orden-1)) mm=mm+vector[i]
  }else{
    for(i in (pos-orden%/%2+1):(pos-orden/2+orden)) mm=mm+vector[i]
  }
  mm/orden
}

mmp<- function(vector, orden, i) (mmp1(vector, orden, i, T)+mmp1(vector, orden, i, F))/2

mmi<- function(vector, orden, i){
  mm<-0
  for(i in (i-orden%/%2):(i+orden%/%2)) mm<-mm+vector[i]
  mm/orden
}

mMovil<- function(vector, orden){
  mm<-c()
  for(i in 1:(orden%/%2)) mm<-c(NaN,mm)
  for(i in (1+orden%/%2):(length(vector)-orden%/%2)){
    if (orden%%2==0){
      mm<-c(mm,mmp(vector, orden, i))
    }else{
      mm<-c(mm,mmi(vector, orden, i))
    }
  }
  for(i in (length(vector)-orden%/%2+1):length(vector)) mm<-c(mm,NaN)
  mm
}

indicesK<-function(vector, k){
  indk<-c()
  for(i in 1:k){
    indk[i]<-0
    cont<-0
    for(j in 1:length(vector)){
      if(j%%k==i%%k && !is.nan(vector[j])){
        indk[i]<-indk[i]+vector[j]
        cont<-cont+1
      }
    }
    indk[i]<-indk[i]/cont
  }
  if(sum(indk)!=k){
    indk<-indk*k/sum(indk)
  }
  indk
}

medAnual<-function(vector, nEst){
  cont<-0
  año<-1
  medA<-c()
  medA[1]=0
  for(i in 1:length(vector)){
    if(cont<nEst){
      cont=cont+1
    }else{
      cont=0
      año=año+1
      medA[año]=0
    }
    medA[año]=medA[año]+vector[i]
  }
  medA/nEst
}

estPorcMed<-function(vector, nEst){
  ma<-medAnual(vector,nEst)
  porc1Anual<-c()
  cont=0
  año<-1
  for(i in 1:length(vector)){
    porc1Anual[i]<-vector[i]/ma[año]
    cont=cont+1
    if(cont==nEst){
      cont=0
      año=año+1
    }
  }
  est<-indicesK(porc1Anual, nEst)
  if(sum(est)!=length(est)){
    est<-est*length(est)/sum(est)
  }
  est
}

estMedMovil<-function(v, nEst){
  mm<-mMovil(v,nEst)
  indEst<-v/mm
  indicesK(indEst, nEst)
}

tendMinCuad<-function(t,v){
  mod<-lm(v ~ t)
  pred<-predict.lm(mod)
  as.vector(unlist(pred))
}

ciclica<-function(v,t,est,orden=3){
  tend<-tendMinCuad(t,v)
  estac<-estPorcMed(v,est)
  vC.A<-v/tend/estac
  mMovil(vC.A,orden)
}

estAdi<-function(t,v,ord){
  td<-tendMinCuad(t,v)
  vMod<-v-td
  est<-indicesK(vMod, est)
  if(sum(est)!=length(est)){
    est<-est*length(est)/sum(est)
  }
  est
}

dfMult<-function(t,v,est,ordA){
  td<-tendMinCuad(t,v)
  e<-estPorcMed(v,est)
  c<-mMovil(v/t/e, ordA)
  a<-v/td/e/c
  data.frame(Temporalidad=t, Valores=v, Tendencia=td, Estacionalidad=e, Comp.Cíclica=c, Comp.Aleatoria=a)
}

dfAdi<-function(t,v,est,ord=3){
  td<-tendMinCuad(t,v)
  e<-estAdi(t,v,est)
  c<-mMovil(v-td-e, ord)
  a<-v-td-e-c
  data.frame(Temporalidad=t, Valores=v, Tendencia=td, Estacionalidad=e, Comp.Cíclica=c, Comp.Aleatoria=a)
}

#Ejercicio 1

pob<-c(9.47, 9.26, 8.86, 8.25, 7.81, 8.01, 7.55, 7.24, 7.01, 6.88, 7.03)
t<-c(1:11)

length(mMovil(pob, 4))
mMovil(pob, 4)

length(mMovil(pob, 5))
mMovil(pob, 5)

df<-data.frame(pob, mMovil(pob, 4), mMovil(pob, 5))
view(df)

#Ejercicio 3

p<-c(3.9,4,4.8,5.1,5,5.5,6.1,6.3,6.9)
t<-c(1,2,3,4,5,6,7,8,9)

  #Calculamos media movil

mm3<-mMovil(p,3)
mm3

  #Calculamos p/me para quitar tendencia y movimientos cíclicos

p1<-p/mm3
p1

  #Calculamos indices de estacionalidad

pind1<-indicesK(p1,3)
pind1

  #Desestracionalizamos

pDesEst<-p/pind1
pDesEst

#Ejercicio 4

v<-c(178.2, 156.7, 164.2, 153.2, 157.5, 172.6, 185.9, 185.8, 165.0, 163.6, 169.0, 183.1,
     196.3, 162.8, 168.6, 156.9, 168.2, 180.2, 197.9, 195.9, 176.0, 166.4, 166.3, 183.9, 
     197.3, 173.7, 173.2, 159.7, 175.2, 187.4, 202.6, 205.6, 185.6, 175.6, 176.3, 191.7,
     209.5, 186.3, 183.0, 169.5, 178.2, 186.7, 202.4, 204.9, 180.6, 179.8, 177.4, 188.9,
     200.0, 188.7, 187.5, 168.6, 175.7, 189.4, 216.1, 215.4, 191.5, 178.5, 178.6, 195.6,
     205.2, 179.6, 185.4, 172.4, 177.7, 202.7, 220.2, 210.2, 186.9, 181.4, 175.6, 195.6)

t<-c(1976:1981)

  #a) Calcular indices de estacionalidad mediante el método de media móvil en porcentajes.

estMedMovil(v,12)

  #b) Calcular indices de estacionalidad mediante el método de porcentaje medio.

indEst2<-estPorcMed(v, 12)
indEst2

#Ejercicio 5

m<-c(66,77,100,62,77,97,63,78,99,61,78,97)
t<-c(1:12)

  #Modelo mult

df<-dfMult(t,m,3,3)
view(df)

  #Mod Aditivo

df<-dfAdi(t,m,3,3)
view(df)

#Examen temas 3 y 4, 11-5-20

  #10 actividad

tendMinCuad<-function(t,v){
  mod<-lm(v ~ t)
  pred<-predict.lm(mod)
  as.vector(unlist(pred))
}

v<-c(120,100,110,130,120,110,130,150,130,100,140,160)

tendMinCuad(c(1:12), v)[6]
