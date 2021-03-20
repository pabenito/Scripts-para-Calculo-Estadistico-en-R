# Autor: Pedro Antonio Benito Rojano

{r}
#Calcula varianza a partir de la cuersivarianza
sv<-function(s,n){s*(n-1)/n}

#Calcula cuarsivarianza a partir de la varianza
vs<-function(v,n){v*n/(n-1)}

#Calcula el intervalo donde se encuentra la media muestral con una probablilidad 1-a,a partir 
#del número de elementos 'n', media población 'me' y desviación típica  de la población'dv'.
ime<-function(n,me,dv,a){
  z<-qnorm(1-a/2/2,0,1)
  i<-z*dv/sqrt(n)
  c(me-i,me+i)
}
n<-48
a<-0.05
dv<-2.8
me<-71

ime(n,me,dv,a) #intervalo

qnorm(1-a/2,0,1)*dv/i # n calculado

#ej6

ime(48,71,2.8,0.05)

#Calcula el intervalo donde se encuentra la media muestral con una probablilidad 1-a,a partir 
#del número de elementos 'n' y media población 'me' desconociendo la desviación típica.
imed<-function(n,me,s,a){
  if(n>30){
    res<-intv(x,qnorm(1-a/2,0,1),s^2/n)
  }else{
    res<-intv(x,qt(1-a/2,n-1),s^2/n)
  }
}

#Devuelve el vector intervalo de confianza a partir de la media 'x',
#el multiplicador 'mult' y el valor de dentro de la raíz 'raiz'. 
intv<-function(x,mult,raiz){
  i<-c(x-mult*sqrt(raiz),x+mult*sqrt(raiz))
  if(i[1]>i[2]){
    i1<-i[2]
    i[2]=i[1]
    i[1]=i1
  }
  i
}

#Calcula la función de welch
f<-function(s1,s2,n1,n2){
  num<-(s1^2/n1+s2^2/n2)^2
  den<-(s1^2/n1)^2/(n1+1)+(s2^2/n2)^2/(n2+1)
  num/den-2
}

#Calcula el intervalo donde se encuntra la diferencia de medias muestrales x1,x2 correspondientes
#a distintas poblaciones, desconociendo la varianza de las poblaciones pero sabiendo que son iguales.
idmei<-function(x1,x2,s1,s2,n1,n2,a){
  t<-qt(1-a/2,n1+n2-2)
  sp<-sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
  rz<-1/n1+1/n2
  intv(x1-x2,t*sp,rz)
}

#Calcula el intervalo donde se encuntra la diferencia de medias muestrales x1,x2 correspondientes
#a distintas poblaciones, desconociendo la varianza de las poblaciones pero sabiendo que son diferentes.
idmed<-function(x1,x2,s1,s2,n1,n2,a){
  t<-qt(1-a/2,f(s1,s2,n1,n2))
  rz<-s1^2/n1+s2^2/n2
  intv(x1-x2,t,rz)
}

#Calcula el intervalo donde se encuntra la diferencia de medias muestrales x1,x2 correspondientes
#a distintas poblaciones, desconociendo la varianza de las poblaciones y sabiendo que n1+n2>30 y n1 aprx.= n2.
idmeg<-function(x1,x2,s1,s2,n1,n2,a){
  intv(abs(x1-x2),qnorm(1-a/2,0,1),s1^2/n1+s2^2/n2)
}

idmeg(204.93,193.83,11.62,11.257,30,27,0.03)

#Calcula el intervalo donde se encuntra la diferencia de medias muestrales x1,x2 correspondientes
#a distintas poblaciones, desconociendo la varianza de las poblaciones. Si la suma de n1+n2>30 y 
#n1n2=True (Que quiere decir que consideramso que n1 aprx.= n2), sino se calculara en funcion de si 
#sabemos que las varianzas son iguales o no (determinado por el parámetro igl).
idme<-function(x1,x2,s1,s2,n1,n2,a,igl=FALSE,n1n2=FALSE){
  if(n1+n2>30 && n1n2){
    res<-idmeg(x1,x2,s1,s2,n1,n2,a)
  }else if(igl){
    res<-idmei(x1,x2,s1,s2,n1,n2,a)
  }else{
    res<-idmed(x1,x2,s1,s2,n1,n2,a)
  }
  res
}


#Calcula el intervalo donde se encuntra la razón de las varianzas v1,v2 corresponcientes a distintas poblaciones.
irv<-function(v1,v2,n1,n2,a){
  ss1<-v1*n1/(n1-1)
  ss2<-v2*n2/(n2-2)
  num<--ss1/ss2
  ii<-num/pf(a/2,n1-1,n2-1)
  is<-num/pf(-a/2,n1-1,n2-1)
  c(ii,is)
}

#Calcula el intervalo donde se encuntra la diferencia de parámetros (p1-p2) de dos distribuciones
#binomiales B(n1, p1) y B(n2, p2) sabiendo que las muestras son grandes n1+n2>0 y n1 aprx.= n2.
idpg<-function(p1,p2,n1,n2,a){
  intv(p1-p2,qnorm(1-a/2,0,1),p1*(1-p1)/n1+p2*(1-p2)/n2)
}

#Intevalo del cociente de varianzas
icva<-function(s1,s2,n1,n2,a){
  c(s1^2/s2^2/qf(1-a/2,n1-1,n2-1),s1^2/s2^2/qf(a/2,n1-1,n2-1))
}

#Contraste de hipótesis igualdad de medias sin conocer
#varianzas y suponiendo que sean distintas y n1+n2<=30. 
chimevd<-function(x1,x2,s1,s2,n1,n2,abs=FALSE){
  if(abs){
    num<-abs(x1-x2)
  }else{
    num<-x1-x2
  }
  den<-sqrt(s1^2/n1+s2^2/n2)
  num/den
}

#Devuelve lo elemnteo no repetidos en orden creciente. 
#Ej: a partir de c(1,2,1,3,3,3) devuelve c(1,2,3).
so<-function(o){so<-sort(o[!duplicated(o)])}

#Frecuencias a partir de las observaciones repetidas. 
#Ej: a partir de c(1,2,1,3,3,3) devuelve
#c(2,1,3) que son la frecuancias absolutas de sus
#elemento ordenados en orden creciente. 
fa<-function(o){
  soo<-so(o)
  for(i in 1:length(soo)){
    soo[i]<-length(o[o==soo[i]])
  }
  soo
}

#Bondad del ajuste binomial
bab<-function(N,na,pa,a){
  o<-c(na,N-na)
  e<-N*c(pn,1-pn)
  xia<-qchisq(1-a,length(o)-1)
  xi<-sum((o-e)^2/e)
  list("Xi2"=xi, "Xi2(a,k-1)"=xia, "Aj. Binom"= xi<xia)
}

#Bondad del ajuste multinomial. Introducimos directamente 
#las ocurrencias observadas. Ej: si han tocado 2 unos(tipo A),
#1 dos(tipo B) y 3 treses(tipo C), el vector 'o' es c(2,1,3)
#(observaciones) y no c(1,2,3) (modalidades), y 'po' las 
#probabilidades de las respcetivas modalidades.
bam<-function(o,po,a){
  e<-sum(o)*po
  xia<-qchisq(1-a,length(o)-1)
  xi<-sum((o-e)^2/e)
  list("Xi2"=xi, "Xi2(a,k-1)"=xia, "Aj. multinomial"= xi<xia)
}

#Bondad del ajuste homogéneo para valores discretos. El vector 'n' contine el tamaño de
#las distintas muestras, y el 'o' el nº de observaciones de las 
#muestras respectivas de la característica a estudiar. 
bahd<-function(o,n,a){
  p<-sum(o)/sum(n)
  e<-n*p
  xia<-qchisq(1-a,length(o)-1)
  xi<-1/(p*(1-p))*sum((o-e)^2/n)
  list("Xi2"=xi, "Xi2(a,k-1)"=xia, "Aj. Homogeneo"= xi<xia)
}


#Bondad del ajuste de contigencia. El vector 'o' es la matriz de las muestras, 
#y a es el nivel de confianza (alpha).
bai<-function(o,a){
  n<-sum(o)
  f<-o
  xi<-0
  ffilas<-rowSums(o)/n
  fcolumnas<-colSums(o)/n
  for(i in 1:ncol(o)){
    for(j in 1:nrow(o)){
      f[j,i]<-fcolumnas[i]*ffilas[j]
    }
  }
  e<-f*n
  xia<-qchisq(1-a,(nrow(o)-1)*(ncol(o)-1))
  for(i in 1:ncol(o)){
    for(j in 1:nrow(o)){
      xi<-xi+(o[j,i]^2/e[j,i])
    }
  }
  xi<-xi-n
  list("Xi2"=xi-n, "Xi2(a,k-1)"=xia, "Aj. independencia"= xi<xia)
}

o1<-matrix(c(1:6),nrow=2,byrow=T)
o2<-matrix(rep(1,6),nrow=2)

bai(o1,0.05)
bai(o2,0.05)

#Bondad del ajuste normal. ¡NO USAR PARA HACER EL EJRCICIO, SOLO PARA COMPROBAR!
ban<-function(o,a){
  N<-length(o)
  me<-mean(o)
  dv<-sd(o)
  numo<-so(o)
  fao<-fa(o)
  p<-dnorm(numo,me,dv)
  if(sum(p)!=1){
    p<-p*1/sum(p)
  }
  e<-N*p
  xia<-qchisq(1-a,length(o)-2)
  xi<-sum((fao-e)^2/e)
  list("Xi2"=xi, "Xi2(a,k-1)"=xia, "Aj. Normal"= xi<xia)
}

#Bondad del ajuste de Poisson ¡NO USAR PARA HACER EL EJRCICIO, SOLO PARA COMPROBAR!
bap<-function(o,a){
  N<-length(o)
  lamda<-mean(o)
  numo<-so(o)
  fao<-fa(o)
  p<-dpois(numo,me,dv)
  if(sum(p)!=1){
    p<-p*1/sum(p)
  }
  e<-N*p
  xia<-qchisq(1-a,length(o)-1)
  xi<-sum((fao-e)^2/e)
  list("Xi2"=xi, "Xi2(a,k-1)"=xia, "Aj. Poisson"= xi<xia)
}

#Realiza el contraste de dependencia e independencia para balores discretos.
#Hay que tener en cuanta que no corrige si algún ei<=5. 
#'o' es la matrix de obsevación, 'x' es el vector de valores de la varible X,
#que corresponde a las columnas de 'o' y 'y' es el vector de valores de la
#varible Y, que corresponde a las filas de 'o'. Devuelve TRUE si son independientes.
cdep<-function(o,a){
  ox<-colSums(o)
  oy<-rowSums(o)
  n<-sum(o)
  pxy<-o
  exy<-o
  xi2<-o
  for(i in 1:ncol(o)){
    for(j in 1:nrow(o)){
      pxy[i,j]<-ox[i]*oy[j]/n^2
    }
  }
  for(i in 1:ncol(o)){
    for(j in 1:nrow(o)){
      exy[i,j]<-n*pxy[i,j]
    }
  }
  for(i in 1:ncol(o)){
    for(j in 1:nrow(o)){
      xi2<-(o[i,j]-exy[i,j])^2/exy[i,j]
    }
  }
  xi2<-sum(xi2)-n
  xi2a<-qchisq(1-a,(length(o)-1)*(nrow(o)-1))
  list("Observaciones"=o, "Xi2"=xi2, "Xi2(a,(k-1)*(m-1))"=xi2a, "Independientes"= xi2<xi2a)
}

########################

  #Ej 1 a)

ppois(70,120,F)

(ppois(70.05,70)-ppois(69.5,70))*ppois(0.05,50)*ppois(70,120,F)/ppois(70,120,F)

  #Ej 2

pnorm(-1,-0.5,0.86)+pnorm(1,-0.5,0.86,F)

pnorm(1,-0.5,0.86,F)/0.321
#########################

#Repaso

  #Ej 10

o<-c(6,7,9,18)
n<-c(rep(40,4))
a<-0.05
bahd(o,n,a)

  #Ej independencia
ncol(o)
o<-data.frame(Enfermos=c(9,18,0), Sanos=c(42,28,0), row.names=c("Vacunados", "No vacunados","0"))
a<-0.05
cdep(o,0.05)

o[2,1]
#######

#Ej 1

qnorm(0.025,0,1)
qnorm(0.025,32000,4000/sqrt(50))

  #Ej 2

pnorm(14,12,4,F)

pnorm(14,12,4/sqrt(9),F)

  #Ej 3

n<-144
me<-160
dv<-10

ime(n,me,dv,0.95)

ime(n,me,dv,0.90)

  #Ej 5

x1<-166
x2<-164.7
s1<-28
s2<-7
n1<-13
n2<-16

idmei(x1,x2,s1,s2,n1,n2,0.05)

idmed(x1,x2,s1,s2,n1,n2,0.05)

  #Ej 7

qt(1-0.1/2,f(50.51,50.41,10,9))

  #Ej 8

chimevd(4.3,3.6,0.9,1.9,12,8)
qt(1-0.05/2,f(0.9,1.9,12,9))

  #Ej 9

N<-100

#Según modelo genético
pn<-3/13 
e<-N*c(pn,1/pn)
e

xi<-qchisq(1-0.05,length(e)-1)
xi

#Obserbado
ng<-15
o<-c(ng,N-ng)
o

xi2<-sum((o-e)^2/e)
xi2

#Contrastamos
xi2<xi

#Mediante la función bab
bab(N,ng,pn,0.05)

  #Ej 10

bah(c(6,7,9,18),0.05)
bah(c(6,7,9,18),0.025)

  #Ej 11

a<-0.05
o<-c(80, 70, 90, 75, 55, 80, 80, 65, 100, 75, 60, 60,
     75, 95, 80, 80, 90, 85, 70, 95, 75, 70, 85, 80,
     80, 65, 65, 50, 75, 75, 85, 85, 90, 70)

N<-length(o)
N
me<-mean(o)
me
dv<-sd(o)
dv
numo<-so(o)
numo
fao<-fa(o)
fao
p<-dnorm(numo,me,dv)
p
p<-dnorm(numo,me,dv)
if(sum(p)!=1){
  p<-p*1/sum(p)
}
p
sum(p)
e<-N*p
e
xi<-qchisq(1-a,length(o)-2)
xi
xi2<-sum((fao-e)^2/e)
xi2
xi2<xi

#Aplicando la función ban que generaliza este proceso

ban(o,a)

 #Ej 12

#Para poder hacer correctamente la comparación de las tres podalidades compararemos la 
#diferencia de las medias 2 a 2, si todas son aproximadamentte iguales entonces aceptamos la Ho.

#Ho: Todos lo métodos de empaquetamiento son igualmente bueno. 
#Ha: No todos son igualmente buenos.

n<-4 #4 meses de eprueba
alf<-0.05

a<-c(6,8,8,9)
b<-c(10,12,8,14)
c<-c(10,12,14,16)

ame<-mean(a)
ame
bme<-mean(b)
bme
cme<-mean(c)
cme

as<-var(a)
as
bs<-var(b)
bs
cs<-var(c)
cs

ab<-chimevd(ame,bme,as,bs,n,n,T)
ab
ac<-chimevd(ame,cme,as,cs,n,n,T)
ac
cb<-chimevd(cme,bme,cs,bs,n,n,T)
cb

abt<-qt(1-alf/2,f(as,bs,n,n))
abt
act<-qt(1-alf/2,f(as,cs,n,n))
act
cbt<-qt(1-alf/2,f(cs,bs,n,n))
cbt

#Contrastamos

ab<=abt
ac<=act
cb<=cbt

  #Funciones usadas

#Contraste de hipótesis igualdad de medias sin conocer
#varianzas y suponiendo que sean distintas y n1+n2<=30. 
chimevd<-function(x1,x2,s1,s2,n1,n2,abs=FALSE){
  if(igl){
    num<-abs(x1-x2)
  }else{
    num<-x1-x2
  }
  den<-sqrt(s1^2/n1+s2^2/n2)
  num/den
}

#Calcula la función de welch
f<-function(s1,s2,n1,n2){
  num<-(s1^2/n1+s2^2/n2)^2
  den<-(s1^2/n1)^2/(n1+1)+(s2^2/n2)^2/(n2+1)
  num/den-2
}


#####

qt(0.975, 26)
ppois(5,1/6,F)
