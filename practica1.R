sum<-0
i<-0
k<-0
while(sum<1000000) {
  sum<-sum+i
  i<-i+1
  k<-k+1
}


i<-0
sum<-0
while(sum<500) {
  aux<-runif(1, min=0, max=1)
  sum<-sum+aux
  i<-i+1
}
i
sum


setwd("~/Documents/Analisis")
tabla = read.table("autos.txt")

tabla[order(tabla[,1]),]

proba<-c()
for (i in 1:10000) {
  cont<-0
  i<-0
  k<-sample(1:365,20,replace=TRUE)
  if (anyDuplicated(k)!=0) {
    cont<-cont+1
  }
  proba<-c(proba,cont)
}
mean(proba)


tiempo<-c()
for (i in 1:500) {
  plata<-100
  cont<-0
  while (plata>0) {
    numero<-sample(1:38,1)
    cont<-cont+1
    if (numero >=19) {
      plata<-plata-1
    } else {
      plata <-plata+1
    }
  }
  tiempo<-c(tiempo,cont)
}
mean(tiempo)


tiempo<-c()
for (i in 1:1000) {
  album<-1:500
  cont<-0
  while (length(album)>0) {
    paquete<-sample(1:500,5)
    cont<-cont+1
    for (i in 1:length(paquete)) {
        album<-album[album!=paquete[i]]
    }
  }
  tiempo<-c(tiempo,cont)
  album
}
mean(tiempo)

A = matrix(c(1,1,2,4,3,0,5,1,4), nrow=3, ncol=3)
det(A)
b=c(1,-1,3)
solve(A,b)

escalar<-function(a,n) {
  diag(a,n,n)
}

obtenerDiagonal<-function(A) {
  diag(A)
}

normaFrobenius<-function(A) {
  suma<-0
  for (j in 1:length(A[1,])) {
    for (i in 1:length(A[,1])) {
      suma<-suma+(A[i,j])**2
    }
  }
  sqrt(suma)
}

grilla<-function(a,b,n) {
  seq(a,b,length.out=n)
}

aprox1<-function(n) {
  a<-1
  i<-0
  while (i<n) {
    a<-(a/2)+(1/a)
    i<-i+1
  }
  a
}

aprox2<-function(eps) {
  a<-1
  while (abs((a/2)-(1/a))>eps) {
    a<-(a/2)+(1/a)
  }
  a
}

aprox3<-function(n,eps) {
  a<-1
  i<-0
  while ((abs((a/2)-(1/a))>eps)|(i<n)) {
    a<-(a/2)+(1/a)
    i<-i+1
  }
  a
}

