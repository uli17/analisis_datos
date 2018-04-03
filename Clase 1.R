v<-1:100
sum(v)

x<-c(1,2,5,7)
y<-c(2,3,1,4)
z<-x>y
z

x<-sample(1:100,size=10000,replace=T)


suma<-function(x,y) {
  return(x+y-1)
}

sumar<-function(n) {
  aux<-0
  for(i in 1:n) {
    aux<-aux+i
  }
  return(aux)
}

sumavec<-function(v) {
  aux<-0
  for (i in 1:length(v)) {
    aux<-aux+v[i]
  }
  return(aux)
}

sumavec2<-function(v) {
  aux<-0
  for (x in v) {
    aux<-aux+x
  }
  return(aux)
}

div<-function(n) {
  aux<-0
  i<-1
  while (aux<=n) {
    aux<-aux+(1/i)
    i<-i+1
  }
  return(i-1)
}

sumarPositivos<-function(v) {
  aux<-0
  for(l in v) {
    if (l>0) {
      aux<-aux+l
    }
  }
}


urna<-seq(c("rojas","azules"),c(8,5))
n<-10000
exitos<-0
for(i in 1:n){
  muestra<-sample(urna,size=6,replace=F)
  x<-muestra=="rojas"
  if(sum(x)>=3){
    exitos<-exitos+1
  }
}
exitos/n


x<-seq(-5,5,lenth=100)
y<-exp(x)
plot(x,y)
plot(x,y,type="l")