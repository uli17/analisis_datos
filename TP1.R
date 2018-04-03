Krep<-function(k) {
  alpha<-c()
  for (i in 1:100) {
    tiempo<-c()
    for (i in 1:k) {
      album<-1:500
      cont<-0
      while (length(album)>0) {
        paquete<-sample(1:500,5)
        cont<-cont+1
        for (i in 1:5) {
          album<-album[album!=paquete[i]]
        }
      }
      tiempo<-c(tiempo,cont)
    }
    alpha<-c(alpha,mean(tiempo))
  }
  alpha
}

k<-c(Krep(5))
k<-c(k,Krep(10))
k<-c(k,Krep(50))
k<-c(k,Krep(100))
res<-matrix(k,100,4)


tabla<-matrix(rep(NA,24),6,4)
colnames(tabla) <- c("k=5","k=10","k=50","k=100")
rownames(tabla) <- c("Media","Mediana","Media .1-podada","Media .2-podada", "SD", "IQR")

i<-1
for (j in 1:4) {
  tabla[i,j]<-mean(res[,j])
}
i<-2
for (j in 1:4) {
  tabla[i,j]<-median(res[,j])
}
i<-3
for (j in 1:4) {
  tabla[i,j]<-mean(res[,j], trim=.1)
}
i<-4
for (j in 1:4) {
  tabla[i,j]<-mean(res[,j], trim=.2)
}
i<-5
for (j in 1:4) {
  tabla[i,j]<-sd(res[,j])
}
i<-6
for (j in 1:4) {
  tabla[i,j]<-IQR(res[,j])
}

boxplot(res)
hist(res[,1])
hist(res[,2])
hist(res[,3])
hist(res[,4])