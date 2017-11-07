#Ejercicio 1
require(stats)
library(stats)
str(swiss)
head(swiss)
x <- swiss$Education[1:25]
x
sort(x, method="sh",index.return = TRUE)
x <- as.integer(rnorm(200, 5, 7))
sort(x, method="quick")
sort(x, method="qu")
sort(x, method="q")
sort(x, partial = 1:5)
#Ejercicio 2
x<- as.integer(rnorm(100,42,2.5))
y<-as.integer(rnorm(100,177,10))
pa<-data.frame(x,y)
apply(pa,2,mean)
apply(pa,2,var)
cov(x,y)
cor(x,y)
plot(x,y)
#Esta función gráfica un scatter con la línea de correlación
#scatter.smooth(x, y)
regresion<-lm(y ~ x)
abline(regresion)
#Ejercicio 3
#Calcular la matriz
matriz<-function(){
filas<-as.integer(readline('Número de filas '))
columnas<-as.integer(readline('Número de columnas '))
lambda<-as.integer(readline('Lambda '))
matriz1<-matrix(data=NA, nrow=filas, ncol=columnas)
for (i in 1:filas){
  for (j in 1:columnas){
    #poison<-rpois(1,lambda)
    matriz1[i,j]<-rpois(1,lambda)
  }
}
return(matriz1)
}
x<-matriz()
x
#transpuesta
trans<-function(x){
matriz1<-matrix(data=NA, nrow=ncol(x), ncol=nrow(x))
for (i in 1:ncol(x)){
  for (j in 1:nrow(x)){
    #poison<-rpois(1,lambda)
    matriz1[i,j]<-x[j,i]
  }
}
return(matriz1)
}
trans(x)
t(x)
