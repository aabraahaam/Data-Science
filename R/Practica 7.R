source("http://www.bioconductor.org/biocLite.R")
biocLite('ROCR')
library(ROCR)
biocLite("multtest")
library(multtest)
data(golub)
gol.true<-factor(golub.cl,levels=0:1,labels=c('TRUE','FALSE'))
pred <- prediction(golub[1042,],gol.true)
perf<-performance(pred,'tpr','fpr')
plot(perf)
#_____________________________________________-
gol.true <- factor(golub.cl,levels=0:1,labels= c(" ALL","not ALL"))
gol.pred <- factor(golub[1042,]>1.27,levels=c("TRUE","FALSE"),labels=c("ALL","notALL"))
#tabla de errores
table(gol.pred,gol.true)
#arbol decisión
set.seed(123); n<-10 ; sigma <- 0.5
fac <- factor(c(rep(1,n),rep(2,n),rep(3,n)))
levels(fac) <- c("ALL1","ALL2","AML")
geneA <- c(rnorm(10,0,sigma),rnorm(10,2,sigma),rnorm(10,4,sigma))
dat <- data.frame(fac,geneA)
library(rpart)
rp  <-  rpart(fac ~ geneA, method="class",data=dat)
plot(rp, branch=0,margin=0.1);  text(rp, digits=3, use.n=TRUE)
