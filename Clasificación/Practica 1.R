#setwd("C:/DAT/JESUS/UNIVERSIDAD/MASTER CUNEF/ARBOLES gender discrimination")

gender <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/GenderDiscrimination.csv")

head(gender, 6) # comprobamos el encabezado de gender 
table(gender$Gender)
## # modelo a estimar Gender~ Experience + Salary
#set.seed(125)
train <- sample(nrow(gender), 0.7*nrow(gender))

df.train <- gender[train,]

df.validate <- gender[-train,]
table(df.train$Gender)

table(df.validate$Gender)
96/(96+49)
44/(44+19)
library(rpart)
arbol <- rpart(Gender~ Experience + Salary, data=df.train, method="class",
               parms=list(split="information"))
print(arbol)

summary(arbol)

arbol$cptable
plotcp(arbol)

arbol.podado <- prune(arbol, cp=0.03061)
library(rpart.plot)
#prp(arbol, type = 2, extra = 104,
 #   fallen.leaves = TRUE, main="Decision Tree")
prp(arbol.podado, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")
prp(arbol, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Arbol")

arbol.pred <- predict(arbol.podado, df.validate, type="class")

arbol.perf <- table(df.validate$Gender, arbol.pred,
                    dnn=c("Actual", "Predicted"))

arbol.perf
(12+11)/(27+11+12+13)

#---------------------------------
library(party)

fit.ctree <- ctree(Gender~ Experience + Salary, data=df.train)

plot(fit.ctree, main="Conditional Inference Tree")

ctree.pred <- predict(fit.ctree, df.validate, type="response")

ctree.perf <- table(df.validate$Gender, ctree.pred,
                    dnn=c("Actual", "Predicted"))

ctree.perf

str(gender)