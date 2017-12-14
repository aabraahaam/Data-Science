library(readxl)
library(MASS)
estudiantes <- read_excel("Base de datos.xlsx")
#estudiantes2 <- read.csv("Base de datos_csv.csv", sep = ";", header = T)
estudiantes[estudiantes == "NS"] = NA
estudiantes <- na.omit(estudiantes)
str(estudiantes)
estudiantes$P3 <- as.factor(estudiantes$P3)
estudiantes$P4 <- as.factor(estudiantes$P4)
estudiantes$P5 <- as.numeric(estudiantes$P5)
estudiantes$P7 <- as.numeric(estudiantes$P7)
estudiantes$P8 <- as.numeric(estudiantes$P8)
estudiantes$P9 <- as.numeric(estudiantes$P9)
estudiantes$P10 <- as.numeric(estudiantes$P10)
estudiantes$P12 <- as.numeric(estudiantes$P12)
estudiantes$P13 <- as.numeric(estudiantes$P13)
estudiantes$P14 <- as.numeric(estudiantes$P14)
estudiantes$P15 <- as.numeric(estudiantes$P15)
estudiantes$P16 <- as.numeric(estudiantes$P16)
estudiantes$P18 <- as.numeric(estudiantes$P18)
estudiantes$P19 <- as.numeric(estudiantes$P19)
estudiantes$P20 <- as.numeric(estudiantes$P20)
str(estudiantes)
#cambiar la variable a predecir 
estudiantes$P20[estudiantes$P20 < 7] = 0
estudiantes$P20[estudiantes$P20 >= 7] = 1
estudiantes$P20 <- as.factor(estudiantes$P20)
#muestras
set.seed(1234)

train <- sample(nrow(estudiantes), 0.8*nrow(estudiantes))
estudiantes.train <- estudiantes[train,]
estudiantes.test <- estudiantes[-train,]
regresion <- glm(P20~. , data = estudiantes.train)
summary(regresion)
credit.glm.step <- step(regresion)
regresion1 <- glm(P20 ~ P4 + P5 + P6 + P9 + P13 + P16 + P18 + P19,data = estudiantes.train)
summary(regresion1)
hist(predict(regresion1,type="response"))
fitted.results <- predict(regresion1,newdata=estudiantes.test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
table(estudiantes.test$P20,fitted.results)
#analisis discriminante
m1 <- lda(P20 ~ P4 + P5 + P6 + P9 + P13 + P16 + P18 + P19, data = estudiantes.train)
m1
fitted.results <- predict(m1,newdata=estudiantes.test,type='response')
table(estudiantes.test$P20,fitted.results$class)
# 3 clases
estudiantes <- read_excel("Base de datos.xlsx")
estudiantes[estudiantes == "NS"] = NA
estudiantes <- na.omit(estudiantes)
str(estudiantes)
estudiantes$P3 <- as.factor(estudiantes$P3)
estudiantes$P4 <- as.factor(estudiantes$P4)
estudiantes$P5 <- as.numeric(estudiantes$P5)
estudiantes$P7 <- as.numeric(estudiantes$P7)
estudiantes$P8 <- as.numeric(estudiantes$P8)
estudiantes$P9 <- as.numeric(estudiantes$P9)
estudiantes$P10 <- as.numeric(estudiantes$P10)
estudiantes$P12 <- as.numeric(estudiantes$P12)
estudiantes$P13 <- as.numeric(estudiantes$P13)
estudiantes$P14 <- as.numeric(estudiantes$P14)
estudiantes$P15 <- as.numeric(estudiantes$P15)
estudiantes$P16 <- as.numeric(estudiantes$P16)
estudiantes$P18 <- as.numeric(estudiantes$P18)
estudiantes$P19 <- as.numeric(estudiantes$P19)
estudiantes$P20 <- as.factor(estudiantes$P20)
str(estudiantes)
#cambiar la variable a predecir 3 grupos
estudiantes$P20[estudiantes$P20 >=0 & estudiantes$P20<=5 ] <- 0
estudiantes$P20[estudiantes$P20 >=6 & estudiantes$P20<=7 ] <- 1
estudiantes$P20[estudiantes$P20 >= 8] <- 2
estudiantes$P20 <- as.factor(estudiantes$P20)
#muestras
set.seed(1234)

train <- sample(nrow(estudiantes), 0.8*nrow(estudiantes))
estudiantes.train <- estudiantes[train,]
estudiantes.test <- estudiantes[-train,]
regresion <- glm(P20~. , data = estudiantes.train)
summary(regresion)
credit.glm.step <- step(regresion)
regresion1 <- glm(P20 ~ P6 + P7 + P9 + P13 + P15 + P18 + P19,data = estudiantes.train)
summary(regresion1)
hist(predict(regresion1,type="response"))
fitted.results <- predict(regresion1,newdata=estudiantes.test,type='response')
fitted.results <- ifelse(fitted.results > 1.5,2,ifelse(fitted.results > .5,1,0))
table(estudiantes.test$P20,fitted.results)
#analisis discriminante
m1 <- lda(P20 ~ P6 + P7 + P9 + P13 + P15 + P18 + P19, data = estudiantes.train)
m1
fitted.results <- predict(m1,newdata=estudiantes.test,type='response')
table(estudiantes.test$P20,fitted.results$class)
