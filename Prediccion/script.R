#install.packages('reader')
library(tidyverse)
library(forecast)
library(ggthemes)
library(ggplot2)
library(quantmod)
library(moments)
library(ggfortify)
library(plotly)
library(reader)
library(gridExtra)

apple <- read.csv('apple.csv')
apple[is.na(apple)] <- 0
str(apple)
summary(apple)
Ventas <- data.frame(apple[1:2],apply(apple[,3:6],1,sum))
colnames(Ventas)[3]<-"Sales"

mData=xts(tempData$Precio, order.by = as.Date(tempData$Fecha,"%Y-%m-%d"),frequency=365)

date <-seq(as.Date("1998/10/01"), as.Date("2016/03/30"),"quarter")
date
date <- as.yearqtr(date, "%Y%q")
date
ventas<- zoo(Ventas[3], date)
ventas
ggplotly(autoplot(as.xts(ventas),ts.colour = 'darkgoldenrod') + xlab('Año') + ylab('Ventas') + ggtitle('Ventas de Apple'))
