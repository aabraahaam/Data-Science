require(cluster)
require(fpc)
require(factoextra)
require(dplyr)
library(dendextend)
require(ggrepel)
library(RWeka)
require(NbClust)
  viajeros <- read.csv('viajeros.csv')
summary(viajeros)
viajerosna <- na.omit(viajeros)
viajerosna <- viajerosna[,4:32]
viajerosna_mas =viajerosna[sample(1:nrow(viajerosna), 1000,replace=FALSE),]
summary(viajerosna_mas)
#Calculamos algunos estadisticos
viajerosna_mas_stats = data.frame(
  Min = apply(viajerosna_mas, 2, min), # mínimo
  Med = apply(viajerosna_mas, 2, median), # mediana
  Mean = apply(viajerosna_mas, 2, mean), # media
  SD = apply(viajerosna_mas, 2, sd), # desviación típica
  Max = apply(viajerosna_mas, 2, max) # máximo
)
viajerosna_mas_stats = round(viajerosna_mas_stats, 1)
head(viajerosna_mas_stats)
#Tipificamos
viajeros_tip=scale(viajerosna_mas)
summary(viajeros_tip)
viajeros_tip_stats = data.frame(
  Min = apply(viajeros_tip, 2, min), # mínimo
  Med = apply(viajeros_tip, 2, median), # mediana
  Mean = apply(viajeros_tip, 2, mean), # media
  SD = apply(viajeros_tip, 2, sd), # desviación típica
  Max = apply(viajeros_tip, 2, max) # máximo
)
viajeros_tip_stats = round(viajeros_tip_stats, 1)
head(viajeros_tip_stats)

viajeros_tip=as.data.frame(viajeros_tip)
d = dist(viajeros_tip, method = "euclidean")
churn.hc = hclust(d, method = "ward.D2" )
plot(viajeros.hc, cex = 0.6, hang = -1, main="Dendrograma - hclust")
rect.hclust(viajeros.hc, k=3, border = 2:4)
rect.hclust(viajeros.hc,k=2,border=2:4)
grp = cutree(viajeros.hc, k = 2)
table(grp)

fviz_cluster(list(data = viajeros_tip, cluster = grp), stand = FALSE, geom = "point", pointsize = 1,title="Clusters generados - hclust")


## AGNES
# library("cluster") # si no estuviese cargada
# Cálculo
viajeros.agnes = agnes(viajeros_tip, method = "ward")
# Coeficiente de aglomeración
viajeros.agnes$ac
pltree(viajeros.agnes, cex = 0.6, hang = -1, main = "Dendrograma - AGNES")
rect.hclust(viajeros.agnes, k = 2, border = 2:4)
grp.ag = cutree(as.hclust(viajeros.agnes), k = 2)
table(grp.ag)

#Diana--------------------------
# Cálculo
viajeros.diana = diana(viajeros_tip)
#coeficiente divisivo
viajeros.diana$dc
pltree(viajeros.diana, cex = 0.6, hang = -1, main = "Dendrograma - DIANA")
rect.hclust(viajeros.diana, k = 2, border = 2:4)
grp.diana = cutree(as.hclust(viajeros.diana), k = 2)
table(grp.diana)


#Métodos no jerarquicos
km.q = kmeans(viajerosna_mas, 3, nstart = 25)
km.q$cluster
km.q$size
tab = table(rownames(viajerosna_mas), km.q$cluster) # para
head(tab)
km.q$centers
#plot(viajerosna_mas,col=km.q$cluster, pch = 19, frame = FALSE,
 #    main = "k medias con k = 3 para AC s/AT vs CteVtas s/Vtas")
#Agregamos los centroides
#points(km.q$centers, col = 1:3, pch = 16, cex = 2)
fviz_nbclust(viajerosna_mas, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
fviz_cluster(km.q, data=viajerosna_mas, labelsize=3, repel=TRUE)

pam.q = pam(viajerosna, 3)
clusplot(pam.q, main = "PAM de k = 2", color = TRUE)
fviz_cluster(pam.q, data=viajerosna_mas, labelsize=2, repel=TRUE)
plot(silhouette(pam.q), col = 2:4)
fviz_silhouette(silhouette(pam.q))


#CLARA
Nb.viajeros=NbClust(viajeros_tip, distance = "euclidean", min.nc = 2,
                 max.nc = 10, method = "complete", index ="all")
fviz_nbclust(Nb.viajeros) + theme_minimal() +
  labs(x="Número k de clusters", y="Frecuencia")

viajeros.clara=clara(viajerosna, 4, samples=200)
fviz_cluster(viajeros.clara, stand = TRUE, geom = "point", pointsize = 1)
plot(silhouette(viajeros.clara), col = 2:5, main = "Gráfico de perfil")
fviz_silhouette(silhouette(viajeros.clara))
clusters<-viajeros.clara$clustering
clusters<-as.data.frame(clusters)                        
resultado<-na.omit(viajeros)
resultado<-data.frame(resultado,clusters)
table(resultado$clusters,resultado$SEXO)
table(resultado$clusters,resultado$OCUPACION)
table(resultado$clusters,resultado$INGRESOS)
table(resultado$clusters,resultado$PAIS_RESID_AGRUP)
table(resultado$clusters,resultado$ALOJ_CATEG_1)
