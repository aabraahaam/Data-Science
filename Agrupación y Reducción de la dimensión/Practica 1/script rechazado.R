##Agnes
```{r}
viajeros.agnes = agnes(viajeros_tip, method = "ward")
viajeros.agnes$ac
pltree(viajeros.agnes, cex = 0.6, hang = -1, main = "Dendrograma - AGNES")
rect.hclust(viajeros.agnes, k = 2, border = 2:4)
grp.ag = cutree(as.hclust(viajeros.agnes), k = 2)
table(grp.ag)
```

##Diana
```{r}
viajeros.diana = diana(viajeros_tip)
viajeros.diana$dc
pltree(viajeros.diana, cex = 0.6, hang = -1, main = "Dendrograma - DIANA")
rect.hclust(viajeros.diana, k = 2, border = 2:4)
grp.diana = cutree(as.hclust(viajeros.diana), k = 2)
table(grp.diana)
```

```{r}
#Métodos no jerarquicos
km.q = kmeans(viajerosna_mas, 2, nstart = 25)
km.q$size
tab = table(rownames(viajerosna_mas), km.q$cluster) # para
head(tab)
km.q$centers

fviz_nbclust(viajerosna_mas, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
fviz_cluster(km.q, data=viajerosna_mas, labelsize=3, repel=TRUE)

pam.q = pam(viajerosna, 3)
clusplot(pam.q, main = "PAM de k = 3", color = TRUE)
fviz_cluster(pam.q, data=viajerosna_mas, labelsize=2, repel=TRUE)
plot(silhouette(pam.q), col = 2:4)
fviz_silhouette(silhouette(pam.q))
```
