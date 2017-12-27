library(plotly)
data <-read.csv('tterreno_euro.csv')
data <- na.omit(data)
summary(data)
dim(data)
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
data.pca <- prcomp(data[,3:15],
                 center = TRUE,
                 scale. = TRUE) 
summary(data.pca)
biplot(data.pca)
# Scatter plot observations by components 1 and 2
plot(data.pca$x[, c(1, 2)], 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(data.pca$x[, c(1, 3)],
     xlab = "PC1", ylab = "PC3")

pve <- data.pca$sdev**2
ve <- pve/sum(pve)
par(mfrow = c(1, 1))
# Plot variance explained for each principal component
plot(ve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(ve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

kmedias <- kmeans(scale(data[,3:15]),2,nstart = 20)
plot(data.pca$x[, c(1, 2)],col=kmedias$cluster+1, 
     xlab = "PC1", ylab = "PC2")
wss <-0
for (i in 1:15) {
  km.out <- kmeans(scale(data[,3:15]), centers = i, nstart=20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

kmedias <- kmeans(scale(data[,3:15]),4,nstart = 20)
plot(data.pca$x[, c(1, 2)],col=kmedias$cluster+1, 
     xlab = "PC1", ylab = "PC2")
install.packages('plot3D')
library(plot3D)
summary(data.pca$x)
str(data.pca$x)
x <- data.pca$x[,1]
x <- as.matrix(x)
y <-data.pca$x[,2]
y <- as.matrix(y)
z <-data.pca$x[,3]
z <- as.matrix(z)
z
surf3D(x = x,
       y = y,
       z = z)
prueba <-as.data.frame(data.pca$x)
prueba$clust <- as.factor(kmedias$cluster)

plot_ly(data =prueba ,x = ~PC1,
        y = ~PC2, z = ~PC3,color = ~clust,type = "scatter3d",showlegend = FALSE)

plot_ly(data =prueba ,x = ~PC1,
        y = ~PC2, z = ~PC3,color = ~clust2,type = "scatter3d",showlegend = FALSE)
