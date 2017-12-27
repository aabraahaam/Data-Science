library(factoextra)
library(FactoMineR)
library(ggplot2)
datos <- read.csv('tterreno_euro.csv')
datos <- na.omit(datos)
datos$modelo <- NULL
datos$marca <- NULL
summary(datos)
q.acp = PCA(datos, graph=T, scale.unit = T)
get_eig(q.acp)

fviz_eig(q.acp, addlabels=T, hjust=-0.3)+
  labs(title="Gráfico de sedimentación")+
  theme_minimal()

fviz_contrib(q.acp, choice="var", axes=1) +
  labs(title="Contribuciones a la explicación del 1er factor")

fviz_contrib(q.acp, choice="var", axes=2) +
  labs(title="Contribuciones a la explicación del 2º factor")

fviz_contrib(q.acp, choice="var", axes=3) +
  labs(title="Contribuciones a la explicación del 3er factor")

q.acp$var$cor[,1:5]

ACP = q.acp$ind$coord[,1:5]
ACP <- as.data.frame(ACP)
ACP$marca <- datos$marca
ggplot(ACP,aes(x=Dim.1,y=Dim.2)) + geom_point(color=ACP$marca)
