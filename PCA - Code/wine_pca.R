# Perform Principal component analysis and 
# perform clustering using first 
# 3 principal component scores

library(FactoMineR)
library(factoextra)
library(ggpubr)
library(ggthemes)

wine <- read.csv(file.choose()) #wine.csv

View(wine)

wine.1 <- wine[-c(1)] #Removing the type column
View(wine.1)

wine.std <- scale(wine.1) #Standardizing the data
View(wine.std)

### Performing PCA

wine.pca <- PCA(wine.std, ncp = 3, graph = T)

### Scree Plot of Eigen Values

fviz_eig(wine.pca, ggtheme = theme_fivethirtyeight(),
         addlabels = TRUE, ylim = c(0, 40))

### Hierarchical Clustering

wine.hcpc <- HCPC(wine.pca, graph = F)

fviz_dend(wine.hcpc, cex = 0.7, palette = "npg",
          rect = T, rect_fill = T, rect_border = "npg")

### K Mean Clustering

fviz_cluster(wine.hcpc,
             repel = TRUE,           
             show.clust.cent = TRUE, 
             palette = "jco",      
             ggtheme = theme_fivethirtyeight(),
             main = "K-Means Clustering"
)
