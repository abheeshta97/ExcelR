#Crimes Data
pacman::p_load(tidyverse, cluster, factoextra)

crimesDF <- read.csv(file.choose())
crimesDF_original <- read.csv(file.choose())
View(crimesDF_original)
head(crimesDF)
#Setting the US state names as indices
rownames(crimesDF) <- crimesDF$X
crimesDF$X <- NULL
head(crimesDF)
View(crimesDF)

#Standardizing the data
crimesDF <- scale(crimesDF)
head(crimesDF)
crimesDF <- na.omit(crimesDF)

#Doesnt work with k-means. Elbow curve has a peak
fviz_nbclust(crimesDF, kmeans, method = "wss")

#Optimal number of clusters
fviz_nbclust(crimesDF, FUN = hcut, method = "wss") 
#4 clusters
fviz_nbclust(crimesDF, FUN = hcut, method = "silhouette") #2 - best, 4 2nd optimal
crimesDF.gapStat <- clusGap(crimesDF, FUN = hcut, nstart = 25, 
                            K.max = 10, B = 50)
fviz_gap_stat(crimesDF.gapStat)
#3 is best - 2nd is 4 clusters

crimes_d <- dist(crimesDF, method = "euclidean") #Dist Matrix
crimes_fit <- hclust(crimes_d, method = "complete")
plot(crimes_fit, cex = 0.6, hang = -1)
rect.hclust(crimes_fit, k = 4, border = 2:5)

crimes_sub_grp <- cutree(crimes_fit, k = 4)
table(crimes_sub_grp)

crime_cluster <- as.matrix(crimes_sub_grp)

crimesDF_final <- data.frame(crimesDF, crime_cluster)
crimesDF_final

subset(crimesDF_final, crime_cluster == 1)
subset(crimesDF_final, crime_cluster == 2)
subset(crimesDF_final, crime_cluster == 3)
subset(crimesDF_final, crime_cluster == 4)


