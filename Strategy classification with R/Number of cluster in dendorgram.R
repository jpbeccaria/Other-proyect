

# Look for the best number of cluster in a dendogra by many methods 

library("readxl")
library("factoextra")


library("readxl")
library("factoextra")


setwd("C:/Users/bk_ri/Dropbox/Papaer JP OC/Datasets publicos")  # Escribir el directorio de trabajo usando "/" y no "\".

matriz_tempD9 <- read_excel("data_display_dia4_2000ms.xlsx")    # abre el archivo excel para formar el dendrograma del día a analizar


matriz <-as.matrix(matriz_tempD9)

# Compute dissimilarity matrix
res.dist <- dist(matriz, method = "euclidean")

# Compute hierarchical clustering
hc <- hclust(res.dist, method = "ward.D")

# Visualize
plot(hc, cex = 1)

dendo <-as.dendrogram(hc)
plot(dendo)


######

GraphHc<-eclust(matriz, FUNcluster = "hclust", hc_metric = "euclidean")

fviz_cluster(GraphHc )



################################

#   Enhanced k-means clustering

#################################
res.km <- eclust(matriz, "kmeans", nstart = 25)


(res.km$gap_stat)

#################################

# Silhouette method #

#################################

fviz_nbclust(matriz, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")


#################################

# MÉTODO GAP STATISTIC #

#################################

fviz_nbclust(matriz, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

############################




