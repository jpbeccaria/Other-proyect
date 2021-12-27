# Entanglement with bootstrap
# By Juan Pablo Beccaria

library("readxl")
library("dendextend")

# Build two dendrograms from two matrices and calculate their correlation coefficient of Baker.


#######################################################################
#
#     Making the trees and getting the entanglement
#
#######################################################################

# Setting working directories

setwd("C:/Users/bk_ri/Dropbox/Papaer JP OC/Excells para R")  



# Apertura de archivos, calcula de distancias euclidias, y creacion de dendrogramas

matriz_temp1<- read_excel("dia9_c_3cp_3v.xlsx")    
matriz1 <-as.matrix(matriz_temp1)


matriz_temp2 <- read_excel("dia9_c_3cp_3v.xlsx")    
matriz2 <-as.matrix(matriz_temp2)


res.dist1 <- dist(matriz1, method = "euclidean")
res.dist2 <- dist(matriz2, method = "euclidean")


hc1 <- hclust(res.dist1, method = "ward.D2")
hc2 <- hclust(res.dist2, method = "ward.D2")



dend1 <- as.dendrogram (hc1)
plot(dend1)

dend2 <- as.dendrogram (hc2)
plot(dend2)


the_cor1<-cor_bakers_gamma(dend1, dend1)
the_cor2<-cor_bakers_gamma(dend1, dend2)


##### Baker's gamma distribution under H0 ########## Para esto sacar los numerales de abajo####

#R <- 100
#cor_bakers_gamma_results <- numeric(R)
#dend_mixed <- dend1
#for(i in 1:R) {
#  dend_mixed <- sample.dendrogram(dend_mixed, replace = FALSE)
#  cor_bakers_gamma_results[i] <- cor_bakers_gamma(dend1, dend_mixed)
#}
#plot(density(cor_bakers_gamma_results),
#     main = "Baker's gamma distribution under H0",
#     xlim = c(-1,1))
#abline(v = 0, lty = 2)
#abline(v = the_cor, lty = 2, col = 2)
#abline(v = the_cor2, lty = 2, col = 4)
#legend("topleft", legend = c("cor", "cor2"), fill = c(2,4))
#round(sum(the_cor2 < cor_bakers_gamma_results)/ R, 4)


# Datos del bootstrap; R = número de repeticiones. Es bastante rápido, se puede subir de 100 si se quiere.

set.seed(23801)

R <- 100
dend1_labels <- labels(dend1)
dend2_labels <- labels(dend2)
cor_bakers_gamma_results <- numeric(R)
for(i in 1:R) {
  sampled_labels <- sample(dend1_labels, replace = TRUE)
  # members needs to be fixed since it will be later used in nleaves  
  # OJOOO!! Replace debe ser falsa para que no cree casos aberrantes
  dend_mixed1 <- sample.dendrogram(dend1, 
                                   dend_labels=dend1_labels,
                                   fix_members=TRUE,fix_order=TRUE,fix_midpoint=FALSE,
                                   replace = FALSE, sampled_labels=sampled_labels
  )
  dend_mixed2 <- sample.dendrogram(dend2, dend_labels=dend2_labels,
                                   fix_members=TRUE,fix_order=TRUE,fix_midpoint=FALSE,
                                   replace = FALSE, sampled_labels=sampled_labels
  )                                    
  cor_bakers_gamma_results[i] <- cor_bakers_gamma(dend_mixed1, dend_mixed2, warn = FALSE)
}


cor_bakers_gamma(dend_mixed1, dend_mixed2, warn = FALSE)


# para IC 95 o 99 poner esto en lugar de lo de abajo por: 
######  CI99 <- quantile(cor_bakers_gamma_results, probs=c(.005,.995)) 
#####   CI95 <- quantile(cor_bakers_gamma_results, probs=c(.025,.975))

CI99 <- quantile(cor_bakers_gamma_results, probs=c(.005,.995)) 
CI99

par(mfrow = c(1,1))
plot(density(cor_bakers_gamma_results),
     main = "Baker's gamma bootstrap distribution",
     xlim = c(-1,1))
abline(v = CI99, lty = 2, col = 3)
abline(v = cor_bakers_gamma(dend1, dend2), lty = 2, col = 2)
legend("topleft", legend =c("99% CI", "Baker's Gamma Index"), fill = c(3,2))


cor_bakers_gamma(dend1, dend2)







