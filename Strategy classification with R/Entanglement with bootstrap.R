# Entanglement with bootstrap
# By Juan Pablo Beccaria

library("readxl")
library("dendextend")

# Makes dendograms from two data matrices (it is advisable to filter the database with any method to reduce dimensions such as PCA)
# Then, calculate de "entanglement" between them (Entanglement is a measure between 1 (full entanglement) and 0 (no entanglement). The exact behavior of the number depends on the L norm which is chosen.)
# Finally, using bootstrap, it gets the chance of getting this entanglement value by chance.


#######################################################################
#
#     Making the trees and getting the entanglement
#
#######################################################################


# Setting working directories

setwd("C:/Users/bk_ri/Dropbox/Papaer JP OC/Excells para R")  # Escribir la "direccion" con "/" y no con "\".


matriz_tempD9 <- read_excel("dia8_c.xlsx")    # abre el archivo excel
matrizD9 <-as.matrix(matriz_tempD9)


matriz_tempD1 <- read_excel("dia9_c_3cp_3v.xlsx")    # abre el archivo excel
matrizD1 <-as.matrix(matriz_tempD1)


k.res<-kmeans(matrizD1, 2, iter.max = 20, nstart = 50)
print(k.res)


res.distD9 <- dist(matrizD9, method = "euclidean")
res.distD1 <- dist(matrizD1, method = "euclidean")

hcD9 <- hclust(res.distD9, method = "ward.D2")
hcD1<- hclust(res.distD1, method = "ward.D2")

dend1 <- as.dendrogram (hcD9)
plot(dend1)

dend2 <- as.dendrogram (hcD1)
plot(dend2)

# Join with lines ends of both trees but no untanglement method applied yet!

tanglegram(dend1, dend2, common_subtrees_color_branches = TRUE)

lista <- dendlist(dend1, dend2)


# Se aplica un método de untangle. Por defecto es "2 steps". Por lo que googlíe es el "mejor"
# hace todas las combinaciones PERMITIDAS de giros de nodos hasta que encuientra el entanglement mas chico

# dends_15_51 %>% untangle(method = "DendSer") %>% entanglement # lower is better

lista %>% untangle(method = "step2side") %>% entanglement # lower is better


x <- lista %>% untangle(method = "step2side") 
x %>% plot(main = paste("entanglement =", round(entanglement(x), 2)))



untangle(dend1,dend2,method = "step2side")%>% entanglement



#######################################################################
#
# Es ese entanglement significativamente diferente del azar??
#
#######################################################################

matriz_tempD9 <- read_excel("dia9_c_3cp_3v.xlsx")    # abre el archivo excel
matrizD9 <-as.matrix(matriz_tempD9)


# Si acá se cambia el archivo que se lee, se puede ir cambiando las parejas que se comparan.


matriz_tempD1 <- read_excel("dia2_c.xlsx")    # abre el archivo excel
matrizD1 <-as.matrix(matriz_tempD1)

# Acá se arma y plotean los dendogramas, lo dejé porque es un "control" de que se cargaron bien los archivos.


res.distD9 <- dist(matrizD9, method = "euclidean")

res.distD1 <- dist(matrizD1, method = "euclidean")




hcD9 <- hclust(res.distD9, method = "ward.D2")
hcD1<- hclust(res.distD1, method = "ward.D2")



dend1 <- as.dendrogram (hcD9)
plot(dend1)

dend2 <- as.dendrogram (hcD1)
plot(dend2)

# Aca se setea lo necesario para el bootstrap, número de repeticiones, R = número de repeticiones.
# Se predefine R=10 porque lleva bastante tiempo, idealmente se podría subir.


R <- 10
dend1_labels <- labels(dend1)
dend2_labels <- labels(dend2)
entanglement_results <- numeric(R)
for(i in 1:R) {
  sampled_labels <- sample(dend1_labels, replace = TRUE)
  # members needs to be fixed since it will be later used in nleaves  
  #OJOOO!! Replace debe ser falsa para que no cree casos aberrantes
  dend_mixed1 <- sample.dendrogram(dend1, 
                                   dend_labels=dend1_labels,
                                   fix_members=TRUE,fix_order=TRUE,fix_midpoint=FALSE,
                                   replace = FALSE, sampled_labels=sampled_labels
  )
  dend_mixed2 <- sample.dendrogram(dend2, dend_labels=dend2_labels,
                                   fix_members=TRUE,fix_order=TRUE,fix_midpoint=FALSE,
                                   replace = FALSE, sampled_labels=sampled_labels
  )                                    
  entanglement_results[i] <- untangle(dend_mixed1, dend_mixed2,method = "step2side")%>% entanglement
}

# para IC 95 o 99 poner esto en lugar de lo de abajo por:
######  CI99 <- quantile(cor_bakers_gamma_results, probs=c(.005,.995)) 
#######################################################           CI95 <- quantile(cor_bakers_gamma_results, probs=c(.025,.975))
untangle(dend1, dend2,method = "step2side")%>% entanglement
CI99 <- quantile(entanglement_results, probs=c(.005,.995)) 
CI99

par(mfrow = c(1,1))
plot(density(entanglement_results),
     main = "entaglement bootstrap distribution",
     xlim = c(-1,1))
abline(v = CI99, lty = 2, col = 3)
abline(v = untangle(dend1, dend2,method = "step2side")%>% entanglement, lty = 2, col = 2)

legend("topleft", legend =c("99% CI", "Baker's Gamma Index"), fill = c(3,2))
