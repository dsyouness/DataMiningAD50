#Librairie FactoMineR
library(FactoMineR)
library(factoextra)

library(gplots)
library(corrplot)

#Librairie ADE4, Cluster
library(cluster)
library(ade4)

bdtmp_multim = read.csv("~/AD50/AFC/budget_temps_multimedia.csv", header=TRUE, row.names=1, sep=";")
t_bdtmp_multim = as.table(as.matrix(bdtmp_multim))
balloonplot(t(t_bdtmp_multim) ,main ="bdtmp_multim", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

##############################   TEST KHI-DEUX  ########################################

test <- chisq.test(bdtmp_multim[1:8,])  
test #p-value of 2.2*10^-16 <<< 0.01 donc il y a dépandance entre les variables lignes et colonnes


test$statistic #: la statistique du Chi2.
test$parameter #: le nombre de degrés de libertés.
test$p.value #: la p-value.
test$observed #: la matrice observée de départ.
test$expected #: la matrice attendue sous l'hypothèse nulle d'absence de biais.

##############################   AFC  ########################################

#Methode AFC FactoMineR
res.ca <- CA(bdtmp_multim, row.sup = 9:nrow(bdtmp_multim),graph=FALSE)


#Methode AFC ADE4
res.ca <- dudi.coa(bdtmp_multim[1:8,],scannf= FALSE, nf = 2) # hclust eclust



#Plots
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 70)) # Inerties

fviz_ca_biplot(res.ca, repel = TRUE) #Sans fleches

fviz_ca_biplot(res.ca, arrow = c(TRUE, TRUE),repel = TRUE)#Avec fleches

#Cos² ROW

fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

corrplot(row$cos2, is.corr=FALSE)

fviz_cos2(res.ca, choice = "row", axes = 1:2)

#Cos² COL

fviz_ca_col(res.ca, col.col = "cos2", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)

fviz_cos2(res.ca, choice = "col", axes = 1:2)


#Contribution

corrplot(row$contrib, is.corr=FALSE)


# Contributions of rows to dimension 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)
# Total contribution to dimension 1 and 2
fviz_contrib(res.ca, choice = "row", axes = 1:2, top = 10)

fviz_ca_row(res.ca, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)



fviz_contrib(res.ca, choice = "col", axes = 1:2) #COL


#Aides à l'interpretation

print(res.ca)

summary(res.ca)



##############################   HCLUST / KMEANS / SILHOUETTE  ########################################

#Methode CLUSTER FactoMiner
clust0 <- HCPC(res.ca,consol = FALSE) #kmeans FALSE
clust <- HCPC(res.ca,consol = TRUE) #kmeans TRUE

clust$desc.axes
clust$desc.ind
head(clust$data.clust, 10) #Données d’origine avec la colonne class
clust$desc.var$quanti #Variables quantitatives décrivant le plus chaque cluster
clust$desc.axes$quanti #Axes principaux associées aux clusters
clust$desc.ind$para #Individus représentatifs de chaque groupe

plot(clust, choice = "3D.map")

fviz_cluster(clust,repel = TRUE,  # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "K-means")


#Methode CLUSTER Factoextra 
res.hc <- eclust(res.ca$li[,1:2], "hclust", k = 4,
                 method = "ward.D2", graph = FALSE)
fviz_dend(res.hc, rect = TRUE, show_labels = TRUE) 


#Methode CLUSTER "Stats"
dist <- dist(res.ca$li[,1:2],method="euclidian")
dist <- dist^2
classif <- hclust(d= dist, method="ward.D2")
plot(classif, main="Dendrogramme \n methode de Ward", xlab = "Les professions",
     sub = "")

#K-means marche avec AFC ADE4

#K-means run = 1
kmeans1 <- kmeans(res.ca$li[,1:2], centers= 4 , nstart =1)
print(kmeans)

#K-means run = 2
kmeans2 <- kmeans(res.ca$li[,1:2], centers= 4 , nstart =2)
print(kmeans)

#Plot K-means run = 1
km.res1 <- eclust(res.ca$li[,1:2], "kmeans", k =4,nstart = 1, graph = TRUE)


#Plot K-means run = 2
km.res2 <- eclust(res.ca$li[,1:2], "kmeans", k =4,nstart = 2, graph = TRUE)

#Plot Silhouette run = 1
fviz_silhouette(km.res1, label=TRUE)

#Plot Silhouette run = 2
fviz_silhouette(km.res1, label=TRUE)



