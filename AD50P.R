library(FactoMineR)
library(gplots)
library(factoextra)
library(corrplot)

library(ade4)

bdtmp_multim = read.csv("~/AD50/AFC/budget_temps_multimedia.csv", header=TRUE, row.names=1, sep=";")
t_bdtmp_multim = as.table(as.matrix(bdtmp_multim))
balloonplot(t(t_bdtmp_multim) ,main ="bdtmp_multim", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

df= bdtmp_multim[1:8,]

##############################   TEST KHI-DEUX  ########################################

test <- chisq.test(bdtmp_multim)  
test #p-value of 2.2*10^-16 <<< 0.01 donc il y a dépandance entre les variables lignes et colonnes
test$statistic #: la statistique du Chi2.
test$parameter #: le nombre de degrés de libertés.
test$p.value #: la p-value.
test$observed #: la matrice observée de départ.
test$expected #: la matrice attendue sous l'hypothèse nulle d'absence de biais.

##############################   AFC  ########################################

res.ca <- CA(bdtmp_multim, row.sup = 9:nrow(bdtmp_multim))

res.ca <- CA(bdtmp_multim[1:8,])

res.ca <- dudi.coa(bdtmp_multim[1:8,],scannf= FALSE, nf = 2) # hclust eclust

row <- get_ca_row(res.ca)

col <- get_ca_col(res.ca)


fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 70)) # Inerties

fviz_ca_biplot(res.ca, repel = TRUE) #Symetric

fviz_ca_biplot(res.ca, arrow = c(TRUE, TRUE),
               repel = TRUE)

#Cos2 ROW

fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

corrplot(row$cos2, is.corr=FALSE)

fviz_cos2(res.ca, choice = "row", axes = 1:2)

#Cos2 COL

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


#Interpretation

print(res.ca)

summary(res.ca)



##############################   HCLUST / KMEANS / SILHOUETTE  ########################################

clust0 <- HCPC(res.ca,consol = FALSE) #kmeans FALSE
clust <- HCPC(res.ca,consol = TRUE) #kmeans TRUE

clust$desc.axes
clust$desc.ind
head(clust$data.clust, 10) #Données d’origine avec la colonne class
clust$desc.var$quanti #Variables quantitatives décrivant le plus chaque cluster
clust$desc.axes$quanti #Axes principaux associées aux clusters
clust$desc.ind$para #Individus représentatifs de chaque groupe

plot(clust, choice = "3D.map")

fviz_cluster(clust,repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "K-means")




dist <- dist(res.ca$li[,1:2],method="euclidian")
dist <- dist^2
classif <- hclust(d= dist, method="ward.D2")
plot(classif, main="Dendrogramme \n methode de Ward", xlab = "Les professions",
     sub = "")

kmeans <- kmeans(res.ca$li[,1:2], centers= 4 , nstart =2)
print(kmeans)


library(cluster)
km.res <- eclust(res.ca$li[,1:2], "kmeans", k =4,
                 nstart = 2, graph = FALSE) #nstart=1 meme résultat que 
fviz_cluster(km.res, frame.type = "norm", frame.level = 0.68)
#sil <- silhouette(km.res$cluster, dist(res.ca$li[,1:2],method="euclidian")^2)
fviz_silhouette(km.res, label=TRUE)


res.hc <- eclust(res.ca$li[,1:2], "hclust", k = 4,
                 method = "ward.D2", graph = FALSE)
fviz_dend(res.hc, rect = TRUE, show_labels = TRUE) 

