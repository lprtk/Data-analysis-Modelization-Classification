############ R : Data Analysis Project ############ 

## Data loading 
bdf <- read.csv("bdf.csv", sep = ":", header = T)
univ <- read.csv("universite.csv", sep = ";", header = T)
villes <- read.table("villes.txt", header = T)

## Library used
library(questionr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(cluster)
library(ggplot2)
library(rgl)

### 1) Family Budget Survey 1994-1995 ###

# Selection of our variables
bdf <- bdf[,c("COEF", "DOMTRAV","TYPMEN2","CC","REVTOT","DIPLOPR","DIPLOCJ")]

# Removing missing data for our variables
bdf <- na.omit(bdf)

# Convert categorical variables into factors
cols <- c("DOMTRAV","TYPMEN2","CC","DIPLOPR","DIPLOCJ")
bdf[cols]=lapply(bdf[cols], factor)

## Modelization

# Model without interaction
model <- glm(DOMTRAV~., data = bdf, family = binomial, weights = COEF)
summary(model)

# Model with interactions
model2 <- glm(DOMTRAV~TYPMEN2+CC+DIPLOPR+DIPLOCJ+REVTOT+CC:TYPMEN2+CC:REVTOT+DIPLOPR:REVTOT+DIPLOCJ:REVTOT, data = bdf, family = binomial, weights = COEF)
summary(model2)

### 2) Temperature and precipitation in France  ###

# Principal Component Analysis
res.pca = PCA(villes, graph = T, scale.unit = T, quanti.sup = c(13:16))
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variable color
                col.ind = "#696969"  # Color of individuals
                )
                
# Look at the representation on the other factorial axes
plot(res.pca, choix="ind", axes=2:3)
plot(res.pca, choix="var", axes=2:3)

# Explanatory graph of axis 2
# Allows you to see that it is the amplitude that explains axis 2
plot(res.pca,choix="ind", habillage=16, cex=0.7)

# Display of eigenvalues 
# % of variance explained by each factorial axis
eig.pca <- get_eigenvalue(res.pca)
eig.pca

# Graph of the inertia explained (in %) by each factorial axis
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Contributions to the factor axes
res.pca$ind$contrib
res.pca$var$contrib

# Correlation matrix
corrplot(res.pca$ind$contrib, is.corr=FALSE) 
corrplot(res.pca$var$contrib, is.corr=FALSE) 

# Graphs of contributions
fviz_contrib(res.pca, choice = "var", axes = 1)
fviz_contrib(res.pca, choice = "var", axes = 2)
fviz_contrib(res.pca, choice = "var", axes = 1:2)

fviz_contrib(res.pca, choice = "ind", axes = 1)
fviz_contrib(res.pca, choice = "ind", axes = 2)
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_ind(res.pca, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Cos2: quality of representation
res.pca$ind$cos2[,1:2]
res.pca$var$cos2[,1:2]

res.pca$ind$cos2[,1]+res.pca$ind$cos2[,2]
res.pca$var$cos2[,1]+res.pca$var$cos2[,2] 

fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Avoids overlapping text
              )
fviz_pca_ind (res.pca, pointsize = "cos2",
              pointshape = 21, fill = "#E7B800",
              repel = TRUE # Avoids overlapping text
              )
              
corrplot(res.pca$ind$cos2, is.corr=FALSE) 
corrplot(res.pca$var$cos2, is.corr=FALSE)

## Clustering and classification
km <- kmeans(villes,3)
km

# Find the optimal number of classes with the silouhette method
fviz_nbclust(villes, kmeans, method = "silouhette")

# Hierarchical classification
hc <- hclust(dist(villes),method="ward.D")
hc
plot(hc)

res.hcpc <- HCPC(res.pca, nb.clust = 3, graph = TRUE)

# Clustering graph
fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoids overlapping text
             show.clust.cent = TRUE,  # Shows the center of the clusters
             palette = "jco",         # Color palette
             ggtheme = theme_minimal(),
             main = "Factor map")

# 3D plot
hcc <- rbind(villes, km$centers)
hcc
plot3d(villes, col=c('red', 'green3', 'blue'))


### 3) Universities ###

rownames(univ)<-univ[,1]
univ<-univ[,-1]

# Correspondence Analysis 
acf=CA(univ, graph = T, quanti.sup = c(7:12))
fviz_ca_biplot(acf, map ="rowprincipal",
               repel = TRUE,
               arrow = c(FALSE, TRUE))

# Egeinvalues
eig.acf <- get_eigenvalue(acf)
eig.acf

# Graph of the inertia explained (in %) by each factorial axis
fviz_eig(acf, addlabels = TRUE, ylim = c(0, 50))

# Contributions
acf$row$contrib
acf$col$contrib

fviz_ca_row(acf, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

fviz_ca_col(acf, col.col = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

corrplot(acf$col$contrib, is.corr=FALSE)

# Cos2
acf$row$cos2[,1]+acf$row$cos2[,2]

acf$col$cos2[,1]+acf$col$cos2[,2]

fviz_ca_row(acf, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

fviz_ca_col(acf, col.col = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

corrplot(acf$col$cos2, is.corr=FALSE) 
