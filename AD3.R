############ R : Data Analysis Project ############ 

## Data loading
chiens =read.table("chiens.txt", header = T)

## Library used
library(FactoMineR)
library(factoextra)

## Database visualization
summary(chiens)
dim(chiens)

# Convert categorical variables into factors
for(k in 1:7) chiens[,k]=as.factor(chiens[,k])

# Make tables of row and column profiles
res.mca=MCA(chiens,quali.sup=7)

# Eigenvalue and variance associated with the axes
eig.val <- get_eigenvalue(res.mca)
print(eig.val)

fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))

# Biplot 
fviz_mca_biplot (res.mca, repel = TRUE, 
                 ggtheme = theme_minimal())

res.mca$var$coord

# Cos2: quality of representation
res.mca$var$cos2

# Contributions to the factor axes
res.mca$var$contrib

# Correlation between the variables and the main axes of the MCA
fviz_mca_var (res.mca, choice = "mca.cor",
              repel = TRUE, 
              ggtheme = theme_minimal ())



# Quality of category representation
# Staining according to cos2
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())


# Contributions of variables to dimension 1
fviz_contrib (res.mca, choice = "var", axes = 1, top = 7)

# Contributions of variables to dimension 2
fviz_contrib (res.mca, choice = "var", axes = 2, top = 7)

# Total contribution to dimensions 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 7)

fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())

# Graph of individuals
ind <- get_mca_ind (res.mca)
ind

res.mca$ind$coord

# Quality of representation
res.mca$ind$cos2

# Contributions
res.mca$ind$contrib

# Quality and contributions of individuals
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             ggtheme = theme_minimal())

fviz_mca_ind(res.mca, col.ind = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             ggtheme = theme_minimal())

# Cos2 of individuals
fviz_cos2(res.mca, choice = "ind", axes = 1, top = 7)

# Contribution of individuals to the dimensions
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 7)

# Elipses of confidence
fviz_mca_ind (res.mca,
              label = "none", # hide the text of individuals
              habillage = "TAI", # colorer par groupes
              palette = c ("#00AFBB", "#E7B800","#FC4E07" ),
              addEllipses = TRUE, ellipse.type = "confidence",
              ggtheme = theme_minimal ())

# Modify variables
fviz_ellipses(res.mca, c("TAI", "POI","VEL"),
              geom = "point")

# Descriptions of dimensions
res.desc <- dimdesc (res.mca, axes = c(1,2))

# Descriptions of dimension 1
res.desc[[1]]

# Descriptions of dimension 2
res.desc[[2]]
