# Liceth Cristina Mosquera Galvis, Code: 201910046228
# Master of Science - Data Science and Analytics
# Octubre 2019

# Bibliotecas-------------------------------------------------
library(cluster)    # clustering algorithms
library(data.table)
library(factoextra) # clustering algorithms & visualization
library(GGally)
library(ggfortify)
library(ggcorrplot) #PCA
library(ggplot2)
library(ggpubr)
library(ggraph)
library(ggmap) #graficar mapas
library(gridExtra)
library(igraph)
library(tidyverse)  # data manipulation
library(tidygraph)

############### Exploración ###############
#Preparación de datos
df=iris[c(1, 2, 3, 4)]
df = na.omit(df)
data_iris=data.table(iris)
ggpairs(data_iris,columns = 1:5,mapping=aes(colour=Species))
summary(iris)
sapply(iris[,-5], var)
y=as.numeric(iris[,5])

## graficar de a dos variables
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()

################## Encontar el numero óptimo de clusters #####

set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(df[,3:4],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(df, kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(df, kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Kmeans sin estandarizar####
not_norm_cluster=kmeans(data_iris[,1:4,with=F],3)
data_iris$cluster=as.factor(not_norm_cluster$cluster)
ggpairs(data_iris,columns = 1:5,mapping=aes(colour=cluster))
 
table(not_norm_cluster$cluster,iris$Species)

# Inspect 'clusters'
str(not_norm_cluster)
print(not_norm_cluster)
#visulización
fviz_cluster(not_norm_cluster, data = df)

#Estadística Descriptiva del Cluster Finaliris[c(1, 2, 3, 4)]
 df%>%
  mutate(Cluster = not_norm_cluster$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


#estandarizar####
df_sca = scale(df)
head(df_sca)
norm_cluster=kmeans(scale(data_iris[,1:4,with=F],scale = T),3)
data_iris$cluster=as.factor(norm_cluster$cluster)
ggpairs(data_iris,columns = 1:5,mapping=aes(colour=cluster))

################# PCA Iris #######

# PCA #Scaling data before PCA is usually advisable! 
pr_out =prcomp(df, center = TRUE, scale = FALSE, rank. = 2) 
summary(pr_out)
# Screeplot
pr_var =  pr_out$sdev ^ 2
pve = pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
       ylim = c(0,1), type = 'b')
# Grafica PVE acumulada
plot(cumsum(pve), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')
# Rotate loadings
rot_loading = varimax(pr_out$rotation[, 1:2])
rot_loading

################## Encontar el numero óptimo de clusters para PCA #####
set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(pr_out[["x"]],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k) PCA", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(pr_out[["x"]], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Aplicar Kmeans
pc = PCAmix(X.quanti = df, X.quali = NULL, ndim = 2, rename.level = FALSE,weight.col.quanti = NULL, 
            weight.col.quali = NULL, graph = TRUE)
plot(pc)
pca_cluster=kmeans(pr_out[["x"]], centers = 3, nstart = 50)
data_iris$cluster=as.factor(pca_cluster$cluster)
ggpairs(data_iris,columns = 1:5,mapping=aes(colour=cluster))

not_norm_cluster$tot.withinss
table( iris$Species, not_norm_cluster$cluster, dnn = c( "Original", "Kmeans_not_norm" ) )
norm_cluster$tot.withinss
table( iris$Species, norm_cluster$cluster, dnn = c( "Original", "Kmeans_norm" ) )
pca_cluster$tot.withinss
table( iris$Species, pca_cluster$cluster, dnn = c( "Original", "Kmeans_PCA" ) )

############### Exploración wine ###############
#Preparación de datos
setwd("D:\\Dcorporativo\\Personal\\Maestria\\AprendizajeAutomatico\\Entrega3")
wine = read.table("./wine_orange.csv",  sep=",",header=T)
df=wine[,2:14]
Class=wine[,1]
dim(df)
df = na.omit(df)

data_wine=data.table(df)
#ggpairs(data_wine,columns = 1:13,mapping=aes(colour=Class))
summary(df)
sapply(df[,-13], var)
y=as.numeric(Class)

## graficar de a dos variables
ggplot(df,aes(x = df[,1], y = df[,2], col= y)) + geom_point()
ggplot(df,aes(x = df[,3], y = df[,4], col= y)) + geom_point()

################## Encontar el numero óptimo de clusters #####

set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(df[,1:13],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(df, kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(df, kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Kmeans sin estandarizar####
not_norm_cluster=kmeans(data_wine[,1:4,with=F],3)
data_wine$cluster=as.factor(not_norm_cluster$cluster)
ggpairs(data_wine,columns = 1:5,mapping=aes(colour=cluster))

table(not_norm_cluster$cluster,y)

# Inspect 'clusters'
str(not_norm_cluster)
print(not_norm_cluster)
#visulización
fviz_cluster(not_norm_cluster, data = df)

#Estadística Descriptiva del Cluster Finaliris[c(1, 2, 3, 4)]
df%>%
  mutate(Cluster = not_norm_cluster$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


#estandarizar####
df_sca = scale(df)
head(df_sca)
norm_cluster=kmeans(scale(data_wine[,1:4,with=F],scale = T),3)
data_wine$cluster=as.factor(norm_cluster$cluster)
ggpairs(data_wine,columns = 1:5,mapping=aes(colour=cluster))

################# PCA Wine #######

# PCA #Scaling data before PCA is usually advisable! 
pr_out =prcomp(df, center = TRUE, scale = FALSE, rank. = 2) 
summary(pr_out)
# Screeplot
pr_var =  pr_out$sdev ^ 2
pve = pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
# Grafica PVE acumulada
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')
# Rotate loadings
rot_loading = varimax(pr_out$rotation[, 1:2])
rot_loading

################## Encontar el numero óptimo de clusters para PCA #####
set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(pr_out[["x"]],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k) PCA", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(pr_out[["x"]], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Aplicar Kmeans
pc = PCAmix(X.quanti = df, X.quali = NULL, ndim = 2, rename.level = FALSE,weight.col.quanti = NULL, 
            weight.col.quali = NULL, graph = TRUE)
plot(pc)
pca_cluster=kmeans(pr_out[["x"]], centers = 3, nstart = 50)
data_wine$cluster=as.factor(pca_cluster$cluster)
ggpairs(data_wine,columns = 1:5,mapping=aes(colour=cluster))

not_norm_cluster$tot.withinss
table( y, not_norm_cluster$cluster, dnn = c( "Original", "Kmeans_not_norm" ) )
norm_cluster$tot.withinss
table( y, norm_cluster$cluster, dnn = c( "Original", "Kmeans_norm" ) )
pca_cluster$tot.withinss
table( y, pca_cluster$cluster, dnn = c( "Original", "Kmeans_PCA" ) )
############### Exploración Ecoli ###############
#Preparación de datos
setwd("D:\\Dcorporativo\\Personal\\Maestria\\AprendizajeAutomatico\\Entrega3")
ecoli = read.table("./ecoli.orange.csv",  sep=",",header=T)
df=ecoli[,2:8]
Class=ecoli[,9]
dim(df)
df = na.omit(df)

data_ecoli=data.table(df)
#ggpairs(data_wine,columns = 1:13,mapping=aes(colour=Class))
summary(df)
sapply(df[,-8], var)
y=as.numeric(Class)

## graficar de a dos variables
ggplot(df,aes(x = df[,1], y = df[,2], col= y)) + geom_point()
ggplot(df,aes(x = df[,3], y = df[,4], col= y)) + geom_point()
ggplot(df,aes(x = 'acc', y = 'mcg', col= y)) + geom_point()

################## Encontar el numero óptimo de clusters #####

set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(df[,1:7],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(df, kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(df, kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 20, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Kmeans sin estandarizar####
not_norm_cluster=kmeans(data_ecoli[,1:7,with=F],5)
data_ecoli$cluster=as.factor(not_norm_cluster$cluster)
ggpairs(data_ecoli,columns = 1:7,mapping=aes(colour=cluster))

table(not_norm_cluster$cluster,y)

# Inspect 'clusters'
str(not_norm_cluster)
print(not_norm_cluster)
#visulización
fviz_cluster(not_norm_cluster, data = df)

#Estadística Descriptiva del Cluster Final Ecoli
df%>%
  mutate(Cluster = not_norm_cluster$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


#estandarizar####
df_sca = scale(df)
head(df_sca)
norm_cluster=kmeans(scale(data_ecoli[,1:7,with=F],scale = T),5)
data_ecoli$cluster=as.factor(norm_cluster$cluster)
#ggpairs(data_ecoli,columns = 1:7,mapping=aes(colour=cluster))
table(norm_cluster$cluster,y)

################# PCA Ecoli #######

# PCA #Scaling data before PCA is usually advisable! 
pr_out =prcomp(df, center = TRUE, scale = FALSE, rank. = 2) 
summary(pr_out)
# Screeplot
pr_var =  pr_out$sdev ^ 2
pve = pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
# Grafica PVE acumulada
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')
# Rotate loadings
rot_loading = varimax(pr_out$rotation[, 1:2])
rot_loading

################## Encontar el numero óptimo de clusters para PCA #####
set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(pr_out[["x"]],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k) PCA", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(pr_out[["x"]], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Aplicar Kmeans
pc = PCAmix(X.quanti = df, X.quali = NULL, ndim = 2, rename.level = FALSE,weight.col.quanti = NULL, 
            weight.col.quali = NULL, graph = TRUE)
plot(pc)
pca_cluster=kmeans(pr_out[["x"]], centers = 5, nstart = 50)
data_ecoli$cluster=as.factor(pca_cluster$cluster)
ggpairs(data_ecoli,columns = 1:5,mapping=aes(colour=cluster))

not_norm_cluster$tot.withinss
table( y, not_norm_cluster$cluster, dnn = c( "Original", "Kmeans_not_norm" ) )
norm_cluster$tot.withinss
table( y, norm_cluster$cluster, dnn = c( "Original", "Kmeans_norm" ) )
pca_cluster$tot.withinss
table( y, pca_cluster$cluster, dnn = c( "Original", "Kmeans_PCA" ) )
#Exploración Thyroid ###############
#Preparación de datos
setwd("D:\\Dcorporativo\\Personal\\Maestria\\AprendizajeAutomatico\\Entrega3")
Thyroid = read.table("./new-thyroid.orange.csv",  sep=",",header=T)
summary(Thyroid)
df=Thyroid[,2:6]
Class=Thyroid[,1]
dim(df)
df = na.omit(df)

data_Thyroid=data.table(df)
#ggpairs(data_Thyroid,columns = 1:13,mapping=aes(colour=Class))
summary(df)
sapply(df[,-8], var)
y=as.numeric(Class)

## graficar de a dos variables
ggplot(df,aes(x = df[,1], y = df[,2], col= y)) + geom_point()
ggplot(df,aes(x = df[,3], y = df[,4], col= y)) + geom_point()
################## Encontar el numero óptimo de clusters #####

set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(df[,1:5],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(df, kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(df, kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 20, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Kmeans sin estandarizar####
not_norm_cluster=kmeans(data_Thyroid[,1:5,with=F],3)
data_Thyroid$cluster=as.factor(not_norm_cluster$cluster)
ggpairs(data_Thyroid,columns = 1:5,mapping=aes(colour=cluster))

table(not_norm_cluster$cluster,y)

# Inspect 'clusters'
str(not_norm_cluster)
print(not_norm_cluster)
#visulización
fviz_cluster(not_norm_cluster, data = df)

#Estadística Descriptiva del Cluster Final Thyroid
df%>%
  mutate(Cluster = not_norm_cluster$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


#####estandarizar####
df_sca = scale(df)
head(df_sca)
norm_cluster=kmeans(scale(data_Thyroid[,1:5,with=F],scale = T),3)
data_Thyroid$cluster=as.factor(norm_cluster$cluster)
#ggpairs(data_Thyroid,columns = 1:7,mapping=aes(colour=cluster))
table(norm_cluster$cluster,y)

################# PCA Thyroid #######

# PCA #Scaling data before PCA is usually advisable! 
pr_out =prcomp(df, center = TRUE, scale = FALSE, rank. = 2) 
summary(pr_out)
# Screeplot
pr_var =  pr_out$sdev ^ 2
pve = pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
# Grafica PVE acumulada
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')
# Rotate loadings
rot_loading = varimax(pr_out$rotation[, 1:2])
rot_loading

################## Encontar el numero óptimo de clusters para PCA #####
set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(pr_out[["x"]],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k) PCA", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(pr_out[["x"]], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Aplicar Kmeans
pc = PCAmix(X.quanti = df, X.quali = NULL, ndim = 2, rename.level = FALSE,weight.col.quanti = NULL, 
            weight.col.quali = NULL, graph = TRUE)
plot(pc)
pca_cluster=kmeans(pr_out[["x"]], centers = 5, nstart = 50)
data_Thyroid$cluster=as.factor(pca_cluster$cluster)
ggpairs(data_Thyroid,columns = 1:5,mapping=aes(colour=cluster))

not_norm_cluster$tot.withinss
table( y, not_norm_cluster$cluster, dnn = c( "Original", "Kmeans_not_norm" ) )
norm_cluster$tot.withinss
table( y, norm_cluster$cluster, dnn = c( "Original", "Kmeans_norm" ) )
pca_cluster$tot.withinss
table( y, pca_cluster$cluster, dnn = c( "Original", "Kmeans_PCA" ) )

#Exploración Breast Cancer Wisconsin###############
#Preparación de datos
setwd("D:\\Dcorporativo\\Personal\\Maestria\\AprendizajeAutomatico\\Entrega3")
BCW = read.table("./breast-cancer-wisconsin_orange.csv",  sep=",",header=T)
summary(BCW)
df=BCW[,2:10]
Class=BCW[,11]
dim(df)
df = na.omit(df)

data_BCW=data.table(df)
#ggpairs(data_BCW,columns = 1:13,mapping=aes(colour=Class))
summary(df)
sapply(df[,-9], var)
y=as.numeric(Class)

## graficar de a dos variables
ggplot(df,aes(x = df[,1], y = df[,7], col= y)) + geom_point()
ggplot(df,aes(x = df[,3], y = df[,4], col= y)) + geom_point()
################## Encontar el numero óptimo de clusters #####

set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(df[,1:9],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(df, kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(df, kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 20, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Kmeans sin estandarizar####
not_norm_cluster=kmeans(data_BCW[,1:9,with=F],2)
data_BCW$cluster=as.factor(not_norm_cluster$cluster)
ggpairs(data_BCW,columns = 1:9,mapping=aes(colour=cluster))

table(not_norm_cluster$cluster,y)

# Inspect 'clusters'
str(not_norm_cluster)
print(not_norm_cluster)
#visulización
fviz_cluster(not_norm_cluster, data = df)

#Estadística Descriptiva del Cluster Final Thyroid
df%>%
  mutate(Cluster = not_norm_cluster$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


#####estandarizar####
df_sca = scale(df)
head(df_sca)
norm_cluster=kmeans(scale(data_BCW[,1:9,with=F],scale = T),2)
data_BCW$cluster=as.factor(norm_cluster$cluster)
#ggpairs(data_BCW,columns = 1:7,mapping=aes(colour=cluster))
table(norm_cluster$cluster,y)

################# PCA BCW #######

# PCA #Scaling data before PCA is usually advisable! 
pr_out =prcomp(df, center = TRUE, scale = FALSE, rank. = 2) 
summary(pr_out)
# Screeplot
pr_var =  pr_out$sdev ^ 2
pve = pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
# Grafica PVE acumulada
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')
# Rotate loadings
rot_loading = varimax(pr_out$rotation[, 1:2])
rot_loading

################## Encontar el numero óptimo de clusters para PCA #####
set.seed(200)
k.max = 10
wss= sapply(1:k.max,function(k){kmeans(pr_out[["x"]],k,nstart = 20,
                                       iter.max = 20)$tot.withinss})
wss

#graficar an elbow point
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k) PCA", 
     ylab = "Within cluster sum of squares")

#Elbow Method con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "wss")

##Silhouettes con fviz_nbclust
fviz_nbclust(pr_out[["x"]], kmeans, method = "silhouette")

# calcular  gap statistic
gap_stat <- clusGap(pr_out[["x"]], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Imprimir el resultado
print(gap_stat, method = "firstmax")
#graficado
fviz_gap_stat(gap_stat)

#Aplicar Kmeans
pc = PCAmix(X.quanti = df, X.quali = NULL, ndim = 2, rename.level = FALSE,weight.col.quanti = NULL, 
            weight.col.quali = NULL, graph = TRUE)
plot(pc)
pca_cluster=kmeans(pr_out[["x"]], centers = 2, nstart = 50)
data_BCW$cluster=as.factor(pca_cluster$cluster)
ggpairs(data_BCW,columns = 1:9,mapping=aes(colour=cluster))

not_norm_cluster$tot.withinss
table( y, not_norm_cluster$cluster, dnn = c( "Original", "Kmeans_not_norm" ) )
norm_cluster$tot.withinss
table( y, norm_cluster$cluster, dnn = c( "Original", "Kmeans_norm" ) )
pca_cluster$tot.withinss
table( y, pca_cluster$cluster, dnn = c( "Original", "Kmeans_PCA" ) )
