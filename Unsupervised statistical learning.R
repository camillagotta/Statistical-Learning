#upload libraries
library(factoextra)
library(reshape2)
library(cluster)
library(ggplot2)
library(FactoMineR)
library(corrplot)
library(dplyr)
library(plotly)
library(stringr)
library(ggthemes)
library(NbClust)

#upload balanced dataset
mydata<-read.csv(file = "C:/Users/USER/Downloads/2019/2019.csv")
summary(mydata)
rownames(mydata)<-mydata$Country.or.region
data<- mydata[c(2:8)]
colnames(data)

#remove NA values
data <- na.omit(data)

#enlarging margins
par("mar")
par(mar=c(1,1,1,1))

#top 10 
hpi_sort <- data[with(data, order(-Score)), ]
hpi_top_10 <- head(hpi_sort, 10)
hpi_top_10 <- hpi_top_10[, c(1:7)]
hpi_top_10

#last 10
hpi_bottom_10 <- tail(hpi_sort, 10)
hpi_bottom_10 <- hpi_bottom_10[, c(1:7)]
hpi_bottom_10

#correlation pca plot
data<- cor(data)
corrplot(data, method="ellipse")

#implementing pca
pr.out=prcomp(data, scale=TRUE)
pr.out$center
pr.out$rotation
pr.out$scale
pr.out$x
pr.out$sdev

pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)

# principal components
fviz_screeplot(pr.out, addlabels = TRUE)

#pve by each component
plot(pve, xlab="Prncipal Component", ylab="Proportion of variance explained",
     ylim=c(0,1), type='b')

#cumulative pve
plot(cumsum(pve), xlab="Principal Component",
     ylab="Cumulative proportion of variance explained",
     ylim=c(0,1), type='b')

#pca variables
var$contrib
var <- get_pca_var(pr.out)
fviz_pca_var(pr.out, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#pca individuals
ind <-get_pca_ind(pr.out)
fviz_pca_ind(pr.out, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)


#CLUSTER - KMEANS
# K-Means

#To standarize the variables
data.stand <- scale(mydata[,-1]) 

#select number of K
set.seed(123)
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(data.stand)

fviz_nbclust(data.stand, kmeans,nstart=15, method = "silhouette")

# k = 3
k.means.fit <- kmeans(data.stand, 3) 
str(k.means.fit)
clusplot(data.stand, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE,
         labels=2, lines=0)
fviz_cluster(k.means.fit, data = data.stand)

#CLUSTER-HIERARCHICAL

rownames(mydata)<-mydata$Country.or.region
data<-mydata[,-1]

#choose the distances
d1 <- dist(data, method="euclidean", diag=F, upper=F)

#function to generate the agglomeration program
agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)],
                               paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE) }

#with complete linkage
h1 <- hclust(d1, method="complete"); h1
agglo(h1)
plot(h1, main="complete linkage")
complete <- cutree(h1, k=3)
rect.hclust(h1, 3)
h1cluster <- cutree(h1, k=3)
h1cluster 

#with avg linkage
h2<-hclust(d1,method="average");h2
agglo(h2)
plot(h2, main="average linkage")
average <- cutree(h2, k=3)
rect.hclust(h2, 3)
h2cluster <- cutree(h2, k=3)
h2cluster 

#with single linkage
h3<-hclust(d1,method="single");h3
agglo(h3)
plot(h3, main="single linkage")
single<- cutree(h3, k=3)
rect.hclust(h3, 3)
h3cluster <- cutree(h3, k=3)
h3cluster

#with ward linkage
h4<-hclust(d1,method="ward.D");h4
agglo(h4)
plot(h4, main="Ward linkage")
ward<- cutree(h4, k=3)
rect.hclust(h4, 3)
h4cluster <- cutree(h4, k=3)
h4cluster

plot(data, col=h4cluster, main="ward likage")

#Add the cluster to the dataset
h4cluster<- cutree(h4, k=3)
data <- as.data.frame(data)
data$clu<-h4cluster
data$clu<-h4cluster

#Means for variables
medie<-aggregate(data, list(h4cluster), mean)
medie 

#Calculus of R^2 for each variables
mydata<-data
R2 <- rep(NA, (ncol(mydata)-1))
for(i in 1:(ncol(mydata)-1)) 
  R2[i] <- anova(aov(mydata[,i] ~ 
                       mydata[,ncol(mydata)]))[1,2]/
  (anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[1,2]+
     anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[2,2])
R2

mydata<-mydata[,-ncol(mydata)]
col<-colnames(mydata)
finali<-cbind(col,R2)
finali

# Plots for cluster interpretation
col<-colnames(mydata)
mydataz<-data.frame(scale(mydata))
mydataz$clu<-h4cluster
dati <- melt(mydataz, measure.vars=col)
ggplot(dati, aes(x = variable, y = value, color=variable)) +
  geom_boxplot() +
  facet_wrap(~ mydataz$clu) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
