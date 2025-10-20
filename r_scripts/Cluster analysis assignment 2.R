#Cluster analysis assignment 
#Data
#Call in the data set “dune_data.csv” from working directory and call it ‘dune’. 
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
#This dataset contains species presences/absence for 30 species across 20 dune meadow sites. Use the Jaccard index to create your dissimilarity matrix.
dune<-read.csv("dune_data.csv", row=1, header=TRUE)

#We will be using the following packages:
#install.packages("raster")
#install.packages("cluster")
#install.packages("pvclust")

#load packages 
library(raster)
library(cluster)
library(vegan)
library(pvclust)

#Calculating the distance/dissimilarity matrix, in this case I used "jaccard", since it's presence and absence data.
distdune<-vegdist(dune,"jaccard")

#Polythetic Agglomerative Hierarchical Clustering (PAHC)
#The hclust function in the stats package to conduct PAHC. 
#This hclust function contains the six fusion methods  
#We will use hclust to cluster the dune_data and construct dendrograms.

#Clustering algorithms
singleTree<-hclust(distdune, method = "single")
completeTree<-hclust(distdune, method = "complete")
centroidTree<-hclust(distdune, method = "centroid")
medianTree<-hclust(distdune, method = "median")
averageTree<-hclust(distdune, method = "average")
wardTree<-hclust(distdune, method = "ward.D2")

#Plot each dendrogram individual and explore the patterns:
plot(singleTree)
plot(completeTree)
plot(centroidTree)
plot(medianTree)
plot(averageTree)
plot(wardTree)

#Evaluating the cluster solution
#The agglomerative coefficient, the cophenetic correlation coefficient, and Monte Carlo simulations (i.e. bootstrapping).
#Agglomerative coefficient

#1 The agglomerative coefficient for each fusion method:
ag1<-coef.hclust(singleTree)
ag2<-coef.hclust(completeTree)
ag3<-NA
ag4<-NA
ag5<-coef.hclust(averageTree)
ag6<-coef.hclust(wardTree)
#Now put them in a table:
methods<-c("single","complete","centroid", "median", "average", "ward")
agc<-round(c(ag1,ag2,ag3,ag4,ag5,ag6),2)
agcTable<-data.frame(methods,agc)

#2 Cophenetic correlation coefficient
#Next,  calculate the cophenetic correlation coefficient. 
#This will allow us to see how well the dendrogram built by each fusion method reproduce the original distance matrix and will also allow us to compare the different fusion methods:
cc1<-cor(distdune,cophenetic(singleTree))
cc2<-cor(distdune,cophenetic(completeTree))
cc3<-cor(distdune,cophenetic(centroidTree))
cc4<-cor(distdune,cophenetic(medianTree))
cc5<-cor(distdune,cophenetic(averageTree))
cc6<-cor(distdune,cophenetic(wardTree))
cophCor<-round(c(cc1,cc2,cc3,cc4,cc5,cc6),2)
#Let’s put this all in a table:
methods<-c("single","complete","centroid", "median", "average", "ward")
dendrogramTable<-data.frame(methods,cophCor,agc)
dendrogramTable

#Bootstrapping
#Last but not least, let’s run a bootstrap permutation to see how many clusters are “good clusters”. 
#Since we know that “single”, “complete”, and “average” linkage methods had the highest cophenetic correlation, let’s focus on these:

 
#Often, we will use a different measure and need to define custom distance function which returns an object of class “dist”. Here we do it for jaccard.
jaccard <- function(x) {
  x <- t(as.matrix(x))
  res <- vegdist(x, method="jaccard")
  res <- as.dist(res)
  attr(res, "method") <- "jaccard"
  return(res)}
#The resampling procedure takes a little bit of time so we will only use 100 bootstraps (nboot = 100). Normally, you would conduct at least 1000:
boot1<-pvclust(t(dune), method.hclust="single",method.dist=jaccard,iseed=22, nboot=100)
boot2<-pvclust(t(dune), method.hclust="complete",method.dist=jaccard,iseed=22, nboot=100)
boot3<-pvclust(t(dune), method.hclust="average",method.dist=jaccard,iseed=22, nboot=100)

#Here, jaccard is our custom distance/dissimilarity function
#Now plot each dendrogram with the p-values from the Monte Carlo simulation. 
#The “au” p-values (in red) correspond to multi-scale bootstrapping, while the “bp” p-values correspond to normal bootstrapping.

par(mfrow=c(2,3))

plot(boot1)
pvrect(boot1, alpha=0.95, pv="au")
plot(boot2)
pvrect(boot2, alpha=0.95, pv="au")
plot(boot3)
pvrect(boot3, alpha=0.95, pv="au")

par(mfrow=c(1,1))
plot(boot1)
pvrect(boot1, alpha=0.95, pv="bp")
plot(boot2)
pvrect(boot2, alpha=0.95, pv="bp")
plot(boot3)
pvrect(boot3, alpha=0.95, pv="bp")

#Polythetic Divisive Hierarchical Clustering (PDHC)
#You will use the diana function in the cluster package to conduct PDHC. 
#We will use diana to cluster the Caribbean bird data and construct dendrograms.

#Clustering algorithm
didune <- diana(distdune)

#Next, plot the dendrogram:
plot(didune, which.plots = 2)

#calculate the divisive coefficient
didune$dc
#calculate the cophenetic correlation coefficient:
d.coph <- cor(distdune,cophenetic(didune))
d.coph


