# Cluster analysis##lecture 5
# start by calling data in
#Data
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
snails<-read.csv("snail_data.csv", row=1, header=TRUE)[,1:3]
snails
####Download packages
###You will be using new packages cluster and raster
#install packages 
install.packages("raster")
install.packages("cluster")
install.packages("mvnormtest")
install.packages("MVN")
##run the packages
library(raster)
library(cluster)
library(mvnormtest)
library(MVN)

#K-means clustering
##K-means clustering is a non-hierarchical clustering method that seeks to find groups that maximize within-group homogeneity:
#Check the histograms of each variable (as discussed in lab 2) and determine if you need to transform them. Let me know what you think.
hist(snails$Proportionality)
#Let’s also test for multivariate and univariate normality (even though these tests are conservative)
mshapiro.test(t(snails))
mvn(snails, mvnTest = "mardia")
#Compare test results to the histograms you plotted.
#If variables (columns) are measured on different scales or have large differences in variance, you must scale the variables (columns). 
#Let’s check this using the coefficient of variation (cv) on column totals:
snail.tot<- apply(snails,2, sum)
cv(snail.tot)
#What do you think? Should you scale the snail data set?
#If so, z standardize (scale function) the variables.
snails<-scale(snails)
#If you remember from lecture, one way to determine the number of clusters, k, for the k-means cluster analysis is to look at a scree plot for the within-group sum of squares. 
#Let’s do a loop that will find the within group sum of squares for the full range of possible cluster solutions (nrows-1). 
#The snails data set has nine samples (rows), so you will explore solutions from 1-8 clusters.
#Set a vector for the loop to fill. 
wss <- rep(0, 8)
#Run a loop for 1 to 8 clusters:
for (i in 1:8) # sets the number of times the loop will be run i.e., the number of clusters in this case)
wss[i] <- sum(kmeans(snails, centers = i,nstart=25)$withinss) # run the kmeans function for each number of clusters (i) and extract the within sum of squares for each.
wss
#Make the scree plot:
plot(1:8, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares") 
#How many clusters are present in the best K-means solution?
#Another indicator of how many cluster to use is looking at the average silhouette width.
?silhouette 
#We can run a similar loop to look at the average silhouette width:
  sil <- rep(0,8)
for (i in 2:8)
  sil[i] <- summary(silhouette(kmeans(snails, centers=i, iter.max=100, nstart=25)$cluster, dist(snails)))$avg.width
plot(2:8, sil[2:8], type = "b", xlab = "Number of groups", ylab = "average silhouette width ")

#Let’s go ahead and plot out the result for the optimal cluster solution that you just found. 
#Fill in the code for the number of centers (clusters) you found above:
snails.kop <- kmeans(snails, centers= 2, iter.max=10, nstart=25)
#Plot a scatter plot showing cluster designations:
pairs(snails, panel=function(x,y,z) text(x,y,snails.kop$cluster))
#Which morphological metric really separates the two groups?
#Now, let’s plot them against principal components 1 and 2:
#Run, plot and interpret the PCA:
snail.pc <- princomp(snails, cor=F)
summary(snail.pc)
snail.pc$loadings
#Set up colors for each cluster:
my.color.vector <- rep("green", times=nrow(snails))
my.color.vector[snails.kop$cluster==1] <- "blue"
my.color.vector[snails.kop$cluster==2] <- "green"
#Plot clusters:
plot(snail.pc$scores[,1], snail.pc$scores[,2], ylim=range(snail.pc$scores[,1]),xlim=range(snail.pc$scores[,1]*1.25), xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(snail.pc$scores[,1], snail.pc$scores[,2], labels=rownames(snails), cex=1.25, lwd=2,
     col=my.color.vector)
#Plot clusters onto biplot:
biplot(snail.pc, xlabs= rep("",9),xlim=range(-.55,.55))
text(snail.pc$scores[,1], snail.pc$scores[,2], labels=rownames(snails), cex=1.25, lwd=2,
     col=my.color.vector)
#How does plotting your clusters on a bi-plot add to your interpretation of the cluster analysis and the PCA?
  