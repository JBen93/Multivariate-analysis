
#Multiple Response Permutation Procedure (MRPP) to test the clusters for the pitcher 
#Set the working directory
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
# Data ~Darlingtonia data and call it pitcher 
pitcher<-read.csv("Darlingtonia.csv", row=1, header=TRUE)
pitcher

# Run packages 
library(raster)
library(sp)
library(cluster)
library(mvnormtest)
library(MVN)
library(vegan)
library(ade4)

#Check the histograms of each variable and determine if you need to transform each independent variable
hist(pitcher$tube_diam)
#Recommended to test for multivariate and univariate normality (even though these tests are conservative)
mshapiro.test(t(pitcher))
mvn(pitcher, mvnTest = "mardia")
#Log transform the selected columns/variables using log1p 
pitcher[,c("tube_diam","keel_diam","wing2_length","hoodarea","wingarea","tubearea")]<-log1p(pitcher[,c("tube_diam","keel_diam","wing2_length","hoodarea","wingarea","tubearea")])
pitcher
#Let’s check if I need to standardize the data using the coefficient of variation (cv) on column totals:If Cv is >50 you must transform your data
pitcher.tot<- apply(pitcher,2, sum)
#cv(pitcher.tot)


#Next, apply a z-score standardization to the data with log transformed variables to scale it. 
pitchers<-scale(pitcher)

#####Multiple Response Permutation Procedure (MRPP)######
#MRPP is a nonparametric method for testing the hypothesis of no differences between two or more groups. 
#First  use K- means clustering to define groups
pitchers<-scale(pitcher)

#After running the analysis, extract the cluster to be used in the MRPP:
pitchers.kop <- kmeans(pitchers, centers= 2, iter.max=10, nstart=25)
#Set groups
groups<-pitchers.kop$cluster

# I am going to use the mrpp function in the vegan package. 
#I created an appropriate distance matrix for the Darlingtonia  data to conduct an mrpp.
distpitcher<-vegdist(pitchers, method="euclidean")
# set seed
set.seed(11)
#run the MRPP
pitcherMRPP<-mrpp(distpitcher, groups, permutations = 1000)
pitcherMRPP

#Next, plot out the histogram of permuted deltas:
hist(pitcherMRPP$boot.deltas, main = "Histogram of pitcher MRPP deltas" )
points(pitcherMRPP$delta,0, pch=19,col="red", bg="red", cex=2)


#####Analysis of Group Similarities (ANOSIM)#####
#ANOSIM is a nonparametric method for testing the hypothesis of no differences between two or more groups based on rank dissimilarities. 
#Anosim is often used with NMDS. You are going to conduct an NMDS analysis on these data first.

#calculate the appropriate distance matrix
#bray curtis does not work for this data, So, use Euclidean
jpitcher<-vegdist(pitchers, "euclidean") 
nmdspitcher<-metaMDS(jpitcher,k=2, trace=T)

#set seed and run Anosim
set.seed(11) 
#Recall your groups
groups<-pitchers.kop$cluster
pitcherAnosim<-anosim(jpitcher,groups, permutations = 1000)
pitcherAnosim
#Explore the output table and then plot the permuted F-ratios:
hist(pitcherAnosim$perm, main = "Histogram of R statistics pitcher plants", xlim=c(-.1,.1 ))
points(pitcherAnosim$statistic,0, pch=19,col="red", bg="red", cex=2)




