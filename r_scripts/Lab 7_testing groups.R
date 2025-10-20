#Lab 7 Testing Groups
#The goal of this lab is to become familiar with the three techniques, 
#Multiple Response Permutation Procedure (MRPP).
#PerMANOVA, Analysis of within Group Similarity (ANOSIM), and for testing if groups are significantly different from one another.
#Data
#You will be using two different data sets today. 
#For the MRPP you will use the snail morphology data set “snails_data.csv” which you are familiar with (remember use only the first three columns for the snail data set).
#For PerMANOVA and ANOSIM, you will use the data set detailing the current and historical composition of bird communities across the Hawaiian Islands “combined_birds.csv” which you are also familiar with.
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
snails<-read.csv("snail_data.csv", row=1, header=TRUE)[,1:3]
#Download packages
#We will be using the following packages:
install.packages("ade4")
#Run libraries
library(raster)
library(cluster)
library(vegan)
library(ade4)



#####Multiple Response Permutation Procedure (MRPP)######

#MRPP is a nonparametric method for testing the hypothesis of no differences between two or more groups. 
#will use MRRP to determine if the southern Atlantic group is significantly different from the Caribbean group from the snail morphology data set. 
#First you must use your clustering skills to create these groups:
#Use k-means clustering to define the south Atlantic and Caribbean clusters (LAB 5). 
#After running the analysis, extract the cluster to be used in the MRPP:
snails<-scale(snails)

snails.kop <- kmeans(snails, centers= 2, iter.max=10, nstart=25)

groups<-snails.kop$cluster

#You are going to use the mrpp function in the vegan package. You must create an appropriate distance matrix for the snail data to conduct an mrpp.
?mrpp
distSnails<-vegdist(snails, method="euclidean")

set.seed(11)

snailMRPP<-mrpp(distSnails, groups, permutations = 1000)

#Next, plot out the histogram of permuted deltas:
hist(snailMRPP$boot.deltas, main = "Histogram of snail MRPP deltas" )
points(snailMRPP$delta,0, pch=19,col="red", bg="red", cex=2)

####PerMANOVA###
#PerMANOVA is a nonparametric method for testing the hypothesis of no differences between two or more groups based on rank dissimilarities. 
#It is a flexible approach that can take any distance metric. 
#You will use PerMANOVA to determine if the current composition of the Hawaiian Island bird communities is significantly different from the historical (i.e. before colonization by Europeans) time period.
#PerMANOVA is often used with NMDS. You are going to conduct an NMDS analysis on these data first (LAB 4):

#NMDS
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
birds<-read.csv("combined_birds.csv", row=1, header=TRUE)
jbirds<-vegdist(birds, "bray") 
nmdsBird<-metaMDS(jbirds,k=2, trace=T)
#For plotting the NMDS, create the groups to assign different colors to each time period. You will also use these groups for the ANOSIM:
group=as.matrix(c(rep("Historical",6),rep("Current",6)))
ordiplot(nmdsBird,type="n",xlim=c(-.5,.5),ylim=c(-.5,.5))
## species scores not available
orditorp(nmdsBird,display="sites",col=c(rep("green",6),rep("blue",6)),air=0.01,cex=1.25)
legend(-.55,.5, c("Historical","Current"), cex=0.8, 
       col=c("green","blue"), pch=15:15)
#Next, you are going to use the adonis function in the vegan package to run the PerMANOVA:
  ?adonis
set.seed(11) 
permBirds<-adonis2(jbirds ~ group, permutations=1000)
#Explore the output table and then plot the permuted F-ratios:
hist(attributes(permBirds)$F.perm, main = "Histogram of F statistics for Hawaiian Birds" ,xlim=c(0,12))
points(permBirds$F[1],0, pch=19,col="red", bg="red", cex=2)

#####Analysis of Group Similarities (ANOSIM)#####
#ANOSIM is a nonparametric method for testing the hypothesis of no differences between two or more groups based on rank dissimilarities. 
#It is a flexible approach that can take any distance metric. You will use ANOSIM to determine if the current composition of the Hawaiian Island bird communities is significantly different from the historical (i.e. before colonization by Europeans) time period. 
#Anosim is often used with NMDS. You are going to conduct an NMDS analysis on these data first (LAB 4):

NMDS
#Do the nmds and grouping again:
jbirds<-vegdist(birds, "bray") 

nmdsBird<-metaMDS(jbirds,k=2, trace=T)
#For plotting the NMDS, create the groups to assign different colors to each time period.
group=as.matrix(c(rep("Historical",6),rep("Current",6)))
ordiplot(nmdsBird,type="n",xlim=c(-.5,.5),ylim=c(-.5,.5))
## species scores not available

orditorp(nmdsBird,display="sites",col=c(rep("green",6),rep("blue",6)),air=0.01,cex=1.25)
legend(-.55,.5, c("Historical","Current"), cex=0.8, 
       col=c("green","blue"), pch=15:15)
#You are going to use the anosim function in the vegan package.
?anosim
set.seed(11) 

birdAnosim<-anosim(jbirds, group, permutations = 1000)
#Explore the output table and then plot the permuted F-ratios:
  #
  hist(birdAnosim$perm, main = "Histogram of R statistics for Hawaiian Birds", xlim=c(-.5,1 ))
points(birdAnosim$statistic,0, pch=19,col="red", bg="red", cex=2)



