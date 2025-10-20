########Non-Metric Multidimensional Analysis (NMDS)#########
#NMDS is the most flexible ordination technique. It operates on a distance matrix and projects samples that are similar, close together and ones that are different, far apart.
#Create Sørensens’s disimilarity martix for the birds data:
#You will be using this data set for the Non-Metric Multidimensional Analysis (NMDS).
#Download packages
#you will be using the packages vegan and ca
install.packages("ca")
library(vegan)
library(ca)
#Call in the data set"
#Next, call in the data set “combined_birds.csv” and call it birds2. 
#You will be using this data set for the Non-Metric Multidimensional Analysis (NMDS).
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
ib<-read.csv("Atlantic_Caribbean.csv", header=TRUE) 

ib2<-ib[,-1:-4]
ib2
rownames(ib2)<-ib[,4]

#Create jaccard distance matrix for the birds data:
ac<-vegdist(ib2, "jaccard")
ac
#You are going to use the metaMDS function in the vegan package. 
#K =2 because we are interested in only two dimension (which is common for NMDS).
?metaMDS
nmdsBird<-metaMDS(ac,k=2, trace=T, trymax=100)
stressplot(nmdsBird)
#What do the stress value and the fit (R2) of the monotonic regression tell you about the NMDS plot?
#Let’s plot out our results and see if there is a difference between the historical and current Hawaiian bird assemblages?
### Extract the results

#Identify the Ocean as groups:
treat=as.matrix(c(rep("Atlantic",35),rep("Caribbean",19)))
treat
#Plot out the points (islands):
ordiplot(nmdsBird,type="n",xlim=c(-.5,.5),ylim=c(-.5,.5))
## species scores not available
orditorp(nmdsBird,display="sites",col=c(rep("green",35),rep("blue",19)),air=0.01,cex=1.25)
legend(-.55,.5, c("Atlantic","Caribbean"), cex=0.8, 
       col=c("green","blue"), pch=15:15)
#Add a convex hull around each group:
ordihull(nmdsBird, treat, display="si",lty=1, col="green", show.groups="Atlantic")
ordihull(nmdsBird, treat, display="si", lty=1, col="blue", show.groups="Caribbean")

#######ggplot###
install.packages("ggplot2")
library(ggplot2)
#ggplot
data.scores <- as.data.frame(scores(nmdsBird)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$Island <- rownames(ib2) # create a column of island names, from the original data frame "ib"
#data.scores$Ocean <- ib$Ocean  #  create a column of ocean names, from the original data frame "ib"
data.scores$Ocean <- ib$Ocean # create a column of archipelago names, from the original data frame "ib"
head(data.scores)  #look at the data

ggplot(data.scores, aes(x= NMDS1, y= NMDS2, col=Ocean)) + #denote groupings by color "col" and shape
  geom_point() +#adds points
  geom_text(aes(label=rownames(data.scores)),hjust=0, vjust=0)+#adds island names
  stat_ellipse() +#adds ellipses
  theme_bw() +
  xlim(-5, 5)+
  ylim(-5,5)+
  labs(title = "NMDS Plot")

