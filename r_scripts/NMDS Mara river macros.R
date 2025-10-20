#Data
#Call in the data set “mara macros nmds.csv” from your working directory and name it macros.


#########Non-Metric Multidimensional Analysis (NMDS)#########
#NMDS is the most flexible ordination technique. It operates on a distance matrix and projects samples that are similar, close together and ones that are different, far apart.
#Create Sørensens’s disimilarity martix for the birds data:
#You will be using this data set for the Non-Metric Multidimensional Analysis (NMDS).
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
macros<-read.csv("mara nmds.csv", row=1, header=TRUE)
install.packages("ca")
library(vegan)
library(ca)
jmacros<-vegdist(macros, "jaccard") 
jmacros
#You are going to use the metaMDS function in the vegan package. 
#K =2 because we are interested in only two dimension (which is common for NMDS).
?metaMDS
nmdsmacros<-metaMDS(jmacros,k=2, trace=T)
stressplot(nmds)macros
plot(nmdsmacros, ylim=c(-1,1), xlim=c(-1,1)) 

#What do the stress value and the fit (R2) of the monotonic regression tell you about the NMDS plot?
#Let’s plot out our results and see if there is a difference between the historical and current Hawaiian bird assemblages?

#Identify the time period as groups:
treat=as.matrix(c(rep("Historical",4),rep("Current",4)))
treat
#Plot out the points (islands):
ordiplot(nmdsmacros,type="n",xlim=c(-2,2),ylim=c(-2,2))
## species scores not available
orditorp(nmdsmacros,display="sites",col=c(rep("green",4),rep("blue",4)),air=0.01,cex=1.25)
legend(-2,2, c("Historical","Current"), cex=0.8, 
       col=c("green","blue"), pch=15:15)
#Add a convex hull around each group:
ordihull(nmdsmacros, treat, display="si",lty=1, col="green", show.groups="Historical")
ordihull(nmdsmacros, treat, display="si", lty=1, col="blue", show.groups="Current")

#######ggplot###
install.packages("ggplot2")
library(ggplot2)
#ggplot
data.scores <- as.data.frame(scores(nmdsmacros)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$sites <- row.names(macros) # create a column of island names, from the original data frame "ib"
#data.scores$Ocean <- ib$Ocean  #  create a column of ocean names, from the original data frame "ib"
data.scores$time <- treat # create a column of archipelago names, from the original data frame "ib"
head(data.scores)  #look at the data

ggplot(data.scores, aes(x= NMDS1, y= NMDS2, col=time)) + #denote groupings by color "col" and shape
  geom_point() +#adds points
  geom_text(aes(label=rownames(data.scores)),hjust=0, vjust=0)+#adds island names
  stat_ellipse() +#adds ellipses
  theme_bw() +
  xlim(-1, 4)+
  ylim(-1.5,1.5)+
  labs(title = "NMDS Plot")

#####Correspondence Analysis (CA)
#Correspondence analysis allows for the simultaneous ordination or rows and columns. It assumes a unimodal relationship between variables and the axes. 
#We are going to use the ca package to start on the tree data set from lecture that looks at tree reproductive status and tree age.
?ca

caTree<- ca(tree)
#Let’s look at the results:
print(caTree) 
#In this analysis the eigenvalues are called inertias. 
#The Mass is simply the column or row total. ChiDist is the distance from the origin in ordination space. Let’s now plot the ordination:
plot(caTree, xlim = c(-.5, 1),ylim = c(-.5, .5)) 

#By “hand”
#Than was a painless function to run. Now let’s do it by hand (all but the singular value decomposition that is):
#Divide the data matrix caTree by the grand total of the matrix:

p <- as.matrix(tree/sum(tree))

#Cross tabulate row and column sums to be used in calculating expected values for the Chi Square values:

rs <- as.vector(apply(p,1,sum))
cs <- as.vector(apply(p,2,sum))

#Calculate expected values for the Chi Square calculation:
cp <- rs %*% t(cs)
#Calculate Chi Square values and check them out:
Qbar <- as.matrix((p - cp) / sqrt(cp))
#Conduct singular value decomposition (svd):
Q.svd <- svd(Qbar)
#Scale eigenvectors for rows and columns by the square root of row and column sums respectively:
V <- diag(1/sqrt(cs)) %*% Q.svd$v 
Vhat <- diag(1/sqrt(rs)) %*% Q.svd$u 
#Calculate ordination coordinates for both rows and columns:
F <- diag(1/rs) %*% p %*% V
Fhat <- diag(1/cs) %*% t(p) %*% Vhat
#Plot row and column coordinates in ordination space.

plot(Fhat[,1:2], xlim = c(-.5, 1),ylim = c(-.5, .5) , type = "n",xlab = "Coordinate 1", ylab = "Coordinate 2", lwd = 2)
text(Fhat[,1:2], labels = colnames(tree), cex = 0.7)
text(F[,1:2], labels = rownames(tree), cex = 0.7)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)












