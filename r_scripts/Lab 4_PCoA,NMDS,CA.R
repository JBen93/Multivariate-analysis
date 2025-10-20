#Data
#Call in the data set “Current_Hawaiian_Birds.csv” from your working directory and name it birds.
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
#Principal Coordinates Analysis (PCoA).
birds<-read.csv("Current_Hawaiian_Birds.csv", row=1, header=TRUE)
birds
#Next, call in the data set “combined_birds.csv” and call it birds2. 
#You will be using this data set for the Non-Metric Multidimensional Analysis (NMDS).
birds2<-read.csv("combined_birds.csv", row=1, header=TRUE)
birds2
#Finally, call in the data set “tree.csv” and call it tree. This is the tree reproduction data set from lecture that you will use for the 4. Correspondence Analysis (CA).
tree<-read.csv("tree.csv", row=1, header=TRUE)
tree
#Download packages
#you will be using the packages vegan and ca
install.packages("ca")
library(vegan)
library(ca)

#Principal Coordinates Analysis (PCoA)
#PCoA is a flexible analysis that is performed on a variety of distance matrices (e.g. Euclidean, Jaccard index, Sorensen index). birds is a binary data set so lets use the Sørensens’s index. Note that there are many possible indices to use for binary data; see Koleff et al. 2003 reading from module 2.
#Sørensens’s index ** Bray-Curtis distance is = to Sørensens’s when using binary data. That is why method = “bray” below**:
jbirds<-vegdist(birds, "bray")
jbirds
#Which island pair is the most similar? Hawaii?
#You are going to use the cmdscale function in the stats package to run the PCoA:
?cmdscale
## starting httpd help server ... done
cmd<-cmdscale(jbirds, k=5, eig=TRUE) 
cmd
#The “points” are the coordinates of each island. They are the eigenvectors scaled by the square root of their eigenvalues (i.e. the standard deviation):
cmd$points
#Let’s make a PCoA table to look at the eigenvalues, and the proportional and cumulative variance:
eigenvalues<-cmd$eig[1:5]
propVar<-eigenvalues/sum(eigenvalues)
cumVar<-cumsum(propVar)
PCoA_Table<-cbind(eigenvalues,propVar,cumVar)
PCoA_Table

#Scree plot:
  plot(eigenvalues)
lines(lowess(eigenvalues))
#How many axes should you keep?
#Now, let’s plot the first two PCoA axes:
x<-cmd$points[,1]
y<-cmd$points[,2]
plot(x,y,xlab= "Coordinate 1", ylab="Coordinate 2", xlim=range(x)*1.2,ylim=range(y)*1.2, type="n")
text(x,y,labels=rownames(cmd$points), cex=.9)


#By “hand”
#Ok, let’s now run a PCoA following the directions that I gave during lecture. The steps are up on the screen in front of the class:
jbirds<-vegdist(birds, "bray")
CORD<--1/2*jbirds^2
C<-as.matrix(CORD)
cs<-colMeans(C)
rs<-rowMeans(C)
C1<-sweep(C,MARGIN=2,cs,FUN="-")
C2<-sweep(C1,MARGIN=1,rs,FUN="-")
delta<-mean(C)+C2

# Next, run an eigen analysis:
EG<-eigen(delta)
eigenvalues2<-EG$values[1:5]

# And make our PCoA table:
propVar2<-eigenvalues2/sum(eigenvalues2)
cumVar2<-cumsum(propVar2)
PCoA_Table2<-cbind(eigenvalues2,propVar2,cumVar2)
PCoA_Table2
#You scale the eigenvectors by the square root of their eigenvalues to get the coordinates (points):
points2<-sweep(EG$vectors[,1:5],MARGIN=2,sqrt(eigenvalues2), FUN="*")
points2
x<-points2[,1]
y<-points2[,2]
#Lets plot this
#The coordinates:
plot(x,y,xlab= "Coordinate 1", ylab="Coordinate 2", xlim=range(x)*1.2,ylim=range(y)*1.2, type="n")
text(x,y,labels=rownames(birds), cex=.9)
# Calculate weighted species scores:
scores1<-sweep(birds,MARGIN=1,x, FUN="*")
scores1
species1<-colSums(scores1)/colSums(birds)
species1
# Add to the plot:
text(cbind(species1,species2),colnames(birds),cex=.7, col="red")

#########Non-Metric Multidimensional Analysis (NMDS)#########
#NMDS is the most flexible ordination technique. It operates on a distance matrix and projects samples that are similar, close together and ones that are different, far apart.
#Create Sørensens’s disimilarity martix for the birds data:
#You will be using this data set for the Non-Metric Multidimensional Analysis (NMDS).
install.packages("ca")
library(vegan)
library(ca)
birds2<-read.csv("combined_birds.csv", row=1, header=TRUE)
birds2
jbirds2<-vegdist(birds2, "bray") 
jbirds2
#You are going to use the metaMDS function in the vegan package. 
#K =2 because we are interested in only two dimension (which is common for NMDS).
?metaMDS
nmdsBird<-metaMDS(jbirds2,k=2, trace=T)
stressplot(nmdsBird)

#What do the stress value and the fit (R2) of the monotonic regression tell you about the NMDS plot?
#Let’s plot out our results and see if there is a difference between the historical and current Hawaiian bird assemblages?

#Identify the time period as groups:
treat=as.matrix(c(rep("Historical",6),rep("Current",6)))
treat
#Plot out the points (islands):
ordiplot(nmdsBird,type="n",xlim=c(-.5,.5),ylim=c(-.5,.5))
## species scores not available
orditorp(nmdsBird,display="sites",col=c(rep("green",6),rep("blue",6)),air=0.01,cex=1.25)
legend(-.55,.5, c("Historical","Current"), cex=0.8, 
       col=c("green","blue"), pch=15:15)
#Add a convex hull around each group:
ordihull(nmdsBird, treat, display="si",lty=1, col="green", show.groups="Historical")
ordihull(nmdsBird, treat, display="si", lty=1, col="blue", show.groups="Current")

#######ggplot###
install.packages("ggplot2")
library(ggplot2)
#ggplot
data.scores <- as.data.frame(scores(nmdsBird)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$island <- row.names(birds2) # create a column of island names, from the original data frame "ib"
#data.scores$Ocean <- ib$Ocean  #  create a column of ocean names, from the original data frame "ib"
data.scores$time <- treat # create a column of archipelago names, from the original data frame "ib"
head(data.scores)  #look at the data

ggplot(data.scores, aes(x= NMDS1, y= NMDS2, col=time)) + #denote groupings by color "col" and shape
  geom_point() +#adds points
  geom_text(aes(label=rownames(data.scores)),hjust=0, vjust=0)+#adds island names
  stat_ellipse() +#adds ellipses
  theme_bw() +
  xlim(-1, 1)+
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








  



