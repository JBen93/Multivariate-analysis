#Principal Coordinates Analysis (PCoA)
#PCoA is a flexible analysis that is performed on a variety of distance matrices (e.g. Euclidean, Jaccard index, Sorensen index). birds is a binary data set so lets use the Sørensens’s index. Note that there are many possible indices to use for binary data; see Koleff et al. 2003 reading from module 2.
#Sørensens’s index ** Bray-Curtis distance is = to Sørensens’s when using binary data. That is why method = “bray” below**:
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
macros<-read.csv("mara nmds.csv", row=1, header=TRUE)
library(vegan)
library(ca)
jmacros<-vegdist(macros, "jaccard")

#How were macros clusters during historal and current year
#You are going to use the cmdscale function in the stats package to run the PCoA:
?cmdscale
## starting httpd help server ... done
cmd<-cmdscale(jmacros, k=5, eig=TRUE) 
cmd
#The “points” are the coordinates of each site. They are the eigenvectors scaled by the square root of their eigenvalues (i.e. the standard deviation):
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

# Calculate weighted species scores:
scores1<-sweep(macros,MARGIN=1,x, FUN="*")
scores1
species1<-colSums(scores1)/colSums(macros)
species1
scores2<-sweep(macros,MARGIN=1,y, FUN="*")
species2<-colSums(scores2)/colSums(macros)

# Add to the plot:
text(cbind(species1,species2),colnames(macros),cex=.7, col="red")