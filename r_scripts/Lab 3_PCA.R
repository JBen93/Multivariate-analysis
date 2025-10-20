#Matrix Algebra and Ordination Part I
#Install and load packages
library(mvnormtest)
library(MVN)
library(MVA)
library(psych)
library(Hmisc)
library(vegan)
library(StatMatch)
library(MASS)
#A primer of matrix algebra
newMatrix<- matrix(c(1,4,5,4,5,6,9,1,9),nrow=3, ncol=3)
newMatrix
#Now let’s check the dimensions of newMatrix:
dim(newMatrix)
#Matrix addition and subtraction
#make another matrix to either add or subtract
onematrix<-matrix(c(0,1,2,7,8,9,6,5,4),nrow=3, ncol=3)
onematrix
#Now add oneMatrix to newMatrix:
newMatrix + onematrix
#Then subtract oneMatrix from newMatrix:
newMatrix - onematrix

#Scalar Multiplication
#A Scalar is a single number. Scalar multiplication multiplies a scalar times a matrix:
3*newMatrix

#Matrix Multiplication
#You use % to signify that are using a matrix operation. Otherwise, R will attempt the operation element by element.
onematrix%*%newMatrix

#Matrix transposition
#Transposing a matrix involves interchanging its rows and columns:
transMatrix<-t(newMatrix)
transMatrix

#Identity Matrices
#An identity matrix is a matrix where all the diagonal terms equal one and the remaining elements equal 0:
Identity<-diag(3)

#Matrix Inversion
#The inverse of matrix A is A-1.
invMatrix<-solve(newMatrix)
#Multiplying a matrix times its inverse yields and identity matrix (A x A-1 = I):
#invMatrix%*%newMatrix
#Let’s round it:
round(invMatrix%*%newMatrix,10)

#Eigenvalues and eigenvectors
#Remember that an eigenvalue is a special scalar and the associated eigenvector is a vector that are key components of PCA.
eig<-eigen(newMatrix)
eig

#Principal Component Analysis (PCA)
#You are going to conduct a PCA on the USairpollution data.
rm(list =ls())
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/R input")
usAir<-USairpollution
usAir

#look at the distributions (i.e., histograms) of the variables to determine if they need to be transformed.
mshapiro.test(t(usAir))
mvn(usAir, mvnTest = "mardia")

usAirsqrt<-sqrt(usAir)
#usAirsqrt
#hist(usAirsqrt)
#par(mfrow=c(1,2))

#hist(usAir[,1])  
#hist(usAirsqrt[,1]) 
#Next, apply a z-score standardization:
ZusAir<-scale(usAirsqrt)
ZusAir
#Running the PCA:
#You are going to use the package princomp function in the stats package. 
#Take some time to read about this function:
?princomp
#Run the princomp function:
usAir_pca <- princomp(ZusAir, cor = F)
usAir_pca
summary(usAir_pca)
#Notice that the summary gives the standard deviation instead of the eigenvalue (variance). 
#Let’s calculate the eigenvalues using what we know about the relationship between standard deviation and variance (var = sd^2):
eigenVal<- (usAir_pca$sdev*sqrt(41/40))^2
#The sqrt(41/40) is to correct for the fact that princomp calculates variances with the divisor N instead of N-1 as is customary. 
#This adjustment will allow direct comparison with “hand” calculated eigenvalues using the function eigen below.
#Let’s make the PCA table with the eigenvalues instead of the standard deviations:
propVar<-eigenVal/sum(eigenVal)
cumVar<-cumsum(propVar)
pca_Table<-t(rbind(eigenVal,propVar,cumVar))
pca_Table
#This calculation and table are just to show you that the eigenvalues and the output from princomp, the stadard deviations, are the same thing. 
#YOU DO NOT NEED TO DO THIS WHEN YOU RUN A PCA
#the factor loadings from princomp:
loadings(usAir_pca)
scores(usAir_pca)

#How many PC Axes to keep?
#You now have 7 PC axes. Which ones give us vital information and which ones can we toss? 
#One method for selecting the number of Axes is a Scree plot:
plot(usAir_pca, type="lines")
#How about the latent root criterion (i.e., keep components with eigenvalues > 1) and the relative percent variance criteria. Check the summary of the PCA explore this:
summary(usAir_pca)

#Significance of factor loadings.
#While many use the “rule of thumb” where a loading > 0.30 dictates an “important” variable. 
#Another method for determining significance of factor loadings is bootstrapping. 
#Here we will run the method that they found to have the lowest type I error rates, Bootstrapped eigenvector.
#For reference, this is the method 6 in Peres-Neto et al. (2003).
sigpca2<-function (x, permutations=1000, ...)
{
  pcnull <- princomp(x, ...)
  res <- pcnull$loadings
  out <- matrix(0, nrow=nrow(res), ncol=ncol(res))
  N <- nrow(x)
  for (i in 1:permutations) {
    pc <- princomp(x[sample(N, replace=TRUE), ], ...)
    pred <- predict(pc, newdata = x)
    r <-  cor(pcnull$scores, pred)
    k <- apply(abs(r), 2, which.max)
    reve <- sign(diag(r[k,]))
    sol <- pc$loadings[ ,k]
    sol <- sweep(sol, 2, reve, "*")
    out <- out + ifelse(res > 0, sol <=  0, sol >= 0)
  }
  out/permutations
}

set.seed(4)

sigpca2(ZusAir, permutations=1000)

#PCA plots
#Plot out the factor loadings for the first 2 PC axes:
plot(usAir_pca$loadings,type="n",xlab="PC 1, 39%", ylab="PC 2, 21%",ylim=c(-.8,.8), xlim=c(-.6,.6))

text(usAir_pca$loadings, labels=as.character(colnames(ZusAir)), pos=1, cex=1)

#Let’s now plot the PC score for each sample (city):
plot(usAir_pca$scores,type="n",xlab="PC 1, 39%", ylab="PC 2, 21%",ylim=c(-2.5,4), xlim=c(-4,8))
text(usAir_pca$scores, labels=as.character(rownames(ZusAir)), pos=1, cex=1)

#And now all together in a biplot:
biplot(usAir_pca$scores,usAir_pca$loading,xlab="PC 1, 39%", ylab="PC 2, 21%",expand= 4, ylim=c(-6.5,6), xlim=c(-4,7.5))
#to replace city names with a symbol:
biplot(usAir_pca$scores,usAir_pca$loading, expand= 4, xlabs= rep("*",41),xlab="PC 1, 39%", ylab="PC 2, 21%",ylim=c(-6.5,6), xlim=c(-4,8))
#Eigen Analysis
#You can also just simply use the Eigen analysis function, eigen and calculate your own scores by hand:
eig<-eigen(cov(ZusAir))
#Extract the first two eigenvectors (because that is what we are interested in plotting):
eigVec<-as.matrix(eig$vectors[,1:2])
rownames(eigVec) <- rownames(cov(ZusAir))
#Then simply multiply each eigenvector times the matrix of standardized observation values (ZusAir) and plot!
scores<-t(rbind(eigVec[,1]%*%t(ZusAir),eigVec[,2]%*%t(ZusAir)))
#hand calculated scores

# and plot

biplot(scores,eigVec,xlab="PC 1, 39%", ylab="PC 2, 21%",expand= 4, ylim=c(-6.5,6), xlim=c(-4,7.5))

