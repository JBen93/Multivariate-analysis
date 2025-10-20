# Install and load packages 
#install.packages("mvnormtest")
#install.packages("MVN")
#install.packages("MVA")
#install.packages("psych")
#install.packages("Hmisc")
#install.packages("vegan")
#install.packages("StatMatch")
#install.packages("MASS")
#install.packages("raster")
#install.packages("cluster")
#clear the environment
rm(list =ls())
#Set up renv
renv::restore()
#set your working directory
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/Multivariate analysis")
library(mvnormtest)
library(MVN)
library(MVA)
library(psych)
library(Hmisc)
library(vegan)
library(StatMatch)
library(MASS)
library(raster)
library(cluster)


#Importing Data

usAir<-USairpollution
usAir
#or
usAir<-read.csv("usAir_mod.csv", row=1, header = TRUE)
usAir

#Data screening
#Your first move when conducting a multivariate analysis (or any analysis) is to screen the data.
#You are looking for data errors, missing data, and outliers that may influence your analysis.

#Data errors
#One way to check for data errors is to examine the summary statistics for your data set.
#First look at the summary statistics for usAir:
describeBy(usAir)

#Missing Data
#When you have missing entries in your data sheet, R replaces them with “NA”. You can check if you have any missing variables in usAir_mod:
describe(usAir)
#describe provides some of the same information as describeBy, but importantly shows you which variables have missing values.
#We talked about three methods for dealing with missing values in lecture; Complete Case, Available Case, Imputation. We will look at complete case and imputation for now.
#Complete Case involves the removal of samples (in this case cities) with missing data:
usAir [complete.cases(usAir),]
#Imputation involves filling in missing values with plausible data. Let’s replace NAs with the mean of the variable.
#First, let’s calculate the mean of each variable (column) with the NA removed:

meanz<-colMeans(usAir,na.rm=T)
meanz
#`na.rm=T`, means that you want to remove NAs
#To replace your NAs with the means you just calculated you will use the following function:
naFunc<-function(column) { column[is.na(column)]  = round(mean(column, na.rm = TRUE),2)
  return(column)}
#and “apply” it to the usair_mod data set

Impute<-apply(usAir,2,naFunc)
Impute

#Multivariate Normal Distribution
#Shapiro-Wilks test
#Shapiro-Wilks tests if the distribution of the observed data differs from multivariate normal distribution. So, we are looking for p-values > 0.05.
mshapiro.test(t(usAir))
?t
mvn(usAir, mvnTest = "mardia")

#Data transformation
#The next step is preparing your data for analysis is transforming the data. 
#Today we will look at the log square root, and arcsine square root transformations.
#Log transformation: b_ij=log(x_ij+1)
#First, let’s look at a histogram of our first variable, SO2, to determine if transformation is necessary:
#Remember, to extract the SO2 column:
  usAir$SO2 
  #or 
  usAir[,1] 
  
#Next you can simply wrap either of those commands in the histogram function:
  
  hist(usAir$SO2) 
  
 #or 
  
  hist(usAir[,1])  
  
#To log transform each value in our data frame:
    usAirlog<-log1p(usAir)
#and the histogram:
    hist(usAirlog$SO2) 
 
#You can compare the histograms side by side using the par function followed by hist:
par(mfrow=c(1,2))
#Placing 1, 2 in parentheses after the c (which stands for concatenate) in the par function indicates that you want your plots arranged in 1 row and two columns.
    
hist(usAir[,1])  
hist(usAirlog[,1])  

#Square root transformation: b_ij=√(x_ij )
#To square root transform each value in our data frame:
  usAirsqrt<-sqrt(usAir)
#and the histogram:
  hist(usAirsqrt$SO2)
#Compare the histograms side by side using the par function followed by hist:
    par(mfrow=c(1,2))
  
  hist(usAir[,1])  
  hist(usAirsqrt[,1]) 
#Remember that square root transformations are best used on count data. 

#Arcsine square root transformation: b_ij = arcsine√(x_ij )
#If you remember arcsine square root transformations are for percentage data. 
#So, your values for your variable must lie between 0 and 1.
#Let’s draw some random numbers between 0 and 1 so we can use the arcsine square root transformation.
  newData<- runif(100, 0, 1)
  newData
#You just chose 100 random values between 0 and 1. Now let’s transform:
asin(sqrt(newData))
# and compare histograms:
    par(mfrow=c(1,2))
  
  hist(newData)
  hist(asin(sqrt(newData)))

#Data standardization##
  
#Column standardization adjusts for differences among variables
#Row standardization adjusts for differences among sample units. 
#Focus is on the profile within a sample unit (good when variables are measured in the same units, e.g. species)
  
#Coefficient of Variation (cv)
#Let’s first see if the air pollution data set needs standardization by calculating the coefficient of variation (cv) for columns totals. 
#Remember, the cv is the ratio of the standard deviation to the mean (σ/μ):
#First calculate the column sums:
cSums<-colSums(usAir)
#Then calculate the standard deviation and mean for the column sums:
Sdev<-sd(cSums)
M<-mean(cSums)
#Finally, calculate the cv:
Cv<-Sdev/M*100
Cv
#Our rule of thumb for cv is that if cv> 50, data standardization is necessary. What do you think for the USairpollution data?  

#Z- standardization b_ij = (x_ij-¯(x_j ))/s_j
#Your goal here is to equalize the variance for variables measured on different scales. There is a built-in function scale that will do this for you:
scaledData<-scale(usAir)
#Let’s look at histograms for the scaled and unscaled data for the first variable, SO2:
  par(mfrow=c(1,2))

hist(usAir[,1] ,main=colnames(usAir)[1],xlab=" ")
hist(scaledData[,1] ,main=colnames(usAir)[1],xlab=" ")

#Detecting Outliers
#Univariate outliers
#One way to deal with outliers in multivariate data is to examine each variable separately. 
#First the z-standardization:
scaledData<-scale(usAir)
#Next, we will create histograms to look for values > than 3 sd. However, this time we will use the par function to look at all seven histograms at once.
par(mfrow=c(2,4))
hist(scaledData [,1] ,main=colnames(usAir)[1],xlab=" ")
hist(scaledData [,2] ,main=colnames(usAir)[2],xlab=" ")  
hist(scaledData [,3] ,main=colnames(usAir)[3],xlab=" ")  
hist(scaledData [,4] ,main=colnames(usAir)[4],xlab=" ")
hist(scaledData [,5] ,main=colnames(usAir)[5],xlab=" ")  
hist(scaledData [,6] ,main=colnames(usAir)[6],xlab=" ")  
hist(scaledData [,7] ,main=colnames(usAir)[7],xlab=" ")
#Finally, you can identify the outlier(s) for each variable:
scaledData [,1][scaledData [,1]>3] 
scaledData [,2][scaledData [,2]>3]  
scaledData [,3][scaledData [,3]>3] 
scaledData [,4][scaledData [,4]>3]
scaledData [,5][scaledData [,5]>3]
scaledData [,6][scaledData [,6]>3]  
scaledData [,7][scaledData [,7]>3]

#Distance and Dissimilarity
#Several distance and dissimilarity measures are used to calculate the distance between data points.

#Euclidean Distance:
#Euclidean distance is one of the most used distance measures.
#It is normally preceded by column standardization (e.g. z standardization)
#You will use the function vegdist from the vegan (vegetation analysis) package
?vegdist
#First, z standardization:
scaledData<-scale(usAir)
scaledData

#Then calculate distance:
eucDist<- vegdist(scaledData,"euclidean")

#Let’s look at a histogram of distances:
hist(eucDist)
#City-block (Manhattan) distance
cbDist<- vegdist(scaledData,"manhattan")

#Bray-Curtis dissimilarity
brayDist<- vegdist(usAir,"bray")

#Histogram:

hist(brayDist)




  