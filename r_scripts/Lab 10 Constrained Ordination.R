####Lab 10 Constrained Ordination###

##The goal of this lab is to apply Constrained Ordination techniques to determine the influence of explanatory variables on patterns of variation in multivariate response variables. 
##Constrained ordination is an extension of unconstrained ordination techniques in which the solution is constrained to be expressed by explanatory variables. 
##The two approaches you will consider are 1) Redundancy Analysis (RDA), 
#which assume a linear relationship between response and explanatory variables and builds off of PCA, and 2) Canonical Correspondence Analysis (CCA), which assumes a unimodal relationship between response and explanatory variables and builds off of CA.

#Data
#Today you will be using a data set from Northern Finland that includes plant cover data for 44 species (varespec in library vegan) and 14 environmental variables (varechem in library vegan) across 24 sites.
#Load packages 
library(raster)
library(vegan)
#You will also be using a function from a Biostats package developed by Kevin McGarigal.
#Save this package (i.e., R script) to your working directory and use the source function to call it in:
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
source("biostats.r")

#Import data
#After downloading the vegan library,explore and call in the data sets varespec and varechem:
data(varespec)

data(varechem)

str(varespec)
summary(varespec)
str(varechem)
summary(varechem)

###Data selection, transformation and standardization##
#Species within community data sets vary greatly in there occurrence, abundance, and habitat specificity. 
#Species that are common, widespread and extremely abundant can obscure patterns in the ordination. 
#Species that are rare and have few occurrences in a data set may not be accurately placed in ecological space. 
#You must decide which species are “rare” and which are super abundant.
##Selecting Species##
#To explore patterns of rarity and commonness, you will use the foa function from the Biostats package. 
#This function will give you a whole series of plots that allow you to explore the occurrence and abundance patterns of the species in your data. 
#The second plot, Empirical Distribution of Species Relative Occurrence, will be the one we use to remove common and/or rare species. 

occur<-foa.plots(varespec)

rare <- which(occur[,2]<5)

common<- which(occur[,2]>95)

reduced<-varespec[,-c(rare,common)]

###Species transformations and standardizations
#First, check if species abundances are normally distributed across sites:
mapply(hist,as.data.frame(varespec[,1:44]),main=colnames(varespec[,1:44]),xlab="abundance")

##As you can see, most of the species distributions are right skewed. Use the log transformation (logx+1) to transform the species distributions for both the full and reduced datasets:
log.full<-log1p(varespec)
log.red<-log1p(reduced)

##Next, check the row and column sum variability using the coefficient of variation (cv) for both data sets:
#Full data set:
rsum<-rowSums(log.full)
csum<-colSums(log.full)


## [1] 15.6402
cv(csum)
## [1] 154.4498
#Reduced data set:
rsumRed<-rowSums(log.red)
csumRed<-colSums(log.red)
cv(rsumRed)
## [1] 41.43926
cv(csumRed)
## [1] 167.6967
#If either the row or column sums have cv >50, standardize by the total:
cSpec<-sweep(log.full,2,csum,"/")
cSpecRed<-sweep(log.red,2,csumRed,"/")

##Determine Response Model (RDA vs. CCA)
##Now that the date are reduced, transformed and standardized, you need to determine if species abundances show a linear (RDA) or a unimodal (CCA) relationship with the underlying gradient.
##First, use Detrended Correspondence Analysis (DCA) to determine the length of the canonical axes. 
##You will use the decorana function in the vegan Library. While DCA is a separate analysis with its own assumptions and multifaceted output, you will focus on axis length. 
##An axis length > 3 is evidence of a unimodal relationship. An axis length of <3 is evidence of a linear relationship. 
?decorana
decorana(cSpec)
decorana(cSpecRed)
###Set Explanatory Variables:
Vars<-varechem[,c(1,2,7)]
env<-as.data.frame(scale(Vars))

##Run CCA:
sp.CCA<-cca(cSpec~.,data=env)
##Function for plotting species abundances vs. CCA Axis 1:
  f2 <- function(x) {
    plot(x~sp.CCA$CC$wa[,1],xlab="CCA AXIS 1", ylab= "Abundance ")
  }

#Apply the function across all the species:

mapply(f2,varespec)

##Explanatory Variables
#Constrained ordination affords you the ability to include explanatory variables in the ordination. 
##You want to avoid mullitcolinearity among explanatory variables and check if they are measured on the same scale. 
##Based on a priori knowledge of this system, use the variables AL, P, and N in the ordination.

##First look at all of the pairwise correlations between these variables: 
Vars<-varechem[,c(1,2,7)]
Vars
round(as.dist(cor(Vars)),2)
##Do the variables AL, P, N look like they are measured on different scale? Check the cv to see if you need to z-standardize them:
cv(colSums(Vars))
## [1] 91.17077
#You need to make a data frame of the scaled variables to run the Constrained Ordination:
env<-as.data.frame(scale(Vars))

##Running the CCA
##You will run the constrained ordination using the cca in the vegan library.
?cca
##Unconstrained Ordination (CA)
##Before running the constrained model, run an unconstrained ordination (i.e. a regular Correspondence Analysis (CA; See Lab 4). CA will give you a measure of the amount of variation in the site by species matrix that you will try to explain with the explanatory variables (i.e. constraints).
#Full Data
ca<-cca(cSpec)
summary(ca)
plot(ca)
summary(ca)
#Reduced Data      
ca<-cca(cSpecRed)
summary(ca)
plot(ca)

#Constrained Ordination using CCA
sp.CCA<-cca(cSpec~.,data=env)
summary(sp.CCA)
##The first thing you should focus on in the summary is the proportion of “inertia” (i.e. variance) explained by the Constrained Ordination. 
##Notice that the total amount of inertia is the same as the Unconstrained Ordination you just ran.
##Now look at the eigenvalue and proportion and cumulative amount of variation.

##Monte Carlo testing of the significance of the constrained axis.
#The permutation allows you to test if you constrained axes explain more variation than would be expected randomly. 
#You will use the anova.cca function in vegan to conduct the permutation. It is “anova-like” but not an anova.  Global Test (i.e. all variables together):
anova(sp.CCA)
#Axes Tests (i.e. each axis individually):
anova(sp.CCA,by='axis')
#Variable Tests (i.e. each variable individually):
anova(sp.CCA,by='terms')
#Observed (F matrix) and Predicted (Z Matrix) Site Scores
#Now look back at you cca summary again:
summary(sp.CCA)
#The matrix labeled “Site scores (weighted averages of species scores)” is the F matrix and the matrix labeled “Site constraints (linear combinations of constraining variables)”is the Z matrix. Look at these two sets of site scores projected in ordination space:
par(mfrow=c(1,2))
plot(sp.CCA$CC$wa[,1],sp.CCA$CC$wa[,2],xlab="CCA AXIS 1", ylab= "CCA AXIS 2")
plot(sp.CCA$CC$u[,1],sp.CCA$CC$u[,2],xlab="CCA AXIS 1", ylab= "CCA AXIS 2")
#correlation between these two matrices. These correlations can lend insight as to how well the predicted site locations match the observed ones. However, they are not to be trusted as the only line of evidence.
spenvcor(sp.CCA)
#Intra-set correlations and biplot scores for the constraining variables.
#Correlations between the Z matrix (predicted site scores) and the environmental variables provide information on which variables have the largest influence on the constrained ordination. These also denote the placement of the environmental variables as vectors on the CCA tri-plot.
sp.CCA$CCA$biplot
#The Tri-Plot (using the site scores from the F matrix):
plot(sp.CCA,choices=c(1,2),display=c('wa','sp','bp'),scaling=2)
#and using the site scores from the Z matrix:
plot(sp.CCA,choices=c(1,2),display=c('lc','sp','bp'),scaling=2)






                                                                       


