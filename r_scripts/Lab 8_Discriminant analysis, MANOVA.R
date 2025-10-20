#####Lab 8 Discriminant Analysis (DA)######
#The goal of this lab is to become familiar with Discriminant Analysis (DA). 
#DA allows you to determine what variables are important in separating groups. 
#It is an eigenanalysis that reduces dimensionality of multivariate data in to new “canonical axes”.
#Set up R session
#Data
#Today you will be using the iris data set found in the package datasets in R. To access this data set, simply type:
  iris
#To learn more about the data set:
  ?iris
#Install and load packages
install.packages("dplyr")
install.packages("MASS")
install.packages("candisc")
install.packages("ade4")
install.packages("vegan")
install.packages("ggplot2")
install.packages("heplots")
install.packages("permute")
install.packages("lattice")
install.packages("rgl")

#We will be using the following packages:
library(dplyr)
library(MASS)
library(ade4)
library(vegan)
library(permute)
library(lattice)
library(ggplot2)
library(heplots)
library(rgl)
library(candisc)
library(mvnormtest)
library(MVN)

#Testing the assumptions of DA
#Homogeneity of within-group variance-covariance matrices
#First, lets look at these groups in NMDS space.
dis <- vegdist(iris[,1:4], "euclidean")
iris_nmds<-metaMDS(dis)
#Now lets plot but using ggplot2 this time.
#First build a data frame with the first two axes and the groups (i.e., species names):
NMDS=data.frame(NMDS1=iris_nmds$point[,1],NMDS2=iris_nmds$point[,2],species=iris$Species)
ggplot(NMDS,aes(x=NMDS1,y=NMDS2))+
  geom_point(data=NMDS,aes(x=NMDS1,y=NMDS2, color=species), alpha = 0.5)+
  viridis::scale_fill_viridis()+
  theme_bw() 
##Next, we will use the Fligner-Killeen test of homogeneity of variances. 
#This test has been shown to be robust to departures from normality (which most of our data does to some extent)
?fligner.test
#You want to test the variance of each variable (n=4) across all three groups (i.e.’ species). 
#Remember, you don’t want there to be significant differences
fligner.test(iris$Sepal.Length, iris$Species)
fligner.test(iris$Sepal.Width, iris$Species)
fligner.test(iris$Petal.Length, iris$Species)
fligner.test(iris$Petal.Width, iris$Species)
#We will also use a Multivariate test of homogeneity of variance called ‘betadisper’ in the vegan package.
?betadisper

#Calculate multivariate dispersions:
  
# Euclidean distances between samples
dis <- vegdist(iris[,1:4], "euclidean")

# groups are the three different species
groups <- iris$Species

#multivariate dispersions
MVdisp <- betadisper(dis, groups)


#Perform parametric test
disp_aov<-anova(MVdisp)

# Tukey's Honest Significant Differences
MVdisp.HSD <- TukeyHSD(MVdisp)
MVdisp.HSD

## non-parametric test: Permutation test for F
perm_MVdisp <- permutest(MVdisp, permutations = 99, pairwise = TRUE)
perm_MVdisp

#Should you transform the iris variables?
#If you think yes, use the code below. If you think no, skip ahead.
log<-cbind.data.frame(apply(iris[,1:4]+1,2,log),iris$Species)
names(log)[5]<-"Species"
#Re-run the Fligner-Killeen test on the transformed data:
fligner.test(log$Sepal.Length,log$Species)
fligner.test(log$Sepal.Width ,log$Species)
fligner.test(log$Petal.Length ,log$Species)
fligner.test(log$Petal.Width, log$Species)

#Re-run multivariate homogeneity of variance test
# Euclidean distances between samples
log_dis <- vegdist(log[,1:4], "euclidean")

# groups are the three different species
groups <- iris$Species

## Calculate multivariate dispersions
log_MVdisp <- betadisper(log_dis, groups)

#Perform parametric test
disp_aov<-anova(log_MVdisp)

# Tukey's Honest Significant Differences
MVdisp.HSD <- TukeyHSD(log_MVdisp)
MVdisp.HSD
# non-parametric test: Permutation test for F
perm_MVdisp <- permutest(log_MVdisp, permutations = 99, pairwise = TRUE)
perm_MVdisp

#Multivariate Normality
#To look for multivariate normality, let’s look at the distribution of each variable for each group and run (overly conservative) mulitvariate normality tests for both the log and untransformed Iris data.
#filter for each species: e.g., setosa below

setosa<-dplyr::filter(iris, Species == "setosa")

#untransformed

mshapiro.test(t(setosa[,1:4]))

mvn(setosa[,1:4], mvnTest = "mardia")

#log

mshapiro.test(t(log(setosa[,1:4])))

mvn(log(setosa[,1:4]), mvnTest = "mardia")

#Multicolinearity
#To test for multicolinearity, you should look at all pairwise correlations. Remember, correlations > 0.7 can be trouble.
cor(iris[,1:4])
#We have some co-linearity. Let’s move ahead using all four variables, but we will also run the analysis without Petal.Length.

####Outliers###
###Outliers can have a large influence on canonical axes in DA. You are going to take a multivariate approach to identifying outliers for both the untransformed and transformed data (we did this in lab 2):
#Calculate a withi-group distance matrix:
#setosa
  
eucDist<- vegdist(iris[1:50,1:4],"euclidean")

#Calculate the average distance of each sample to all other samples (i.e. column average) and turn the means in z-scores:

multOut<-scale(colMeans(as.matrix(eucDist)))

#Now look at a histogram of these data to identify samples that are > 3 sd from the mean:

hist(multOut)

#and get the number of those samples:

setosa_out<-multOut [multOut >3,]
setosa_out
#Repeat for the other two groups (i.e.,species):

#versicolor
eucDist<- vegdist(iris[51:100,1:4],"euclidean")
multOut<-scale(colMeans(as.matrix(eucDist)))
hist(multOut)
versicolor_out<-multOut [multOut >3,]
versicolor_out

#virginica
eucDist<- vegdist(iris[101:150,1:4],"euclidean")
multOut<-scale(colMeans(as.matrix(eucDist)))
hist(multOut)
virginica_out<-multOut [multOut >3,]

#Finally, make a vector of the outliers to pull out of the data set later:
Outliers<-c(setosa_out,versicolor_out,virginica_out)
Outliers

###Linearity###
##Next we test for linear relationship between variables. This is key to making sure that the variables change in a linear fashion along underlying gradients (i.e. canonical axes):
pairs(iris[,1:4])

###Discriminant analysis###
###You will use the LDA function in the MASS package to conduct DA and will also use the candisc function in package candisc.
?lda
?candisc
#First run the analysis on the un-transformed data with all of the variables. 
#We are going to split our data set into “training” data and “testing” data. 
#We randomly select 75 samples from the iris data set:

set.seed(11)
train <- sample(1:150, 75)
#Check the frequency of each species in the training data, which will help set your priors:
prior<-table(iris$Sp[train])
#Next run the DA for the training data:
iris.LDA <- lda(Species ~ ., iris, prior = cbind(prior/75), subset = train)
#“Species ~ .” is the model and the “dot” stands for all of the variables (so you don’t have to type them all)
#What do the results tell you about the number of meaningful canonical axes and the absolute contribution of each variable to those axes?
 
####Assessing and interpreting canonical axes####
##Relative % Criterion##
##The “proportion of trace” from the lda output is the Relative % criterion.
#Canonical Correlation Criterion
#Here, you want to test the correlation between the canonical scores and the grouping variable. First you need to predict the membership (i.e. calculate canonical scores) of each sample in the training set. You will use the predict.lda function from the MASS package:
  ?predict.lda
iris.LDA.p<-predict(iris.LDA, iris[train, ])
#You want to test the canonical scores (iris.LDA.P$x) with the grouping variable (i.e. species):
corTest<-lm(iris.LDA.p$x~iris$Sp[train])
summary(corTest)
#How does the correlation and significance look for each canonical axis?
#Plot canonical scores and axes
#Now visually asses the discrimination of the three species of iris by the canonical axes:
plot(iris.LDA, xlim=c(-11,11), ylim=c(-6,6))

#Classification accuracy
#You now want to measure how well the axes discriminate. 
#The higher the correct classification rate, the greater degree of group discrimination achieved by the canonical axes. 
#We will classify the training data first. Create a classification matrix:
ct<- table( iris[train, ]$Species, predict(iris.LDA, iris[train, ])$class) 

#Change to a table of proportions:
pct<-prop.table(ct)
pct
#Calculate classification rate by summing the diagonal:
sum(diag(pct))
#Interpreting canonical axes (raw coefficients, standardized weights, and structure coefficients)
#Unfortunately, lda doesn’t provide an easy way to calculate the standardized weights, and structure coefficients. For this, you will use candisc in the candisc package.
?candisc
#For candisc, you build a linear model using lm:
iris.mod <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data=iris[train, ])
#And run the candisc function:
iris.can <- candisc(iris.mod, data=iris[train, ])
#Next pull the raw coefficients, standardized weights, and structure coefficients from the output:
iris.can$coeffs.raw
iris.can$coeffs.std
iris.can$structure
iris.LDA$scaling

#Validating canonical axes
#A DA is only as good as its ability to classify new data correctly. Here, you are going to use the “testing” data from your split sample to see how well the canonical axes predict group membership of “new” samples. You are going to use the predict.lda function again. This time with the testing data (i.e., iris[-train, ])
iris.LDA.new<-predict(iris.LDA , iris[-train, ])
#Next, make a classification table and calculate the classification rate:
ynew.table<-table(iris[-train,]$Species,iris.LDA.new$class)
ynew.table
sum(diag(prop.table(ynew.table)))

#MANOVA, the other side of the coin of DA
#In MANOVA, we are interested if groups differ in their measured variables. In DA, we are interested in a linear combination of variables that maximize differences between groups. In this case, MANOVA can provide a test of whether or not the groups are different.
? manova
Y<-as.matrix(iris[,1:4])
Sp<-factor(iris[,5])

fit <- manova(Y ~ Sp)
summary(fit,test="Wilks")
###Post-hoc tests###
Yset<-as.matrix(iris[1:50,1:4])
Yversi<-as.matrix(iris[51:100,1:4])
Yvirg<-as.matrix(iris[101:150,1:4])
Sp<-factor(iris[,5])
fit1 <- manova(rbind(Yset,Yversi) ~ Sp[1:100])
summary(fit1,test="Hotelling-Lawley")
fit2 <- manova(rbind(Yversi,Yvirg) ~ Sp[51:150])
summary(fit2,test="Hotelling-Lawley")
fit3 <- manova(rbind(Yset,Yvirg) ~ Sp[-c(51:100)])
summary(fit3,test="Hotelling-Lawley")
#If you have time, now run the analysis on the log transformed data without the variable “Petal.Length” and the outliers. Remove the Petal.Length column and outliers before analyzing.
#Does meeting the assumptions of DA change our interpretation of our results?
  
