###Lab 11: RDA and Variance partitioning###
##The goal of this lab is to apply Redundancy Analysis (RDA) to determine the influence of explanatory variables on patterns of variation in multivariate response variables.
##Constrained ordination is an extension of unconstrained ordination techniques in which the solution is constrained to be expressed by explanatory variables. 
##You will also explore variable selection and variance partitioning to further interpret the results of RDA. These methods can also be applied to CCA.
rm(list =ls())
#Set up renv
renv::restore()
#set your working directory
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/Multivariate analysis")
##Download packages###
##We will be using the following packages:
library(vegan)

##Import Data##
##Today you will be using a Doubs fish data set (Verneaux 1973) from the Doubs River in France and Switzerland. 
##This data set includes a site by species matrix (“DoubsSpe.csv”) that includes fish abundances for 27 species across 30 sites and a site by environment (“DoubsEnv.csv”) matrix containing measurements of 11 environmental variables across 30 sites.
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/multivariate_2023")
spe <- read.csv("DoubsSpe.csv", row.names=1)
env <- read.csv("DoubsEnv.csv", row.names=1)

#########Modify Data
#remove site 8 -we removed this site 8 because it does not have any species. 
spe <- spe[-8, ]
env <- env[-8, ]

env$pen
# Remove the 'das' variable from the env dataset
env <- env[, -1]
#Explore the data set and modify it in the following way:  1. Notice that one row (i.e., site) has no species. Remove that row before proceeding. 2. Also, remove the variable “das” from the data set. 3. Recode the slope variable (pen) into a factor (qualitative) variable (to explore how these are handled in the ordinations). Here you divide the data into quantiles and assign a factor level to each quantile (very steep, steep, moderate, low): 

##Incorporate the new qualitative slope variable “pen2” into the new environmental data frame:
env2 <- env
env2$pen <- pen2

###Data transformation##
##A common transformation for species data in RDA is the Hellinger transformation. 
#This transformation is simply the square root of row normalized data (i.e., relative abundance). 
#This transformation has been shown to have good statistical properties for constrained ordination. You will use the function deconstand in the vegan package:
spe.hel <- decostand(spe, "hellinger")

#Run RDA using function rda in vegan using all the environmental variables.
?rda
spe.rda <- rda(spe.hel ~ ., env2)
#Explore the results:
summary(spe.rda)
#Unadjusted R^2 retrieved from the rda object
R2 <- RsquareAdj(spe.rda)$r.squared
#Adjusted R^2 retrieved from the rda object
#R2adj <- RsquareAdj(spe.rda)$adj.r.squared
#Plot using the F-scores:
plot(spe.rda, scaling=1, 
     main="Triplot RDA spe.hel ~ env2 - scaling 1 - wa scores")
spe.sc <- scores(spe.rda, choices=1:2, scaling=1, display="sp")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

##Plot using the Z-scores:
  plot(spe.rda, scaling=1, display=c("sp", "lc", "cn"), 
       main="Triplot RDA spe.hel ~ env2 - scaling 1 - lc scores")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

#Conduct a permutation test using anova function in vegan to test the significance of the model and individual axes:
anova(spe.rda, step=1000)

##Tests of all canonical axes:
anova(spe.rda, by="axis", step=1000)

##Model selection
##Variable Selection: forward selection using ordiR2step in vegan.
##We realize that we have used too many variables and that many variables are closely related in our data set. Here we want to reduce the number of variables while maintaining a model that describes as much variance as possible. Here selection is based on increasing and R2 and p-value of the permutation test. See settings in ordiR2step for more options. 
??ordiR2step
step.forward <- ordiR2step(rda(spe.hel ~ 1, data=env2), 
                           scope=formula(spe.rda), R2scope = F, direction="forward", pstep=1000)
##??What is the most parsimonious model?? 
##Conduct an RDA with this model.
##Partial RDA
##Partial RDA allows us to quantify the unique contribution of an explanatory variable in describing variation in a response data set. It allows us to hold other explanatory variables constant while assessing the role of the focal variable. Here, I show a partial RDA for the altitude variable “alt”. You can conduct a partial RDA for any or all explanatory variables.
partial.alt <- rda(spe.hel ~ alt + Condition(oxy + dbo + dur) , data=env2)
summary(partial.alt)
R2adj<-RsquareAdj(partial.alt)$adj.r.squared
##Explore the results of this model and conduct a permutation test to assess the significance of this model. Feel free to explore partial RDA for other variables in the data set.
#here you testing the significance of each explanatory variable. 
anova(partial.alt)
##Variance partitioning
##Variance partitioning allows us quantify the amount of variation attributed to each variable and the amount that is shared between variables. In some ways, it is an extension of partial RDA. We can partition variance between up to four variables (or matrices) with the varpart function in vegan. You can use more variables but have to partition by hand.
??varpart
##Here we partition the variance for the model we constructed through forward selection above:
spe.part <- varpart(spe.hel,~ alt, ~oxy,~ dur ,~dbo ,data=env2)
spe.part

plot(spe.part, digits=2)
plot(spe.part,digits=2,Xnames = c('alt','oxy','dur','dbo'),bg=2:5)

## separate your explanatory variables into topographical and water quality
top<-env[,1:3]
chem<-env[,4:10]
spe.part <- varpart(spe.hel, top, chem)
plot(spe.part,digits=2,Xnames = c('topo','chem'),bg=2:5)

##End

###You can also also test the significance of the variance fractions using a permutation test.
##Handmade RDA adapted from Borcard et al. (2011)
Yc <- as.matrix(spe.hel)  #your Hellinger transformed site by species matrix

Xcr <- as.matrix(env)# here we are using the slope variable  “pen” as continuous.

# 2. Computation of the multivariate linear regression
# ****************************************************

# Matrix of regression coefficients (eq. 11.4)
B <- solve(t(Xcr) %*% Xcr) %*% t(Xcr) %*% Yc

# Matrix of fitted values 
Yhat <- Xcr %*% B

# Matrix of residuals
Yres <- Yc - Yhat

# Dimensions
n <- nrow(Yc)
p <- ncol(Yc)
m <- ncol(Xcr)
# 3. PCA on fitted values
# ***********************
# Covariance matrix 
S <- cov(Yhat)

# Eigenvalue decomposition-gives eigenvalues and eigenvectors (factor loadings)
eigenS <- eigen(S)

# How many canonical axes to keep/explore?
kc <- length(which(eigenS$values > 0.00000001))

# Eigenvalues of canonical axes
ev <- eigenS$values[1:kc]

# Total variance (inertia) of the response matrix
trace = sum(diag(cov(Yc)))

# Orthonormal eigenvectors (species score/FACTOR LOADINGS)
U <- eigenS$vectors[,1:kc]
row.names(U) <- colnames(Yc)

# Site scores (F-SCORES)
F <- Yc %*% U
row.names(F) <- row.names(Yc)

# Site constraints (Z-SCORES)
Z <- Yhat %*% U
row.names(Z) <- row.names(Yc)

# Canonical coefficients (regression coefficients for each explanatory variable on #each rda axis)
CC <- B %*% U
row.names(CC) <- colnames(Xcr)

# Explanatory variables
# Species-environment correlations
corXZ <- cor(Xcr,Z)

# Diagonal matrix of weights
D <- diag(sqrt(ev/trace))

# Biplot scores of explanatory variables
coordX <- corXZ %*% D    # Scaled


# Unadjusted R2
R2 <- sum(ev/trace)
# Adjusted R2
R2a <- 1-((n-1)/(n-m-1))*(1-R2)


result <- list(trace, R2, R2a, ev, CC, U, F, Z, coordX)

names(result) <- c("Total_variance", "R2", "R2adj", "Can_ev", "Can_coeff", 
                   "Species_sc1", "wa_sc1", "lc_sc1", "Biplot_sc1")

