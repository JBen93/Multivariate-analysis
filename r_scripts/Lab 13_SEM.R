##Lab 13 Structural Equation Modeling (SEM) and Path Analysis##
##The goal of this lab is to apply SEM techniques to determine relationships within a multivariate data set when variables can be both predictor and response variables simultaneously.
#Set up R session
#Download packages
#We will be using the following packages:
install.packages("lavaan")
install.packages("piecewiseSEM")
install.packages("semPlot")
install.packages("dagitty")

library(lavaan)
library(piecewiseSEM)
library(semPlot)
library(mvnormtest)
library(MVN)
library(dagitty)

#Import data
#Today you will be using a data set looking at fire severity and plant richness in Costal California that contains 8 variables measured over 90 sites.
#After downloading the piecewiseSEM library, call in the data set keeley and explore it:
#To learn more about the data sets:
?keeley
#Develop Hypotheses with path diagrams
#Path diagrams provide the opportunity to visualize hypotheses to be tested in SEM. 
#Here we use the library dagitty to plot our hypotheses.

#Indirect Model###
#detail all the relationships for the hypothesis/path diagram
indirect <- dagitty("dag{
             age -> fire_sev -> richness
             elevation -> fire_sev -> richness
             distance -> richness
           }")
#plot the path diagram
plot(graphLayout(indirect))

#Direct Model###
#detail all the relationships for the hypothesis/path diagram
direct <- dagitty("dag{
             age -> fire_sev -> richness
             elevation -> fire_sev -> richness
             distance -> richness
             age ->richness
             elevation ->richness
           }")
#plot the graph
plot(graphLayout(direct))

#Model Specification
#Our next step is to specify the models for the path diagrams we have developed.

#Indirect Model###
Model1<-' firesev ~ age+ elev
rich ~ firesev + distance'
#Run the model using the sem function in lavaan
indirect<-sem(Model1,data=keeley,meanstructure=T)

##Direct Model###
Model2<-' firesev ~ age +elev
rich ~ firesev + age + elev + distance'
#Run the model using the sem function in lavaan
direct<-sem(Model2,data=keeley,meanstructure=T)

##Model Assumptions
#SEM using a covariance matrix has several assumptions. 
#Arguably, the most important is that the data are multivariate normal. Although we know that tests of multivaraite normality tend to be conservative, we will run one here.
#First, we will run the MVN test on the data set to determine if the data are multivariate normal.
#Testing normality of the observed data
#This test is the same for both the direct and indirect model because ther observed data are the same. Univariate normality is also tested
#pull the observed data used in each model
raw_data <- inspect(indirect, "data")
mvn(raw_data, mvnTest="mardia")

#Exploring the model parameters and goodness-of-fit#
#Indirect Model
indirect<-sem(Model1,data=keeley)

#Summary includes unstandardized coefficents and chi-square test
sum_indirect<-summary(indirect)

#standardized coefficients
standardizedSolution(indirect) 

#Rsquared for endogenous variables
inspect(indirect, "r2")
#Assess Goodness-of-Fit. Look at the lecture slides for "rules of thumb" on goodness-of-fit mesures from Klien et al. 2012.
fitMeasures(indirect, c("chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
#plot path diagram with standardized coefficients
semPaths(indirect,"std")

#Direct Model#
direct<-sem(Model2,data=keeley)

#Summary includes unstandardized coefficents and chi-square test
sum_indirect<-summary(direct)
#Rsquared for endogenous variables
inspect(direct, "r2")

#Assess Goodness-of-Fit. Look at the lecture slides for "rules of thumb" on goodness-of-fit mesures from Klien et al. 2012.
fitMeasures(direct, c("chisq", "df", "pvalue", "cfi", "rmsea","srmr"))

#plot path diagram with standardized coefficients
semPaths(direct,"std")

##Comparing Models
#The ability to compare different hypotheses depicted by path diagrams is a strenght of SEM models. 
#Here we will use AIC, BIC, and a likelihood ratio test to compare our direct and indirect hypotheses.
#Likelihood ratio test using the `anova` function
anova(indirect, direct)
#Comparing mopdels with AIC and BIC
AIC(direct, indirect)
BIC(direct, indirect)

#Estimating and testing indirect effects
#We can walk through this together
#Indirect model
Model1<-' 
rich ~ b * firesev + d * distance
firesev ~ a1 * age + a2 * elev
indirect1:=a1*b
indirect2:=a2*b
total:=d+(a1*b)+(a2*b)'
indirect<-sem(Model1,data=keeley)
standardizedsolution(indirect)

#Direct model
Model2<-' 
rich ~ b * firesev + d * distance + c*age + e*elev
firesev ~ a1 * age + a2 * elev
indirect1:=a1*b
indirect2:=a2*b
total:=d+c+e+(a1*b)+(a2*b)'

direct<-sem(Model2,data=keeley)
standardizedsolution(indirect)

#piecewiseSEM
#Let’s runs the same models using local estimation in the package ‘piecewiseSEM’
#Indirect model
#model "pieces"
fire<-lm(firesev ~ age+ elev,data=keeley)
richness<-lm(rich ~ firesev + distance,data=keeley)

#check model assumptions
fire_res<-residuals(fire)
richness_res<-residuals(richness)
res_indirect<-cbind(fire_res,richness_res)
#qqplots
apply(res_indirect[,1:2], 2, function(x){
  qqnorm(x, cex=1.5, cex.lab=1.5, cex.axis=1.3)
  qqline(x)
})

#Plotting residuals vs. predicted values to assess homogeneity of variance and linearity

#fire
predicted_fire<-predict.lm(fire)

#plot residuals vs. predicted values
plot(predicted_fire,fire_res, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")
#Richness
predicted_richness<-predict.lm(richness)

#plot residuals vs. predicted values
plot(predicted_richness,richness_res, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

#"cobble" the model together using the "psem" function 
model_indirect <- psem(fire,richness)
#use the summary function to evaluate model
summary(model_indirect)


##Goodness of fit
##The goodness-of-fit of a piecewise structural equation model is obtained using ‘tests of directed seperation’. The P-values of these tests are then combined in a single Fisher’s C statistic which is χ2-distributed with 2k degrees of freedom. As with other goodness-of-fit tests, we are looking for p-values >0.05.
#Direct model
#model "pieces"

fire2<-lm(firesev ~ age+ elev,data=keeley)
richness2<-lm(rich ~ firesev + distance+age+elev,data=keeley)

#check model assumptions
fire_res2<-residuals(fire2)
richness_res2<-residuals(richness2)

res_indirect2<-cbind(fire_res2,richness_res2)

#qqplots
apply(res_indirect2[,1:2], 2, function(x){
  qqnorm(x, cex=1.5, cex.lab=1.5, cex.axis=1.3)
  qqline(x)
})

#residuals vs. predicted values

#fire
predicted_fire2<-predict.lm(fire2)

##  plot residuals vs. the predicted values:
plot(predicted_fire2,fire_res2, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

#Richness
predicted_richness2<-predict.lm(richness2)
##  plot residuals vs. the predicted values:
plot(predicted_richness2,richness_res2, ylab=" Residuals", xlab= "Predicted Y")
abline(a=0,b=0, col="red", lwd=3,lty="dashed")

#cobble the model together using the "psem" function 
model_direct <- psem(fire2,richness2)

#use the summary function to evaluate model
summary(model_direct)

#Compare models using AIC
AIC(model_direct,model_indirect,AIC.type="dsep")

AIC_psem(model_direct,AIC.type = "dsep")
AIC_psem(model_indirect,AIC.type = "dsep")



#Not a huge difference between the models. I would go with the more parsimonious model (i.e., indirect)








  