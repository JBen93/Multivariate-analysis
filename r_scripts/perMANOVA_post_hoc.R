####Post-Hoc Test for Permanova (use when you have greater than two groups)
iris<data(iris)
library(vegan)
data(iris)#we will use Fisher's Iris data set

install.packages("RVAideMemoire")#package that has the pairwise comparison function
library(RVAideMemoire)


# perMANOVA using adonis
Iris_perm<-adonis2(iris[,1:4]~Species,data=iris,method="euclidean")
Iris_perm

# Post-hoc Pairwise comparisons using "pairwise.perm.manova" function

pairwise.perm.manova

Iris_post<-pairwise.perm.manova(iris[,1:4],iris$Species,nperm=100)
# or
Iris_post2<-pairwise.perm.manova(dist(iris[,1:4],"euclidean"),iris$Species,nperm=100)

summary(Iris_post)


