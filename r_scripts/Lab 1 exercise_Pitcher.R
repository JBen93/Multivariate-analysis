rm(list =ls())
#Set up renv
renv::restore()
Pitcher<-read.csv("Pitcher.csv", row=1, header = TRUE)
Pitcher
##checking your data
dim(Pitcher)
?dim
#####let’s look at the structure of your data:
str(Pitcher)

###Manipulating data##
##The first thing that you should know and always remember when it comes to data frames or matrices is that an element in a matrix is identified by [ROW, COLUMN]

###Extracting elements
Pitcher[1,1]
Pitcher[1,7]
###Extracting single rows and columns
#To extract the first row from “usAir”:
Pitcher[1,]
#To extract the first column from “usAir”:
Pitcher[,1]

###Dropping single rows and columns
#To drop the first row from “usAir”:
Pitcher[-1,]
###To drop the first column from “usAir”:
Pitcher[,-1]

###Extracting multiple rows and columns
#You can also extract multiple rows or columns. To extract the first five rows from “usAir”:
Pitcher[1:5,]
###To extract the first five columns from “usAir”:
Pitcher[,1:5]

## Dropping multiple rows and columns
# you can also drop multiple rows or columns. To drop the first five rows from “usAir”:
Pitcher[-(1:5),]
#To drop the first five columns from “usAir”:
Pitcher[,-(1:5)]

### Selecting columns and rows with logical operators
Pitcher[Pitcher[,2]<50,]

##Transposing data
#You can transpose a data frame:
t(Pitcher)
#but to transpose a row or column, you must first convert it from a data frame into a matrix:
matrix_Pitcher<-as.matrix(Pitcher$SO2)
#then transpose it:
t(matrix_Pitcher)

##Sorting data
##rank, sort, and order
temp<-Pitcher$temp
temp

#Let’s apply each of these functions:
ranks<-rank(temp)
sorted<-sort(temp)
ordered<-order(temp)

#and combine them with the original temp values in a data table:
table<-data.frame(temp,ranks,sorted,ordered)

#Mathematical operations on:
#Rows and Columns
#To calculate mean, median, variance, or sum of a specific column, in this case column 3:
mean(Pitcher[,3])
median(Pitcher [,3])
var(Pitcher [,3])
sum(Pitcher [,3])
#To calculate mean, median, variance, or sum of a specific row, in this case row 3, we need to transpose the row first:
mean(t(Pitcher [3,]))
median(t(Pitcher [3,]))    
var(t(Pitcher [3,]))  

#Data frames and matrices 
columnSum<-colSums(Pitcher)
rowSums(Pitcher)
colSums(Pitcher)
colMeans(Pitcher)
rowMeans(Pitcher)

##Exporting Data
write.csv(columnSum, "Output/filename.csv")