#Implementation of KNN Algorithm using R
install.packages("ISLR")
library(ISLR)
head(iris)
str(iris)
#Standardizing data
standardized.sepal.length <- scale(iris$Sepal.Length)
var(standardized.sepal.length)
standardized.sepal.width <- scale(iris$Sepal.Width)
var(standardized.sepal.width)
standardized.petal.length <- scale(iris$Petal.Length)
var(standardized.petal.length)
standardized.petal.width <- scale(iris$Petal.Width)
var(standardized.petal.width)
final.data <- cbind(standardized.sepal.length,standardized.sepal.width,  standardized.petal.length, standardized.petal.width, iris[5])
head(final.data)
#Spliting the data into training set and testing set
library(caTools)
set.seed(101)
library(class)
spilt.data <- sample.split(final.data$Species, SplitRatio = 0.7)
train.data <- subset(final.data, spilt.data == TRUE)
test.data <- subset(final.data, spilt.data == FALSE)
head(train.data)
#Building a KNN Model
predicted.species <- knn(train.data[1:4], test.data[1:4], train.data$Species, k=1)
head(predicted.species)
#Misclassification rate
mean(test.data$Species != predicted.species)
#Choosing a K value
predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train.data[1:4],test.data[1:4],train.data$Species,k=i)
  error.rate[i] <- mean(test.data$Species != predicted.species)
}
#Plotting the K values
library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)
pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl + geom_line(lty="dotted",color='red')







