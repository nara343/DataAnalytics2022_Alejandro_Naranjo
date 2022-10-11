library(kknn)
require(kknn)

# We will be using weighted K-nearest Neighbor models for classification
data(iris)

#Getting the dimmension of the iris data
m <- dim(iris)[1]

val <- sample(1:m, size = round(m/3), replace = FALSE, 
	prob = rep(1/m, m)) 
#Getting the training set 
iris.learn <- iris[-val,]
#Testing set
iris.valid <- iris[val,]
#Building the model 
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
	kernel = "triangular")
#Results 
summary(iris.kknn)

#Fitting the data
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red”)[(iris.valid$Species != fit)+1])

