require(kknn)
data(ionosphere)
#Splitting into train and test 
ionosphere.learn <- ionosphere[1:200,]
ionosphere.valid <- ionosphere[-c(1:200),]

#Creating the model
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
#Looking at the model with the class data
table(ionosphere.valid$class, fit.kknn$fit)
#training the model with the train class
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
#Training the class with different parameters
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)

# Using Iris
data(iris)
m <- dim(iris)[1]
# Sampling 
val <- sample(1:m, size = round(m/3), replace = FALSE, 
	prob = rep(1/m, m))

# Spliting into train and test
iris.learn <- iris[-val,]
iris.valid <- iris[val,]

# Creating the model 
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
	kernel = "triangular")
summary(iris.kknn)
# Fiting the model 
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")
	[(iris.valid$Species != fit)+1])

