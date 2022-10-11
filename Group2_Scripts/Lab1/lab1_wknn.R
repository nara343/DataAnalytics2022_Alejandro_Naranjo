require(kknn)
data(ionosphere)

#Splitting the data into a training/testing 
ionosphere.learn <- ionosphere[1:200,]
ionosphere.valid <- ionosphere[-c(1:200),]

# Creating the model using the training and testing set
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
table(ionosphere.valid$class, fit.kknn$fit)

# Training the model using the training set using a distance of 1
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)

# Training using a distance of 2
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)


data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
	prob = rep(1/m, m)) 

#Splitting the data into train and test 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
	kernel = "triangular")
summary(iris.kknn)

fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")
	[(iris.valid$Species != fit)+1])
