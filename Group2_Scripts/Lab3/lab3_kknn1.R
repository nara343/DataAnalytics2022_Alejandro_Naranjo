require(kknn)
library(kknn)
data(iris)

m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
# Splitting into Training and Testing datasets
iris.learn <- iris[-val,] 	# train
iris.valid <- iris[val,]	# test

# Training the KNN model
iris.kknn <- train.kknn(Species~., iris.learn, distance = 1, kernel = c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal") )


summary(iris.kknn)
# View of the results 
table(predict(iris.kknn,iris.valid),iris.valid$Species)

head(iris.kknn$W)
head(iris.kknn$D)
head(iris.kknn$C)
head(iris.kknn$fitted.values)

