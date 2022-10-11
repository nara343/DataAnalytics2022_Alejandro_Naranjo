require(mlbench)
data(HouseVotes84)
library(e1071)
#Creating the model and will be classifying 
model <- naiveBayes(Class ~ ., data = HouseVotes84)

#Prediting
predict(model, HouseVotes84[1:10,-1])
predict(model, HouseVotes84[1:10,-1], type = "raw")


pred <- predict(model, HouseVotes84[,-1])
#Checking the results 
table(pred, HouseVotes84$Class)

## Example of using a contingency table:
data(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
#Checking the results based off of category of Sex and Age
predict(m, as.data.frame(Titanic)[,1:3])

## Example with metric predictors:
data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris[,-5]), iris[,5])
