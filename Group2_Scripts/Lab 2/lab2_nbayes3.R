require(mlbench)
data(HouseVotes84)
library(klaR)
View(HouseVotes84)
#Creating the class, we are trying to classify the type of voter
# Either the person is Republican or Democrat
# Looking at the voting pattern and determining what party they belong too 
model <- NaiveBayes(Class ~ ., data = HouseVotes84)

predict(model, HouseVotes84[1:10,-1])

pred <- predict(model, HouseVotes84[,-1])
#Predicting the type of model
table(pred$class, HouseVotes84$Class)
