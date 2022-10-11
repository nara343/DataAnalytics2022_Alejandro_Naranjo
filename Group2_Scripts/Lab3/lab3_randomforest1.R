require(randomForest)
library(randomForest)

# Creating the randomforest Model
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)

# Based off the dependent variable the type of model is a classification
# That is handled by the model 
print(fitKF) 	# view results
#We see we have an error rate of 20.99%
importance(fitKF) # importance of each predictor

#Using swiss data
fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
# Regression Model
print(fitSwiss) # view results

importance(fitSwiss) # importance of each predictor
varImpPlot(fitSwiss)

plot(fitSwiss)


getTree(fitSwiss,1, labelVar=TRUE)

help(randomForest) # look at all the package contents and the randomForest method options

# look at rfcv - random forest cross-validation - 
help(rfcv)

# other data....
data(imports85)

# perform randomForest and other tree methods.....

View(imports85)

df <- data.frame(imports85)

df <- na.omit(df)
summary(df)
df['make']
fit_import <- randomForest(make ~ engineSize + bore + horsepower, data=df)
