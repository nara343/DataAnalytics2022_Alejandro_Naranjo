data(Titanic)
#Looking at a simple example of a naive bayes model
mdl <- naiveBayes(Survived ~ ., data = Titanic)
mdl
# etc.

