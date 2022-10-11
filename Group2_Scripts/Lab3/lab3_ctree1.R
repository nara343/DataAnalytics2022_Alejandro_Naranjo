require(rpart)

# Creating tree, dependent variable is the dertility and the independent are
# Arg, Edu, Catholic
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)

plot(Swiss_rpart) # try some different plot options
text(Swiss_rpart) # try some different text options

require(party)

data("iris")
# Using the iris data set
treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.

library(tree)
# Using a regular tree instead of a random forest
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)
#find "prettier" ways to plot the tree



