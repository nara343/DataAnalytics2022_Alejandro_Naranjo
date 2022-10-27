

data("Titanic")

df <- data.frame(Titanic)

library(rpart)
library(rpart.plot)

#Splitting the data into train/test
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]
train
decisionTreeModel <- rpart(Survived~.,df)
rpart.plot(decisionTreeModel)
install.packages("party")
library("party")

decisionCtree <- ctree(Survived~., df)
plot(decisionCtree)

df_na_removed <- df$Survived
lastHclust <- hclust(df_na_removed, method="average")
df
