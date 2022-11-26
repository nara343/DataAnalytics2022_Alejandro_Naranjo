data("Titanic")

df <- data.frame(Titanic)
df

new_df <- data.frame()
for (n in 1:nrow(df)) {
  for (x in 1:df[n, 5]) {
    new_df <- rbind(new_df, df[n, 1:4])
  }
}

library(rpart)
library(rpart.plot)


decisionTreeModel <- rpart(Survived~.,new_df)

rpart.plot(decisionTreeModel)
install.packages("party")
library("party")

decisionCtree <- ctree(Survived~., df)
plot(decisionCtree)

df_na_removed <- df$Survived
lastHclust <- hclust(df_na_removed, method="average")
df
