install.packages("ISLR")
install.packages("dplyr")
library(dplyr)
library(ISLR)

head(Hitters)
dim(Hitters)

is.na(Hitters)

HittersData <- na.omit(Hitters)
dim(HittersData)
glimpse(HittersData)

head(HittersData)
SalaryPredictModel1 <- lm(Salary~., data=HittersData)
summary(SalaryPredictModel1)

cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD > (3*mean(cooksD, na.rm=TRUE)))]
influential

names_of_influential <- names(influential)
names_of_influential

outliers <- HittersData[names_of_influential,]

Hitters_Without_Outliers <- HittersData %>% anti_join(outliers)

SalaryPredictModel2 <- lm(Salary~., data=Hitters_Without_Outliers)
summary(SalaryPredictModel2)
