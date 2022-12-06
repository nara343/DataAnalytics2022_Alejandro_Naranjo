# Load Obesity data #
setwd("C:/Users/Naran/DataAnalytics2022_Alejandro_Naranjo/UCI_Obesity_And_Diabetes/Obesity datasets")
obesity_data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
View(obesity_data)
colnames(obesity_data)

# [1] "Gender"                         "Age"                            "Height"                         "Weight"                        
# [5] "family_history_with_overweight" "FAVC"                           "FCVC"                           "NCP"                           
# [9] "CAEC"                           "SMOKE"                          "CH2O"                           "SCC"                           
# [13] "FAF"                            "TUE"                            "CALC"                           "MTRANS"                        
# [17] "NObeyesdad" 

summary(obesity_data)
# Gender               Age            Height          Weight      
# Length:2111        Min.   :14.00   Min.   :1.450   Min.   : 39.00  
# Class :character   1st Qu.:19.95   1st Qu.:1.630   1st Qu.: 65.47  
# Mode  :character   Median :22.78   Median :1.700   Median : 83.00  
# Mean   :24.31   Mean   :1.702   Mean   : 86.59  
# 3rd Qu.:26.00   3rd Qu.:1.768   3rd Qu.:107.43  
# Max.   :61.00   Max.   :1.980   Max.   :173.00  
# 
# family_history_with_overweight     FAVC                FCVC            NCP       
# Length:2111                    Length:2111        Min.   :1.000   Min.   :1.000  
# Class :character               Class :character   1st Qu.:2.000   1st Qu.:2.659  
# Mode  :character               Mode  :character   Median :2.386   Median :3.000  
# Mean   :2.419   Mean   :2.686  
# 3rd Qu.:3.000   3rd Qu.:3.000  
# Max.   :3.000   Max.   :4.000  
# 
# CAEC              SMOKE                CH2O           SCC           
# Length:2111        Length:2111        Min.   :1.000   Length:2111       
# Class :character   Class :character   1st Qu.:1.585   Class :character  
# Mode  :character   Mode  :character   Median :2.000   Mode  :character  
# Mean   :2.008                     
# 3rd Qu.:2.477                     
# Max.   :3.000         
# 
# FAF              TUE             CALC              MTRANS         
# Min.   :0.0000   Min.   :0.0000   Length:2111        Length:2111       
# 1st Qu.:0.1245   1st Qu.:0.0000   Class :character   Class :character  
# Median :1.0000   Median :0.6253   Mode  :character   Mode  :character  
# Mean   :1.0103   Mean   :0.6579                                        
# 3rd Qu.:1.6667   3rd Qu.:1.0000                                        
# Max.   :3.0000   Max.   :2.0000   
# 
# NObeyesdad       
# Length:2111       
# Class :character  
# Mode  :character  


library(ggplot2)
obesity_data[obesity_data$Gender == "Male",]
# Checking to see what the distribution between male and female entries 
Freq_male <- nrow(obesity_data[obesity_data$Gender == "Male",])
Freq_female <- nrow(obesity_data[obesity_data$Gender == "Female",])
Frequency_by_Gender <- data.frame(Sex = c("Male","Female"),
                                  Freq = c(Freq_male, Freq_female))

#Plot of the age frequency                                 
Age_Fre <- ggplot(Frequency_by_Gender, aes(x = Sex, y = Freq, fill=sex)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = Freq), vjust = -0.4) + 
  xlab(" Sex ") + 
  ylab(" Count ") +
  labs(title = "Counting the distribution of Male and Female")

Age_Fre 

# Distribution of Age
ggplot(obesity_data, aes(x = Age, fill = family_history_with_overweight)) +
  stat_bin(binwidth=1,alpha=0.75)+  
  xlab(" Age (Years) ") + 
  ylab(" Count ") +
  labs(title = "Age Distribution")

# Distribution of Age with category of overweight
ggplot(obesity_data, aes(x = Age, fill = NObeyesdad)) +
  stat_bin(binwidth=1,alpha=0.75)+  
  xlab(" Age (Years) ") + 
  ylab(" Count ") +
  labs(title = "Age Distribution Filled by Weight Category")

# Distribution of Category
ggplot(obesity_data, aes(x = NObeyesdad,fill= MTRANS, label=..count..)) +
  geom_histogram(stat="count")+
  xlab(" Category ") + 
  ylab(" Count ") +
  labs(title = "Distribution of Weight Category Including Means Of Transportation")

# Distribution of weight
ggplot(obesity_data, aes(x = Weight, fill = NObeyesdad)) +
  geom_histogram(binwidth = 3)+
  xlab(" Weight ") + 
  ylab(" Count ") +
  labs(title = "Weight Distribution (Binsize = 3")



#Scatter plot of Height and Weight
scatter_height_weight <- ggplot(obesity_data, 
                                      aes(x = Height, Weight)) +
  geom_point(aes(color = Gender), size = 3, alpha = 0.5) +
  xlab(" Height (meters) ") + 
  ylab(" Weight kg ") +
  ggtitle("Scatter Plot of Height and Weight")

scatter_height_weight

#Scatter plot of Height and Weight with Category of Weightness
scatter_height_weight <- ggplot(obesity_data, 
                                aes(x = Height, Weight)) +
  geom_point(aes(color = NObeyesdad), size = 3, alpha = 0.5) +
  xlab(" Height (meters) ") + 
  ylab(" Weight kg ") +
  ggtitle("Scatter Plot of Height and Weight")

scatter_height_weight


# Using a clustering Algorithm this requires chaning the categorical data
# Logestic Regression
# Decision Tree

# To numeric values. The categories will be changed accordingly 

catergorical_data_to_numeric <- data.frame(data.matrix(obesity_data[1:16]))

# Need to normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

catergorical_data_to_numeric[1:16] <- as.data.frame(
  lapply(catergorical_data_to_numeric[1:16],normalize))
# Our new data
# What we have done so far
#   - We took our categorical data and changed it to numeric
#   - We have normalized our data 

new_obesity_data <- cbind(catergorical_data_to_numeric, obesity_data$NObeyesdad)

#### Kmeans Cluster: Predicting Gender based on Height and Weight #### 

# Becuase of the two scatter plots it looks like we can be able to indetify 
# someones Sex based off their height and weight this will require using two different 
# clusters. THe reason more isn't added is because we are only looking for Male and Female

set.seed(123)

obesity_gender_cluster <- kmeans(new_obesity_data[, c(3:4)], center=2, nstart=20)
obesity_gender_cluster

# K-means clustering with 2 clusters of sizes 1043, 1068

# Cluster means:
#   Gender    Height    Weight
# 1      0 0.3647128 0.3231520
# 2      1 0.5824346 0.3863394
# 
# Clustering vector:
#   [1] 1 1 2 2 2 2 1 2 2 2 2 1 2 2 2 1 2 1 1 1 2 1 1 1 2 2 2 1 2 2 2 1 1 2 2 1 1 1 2 1 1 2 1 2 2 1 2 2 1 1 1 1 1 1 1 2 2 2 1 2 1 2 2 2 1 1 2 2 2
# [70] 2 1 1 1 2 2 1 1 1 1 2 1 2 2 1 2 2 2 2 1 1 1 1 2 1 1 2 1 1 1 2 1 2 1 1 1 1 1 2 2 1 2 1 1 1 1 1 1 1 2 1 1 2 2 2 2 2 2 1 2 2 1 2 1 1 2 2 2 2
# [139] 2 2 2 2 2 1 2 2 1 1 1 1 2 1 1 2 2 1 1 2 1 2 2 2 1 2 2 2 1 2 1 1 2 2 1 2 2 1 1 2 2 2 1 1 1 1 2 1 2 2 2 1 2 2 2 2 1 2 2 2 1 1 1 1 1 2 2 1 2
# [208] 2 1 1 2 1 1 2 2 2 1 2 1 1 1 2 1 1 1 2 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 2 1 2 1 1 1 1 2 2 1 2 2 1 1 2 2 2 2 1 2 1 2 2 2 2 1 2 2 2 2 1 2 2 1 1
# [277] 2 2 1 2 2 2 1 2 2 2 1 1 1 2 2 2 2 2 2 1 1 2 2 2 2 2 2 1 2 2 1 1 2 1 2 2 1 2 1 2 2 2 1 1 2 2 1 2 1 1 2 2 2 1 1 2 2 1 1 2 2 2 1 1 2 2 2 2 2
# [346] 2 1 2 2 2 2 1 1 1 1 1 2 1 2 1 2 1 1 2 2 2 2 1 1 2 2 2 1 1 2 2 1 2 2 2 2 2 1 2 1 1 1 1 2 1 2 1 2 2 1 1 1 2 2 2 2 2 2 1 2 2 2 1 2 1 2 1 2 2
# [415] 2 2 2 2 2 2 2 2 2 2 2 1 2 1 2 2 2 1 2 1 2 1 1 1 1 1 1 1 2 2 2 1 2 2 1 1 2 2 2 1 1 2 1 1 2 2 1 2 2 2 2 1 2 2 1 1 1 1 1 1 2 1 1 1 2 1 1 2 1
# [484] 1 1 2 2 2 2 1 2 2 2 2 1 2 2 2 1 1 1 1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [553] 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 1 1 1 1 1 1 1 2 1 1 1
# [622] 1 1 1 2 2 2 1 2 1 1 1 1 2 1 1 1 2 2 2 1 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 1 2 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 2 1
# [691] 2 2 2 2 2 2 2 1 1 1 2 2 2 1 2 1 1 1 1 1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 2 2 1 2 1 1 2
# [760] 1 1 1 2 2 2 1 2 2 2 2 2 1 2 1 1 2 2 1 2 2 2 1 1 2 1 1 1 1 2 1 2 1 1 1 1 2 1 2 2 1 1 1 1 2 2 2 2 2 2 1 1 2 2 1 2 2 2 2 2 2 1 1 1 1 2 1 1 1
# [829] 1 1 2 2 2 2 2 1 1 2 2 2 2 2 2 2 1 1 1 1 2 2 2 1 1 1 2 2 1 2 2 2 2 1 1 1 2 1 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 1 2 1 1 1 2 2 1 1 1 1 1 2 2 2 2
# [898] 2 2 2 1 2 1 1 2 2 2 1 2 2 1 1 1 2 2 1 1 1 1 1 2 2 2 2 1 1 1 2 2 2 1 1 2 2 2 1 1 1 1 2 2 2 1 2 2 2 2 1 1 2 2 1 1 1 1 1 2 1 2 1 1 2 1 1 1 1
# [967] 2 1 1 2 2 2 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 2 2 1 1 2 1 1 2 2 1 1 2 2
# [ reached getOption("max.print") -- omitted 1111 entries ]
# 
# Within cluster sum of squares by cluster:
#   [1] 71.86468 47.01819
# (between_SS / total_SS =  82.4 %)
# 
# Available components:
#   
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault" 
# 


# Results # 
table <- table(obesity_data$Gender, obesity_gender_cluster$cluster)
# 1   2
# Female 729 314
# Male   433 635
#Clustering Accuracy 
sum(diag(table))/sum(table)
# 0.6461393

library(cluster)

clusplot(obesity_data, obesity_gender_cluster$cluster, color=T, shade=T, labels=0, lines=0)

#### Kmeans Clustering: Weight Category using Height and Weight ####

# The next model that will be built will be identifying which category someone falls 
# into based off their height and weight. This will require only 7 different clusters. The 
# reason for  this is because we are looking for the category of Obesity. 

set.seed(123)

obesity_gender_cluster <- kmeans(new_obesity_data[,(3:4)], center=7, nstart=20)
obesity_gender_cluster

# K-means clustering with 7 clusters of sizes 318, 135, 351, 434, 360, 232, 281
# 
# Cluster means:
#   Height    Weight
# 1 0.6529166 0.3109620
# 2 0.5764381 0.7342344
# 3 0.2185448 0.1449098
# 4 0.4111515 0.3000301
# 5 0.6716680 0.5621894
# 6 0.4896341 0.1158350
# 7 0.3788060 0.5028901
# 
# Clustering vector:
#   [1] 3 3 1 1 1 3 3 6 1 6 5 4 6 5 6 6 5 3 4 4 4 4 6 4 1 3 6 3 1 4 1 3 1 1 6 3 3 3 4 1 4 6 4 6 1 3 1 1 6 3 3 3 6 3 3 6 4 4 4 6 3 6 6 6 6 4 1 7 5
# [70] 1 4 6 6 1 4 3 3 4 4 6 3 1 4 3 4 6 1 1 4 4 7 3 1 3 3 4 6 3 3 6 6 6 3 6 3 3 3 1 1 4 1 3 3 3 3 6 4 4 3 4 4 1 6 4 1 1 4 6 6 4 3 3 6 4 5 4 1 4
# [139] 1 6 1 1 1 4 3 4 3 4 3 4 1 3 3 4 1 6 3 6 3 4 1 4 3 1 1 5 1 4 4 4 4 3 6 6 4 6 3 1 1 4 3 4 6 3 4 4 1 5 5 3 3 1 1 1 4 1 1 5 3 6 4 3 7 4 1 4 5
# [208] 1 6 6 5 3 3 1 6 4 3 4 3 3 1 4 6 3 4 5 3 3 4 5 1 4 3 4 3 4 6 6 3 6 4 3 3 5 3 6 3 3 4 1 4 4 1 3 6 1 1 5 4 4 4 4 6 6 1 6 6 4 6 6 5 3 6 4 3 3
# [277] 6 1 3 6 6 6 3 6 1 5 3 3 3 1 1 1 6 1 5 6 3 1 1 4 4 6 6 3 1 5 3 3 1 6 6 6 3 6 4 4 1 1 3 3 4 1 6 1 4 3 6 4 1 4 6 6 1 6 3 6 6 1 3 3 1 3 5 4 2
# [346] 6 3 1 1 5 1 6 3 3 3 3 1 6 5 3 1 3 4 1 1 6 4 3 4 3 1 4 3 3 1 1 3 1 6 4 4 1 3 7 3 3 3 3 5 3 6 6 1 4 3 3 3 6 5 6 4 1 5 7 1 1 1 3 1 3 1 3 1 1
# [415] 5 4 1 1 4 4 1 1 6 1 1 3 1 6 4 1 4 3 1 6 1 1 3 3 6 3 3 3 6 6 6 3 1 1 7 3 4 6 1 3 4 1 3 3 6 1 3 7 4 1 1 3 1 1 3 3 3 4 3 3 1 4 4 6 6 3 3 6 3
# [484] 3 4 1 1 5 1 3 1 4 4 1 4 1 6 3 7 2 7 2 2 2 7 7 2 6 6 6 6 6 6 3 3 3 3 3 3 6 3 3 3 3 3 3 3 3 6 6 6 1 1 1 6 6 6 3 3 3 6 6 6 6 6 6 6 6 6 3 6 6
# [553] 6 6 6 6 6 6 1 6 6 6 3 6 1 1 6 6 6 6 6 6 6 6 6 6 3 3 3 1 1 1 6 6 6 6 6 6 3 3 3 6 1 1 1 1 1 6 6 6 3 3 3 3 3 3 6 6 6 6 6 3 3 3 3 3 6 1 6 3 6
# [622] 6 6 6 6 6 1 6 6 6 6 6 3 1 6 6 3 1 1 6 3 3 6 6 6 6 6 6 6 3 3 3 3 3 3 6 3 3 3 3 3 3 3 3 6 6 6 1 1 6 6 6 6 3 3 3 6 6 6 6 6 6 6 6 6 6 3 6 6 6
# [691] 6 6 6 6 1 6 6 3 6 6 1 1 6 6 6 6 6 6 6 6 6 6 3 3 3 1 1 1 6 6 6 6 6 6 3 3 3 6 1 1 1 1 1 6 6 6 3 3 3 3 3 3 6 6 6 1 4 1 4 4 1 4 1 4 1 4 4 3 3
# [760] 3 3 4 4 4 4 4 1 1 1 4 3 4 1 3 1 1 4 4 1 4 1 3 4 4 3 3 3 4 1 3 4 3 4 3 3 1 4 1 1 1 4 1 3 1 1 4 4 1 4 3 3 1 1 1 1 1 4 4 1 4 4 4 3 3 4 3 3 3
# [829] 4 1 4 4 4 4 4 4 4 1 1 1 1 1 1 4 3 3 1 1 1 1 3 1 1 1 4 4 4 1 1 4 1 3 4 4 4 3 3 3 3 3 4 1 1 3 3 4 3 4 4 3 3 3 1 4 1 1 1 1 1 1 4 4 3 1 1 4 4
# [898] 1 4 4 4 1 1 1 1 1 4 1 4 4 4 3 4 3 3 3 3 3 4 4 4 4 4 4 4 4 1 1 1 4 3 3 4 4 1 3 3 1 1 1 4 4 4 1 1 4 1 3 4 4 4 3 3 3 3 4 1 3 4 3 3 4 3 3 3 3
# [967] 1 4 4 1 1 1 1 4 4 4 3 1 1 4 4 5 1 4 4 4 3 1 1 3 3 1 3 3 1 1 4 4 4 4
# [ reached getOption("max.print") -- omitted 1111 entries ]
# 
# Within cluster sum of squares by cluster:
#   [1] 4.313729 1.210826 4.707155 3.194446 4.244057 1.919691 1.823456
# (between_SS / total_SS =  85.3 %)
# 
# Available components:
#   
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault"  


# Results # 
table <- table(obesity_data$NObeyesdad, obesity_gender_cluster$cluster)
table
# 1   2   3   4   5   6   7
# Insufficient_Weight  37   0  82   0   0 153   0
# Normal_Weight        74   0 104  30   0  79   0
# Obesity_Type_I       23   0  50 138 110   0  30
# Obesity_Type_II       0   0   0   0 235   0  62
# Obesity_Type_III      0 135   0   0   0   0 189
# Overweight_Level_I  100   0  75 115   0   0   0
# Overweight_Level_II  84   0  40 151  15   0   0

#Clustering Accuracy 
sum(diag(table))/sum(table)
# 0.0412127
sn
library(cluster)

clusplot(obesity_data, obesity_gender_cluster$cluster, color=T, shade=T, labels=0, lines=0)



# Elbow Plot For Both 
tot.withinss <- vector(mode="character", length=10)
for (i in 1:10){
  obesity_ <- kmeans(new_obesity_data[,3:4], center=i, nstart=20)
  tot.withinss[i] <- obesity_$tot.withinss
}

plot(1:10, tot.withinss, type="b", pch=19)
title("Elbow Plot For Kmeans Model")

#### Random Forest Model ####

library(caret)
library(randomForest)


Train <- createDataPartition(obesity_data$NObeyesdad, p = 0.7, list = FALSE)

training <- obesity_data[ Train, ]
testing <- obesity_data[ - Train, ]

set.seed(123)

random_forest <- randomForest(as.factor(NObeyesdad)~., 
                              data = training, 
                              ntree = 50,
                              importance =TRUE,
                              na.action=na.exclude)
plot(random_forest)

pred_w_random <- predict(random_forest, testing, type = "class")
tableCheck <- table(pred_w_random, testing$NObeyesdad)
sum(diag(tableCheck))/sum(tableCheck)
#0.9303797



