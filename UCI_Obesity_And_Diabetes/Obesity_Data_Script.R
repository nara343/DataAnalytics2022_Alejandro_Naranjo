# Load Obesity data #
setwd("C:/Users/Naran/DataAnalytics2022_Alejandro_Naranjo/UCI_Obesity_And_Diabetes/Obesity datasets")
obesity_data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
View(obesity_data)

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
Age_Fre <- ggplot(Frequency_by_Gender, aes(x = Sex, y = Freq, fill=Sex)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = Freq), vjust = -0.4) + 
  xlab(" Sex ") + 
  ylab(" Count ") +
  labs(title = "Counting the distribution of Male and Female")

Age_Fre 

# Distribution of Age
ggplot(obesity_data, aes(x = Age)) +
  stat_bin(binwidth=1)+  
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.5)+
  xlab(" Age (Years) ") + 
  ylab(" Count ") +
  labs(title = "Age Distribution")

# Distribution of Category
ggplot(obesity_data, aes(x = NObeyesdad,fill= NObeyesdad, label=..count..)) +
  geom_histogram(stat="count")+
  xlab(" Category ") + 
  ylab(" Count ") +
  labs(title = "Destibution of Dependent Feature (NOBeyesdad)")

# Distribution of weight
ggplot(obesity_data, aes(x = Weight)) +
  geom_histogram(binwidth = 3)+
  xlab(" Weight ") + 
  ylab(" Count ") +
  labs(title = "Weight Distribution (Binsize = 3")

# Scatter plot of Age and Weight

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


