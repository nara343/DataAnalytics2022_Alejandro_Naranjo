setwd("C:/Users/Naran/DataAnalytics2022_Alejandro_Naranjo/UCI_Obesity_And_Diabetes/Diabetes datasets")

diabetic_data <- read.csv("diabetic_data.csv")


library(ggplot2)
library(ggcorrplot)
#Scatter plot between admission and discharge
colnames(diabetic_data)
# "encounter_id"             "patient_nbr"              "race"                    
# [4] "gender"                   "age"                      "weight"                  
# [7] "admission_type_id"        "discharge_disposition_id" "admission_source_id"     
# [10] "time_in_hospital"         "payer_code"               "medical_specialty"       
# [13] "num_lab_procedures"       "num_procedures"           "num_medications"         
# [16] "number_outpatient"        "number_emergency"         "number_inpatient"        
# [19] "diag_1"                   "diag_2"                   "diag_3"                  
# [22] "number_diagnoses"         "max_glu_serum"            "A1Cresult"               
# [25] "metformin"                "repaglinide"              "nateglinide"             
# [28] "chlorpropamide"           "glimepiride"              "acetohexamide"           
# [31] "glipizide"                "glyburide"                "tolbutamide"             
# [34] "pioglitazone"             "rosiglitazone"            "acarbose"                
# [37] "miglitol"                 "troglitazone"             "tolazamide"              
# [40] "examide"                  "citoglipton"              "insulin"                 
# [43] "glyburide.metformin"      "glipizide.metformin"      "glimepiride.pioglitazone"
# [46] "metformin.rosiglitazone"  "metformin.pioglitazone"   "change"                  
# [49] "diabetesMed"              "readmitted"            

# Corelation Matrix only numeric values 
numeric_names <- !(unlist(lapply(diabetic_data, is.character), use.names = FALSE))
numeric_df <- diabetic_data[, numeric_names]
cor_matrix <- round(cor(numeric_df),3)

ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab =TRUE) + 
  ggtitle("Correlation Matrix Only For Numeric Features")

#Scatter plot of admissions
scatter_admission_discharge <- ggplot(diabetic_data, 
                                      aes(x = admission_type_id, discharge_disposition_id)) +
  geom_point(aes(color = gender, shape=gender), size = 3) +
  xlab("Admission Type") + 
  ylab("discharge_diposition") +
  ggtitle("Admission Type Id vs Discharge Diposition Id")

scatter_admission_discharge


#Distribution of Age
age_distribution <- ggplot(diabetic_data, aes(x = age, fill = readmitted,label=..count..)) + 
  stat_count(binwidth=1)+  
  stat_count(binwidth=1)+
  xlab("Age Range") +
  ylab("Count") +
  ggtitle("Age Distribution Inclduing Readmission")

age_distribution


#Distribution of the admission code
admission_distribution <-ggplot(diabetic_data, aes(x = admission_type_id, fill = gender,label=..count..)) + 
  stat_count(binwidth=1)+  
  stat_count(binwidth=1, geom="text", aes(label=..count..), vjust=-1)+
  xlab("Admission") +
  ylab("Count") +
  ggtitle("Admission Distribution by Gender")

admission_distribution


#Scatter Plot of Age and time spent in the hospital

new_data <- diabetic_data
unique(new_data$age)

# [1] "[0-10)"   "[10-20)"  "[20-30)"  "[30-40)"  "[40-50)"  "[50-60)"  "[60-70)" 
# [8] "[70-80)"  "[80-90)"  "[90-100

new_data$age[new_data$age == "[0-10)"] <- 1
new_data$age[new_data$age == "[10-20)"] <- 2
new_data$age[new_data$age == "[20-30)"] <- 3
new_data$age[new_data$age == "[30-40)"] <- 4
new_data$age[new_data$age == "[40-50)"] <- 5
new_data$age[new_data$age == "[50-60)"] <- 6
new_data$age[new_data$age == "[60-70)"] <- 7
new_data$age[new_data$age == "[70-80)"] <- 8
new_data$age[new_data$age == "[80-90)"] <- 9
new_data$age[new_data$age == "[90-100)"] <- 10

new_data$age = as.numeric(as.character(new_data$age))

scatter_age_time_spent <- ggplot(new_data, 
                                      aes(x = age, y = time_in_hospital)) +
  geom_point(aes(color = age), size = 3) +
  xlab("Age ") + 
  ylab("Time Spent") +
  ggtitle("Age vs Time Spent")

scatter_age_time_spent


#Distribution of Who is taking medication
admission_taking_diabetes_med <-ggplot(diabetic_data, aes(x = admission_type_id, fill = diabetesMed,label=..count..)) + 
  stat_count(binwidth=1)+  
  stat_count(binwidth=1, geom="text", aes(label=..count..), vjust=-1)+
  xlab("Admission ID") +
  ylab("Count") +
  ggtitle("Admission Distribution by Diabetes Med")

admission_taking_diabetes_med

#Distribution Admission and insulin levels
admission_distrubition_w_insulin_level <-ggplot(diabetic_data, aes(x = admission_type_id, fill = insulin,label=..count..)) + 
  stat_count(binwidth=1)+  
  stat_count(binwidth=1, geom="text", aes(label=..count..), vjust=1)+
  xlab("Admission ID") +
  ylab("Count") +
  ggtitle("Admission Type Distribution With Insulin Level")

admission_distrubition_w_insulin_level

#Distribution of A1Cresult including information about if re-admited

unique(diabetic_data$A1Cresult)
unique(diabetic_data$readmitted)

ALCResult_Distribution <- ggplot(diabetic_data, aes(x = A1Cresult, fill = readmitted, label = ..count..)) +
  stat_count(binwidth = 1) +
  stat_count(binwidth=1, geom="text", aes(label=..count..), vjust=1, size = 2.75)+
  xlab("A1Cresult") +
  ylab("Count") +
  ggtitle("A1Cresult Distribution Filled by Readmitted Results")

ALCResult_Distribution

#### Model building: Linear Regression ####

# For this Model we are only going to be focusing on certain features
# We will be predicting if a patient is re-admitted after having gone to the hopital
# We will be removing information about their medicine and the type of diagnosis 
# Race will also be dropped there is a dominant number of Caucasian patients and 
# might affect the results 

#Cleaning the data

remove_nas <- function(df, Null) {
  totalRows <- nrow(df)
  print(totalRows)
  for ( i in colnames(df) ){
    found = nrow(df[df[[i]] == Null, ])
    cat(i, "\t",found,"\n")
  }
  return(info)
}

find_nas <- remove_nas(diabetic_data, "?")
#Values which stand out

# weight 	 98569 
# payer_code 	 40256 
# medical_specialty 	 49949 
 
linear_regression_data <- diabetic_data[-c(1:3,6,11:12, 19:21, 25:47 )]
colnames(linear_regression_data)
logistic_regression_data <- diabetic_data[-c(1:3,6,11:12, 19:21, 25:47 )]

# Now we need to tranform the data to all numeric values
# Female -> 1, Male -> 2
# [0-10) -> 1 ... [90,100) -> 10
# A1CResult: >7 -> 1, >8 -> 2, None -> 3, Norm -> 4
# max_glu_serum: >200 -> 1 , >300 -> 2 , normal -> 4, none -> 3
# Change: No -> 2, Ch -> 1
# diabetesMed: Yes -> 2, No -> 1 
# Readmitted: >30 -> 2, <30 -> 1, NO->3 

linear_regression_data <- data.frame(data.matrix(linear_regression_data))

# Checking the Correlation of the Variable now that we have made it all numeric


cor_matrix <- round(cor(linear_regression_data),3)
ggcorrplot(cor_matrix, type = "lower", hc.order = TRUE, lab = TRUE) + 
  ggtitle("Correlation Matrix After Transforming Data")

View(cor_matrix)
# Using a Linear Regression Model 
lm.fit <- lm(readmitted~., linear_regression_data)

summary(lm.fit)

# Call:
#   lm(formula = readmitted ~ ., data = linear_regression_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max
# -1.7490 -0.4601  0.3717  0.5244  2.4964
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)               2.879e+00  3.156e-02  91.220  < 2e-16 ***
#   gender                    9.486e-03  4.172e-03   2.274 0.022982 *
#   age                      -9.704e-03  1.366e-03  -7.105 1.21e-12 ***
#   admission_type_id        -5.317e-03  1.507e-03  -3.528 0.000418 ***
#   discharge_disposition_id  5.423e-05  4.019e-04   0.135 0.892659
# admission_source_id      -1.523e-03  5.252e-04  -2.899 0.003744 **
#   time_in_hospital         -5.003e-03  8.203e-04  -6.099 1.07e-09 ***
#   num_lab_procedures       -4.176e-04  1.151e-04  -3.629 0.000285 ***
#   num_procedures            1.349e-02  1.358e-03   9.940  < 2e-16 ***
#   num_medications          -6.630e-04  3.292e-04  -2.014 0.044017 *
#   number_outpatient        -1.806e-02  1.656e-03 -10.905  < 2e-16 ***
#   number_emergency         -2.928e-02  2.329e-03 -12.572  < 2e-16 ***
#   number_inpatient         -1.127e-01  1.727e-03 -65.273  < 2e-16 ***
#   number_diagnoses         -2.315e-02  1.176e-03 -19.681  < 2e-16 ***
#   max_glu_serum             7.872e-03  6.747e-03   1.167 0.243271
#   A1Cresult                -7.469e-06  4.049e-03  -0.002 0.998528
#   change                    7.564e-03  4.919e-03   1.538 0.124149
#   diabetesMed              -7.012e-02  5.737e-03 -12.222  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6601 on 101748 degrees of freedom
# Multiple R-squared:  0.06913,	Adjusted R-squared:  0.06898
# F-statistic: 444.5 on 17 and 101748 DF,  p-value: < 2.2e-16


# Retraining and removing the non significant features:
#   Discharge_diposition_id
#   A1CResult
#   Change
#   max_glu_serum
# I believed these would be statistically significant but I was wrong 

# Removing the data that isn't statistically significant 
linear_regression_data_remove_A1CResult <- linear_regression_data[-c(4, 14:16)]
lm.fit2 <- lm(readmitted~., linear_regression_data_remove_A1CResult)

summary(lm.fit2)

# Call:
#   lm(formula = readmitted ~ ., data = linear_regression_data_remove_A1CResult)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max
# -1.7506 -0.4597  0.3718  0.5245  2.4870
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)          2.9236195  0.0165018 177.170  < 2e-16 ***
#   gender               0.0093726  0.0041709   2.247 0.024634 *
#   age                 -0.0095916  0.0013565  -7.071 1.55e-12 ***
#   admission_type_id   -0.0054517  0.0014968  -3.642 0.000270 ***
#   admission_source_id -0.0015739  0.0005235  -3.006 0.002644 **
#   time_in_hospital    -0.0050393  0.0008135  -6.195 5.86e-10 ***
#   num_lab_procedures  -0.0004155  0.0001142  -3.637 0.000276 ***
#   num_procedures       0.0136725  0.0013529  10.106  < 2e-16 ***
#   num_medications     -0.0007487  0.0003241  -2.310 0.020877 *
#   number_outpatient   -0.0180946  0.0016561 -10.926  < 2e-16 ***
#   number_emergency    -0.0293865  0.0023278 -12.624  < 2e-16 ***
#   number_inpatient    -0.1126838  0.0017255 -65.306  < 2e-16 ***
#   number_diagnoses    -0.0231508  0.0011759 -19.688  < 2e-16 ***
#   diabetesMed         -0.0745495  0.0050338 -14.810  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6601 on 101752 degrees of freedom
# Multiple R-squared:  0.0691,	Adjusted R-squared:  0.06898
# F-statistic:   581 on 13 and 101752 DF,  p-value: < 2.2e-16

#### BUilding a linear Classification Model #### 
# With the logistic Regression we don't have to change the categorical information
# And we can leave it as is. 
# For this model we're focuing on two different types of readmissions, those 
# greater than 30 days and No re-admission at all
library(caret)
logistic_data <- linear_regression_data[linear_regression_data$readmitted != 1, ]
logistic_data$readmitted <- as.factor(logistic_data$readmitted)
Train <- createDataPartition(logistic_data$readmitted, p = 0.7, list = FALSE)

training <- logistic_data[ Train, ]
testing <- logistic_data[ - Train, ]

glm.fit <- glm(readmitted~., training,family=binomial(link = "logit"))
summary(glm.fit)

# Call:
#   glm(formula = readmitted ~ ., family = binomial, data = training)
# 
# Deviance Residuals:
#   Min       1Q   Median       3Q      Max
# -1.9988  -1.2770   0.8122   0.9651   3.7338
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)               1.7522808  0.1281664  13.672  < 2e-16 ***
#   gender                    0.0646384  0.0169741   3.808 0.000140 ***
#   age                      -0.0202088  0.0056154  -3.599 0.000320 ***
#   admission_type_id        -0.0321101  0.0061176  -5.249 1.53e-07 ***
#   discharge_disposition_id  0.0242230  0.0017527  13.821  < 2e-16 ***
#   admission_source_id      -0.0074656  0.0021230  -3.516 0.000437 ***
#   time_in_hospital         -0.0138596  0.0033162  -4.179 2.92e-05 ***
#   num_lab_procedures       -0.0017410  0.0004653  -3.742 0.000183 ***
#   num_procedures            0.0458554  0.0055539   8.257  < 2e-16 ***
#   num_medications           0.0004544  0.0013415   0.339 0.734805
# number_outpatient        -0.0823072  0.0073859 -11.144  < 2e-16 ***
#   number_emergency         -0.2026468  0.0152208 -13.314  < 2e-16 ***
#   number_inpatient         -0.3434939  0.0088250 -38.923  < 2e-16 ***
#   number_diagnoses         -0.0881494  0.0048598 -18.139  < 2e-16 ***
#   max_glu_serum             0.0171613  0.0271854   0.631 0.527864
# A1Cresult                 0.0084542  0.0162419   0.521 0.602702
# change                    0.0360218  0.0198937   1.811 0.070186 .
# diabetesMed              -0.2230905  0.0235512  -9.473  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 84822  on 63286  degrees of freedom
# Residual deviance: 80690  on 63269  degrees of freedom
# AIC: 80726
# 
# Number of Fisher Scoring iterations: 4


pred <- predict(glm.fit, newdata = testing, type="response")
pred.class <- ifelse(pred >0.5 , 3, 2)

accuracy_logistic_model <- table(pred.class, testing[,"readmitted"])
accuracy_logistic_model
sum(diag(accuracy_logistic_model))/sum(accuracy_logistic_model)

#ACcuracy of ~64%

#### Comparing To A Decision Tree ####
library(caret)
library(randomForest)
rn_data <- diabetic_data[-c(1:3,6,11:12, 19:21, 25:47 )]

Train <- createDataPartition(rn_data$readmitted, p = 0.5, list = FALSE)

training <- logistic_data[ Train, ]
testing <- logistic_data[ - Train, ]

set.seed(123)

random_forest <- randomForest(readmitted~., 
                              data = training, 
                              ntree = 50,
                              importance =TRUE,
                              na.action=na.exclude)

pred_w_random <- predict(random_forest, testing, type = "class")
tableCheck <- table(pred_w_random, testing$readmitted)
sum(diag(tableCheck))/sum(tableCheck)

plot(random_forest)
#Accuracy of 0.6439493


