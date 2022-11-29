setwd("C:/Users/Naran/DataAnalytics2022_Alejandro_Naranjo/UCI_Obesity_And_Diabetes/Diabetes datasets")

diabetic_data <- read.csv("diabetic_data.csv")


library(ggplot2)
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

scatter_admission_discharge <- ggplot(diabetic_data, 
                                      aes(x = admission_type_id, discharge_disposition_id)) +
  geom_point(aes(color = gender, shape=gender), size = 3) +
  xlab("Admission Type") + 
  ylab("discharge_diposition") +
  ggtitle("Admission Type Id vs Discharge Diposition Id")

scatter_admission_discharge


#Distribution of Age
age_distribution <- ggplot(diabetic_data, aes(x = age, fill = race,label=..count..)) + 
  stat_count(binwidth=1)+  
  stat_count(binwidth=1)+
  xlab("Age Range") +
  ylab("Count") +
  ggtitle("Age Distribution Inclduing Race")

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

# Now we need to tranform the data to all numeric values
# Female -> 1, Male -> 2
# [0-10) -> 1 ... [90,100) -> 10
# A1CResult: >7 -> 1, >8 -> 2, None -> 3, Norm -> 4
# max_glu_serum: >200 -> 1 , >300 -> 2 , normal -> 4, none -> 3
# Change: No -> 2, Ch -> 1
# diabetesMed: Yes -> 2, No -> 1 
# Readmitted: >30 -> 2, <30 -> 1, NO->3 

linear_regression_data <- data.frame(data.matrix(linear_regression_data))

# Using a Linear Regression Model 
lm.fit <- lm(readmitted~., linear_regression_data)

summary(lm.fit)

# Retraining and removing A1CResult as it isn't significant
linear_regression_data_remove_A1CResult <- linear_regression_data[-c(15)]
lm.fit2 <- lm(readmitted~., linear_regression_data_remove_A1CResult)

summary(lm.fit2)
