#****************************************************************************************
#
# PROJECT: 20181002
#
# MODULE: 010 - SOURCE - Import and data ETL
#
# DESCRIPTION:

#
#              
#         
# STEPS
# 1.Set libraries and import data
# 2. Data cleaning    
# 3. Partition data to test and train datasets
#
#****************************************************************************************


## 1. Set libraries and import data ####


library(data.table)
library(readr)
library(dplyr)
library(caret)
library(dummies)





ou_data<-read_csv("C:\\Users\\ACAG077\\Desktop\\R learning\\Solving Business Problems\\anonymisedData\\ou_dataset.csv")

seed=1270
## 2. Data cleaning ####
# Remove duplicate rows
unique_ou_data<-unique(ou_data) # no duplicate rows found





drops <- c("id_student","Sum_weighted_score")
unique_ou_data<-unique_ou_data[ , !(names(unique_ou_data) %in% drops)]


factor_cols <- c("code_module","code_presentation","gender","region","highest_education",
                 "imd_band","age_band","disability","final_result","trimmed_assessment_type")

ou_data_DT <- data.table(unique_ou_data)

ads1<-ou_data_DT[,(factor_cols):= lapply(.SD, as.factor), .SDcols = factor_cols]

#re-define dependent variable
ads1$success<-NA
ads1$success[ads1$final_result == 'Pass' | ads1$final_result == 'Distinction'] <- 'Y'
ads1$success[ads1$final_result == 'Withdrawn' | ads1$final_result == 'Fail'] <- 'N'

#remove final_result
ads1<-ads1[, !c("final_result"), with=FALSE]
ads1$success<-as.factor(ads1$success)

## 3. partition data to test and train datasets ####
set.seed(seed)
trainIndex <- createDataPartition(ads1$final_result, p = .8, 
                                  list = FALSE, 
                                  times = 1)



train = ads1[trainIndex,]
test = ads1[-trainIndex,]

saveRDS(train, "./train.rds")
saveRDS(test, "./test.rds")




