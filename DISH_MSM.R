
# Applying Multiple Source Method (MSM) to DISH
# https://msm.dife.de/static/MSM_UserGuide.pdf
# Author: Lindsay Jaacks
# Date: 2024-09-27




# Prepare R Environment ####

## Clear R environment ####
rm(list = ls())

## Libraries ####
library(e1071)
library(dplyr)

## Set working directory ####
#setwd("C:/Users/ljaacks/OneDrive - University of Edinburgh/Scottish Diets/_National Monitoring of Diet Scotland/3_DISH/Analysis")

## MSM code ####
# Author: Sven Knuppel, Sven.Knueppel@bfr.bund.de
# Date: 2022-07-08
source("/Users/ricki/Desktop/DISH/Code/DISH_MSM.R")

## Read in day level dataset with one row for each recall ####
df.intake24 <- read.csv("Data/intake24_recall.csv") 



# Prepare dataset for MSM #### 

# Set Males=1 and Females=2
df.intake24$Sex = ifelse(df.intake24$Sex == "Male", 1, 2)

# Create reporting groups 
df.intake24$Reportinggroup[df.intake24$age_sex_cat == "Female, 2-4y"] = 1 
df.intake24$Reportinggroup[df.intake24$age_sex_cat == "Female, 5-10y"] = 2 
df.intake24$Reportinggroup[df.intake24$age_sex_cat == "Female, 11-15y"] = 3
df.intake24$Reportinggroup[df.intake24$age_sex_cat == "Male, 2-4y"] = 4 
df.intake24$Reportinggroup[df.intake24$age_sex_cat == "Male, 5-10y"] = 5 
df.intake24$Reportinggroup[df.intake24$age_sex_cat == "Male, 11-15y"] = 6

# Order by Reportinggroup 
df.intake24 <- df.intake24[order(df.intake24$Reportinggroup), ] 

# Drop 'Prefer not to say' Sex
df.intake24 <- df.intake24 %>%
  filter(!is.na(age_sex_cat))


# Analysis for all nutrients and food groups consumed by >15% of participants ####

list.variables = list("Day_Energykcal", "Day_Carbohydrateg", "Day_FreeSugarsg", "Day_Fatg", "Day_SatFatg", "Day_TransFatg", "Day_Proteing", "Day_AOACFibreg", 
                      "Day_VitaminAug", "Day_Riboflavinmg", "Day_Folateug", "Day_VitaminDug", "Day_VitaminB12ug", "Day_VitaminCmg", "Day_Ironmg", "Day_Calciummg",
                      "Day_Sodiummg", "Day_Magnesiummg", "Day_Potassiummg", "Day_Iodineug", "Day_Seleniumug", "Day_Zincmg",
                      "Day_ndnsg_1","Day_ndnsg_2","Day_ndnsg_3","Day_ndnsg_5","Day_ndnsg_6","Day_ndnsg_7","Day_ndnsg_8","Day_ndnsg_9",
                      "Day_ndnsg_10","Day_ndnsg_11","Day_ndnsg_13","Day_ndnsg_14","Day_ndnsg_15","Day_ndnsg_16","Day_ndnsg_17","Day_ndnsg_21",
                      "Day_ndnsg_23","Day_ndnsg_26","Day_ndnsg_27","Day_ndnsg_30","Day_ndnsg_31","Day_ndnsg_33","Day_ndnsg_36","Day_ndnsg_37",
                      "Day_ndnsg_38","Day_ndnsg_39","Day_ndnsg_40","Day_ndnsg_41","Day_ndnsg_42","Day_ndnsg_43","Day_ndnsg_44","Day_ndnsg_45",
                      "Day_ndnsg_50","Day_ndnsg_53","Day_ndnsg_57","Day_ndnsg_58","Day_ndnsg_62")

for( i in list.variables)
{
  print(i)
  
  ##  run models for each nutrient
  MSM_call_by_group(
    data_set = df.intake24, 
    bygroup = "Reportinggroup",
    output = "MSM",
    id = "UserID",
    intake = i, 
    model = 1,  # no covariates
    P_cons = NULL  # assuming all individuals are habitual consumers
  )
  
  MSM_output = rbind(MSM_Reportinggroup_1_MSM_,
                     MSM_Reportinggroup_2_MSM_,
                     MSM_Reportinggroup_3_MSM_,
                     MSM_Reportinggroup_4_MSM_,
                     MSM_Reportinggroup_5_MSM_,
                     MSM_Reportinggroup_6_MSM_)
  
  # keep UserID and estimated usual intake
  MSM_output_1 = cbind(MSM_output[,1],MSM_output[,15])
  
  # re-name variables
  colnames(MSM_output_1) = c("UserID",paste0(i,"UI_"))
  
  # with each nutrient loop, merge usual intake variables
  if(i=="Day_Energykcal")   # FIRST NAMED VARIABLE IN THE LIST
  {
    MSM_TABLEnut = MSM_output_1
  }
  else
  {
    MSM_TABLEnut = merge(MSM_TABLEnut,MSM_output_1,by="UserID")
  }
} 

## merge back to participant-level data frame ####
df.intake24_ui = as.data.frame(MSM_TABLEnut)
df.intake24_participant <- read.csv("Data/intake24_participant.csv") 
df.intake24 <- left_join(df.intake24_participant, df.intake24_ui, by="UserID")
write.csv(df.intake24_participant, file = ("Data/intake24_participant.csv"))
