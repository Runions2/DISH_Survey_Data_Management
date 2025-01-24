
# DISH Intake24 Processing
# Date: 2024-10-11


# Prepare R Environment ####


#clear R environment
rm(list = ls())

#load required packages 
library(dplyr)
library(tidyr)
library(rio)
library(ggplot2)
library(ggthemes)
library(grafify)
library(lubridate)
library(stringr)
library(readxl)
library(flextable)
library(gtsummary)
library(writexl)
library(forcats)
library(openxlsx)
library(e1071)



#set working directory
#setwd("[@update directory path here]")



# Import the data ####
df.intake24_item <- read.csv("Data/intake24_item.csv")



# Recall Level Values ####

## Total daily nutrient and food group intake (across items) ####
df.intake24_recall <- df.intake24_item %>% 
  group_by(UserID, RecallNo) %>%
  summarise(
    HouseholdID = first(HouseholdID),
    psu = first(psu),
    Device_group = first(Device_group),
    Month = first(Month),
    DayofWeek = first(DayofWeek),
    Age = first(Age),
    age_cat = first(age_cat),
    Sex = first(Sex),
    age_sex_cat = first(age_sex_cat),
    age_cat_drv = first(age_cat_drv),
    age_sex_cat_drv = first(age_sex_cat_drv),
    Ethnicity = first(Ethnicity),
    simd_quintile = first(simd_quintile),
    Education = first(Education),
    adults_cat = first(adults_cat),
    children_cat = first(children_cat),
    AFreqOilyFish = first(AFreqOilyFish),
    AFreqToddlerMilk = first(AFreqToddlerMilk),
    ChildCompletedFoodDiary = first(ChildCompletedFoodDiary),
    HHFOOD1 = first(HHFOOD1),
    HHFOOD2 = first(HHFOOD2),
    HHFOOD3 = first(HHFOOD3),
    HHFOOD4 = first(HHFOOD4),
    NumberOfRecalls = first(NumberOfRecalls), 
    Day_Energykcal = sum(Energykcal),
    Day_FoodEnergy = sum(FoodEnergy),
    Day_FoodMilkEnergy = sum(FoodMilkEnergy),
    Day_TotalGrams = sum(TotalGrams),
    Day_FoodGrams = sum(FoodGrams),
    Day_FoodMilkGrams = sum(FoodMilkGrams),
    Day_Carbohydrateg = sum(Carbohydrateg),
    Day_Carbs_kcal=sum(Carbs_kcal),
    Day_FreeSugarsg = sum(Freesugarsg), 
    Day_FreeSugars_kcal=sum(Freesugars_kcal),
    Day_Fatg = sum(Fatg),
    Day_Fat_kcal=sum(Fat_kcal),
    Day_SatFatg = sum(Saturatedfattyacidsg),
    Day_SatFat_kcal=sum(SatFat_Kcal),
    Day_TransFatg = sum(Transfattyacidsg),
    Day_TransFat_kcal=sum(TransFat_Kcal),
    Day_Proteing = sum(Proteing),
    Day_Protein_kcal = sum(Protein_kcal),
    Day_Alcoholg = sum(Alcoholg),
    Day_Alcohol_kcal = sum(Alcohol_kcal),
    Day_AOACFibreg = sum(AOACg),
    Day_VitaminAug = sum(VitaminAug),
    Day_Riboflavinmg = sum(Riboflavinmg),
    Day_Folateug = sum(Folateug),
    Day_VitaminDug = sum(VitaminDug),
    Day_VitaminB12ug = sum(VitaminB12ug),
    Day_VitaminCmg = sum(VitaminCmg),
    Day_Ironmg = sum(Ironmg),
    Day_Calciummg = sum(Calciummg),
    Day_Sodiummg = sum(Sodiummg),
    Day_Saltg = sum(Saltg), 
    Day_Magnesiummg = sum(Magnesiummg),
    Day_Potassiummg = sum(Potassiummg),
    Day_Iodineug = sum(Iodineug),
    Day_Seleniumug = sum(Seleniumug),
    Day_Zincmg = sum(Zincmg),
    
    Day_foodcatg_1 = sum(TotalGrams[FoodCatCode == 1], na.rm = TRUE),
    Day_foodcatg_2 = sum(TotalGrams[FoodCatCode == 2], na.rm = TRUE),
    Day_foodcatg_3 = sum(TotalGrams[FoodCatCode == 3], na.rm = TRUE),
    Day_foodcatg_4 = sum(TotalGrams[FoodCatCode == 4], na.rm = TRUE),
    Day_foodcatg_5 = sum(TotalGrams[FoodCatCode == 5], na.rm = TRUE),
    Day_foodcatg_6 = sum(TotalGrams[FoodCatCode == 6], na.rm = TRUE),
    Day_foodcatg_7 = sum(TotalGrams[FoodCatCode == 7], na.rm = TRUE),
    Day_foodcatg_8 = sum(TotalGrams[FoodCatCode == 8], na.rm = TRUE),
    Day_foodcatg_9 = sum(TotalGrams[FoodCatCode == 9], na.rm = TRUE),
    Day_foodcatg_10 = sum(TotalGrams[FoodCatCode == 10], na.rm = TRUE),
    Day_foodcatg_11 = sum(TotalGrams[FoodCatCode == 11], na.rm = TRUE),
    Day_foodcatg_12 = sum(TotalGrams[FoodCatCode == 12], na.rm = TRUE),
    Day_foodcatg_13 = sum(TotalGrams[FoodCatCode == 13], na.rm = TRUE),
    Day_foodcatg_14 = sum(TotalGrams[FoodCatCode == 14], na.rm = TRUE),
    Day_foodcatg_15 = sum(TotalGrams[FoodCatCode == 15], na.rm = TRUE),
    Day_foodcatg_16 = sum(TotalGrams[FoodCatCode == 16], na.rm = TRUE),
    Day_foodcatg_17 = sum(TotalGrams[FoodCatCode == 17], na.rm = TRUE),
    Day_foodcatg_18 = sum(TotalGrams[FoodCatCode == 18], na.rm = TRUE),
    
    Day_ndnsg_1 = sum(TotalGrams[MainFoodGroupCode == 1], na.rm = TRUE),
    Day_ndnsg_2 = sum(TotalGrams[MainFoodGroupCode == 2], na.rm = TRUE),
    Day_ndnsg_3 = sum(TotalGrams[MainFoodGroupCode == 3], na.rm = TRUE),
    Day_ndnsg_4 = sum(TotalGrams[MainFoodGroupCode == 4], na.rm = TRUE),
    Day_ndnsg_5 = sum(TotalGrams[MainFoodGroupCode == 5], na.rm = TRUE),
    Day_ndnsg_6 = sum(TotalGrams[MainFoodGroupCode == 6], na.rm = TRUE),
    Day_ndnsg_7 = sum(TotalGrams[MainFoodGroupCode == 7], na.rm = TRUE),
    Day_ndnsg_8 = sum(TotalGrams[MainFoodGroupCode == 8], na.rm = TRUE),
    Day_ndnsg_9 = sum(TotalGrams[MainFoodGroupCode == 9], na.rm = TRUE),
    Day_ndnsg_10 = sum(TotalGrams[MainFoodGroupCode == 10], na.rm = TRUE),
    Day_ndnsg_11 = sum(TotalGrams[MainFoodGroupCode == 11], na.rm = TRUE),
    Day_ndnsg_12 = sum(TotalGrams[MainFoodGroupCode == 12], na.rm = TRUE),
    Day_ndnsg_13 = sum(TotalGrams[MainFoodGroupCode == 13], na.rm = TRUE),
    Day_ndnsg_14 = sum(TotalGrams[MainFoodGroupCode == 14], na.rm = TRUE),
    Day_ndnsg_15 = sum(TotalGrams[MainFoodGroupCode == 15], na.rm = TRUE),
    Day_ndnsg_16 = sum(TotalGrams[MainFoodGroupCode == 16], na.rm = TRUE),
    Day_ndnsg_17 = sum(TotalGrams[MainFoodGroupCode == 17], na.rm = TRUE),
    Day_ndnsg_18 = sum(TotalGrams[MainFoodGroupCode == 18], na.rm = TRUE),
    Day_ndnsg_19 = sum(TotalGrams[MainFoodGroupCode == 19], na.rm = TRUE),
    Day_ndnsg_20 = sum(TotalGrams[MainFoodGroupCode == 20], na.rm = TRUE),
    Day_ndnsg_21 = sum(TotalGrams[MainFoodGroupCode == 21], na.rm = TRUE),
    Day_ndnsg_22 = sum(TotalGrams[MainFoodGroupCode == 22], na.rm = TRUE),
    Day_ndnsg_23 = sum(TotalGrams[MainFoodGroupCode == 23], na.rm = TRUE),
    Day_ndnsg_24 = sum(TotalGrams[MainFoodGroupCode == 24], na.rm = TRUE),
    Day_ndnsg_25 = sum(TotalGrams[MainFoodGroupCode == 25], na.rm = TRUE),
    Day_ndnsg_26 = sum(TotalGrams[MainFoodGroupCode == 26], na.rm = TRUE),
    Day_ndnsg_27 = sum(TotalGrams[MainFoodGroupCode == 27], na.rm = TRUE),
    Day_ndnsg_28 = sum(TotalGrams[MainFoodGroupCode == 28], na.rm = TRUE),
    Day_ndnsg_29 = sum(TotalGrams[MainFoodGroupCode == 29], na.rm = TRUE),
    Day_ndnsg_30 = sum(TotalGrams[MainFoodGroupCode == 30], na.rm = TRUE),
    Day_ndnsg_31 = sum(TotalGrams[MainFoodGroupCode == 31], na.rm = TRUE),
    Day_ndnsg_32 = sum(TotalGrams[MainFoodGroupCode == 32], na.rm = TRUE),
    Day_ndnsg_33 = sum(TotalGrams[MainFoodGroupCode == 33], na.rm = TRUE),
    Day_ndnsg_34 = sum(TotalGrams[MainFoodGroupCode == 34], na.rm = TRUE),
    Day_ndnsg_35 = sum(TotalGrams[MainFoodGroupCode == 35], na.rm = TRUE),
    Day_ndnsg_36 = sum(TotalGrams[MainFoodGroupCode == 36], na.rm = TRUE),
    Day_ndnsg_37 = sum(TotalGrams[MainFoodGroupCode == 37], na.rm = TRUE),
    Day_ndnsg_38 = sum(TotalGrams[MainFoodGroupCode == 38], na.rm = TRUE),
    Day_ndnsg_39 = sum(TotalGrams[MainFoodGroupCode == 39], na.rm = TRUE),
    Day_ndnsg_40 = sum(TotalGrams[MainFoodGroupCode == 40], na.rm = TRUE),
    Day_ndnsg_41 = sum(TotalGrams[MainFoodGroupCode == 41], na.rm = TRUE),
    Day_ndnsg_42 = sum(TotalGrams[MainFoodGroupCode == 42], na.rm = TRUE),
    Day_ndnsg_43 = sum(TotalGrams[MainFoodGroupCode == 43], na.rm = TRUE),
    Day_ndnsg_44 = sum(TotalGrams[MainFoodGroupCode == 44], na.rm = TRUE),
    Day_ndnsg_45 = sum(TotalGrams[MainFoodGroupCode == 45], na.rm = TRUE),
    Day_ndnsg_47 = sum(TotalGrams[MainFoodGroupCode == 47], na.rm = TRUE),
    Day_ndnsg_48 = sum(TotalGrams[MainFoodGroupCode == 48], na.rm = TRUE),
    Day_ndnsg_49 = sum(TotalGrams[MainFoodGroupCode == 49], na.rm = TRUE),
    Day_ndnsg_50 = sum(TotalGrams[MainFoodGroupCode == 50], na.rm = TRUE),
    Day_ndnsg_51 = sum(TotalGrams[MainFoodGroupCode == 51], na.rm = TRUE),
    Day_ndnsg_52 = sum(TotalGrams[MainFoodGroupCode == 52], na.rm = TRUE),
    Day_ndnsg_53 = sum(TotalGrams[MainFoodGroupCode == 53], na.rm = TRUE),
    Day_ndnsg_54 = sum(TotalGrams[MainFoodGroupCode == 54], na.rm = TRUE),
    Day_ndnsg_55 = sum(TotalGrams[MainFoodGroupCode == 55], na.rm = TRUE),
    Day_ndnsg_56 = sum(TotalGrams[MainFoodGroupCode == 56], na.rm = TRUE),
    Day_ndnsg_57 = sum(TotalGrams[MainFoodGroupCode == 57], na.rm = TRUE),
    Day_ndnsg_58 = sum(TotalGrams[MainFoodGroupCode == 58], na.rm = TRUE),
    Day_ndnsg_59 = sum(TotalGrams[MainFoodGroupCode == 59], na.rm = TRUE),
    Day_ndnsg_60 = sum(TotalGrams[MainFoodGroupCode == 60], na.rm = TRUE),
    Day_ndnsg_62 = sum(TotalGrams[MainFoodGroupCode == 62], na.rm = TRUE),
    Day_ndnsg_63 = sum(TotalGrams[MainFoodGroupCode == 63], na.rm = TRUE),
    Day_ndnsg_64 = sum(TotalGrams[MainFoodGroupCode == 64], na.rm = TRUE),
    Day_ndnsg_65 = sum(TotalGrams[MainFoodGroupCode == 65], na.rm = TRUE),
    Day_ndnsg_66 = sum(TotalGrams[MainFoodGroupCode == 66], na.rm = TRUE),
    
    Day_sweetbiscuitsg = sum(TotalGrams[DiscretionaryFoodGroupCode == 1], na.rm = TRUE),
    Day_cakespastriespuddingsg = sum(TotalGrams[DiscretionaryFoodGroupCode == 2], na.rm = TRUE),
    Day_crispsg = sum(TotalGrams[DiscretionaryFoodGroupCode == 3], na.rm = TRUE),
    Day_confectioneryg = sum(TotalGrams[DiscretionaryFoodGroupCode == 4], na.rm = TRUE),
    Day_icecreamg = sum(TotalGrams[DiscretionaryFoodGroupCode == 5], na.rm = TRUE),
    Day_sugarydrinksg = sum(TotalGrams[DiscretionaryFoodGroupCode == 6], na.rm = TRUE),
    Day_bfastcerealsg = sum(TotalGrams[DiscretionaryFoodGroupCode == 7], na.rm = TRUE),
    Day_potatoesg = sum(TotalGrams[DiscretionaryFoodGroupCode == 8], na.rm = TRUE),
    Day_pizzag = sum(TotalGrams[DiscretionaryFoodGroupCode == 9], na.rm = TRUE),
    Day_dairydessertsg = sum(TotalGrams[DiscretionaryFoodGroupCode == 10], na.rm = TRUE),
    Day_readymealsg = sum(TotalGrams[DiscretionaryFoodGroupCode == 11], na.rm = TRUE),
    
    Day_RRPMg = sum(Beefg, Lambg, Porkg, ProcessedRedMeatg, OtherRedMeatg, Burgersg, Sausagesg, Offalg),
    Day_FV_Beansg = sum(Beansg),
    Day_FV_FruitJuiceg = sum(FruitJuiceg),
    Day_FV_Smoothieg = sum(SmoothieFruitg),
    Day_FV_DriedFruitg = sum(DriedFruitg),
    Day_FV_FreshFruitg = sum(Fruitg),
    Day_FV_Vegg = sum(Tomatoesg, TomatoPureeg, Brassicaceaeg, YellowRedGreeng, OtherVegg)) %>%
  ungroup()



df.intake24_recall <- df.intake24_recall %>%
  mutate(Day_FV_JuiceSmoothieg = Day_FV_FruitJuiceg + Day_FV_Smoothieg)

df.intake24_recall <- df.intake24_recall %>%
  mutate(Day_FV_Portions_Beans = case_when(
    Age >= 2 & Age < 11 & Day_FV_Beansg > 40 ~ 1,
    Age >= 2 & Age < 11 & Day_FV_Beansg <= 40 ~ Day_FV_Beansg/40,
    Age >= 11 & Age < 16 & Day_FV_Beansg > 80 ~ 1,
    Age >= 11 & Age < 16 & Day_FV_Beansg <= 80 ~ Day_FV_Beansg/80,
    TRUE ~ NA),
    
    Day_FV_cap_Beans = case_when(
      Age >= 2 & Age < 11 & Day_FV_Beansg > 40 ~ 40,
      Age >= 2 & Age < 11 & Day_FV_Beansg <= 40 ~ Day_FV_Beansg,
      Age >= 11 & Age < 16 & Day_FV_Beansg > 80 ~ 80,
      Age >= 11 & Age < 16 & Day_FV_Beansg <= 80 ~ Day_FV_Beansg,
      TRUE ~ NA),
    
    Day_FV_Portions_JuiceSmoothie = case_when(
      Age >= 2 & Age < 11 & Day_FV_JuiceSmoothieg >75 ~ 1,
      Age >= 2 & Age < 11 & Day_FV_JuiceSmoothieg <= 75 ~ Day_FV_JuiceSmoothieg/75,
      Age >= 11 & Age < 16 & Day_FV_JuiceSmoothieg >150 ~ 1,
      Age >= 11 & Age < 16 & Day_FV_JuiceSmoothieg <= 150 ~ Day_FV_JuiceSmoothieg/150,
      TRUE ~ NA),
    
    Day_FV_cap_JuiceSmoothie = case_when(
      Age >= 2 & Age < 11 & Day_FV_JuiceSmoothieg >75 ~ 75,
      Age >= 2 & Age < 11 & Day_FV_JuiceSmoothieg <= 75 ~ Day_FV_JuiceSmoothieg,
      Age >= 11 & Age < 16 & Day_FV_JuiceSmoothieg >150 ~ 150,
      Age >= 11 & Age < 16 & Day_FV_JuiceSmoothieg <= 150 ~ Day_FV_JuiceSmoothieg,
      TRUE ~ NA),
    
    Day_FV_Portions_DriedFruit = case_when(
      Age >= 2 & Age < 11 ~ Day_FV_DriedFruitg/15,
      Age >= 11 & Age < 16 ~ Day_FV_DriedFruitg/30,
      TRUE ~ NA),
    
    Day_FV_Portions_FreshFruit = case_when(
      Age >= 2 & Age < 11 ~ Day_FV_FreshFruitg/40,
      Age >= 11 & Age < 16 ~ Day_FV_FreshFruitg/80,
      TRUE ~ NA),
    
    Day_FV_Portions_Veg = case_when(
      Age >= 2 & Age < 11 ~ Day_FV_Vegg/40,
      Age >= 11 & Age < 16 ~ Day_FV_Vegg/80,
      TRUE ~ NA))

df.intake24_recall <- df.intake24_recall %>%
  mutate(Day_FV_Portions_Total = Day_FV_Portions_Beans+Day_FV_Portions_JuiceSmoothie+Day_FV_Portions_DriedFruit+Day_FV_Portions_FreshFruit+Day_FV_Portions_Veg,
         Day_FV_Portions_Total_NoJuiceSmoothie = Day_FV_Portions_Beans+Day_FV_Portions_DriedFruit+Day_FV_Portions_FreshFruit+Day_FV_Portions_Veg,
         Day_FV_g_Total = Day_FV_DriedFruitg+Day_FV_FreshFruitg+Day_FV_Vegg+Day_FV_cap_Beans+Day_FV_cap_JuiceSmoothie) %>%
  relocate(Day_FV_Beansg, Day_FV_Portions_Beans, Day_FV_FruitJuiceg, Day_FV_Smoothieg, Day_FV_JuiceSmoothieg, Day_FV_Portions_JuiceSmoothie, Day_FV_DriedFruitg, 
           Day_FV_Portions_DriedFruit, Day_FV_FreshFruitg, Day_FV_Portions_FreshFruit, Day_FV_Vegg, Day_FV_Portions_Veg, Day_FV_g_Total, Day_FV_Portions_Total, Day_FV_Portions_Total_NoJuiceSmoothie, .after = Day_icecreamg)


## Intakes as a percentage of energy ####
df.intake24_recall <- df.intake24_recall %>%
  mutate(
    Day_Carbs_PropFoodEnergy = (Day_Carbs_kcal/Day_FoodEnergy)*100,
    Day_FreeSugars_PropFoodEnergy = (Day_FreeSugars_kcal/Day_FoodEnergy)*100,
    Day_Fat_PropFoodEnergy = (Day_Fat_kcal/Day_FoodEnergy)*100,
    Day_SatFat_PropFoodEnergy = (Day_SatFat_kcal/Day_FoodEnergy)*100,
    Day_TransFat_PropFoodEnergy = (Day_TransFat_kcal/Day_FoodEnergy)*100,
    Day_Protein_PropFoodEnergy = (Day_Protein_kcal/Day_FoodEnergy)*100) %>%
  relocate(Day_Carbs_PropFoodEnergy, .after = Day_Carbs_kcal) %>%
  relocate(Day_FreeSugars_PropFoodEnergy, .after = Day_FreeSugars_kcal) %>%
  relocate(Day_Fat_PropFoodEnergy, .after = Day_Fat_kcal) %>%
  relocate(Day_SatFat_PropFoodEnergy, .after = Day_SatFat_kcal) %>%
  relocate(Day_TransFat_PropFoodEnergy, .after = Day_TransFat_kcal) %>%
  relocate(Day_Protein_PropFoodEnergy, .after = Day_Protein_kcal)


## Energy Density ####
df.intake24_recall <- df.intake24_recall %>%
  mutate(Day_EnergyDensity = (Day_FoodMilkEnergy/Day_FoodMilkGrams)*100) %>%
  relocate(Day_EnergyDensity, .after = Day_FoodMilkGrams)


# Participant Level Values ####

## Mean participant nutrient and food group intake (across 1-4 recalls) 
df.intake24_participant <- df.intake24_recall %>% 
  group_by(UserID) %>%
  summarise(
    HouseholdID = first(HouseholdID),
    psu = first(psu),
    Device_group = first(Device_group),
    Age = first(Age),
    age_cat = first(age_cat),
    Sex = first(Sex),
    age_sex_cat = first(age_sex_cat),
    age_cat_drv = first(age_cat_drv),
    age_sex_cat_drv = first(age_sex_cat_drv),
    Ethnicity = first(Ethnicity),
    simd_quintile = first(simd_quintile),
    Education = first(Education),
    adults_cat = first(adults_cat),
    children_cat = first(children_cat),
    AFreqOilyFish = first(AFreqOilyFish),
    AFreqOilyFish = first(AFreqOilyFish),
    AFreqToddlerMilk = first(AFreqToddlerMilk),
    ChildCompletedFoodDiary = first(ChildCompletedFoodDiary),
    HHFOOD1 = first(HHFOOD1),
    HHFOOD2 = first(HHFOOD2),
    HHFOOD3 = first(HHFOOD3),
    HHFOOD4 = first(HHFOOD4),
    NumberOfRecalls = first(NumberOfRecalls), 
    Avg_Energykcal = mean(Day_Energykcal),
    Avg_FoodEnergy = mean(Day_FoodEnergy),
    Avg_FoodMilkEnergy = mean(Day_FoodMilkEnergy),
    Avg_TotalGrams = mean(Day_TotalGrams),
    Avg_FoodGrams = mean(Day_FoodGrams),
    Avg_FoodMilkGrams = mean(Day_FoodMilkGrams),
    Avg_EnergyDensity = mean(Day_EnergyDensity),
    Avg_Carbohydrateg = mean(Day_Carbohydrateg),
    Avg_Carbs_kcal = mean(Day_Carbs_kcal),
    Avg_Carbs_PropFoodEnergy = mean(Day_Carbs_PropFoodEnergy),
    Avg_FreeSugarsg = mean(Day_FreeSugarsg),
    Avg_FreeSugars_kcal = mean(Day_FreeSugars_kcal),
    Avg_FreeSugars_PropFoodEnergy = mean(Day_FreeSugars_PropFoodEnergy),
    Avg_Fatg = mean(Day_Fatg),
    Avg_Fat_kcal = mean(Day_Fat_kcal),
    Avg_Fat_PropFoodEnergy = mean(Day_Fat_PropFoodEnergy),
    Avg_SatFatg = mean(Day_SatFatg),
    Avg_SatFat_kcal = mean(Day_SatFat_kcal),
    Avg_SatFat_PropFoodEnergy = mean(Day_SatFat_PropFoodEnergy),
    Avg_TransFatg = mean(Day_TransFatg),
    Avg_TransFat_kcal = mean(Day_TransFat_kcal),
    Avg_TransFat_PropFoodEnergy = mean(Day_TransFat_PropFoodEnergy),
    Avg_Proteing = mean(Day_Proteing),
    Avg_Protein_kcal = mean(Day_Protein_kcal),
    Avg_Protein_PropFoodEnergy = mean(Day_Protein_PropFoodEnergy),
    Avg_AOACFibreg = mean(Day_AOACFibreg),
    Avg_Alcoholg = mean(Day_Alcoholg),
    Avg_Alcohol_kcal = mean(Day_Alcohol_kcal),
    Avg_VitaminAug = mean(Day_VitaminAug),
    Avg_Riboflavinmg = mean(Day_Riboflavinmg),
    Avg_Folateug = mean(Day_Folateug),
    Avg_VitaminDug = mean(Day_VitaminDug),
    Avg_VitaminB12ug = mean(Day_VitaminB12ug),
    Avg_VitaminCmg = mean(Day_VitaminCmg),
    Avg_Ironmg = mean(Day_Ironmg),
    Avg_Calciummg = mean(Day_Calciummg),
    Avg_Sodiummg = mean(Day_Sodiummg),
    Avg_Saltg = mean(Day_Saltg),
    Avg_Magnesiummg = mean(Day_Magnesiummg),
    Avg_Potassiummg = mean(Day_Potassiummg),
    Avg_Iodineug = mean(Day_Iodineug),
    Avg_Seleniumug = mean(Day_Seleniumug),
    Avg_Zincmg = mean(Day_Zincmg),
    
    Avg_foodcatg_1 = mean(Day_foodcatg_1),
    Avg_foodcatg_2 = mean(Day_foodcatg_2),
    Avg_foodcatg_3 = mean(Day_foodcatg_3),
    Avg_foodcatg_4 = mean(Day_foodcatg_4),
    Avg_foodcatg_5 = mean(Day_foodcatg_5),
    Avg_foodcatg_6 = mean(Day_foodcatg_6),
    Avg_foodcatg_7 = mean(Day_foodcatg_7),
    Avg_foodcatg_8 = mean(Day_foodcatg_8),
    Avg_foodcatg_9 = mean(Day_foodcatg_9),
    Avg_foodcatg_10 = mean(Day_foodcatg_10),
    Avg_foodcatg_11 = mean(Day_foodcatg_11),
    Avg_foodcatg_12 = mean(Day_foodcatg_12),
    Avg_foodcatg_13 = mean(Day_foodcatg_13),
    Avg_foodcatg_14 = mean(Day_foodcatg_14),
    Avg_foodcatg_15 = mean(Day_foodcatg_15),
    Avg_foodcatg_16 = mean(Day_foodcatg_16),
    Avg_foodcatg_17 = mean(Day_foodcatg_17),
    Avg_foodcatg_18 = mean(Day_foodcatg_18),
    
    Avg_ndnsg_1 = mean(Day_ndnsg_1),
    Avg_ndnsg_2 = mean(Day_ndnsg_2),
    Avg_ndnsg_3 = mean(Day_ndnsg_3),
    Avg_ndnsg_4 = mean(Day_ndnsg_4),
    Avg_ndnsg_5 = mean(Day_ndnsg_5),
    Avg_ndnsg_6 = mean(Day_ndnsg_6),
    Avg_ndnsg_7 = mean(Day_ndnsg_7),
    Avg_ndnsg_8 = mean(Day_ndnsg_8),
    Avg_ndnsg_9 = mean(Day_ndnsg_9),
    Avg_ndnsg_10 = mean(Day_ndnsg_10),
    Avg_ndnsg_11 = mean(Day_ndnsg_11),
    Avg_ndnsg_12 = mean(Day_ndnsg_12),
    Avg_ndnsg_13 = mean(Day_ndnsg_13),
    Avg_ndnsg_14 = mean(Day_ndnsg_14),
    Avg_ndnsg_15 = mean(Day_ndnsg_15),
    Avg_ndnsg_16 = mean(Day_ndnsg_16),
    Avg_ndnsg_17 = mean(Day_ndnsg_17),
    Avg_ndnsg_18 = mean(Day_ndnsg_18),
    Avg_ndnsg_19 = mean(Day_ndnsg_19),
    Avg_ndnsg_20 = mean(Day_ndnsg_20),
    Avg_ndnsg_21 = mean(Day_ndnsg_21),
    Avg_ndnsg_22 = mean(Day_ndnsg_22),
    Avg_ndnsg_23 = mean(Day_ndnsg_23),
    Avg_ndnsg_24 = mean(Day_ndnsg_24),
    Avg_ndnsg_25 = mean(Day_ndnsg_25),
    Avg_ndnsg_26 = mean(Day_ndnsg_26),
    Avg_ndnsg_27 = mean(Day_ndnsg_27),
    Avg_ndnsg_28 = mean(Day_ndnsg_28),
    Avg_ndnsg_29 = mean(Day_ndnsg_29),
    Avg_ndnsg_30 = mean(Day_ndnsg_30),
    Avg_ndnsg_31 = mean(Day_ndnsg_31),
    Avg_ndnsg_32 = mean(Day_ndnsg_32),
    Avg_ndnsg_33 = mean(Day_ndnsg_33),
    Avg_ndnsg_34 = mean(Day_ndnsg_34),
    Avg_ndnsg_35 = mean(Day_ndnsg_35),
    Avg_ndnsg_36 = mean(Day_ndnsg_36),
    Avg_ndnsg_37 = mean(Day_ndnsg_37),
    Avg_ndnsg_38 = mean(Day_ndnsg_38),
    Avg_ndnsg_39 = mean(Day_ndnsg_39),
    Avg_ndnsg_40 = mean(Day_ndnsg_40),
    Avg_ndnsg_41 = mean(Day_ndnsg_41),
    Avg_ndnsg_42 = mean(Day_ndnsg_42),
    Avg_ndnsg_43 = mean(Day_ndnsg_43),
    Avg_ndnsg_44 = mean(Day_ndnsg_44),
    Avg_ndnsg_45 = mean(Day_ndnsg_45),
    Avg_ndnsg_47 = mean(Day_ndnsg_47),
    Avg_ndnsg_48 = mean(Day_ndnsg_48),
    Avg_ndnsg_49 = mean(Day_ndnsg_49),
    Avg_ndnsg_50 = mean(Day_ndnsg_50),
    Avg_ndnsg_51 = mean(Day_ndnsg_51),
    Avg_ndnsg_52 = mean(Day_ndnsg_52),
    Avg_ndnsg_53 = mean(Day_ndnsg_53),
    Avg_ndnsg_54 = mean(Day_ndnsg_54),
    Avg_ndnsg_55 = mean(Day_ndnsg_55),
    Avg_ndnsg_56 = mean(Day_ndnsg_56),
    Avg_ndnsg_57 = mean(Day_ndnsg_57),
    Avg_ndnsg_58 = mean(Day_ndnsg_58),
    Avg_ndnsg_59 = mean(Day_ndnsg_59),
    Avg_ndnsg_60 = mean(Day_ndnsg_60),
    Avg_ndnsg_62 = mean(Day_ndnsg_62),
    Avg_ndnsg_63 = mean(Day_ndnsg_63),
    Avg_ndnsg_64 = mean(Day_ndnsg_64),
    Avg_ndnsg_65 = mean(Day_ndnsg_65),
    Avg_ndnsg_66 = mean(Day_ndnsg_66),
    
    Avg_sweetbiscuitsg = mean(Day_sweetbiscuitsg),
    Avg_cakespastriespuddingsg = mean(Day_cakespastriespuddingsg),
    Avg_crispsg = mean(Day_crispsg),
    Avg_confectioneryg = mean(Day_confectioneryg),
    Avg_icecreamg = mean(Day_icecreamg),
    Avg_sugarydrinksg = mean(Day_sugarydrinksg),
    Avg_bfastcerealsg = mean(Day_bfastcerealsg),
    Avg_potatoesg = mean(Day_potatoesg),
    Avg_pizzag = mean(Day_pizzag),
    Avg_dairydessertsg = mean(Day_dairydessertsg),
    Avg_readymealsg = mean(Day_readymealsg),
    
    Avg_RRPMg = mean(Day_RRPMg),
    Avg_FV_Beansg = mean(Day_FV_Beansg),
    Avg_FV_Portions_Beans = mean(Day_FV_Portions_Beans),
    Avg_FV_FruitJuiceg = mean(Day_FV_FruitJuiceg),
    Avg_FV_Smoothieg = mean(Day_FV_Smoothieg),
    Avg_FV_JuiceSmoothieg = mean(Day_FV_JuiceSmoothieg),
    Avg_FV_Portions_JuiceSmoothie = mean(Day_FV_Portions_JuiceSmoothie),
    Avg_FV_DriedFruitg = mean(Day_FV_DriedFruitg),
    Avg_FV_Portions_DriedFruit = mean(Day_FV_Portions_DriedFruit),
    Avg_FV_FreshFruitg = mean(Day_FV_FreshFruitg),
    Avg_FV_Portions_FreshFruit = mean(Day_FV_Portions_FreshFruit),
    Avg_FV_Vegg = mean(Day_FV_Vegg),
    Avg_FV_Portions_Veg = mean(Day_FV_Portions_Veg),
    Avg_FV_g_Total = mean(Day_FV_g_Total),
    Avg_FV_Portions_Total = mean(Day_FV_Portions_Total),
    Avg_FV_Portions_Total_NoJuiceSmoothie = mean(Day_FV_Portions_Total_NoJuiceSmoothie)) %>%
  ungroup()



# Food Groups ####

## Contributions of food categories to energy and nutrient intake ####

## Total daily energy and nutrient intakes from food categories 
df.intake24_foodcat <- df.intake24_item %>%
  group_by(UserID, RecallNo, FoodCatCode) %>% 
  summarise(
    NumberOfRecalls = first(NumberOfRecalls),
    Day_Energykcal_foodcat = sum(Energykcal),
    Day_Carbohydrateg_foodcat = sum(Carbohydrateg),
    Day_FreeSugarsg_foodcat = sum(Freesugarsg), 
    Day_Fatg_foodcat = sum(Fatg),
    Day_SatFatg_foodcat = sum(Saturatedfattyacidsg),
    Day_TransFatg_foodcat = sum(Transfattyacidsg),
    Day_Proteing_foodcat = sum(Proteing),
    Day_AOACFibreg_foodcat = sum(AOACg),
    Day_VitaminAug_foodcat = sum(VitaminAug),
    Day_Riboflavinmg_foodcat = sum(Riboflavinmg),
    Day_Folateug_foodcat = sum(Folateug),
    Day_VitaminDug_foodcat = sum(VitaminDug),
    Day_VitaminB12ug_foodcat = sum(VitaminB12ug),
    Day_VitaminCmg_foodcat = sum(VitaminCmg),
    Day_Ironmg_foodcat = sum(Ironmg),
    Day_Calciummg_foodcat = sum(Calciummg),
    Day_Sodiummg_foodcat = sum(Sodiummg),
    Day_Magnesiummg_foodcat = sum(Magnesiummg),
    Day_Potassiummg_foodcat = sum(Potassiummg),
    Day_Iodineug_foodcat = sum(Iodineug),
    Day_Seleniumug_foodcat = sum(Seleniumug),
    Day_Zincmg_foodcat = sum(Zincmg)) %>%
  ungroup()

# Create a grid with all possible food category and recall number combinations for each participant 
# Need to do this separately for different recall numbers

# 1 recall
df.onerecall <- df.intake24_recall %>%
  filter(NumberOfRecalls == 1)

foodgrid_onerecall <- expand.grid( #create a grid of each combination of the below:
  UserID = unique(df.onerecall$UserID), #take each unique UserID
  RecallNo = unique(df.onerecall$RecallNo), #take each unique RecallNo
  FoodCatCode = factor(1:18)) #create a column of with 1-18 for food categories

# 2 recalls
df.tworecalls <- df.intake24_recall %>%
  filter(NumberOfRecalls == 2)

foodgrid_tworecalls <- expand.grid(
  UserID = unique(df.tworecalls$UserID),
  RecallNo = unique(df.tworecalls$RecallNo),
  FoodCatCode = factor(1:18))

# 3 recalls
df.threerecalls <- df.intake24_recall %>%
  filter(NumberOfRecalls == 3)

foodgrid_threerecalls <- expand.grid(
  UserID = unique(df.threerecalls$UserID),
  RecallNo = unique(df.threerecalls$RecallNo),
  FoodCatCode = factor(1:18))

#4 recalls 
df.fourrecalls <- df.intake24_recall %>%
  filter(NumberOfRecalls == 4)

foodgrid_fourrecalls <- expand.grid(
  UserID = unique(df.fourrecalls$UserID),
  RecallNo = unique(df.fourrecalls$RecallNo),
  FoodCatCode = factor(1:18))

# Combine expanded grids 
foodgroup_grid <- bind_rows(foodgrid_onerecall, foodgrid_tworecalls, foodgrid_threerecalls, foodgrid_fourrecalls) %>%
  arrange(UserID, RecallNo) %>%
  mutate(FoodCatCode = as.numeric(FoodCatCode))

# Join daily intakes with expanded grids
df.intake24_foodcat <- left_join(foodgroup_grid, df.intake24_foodcat, by = c("UserID", "RecallNo", "FoodCatCode")) %>%
  select(-NumberOfRecalls)

# Fill in NumberOfRecalls
df.numberrecalls <- df.intake24_participant %>% select(UserID, NumberOfRecalls)
df.intake24_foodcat <- left_join(df.intake24_foodcat, df.numberrecalls, by = "UserID") 
df.intake24_foodcat <- df.intake24_foodcat %>%
  relocate(NumberOfRecalls, .after=UserID)

# Change NA values to 0 
df.intake24_foodcat <- df.intake24_foodcat %>%
  mutate_all(~ifelse(is.na(.), 0, .))

# Remove datasets no longer needed
rm(df.onerecall, df.tworecalls, df.threerecalls, df.fourrecalls, foodgrid_onerecall, foodgrid_tworecalls, foodgrid_threerecalls, foodgrid_fourrecalls, foodgroup_grid)


## Percent energy and nutrients from food categories 

### Pull in mean daily intakes of energy and nutrients 
df.day_nutrients <- df.intake24_recall %>% 
  select(UserID, RecallNo, Day_Energykcal, Day_Carbohydrateg, Day_FreeSugarsg, Day_Fatg, Day_SatFatg, Day_TransFatg, Day_Proteing, 
         Day_AOACFibreg, Day_VitaminAug, Day_Riboflavinmg, Day_Folateug, Day_VitaminDug, Day_VitaminB12ug, Day_VitaminCmg,
         Day_Ironmg, Day_Calciummg, Day_Sodiummg, Day_Magnesiummg, Day_Potassiummg, Day_Iodineug, Day_Seleniumug, Day_Zincmg)

df.intake24_foodcat <- left_join(df.intake24_foodcat, df.day_nutrients, by=c("UserID", "RecallNo"))

### Calculate proportion of energy and nutrients from food categories 
df.intake24_foodcat <- df.intake24_foodcat %>%
  mutate(
    Day_Prop_Energykcal_foodcat=(Day_Energykcal_foodcat/Day_Energykcal)*100,
    Day_Prop_Carbohydrateg_foodcat=(Day_Carbohydrateg_foodcat/Day_Carbohydrateg)*100,
    Day_Prop_FreeSugarsg_foodcat=(Day_FreeSugarsg_foodcat/Day_FreeSugarsg)*100,
    Day_Prop_Fatg_foodcat=(Day_Fatg_foodcat/Day_Fatg)*100,
    Day_Prop_SatFatg_foodcat=(Day_SatFatg_foodcat/Day_SatFatg)*100,
    Day_Prop_TransFatg_foodcat=(Day_TransFatg_foodcat/Day_TransFatg)*100,
    Day_Prop_Proteing_foodcat=(Day_Proteing_foodcat/Day_Proteing)*100,
    Day_Prop_AOACFibreg_foodcat=(Day_AOACFibreg_foodcat/Day_AOACFibreg)*100,
    Day_Prop_VitaminAug_foodcat=(Day_VitaminAug_foodcat/Day_VitaminAug)*100,
    Day_Prop_Riboflavinmg_foodcat=(Day_Riboflavinmg_foodcat/Day_Riboflavinmg)*100,
    Day_Prop_Folateug_foodcat=(Day_Folateug_foodcat/Day_Folateug)*100,
    Day_Prop_VitaminDug_foodcat=(Day_VitaminDug_foodcat/Day_VitaminDug)*100,
    Day_Prop_VitaminB12ug_foodcat=(Day_VitaminB12ug_foodcat/Day_VitaminB12ug)*100,
    Day_Prop_VitaminCmg_foodcat=(Day_VitaminCmg_foodcat/Day_VitaminCmg)*100,
    Day_Prop_Ironmg_foodcat=(Day_Ironmg_foodcat/Day_Ironmg)*100,
    Day_Prop_Calciummg_foodcat=(Day_Calciummg_foodcat/Day_Calciummg)*100,
    Day_Prop_Sodiummg_foodcat=(Day_Sodiummg_foodcat/Day_Sodiummg)*100,
    Day_Prop_Magnesiummg_foodcat=(Day_Magnesiummg_foodcat/Day_Magnesiummg)*100,
    Day_Prop_Potassiummg_foodcat=(Day_Potassiummg_foodcat/Day_Potassiummg)*100,
    Day_Prop_Iodineug_foodcat=(Day_Iodineug_foodcat/Day_Iodineug)*100,
    Day_Prop_Seleniumug_foodcat=(Day_Seleniumug_foodcat/Day_Seleniumug)*100,
    Day_Prop_Zincmg_foodcat=(Day_Zincmg_foodcat/Day_Zincmg)*100
  ) %>% 
  select(UserID, NumberOfRecalls, RecallNo, FoodCatCode, Day_Prop_Energykcal_foodcat:Day_Prop_Zincmg_foodcat)  

#Replace NaN (not a number) to 0 - these are instances when daily intake of the nutrient was 0
df.intake24_foodcat <- df.intake24_foodcat %>%
  mutate(across(c(Day_Prop_Energykcal_foodcat:Day_Prop_Zincmg_foodcat), as.numeric))

df.intake24_foodcat <- df.intake24_foodcat %>%
  mutate(across(c(Day_Prop_Energykcal_foodcat:Day_Prop_Zincmg_foodcat), ~ifelse(is.nan(.), 0, .)))


## Mean participant energy and nutrients from food categories
df.intake24_foodcat <- df.intake24_foodcat %>% 
  group_by(UserID, FoodCatCode) %>%
  summarise(
    Avg_Prop_Energykcal_foodcat = mean(Day_Prop_Energykcal_foodcat),
    Avg_Prop_Carbohydrateg_foodcat = mean(Day_Prop_Carbohydrateg_foodcat),
    Avg_Prop_FreeSugarsg_foodcat=mean(Day_Prop_FreeSugarsg_foodcat),
    Avg_Prop_Fatg_foodcat=mean(Day_Prop_Fatg_foodcat),
    Avg_Prop_SatFatg_foodcat=mean(Day_Prop_SatFatg_foodcat),
    Avg_Prop_TransFatg_foodcat=mean(Day_Prop_TransFatg_foodcat),
    Avg_Prop_Proteing_foodcat=mean(Day_Prop_Proteing_foodcat),
    Avg_Prop_AOACFibreg_foodcat=mean(Day_Prop_AOACFibreg_foodcat),
    Avg_Prop_VitaminAug_foodcat=mean(Day_Prop_VitaminAug_foodcat),
    Avg_Prop_Riboflavinmg_foodcat=mean(Day_Prop_Riboflavinmg_foodcat),
    Avg_Prop_Folateug_foodcat=mean(Day_Prop_Folateug_foodcat),
    Avg_Prop_VitaminDug_foodcat=mean(Day_Prop_VitaminDug_foodcat),
    Avg_Prop_VitaminB12ug_foodcat=mean(Day_Prop_VitaminB12ug_foodcat),
    Avg_Prop_VitaminCmg_foodcat=mean(Day_Prop_VitaminCmg_foodcat),
    Avg_Prop_Ironmg_foodcat=mean(Day_Prop_Ironmg_foodcat),
    Avg_Prop_Calciummg_foodcat=mean(Day_Prop_Calciummg_foodcat),
    Avg_Prop_Sodiummg_foodcat=mean(Day_Prop_Sodiummg_foodcat),
    Avg_Prop_Magnesiummg_foodcat=mean(Day_Prop_Magnesiummg_foodcat),
    Avg_Prop_Potassiummg_foodcat=mean(Day_Prop_Potassiummg_foodcat),
    Avg_Prop_Iodineug_foodcat=mean(Day_Prop_Iodineug_foodcat),
    Avg_Prop_Seleniumug_foodcat=mean(Day_Prop_Seleniumug_foodcat),
    Avg_Prop_Zincmg_foodcat=mean(Day_Prop_Zincmg_foodcat)
  ) %>%
  ungroup()

# pivot to wide format
df.intake24_foodcat <- df.intake24_foodcat %>%
  pivot_wider(
    names_from = FoodCatCode,
    values_from = c(Avg_Prop_Energykcal_foodcat, Avg_Prop_Carbohydrateg_foodcat, Avg_Prop_FreeSugarsg_foodcat, Avg_Prop_Fatg_foodcat, Avg_Prop_SatFatg_foodcat, Avg_Prop_TransFatg_foodcat,
                    Avg_Prop_Proteing_foodcat, Avg_Prop_AOACFibreg_foodcat, Avg_Prop_VitaminAug_foodcat, Avg_Prop_Riboflavinmg_foodcat, Avg_Prop_Folateug_foodcat, Avg_Prop_VitaminDug_foodcat,
                    Avg_Prop_VitaminB12ug_foodcat, Avg_Prop_VitaminCmg_foodcat, Avg_Prop_Ironmg_foodcat, Avg_Prop_Calciummg_foodcat, Avg_Prop_Sodiummg_foodcat, Avg_Prop_Magnesiummg_foodcat,
                    Avg_Prop_Potassiummg_foodcat, Avg_Prop_Iodineug_foodcat, Avg_Prop_Seleniumug_foodcat, Avg_Prop_Zincmg_foodcat))


## Contributions of NDNS main food groups to energy and nutrient intakes ####

## Total daily energy and nutrient intakes from NDNS main food groups
df.intake24_main_fg <- df.intake24_item %>%
  group_by(UserID, RecallNo, MainFoodGroupCode) %>% 
  summarise(
    NumberOfRecalls = first(NumberOfRecalls),
    Day_Energykcal_fg = sum(Energykcal),
    Day_Carbohydrateg_fg = sum(Carbohydrateg),
    Day_FreeSugarsg_fg = sum(Freesugarsg), 
    Day_Fatg_fg = sum(Fatg),
    Day_SatFatg_fg = sum(Saturatedfattyacidsg),
    Day_TransFatg_fg = sum(Transfattyacidsg),
    Day_Proteing_fg = sum(Proteing),
    Day_AOACFibreg_fg = sum(AOACg),
    Day_VitaminAug_fg = sum(VitaminAug),
    Day_Riboflavinmg_fg = sum(Riboflavinmg),
    Day_Folateug_fg = sum(Folateug),
    Day_VitaminDug_fg = sum(VitaminDug),
    Day_VitaminB12ug_fg = sum(VitaminB12ug),
    Day_VitaminCmg_fg = sum(VitaminCmg),
    Day_Ironmg_fg = sum(Ironmg),
    Day_Calciummg_fg = sum(Calciummg),
    Day_Sodiummg_fg = sum(Sodiummg),
    Day_Magnesiummg_fg = sum(Magnesiummg),
    Day_Potassiummg_fg = sum(Potassiummg),
    Day_Iodineug_fg = sum(Iodineug),
    Day_Seleniumug_fg = sum(Seleniumug),
    Day_Zincmg_fg = sum(Zincmg)) %>%
  ungroup()

# Create a grid with all possible food category and recall number combinations for each participant 
# Need to do this separately for different recall numbers

# 1 recall
df.onerecall <- df.intake24_recall %>%
  filter(NumberOfRecalls == 1)

foodgrid_onerecall <- expand.grid( #create a grid of each combination of the below:
  UserID = unique(df.onerecall$UserID), #take each unique UserID
  RecallNo = unique(df.onerecall$RecallNo), #take each unique RecallNo
  MainFoodGroupCode = factor(1:66)) #create a column of with 1-66 for NDNS main food groups

# 2 recalls
df.tworecalls <- df.intake24_recall %>%
  filter(NumberOfRecalls == 2)

foodgrid_tworecalls <- expand.grid(
  UserID = unique(df.tworecalls$UserID),
  RecallNo = unique(df.tworecalls$RecallNo),
  MainFoodGroupCode = factor(1:66))

# 3 recalls
df.threerecalls <- df.intake24_recall %>%
  filter(NumberOfRecalls == 3)

foodgrid_threerecalls <- expand.grid(
  UserID = unique(df.threerecalls$UserID),
  RecallNo = unique(df.threerecalls$RecallNo),
  MainFoodGroupCode = factor(1:66))

#4 recalls 
df.fourrecalls <- df.intake24_recall %>%
  filter(NumberOfRecalls == 4)

foodgrid_fourrecalls <- expand.grid(
  UserID = unique(df.fourrecalls$UserID),
  RecallNo = unique(df.fourrecalls$RecallNo),
  MainFoodGroupCode = factor(1:66))

# Combine expanded grids
foodgroup_grid <- bind_rows(foodgrid_onerecall, foodgrid_tworecalls, foodgrid_threerecalls, foodgrid_fourrecalls) %>%
  arrange(UserID, RecallNo) %>%
  mutate(MainFoodGroupCode = as.numeric(MainFoodGroupCode))

# Join daily intakes with expanded grids
df.intake24_main_fg <- left_join(foodgroup_grid, df.intake24_main_fg, by = c("UserID", "RecallNo", "MainFoodGroupCode")) %>%
  select(-NumberOfRecalls)

# Fill in NumberOfRecalls
df.numberrecalls <- df.intake24_participant %>% select(UserID, NumberOfRecalls)
df.intake24_main_fg <- left_join(df.intake24_main_fg, df.numberrecalls, by = "UserID") 
df.intake24_main_fg <- df.intake24_main_fg %>%
  relocate(NumberOfRecalls, .after=UserID)

# Change NA values to 0 
df.intake24_main_fg <- df.intake24_main_fg %>%
  mutate_all(~ifelse(is.na(.), 0, .))

# Remove datasets no longer needed
rm(df.onerecall, df.tworecalls, df.threerecalls, df.fourrecalls, foodgrid_onerecall, foodgrid_tworecalls, foodgrid_threerecalls, foodgrid_fourrecalls, foodgroup_grid)


## Percent energy and nutrients from NDNS main food groups

### Pull in mean daily intakes of energy and nutrients 
df.intake24_main_fg <- left_join(df.intake24_main_fg, df.day_nutrients, by=c("UserID", "RecallNo"))

### Calculate proportion of energy and nutrients from NDNS main food groups 
df.intake24_main_fg <- df.intake24_main_fg %>%
  mutate(
    Day_Prop_Energykcal_fg=(Day_Energykcal_fg/Day_Energykcal)*100,
    Day_Prop_Carbohydrateg_fg=(Day_Carbohydrateg_fg/Day_Carbohydrateg)*100,
    Day_Prop_FreeSugarsg_fg=(Day_FreeSugarsg_fg/Day_FreeSugarsg)*100,
    Day_Prop_Fatg_fg=(Day_Fatg_fg/Day_Fatg)*100,
    Day_Prop_SatFatg_fg=(Day_SatFatg_fg/Day_SatFatg)*100,
    Day_Prop_TransFatg_fg=(Day_TransFatg_fg/Day_TransFatg)*100,
    Day_Prop_Proteing_fg=(Day_Proteing_fg/Day_Proteing)*100,
    Day_Prop_AOACFibreg_fg=(Day_AOACFibreg_fg/Day_AOACFibreg)*100,
    Day_Prop_VitaminAug_fg=(Day_VitaminAug_fg/Day_VitaminAug)*100,
    Day_Prop_Riboflavinmg_fg=(Day_Riboflavinmg_fg/Day_Riboflavinmg)*100,
    Day_Prop_Folateug_fg=(Day_Folateug_fg/Day_Folateug)*100,
    Day_Prop_VitaminDug_fg=(Day_VitaminDug_fg/Day_VitaminDug)*100,
    Day_Prop_VitaminB12ug_fg=(Day_VitaminB12ug_fg/Day_VitaminB12ug)*100,
    Day_Prop_VitaminCmg_fg=(Day_VitaminCmg_fg/Day_VitaminCmg)*100,
    Day_Prop_Ironmg_fg=(Day_Ironmg_fg/Day_Ironmg)*100,
    Day_Prop_Calciummg_fg=(Day_Calciummg_fg/Day_Calciummg)*100,
    Day_Prop_Sodiummg_fg=(Day_Sodiummg_fg/Day_Sodiummg)*100,
    Day_Prop_Magnesiummg_fg=(Day_Magnesiummg_fg/Day_Magnesiummg)*100,
    Day_Prop_Potassiummg_fg=(Day_Potassiummg_fg/Day_Potassiummg)*100,
    Day_Prop_Iodineug_fg=(Day_Iodineug_fg/Day_Iodineug)*100,
    Day_Prop_Seleniumug_fg=(Day_Seleniumug_fg/Day_Seleniumug)*100,
    Day_Prop_Zincmg_fg=(Day_Zincmg_fg/Day_Zincmg)*100
  ) %>% 
  select(UserID, NumberOfRecalls, RecallNo, MainFoodGroupCode, Day_Prop_Energykcal_fg:Day_Prop_Zincmg_fg)  

#Replace NaN (not a number) to 0 - these are instances when daily intake of the nutrient was 0
df.intake24_main_fg <- df.intake24_main_fg %>%
  mutate(across(c(Day_Prop_Energykcal_fg:Day_Prop_Zincmg_fg), as.numeric))

df.intake24_main_fg <- df.intake24_main_fg %>%
  mutate(across(c(Day_Prop_Energykcal_fg:Day_Prop_Zincmg_fg), ~ifelse(is.nan(.), 0, .)))


## Mean participant energy and nutrients from food groups 
df.intake24_main_fg <- df.intake24_main_fg %>% 
  group_by(UserID, MainFoodGroupCode) %>%
  summarise(
    Avg_Prop_Energykcal_fg = mean(Day_Prop_Energykcal_fg),
    Avg_Prop_Carbohydrateg_fg = mean(Day_Prop_Carbohydrateg_fg),
    Avg_Prop_FreeSugarsg_fg=mean(Day_Prop_FreeSugarsg_fg),
    Avg_Prop_Fatg_fg=mean(Day_Prop_Fatg_fg),
    Avg_Prop_SatFatg_fg=mean(Day_Prop_SatFatg_fg),
    Avg_Prop_TransFatg_fg=mean(Day_Prop_TransFatg_fg),
    Avg_Prop_Proteing_fg=mean(Day_Prop_Proteing_fg),
    Avg_Prop_AOACFibreg_fg=mean(Day_Prop_AOACFibreg_fg),
    Avg_Prop_VitaminAug_fg=mean(Day_Prop_VitaminAug_fg),
    Avg_Prop_Riboflavinmg_fg=mean(Day_Prop_Riboflavinmg_fg),
    Avg_Prop_Folateug_fg=mean(Day_Prop_Folateug_fg),
    Avg_Prop_VitaminDug_fg=mean(Day_Prop_VitaminDug_fg),
    Avg_Prop_VitaminB12ug_fg=mean(Day_Prop_VitaminB12ug_fg),
    Avg_Prop_VitaminCmg_fg=mean(Day_Prop_VitaminCmg_fg),
    Avg_Prop_Ironmg_fg=mean(Day_Prop_Ironmg_fg),
    Avg_Prop_Calciummg_fg=mean(Day_Prop_Calciummg_fg),
    Avg_Prop_Sodiummg_fg=mean(Day_Prop_Sodiummg_fg),
    Avg_Prop_Magnesiummg_fg=mean(Day_Prop_Magnesiummg_fg),
    Avg_Prop_Potassiummg_fg=mean(Day_Prop_Potassiummg_fg),
    Avg_Prop_Iodineug_fg=mean(Day_Prop_Iodineug_fg),
    Avg_Prop_Seleniumug_fg=mean(Day_Prop_Seleniumug_fg),
    Avg_Prop_Zincmg_fg=mean(Day_Prop_Zincmg_fg)
  ) %>%
  ungroup()

# drop rows that are not main food groups
df.intake24_main_fg <- df.intake24_main_fg %>%
  filter(!MainFoodGroupCode == 46)

df.intake24_main_fg <- df.intake24_main_fg %>%
  filter(!MainFoodGroupCode == 61)

# pivot to wide format
df.intake24_main_fg <- df.intake24_main_fg %>%
  pivot_wider(
    names_from = MainFoodGroupCode,
    values_from = c(Avg_Prop_Energykcal_fg, Avg_Prop_Carbohydrateg_fg, Avg_Prop_FreeSugarsg_fg, Avg_Prop_Fatg_fg, Avg_Prop_SatFatg_fg, Avg_Prop_TransFatg_fg,
                    Avg_Prop_Proteing_fg, Avg_Prop_AOACFibreg_fg, Avg_Prop_VitaminAug_fg, Avg_Prop_Riboflavinmg_fg, Avg_Prop_Folateug_fg, Avg_Prop_VitaminDug_fg,
                    Avg_Prop_VitaminB12ug_fg, Avg_Prop_VitaminCmg_fg, Avg_Prop_Ironmg_fg, Avg_Prop_Calciummg_fg, Avg_Prop_Sodiummg_fg, Avg_Prop_Magnesiummg_fg,
                    Avg_Prop_Potassiummg_fg, Avg_Prop_Iodineug_fg, Avg_Prop_Seleniumug_fg, Avg_Prop_Zincmg_fg)
  )



## Contributions of discretionary food groups to energy, fat, saturated fat, free sugars ####

## Total daily energy, fat, saturated fat, free sugars from discretionary food groups 
df.intake24_discretionary_fg <- df.intake24_item %>%
  group_by(UserID, RecallNo, DiscretionaryFoodGroupCode) %>% 
  summarise(
    NumberOfRecalls = first(NumberOfRecalls),
    Day_Energykcal_discr =sum(Energykcal),
    Day_FreeSugarsg_discr =sum(Freesugarsg),
    Day_Fatg_discr = sum(Fatg),
    Day_SatFatg_discr = sum(Saturatedfattyacidsg))

# Create a grid with all possible food category and recall number combinations for each participant
# Need to do this separately for different recall numbers

# 1 recall
df.onerecall <- df.intake24_recall %>%
  filter(NumberOfRecalls == 1)

foodgrid_onerecall <- expand.grid( #create a grid of each combination of the below:
  UserID = unique(df.onerecall$UserID), #take each unique UserID
  RecallNo = unique(df.onerecall$RecallNo), #take each unique RecallNo
  DiscretionaryFoodGroupCode = factor(1:11)) #create a column of with 1-11 for discretionary food groups

# 2 recalls
df.tworecalls <- df.intake24_recall %>%
  filter(NumberOfRecalls == 2)

foodgrid_tworecalls <- expand.grid(
  UserID = unique(df.tworecalls$UserID),
  RecallNo = unique(df.tworecalls$RecallNo),
  DiscretionaryFoodGroupCode = factor(1:11))

# 3 recalls
df.threerecalls <- df.intake24_recall %>%
  filter(NumberOfRecalls == 3)

foodgrid_threerecalls <- expand.grid(
  UserID = unique(df.threerecalls$UserID),
  RecallNo = unique(df.threerecalls$RecallNo),
  DiscretionaryFoodGroupCode = factor(1:11))

#4 recalls 
df.fourrecalls <- df.intake24_recall %>%
  filter(NumberOfRecalls == 4)

foodgrid_fourrecalls <- expand.grid(
  UserID = unique(df.fourrecalls$UserID),
  RecallNo = unique(df.fourrecalls$RecallNo),
  DiscretionaryFoodGroupCode = factor(1:11))

# Combine expanded grids
foodgroup_grid <- bind_rows(foodgrid_onerecall, foodgrid_tworecalls, foodgrid_threerecalls, foodgrid_fourrecalls) %>%
  arrange(UserID, RecallNo) %>%
  mutate(DiscretionaryFoodGroupCode = as.numeric(DiscretionaryFoodGroupCode))

# Join daily intakes with expanded grids
df.intake24_discretionary_fg <- left_join(foodgroup_grid, df.intake24_discretionary_fg, by = c("UserID", "RecallNo", "DiscretionaryFoodGroupCode")) %>%
  select(-NumberOfRecalls)

# Fill in NumberOfRecalls
df.intake24_discretionary_fg <- left_join(df.intake24_discretionary_fg, df.numberrecalls, by = "UserID") 
df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>%
  relocate(NumberOfRecalls, .after=UserID)

# Change NA values to 0 
df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>%
  mutate_all(~ifelse(is.na(.), 0, .))

# Remove datasets no longer needed
rm(df.onerecall, df.tworecalls, df.threerecalls, df.fourrecalls, foodgrid_onerecall, foodgrid_tworecalls, foodgrid_threerecalls, foodgrid_fourrecalls, foodgroup_grid, df.numberrecalls)


## Percent energy, fat, saturated fat, free sugars from discretionary food groups

### Pull in mean daily intakes of energy and nutrients 
df.day_nutrients <- df.intake24_recall %>% 
  select(UserID, RecallNo, Day_Energykcal, Day_FreeSugarsg, Day_Fatg, Day_SatFatg)

df.intake24_discretionary_fg <- left_join(df.intake24_discretionary_fg, df.day_nutrients, by=c("UserID", "RecallNo"))

#### Calculate proportion of energy and nutrients from food groups 
df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>%
  mutate(
    Day_Prop_Energykcal_discr=(Day_Energykcal_discr/Day_Energykcal)*100,
    Day_Prop_FreeSugarsg_discr=(Day_FreeSugarsg_discr/Day_FreeSugarsg)*100,
    Day_Prop_Fatg_discr=(Day_Fatg_discr/Day_Fatg)*100,
    Day_Prop_SatFatg_discr=(Day_SatFatg_discr/Day_SatFatg)*100
  ) %>% 
  select(UserID, NumberOfRecalls, RecallNo, DiscretionaryFoodGroupCode, Day_Prop_Energykcal_discr:Day_Prop_SatFatg_discr)  

#Replace NaN (not a number) to 0 - these are instances when daily intake of the nutrient was 0
df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>%
  mutate(across(c(Day_Prop_Energykcal_discr:Day_Prop_SatFatg_discr), as.numeric))

df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>%
  mutate(across(c(Day_Prop_Energykcal_discr:Day_Prop_SatFatg_discr), ~ifelse(is.nan(.), 0, .)))


## Mean participant energy, fat, saturated fat, free sugars from discretionary food groups 

df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>% 
  group_by(UserID, DiscretionaryFoodGroupCode) %>%
  summarise(
    Avg_Prop_Energykcal_discr=mean(Day_Prop_Energykcal_discr),
    Avg_Prop_FreeSugarsg_discr=mean(Day_Prop_FreeSugarsg_discr),
    Avg_Prop_Fatg_discr=mean(Day_Prop_Fatg_discr),
    Avg_Prop_SatFatg_discr=mean(Day_Prop_SatFatg_discr)
  ) %>%
  ungroup()

# pivot to wide format
df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>%
  pivot_wider(
    names_from = DiscretionaryFoodGroupCode,
    values_from = c(Avg_Prop_Energykcal_discr, Avg_Prop_FreeSugarsg_discr, Avg_Prop_Fatg_discr, Avg_Prop_SatFatg_discr)
  )



# Multiple Source Method (MSM) ####
# Code not publicly available. Users may contact Sven Knüppel, Sven.Knueppel@bfr.bund.de for more information. 
# Usual dietary intake can be estimated via the web platform at: https://msm.dife.de/

# Create reporting groups 
df.intake24_recall$Reportinggroup[df.intake24_recall$age_sex_cat == "Female, 2-4y"] = 1 
df.intake24_recall$Reportinggroup[df.intake24_recall$age_sex_cat == "Female, 5-10y"] = 2 
df.intake24_recall$Reportinggroup[df.intake24_recall$age_sex_cat == "Female, 11-15y"] = 3
df.intake24_recall$Reportinggroup[df.intake24_recall$age_sex_cat == "Male, 2-4y"] = 4 
df.intake24_recall$Reportinggroup[df.intake24_recall$age_sex_cat == "Male, 5-10y"] = 5 
df.intake24_recall$Reportinggroup[df.intake24_recall$age_sex_cat == "Male, 11-15y"] = 6


df.intake24_recall <- df.intake24_recall %>%
  mutate(
    Reportinggroup = case_when(
      age_sex_cat == "Female, 2-4y"  ~ 1,
      age_sex_cat == "Female, 5-10y" ~ 2,
      age_sex_cat == "Female, 11-15y" ~ 3,
      age_sex_cat == "Male, 2-4y"    ~ 4,
      age_sex_cat == "Male, 5-10y"   ~ 5,
      age_sex_cat == "Male, 11-15y"  ~ 6,
      TRUE ~ NA_real_ 
    )
  )


# Order by Reportinggroup 
df.intake24_recall <- df.intake24_recall[order(df.intake24_recall$Reportinggroup), ] 

# # Drop 'Prefer not to say' Sex
df.msm <- df.intake24_recall %>%
  filter(!is.na(Reportinggroup))


# Convert tibble to data.frame to avoid errors of matrix vs vector in aggregate in MSM prep 
df.msm <- as.data.frame(df.msm)


## Run MSM functions on nutrients and food groups ####

# *code not publicly available

## tidy the variable names
df.intake24_ui = as.data.frame(MSM_TABLEnut)
colnames(df.intake24_ui) <- gsub("^UI_Day_", "UI_", colnames(df.intake24_ui))

## merge to participant-level data frame 
df.intake24_participant <- left_join(df.intake24_participant, df.intake24_ui, by="UserID")


## Re-calculate variables using MSM values ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    UI_Carbkcal = (UI_Carbohydrateg*3.75),
    UI_FreeSugarskcal = (UI_FreeSugarsg*3.75),
    UI_Fatkcal = (UI_Fatg*9),
    UI_SatFatkcal = (UI_SatFatg*9),
    UI_TransFatKcal = (UI_TransFatg*9),
    UI_Proteinkcal = (UI_Proteing*4),
    
    UI_Carbs_PropFoodEnergy = (UI_Carbkcal/UI_Energykcal)*100,
    UI_FreeSugars_PropFoodEnergy = (UI_FreeSugarskcal/UI_Energykcal)*100,
    UI_Fat_PropFoodEnergy = (UI_Fatkcal/UI_Energykcal)*100,
    UI_SatFat_PropFoodEnergy = (UI_SatFatkcal/UI_Energykcal)*100,
    UI_TransFat_PropFoodEnergy = (UI_TransFatKcal/UI_Energykcal)*100,
    UI_Protein_PropFoodEnergy = (UI_Proteinkcal/UI_Energykcal)*100,
    
    UI_Saltg = ((UI_Sodiummg/1000)*2.498),
    
    UI_FV_JuiceSmoothieg = UI_FV_FruitJuiceg + UI_FV_Smoothieg,
    
    UI_FV_Portions_Beans = case_when(
      Age >= 2 & Age < 11 & UI_FV_Beansg > 40 ~ 1,
      Age >= 2 & Age < 11 & UI_FV_Beansg <= 40 ~ UI_FV_Beansg/40,
      Age >= 11 & Age < 16 & UI_FV_Beansg > 80 ~ 1,
      Age >= 11 & Age < 16 & UI_FV_Beansg <= 80 ~ UI_FV_Beansg/80,
      TRUE ~ NA),
    
    UI_FV_cap_Beans = case_when(
      Age >= 2 & Age < 11 & UI_FV_Beansg > 40 ~ 40,
      Age >= 2 & Age < 11 & UI_FV_Beansg <= 40 ~ UI_FV_Beansg,
      Age >= 11 & Age < 16 & UI_FV_Beansg > 80 ~ 80,
      Age >= 11 & Age < 16 & UI_FV_Beansg <= 80 ~ UI_FV_Beansg,
      TRUE ~ NA),
    
    UI_FV_Portions_JuiceSmoothie = case_when(
      Age >= 2 & Age < 11 & UI_FV_JuiceSmoothieg >75 ~ 1,
      Age >= 2 & Age < 11 & UI_FV_JuiceSmoothieg <= 75 ~ UI_FV_JuiceSmoothieg/75,
      Age >= 11 & Age < 16 & UI_FV_JuiceSmoothieg >150 ~ 1,
      Age >= 11 & Age < 16 & UI_FV_JuiceSmoothieg <= 150 ~ UI_FV_JuiceSmoothieg/150,
      TRUE ~ NA),
    
    UI_FV_cap_JuiceSmoothie = case_when(
      Age >= 2 & Age < 11 & UI_FV_JuiceSmoothieg >75 ~ 75,
      Age >= 2 & Age < 11 & UI_FV_JuiceSmoothieg <= 75 ~ UI_FV_JuiceSmoothieg,
      Age >= 11 & Age < 16 & UI_FV_JuiceSmoothieg >150 ~ 150,
      Age >= 11 & Age < 16 & UI_FV_JuiceSmoothieg <= 150 ~ UI_FV_JuiceSmoothieg,
      TRUE ~ NA),
    
    UI_FV_Portions_DriedFruit = case_when(
      Age >= 2 & Age < 11 ~ UI_FV_DriedFruitg/15,
      Age >= 11 & Age < 16 ~ UI_FV_DriedFruitg/30,
      TRUE ~ NA),
    
    UI_FV_Portions_FreshFruit = case_when(
      Age >= 2 & Age < 11 ~ UI_FV_FreshFruitg/40,
      Age >= 11 & Age < 16 ~ UI_FV_FreshFruitg/80,
      TRUE ~ NA),
    
    UI_FV_Portions_Veg = case_when(
      Age >= 2 & Age < 11 ~ UI_FV_Vegg/40,
      Age >= 11 & Age < 16 ~ UI_FV_Vegg/80,
      TRUE ~ NA),
    
    UI_FV_Portions_Total = UI_FV_Portions_Beans+UI_FV_Portions_JuiceSmoothie+UI_FV_Portions_DriedFruit+UI_FV_Portions_FreshFruit+UI_FV_Portions_Veg,
    UI_FV_Portions_Total_NoJuiceSmoothie = UI_FV_Portions_Beans+UI_FV_Portions_DriedFruit+UI_FV_Portions_FreshFruit+UI_FV_Portions_Veg,
    UI_FV_g_Total = UI_FV_DriedFruitg+UI_FV_FreshFruitg+UI_FV_Vegg+UI_FV_cap_Beans+UI_FV_cap_JuiceSmoothie,
    UI_FV_g_Total_NoJuiceSmoothie = UI_FV_DriedFruitg+UI_FV_FreshFruitg+UI_FV_Vegg+UI_FV_cap_Beans)  %>%
  
  relocate(UI_Carbkcal, UI_Carbs_PropFoodEnergy, .after = UI_Carbohydrateg,) %>%
  relocate(UI_FreeSugarskcal, UI_FreeSugars_PropFoodEnergy, .after = UI_FreeSugarsg) %>%
  relocate(UI_Fatkcal, UI_Fat_PropFoodEnergy, .after = UI_Fatg) %>%
  relocate(UI_SatFatkcal, UI_SatFat_PropFoodEnergy, .after = UI_SatFatg) %>%
  relocate(UI_TransFatKcal, UI_TransFat_PropFoodEnergy, .after = UI_TransFatg) %>%
  relocate(UI_Proteinkcal, UI_Protein_PropFoodEnergy, .after = UI_Proteing) %>%
  relocate(UI_Saltg, .after = UI_Sodiummg)



# Scottish Dietary Goals ####

## Energy density (max 125kcal/100g) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_Energy = case_when(
      Avg_EnergyDensity <= 125 ~ 1, 
      Avg_EnergyDensity > 125 ~ 0,
      TRUE ~ NA)) 

## Total fat (max 35% of food energy) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_Fat = case_when(
      Avg_Fat_PropFoodEnergy <= 35 ~ 1,
      Avg_Fat_PropFoodEnergy > 35 ~ 0,
      TRUE ~ NA),
    UI_SDG_Fat = case_when(
      UI_Fat_PropFoodEnergy <= 35 ~ 1,
      UI_Fat_PropFoodEnergy > 35 ~ 0,
      TRUE ~ NA)) 

## Saturated fat (max 10% of food energy) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_SatFat = case_when(
      Avg_SatFat_PropFoodEnergy <= 10 ~ 1,
      Avg_SatFat_PropFoodEnergy > 10 ~ 0,
      TRUE ~ NA),
    UI_SDG_SatFat = case_when(
      UI_SatFat_PropFoodEnergy <= 10 ~ 1,
      UI_SatFat_PropFoodEnergy > 10 ~ 0,
      TRUE ~ NA)) 

## Trans fat (<1% of food energy) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_TransFat = case_when(
      Avg_TransFat_PropFoodEnergy < 1 ~ 1,
      Avg_TransFat_PropFoodEnergy >= 1 ~ 0,
      TRUE ~ NA),
    UI_SDG_TransFat = case_when(
      UI_TransFat_PropFoodEnergy < 1 ~ 1,
      UI_TransFat_PropFoodEnergy >= 1 ~ 0,
      TRUE ~ NA)) 

## Free sugars (max 5% of food energy) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_FreeSugar = case_when(
      Avg_FreeSugars_PropFoodEnergy <= 5 ~ 1,
      Avg_FreeSugars_PropFoodEnergy > 5 ~ 0,
      TRUE ~ NA),
    UI_SDG_FreeSugar = case_when(
      UI_FreeSugars_PropFoodEnergy <= 5 ~ 1,
      UI_FreeSugars_PropFoodEnergy > 5 ~ 0,
      TRUE ~ NA)) 

## Carbohydrates (~50% of food energy; created upper/lower limits of 45-55%) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_Carbs = case_when(
      Avg_Carbs_PropFoodEnergy >= 45 & Avg_Carbs_PropFoodEnergy <= 55 ~ 1,
      Avg_Carbs_PropFoodEnergy < 45 | Avg_Carbs_PropFoodEnergy > 55 ~ 0,
      TRUE ~ NA),
    UI_SDG_Carbs = case_when(
      UI_Carbs_PropFoodEnergy >= 45 & UI_Carbs_PropFoodEnergy <= 55 ~ 1,
      UI_Carbs_PropFoodEnergy < 45 | UI_Carbs_PropFoodEnergy > 55 ~ 0,
      TRUE ~ NA)) 

## Fibre (2-4y 15g/d; 5-10y 20g/d; 11-15y 25g/d) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_Fibre = case_when(
      Age >= 2 & Age < 5 & Avg_AOACFibreg >= 15 ~ 1,
      Age >= 2 & Age < 5 & Avg_AOACFibreg < 15 ~ 0,
      Age >= 5 & Age < 11 & Avg_AOACFibreg >= 20 ~ 1,
      Age >= 5 & Age < 11 & Avg_AOACFibreg < 20 ~ 0,
      Age >= 11 & Age < 16 & Avg_AOACFibreg >= 25 ~ 1,
      Age >= 11 & Age < 16 & Avg_AOACFibreg < 25 ~ 0,
      TRUE ~ NA),
    UI_SDG_Fibre = case_when(
      Age >= 2 & Age < 5 & UI_AOACFibreg >= 15 ~ 1,
      Age >= 2 & Age < 5 & UI_AOACFibreg < 15 ~ 0,
      Age >= 5 & Age < 11 & UI_AOACFibreg >= 20 ~ 1,
      Age >= 5 & Age < 11 & UI_AOACFibreg < 20 ~ 0,
      Age >= 11 & Age < 16 & UI_AOACFibreg >= 25 ~ 1,
      Age >= 11 & Age < 16 & UI_AOACFibreg < 25 ~ 0,
      TRUE ~ NA)) 

## Salt (2-3y <2g/d; 4-6y <3g/d; 7-10y <5g/d; 11+y <6g/d) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_Salt = case_when(
      Age >= 2 & Age < 4 & Avg_Saltg < 2 ~ 1,
      Age >= 2 & Age < 4 & Avg_Saltg >= 2 ~ 0,
      Age >= 4 & Age < 7 & Avg_Saltg < 3 ~ 1,
      Age >= 4 & Age < 7 & Avg_Saltg >= 3 ~ 0,
      Age >= 7 & Age < 11 & Avg_Saltg < 5 ~ 1,
      Age >= 7 & Age < 11 & Avg_Saltg >= 5 ~ 0,
      Age >= 11 & Avg_Saltg < 6 ~ 1,
      Age >= 11 & Avg_Saltg >= 6 ~ 0,
      TRUE ~ NA),
    UI_SDG_Salt = case_when(
      Age >= 2 & Age < 4 & UI_Saltg < 2 ~ 1,
      Age >= 2 & Age < 4 & UI_Saltg >= 2 ~ 0,
      Age >= 4 & Age < 7 & UI_Saltg < 3 ~ 1,
      Age >= 4 & Age < 7 & UI_Saltg >= 3 ~ 0,
      Age >= 7 & Age < 11 & UI_Saltg < 5 ~ 1,
      Age >= 7 & Age < 11 & UI_Saltg >= 5 ~ 0,
      Age >= 11 & UI_Saltg < 6 ~ 1,
      Age >= 11 & UI_Saltg >= 6 ~ 0,
      TRUE ~ NA))

## Vegetables and fruits (>=5 portions/d) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_FV = case_when(
      Avg_FV_Portions_Total >= 5 ~ 1,
      Avg_FV_Portions_Total < 5 ~ 0,
      TRUE ~ NA),
    SDG_FV_NoJuiceSmoothie = case_when(
      Avg_FV_Portions_Total_NoJuiceSmoothie >= 5 ~ 1,
      Avg_FV_Portions_Total_NoJuiceSmoothie < 5 ~ 0,
      TRUE ~ NA),
    UI_SDG_FV = case_when(
      UI_FV_Portions_Total >= 5 ~ 1,
      UI_FV_Portions_Total < 5 ~ 0,
      TRUE ~ NA),
    UI_SDG_FV_NoJuiceSmoothie = case_when(
      UI_FV_Portions_Total_NoJuiceSmoothie >= 5 ~ 1,
      UI_FV_Portions_Total_NoJuiceSmoothie < 5 ~ 0,
      TRUE ~ NA))

## Red and red processed meat (<=70 g/d) ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_RRPM = case_when(
      Avg_RRPMg <= 70 ~ 1, 
      Avg_RRPMg > 70 ~ 0,
      TRUE ~ NA),
    UI_SDG_RRPM = case_when(
      UI_RRPMg <= 70 ~ 1, 
      UI_RRPMg > 70 ~ 0,
      TRUE ~ NA)) 



## Oily fish ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(SDG_oilyfish = case_when(
    AFreqOilyFish %in% c("Never", "Not sure", "Less than once a week") ~ 0,
    TRUE ~ 1
  ))

df.intake24_participant <- df.intake24_participant %>%
  mutate(SDG_cat_oilyfish = case_when(
    AFreqOilyFish %in% c("Never", "Not sure") ~ "Never",
    AFreqOilyFish == "Less than once a week" ~ "Less than once a week",
    AFreqOilyFish == "Once a week" ~ "Once a week",
    AFreqOilyFish %in% c("Every day", "4-6 times a week", "2-3 times a week") ~ "More than once per week",
    TRUE ~ NA
  ))

## Add values "Yes" or "No" ####
SDG_list <- c("SDG_Energy", "SDG_Fat", "SDG_SatFat", "SDG_TransFat", "SDG_FreeSugar", "SDG_Carbs", "SDG_Fibre", 
              "SDG_Salt", "SDG_FV", "SDG_FV_NoJuiceSmoothie", "SDG_RRPM", "SDG_oilyfish", "UI_SDG_Fat", "UI_SDG_SatFat", "UI_SDG_TransFat",
              "UI_SDG_FreeSugar", "UI_SDG_Carbs", "UI_SDG_Fibre", "UI_SDG_Salt", "UI_SDG_FV", "UI_SDG_FV_NoJuiceSmoothie", "UI_SDG_RRPM")

#set as factors
df.intake24_participant <- df.intake24_participant %>%
  mutate(across(all_of(SDG_list),as.factor))

#recode factor levels
df.intake24_participant <- df.intake24_participant %>%
  mutate(across(all_of(SDG_list), ~ fct_recode(.x, "Yes" = "1", "No" = "0")))


# Adherence to RNIs and LRNIs ####
# 1 = do NOT adhere i.e. below the RNI or below the LRNI

## Energy and EAR ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(UI_EAR_prct_Energy = case_when(Sex == "Male" & Age == 2 ~ (UI_Energykcal/1004)*100,
                                Sex == "Male" & Age == 3 ~ (UI_Energykcal/1171)*100,
                                Sex == "Male" & Age == 4 ~ (UI_Energykcal/1386)*100,
                                Sex == "Male" & Age == 5 ~ (UI_Energykcal/1482)*100,
                                Sex == "Male" & Age == 6 ~ (UI_Energykcal/1577)*100,
                                Sex == "Male" & Age == 7 ~ (UI_Energykcal/1649)*100,
                                Sex == "Male" & Age == 8 ~ (UI_Energykcal/1745)*100,
                                Sex == "Male" & Age == 9 ~ (UI_Energykcal/1840)*100,
                                Sex == "Male" & Age == 10 ~ (UI_Energykcal/2032)*100,
                                Sex == "Male" & Age == 11 ~ (UI_Energykcal/2127)*100,
                                Sex == "Male" & Age == 12 ~ (UI_Energykcal/2247)*100,
                                Sex == "Male" & Age == 13 ~ (UI_Energykcal/2414)*100,
                                Sex == "Male" & Age == 14 ~ (UI_Energykcal/2629)*100,
                                Sex == "Male" & Age == 15 ~ (UI_Energykcal/2820)*100,
                                Sex == "Male" & Age == 16 ~ (UI_Energykcal/2964)*100,
                                Sex == "Male" & Age == 17 ~ (UI_Energykcal/3083)*100,
                                Sex == "Male" & Age == 18 ~ (UI_Energykcal/3155)*100,
                                
                                Sex == "Female" & Age == 2 ~ (UI_Energykcal/932)*100,
                                Sex == "Female" & Age == 3 ~ (UI_Energykcal/1076)*100,
                                Sex == "Female" & Age == 4 ~ (UI_Energykcal/1291)*100,
                                Sex == "Female" & Age == 5 ~ (UI_Energykcal/1362)*100,
                                Sex == "Female" & Age == 6 ~ (UI_Energykcal/1482)*100,
                                Sex == "Female" & Age == 7 ~ (UI_Energykcal/1530)*100,
                                Sex == "Female" & Age == 8 ~ (UI_Energykcal/1625)*100,
                                Sex == "Female" & Age == 9 ~ (UI_Energykcal/1721)*100,
                                Sex == "Female" & Age == 10 ~ (UI_Energykcal/1936)*100,
                                Sex == "Female" & Age == 11 ~ (UI_Energykcal/2032)*100,
                                Sex == "Female" & Age == 12 ~ (UI_Energykcal/2103)*100,
                                Sex == "Female" & Age == 13 ~ (UI_Energykcal/2223)*100,
                                Sex == "Female" & Age == 14 ~ (UI_Energykcal/2342)*100,
                                Sex == "Female" & Age == 15 ~ (UI_Energykcal/2390)*100,
                                Sex == "Female" & Age == 16 ~ (UI_Energykcal/2414)*100,
                                Sex == "Female" & Age == 17 ~ (UI_Energykcal/2462)*100,
                                Sex == "Female" & Age == 18 ~ (UI_Energykcal/2462)*100,
                                
                                TRUE ~ NA_real_)) %>%
           relocate(UI_EAR_prct_Energy, .before = UI_Energykcal)



## Protein ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(RNI_Protein = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Proteing >= 14.5 ~ 0,
                                 Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Proteing >= 19.7 ~ 0,
                                 Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Proteing >= 28.3 ~ 0,
                                 Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Proteing >= 42.1 ~ 0,
                                 Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Proteing >= 55.2 ~ 0,
                                 
                                 Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Proteing < 14.5 ~ 1,
                                 Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Proteing < 19.7 ~ 1,
                                 Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Proteing < 28.3 ~ 1,
                                 Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Proteing < 42.1 ~ 1,
                                 Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Proteing < 55.2 ~ 1,
                                 
                                 Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Proteing >= 14.5 ~ 0,
                                 Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Proteing >= 19.7 ~ 0,
                                 Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Proteing >= 28.3 ~ 0,
                                 Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Proteing >= 41.2 ~ 0,
                                 Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Proteing >= 45.4 ~ 0,
                                 
                                 Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Proteing < 14.5 ~ 1,
                                 Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Proteing < 19.7 ~ 1,
                                 Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Proteing < 28.3 ~ 1,
                                 Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Proteing < 41.2 ~ 1,
                                 Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Proteing < 45.4 ~ 1,
                                 
                                 TRUE ~ NA_integer_),
         
         RNI_prct_Protein = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (Avg_Proteing/14.5)*100,
                                      Sex == "Male" & Age >= 4 & Age <= 6 ~ (Avg_Proteing/19.7)*100,
                                      Sex == "Male" & Age >= 7 & Age <= 10 ~ (Avg_Proteing/28.3)*100,
                                      Sex == "Male" & Age >= 11 & Age <= 14 ~ (Avg_Proteing/42.1)*100,
                                      Sex == "Male" & Age >= 15 & Age <= 18 ~ (Avg_Proteing/55.2)*100,
                                      
                                      Sex == "Female" & Age >= 1 & Age <= 3 ~ (Avg_Proteing/14.5)*100,
                                      Sex == "Female" & Age >= 4 & Age <= 6 ~ (Avg_Proteing/19.7)*100,
                                      Sex == "Female" & Age >= 7 & Age <= 10 ~ (Avg_Proteing/28.3)*100,
                                      Sex == "Female" & Age >= 11 & Age <= 14 ~ (Avg_Proteing/41.2)*100,
                                      Sex == "Female" & Age >= 15 & Age <= 18 ~ (Avg_Proteing/45.4)*100,
                                      
                                      TRUE ~ NA_integer_),
         
         UI_RNI_Protein = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & UI_Proteing >= 14.5 ~ 0,
                                    Sex == "Male" & Age >= 4 & Age <= 6 & UI_Proteing >= 19.7 ~ 0,
                                    Sex == "Male" & Age >= 7 & Age <= 10 & UI_Proteing >= 28.3 ~ 0,
                                    Sex == "Male" & Age >= 11 & Age <= 14 & UI_Proteing >= 42.1 ~ 0,
                                    Sex == "Male" & Age >= 15 & Age <= 18 & UI_Proteing >= 55.2 ~ 0,
                                    
                                    Sex == "Male" & Age >= 1 & Age <= 3 & UI_Proteing < 14.5 ~ 1,
                                    Sex == "Male" & Age >= 4 & Age <= 6 & UI_Proteing < 19.7 ~ 1,
                                    Sex == "Male" & Age >= 7 & Age <= 10 & UI_Proteing < 28.3 ~ 1,
                                    Sex == "Male" & Age >= 11 & Age <= 14 & UI_Proteing < 42.1 ~ 1,
                                    Sex == "Male" & Age >= 15 & Age <= 18 & UI_Proteing < 55.2 ~ 1,
                                    
                                    Sex == "Female" & Age >= 1 & Age <= 3 & UI_Proteing >= 14.5 ~ 0,
                                    Sex == "Female" & Age >= 4 & Age <= 6 & UI_Proteing >= 19.7 ~ 0,
                                    Sex == "Female" & Age >= 7 & Age <= 10 & UI_Proteing >= 28.3 ~ 0,
                                    Sex == "Female" & Age >= 11 & Age <= 14 & UI_Proteing >= 41.2 ~ 0,
                                    Sex == "Female" & Age >= 15 & Age <= 18 & UI_Proteing >= 45.4 ~ 0,
                                    
                                    Sex == "Female" & Age >= 1 & Age <= 3 & UI_Proteing < 14.5 ~ 1,
                                    Sex == "Female" & Age >= 4 & Age <= 6 & UI_Proteing < 19.7 ~ 1,
                                    Sex == "Female" & Age >= 7 & Age <= 10 & UI_Proteing < 28.3 ~ 1,
                                    Sex == "Female" & Age >= 11 & Age <= 14 & UI_Proteing < 41.2 ~ 1,
                                    Sex == "Female" & Age >= 15 & Age <= 18 & UI_Proteing < 45.4 ~ 1,
                                    
                                    TRUE ~ NA_integer_),
         
         UI_RNI_prct_Protein = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (UI_Proteing/14.5)*100,
                                         Sex == "Male" & Age >= 4 & Age <= 6 ~ (UI_Proteing/19.7)*100,
                                         Sex == "Male" & Age >= 7 & Age <= 10 ~ (UI_Proteing/28.3)*100,
                                         Sex == "Male" & Age >= 11 & Age <= 14 ~ (UI_Proteing/42.1)*100,
                                         Sex == "Male" & Age >= 15 & Age <= 18 ~ (UI_Proteing/55.2)*100,
                                         
                                         Sex == "Female" & Age >= 1 & Age <= 3 ~ (UI_Proteing/14.5)*100,
                                         Sex == "Female" & Age >= 4 & Age <= 6 ~ (UI_Proteing/19.7)*100,
                                         Sex == "Female" & Age >= 7 & Age <= 10 ~ (UI_Proteing/28.3)*100,
                                         Sex == "Female" & Age >= 11 & Age <= 14 ~ (UI_Proteing/41.2)*100,
                                         Sex == "Female" & Age >= 15 & Age <= 18 ~ (UI_Proteing/45.4)*100,
                                         
                                         TRUE ~ NA_integer_)
  ) %>%  
  
  
  relocate(RNI_Protein, RNI_prct_Protein, UI_RNI_Protein, UI_RNI_prct_Protein, .after = Avg_Proteing)

## Vitamin A ####
df.intake24_participant <- df.intake24_participant %>%  
  mutate(RNI_VitaminA = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & Avg_VitaminAug >= 400 ~ 0,
                                  Sex == "Male" & Age >= 4 & Age <= 6 & Avg_VitaminAug >= 400 ~ 0,
                                  Sex == "Male" & Age >= 7 & Age <= 10 & Avg_VitaminAug >= 500 ~ 0,
                                  Sex == "Male" & Age >= 11 & Age <= 14 & Avg_VitaminAug >= 600 ~ 0,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & Avg_VitaminAug >= 700 ~ 0,
                                  
                                  Sex == "Male" & Age >= 1 & Age <= 3 & Avg_VitaminAug < 400 ~ 1,
                                  Sex == "Male" & Age >= 4 & Age <= 6 & Avg_VitaminAug < 400 ~ 1,
                                  Sex == "Male" & Age >= 7 & Age <= 10 & Avg_VitaminAug < 500 ~ 1,
                                  Sex == "Male" & Age >= 11 & Age <= 14 & Avg_VitaminAug < 600 ~ 1,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & Avg_VitaminAug < 700 ~ 1,
                                  
                                  Sex == "Female" & Age >= 1 & Age <= 3 & Avg_VitaminAug >= 400 ~ 0,
                                  Sex == "Female" & Age >= 4 & Age <= 6 & Avg_VitaminAug >= 400 ~ 0,
                                  Sex == "Female" & Age >= 7 & Age <= 10 & Avg_VitaminAug >= 500 ~ 0,
                                  Sex == "Female" & Age >= 11 & Age <= 14 & Avg_VitaminAug >= 600 ~ 0,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & Avg_VitaminAug >= 600 ~ 0,
                                  
                                  Sex == "Female" & Age >= 1 & Age <= 3 & Avg_VitaminAug < 400 ~ 1,
                                  Sex == "Female" & Age >= 4 & Age <= 6 & Avg_VitaminAug < 400 ~ 1,
                                  Sex == "Female" & Age >= 7 & Age <= 10 & Avg_VitaminAug < 500 ~ 1,
                                  Sex == "Female" & Age >= 11 & Age <= 14 & Avg_VitaminAug < 600 ~ 1,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & Avg_VitaminAug < 600 ~ 1,
                                  
                                  TRUE ~ NA_integer_),
         
         LRNI_VitaminA = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & Avg_VitaminAug >= 200 ~ 0,
                                   Sex == "Male" & Age >= 4 & Age <= 6 & Avg_VitaminAug >= 200 ~ 0,
                                   Sex == "Male" & Age >= 7 & Age <= 10 & Avg_VitaminAug >= 250 ~ 0,
                                   Sex == "Male" & Age >= 11 & Age <= 14 & Avg_VitaminAug >= 250 ~ 0,
                                   Sex == "Male" & Age >= 15 & Age <= 18 & Avg_VitaminAug >= 300 ~ 0,
                                   
                                   Sex == "Male" & Age >= 1 & Age <= 3 & Avg_VitaminAug < 200 ~ 1,
                                   Sex == "Male" & Age >= 4 & Age <= 6 & Avg_VitaminAug < 200 ~ 1,
                                   Sex == "Male" & Age >= 7 & Age <= 10 & Avg_VitaminAug < 250 ~ 1,
                                   Sex == "Male" & Age >= 11 & Age <= 14 & Avg_VitaminAug < 250 ~ 1,
                                   Sex == "Male" & Age >= 15 & Age <= 18 & Avg_VitaminAug < 300 ~ 1,
                                   
                                   Sex == "Female" & Age >= 1 & Age <= 3 & Avg_VitaminAug >= 200 ~ 0,
                                   Sex == "Female" & Age >= 4 & Age <= 6 & Avg_VitaminAug >= 200 ~ 0,
                                   Sex == "Female" & Age >= 7 & Age <= 10 & Avg_VitaminAug >= 250 ~ 0,
                                   Sex == "Female" & Age >= 11 & Age <= 14 & Avg_VitaminAug >= 250 ~ 0,
                                   Sex == "Female" & Age >= 15 & Age <= 18 & Avg_VitaminAug >= 250 ~ 0,
                                   
                                   Sex == "Female" & Age >= 1 & Age <= 3 & Avg_VitaminAug < 200 ~ 1,
                                   Sex == "Female" & Age >= 4 & Age <= 6 & Avg_VitaminAug < 200 ~ 1,
                                   Sex == "Female" & Age >= 7 & Age <= 10 & Avg_VitaminAug < 250 ~ 1,
                                   Sex == "Female" & Age >= 11 & Age <= 14 & Avg_VitaminAug < 250 ~ 1,
                                   Sex == "Female" & Age >= 15 & Age <= 18 & Avg_VitaminAug < 250 ~ 1,
                                   
                                   TRUE ~ NA_integer_),
         
         RNI_prct_VitaminA = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (Avg_VitaminAug/400)*100,
                                       Sex == "Male" & Age >= 4 & Age <= 6 ~ (Avg_VitaminAug/400)*100,
                                       Sex == "Male" & Age >= 7 & Age <= 10 ~ (Avg_VitaminAug/500)*100,
                                       Sex == "Male" & Age >= 11 & Age <= 14 ~ (Avg_VitaminAug/600)*100,
                                       Sex == "Male" & Age >= 15 & Age <= 18 ~ (Avg_VitaminAug/700)*100,
                                       
                                       Sex == "Female" & Age >= 1 & Age <= 3 ~ (Avg_VitaminAug/400)*100,
                                       Sex == "Female" & Age >= 4 & Age <= 6 ~ (Avg_VitaminAug/400)*100,
                                       Sex == "Female" & Age >= 7 & Age <= 10 ~ (Avg_VitaminAug/500)*100,
                                       Sex == "Female" & Age >= 11 & Age <= 14 ~ (Avg_VitaminAug/600)*100,
                                       Sex == "Female" & Age >= 15 & Age <= 18 ~ (Avg_VitaminAug/600)*100,
                                       
                                       TRUE ~ NA_integer_),
         
         UI_RNI_VitaminA = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & UI_VitaminAug >= 400 ~ 0,
                                     Sex == "Male" & Age >= 4 & Age <= 6 & UI_VitaminAug >= 400 ~ 0,
                                     Sex == "Male" & Age >= 7 & Age <= 10 & UI_VitaminAug >= 500 ~ 0,
                                     Sex == "Male" & Age >= 11 & Age <= 14 & UI_VitaminAug >= 600 ~ 0,
                                     Sex == "Male" & Age >= 15 & Age <= 18 & UI_VitaminAug >= 700 ~ 0,
                                     
                                     Sex == "Male" & Age >= 1 & Age <= 3 & UI_VitaminAug < 400 ~ 1,
                                     Sex == "Male" & Age >= 4 & Age <= 6 & UI_VitaminAug < 400 ~ 1,
                                     Sex == "Male" & Age >= 7 & Age <= 10 & UI_VitaminAug < 500 ~ 1,
                                     Sex == "Male" & Age >= 11 & Age <= 14 & UI_VitaminAug < 600 ~ 1,
                                     Sex == "Male" & Age >= 15 & Age <= 18 & UI_VitaminAug < 700 ~ 1,
                                     
                                     Sex == "Female" & Age >= 1 & Age <= 3 & UI_VitaminAug >= 400 ~ 0,
                                     Sex == "Female" & Age >= 4 & Age <= 6 & UI_VitaminAug >= 400 ~ 0,
                                     Sex == "Female" & Age >= 7 & Age <= 10 & UI_VitaminAug >= 500 ~ 0,
                                     Sex == "Female" & Age >= 11 & Age <= 14 & UI_VitaminAug >= 600 ~ 0,
                                     Sex == "Female" & Age >= 15 & Age <= 18 & UI_VitaminAug >= 600 ~ 0,
                                     
                                     Sex == "Female" & Age >= 1 & Age <= 3 & UI_VitaminAug < 400 ~ 1,
                                     Sex == "Female" & Age >= 4 & Age <= 6 & UI_VitaminAug < 400 ~ 1,
                                     Sex == "Female" & Age >= 7 & Age <= 10 & UI_VitaminAug < 500 ~ 1,
                                     Sex == "Female" & Age >= 11 & Age <= 14 & UI_VitaminAug < 600 ~ 1,
                                     Sex == "Female" & Age >= 15 & Age <= 18 & UI_VitaminAug < 600 ~ 1,
                                     
                                     TRUE ~ NA_integer_),
         
         UI_LRNI_VitaminA = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & UI_VitaminAug >= 200 ~ 0,
                                      Sex == "Male" & Age >= 4 & Age <= 6 & UI_VitaminAug >= 200 ~ 0,
                                      Sex == "Male" & Age >= 7 & Age <= 10 & UI_VitaminAug >= 250 ~ 0,
                                      Sex == "Male" & Age >= 11 & Age <= 14 & UI_VitaminAug >= 250 ~ 0,
                                      Sex == "Male" & Age >= 15 & Age <= 18 & UI_VitaminAug >= 300 ~ 0,
                                      
                                      Sex == "Male" & Age >= 1 & Age <= 3 & UI_VitaminAug < 200 ~ 1,
                                      Sex == "Male" & Age >= 4 & Age <= 6 & UI_VitaminAug < 200 ~ 1,
                                      Sex == "Male" & Age >= 7 & Age <= 10 & UI_VitaminAug < 250 ~ 1,
                                      Sex == "Male" & Age >= 11 & Age <= 14 & UI_VitaminAug < 250 ~ 1,
                                      Sex == "Male" & Age >= 15 & Age <= 18 & UI_VitaminAug < 300 ~ 1,
                                      
                                      Sex == "Female" & Age >= 1 & Age <= 3 & UI_VitaminAug >= 200 ~ 0,
                                      Sex == "Female" & Age >= 4 & Age <= 6 & UI_VitaminAug >= 200 ~ 0,
                                      Sex == "Female" & Age >= 7 & Age <= 10 & UI_VitaminAug >= 250 ~ 0,
                                      Sex == "Female" & Age >= 11 & Age <= 14 & UI_VitaminAug >= 250 ~ 0,
                                      Sex == "Female" & Age >= 15 & Age <= 18 & UI_VitaminAug >= 250 ~ 0,
                                      
                                      Sex == "Female" & Age >= 1 & Age <= 3 & UI_VitaminAug < 200 ~ 1,
                                      Sex == "Female" & Age >= 4 & Age <= 6 & UI_VitaminAug < 200 ~ 1,
                                      Sex == "Female" & Age >= 7 & Age <= 10 & UI_VitaminAug < 250 ~ 1,
                                      Sex == "Female" & Age >= 11 & Age <= 14 & UI_VitaminAug < 250 ~ 1,
                                      Sex == "Female" & Age >= 15 & Age <= 18 & UI_VitaminAug < 250 ~ 1,
                                      
                                      TRUE ~ NA_integer_),
         
         UI_RNI_prct_VitaminA = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (UI_VitaminAug/400)*100,
                                          Sex == "Male" & Age >= 4 & Age <= 6 ~ (UI_VitaminAug/400)*100,
                                          Sex == "Male" & Age >= 7 & Age <= 10 ~ (UI_VitaminAug/500)*100,
                                          Sex == "Male" & Age >= 11 & Age <= 14 ~ (UI_VitaminAug/600)*100,
                                          Sex == "Male" & Age >= 15 & Age <= 18 ~ (UI_VitaminAug/700)*100,
                                          
                                          Sex == "Female" & Age >= 1 & Age <= 3 ~ (UI_VitaminAug/400)*100,
                                          Sex == "Female" & Age >= 4 & Age <= 6 ~ (UI_VitaminAug/400)*100,
                                          Sex == "Female" & Age >= 7 & Age <= 10 ~ (UI_VitaminAug/500)*100,
                                          Sex == "Female" & Age >= 11 & Age <= 14 ~ (UI_VitaminAug/600)*100,
                                          Sex == "Female" & Age >= 15 & Age <= 18 ~ (UI_VitaminAug/600)*100,
                                          
                                          TRUE ~ NA_integer_)
  ) %>%  
  
  relocate(RNI_VitaminA, LRNI_VitaminA, RNI_prct_VitaminA, UI_RNI_VitaminA, UI_LRNI_VitaminA, UI_RNI_prct_VitaminA, .after = Avg_VitaminAug)

## Riboflavin ####
df.intake24_participant <- df.intake24_participant %>%  
  mutate(RNI_Riboflavin = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Riboflavinmg >= 0.6 ~ 0,
                                    Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Riboflavinmg >= 0.8 ~ 0,
                                    Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Riboflavinmg >= 1.0 ~ 0,
                                    Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Riboflavinmg >= 1.2 ~ 0,
                                    Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Riboflavinmg >= 1.3 ~ 0,
                                    
                                    Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Riboflavinmg < 0.6 ~ 1,
                                    Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Riboflavinmg < 0.8 ~ 1,
                                    Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Riboflavinmg < 1.0 ~ 1,
                                    Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Riboflavinmg < 1.2 ~ 1,
                                    Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Riboflavinmg < 1.3 ~ 1,
                                    
                                    Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Riboflavinmg >= 0.6 ~ 0,
                                    Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Riboflavinmg >= 0.8 ~ 0,
                                    Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Riboflavinmg >= 1.0 ~ 0,
                                    Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Riboflavinmg >= 1.1 ~ 0,
                                    Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Riboflavinmg >= 1.1 ~ 0,
                                    
                                    Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Riboflavinmg < 0.6 ~ 1,
                                    Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Riboflavinmg < 0.8 ~ 1,
                                    Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Riboflavinmg < 1.0 ~ 1,
                                    Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Riboflavinmg < 1.1 ~ 1,
                                    Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Riboflavinmg < 1.1 ~ 1,
                                    
                                    TRUE ~ NA_integer_),
         
         LRNI_Riboflavin = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Riboflavinmg >= 0.3 ~ 0,
                                     Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Riboflavinmg >= 0.4 ~ 0,
                                     Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Riboflavinmg >= 0.5 ~ 0,
                                     Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Riboflavinmg >= 0.8 ~ 0,
                                     Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Riboflavinmg >= 0.8 ~ 0,
                                     
                                     Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Riboflavinmg < 0.3 ~ 1,
                                     Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Riboflavinmg < 0.4 ~ 1,
                                     Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Riboflavinmg < 0.5 ~ 1,
                                     Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Riboflavinmg < 0.8 ~ 1,
                                     Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Riboflavinmg < 0.8 ~ 1,
                                     
                                     Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Riboflavinmg >= 0.3 ~ 0,
                                     Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Riboflavinmg >= 0.4 ~ 0,
                                     Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Riboflavinmg >= 0.5 ~ 0,
                                     Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Riboflavinmg >= 0.8 ~ 0,
                                     Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Riboflavinmg >= 0.8 ~ 0,
                                     
                                     Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Riboflavinmg < 0.3 ~ 1,
                                     Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Riboflavinmg < 0.4 ~ 1,
                                     Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Riboflavinmg < 0.5 ~ 1,
                                     Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Riboflavinmg < 0.8 ~ 1,
                                     Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Riboflavinmg < 0.8 ~ 1,
                                     
                                     TRUE ~ NA_integer_),
         
         RNI_prct_Riboflavin = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (Avg_Riboflavinmg/0.6)*100,
                                         Sex == "Male" & Age >= 4 & Age <= 6 ~ (Avg_Riboflavinmg/0.8)*100,
                                         Sex == "Male" & Age >= 7 & Age <= 10 ~ (Avg_Riboflavinmg/1.0)*100,
                                         Sex == "Male" & Age >= 11 & Age <= 14 ~ (Avg_Riboflavinmg/1.2)*100,
                                         Sex == "Male" & Age >= 15 & Age <= 18 ~ (Avg_Riboflavinmg/1.3)*100,
                                         
                                         Sex == "Female" & Age >= 1 & Age <= 3 ~ (Avg_Riboflavinmg/0.6)*100,
                                         Sex == "Female" & Age >= 4 & Age <= 6 ~ (Avg_Riboflavinmg/0.8)*100,
                                         Sex == "Female" & Age >= 7 & Age <= 10 ~ (Avg_Riboflavinmg/1.0)*100,
                                         Sex == "Female" & Age >= 11 & Age <= 14 ~ (Avg_Riboflavinmg/1.1)*100,
                                         Sex == "Female" & Age >= 15 & Age <= 18 ~ (Avg_Riboflavinmg/1.1)*100,
                                         
                                         TRUE ~ NA_integer_),
         
         UI_RNI_Riboflavin = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & UI_Riboflavinmg >= 0.6 ~ 0,
                                       Sex == "Male" & Age >= 4 & Age <= 6 & UI_Riboflavinmg >= 0.8 ~ 0,
                                       Sex == "Male" & Age >= 7 & Age <= 10 & UI_Riboflavinmg >= 1.0 ~ 0,
                                       Sex == "Male" & Age >= 11 & Age <= 14 & UI_Riboflavinmg >= 1.2 ~ 0,
                                       Sex == "Male" & Age >= 15 & Age <= 18 & UI_Riboflavinmg >= 1.3 ~ 0,
                                       
                                       Sex == "Male" & Age >= 1 & Age <= 3 & UI_Riboflavinmg < 0.6 ~ 1,
                                       Sex == "Male" & Age >= 4 & Age <= 6 & UI_Riboflavinmg < 0.8 ~ 1,
                                       Sex == "Male" & Age >= 7 & Age <= 10 & UI_Riboflavinmg < 1.0 ~ 1,
                                       Sex == "Male" & Age >= 11 & Age <= 14 & UI_Riboflavinmg < 1.2 ~ 1,
                                       Sex == "Male" & Age >= 15 & Age <= 18 & UI_Riboflavinmg < 1.3 ~ 1,
                                       
                                       Sex == "Female" & Age >= 1 & Age <= 3 & UI_Riboflavinmg >= 0.6 ~ 0,
                                       Sex == "Female" & Age >= 4 & Age <= 6 & UI_Riboflavinmg >= 0.8 ~ 0,
                                       Sex == "Female" & Age >= 7 & Age <= 10 & UI_Riboflavinmg >= 1.0 ~ 0,
                                       Sex == "Female" & Age >= 11 & Age <= 14 & UI_Riboflavinmg >= 1.1 ~ 0,
                                       Sex == "Female" & Age >= 15 & Age <= 18 & UI_Riboflavinmg >= 1.1 ~ 0,
                                       
                                       Sex == "Female" & Age >= 1 & Age <= 3 & UI_Riboflavinmg < 0.6 ~ 1,
                                       Sex == "Female" & Age >= 4 & Age <= 6 & UI_Riboflavinmg < 0.8 ~ 1,
                                       Sex == "Female" & Age >= 7 & Age <= 10 & UI_Riboflavinmg < 1.0 ~ 1,
                                       Sex == "Female" & Age >= 11 & Age <= 14 & UI_Riboflavinmg < 1.1 ~ 1,
                                       Sex == "Female" & Age >= 15 & Age <= 18 & UI_Riboflavinmg < 1.1 ~ 1,
                                       
                                       TRUE ~ NA_integer_),
         
         UI_LRNI_Riboflavin = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & UI_Riboflavinmg >= 0.3 ~ 0,
                                        Sex == "Male" & Age >= 4 & Age <= 6 & UI_Riboflavinmg >= 0.4 ~ 0,
                                        Sex == "Male" & Age >= 7 & Age <= 10 & UI_Riboflavinmg >= 0.5 ~ 0,
                                        Sex == "Male" & Age >= 11 & Age <= 14 & UI_Riboflavinmg >= 0.8 ~ 0,
                                        Sex == "Male" & Age >= 15 & Age <= 18 & UI_Riboflavinmg >= 0.8 ~ 0,
                                        
                                        Sex == "Male" & Age >= 1 & Age <= 3 & UI_Riboflavinmg < 0.3 ~ 1,
                                        Sex == "Male" & Age >= 4 & Age <= 6 & UI_Riboflavinmg < 0.4 ~ 1,
                                        Sex == "Male" & Age >= 7 & Age <= 10 & UI_Riboflavinmg < 0.5 ~ 1,
                                        Sex == "Male" & Age >= 11 & Age <= 14 & UI_Riboflavinmg < 0.8 ~ 1,
                                        Sex == "Male" & Age >= 15 & Age <= 18 & UI_Riboflavinmg < 0.8 ~ 1,
                                        
                                        Sex == "Female" & Age >= 1 & Age <= 3 & UI_Riboflavinmg >= 0.3 ~ 0,
                                        Sex == "Female" & Age >= 4 & Age <= 6 & UI_Riboflavinmg >= 0.4 ~ 0,
                                        Sex == "Female" & Age >= 7 & Age <= 10 & UI_Riboflavinmg >= 0.5 ~ 0,
                                        Sex == "Female" & Age >= 11 & Age <= 14 & UI_Riboflavinmg >= 0.8 ~ 0,
                                        Sex == "Female" & Age >= 15 & Age <= 18 & UI_Riboflavinmg >= 0.8 ~ 0,
                                        
                                        Sex == "Female" & Age >= 1 & Age <= 3 & UI_Riboflavinmg < 0.3 ~ 1,
                                        Sex == "Female" & Age >= 4 & Age <= 6 & UI_Riboflavinmg < 0.4 ~ 1,
                                        Sex == "Female" & Age >= 7 & Age <= 10 & UI_Riboflavinmg < 0.5 ~ 1,
                                        Sex == "Female" & Age >= 11 & Age <= 14 & UI_Riboflavinmg < 0.8 ~ 1,
                                        Sex == "Female" & Age >= 15 & Age <= 18 & UI_Riboflavinmg < 0.8 ~ 1,
                                        
                                        TRUE ~ NA_integer_),
         
         UI_RNI_prct_Riboflavin = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (UI_Riboflavinmg/0.6)*100,
                                            Sex == "Male" & Age >= 4 & Age <= 6 ~ (UI_Riboflavinmg/0.8)*100,
                                            Sex == "Male" & Age >= 7 & Age <= 10 ~ (UI_Riboflavinmg/1.0)*100,
                                            Sex == "Male" & Age >= 11 & Age <= 14 ~ (UI_Riboflavinmg/1.2)*100,
                                            Sex == "Male" & Age >= 15 & Age <= 18 ~ (UI_Riboflavinmg/1.3)*100,
                                            
                                            Sex == "Female" & Age >= 1 & Age <= 3 ~ (UI_Riboflavinmg/0.6)*100,
                                            Sex == "Female" & Age >= 4 & Age <= 6 ~ (UI_Riboflavinmg/0.8)*100,
                                            Sex == "Female" & Age >= 7 & Age <= 10 ~ (UI_Riboflavinmg/1.0)*100,
                                            Sex == "Female" & Age >= 11 & Age <= 14 ~ (UI_Riboflavinmg/1.1)*100,
                                            Sex == "Female" & Age >= 15 & Age <= 18 ~ (UI_Riboflavinmg/1.1)*100,
                                            
                                            TRUE ~ NA_integer_)
  ) %>%  
  
  relocate(RNI_Riboflavin, LRNI_Riboflavin, RNI_prct_Riboflavin, UI_RNI_Riboflavin, UI_LRNI_Riboflavin, UI_RNI_prct_Riboflavin, .after = Avg_Riboflavinmg)

## Folate ####
df.intake24_participant <- df.intake24_participant %>%  
  mutate(RNI_Folate = case_when(Age >= 1 & Age <= 3 & Avg_Folateug >= 70 ~ 0,
                                Age >= 4 & Age <= 6 & Avg_Folateug >= 100 ~ 0,
                                Age >= 7 & Age <= 10 & Avg_Folateug >= 150 ~ 0,
                                Age >= 11 & Age <= 14 & Avg_Folateug >= 200 ~ 0,
                                Age >= 15 & Age <= 18 & Avg_Folateug >= 200 ~ 0,
                                
                                Age >= 1 & Age <= 3 & Avg_Folateug < 70 ~ 1,
                                Age >= 4 & Age <= 6 & Avg_Folateug < 100 ~ 1,
                                Age >= 7 & Age <= 10 & Avg_Folateug < 150 ~ 1,
                                Age >= 11 & Age <= 14 & Avg_Folateug < 200 ~ 1,
                                Age >= 15 & Age <= 18 & Avg_Folateug < 200 ~ 1,
                                
                                TRUE ~ NA_integer_),
         
         LRNI_Folate = case_when(Age >= 1 & Age <= 3 & Avg_Folateug >= 35 ~ 0,
                                 Age >= 4 & Age <= 6 & Avg_Folateug >= 50 ~ 0,
                                 Age >= 7 & Age <= 10 & Avg_Folateug >= 75 ~ 0,
                                 Age >= 11 & Age <= 14 & Avg_Folateug >= 100 ~ 0,
                                 Age >= 15 & Age <= 18 & Avg_Folateug >= 100 ~ 0,
                                 
                                 Age >= 1 & Age <= 3 & Avg_Folateug < 35 ~ 1,
                                 Age >= 4 & Age <= 6 & Avg_Folateug < 50 ~ 1,
                                 Age >= 7 & Age <= 10 & Avg_Folateug < 75 ~ 1,
                                 Age >= 11 & Age <= 14 & Avg_Folateug < 100 ~ 1,
                                 Age >= 15 & Age <= 18 & Avg_Folateug < 100 ~ 1,
                                 
                                 TRUE ~ NA_integer_),
         
         RNI_prct_Folate = case_when(Age >= 1 & Age <= 3 ~ (Avg_Folateug/70)*100,
                                     Age >= 4 & Age <= 6 ~ (Avg_Folateug/100)*100,
                                     Age >= 7 & Age <= 10 ~ (Avg_Folateug/150)*100,
                                     Age >= 11 & Age <= 14 ~ (Avg_Folateug/200)*100,
                                     Age >= 15 & Age <= 18 ~ (Avg_Folateug/200)*100,
                                     
                                     TRUE ~ NA_real_),
         
         UI_RNI_Folate = case_when(Age >= 1 & Age <= 3 & UI_Folateug >= 70 ~ 0,
                                   Age >= 4 & Age <= 6 & UI_Folateug >= 100 ~ 0,
                                   Age >= 7 & Age <= 10 & UI_Folateug >= 150 ~ 0,
                                   Age >= 11 & Age <= 14 & UI_Folateug >= 200 ~ 0,
                                   Age >= 15 & Age <= 18 & UI_Folateug >= 200 ~ 0,
                                   
                                   Age >= 1 & Age <= 3 & UI_Folateug < 70 ~ 1,
                                   Age >= 4 & Age <= 6 & UI_Folateug < 100 ~ 1,
                                   Age >= 7 & Age <= 10 & UI_Folateug < 150 ~ 1,
                                   Age >= 11 & Age <= 14 & UI_Folateug < 200 ~ 1,
                                   Age >= 15 & Age <= 18 & UI_Folateug < 200 ~ 1,
                                   
                                   TRUE ~ NA_integer_),
         
         UI_LRNI_Folate = case_when(Age >= 1 & Age <= 3 & UI_Folateug >= 35 ~ 0,
                                    Age >= 4 & Age <= 6 & UI_Folateug >= 50 ~ 0,
                                    Age >= 7 & Age <= 10 & UI_Folateug >= 75 ~ 0,
                                    Age >= 11 & Age <= 14 & UI_Folateug >= 100 ~ 0,
                                    Age >= 15 & Age <= 18 & UI_Folateug >= 100 ~ 0,
                                    
                                    Age >= 1 & Age <= 3 & UI_Folateug < 35 ~ 1,
                                    Age >= 4 & Age <= 6 & UI_Folateug < 50 ~ 1,
                                    Age >= 7 & Age <= 10 & UI_Folateug < 75 ~ 1,
                                    Age >= 11 & Age <= 14 & UI_Folateug < 100 ~ 1,
                                    Age >= 15 & Age <= 18 & UI_Folateug < 100 ~ 1,
                                    
                                    TRUE ~ NA_integer_),
         
         UI_RNI_prct_Folate = case_when(Age >= 1 & Age <= 3 ~ (UI_Folateug/70)*100,
                                        Age >= 4 & Age <= 6 ~ (UI_Folateug/100)*100,
                                        Age >= 7 & Age <= 10 ~ (UI_Folateug/150)*100,
                                        Age >= 11 & Age <= 14 ~ (UI_Folateug/200)*100,
                                        Age >= 15 & Age <= 18 ~ (UI_Folateug/200)*100,
                                        
                                        TRUE ~ NA_real_)
  ) %>%  
  
  relocate(RNI_Folate, LRNI_Folate, RNI_prct_Folate, UI_RNI_Folate, UI_LRNI_Folate, UI_RNI_prct_Folate, .after = Avg_Folateug)

## Vitamin D ####
df.intake24_participant <- df.intake24_participant %>%  
  mutate(RNI_VitaminD = case_when(Age >= 1 & Age <= 3 & Avg_VitaminDug >= 10 ~ 0,
                                  Age >= 4 & Age <= 6 & Avg_VitaminDug >= 10 ~ 0,
                                  Age >= 7 & Age <= 10 & Avg_VitaminDug >= 10 ~ 0,
                                  Age >= 11 & Age <= 14 & Avg_VitaminDug >= 10 ~ 0,
                                  Age >= 15 & Age <= 18 & Avg_VitaminDug >= 10 ~ 0,
                                  
                                  Age >= 1 & Age <= 3 & Avg_VitaminDug < 10 ~ 1,
                                  Age >= 4 & Age <= 6 & Avg_VitaminDug < 10 ~ 1,
                                  Age >= 7 & Age <= 10 & Avg_VitaminDug < 10 ~ 1,
                                  Age >= 11 & Age <= 14 & Avg_VitaminDug < 10 ~ 1,
                                  Age >= 15 & Age <= 18 & Avg_VitaminDug < 10 ~ 1,
                                  TRUE ~ NA_integer_),
         
         RNI_prct_VitaminD = case_when(Age >= 1 & Age <= 3 ~ (Avg_VitaminDug/10)*100,
                                       Age >= 4 & Age <= 6 ~ (Avg_VitaminDug/10)*100,
                                       Age >= 7 & Age <= 10 ~ (Avg_VitaminDug/10)*100,
                                       Age >= 11 & Age <= 14 ~ (Avg_VitaminDug/10)*100,
                                       Age >= 15 & Age <= 18 ~ (Avg_VitaminDug/10)*100,
                                       
                                       TRUE ~ NA_integer_), 
         
         UI_RNI_VitaminD = case_when(Age >= 1 & Age <= 3 & UI_VitaminDug >= 10 ~ 0,
                                     Age >= 4 & Age <= 6 & UI_VitaminDug >= 10 ~ 0,
                                     Age >= 7 & Age <= 10 & UI_VitaminDug >= 10 ~ 0,
                                     Age >= 11 & Age <= 14 & UI_VitaminDug >= 10 ~ 0,
                                     Age >= 15 & Age <= 18 & UI_VitaminDug >= 10 ~ 0,
                                     
                                     Age >= 1 & Age <= 3 & UI_VitaminDug < 10 ~ 1,
                                     Age >= 4 & Age <= 6 & UI_VitaminDug < 10 ~ 1,
                                     Age >= 7 & Age <= 10 & UI_VitaminDug < 10 ~ 1,
                                     Age >= 11 & Age <= 14 & UI_VitaminDug < 10 ~ 1,
                                     Age >= 15 & Age <= 18 & UI_VitaminDug < 10 ~ 1,
                                     TRUE ~ NA_integer_),
         
         UI_RNI_prct_VitaminD = case_when(Age >= 1 & Age <= 3 ~ (UI_VitaminDug/10)*100,
                                          Age >= 4 & Age <= 6 ~ (UI_VitaminDug/10)*100,
                                          Age >= 7 & Age <= 10 ~ (UI_VitaminDug/10)*100,
                                          Age >= 11 & Age <= 14 ~ (UI_VitaminDug/10)*100,
                                          Age >= 15 & Age <= 18 ~ (UI_VitaminDug/10)*100,
                                          
                                          TRUE ~ NA_integer_)
  ) %>%  
  
  relocate(RNI_VitaminD, RNI_prct_VitaminD, UI_RNI_VitaminD, UI_RNI_prct_VitaminD, .after = Avg_VitaminDug)

## Vitamin b12 ####
df.intake24_participant <- df.intake24_participant %>%  
  mutate(RNI_VitaminB12 = case_when(Age >= 1 & Age <= 3 & Avg_VitaminB12ug >= 0.5 ~ 0,
                                    Age >= 4 & Age <= 6 & Avg_VitaminB12ug >= 0.8 ~ 0,
                                    Age >= 7 & Age <= 10 & Avg_VitaminB12ug >= 1.0 ~ 0,
                                    Age >= 11 & Age <= 14 & Avg_VitaminB12ug >= 1.2 ~ 0,
                                    Age >= 15 & Age <= 18 & Avg_VitaminB12ug >= 1.5 ~ 0,
                                    
                                    Age >= 1 & Age <= 3 & Avg_VitaminB12ug < 0.5 ~ 1,
                                    Age >= 4 & Age <= 6 & Avg_VitaminB12ug < 0.8 ~ 1,
                                    Age >= 7 & Age <= 10 & Avg_VitaminB12ug < 1.0 ~ 1,
                                    Age >= 11 & Age <= 14 & Avg_VitaminB12ug < 1.2 ~ 1,
                                    Age >= 15 & Age <= 18 & Avg_VitaminB12ug < 1.5 ~ 1,
                                    
                                    TRUE ~ NA_integer_),
         
         LRNI_VitaminB12 = case_when(Age >= 1 & Age <= 3 & Avg_VitaminB12ug >= 0.3 ~ 0, 
                                     Age >= 4 & Age <= 6 & Avg_VitaminB12ug >= 0.5 ~ 0,
                                     Age >= 7 & Age <= 10 & Avg_VitaminB12ug >= 0.6 ~ 0,
                                     Age >= 11 & Age <= 14 & Avg_VitaminB12ug >= 0.8 ~ 0,
                                     Age >= 15 & Age <=18 & Avg_VitaminB12ug >= 1.0 ~ 0,
                                     
                                     Age >= 1 & Age <= 3 & Avg_VitaminB12ug < 0.3 ~ 1,
                                     Age >= 4 & Age <= 6 & Avg_VitaminB12ug < 0.5 ~ 1,
                                     Age >= 7 & Age <= 10 & Avg_VitaminB12ug < 0.6 ~ 1,
                                     Age >= 11 & Age <= 14 & Avg_VitaminB12ug < 0.8 ~ 1,
                                     Age >= 15 & Age <=18 & Avg_VitaminB12ug < 1.0 ~ 1,
                                     
                                     TRUE ~ NA_integer_),
         
         RNI_prct_VitaminB12 = case_when(Age >= 1 & Age <= 3 ~ (Avg_VitaminB12ug/0.5)*100,
                                         Age >= 4 & Age <= 6 ~ (Avg_VitaminB12ug/0.8)*100,
                                         Age >= 7 & Age <= 10 ~ (Avg_VitaminB12ug/1.0)*100,
                                         Age >= 11 & Age <= 14 ~ (Avg_VitaminB12ug/1.2)*100,
                                         Age >= 15 & Age <= 18 ~ (Avg_VitaminB12ug/1.5)*100,
                                         
                                         TRUE ~ NA_integer_),
         
         UI_RNI_VitaminB12 = case_when(Age >= 1 & Age <= 3 & UI_VitaminB12ug >= 0.5 ~ 0,
                                       Age >= 4 & Age <= 6 & UI_VitaminB12ug >= 0.8 ~ 0,
                                       Age >= 7 & Age <= 10 & UI_VitaminB12ug >= 1.0 ~ 0,
                                       Age >= 11 & Age <= 14 & UI_VitaminB12ug >= 1.2 ~ 0,
                                       Age >= 15 & Age <= 18 & UI_VitaminB12ug >= 1.5 ~ 0,
                                       
                                       Age >= 1 & Age <= 3 & UI_VitaminB12ug < 0.5 ~ 1,
                                       Age >= 4 & Age <= 6 & UI_VitaminB12ug < 0.8 ~ 1,
                                       Age >= 7 & Age <= 10 & UI_VitaminB12ug < 1.0 ~ 1,
                                       Age >= 11 & Age <= 14 & UI_VitaminB12ug < 1.2 ~ 1,
                                       Age >= 15 & Age <= 18 & UI_VitaminB12ug < 1.5 ~ 1,
                                       
                                       TRUE ~ NA_integer_),
         
         UI_LRNI_VitaminB12 = case_when(Age >= 1 & Age <= 3 & UI_VitaminB12ug >= 0.3 ~ 0, 
                                        Age >= 4 & Age <= 6 & UI_VitaminB12ug >= 0.5 ~ 0,
                                        Age >= 7 & Age <= 10 & UI_VitaminB12ug >= 0.6 ~ 0,
                                        Age >= 11 & Age <= 14 & UI_VitaminB12ug >= 0.8 ~ 0,
                                        Age >= 15 & Age <=18 & UI_VitaminB12ug >= 1.0 ~ 0,
                                        
                                        Age >= 1 & Age <= 3 & UI_VitaminB12ug < 0.3 ~ 1,
                                        Age >= 4 & Age <= 6 & UI_VitaminB12ug < 0.5 ~ 1,
                                        Age >= 7 & Age <= 10 & UI_VitaminB12ug < 0.6 ~ 1,
                                        Age >= 11 & Age <= 14 & UI_VitaminB12ug < 0.8 ~ 1,
                                        Age >= 15 & Age <=18 & UI_VitaminB12ug < 1.0 ~ 1,
                                        
                                        TRUE ~ NA_integer_),
         
         UI_RNI_prct_VitaminB12 = case_when(Age >= 1 & Age <= 3 ~ (UI_VitaminB12ug/0.5)*100,
                                            Age >= 4 & Age <= 6 ~ (UI_VitaminB12ug/0.8)*100,
                                            Age >= 7 & Age <= 10 ~ (UI_VitaminB12ug/1.0)*100,
                                            Age >= 11 & Age <= 14 ~ (UI_VitaminB12ug/1.2)*100,
                                            Age >= 15 & Age <= 18 ~ (UI_VitaminB12ug/1.5)*100,
                                            
                                            TRUE ~ NA_integer_)
  ) %>%  
  
  relocate(RNI_VitaminB12, LRNI_VitaminB12, RNI_prct_VitaminB12, UI_RNI_VitaminB12, UI_LRNI_VitaminB12, UI_RNI_prct_VitaminB12, .after = Avg_VitaminB12ug)

## Vitamin C ####
df.intake24_participant <- df.intake24_participant %>%  
  mutate(RNI_VitaminC = case_when(Age >= 1 & Age <= 10 & Avg_VitaminCmg >= 30 ~ 0,
                                  Age >= 11 & Age <= 14 & Avg_VitaminCmg >= 35 ~ 0,
                                  Age >= 15 & Age <= 18 & Avg_VitaminCmg >= 40 ~ 0,
                                  
                                  Age >= 1 & Age <= 10 & Avg_VitaminCmg < 30 ~ 1,
                                  Age >= 11 & Age <= 14 & Avg_VitaminCmg < 35 ~ 1,
                                  Age >= 15 & Age <= 18 & Avg_VitaminCmg < 40 ~ 1,
                                  
                                  TRUE ~ NA_integer_),
         
         LRNI_VitaminC = case_when(Age >= 1 & Age <= 10 & Avg_VitaminCmg >= 8 ~ 0, 
                                   Age >= 11 & Age <= 14 & Avg_VitaminCmg >= 9 ~ 0,
                                   Age >= 15 & Age <= 18 & Avg_VitaminCmg >= 10 ~ 0,
                                   
                                   Age >= 1 & Age <= 10 & Avg_VitaminCmg < 8 ~ 1,
                                   Age >= 11 & Age <= 14 & Avg_VitaminCmg < 9 ~ 1,
                                   Age >= 15 & Age <= 18 & Avg_VitaminCmg < 10 ~ 1,
                                   
                                   TRUE ~ NA_integer_),
         
         RNI_prct_VitaminC = case_when(Age >= 1 & Age <= 10 ~ (Avg_VitaminCmg/30)*100,
                                       Age >= 11 & Age <= 14 ~ (Avg_VitaminCmg/35)*100,
                                       Age >= 15 & Age <= 18 ~ (Avg_VitaminCmg/40)*100,
                                       
                                       TRUE ~ NA_integer_),
         
         UI_RNI_VitaminC = case_when(Age >= 1 & Age <= 10 & UI_VitaminCmg >= 30 ~ 0,
                                     Age >= 11 & Age <= 14 & UI_VitaminCmg >= 35 ~ 0,
                                     Age >= 15 & Age <= 18 & UI_VitaminCmg >= 40 ~ 0,
                                     
                                     Age >= 1 & Age <= 10 & UI_VitaminCmg < 30 ~ 1,
                                     Age >= 11 & Age <= 14 & UI_VitaminCmg < 35 ~ 1,
                                     Age >= 15 & Age <= 18 & UI_VitaminCmg < 40 ~ 1,
                                     
                                     TRUE ~ NA_integer_),
         
         UI_LRNI_VitaminC = case_when(Age >= 1 & Age <= 10 & UI_VitaminCmg >= 8 ~ 0, 
                                      Age >= 11 & Age <= 14 & UI_VitaminCmg >= 9 ~ 0,
                                      Age >= 15 & Age <= 18 & UI_VitaminCmg >= 10 ~ 0,
                                      
                                      Age >= 1 & Age <= 10 & UI_VitaminCmg < 8 ~ 1,
                                      Age >= 11 & Age <= 14 & UI_VitaminCmg < 9 ~ 1,
                                      Age >= 15 & Age <= 18 & UI_VitaminCmg < 10 ~ 1,
                                      
                                      TRUE ~ NA_integer_),
         
         UI_RNI_prct_VitaminC = case_when(Age >= 1 & Age <= 10 ~ (UI_VitaminCmg/30)*100,
                                          Age >= 11 & Age <= 14 ~ (UI_VitaminCmg/35)*100,
                                          Age >= 15 & Age <= 18 ~ (UI_VitaminCmg/40)*100,
                                          
                                          TRUE ~ NA_integer_)
  ) %>%  
  
  relocate(RNI_VitaminC, LRNI_VitaminC, RNI_prct_VitaminC, UI_RNI_VitaminC, UI_LRNI_VitaminC, UI_RNI_prct_VitaminC, .after = Avg_VitaminCmg)

## Iron ####   
df.intake24_participant <- df.intake24_participant %>%
  mutate(RNI_Iron = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Ironmg >= 6.9 ~ 0,
                              Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Ironmg >= 6.1 ~ 0,
                              Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Ironmg >= 8.7 ~ 0,
                              Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Ironmg >= 11.3 ~ 0,
                              Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Ironmg >= 11.3 ~ 0,
                              
                              Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Ironmg < 6.9 ~ 1,
                              Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Ironmg < 6.1 ~ 1,
                              Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Ironmg < 8.7 ~ 1,
                              Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Ironmg < 11.3 ~ 1,
                              Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Ironmg < 11.3 ~ 1,
                              
                              Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Ironmg >= 6.9 ~ 0,
                              Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Ironmg >= 6.1 ~ 0,
                              Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Ironmg >= 8.7 ~ 0,
                              Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Ironmg >= 14.8 ~ 0,
                              Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Ironmg >= 14.8 ~ 0,
                              
                              Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Ironmg < 6.9 ~ 1,
                              Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Ironmg < 6.1 ~ 1,
                              Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Ironmg < 8.7 ~ 1,
                              Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Ironmg < 14.8 ~ 1,
                              Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Ironmg < 14.8 ~ 1,
                              
                              TRUE ~ NA_integer_),
         
         LRNI_Iron = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Ironmg >= 3.7 ~ 0,
                               Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Ironmg >= 3.3 ~ 0,
                               Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Ironmg >= 4.7 ~ 0,
                               Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Ironmg >= 6.1 ~ 0,
                               Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Ironmg >= 6.1 ~ 0,
                               
                               Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Ironmg < 3.7 ~ 1,
                               Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Ironmg < 3.3 ~ 1,
                               Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Ironmg < 4.7 ~ 1,
                               Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Ironmg < 6.1 ~ 1,
                               Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Ironmg < 6.1 ~ 1,
                               
                               Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Ironmg >= 3.7 ~ 0,
                               Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Ironmg >= 3.3 ~ 0,
                               Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Ironmg >= 4.7 ~ 0,
                               Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Ironmg >= 8.0 ~ 0,
                               Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Ironmg >= 8.0 ~ 0,
                               
                               Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Ironmg < 3.7 ~ 1,
                               Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Ironmg < 3.3 ~ 1,
                               Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Ironmg < 4.7 ~ 1,
                               Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Ironmg < 8.0 ~ 1,
                               Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Ironmg < 8.0 ~ 1,
                               
                               TRUE ~ NA_integer_),
         
         RNI_prct_Iron = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (Avg_Ironmg/6.9)*100,
                                   Sex == "Male" & Age >= 4 & Age <= 6 ~ (Avg_Ironmg/6.1)*100,
                                   Sex == "Male" & Age >= 7 & Age <= 10 ~ (Avg_Ironmg/8.7)*100,
                                   Sex == "Male" & Age >= 11 & Age <= 14 ~ (Avg_Ironmg/11.3)*100,
                                   Sex == "Male" & Age >= 15 & Age <= 18 ~ (Avg_Ironmg/11.3)*100,
                                   
                                   Sex == "Female" & Age >= 1 & Age <= 3 ~ (Avg_Ironmg/6.9)*100,
                                   Sex == "Female" & Age >= 4 & Age <= 6 ~ (Avg_Ironmg/6.1)*100,
                                   Sex == "Female" & Age >= 7 & Age <= 10 ~ (Avg_Ironmg/8.7)*100,
                                   Sex == "Female" & Age >= 11 & Age <= 14 ~ (Avg_Ironmg/14.8)*100,
                                   Sex == "Female" & Age >= 15 & Age <= 18 ~ (Avg_Ironmg/14.8)*100,
                                   
                                   TRUE ~ NA_integer_),
         
         UI_RNI_Iron = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & UI_Ironmg >= 6.9 ~ 0,
                                 Sex == "Male" & Age >= 4 & Age <= 6 & UI_Ironmg >= 6.1 ~ 0,
                                 Sex == "Male" & Age >= 7 & Age <= 10 & UI_Ironmg >= 8.7 ~ 0,
                                 Sex == "Male" & Age >= 11 & Age <= 14 & UI_Ironmg >= 11.3 ~ 0,
                                 Sex == "Male" & Age >= 15 & Age <= 18 & UI_Ironmg >= 11.3 ~ 0,
                                 
                                 Sex == "Male" & Age >= 1 & Age <= 3 & UI_Ironmg < 6.9 ~ 1,
                                 Sex == "Male" & Age >= 4 & Age <= 6 & UI_Ironmg < 6.1 ~ 1,
                                 Sex == "Male" & Age >= 7 & Age <= 10 & UI_Ironmg < 8.7 ~ 1,
                                 Sex == "Male" & Age >= 11 & Age <= 14 & UI_Ironmg < 11.3 ~ 1,
                                 Sex == "Male" & Age >= 15 & Age <= 18 & UI_Ironmg < 11.3 ~ 1,
                                 
                                 Sex == "Female" & Age >= 1 & Age <= 3 & UI_Ironmg >= 6.9 ~ 0,
                                 Sex == "Female" & Age >= 4 & Age <= 6 & UI_Ironmg >= 6.1 ~ 0,
                                 Sex == "Female" & Age >= 7 & Age <= 10 & UI_Ironmg >= 8.7 ~ 0,
                                 Sex == "Female" & Age >= 11 & Age <= 14 & UI_Ironmg >= 14.8 ~ 0,
                                 Sex == "Female" & Age >= 15 & Age <= 18 & UI_Ironmg >= 14.8 ~ 0,
                                 
                                 Sex == "Female" & Age >= 1 & Age <= 3 & UI_Ironmg < 6.9 ~ 1,
                                 Sex == "Female" & Age >= 4 & Age <= 6 & UI_Ironmg < 6.1 ~ 1,
                                 Sex == "Female" & Age >= 7 & Age <= 10 & UI_Ironmg < 8.7 ~ 1,
                                 Sex == "Female" & Age >= 11 & Age <= 14 & UI_Ironmg < 14.8 ~ 1,
                                 Sex == "Female" & Age >= 15 & Age <= 18 & UI_Ironmg < 14.8 ~ 1,
                                 
                                 TRUE ~ NA_integer_),
         
         UI_LRNI_Iron = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & UI_Ironmg >= 3.7 ~ 0,
                                  Sex == "Male" & Age >= 4 & Age <= 6 & UI_Ironmg >= 3.3 ~ 0,
                                  Sex == "Male" & Age >= 7 & Age <= 10 & UI_Ironmg >= 4.7 ~ 0,
                                  Sex == "Male" & Age >= 11 & Age <= 14 & UI_Ironmg >= 6.1 ~ 0,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & UI_Ironmg >= 6.1 ~ 0,
                                  
                                  Sex == "Male" & Age >= 1 & Age <= 3 & UI_Ironmg < 3.7 ~ 1,
                                  Sex == "Male" & Age >= 4 & Age <= 6 & UI_Ironmg < 3.3 ~ 1,
                                  Sex == "Male" & Age >= 7 & Age <= 10 & UI_Ironmg < 4.7 ~ 1,
                                  Sex == "Male" & Age >= 11 & Age <= 14 & UI_Ironmg < 6.1 ~ 1,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & UI_Ironmg < 6.1 ~ 1,
                                  
                                  Sex == "Female" & Age >= 1 & Age <= 3 & UI_Ironmg >= 3.7 ~ 0,
                                  Sex == "Female" & Age >= 4 & Age <= 6 & UI_Ironmg >= 3.3 ~ 0,
                                  Sex == "Female" & Age >= 7 & Age <= 10 & UI_Ironmg >= 4.7 ~ 0,
                                  Sex == "Female" & Age >= 11 & Age <= 14 & UI_Ironmg >= 8.0 ~ 0,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & UI_Ironmg >= 8.0 ~ 0,
                                  
                                  Sex == "Female" & Age >= 1 & Age <= 3 & UI_Ironmg < 3.7 ~ 1,
                                  Sex == "Female" & Age >= 4 & Age <= 6 & UI_Ironmg < 3.3 ~ 1,
                                  Sex == "Female" & Age >= 7 & Age <= 10 & UI_Ironmg < 4.7 ~ 1,
                                  Sex == "Female" & Age >= 11 & Age <= 14 & UI_Ironmg < 8.0 ~ 1,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & UI_Ironmg < 8.0 ~ 1,
                                  
                                  TRUE ~ NA_integer_),
         
         UI_RNI_prct_Iron = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (UI_Ironmg/6.9)*100,
                                      Sex == "Male" & Age >= 4 & Age <= 6 ~ (UI_Ironmg/6.1)*100,
                                      Sex == "Male" & Age >= 7 & Age <= 10 ~ (UI_Ironmg/8.7)*100,
                                      Sex == "Male" & Age >= 11 & Age <= 14 ~ (UI_Ironmg/11.3)*100,
                                      Sex == "Male" & Age >= 15 & Age <= 18 ~ (UI_Ironmg/11.3)*100,
                                      
                                      Sex == "Female" & Age >= 1 & Age <= 3 ~ (UI_Ironmg/6.9)*100,
                                      Sex == "Female" & Age >= 4 & Age <= 6 ~ (UI_Ironmg/6.1)*100,
                                      Sex == "Female" & Age >= 7 & Age <= 10 ~ (UI_Ironmg/8.7)*100,
                                      Sex == "Female" & Age >= 11 & Age <= 14 ~ (UI_Ironmg/14.8)*100,
                                      Sex == "Female" & Age >= 15 & Age <= 18 ~ (UI_Ironmg/14.8)*100,
                                      
                                      TRUE ~ NA_integer_)
  ) %>%  
  
  relocate(RNI_Iron, LRNI_Iron, RNI_prct_Iron, UI_RNI_Iron, UI_LRNI_Iron, UI_RNI_prct_Iron, .after = Avg_Ironmg)

## Calcium ####  
df.intake24_participant <- df.intake24_participant %>%
  mutate(RNI_Calcium = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Calciummg >= 350 ~ 0,
                                 Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Calciummg >= 450 ~ 0,
                                 Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Calciummg >= 550 ~ 0,
                                 Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Calciummg >= 1000 ~ 0,
                                 Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Calciummg >= 1000 ~ 0,
                                 
                                 Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Calciummg < 350 ~ 1,
                                 Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Calciummg < 450 ~ 1,
                                 Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Calciummg < 550 ~ 1,
                                 Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Calciummg < 1000 ~ 1,
                                 Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Calciummg < 1000 ~ 1,
                                 
                                 Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Calciummg >= 350 ~ 0,
                                 Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Calciummg >= 450 ~ 0,
                                 Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Calciummg >= 550 ~ 0,
                                 Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Calciummg >= 800 ~ 0,
                                 Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Calciummg >= 800 ~ 0,
                                 
                                 Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Calciummg < 350 ~ 1,
                                 Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Calciummg < 450 ~ 1,
                                 Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Calciummg < 550 ~ 1,
                                 Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Calciummg < 800 ~ 1,
                                 Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Calciummg < 800 ~ 1,
                                 
                                 TRUE ~ NA_integer_),
         
         LRNI_Calcium = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Calciummg >= 200 ~ 0,
                                  Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Calciummg >= 275 ~ 0,
                                  Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Calciummg >= 325 ~ 0,
                                  Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Calciummg >= 480 ~ 0,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Calciummg >= 480 ~ 0,
                                  
                                  Sex == "Male" & Age >= 1 & Age <= 3 & Avg_Calciummg < 200 ~ 1,
                                  Sex == "Male" & Age >= 4 & Age <= 6 & Avg_Calciummg < 275 ~ 1,
                                  Sex == "Male" & Age >= 7 & Age <= 10 & Avg_Calciummg < 325 ~ 1,
                                  Sex == "Male" & Age >= 11 & Age <= 14 & Avg_Calciummg < 480 ~ 1,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Calciummg < 480 ~ 1,
                                  
                                  Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Calciummg >= 200 ~ 0,
                                  Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Calciummg >= 275 ~ 0,
                                  Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Calciummg >= 325 ~ 0,
                                  Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Calciummg >= 450 ~ 0,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Calciummg >= 450 ~ 0,
                                  
                                  Sex == "Female" & Age >= 1 & Age <= 3 & Avg_Calciummg < 200 ~ 1,
                                  Sex == "Female" & Age >= 4 & Age <= 6 & Avg_Calciummg < 275 ~ 1,
                                  Sex == "Female" & Age >= 7 & Age <= 10 & Avg_Calciummg < 325 ~ 1,
                                  Sex == "Female" & Age >= 11 & Age <= 14 & Avg_Calciummg < 450 ~ 1,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Calciummg < 450 ~ 1,
                                  
                                  TRUE ~ NA_integer_),
         
         RNI_prct_Calcium = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (Avg_Calciummg/350)*100,
                                      Sex == "Male" & Age >= 4 & Age <= 6 ~ (Avg_Calciummg/450)*100,
                                      Sex == "Male" & Age >= 7 & Age <= 10 ~ (Avg_Calciummg/550)*100,
                                      Sex == "Male" & Age >= 11 & Age <= 14 ~ (Avg_Calciummg/1000)*100,
                                      Sex == "Male" & Age >= 15 & Age <= 18 ~ (Avg_Calciummg/1000)*100,
                                      
                                      Sex == "Female" & Age >= 1 & Age <= 3 ~ (Avg_Calciummg/350)*100,
                                      Sex == "Female" & Age >= 4 & Age <= 6 ~ (Avg_Calciummg/450)*100,
                                      Sex == "Female" & Age >= 7 & Age <= 10 ~ (Avg_Calciummg/550)*100,
                                      Sex == "Female" & Age >= 11 & Age <= 14 ~ (Avg_Calciummg/800)*100,
                                      Sex == "Female" & Age >= 15 & Age <= 18 ~ (Avg_Calciummg/800)*100,
                                      
                                      TRUE ~ NA_integer_),
         
         UI_RNI_Calcium = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & UI_Calciummg >= 350 ~ 0,
                                    Sex == "Male" & Age >= 4 & Age <= 6 & UI_Calciummg >= 450 ~ 0,
                                    Sex == "Male" & Age >= 7 & Age <= 10 & UI_Calciummg >= 550 ~ 0,
                                    Sex == "Male" & Age >= 11 & Age <= 14 & UI_Calciummg >= 1000 ~ 0,
                                    Sex == "Male" & Age >= 15 & Age <= 18 & UI_Calciummg >= 1000 ~ 0,
                                    
                                    Sex == "Male" & Age >= 1 & Age <= 3 & UI_Calciummg < 350 ~ 1,
                                    Sex == "Male" & Age >= 4 & Age <= 6 & UI_Calciummg < 450 ~ 1,
                                    Sex == "Male" & Age >= 7 & Age <= 10 & UI_Calciummg < 550 ~ 1,
                                    Sex == "Male" & Age >= 11 & Age <= 14 & UI_Calciummg < 1000 ~ 1,
                                    Sex == "Male" & Age >= 15 & Age <= 18 & UI_Calciummg < 1000 ~ 1,
                                    
                                    Sex == "Female" & Age >= 1 & Age <= 3 & UI_Calciummg >= 350 ~ 0,
                                    Sex == "Female" & Age >= 4 & Age <= 6 & UI_Calciummg >= 450 ~ 0,
                                    Sex == "Female" & Age >= 7 & Age <= 10 & UI_Calciummg >= 550 ~ 0,
                                    Sex == "Female" & Age >= 11 & Age <= 14 & UI_Calciummg >= 800 ~ 0,
                                    Sex == "Female" & Age >= 15 & Age <= 18 & UI_Calciummg >= 800 ~ 0,
                                    
                                    Sex == "Female" & Age >= 1 & Age <= 3 & UI_Calciummg < 350 ~ 1,
                                    Sex == "Female" & Age >= 4 & Age <= 6 & UI_Calciummg < 450 ~ 1,
                                    Sex == "Female" & Age >= 7 & Age <= 10 & UI_Calciummg < 550 ~ 1,
                                    Sex == "Female" & Age >= 11 & Age <= 14 & UI_Calciummg < 800 ~ 1,
                                    Sex == "Female" & Age >= 15 & Age <= 18 & UI_Calciummg < 800 ~ 1,
                                    
                                    TRUE ~ NA_integer_),
         
         UI_LRNI_Calcium = case_when(Sex == "Male" & Age >= 1 & Age <= 3 & UI_Calciummg >= 200 ~ 0,
                                     Sex == "Male" & Age >= 4 & Age <= 6 & UI_Calciummg >= 275 ~ 0,
                                     Sex == "Male" & Age >= 7 & Age <= 10 & UI_Calciummg >= 325 ~ 0,
                                     Sex == "Male" & Age >= 11 & Age <= 14 & UI_Calciummg >= 480 ~ 0,
                                     Sex == "Male" & Age >= 15 & Age <= 18 & UI_Calciummg >= 480 ~ 0,
                                     
                                     Sex == "Male" & Age >= 1 & Age <= 3 & UI_Calciummg < 200 ~ 1,
                                     Sex == "Male" & Age >= 4 & Age <= 6 & UI_Calciummg < 275 ~ 1,
                                     Sex == "Male" & Age >= 7 & Age <= 10 & UI_Calciummg < 325 ~ 1,
                                     Sex == "Male" & Age >= 11 & Age <= 14 & UI_Calciummg < 480 ~ 1,
                                     Sex == "Male" & Age >= 15 & Age <= 18 & UI_Calciummg < 480 ~ 1,
                                     
                                     Sex == "Female" & Age >= 1 & Age <= 3 & UI_Calciummg >= 200 ~ 0,
                                     Sex == "Female" & Age >= 4 & Age <= 6 & UI_Calciummg >= 275 ~ 0,
                                     Sex == "Female" & Age >= 7 & Age <= 10 & UI_Calciummg >= 325 ~ 0,
                                     Sex == "Female" & Age >= 11 & Age <= 14 & UI_Calciummg >= 450 ~ 0,
                                     Sex == "Female" & Age >= 15 & Age <= 18 & UI_Calciummg >= 450 ~ 0,
                                     
                                     Sex == "Female" & Age >= 1 & Age <= 3 & UI_Calciummg < 200 ~ 1,
                                     Sex == "Female" & Age >= 4 & Age <= 6 & UI_Calciummg < 275 ~ 1,
                                     Sex == "Female" & Age >= 7 & Age <= 10 & UI_Calciummg < 325 ~ 1,
                                     Sex == "Female" & Age >= 11 & Age <= 14 & UI_Calciummg < 450 ~ 1,
                                     Sex == "Female" & Age >= 15 & Age <= 18 & UI_Calciummg < 450 ~ 1,
                                     
                                     TRUE ~ NA_integer_),
         
         UI_RNI_prct_Calcium = case_when(Sex == "Male" & Age >= 1 & Age <= 3 ~ (UI_Calciummg/350)*100,
                                         Sex == "Male" & Age >= 4 & Age <= 6 ~ (UI_Calciummg/450)*100,
                                         Sex == "Male" & Age >= 7 & Age <= 10 ~ (UI_Calciummg/550)*100,
                                         Sex == "Male" & Age >= 11 & Age <= 14 ~ (UI_Calciummg/1000)*100,
                                         Sex == "Male" & Age >= 15 & Age <= 18 ~ (UI_Calciummg/1000)*100,
                                         
                                         Sex == "Female" & Age >= 1 & Age <= 3 ~ (UI_Calciummg/350)*100,
                                         Sex == "Female" & Age >= 4 & Age <= 6 ~ (UI_Calciummg/450)*100,
                                         Sex == "Female" & Age >= 7 & Age <= 10 ~ (UI_Calciummg/550)*100,
                                         Sex == "Female" & Age >= 11 & Age <= 14 ~ (UI_Calciummg/800)*100,
                                         Sex == "Female" & Age >= 15 & Age <= 18 ~ (UI_Calciummg/800)*100,
                                         
                                         TRUE ~ NA_integer_)
  ) %>%  
  
  relocate(RNI_Calcium, LRNI_Calcium, RNI_prct_Calcium, UI_RNI_Calcium, UI_LRNI_Calcium, UI_RNI_prct_Calcium, .after = Avg_Calciummg)

## Magnesium ####
df.intake24_participant <- df.intake24_participant %>%  
  mutate(RNI_Magnesium = case_when(Age >= 1 & Age <= 3 & Avg_Magnesiummg >= 85 ~ 0,
                                   Age >= 4 & Age <= 6 & Avg_Magnesiummg >= 120 ~ 0,
                                   Age >= 7 & Age <= 10 & Avg_Magnesiummg >= 200 ~ 0,
                                   Age >= 11 & Age <= 14 & Avg_Magnesiummg >= 280 ~ 0,
                                   Age >= 15 & Age <= 18 & Avg_Magnesiummg >= 300 ~ 0,
                                   
                                   Age >= 1 & Age <= 3 & Avg_Magnesiummg < 85 ~ 1,
                                   Age >= 4 & Age <= 6 & Avg_Magnesiummg < 120 ~ 1,
                                   Age >= 7 & Age <= 10 & Avg_Magnesiummg < 200 ~ 1,
                                   Age >= 11 & Age <= 14 & Avg_Magnesiummg < 280 ~ 1,
                                   Age >= 15 & Age <= 18 & Avg_Magnesiummg < 300 ~ 1,
                                   
                                   TRUE ~ NA_integer_),
         
         LRNI_Magnesium = case_when(Age >= 1 & Age <= 3 & Avg_Magnesiummg >= 50 ~ 0, 
                                    Age >= 4 & Age <= 6 & Avg_Magnesiummg >= 70 ~ 0,
                                    Age >= 7 & Age <= 10 & Avg_Magnesiummg >= 115 ~ 0,
                                    Age >= 11 & Age <= 14 & Avg_Magnesiummg >= 180 ~ 0,
                                    Age >= 15 & Age <=18 & Avg_Magnesiummg >= 190 ~ 0,
                                    
                                    Age >= 1 & Age <= 3 & Avg_Magnesiummg < 50 ~ 1,
                                    Age >= 4 & Age <= 6 & Avg_Magnesiummg < 70 ~ 1,
                                    Age >= 7 & Age <= 10 & Avg_Magnesiummg < 115 ~ 1,
                                    Age >= 11 & Age <= 14 & Avg_Magnesiummg < 180 ~ 1,
                                    Age >= 15 & Age <=18 & Avg_Magnesiummg < 190 ~ 1,
                                    
                                    TRUE ~ NA_integer_),
         
         RNI_prct_Magnesium = case_when(Age >= 1 & Age <= 3 ~ (Avg_Magnesiummg/85)*100,
                                        Age >= 4 & Age <= 6 ~ (Avg_Magnesiummg/120)*100,
                                        Age >= 7 & Age <= 10 ~ (Avg_Magnesiummg/200)*100,
                                        Age >= 11 & Age <= 14 ~ (Avg_Magnesiummg/280)*100,
                                        Age >= 15 & Age <= 18 ~ (Avg_Magnesiummg/300)*100,
                                        
                                        TRUE ~ NA_integer_),
         
         UI_RNI_Magnesium = case_when(Age >= 1 & Age <= 3 & UI_Magnesiummg >= 85 ~ 0,
                                      Age >= 4 & Age <= 6 & UI_Magnesiummg >= 120 ~ 0,
                                      Age >= 7 & Age <= 10 & UI_Magnesiummg >= 200 ~ 0,
                                      Age >= 11 & Age <= 14 & UI_Magnesiummg >= 280 ~ 0,
                                      Age >= 15 & Age <= 18 & UI_Magnesiummg >= 300 ~ 0,
                                      
                                      Age >= 1 & Age <= 3 & UI_Magnesiummg < 85 ~ 1,
                                      Age >= 4 & Age <= 6 & UI_Magnesiummg < 120 ~ 1,
                                      Age >= 7 & Age <= 10 & UI_Magnesiummg < 200 ~ 1,
                                      Age >= 11 & Age <= 14 & UI_Magnesiummg < 280 ~ 1,
                                      Age >= 15 & Age <= 18 & UI_Magnesiummg < 300 ~ 1,
                                      
                                      TRUE ~ NA_integer_),
         
         UI_LRNI_Magnesium = case_when(Age >= 1 & Age <= 3 & UI_Magnesiummg >= 50 ~ 0, 
                                       Age >= 4 & Age <= 6 & UI_Magnesiummg >= 70 ~ 0,
                                       Age >= 7 & Age <= 10 & UI_Magnesiummg >= 115 ~ 0,
                                       Age >= 11 & Age <= 14 & UI_Magnesiummg >= 180 ~ 0,
                                       Age >= 15 & Age <=18 & UI_Magnesiummg >= 190 ~ 0,
                                       
                                       Age >= 1 & Age <= 3 & UI_Magnesiummg < 50 ~ 1,
                                       Age >= 4 & Age <= 6 & UI_Magnesiummg < 70 ~ 1,
                                       Age >= 7 & Age <= 10 & UI_Magnesiummg < 115 ~ 1,
                                       Age >= 11 & Age <= 14 & UI_Magnesiummg < 180 ~ 1,
                                       Age >= 15 & Age <=18 & UI_Magnesiummg < 190 ~ 1,
                                       
                                       TRUE ~ NA_integer_),
         
         UI_RNI_prct_Magnesium = case_when(Age >= 1 & Age <= 3 ~ (UI_Magnesiummg/85)*100,
                                           Age >= 4 & Age <= 6 ~ (UI_Magnesiummg/120)*100,
                                           Age >= 7 & Age <= 10 ~ (UI_Magnesiummg/200)*100,
                                           Age >= 11 & Age <= 14 ~ (UI_Magnesiummg/280)*100,
                                           Age >= 15 & Age <= 18 ~ (UI_Magnesiummg/300)*100,
                                           
                                           TRUE ~ NA_integer_)
  ) %>% 
  
  relocate(RNI_Magnesium, LRNI_Magnesium, RNI_prct_Magnesium, UI_RNI_Magnesium, UI_LRNI_Magnesium, UI_RNI_prct_Magnesium, .after = Avg_Magnesiummg)

## Potassium ####
df.intake24_participant <- df.intake24_participant %>%  
  mutate(RNI_Potassium = case_when(Age >= 1 & Age <= 3 & Avg_Potassiummg >= 800 ~ 0,
                                   Age >= 4 & Age <= 6 & Avg_Potassiummg >= 1100 ~ 0,
                                   Age >= 7 & Age <= 10 & Avg_Potassiummg >= 2000 ~ 0,
                                   Age >= 11 & Age <= 14 & Avg_Potassiummg >= 3100 ~ 0,
                                   Age >= 15 & Age <= 18 & Avg_Potassiummg >= 3500 ~ 0,
                                   
                                   Age >= 1 & Age <= 3 & Avg_Potassiummg < 800 ~ 1,
                                   Age >= 4 & Age <= 6 & Avg_Potassiummg < 1100 ~ 1,
                                   Age >= 7 & Age <= 10 & Avg_Potassiummg < 2000 ~ 1,
                                   Age >= 11 & Age <= 14 & Avg_Potassiummg < 3100 ~ 1,
                                   Age >= 15 & Age <= 18 & Avg_Potassiummg < 3500 ~ 1,
                                   
                                   TRUE ~ NA_integer_),
         
         LRNI_Potassium = case_when(Age >= 1 & Age <= 3 & Avg_Potassiummg >= 450 ~ 0, 
                                    Age >= 4 & Age <= 6 & Avg_Potassiummg >= 600 ~ 0,
                                    Age >= 7 & Age <= 10 & Avg_Potassiummg >= 950 ~ 0,
                                    Age >= 11 & Age <= 14 & Avg_Potassiummg >= 1600 ~ 0,
                                    Age >= 15 & Age <=18 & Avg_Potassiummg >= 2000 ~ 0,
                                    
                                    Age >= 1 & Age <= 3 & Avg_Potassiummg < 450 ~ 1,
                                    Age >= 4 & Age <= 6 & Avg_Potassiummg < 600 ~ 1,
                                    Age >= 7 & Age <= 10 & Avg_Potassiummg < 950 ~ 1,
                                    Age >= 11 & Age <= 14 & Avg_Potassiummg < 1600 ~ 1,
                                    Age >= 15 & Age <=18 & Avg_Potassiummg < 2000 ~ 1,
                                    
                                    TRUE ~ NA_integer_),
         
         RNI_prct_Potassium = case_when(Age >= 1 & Age <= 3 ~ (Avg_Potassiummg/800)*100,
                                        Age >= 4 & Age <= 6 ~ (Avg_Potassiummg/1100)*100,
                                        Age >= 7 & Age <= 10 ~ (Avg_Potassiummg/2000)*100,
                                        Age >= 11 & Age <= 14 ~ (Avg_Potassiummg/3100)*100,
                                        Age >= 15 & Age <= 18 ~ (Avg_Potassiummg/3500)*100,
                                        
                                        TRUE ~ NA_integer_),
         
         UI_RNI_Potassium = case_when(Age >= 1 & Age <= 3 & UI_Potassiummg >= 800 ~ 0,
                                      Age >= 4 & Age <= 6 & UI_Potassiummg >= 1100 ~ 0,
                                      Age >= 7 & Age <= 10 & UI_Potassiummg >= 2000 ~ 0,
                                      Age >= 11 & Age <= 14 & UI_Potassiummg >= 3100 ~ 0,
                                      Age >= 15 & Age <= 18 & UI_Potassiummg >= 3500 ~ 0,
                                      
                                      Age >= 1 & Age <= 3 & UI_Potassiummg < 800 ~ 1,
                                      Age >= 4 & Age <= 6 & UI_Potassiummg < 1100 ~ 1,
                                      Age >= 7 & Age <= 10 & UI_Potassiummg < 2000 ~ 1,
                                      Age >= 11 & Age <= 14 & UI_Potassiummg < 3100 ~ 1,
                                      Age >= 15 & Age <= 18 & UI_Potassiummg < 3500 ~ 1,
                                      
                                      TRUE ~ NA_integer_),
         
         UI_LRNI_Potassium = case_when(Age >= 1 & Age <= 3 & UI_Potassiummg >= 450 ~ 0, 
                                       Age >= 4 & Age <= 6 & UI_Potassiummg >= 600 ~ 0,
                                       Age >= 7 & Age <= 10 & UI_Potassiummg >= 950 ~ 0,
                                       Age >= 11 & Age <= 14 & UI_Potassiummg >= 1600 ~ 0,
                                       Age >= 15 & Age <=18 & UI_Potassiummg >= 2000 ~ 0,
                                       
                                       Age >= 1 & Age <= 3 & UI_Potassiummg < 450 ~ 1,
                                       Age >= 4 & Age <= 6 & UI_Potassiummg < 600 ~ 1,
                                       Age >= 7 & Age <= 10 & UI_Potassiummg < 950 ~ 1,
                                       Age >= 11 & Age <= 14 & UI_Potassiummg < 1600 ~ 1,
                                       Age >= 15 & Age <=18 & UI_Potassiummg < 2000 ~ 1,
                                       
                                       TRUE ~ NA_integer_),
         
         UI_RNI_prct_Potassium = case_when(Age >= 1 & Age <= 3 ~ (UI_Potassiummg/800)*100,
                                           Age >= 4 & Age <= 6 ~ (UI_Potassiummg/1100)*100,
                                           Age >= 7 & Age <= 10 ~ (UI_Potassiummg/2000)*100,
                                           Age >= 11 & Age <= 14 ~ (UI_Potassiummg/3100)*100,
                                           Age >= 15 & Age <= 18 ~ (UI_Potassiummg/3500)*100,
                                           
                                           TRUE ~ NA_integer_)
  ) %>% 
  
  relocate(RNI_Potassium, LRNI_Potassium, RNI_prct_Potassium, UI_RNI_Potassium, UI_LRNI_Potassium, UI_RNI_prct_Potassium, .after = Avg_Potassiummg)

## Iodine ####
df.intake24_participant <- df.intake24_participant %>%  
  mutate(RNI_Iodine = case_when(Age >= 1 & Age <= 3 & Avg_Iodineug >= 70 ~ 0,
                                Age >= 4 & Age <= 6 & Avg_Iodineug >= 100 ~ 0,
                                Age >= 7 & Age <= 10 & Avg_Iodineug >= 110 ~ 0,
                                Age >= 11 & Age <= 14 & Avg_Iodineug >= 130 ~ 0,
                                Age >= 15 & Age <= 18 & Avg_Iodineug >= 140 ~ 0,
                                
                                Age >= 1 & Age <= 3 & Avg_Iodineug < 70 ~ 1,
                                Age >= 4 & Age <= 6 & Avg_Iodineug < 100 ~ 1,
                                Age >= 7 & Age <= 10 & Avg_Iodineug < 110 ~ 1,
                                Age >= 11 & Age <= 14 & Avg_Iodineug < 130 ~ 1,
                                Age >= 15 & Age <= 18 & Avg_Iodineug < 140 ~ 1,
                                
                                TRUE ~ NA_integer_),
         
         LRNI_Iodine = case_when(Age >= 1 & Age <= 3 & Avg_Iodineug >= 40 ~ 0, 
                                 Age >= 4 & Age <= 6 & Avg_Iodineug >= 50 ~ 0,
                                 Age >= 7 & Age <= 10 & Avg_Iodineug >= 55 ~ 0,
                                 Age >= 11 & Age <= 14 & Avg_Iodineug >= 65 ~ 0,
                                 Age >= 15 & Age <=18 & Avg_Iodineug >= 70 ~ 0,
                                 
                                 Age >= 1 & Age <= 3 & Avg_Iodineug < 40 ~ 1,
                                 Age >= 4 & Age <= 6 & Avg_Iodineug < 50 ~ 1,
                                 Age >= 7 & Age <= 10 & Avg_Iodineug < 55 ~ 1,
                                 Age >= 11 & Age <= 14 & Avg_Iodineug < 65 ~ 1,
                                 Age >= 15 & Age <=18 & Avg_Iodineug < 70 ~ 1,
                                 
                                 TRUE ~ NA_integer_),
         
         RNI_prct_Iodine = case_when(Age >= 1 & Age <= 3 ~ (Avg_Iodineug/85)*70,
                                     Age >= 4 & Age <= 6 ~ (Avg_Iodineug/100)*100,
                                     Age >= 7 & Age <= 10 ~ (Avg_Iodineug/110)*100,
                                     Age >= 11 & Age <= 14 ~ (Avg_Iodineug/130)*100,
                                     Age >= 15 & Age <= 18 ~ (Avg_Iodineug/140)*100,
                                     
                                     TRUE ~ NA_integer_),
         
         UI_RNI_Iodine = case_when(Age >= 1 & Age <= 3 & UI_Iodineug >= 70 ~ 0,
                                   Age >= 4 & Age <= 6 & UI_Iodineug >= 100 ~ 0,
                                   Age >= 7 & Age <= 10 & UI_Iodineug >= 110 ~ 0,
                                   Age >= 11 & Age <= 14 & UI_Iodineug >= 130 ~ 0,
                                   Age >= 15 & Age <= 18 & UI_Iodineug >= 140 ~ 0,
                                   
                                   Age >= 1 & Age <= 3 & UI_Iodineug < 70 ~ 1,
                                   Age >= 4 & Age <= 6 & UI_Iodineug < 100 ~ 1,
                                   Age >= 7 & Age <= 10 & UI_Iodineug < 110 ~ 1,
                                   Age >= 11 & Age <= 14 & UI_Iodineug < 130 ~ 1,
                                   Age >= 15 & Age <= 18 & UI_Iodineug < 140 ~ 1,
                                   
                                   TRUE ~ NA_integer_),
         
         UI_LRNI_Iodine = case_when(Age >= 1 & Age <= 3 & UI_Iodineug >= 40 ~ 0, 
                                    Age >= 4 & Age <= 6 & UI_Iodineug >= 50 ~ 0,
                                    Age >= 7 & Age <= 10 & UI_Iodineug >= 55 ~ 0,
                                    Age >= 11 & Age <= 14 & UI_Iodineug >= 65 ~ 0,
                                    Age >= 15 & Age <=18 & UI_Iodineug >= 70 ~ 0,
                                    
                                    Age >= 1 & Age <= 3 & UI_Iodineug < 40 ~ 1,
                                    Age >= 4 & Age <= 6 & UI_Iodineug < 50 ~ 1,
                                    Age >= 7 & Age <= 10 & UI_Iodineug < 55 ~ 1,
                                    Age >= 11 & Age <= 14 & UI_Iodineug < 65 ~ 1,
                                    Age >= 15 & Age <=18 & UI_Iodineug < 70 ~ 1,
                                    
                                    TRUE ~ NA_integer_),
         
         UI_RNI_prct_Iodine = case_when(Age >= 1 & Age <= 3 ~ (UI_Iodineug/85)*70,
                                        Age >= 4 & Age <= 6 ~ (UI_Iodineug/100)*100,
                                        Age >= 7 & Age <= 10 ~ (UI_Iodineug/110)*100,
                                        Age >= 11 & Age <= 14 ~ (UI_Iodineug/130)*100,
                                        Age >= 15 & Age <= 18 ~ (UI_Iodineug/140)*100,
                                        
                                        TRUE ~ NA_integer_)
  ) %>% 
  
  relocate(RNI_Iodine, LRNI_Iodine, RNI_prct_Iodine, UI_RNI_Iodine, UI_LRNI_Iodine, UI_RNI_prct_Iodine, .after = Avg_Iodineug)

## Selenium ####
df.intake24_participant <- df.intake24_participant %>%
  mutate(RNI_Selenium = case_when(Age >= 1 & Age <= 3 & Avg_Seleniumug >= 15 ~ 0,
                                  Age >= 4 & Age <= 6 & Avg_Seleniumug >=20 ~ 0,
                                  Age >= 7 & Age <= 10 & Avg_Seleniumug >= 30 ~ 0,
                                  Age >= 11 & Age <= 14 & Avg_Seleniumug >= 45 ~ 0,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Seleniumug >= 70 ~ 0,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Seleniumug >= 60 ~ 0,
                                  
                                  Age >= 1 & Age <= 3 & Avg_Seleniumug < 15 ~ 1,
                                  Age >= 4 & Age <= 6 & Avg_Seleniumug < 20 ~ 1,
                                  Age >= 7 & Age <= 10 & Avg_Seleniumug < 30 ~ 1,
                                  Age >= 11 & Age <= 14 & Avg_Seleniumug < 45 ~ 1,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Seleniumug < 70 ~ 1,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Seleniumug < 60 ~ 1,
                                  
                                  TRUE ~ NA_integer_),
         
         LRNI_Selenium = case_when(Age >= 1 & Age <= 3 & Avg_Seleniumug >= 7 ~ 0,
                                   Age >= 4 & Age <= 6 & Avg_Seleniumug >= 10 ~ 0,
                                   Age >= 7 & Age <= 10 & Avg_Seleniumug >= 16 ~ 0,
                                   Age >= 11 & Age <= 14 & Avg_Seleniumug >= 25 ~ 0,
                                   Age >= 15 & Age <= 18 & Avg_Seleniumug >= 40 ~ 0,
                                   
                                   Age >= 1 & Age <= 3 & Avg_Seleniumug < 7 ~ 1,
                                   Age >= 4 & Age <= 6 & Avg_Seleniumug < 10 ~ 1,
                                   Age >= 7 & Age <= 10 & Avg_Seleniumug < 16 ~ 1,
                                   Age >= 11 & Age <= 14 & Avg_Seleniumug < 25 ~ 1,
                                   Age >= 15 & Age <= 18 & Avg_Seleniumug < 40 ~ 1,
                                   
                                   TRUE ~ NA_integer_),
         
         RNI_prct_Selenium = case_when(Age >= 1 & Age <= 3 ~ (Avg_Seleniumug/15)*70,
                                       Age >= 4 & Age <= 6 ~ (Avg_Seleniumug/20)*100,
                                       Age >= 7 & Age <= 10 ~ (Avg_Seleniumug/30)*100,
                                       Age >= 11 & Age <= 14 ~ (Avg_Seleniumug/45)*100,
                                       Sex == "Male" & Age >= 15 & Age <= 18 ~ (Avg_Seleniumug/70)*100,
                                       Sex == "Female" & Age >= 15 & Age <= 18 ~ (Avg_Seleniumug/60)*100,
                                       
                                       TRUE ~ NA_integer_),
         
         UI_RNI_Selenium = case_when(Age >= 1 & Age <= 3 & UI_Seleniumug >= 15 ~ 0,
                                     Age >= 4 & Age <= 6 & UI_Seleniumug >=20 ~ 0,
                                     Age >= 7 & Age <= 10 & UI_Seleniumug >= 30 ~ 0,
                                     Age >= 11 & Age <= 14 & UI_Seleniumug >= 45 ~ 0,
                                     Sex == "Male" & Age >= 15 & Age <= 18 & UI_Seleniumug >= 70 ~ 0,
                                     Sex == "Female" & Age >= 15 & Age <= 18 & UI_Seleniumug >= 60 ~ 0,
                                     
                                     Age >= 1 & Age <= 3 & UI_Seleniumug < 15 ~ 1,
                                     Age >= 4 & Age <= 6 & UI_Seleniumug < 20 ~ 1,
                                     Age >= 7 & Age <= 10 & UI_Seleniumug < 30 ~ 1,
                                     Age >= 11 & Age <= 14 & UI_Seleniumug < 45 ~ 1,
                                     Sex == "Male" & Age >= 15 & Age <= 18 & UI_Seleniumug < 70 ~ 1,
                                     Sex == "Female" & Age >= 15 & Age <= 18 & UI_Seleniumug < 60 ~ 1,
                                     
                                     TRUE ~ NA_integer_),
         
         UI_LRNI_Selenium = case_when(Age >= 1 & Age <= 3 & UI_Seleniumug >= 7 ~ 0,
                                      Age >= 4 & Age <= 6 & UI_Seleniumug >= 10 ~ 0,
                                      Age >= 7 & Age <= 10 & UI_Seleniumug >= 16 ~ 0,
                                      Age >= 11 & Age <= 14 & UI_Seleniumug >= 25 ~ 0,
                                      Age >= 15 & Age <= 18 & UI_Seleniumug >= 40 ~ 0,
                                      
                                      Age >= 1 & Age <= 3 & UI_Seleniumug < 7 ~ 1,
                                      Age >= 4 & Age <= 6 & UI_Seleniumug < 10 ~ 1,
                                      Age >= 7 & Age <= 10 & UI_Seleniumug < 16 ~ 1,
                                      Age >= 11 & Age <= 14 & UI_Seleniumug < 25 ~ 1,
                                      Age >= 15 & Age <= 18 & UI_Seleniumug < 40 ~ 1,
                                      
                                      TRUE ~ NA_integer_),
         
         UI_RNI_prct_Selenium = case_when(Age >= 1 & Age <= 3 ~ (UI_Seleniumug/15)*70,
                                          Age >= 4 & Age <= 6 ~ (UI_Seleniumug/20)*100,
                                          Age >= 7 & Age <= 10 ~ (UI_Seleniumug/30)*100,
                                          Age >= 11 & Age <= 14 ~ (UI_Seleniumug/45)*100,
                                          Sex == "Male" & Age >= 15 & Age <= 18 ~ (UI_Seleniumug/70)*100,
                                          Sex == "Female" & Age >= 15 & Age <= 18 ~ (UI_Seleniumug/60)*100,
                                          
                                          TRUE ~ NA_integer_)
  ) %>% 
  
  relocate(RNI_Selenium, LRNI_Selenium, RNI_prct_Selenium, UI_RNI_Selenium, UI_LRNI_Selenium, UI_RNI_prct_Selenium, .after = Avg_Seleniumug)

## Zinc #### 
df.intake24_participant <- df.intake24_participant %>%
  mutate(RNI_Zinc = case_when(Age >= 1 & Age <= 3 & Avg_Zincmg >= 5.0 ~ 0,
                              Age >= 4 & Age <= 6 & Avg_Zincmg >= 6.5 ~ 0,
                              Age >= 7 & Age <= 10 & Avg_Zincmg >= 7.0 ~ 0,
                              Age >= 11 & Age <= 14 & Avg_Zincmg >= 9.0 ~ 0,
                              Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Zincmg >= 9.5 ~ 0,
                              Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Zincmg >= 7.0 ~ 0,
                              
                              Age >= 1 & Age <= 3 & Avg_Zincmg < 5.0 ~ 1,
                              Age >= 4 & Age <= 6 & Avg_Zincmg < 6.5 ~ 1,
                              Age >= 7 & Age <= 10 & Avg_Zincmg < 7.0 ~ 1,
                              Age >= 11 & Age <= 14 & Avg_Zincmg < 9.0 ~ 1,
                              Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Zincmg < 9.5 ~ 1,
                              Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Zincmg < 7.0 ~ 1,
                              
                              TRUE ~ NA_integer_),
         
         LRNI_Zinc = case_when(Age >= 1 & Age <= 3 & Avg_Zincmg >= 3.0 ~ 0, 
                               Age >= 4 & Age <= 6 & Avg_Zincmg >= 4.0 ~ 0,
                               Age >= 7 & Age <= 10 & Avg_Zincmg >= 4.0 ~ 0,
                               Age >= 11 & Age <= 14 & Avg_Zincmg >= 5.3 ~ 0,
                               Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Zincmg >= 5.5 ~ 0,
                               Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Zincmg >= 4.0 ~ 0,
                               
                               Age >= 1 & Age < 4 & Avg_Zincmg < 3.0 ~ 1,
                               Age >= 4 & Age < 7 & Avg_Zincmg < 4.0 ~ 1,
                               Age >= 7 & Age < 11 & Avg_Zincmg < 4.0 ~ 1,
                               Age >= 11 & Age < 15 & Avg_Zincmg < 5.3 ~ 1,
                               Sex == "Male" & Age >= 15 & Age <= 18 & Avg_Zincmg < 5.5 ~ 1,
                               Sex == "Female" & Age >= 15 & Age <= 18 & Avg_Zincmg < 4.0 ~ 1,
                               
                               TRUE ~ NA_integer_),
         
         RNI_prct_Zinc = case_when(Age >= 1 & Age <= 3 ~ (Avg_Zincmg/5.0)*70,
                                   Age >= 4 & Age <= 6 ~ (Avg_Zincmg/6.5)*100,
                                   Age >= 7 & Age <= 10 ~ (Avg_Zincmg/7.0)*100,
                                   Age >= 11 & Age <= 14 ~ (Avg_Zincmg/9.0)*100,
                                   Sex == "Male" & Age >= 15 & Age <= 18 ~ (Avg_Zincmg/9.5)*100,
                                   Sex == "Female" & Age >= 15 & Age <= 18 ~ (Avg_Zincmg/7.0)*100,
                                   
                                   TRUE ~ NA_integer_),
         
         UI_RNI_Zinc = case_when(Age >= 1 & Age <= 3 & UI_Zincmg >= 5.0 ~ 0,
                                 Age >= 4 & Age <= 6 & UI_Zincmg >= 6.5 ~ 0,
                                 Age >= 7 & Age <= 10 & UI_Zincmg >= 7.0 ~ 0,
                                 Age >= 11 & Age <= 14 & UI_Zincmg >= 9.0 ~ 0,
                                 Sex == "Male" & Age >= 15 & Age <= 18 & UI_Zincmg >= 9.5 ~ 0,
                                 Sex == "Female" & Age >= 15 & Age <= 18 & UI_Zincmg >= 7.0 ~ 0,
                                 
                                 Age >= 1 & Age <= 3 & UI_Zincmg < 5.0 ~ 1,
                                 Age >= 4 & Age <= 6 & UI_Zincmg < 6.5 ~ 1,
                                 Age >= 7 & Age <= 10 & UI_Zincmg < 7.0 ~ 1,
                                 Age >= 11 & Age <= 14 & UI_Zincmg < 9.0 ~ 1,
                                 Sex == "Male" & Age >= 15 & Age <= 18 & UI_Zincmg < 9.5 ~ 1,
                                 Sex == "Female" & Age >= 15 & Age <= 18 & UI_Zincmg < 7.0 ~ 1,
                                 
                                 TRUE ~ NA_integer_),
         
         UI_LRNI_Zinc = case_when(Age >= 1 & Age <= 3 & UI_Zincmg >= 3.0 ~ 0, 
                                  Age >= 4 & Age <= 6 & UI_Zincmg >= 4.0 ~ 0,
                                  Age >= 7 & Age <= 10 & UI_Zincmg >= 4.0 ~ 0,
                                  Age >= 11 & Age <= 14 & UI_Zincmg >= 5.3 ~ 0,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & UI_Zincmg >= 5.5 ~ 0,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & UI_Zincmg >= 4.0 ~ 0,
                                  
                                  Age >= 1 & Age < 4 & UI_Zincmg < 3.0 ~ 1,
                                  Age >= 4 & Age < 7 & UI_Zincmg < 4.0 ~ 1,
                                  Age >= 7 & Age < 11 & UI_Zincmg < 4.0 ~ 1,
                                  Age >= 11 & Age < 15 & UI_Zincmg < 5.3 ~ 1,
                                  Sex == "Male" & Age >= 15 & Age <= 18 & UI_Zincmg < 5.5 ~ 1,
                                  Sex == "Female" & Age >= 15 & Age <= 18 & UI_Zincmg < 4.0 ~ 1,
                                  
                                  TRUE ~ NA_integer_),
         
         UI_RNI_prct_Zinc = case_when(Age >= 1 & Age <= 3 ~ (UI_Zincmg/5.0)*70,
                                      Age >= 4 & Age <= 6 ~ (UI_Zincmg/6.5)*100,
                                      Age >= 7 & Age <= 10 ~ (UI_Zincmg/7.0)*100,
                                      Age >= 11 & Age <= 14 ~ (UI_Zincmg/9.0)*100,
                                      Sex == "Male" & Age >= 15 & Age <= 18 ~ (UI_Zincmg/9.5)*100,
                                      Sex == "Female" & Age >= 15 & Age <= 18 ~ (UI_Zincmg/7.0)*100,
                                      
                                      TRUE ~ NA_integer_)
  ) %>% 
  
  relocate(RNI_Zinc, LRNI_Zinc, RNI_prct_Zinc, UI_RNI_Zinc, UI_LRNI_Zinc, UI_RNI_prct_Zinc, .after = Avg_Zincmg)

##Add values "Below" or "Above" ####

#define variables 
intakes <- c("RNI_Protein", "RNI_VitaminA", "LRNI_VitaminA", "RNI_Riboflavin", "LRNI_Riboflavin", "RNI_Folate", 
             "LRNI_Folate", "RNI_VitaminD", "RNI_VitaminB12", "LRNI_VitaminB12", "RNI_VitaminC", "LRNI_VitaminC", "RNI_Iron", 
             "LRNI_Iron", "RNI_Calcium", "LRNI_Calcium", "RNI_Magnesium", "LRNI_Magnesium", "RNI_Potassium", "LRNI_Potassium", 
             "RNI_Iodine", "LRNI_Iodine", "RNI_Selenium", "LRNI_Selenium", "RNI_Zinc", "LRNI_Zinc","UI_RNI_Protein", "UI_RNI_VitaminA", 
             "UI_LRNI_VitaminA", "UI_RNI_Riboflavin", "UI_LRNI_Riboflavin", "UI_RNI_Folate", "UI_LRNI_Folate", "UI_RNI_VitaminD", 
             "UI_RNI_VitaminB12", "UI_LRNI_VitaminB12", "UI_RNI_VitaminC", "UI_LRNI_VitaminC", "UI_RNI_Iron", "UI_LRNI_Iron", 
             "UI_RNI_Calcium", "UI_LRNI_Calcium", "UI_RNI_Magnesium", "UI_LRNI_Magnesium", "UI_RNI_Potassium", "UI_LRNI_Potassium",
             "UI_RNI_Iodine", "UI_LRNI_Iodine", "UI_RNI_Selenium", "UI_LRNI_Selenium", "UI_RNI_Zinc", "UI_LRNI_Zinc")

#set as factors
df.intake24_participant <- df.intake24_participant %>%
  mutate(across(all_of(intakes),as.factor))

#recode factor levels
df.intake24_participant <- df.intake24_participant %>%
  mutate(across(all_of(intakes), ~ fct_recode(.x, "Below" = "1", "Above" = "0")))



# Consumers ####

## Food categories ####
df.intake24_foodcat <- df.intake24_foodcat %>%
  mutate(across(starts_with("Avg_Prop_Energykcal_foodcat_"),
                ~case_when(.x > 0 ~ 1, TRUE ~ 0),
                .names = "Consumer_foodcat_{str_extract(.col, '\\\\d+$')}")) #dynamically name columns calling out the last digits attached at the end of the string)

df.intake24_foodcat <- df.intake24_foodcat %>%
  mutate(across(starts_with("Consumer_foodcat_"), ~as.factor(.x)))

df.intake24_foodcat <- df.intake24_foodcat %>%
  mutate(across(starts_with("Consumer_foodcat_"),
                ~fct_recode(.x, "Consumer"="1", "Non-consumer"="0")))


## NDNS main food groups ####
df.intake24_main_fg <- df.intake24_main_fg %>%
  mutate(across(starts_with("Avg_Prop_Energykcal_fg_"), 
                ~ case_when(.x > 0 ~ 1, TRUE ~ 0), 
                .names = "Consumer_ndns_{str_extract(.col, '\\\\d+$')}")) 

df.intake24_main_fg <- df.intake24_main_fg %>%
  mutate(across(starts_with("Consumer_ndns_"), ~as.factor(.x)))

df.intake24_main_fg <- df.intake24_main_fg %>%
  mutate(across(starts_with("Consumer_ndns_"),
                ~fct_recode(.x, "Consumer"="1", "Non-consumer"="0")))


## Discretionary foods ####
df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>%
  mutate(across(starts_with("Avg_Prop_Energykcal_discr_"),
                ~case_when(.x > 0 ~ 1, TRUE ~ 0),
                .names = "Consumer_discr_{str_extract(.col, '\\\\d+$')}")) 

df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>%
  mutate(across(c(Consumer_discr_1:Consumer_discr_11),as.factor))

df.intake24_discretionary_fg <- df.intake24_discretionary_fg %>%
  mutate(across(starts_with("Consumer_discr_"), 
                ~ fct_recode(.x, "Consumer"="1", "Non-consumer"="0")))



# Write files for analysis ####
df.intake24_participant <- left_join(df.intake24_participant,df.intake24_foodcat, by="UserID") 
df.intake24_participant <- left_join(df.intake24_participant,df.intake24_main_fg, by="UserID") 
df.intake24_participant <- left_join(df.intake24_participant,df.intake24_discretionary_fg, by="UserID")

rm(df.intake24_foodcat,df.intake24_main_fg,df.intake24_discretionary_fg,df.day_nutrients,df.intake24_ui)

write.csv(df.intake24_participant, file = "Data/intake24_participant.csv")
write.csv(df.intake24_recall, file = "Data/intake24_recall.csv")
write.csv(df.intake24_item, file = "Data/intake24_item.csv")

