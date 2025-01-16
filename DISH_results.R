
# DISH Results Tables
# Date: 2024-10-11


# Prepare R Environment ####

# Clear R environment
rm(list = ls())

# Load required packages
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
library(purrr)
library(survey)
library(reshape2)
library(RColorBrewer)
library(pals)
library(flexlsx)
library(openxlsx2)
library(janitor)
library(tibble)
library(openxlsx)

#set working directory
#setwd("C:/Users/ljaacks/OneDrive - University of Edinburgh/Scottish Diets/_National Monitoring of Diet Scotland/3_DISH/Analysis/")



# Import the data ####
df.intake24_item <- read.csv("Data/intake24_item.csv")
df.intake24_recall <- read.csv("Data/intake24_recall.csv")
df.intake24_participant <- read.csv("Data/intake24_participant.csv")
df.survey <- read.csv("Data/survey_clean.csv")
df.weights <- read.csv("Data/sample_survey_weights_081024.csv")
df.supplements <- read.csv("Data/supplements.csv")



# Sample weights ####

#re-scale survey weights for subsets by sex - 11 participants without sex in survey data
df.weights <- df.weights %>%
  mutate(sampleweight_fm_2 = (sampweight_fm*1689)/1700)

#merge with survey data
df.weights <- df.weights %>%
  filter(!is.na(sampweight_all)) %>%
  mutate(sampleweight_all = sampweight_all,
         sampleweight_fm = sampweight_fm) %>%
  select(UserID, sampleweight_all, sampleweight_fm, sampleweight_fm_2)

df.survey <- left_join(df.survey, df.weights, by = "UserID")
df.survey <- df.survey %>%
  relocate(sampleweight_all, sampleweight_fm, sampleweight_fm_2, .after=UserID)

df.intake24_participant <- left_join(df.intake24_participant, df.weights, by = "UserID")
df.intake24_participant <- df.intake24_participant %>%
  relocate(sampleweight_all, sampleweight_fm, sampleweight_fm_2, .after=UserID)


#set factor levels
df.survey$age_cat <- factor(df.survey$age_cat, levels = c("2-4y", "5-10y", "11-15y"))
df.survey$Ethnicity <- factor(df.survey$Ethnicity, levels = c("White", "Asian or Asian British", "Mixed or multiple ethnic groups", "Black, Black British, Caribbean or African", "Other", "Prefer not to say"))
df.survey$FoodParcels <- factor(df.survey$FoodParcels, levels = c("Yes", "No", "Prefer not to say"))
df.survey$YPFreqOffSchlGrounds <- factor(df.survey$YPFreqOffSchlGrounds, levels = c("Never", "1-2 days a week", "3-4 days a week", "Every day", "Not sure"))
df.survey$AgreeOSG1 <- factor(df.survey$AgreeOSG1, levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree"))
df.survey$AgreeOSG2 <- factor(df.survey$AgreeOSG2, levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree"))
df.survey$AgreeOSG3 <- factor(df.survey$AgreeOSG3, levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree"))
df.survey$AgreeOSG4 <- factor(df.survey$AgreeOSG4, levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree"))
df.survey$AgreeOSG5 <- factor(df.survey$AgreeOSG5, levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree"))
df.survey$AgreeOSG6 <- factor(df.survey$AgreeOSG6, levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree"))
df.survey$AgreeOSG7 <- factor(df.survey$AgreeOSG7, levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree"))
df.survey$UseFoodApps <- factor(df.survey$UseFoodApps, levels = c("Yes","No"))
df.survey$FreqFoodApps <- factor(df.survey$FreqFoodApps, levels = c("Less than once a month","About once a month,","About 2-3 times a month,","About once a week,","Several times a week,"))
df.survey$FreqEnergyDrinks <- factor(df.survey$FreqEnergyDrinks, levels = c("Never","Less than once a week","1-2 days a week","3-4 days a week","5-6 days a week","Every day","Not sure"))
df.survey$ImportanceED1 <- factor(df.survey$ImportanceED1, levels = c("Extremely important","Very important","Somewhat important","Slightly important","Not important"))
df.survey$ImportanceED2 <- factor(df.survey$ImportanceED2, levels = c("Extremely important","Very important","Somewhat important","Slightly important","Not important"))
df.survey$ImportanceED3 <- factor(df.survey$ImportanceED3, levels = c("Extremely important","Very important","Somewhat important","Slightly important","Not important"))
df.survey$ImportanceED4 <- factor(df.survey$ImportanceED4, levels = c("Extremely important","Very important","Somewhat important","Slightly important","Not important"))
df.survey$ImportanceED5 <- factor(df.survey$ImportanceED5, levels = c("Extremely important","Very important","Somewhat important","Slightly important","Not important"))

df.intake24_participant$age_cat <- factor(df.intake24_participant$age_cat, levels = c("2-4y", "5-10y", "11-15y"))
df.intake24_participant$age_sex_cat <- factor(df.intake24_participant$age_sex_cat, levels = c("Female, 2-4y", "Female, 5-10y", "Female, 11-15y", "Male, 2-4y", "Male, 5-10y", "Male, 11-15y"))
df.intake24_participant$AFreqOilyFish <- factor(df.intake24_participant$AFreqOilyFish, levels = c("Every day", "4-6 times a week", "2-3 times a week", "Once a week", "Less than once a week", "Never", "Not sure"))
df.intake24_participant$AFreqToddlerMilk <- factor(df.intake24_participant$AFreqToddlerMilk, levels = c("Every day", "4-6 times a week", "2-3 times a week", "Once a week", "Less than once a week", "Never", "Not sure"))
df.intake24_participant$SDG_cat_oilyfish <- factor(df.intake24_participant$SDG_cat_oilyfish, levels = c("More than once per week", "Once a week", "Less than once a week", "Never"))

df.intake24_item$age_cat <- factor(df.intake24_item$age_cat, levels = c("2-4y", "5-10y", "11-15y"))
df.intake24_item$age_sex_cat <- factor(df.intake24_item$age_sex_cat, levels = c("Female, 2-4y", "Female, 5-10y", "Female, 11-15y", "Male, 2-4y", "Male, 5-10y", "Male, 11-15y"))

df.intake24_recall$Month <- factor(df.intake24_recall$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August"))


# set survey weights and psu
svy.df.survey <- svydesign(id = ~psu, weights = ~sampleweight_all, data = df.survey)
svy.df.intake24_participant <- svydesign(id = ~psu, weights = ~sampleweight_all, data = df.intake24_participant)

# set "prefer not to say" sex to missing
df.intake24_participant <- df.intake24_participant %>%
  mutate(Sex = na_if(Sex, "Prefer not to say"))

#remove 'prefer not to say' in sex
df.intake24_participant_fm <- df.intake24_participant %>%
  filter(!is.na(Sex))


# survey weights without 'prefer not to say' in sex
svy.df.survey_fm <- svydesign(id = ~psu, weights = ~sampleweight_fm_2, data = df.survey)
svy.df.intake24_participant_fm <- svydesign(id = ~psu, weights = ~sampleweight_fm_2, data = df.intake24_participant_fm)


# Methodology ####

# Siblings
df.survey %>%
  filter(grepl("^3", UserID)) %>% #filter UserIDs starting with 3, ^ indicates 'starts with'
  summarise(count = n_distinct(UserID)) 

# SIMD
table(df.intake24_participant$simd_quintile)

# Device
table(df.intake24_participant$Device_group)

# Secondary school participants completing recall themselves
table(df.survey$ChildCompletedFoodDiary)

# Number of recalls
table(df.intake24_participant$NumberOfRecalls)

# Day of week
table(df.intake24_recall$DayofWeek)

# Month
df.intake24_recall %>%
  select(Month) %>%  
  tbl_summary(statistic = all_categorical() ~ c("{p}% ({n})")) %>%  
  bold_labels() %>% 
  as_flex_table() 

# Sample characteristics ####
table.demo <- tbl_svysummary(
  svy.df.survey,
  label = c(Age ~ "Age, years",
            age_cat ~ "Age group",
            simd_quintile ~ "Scottish Index of Multiple Deprivation",
            Education ~ "Stage of education",
            adults_cat ~ "Number of adults in household",
            children_cat ~ "Number of children in household"), 
  statistic = list(all_continuous() ~ "{median} ({p25}, {p75})", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  missing = "ifany",
  include = c(Age, age_cat, Ethnicity, simd_quintile, Education, adults_cat, children_cat)) %>%
  bold_labels() %>% 
  as_flex_table()

table.sex <- tbl_svysummary(
  svy.df.survey_fm,
  by = Sex,
  label = c(Age ~ "Age, years",
            age_cat ~ "Age group",
            simd_quintile ~ "Scottish Index of Multiple Deprivation",
            Education ~ "Stage of education",
            adults_cat ~ "Number of adults in household",
            children_cat ~ "Number of children in household"), 
  statistic = list(all_continuous() ~ "{median} ({p25}, {p75})", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  missing = "ifany",
  include = c(Age, age_cat, Ethnicity, simd_quintile, Education, adults_cat, children_cat)) %>%
  bold_labels() %>% 
  as_flex_table()

save_as_docx("Demographics" = table.demo, "By Sex" = table.sex, path="Output/Table_Demographics.docx")



# Scottish Dietary Goals - Chpt 4 ####

## Means ####

### overall ####
table.sdg.mean_annex <- tbl_svysummary(
  svy.df.intake24_participant, 
  label = c(             
    UI_Energykcal ~ "Energy density, kcal/100g",
    UI_Fat_PropFoodEnergy ~ "Fat, % of energy excluding alcohol",
    UI_SatFat_PropFoodEnergy ~ "Saturated fat, % of energy excluding alcohol",
    UI_TransFat_PropFoodEnergy ~ "Trans fat, % of energy excluding alcohol",
    UI_FreeSugars_PropFoodEnergy ~ "Free sugars, % of energy excluding alcohol",
    UI_Carbs_PropFoodEnergy ~ "Carbohydrates, % of energy excluding alcohol",
    UI_AOACFibreg ~ "Fibre, g/d",
    UI_Saltg ~ "Salt, g/d",
    UI_FV_Portions_Total ~ "Fruit and vegetables, portions/d",
    UI_FV_g_Total ~ "Fruit and vegetables, g/d",
    UI_RRPMg ~ "Red and red processed meat, g/d",
    SDG_oilyfish ~ "Oily fish"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}", 
                   all_categorical() ~ "{p}%"),
  digits = list(UI_Energykcal ~ c(0,0), 
                UI_Fat_PropFoodEnergy ~ c(0,0),
                UI_SatFat_PropFoodEnergy ~ c(0,0),
                UI_TransFat_PropFoodEnergy ~ c(0,0),
                UI_FreeSugars_PropFoodEnergy ~ c(0,0),
                UI_Carbs_PropFoodEnergy ~ c(0,0),
                UI_AOACFibreg ~ c(1,0),
                UI_Saltg ~ c(1,0),
                UI_FV_Portions_Total ~ c(1,0),
                UI_FV_g_Total ~ c(0,0),
                UI_RRPMg ~ c(0,0),
                all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(UI_Energykcal, UI_Fat_PropFoodEnergy, UI_SatFat_PropFoodEnergy, UI_TransFat_PropFoodEnergy, UI_FreeSugars_PropFoodEnergy, UI_Carbs_PropFoodEnergy, 
              UI_AOACFibreg, UI_Saltg, UI_FV_Portions_Total, UI_FV_g_Total, UI_RRPMg, SDG_oilyfish)) %>%
  bold_labels() %>%
  as_flex_table()

table.sdg.mean_annex

save_as_docx(table.sdg.mean_annex, path="Output/Table_SDG_Means_Overall.docx")



### by demographics ####
#sex
table.sdg.mean.sex_annex <- tbl_svysummary(
  svy.df.intake24_participant_fm, 
  by = Sex,
  label = c(             
    UI_Energykcal ~ "Energy density, kcal/100g",
    UI_Fat_PropFoodEnergy ~ "Fat, % of energy excluding alcohol",
    UI_SatFat_PropFoodEnergy ~ "Saturated fat, % of energy excluding alcohol",
    UI_TransFat_PropFoodEnergy ~ "Trans fat, % of energy excluding alcohol",
    UI_FreeSugars_PropFoodEnergy ~ "Free sugars, % of energy excluding alcohol",
    UI_Carbs_PropFoodEnergy ~ "Carbohydrates, % of energy excluding alcohol",
    UI_AOACFibreg ~ "Fibre, g/d",
    UI_Saltg ~ "Salt, g/d",
    UI_FV_Portions_Total ~ "Fruit and vegetables, portions/d",
    UI_FV_g_Total ~ "Fruit and vegetables, g/d",
    UI_RRPMg ~ "Red and red processed meat, g/d",
    SDG_oilyfish ~ "Oily fish"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                   {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  digits = list(UI_Energykcal ~ c(0,0), 
                UI_Fat_PropFoodEnergy ~ c(0,0),
                UI_SatFat_PropFoodEnergy ~ c(0,0),
                UI_TransFat_PropFoodEnergy ~ c(0,0),
                UI_FreeSugars_PropFoodEnergy ~ c(0,0),
                UI_Carbs_PropFoodEnergy ~ c(0,0),
                UI_AOACFibreg ~ c(1,0),
                UI_Saltg ~ c(1,0),
                UI_FV_Portions_Total ~ c(1,0),
                UI_FV_g_Total ~ c(0,0),
                UI_RRPMg ~ c(0,0),
                all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(Sex, UI_Energykcal, UI_Fat_PropFoodEnergy, UI_SatFat_PropFoodEnergy, UI_TransFat_PropFoodEnergy, UI_FreeSugars_PropFoodEnergy, UI_Carbs_PropFoodEnergy, 
              UI_AOACFibreg, UI_Saltg, UI_FV_Portions_Total, UI_FV_g_Total, UI_RRPMg, SDG_oilyfish)) %>%
  bold_labels() %>%
  add_p(test = list(all_continuous() ~ "svy.wilcox.test",
                    all_categorical() ~ "svy.chisq.test")) %>%
  add_significance_stars()%>%
  add_overall()%>%
  as_flex_table()

table.sdg.mean.sex_annex


#by age
table.sdg.mean.age_annex <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(             
    UI_Energykcal ~ "Energy density, kcal/100g",
    UI_Fat_PropFoodEnergy ~ "Fat, % of energy excluding alcohol",
    UI_SatFat_PropFoodEnergy ~ "Saturated fat, % of energy excluding alcohol",
    UI_TransFat_PropFoodEnergy ~ "Trans fat, % of energy excluding alcohol",
    UI_FreeSugars_PropFoodEnergy ~ "Free sugars, % of energy excluding alcohol",
    UI_Carbs_PropFoodEnergy ~ "Carbohydrates, % of energy excluding alcohol",
    UI_AOACFibreg ~ "Fibre, g/d",
    UI_Saltg ~ "Salt, g/d",
    UI_FV_Portions_Total ~ "Fruit and vegetables, portions/d",
    UI_FV_g_Total ~ "Fruit and vegetables, g/d",
    UI_RRPMg ~ "Red and red processed meat, g/d",
    SDG_oilyfish ~ "Oily fish"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  digits = list(UI_Energykcal ~ c(0,0), 
                UI_Fat_PropFoodEnergy ~ c(0,0),
                UI_SatFat_PropFoodEnergy ~ c(0,0),
                UI_TransFat_PropFoodEnergy ~ c(0,0),
                UI_FreeSugars_PropFoodEnergy ~ c(0,0),
                UI_Carbs_PropFoodEnergy ~ c(0,0),
                UI_AOACFibreg ~ c(1,0),
                UI_Saltg ~ c(1,0),
                UI_FV_Portions_Total ~ c(1,0),
                UI_FV_g_Total ~ c(0,0),
                UI_RRPMg ~ c(0,0),
                all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(age_cat, UI_Energykcal, UI_Fat_PropFoodEnergy, UI_SatFat_PropFoodEnergy, UI_TransFat_PropFoodEnergy, UI_FreeSugars_PropFoodEnergy, UI_Carbs_PropFoodEnergy, 
              UI_AOACFibreg, UI_Saltg, UI_FV_Portions_Total, UI_FV_g_Total, UI_RRPMg, SDG_oilyfish)) %>%
  bold_labels() %>% 
  add_p(test = list(all_continuous() ~ "svy.wilcox.test",
                    all_categorical() ~ "svy.chisq.test")) %>%
  add_significance_stars()%>%
  add_overall()%>%
  as_flex_table()

#by age and sex
table.sdg.mean.age.sex_annex <- tbl_svysummary(
  svy.df.intake24_participant_fm, 
  by = age_sex_cat,
  label = c(             
    UI_Energykcal ~ "Energy density, kcal/100g",
    UI_Fat_PropFoodEnergy ~ "Fat, % of energy excluding alcohol",
    UI_SatFat_PropFoodEnergy ~ "Saturated fat, % of energy excluding alcohol",
    UI_TransFat_PropFoodEnergy ~ "Trans fat, % of energy excluding alcohol",
    UI_FreeSugars_PropFoodEnergy ~ "Free sugars, % of energy excluding alcohol",
    UI_Carbs_PropFoodEnergy ~ "Carbohydrates, % of energy excluding alcohol",
    UI_AOACFibreg ~ "Fibre, g/d",
    UI_Saltg ~ "Salt, g/d",
    UI_FV_Portions_Total ~ "Fruit and vegetables, portions/d",
    UI_FV_g_Total ~ "Fruit and vegetables, g/d",
    UI_RRPMg ~ "Red and red processed meat, g/d",
    SDG_oilyfish ~ "Oily fish"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  digits = list(UI_Energykcal ~ c(0,0), 
                UI_Fat_PropFoodEnergy ~ c(0,0),
                UI_SatFat_PropFoodEnergy ~ c(0,0),
                UI_TransFat_PropFoodEnergy ~ c(0,0),
                UI_FreeSugars_PropFoodEnergy ~ c(0,0),
                UI_Carbs_PropFoodEnergy ~ c(0,0),
                UI_AOACFibreg ~ c(1,0),
                UI_Saltg ~ c(1,0),
                UI_FV_Portions_Total ~ c(1,0),
                UI_FV_g_Total ~ c(0,0),
                UI_RRPMg ~ c(0,0),
                all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(age_sex_cat, UI_Energykcal, UI_Fat_PropFoodEnergy, UI_SatFat_PropFoodEnergy, UI_TransFat_PropFoodEnergy, UI_FreeSugars_PropFoodEnergy, UI_Carbs_PropFoodEnergy, 
              UI_AOACFibreg, UI_Saltg, UI_FV_Portions_Total, UI_FV_g_Total, UI_RRPMg, SDG_oilyfish)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  add_overall()%>%
  as_flex_table()

#by simd
table.sdg.mean.simd_annex <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(             
    UI_Energykcal ~ "Energy density, kcal/100g",
    UI_Fat_PropFoodEnergy ~ "Fat, % of energy excluding alcohol",
    UI_SatFat_PropFoodEnergy ~ "Saturated fat, % of energy excluding alcohol",
    UI_TransFat_PropFoodEnergy ~ "Trans fat, % of energy excluding alcohol",
    UI_FreeSugars_PropFoodEnergy ~ "Free sugars, % of energy excluding alcohol",
    UI_Carbs_PropFoodEnergy ~ "Carbohydrates, % of energy excluding alcohol",
    UI_AOACFibreg ~ "Fibre, g/d",
    UI_Saltg ~ "Salt, g/d",
    UI_FV_Portions_Total ~ "Fruit and vegetables, portions/d",
    UI_FV_g_Total ~ "Fruit and vegetables, g/d",
    UI_RRPMg ~ "Red and red processed meat, g/d",
    SDG_oilyfish ~ "Oily fish"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  digits = list(UI_Energykcal ~ c(0,0), 
                UI_Fat_PropFoodEnergy ~ c(0,0),
                UI_SatFat_PropFoodEnergy ~ c(0,0),
                UI_TransFat_PropFoodEnergy ~ c(0,0),
                UI_FreeSugars_PropFoodEnergy ~ c(0,0),
                UI_Carbs_PropFoodEnergy ~ c(0,0),
                UI_AOACFibreg ~ c(1,0),
                UI_Saltg ~ c(1,0),
                UI_FV_Portions_Total ~ c(1,0),
                UI_FV_g_Total ~ c(0,0),
                UI_RRPMg ~ c(0,0),
                all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(simd_quintile, UI_Energykcal, UI_Fat_PropFoodEnergy, UI_SatFat_PropFoodEnergy, UI_TransFat_PropFoodEnergy, UI_FreeSugars_PropFoodEnergy, UI_Carbs_PropFoodEnergy, 
              UI_AOACFibreg, UI_Saltg, UI_FV_Portions_Total, UI_FV_g_Total, UI_RRPMg, SDG_oilyfish)) %>%
  bold_labels() %>% 
  add_p(test = list(all_continuous() ~ "svy.wilcox.test",
                    all_categorical() ~ "svy.chisq.test")) %>%
  add_significance_stars()%>%
  add_overall()%>%
  as_flex_table()

save_as_docx(table.sdg.mean.sex_annex, table.sdg.mean.age_annex, table.sdg.mean.age.sex_annex, table.sdg.mean.simd_annex, path="Output/Table_SDG_Means_ByDemographics.docx")


table.sdg.mean.sex_annex
table.sdg.mean.age_annex
table.sdg.mean.age.sex_annex
table.sdg.mean.simd_annex


###output to Excel ####

#create workbook
wb <- wb_workbook()

#create lists for tabs
sdg_sheet_names <- c("Age", "Sex", "Age_Sex", "SIMD")
sdg_tables <- list(table.sdg.mean.age_annex, table.sdg.mean.sex_annex, table.sdg.mean.age.sex_annex, table.sdg.mean.simd_annex)

#assign flextables to tabs 
for (i in seq_along(sdg_sheet_names)) {
  wb <- wb_add_worksheet(wb, sheet = sdg_sheet_names[i])
  wb <- wb_add_flextable(wb, sheet = sdg_sheet_names[i], sdg_tables[[i]])
}

#save workbook
wb_save(wb, file = paste0("Output/Annexe_Tables_SDGs_", format(Sys.time(), "%d%m%Y"), ".xlsx"))




## Tables - Main Text ####
table.sdg.mean.sex <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = Sex,
  label = c(             
    UI_Energykcal ~ "Energy density, kcal/100g",
    SDG_Energy ~ "Energy density, % meeting goal",
    UI_Fatg ~ "Fat, g/day",
    UI_Fat_PropFoodEnergy ~ "Fat, % of energy excluding alcohol",
    UI_SDG_Fat ~ "Fat, % meeting goal",
    UI_SatFatg ~ "Saturated fat, g/day",
    UI_SatFat_PropFoodEnergy ~ "Saturated fat, % of energy excluding alcohol",
    UI_SDG_SatFat ~ "Saturated fat, % meeting goal",
    UI_TransFatg ~ "Trans fat, g/day",
    UI_TransFat_PropFoodEnergy ~ "Trans fat, % of energy excluding alcohol",
    UI_SDG_TransFat ~ "Trans fat, % meeting goal",
    UI_FreeSugarsg ~ "Free sugars, g/day",
    UI_FreeSugars_PropFoodEnergy ~ "Free sugars, % of energy excluding alcohol",
    UI_SDG_FreeSugar ~ "Free sugars, % meeting goal",
    UI_Carbohydrateg ~ "Carbohydrates, g/day",
    UI_Carbs_PropFoodEnergy ~ "Carbohydrates, % of energy excluding alcohol",
    UI_SDG_Carbs ~ "Carbohydrates, % meeting goal",
    UI_AOACFibreg ~ "Fibre, g/day",
    UI_SDG_Fibre ~ "Fibre, % meeting goal",
    UI_Saltg ~ "Salt, g/day",
    UI_SDG_Salt ~ "Salt, % meeting goal",
    UI_FV_Portions_Total ~ "Fruit and vegetables, portions/day",
    UI_FV_g_Total ~ "Fruit and vegetables, g/day",
    UI_RRPMg ~ "Red and red processed meat, g/day",
    SDG_oilyfish ~ "Oily fish"),
  statistic = list(UI_Energykcal ~ "{mean}", 
                   UI_Fatg ~ "{mean}", 
                   UI_Fat_PropFoodEnergy ~ "{mean}%",
                   UI_SatFatg ~ "{mean}", 
                   UI_SatFat_PropFoodEnergy ~ "{mean}%",
                   UI_TransFatg ~ "{mean}", 
                   UI_TransFat_PropFoodEnergy ~ "{mean}%",
                   UI_FreeSugarsg ~ "{mean}", 
                   UI_FreeSugars_PropFoodEnergy ~ "{mean}%",
                   UI_Carbohydrateg ~ "{mean}", 
                   UI_Carbs_PropFoodEnergy ~ "{mean}%",
                   UI_AOACFibreg ~ "{mean}",
                   UI_Saltg ~ "{mean}",
                   UI_FV_Portions_Total ~ "{mean}",
                   UI_FV_g_Total ~ "{mean}",
                   UI_RRPMg ~ "{mean}",
                   all_categorical() ~ "{p}%"),
  digits = list(UI_Energykcal ~ c(0,0), 
                UI_Fatg ~ c(0,0),
                UI_Fat_PropFoodEnergy ~ c(0,0),
                UI_SatFatg ~ c(0,0),
                UI_SatFat_PropFoodEnergy ~ c(0,0),
                UI_TransFatg ~ c(1,0),
                UI_TransFat_PropFoodEnergy ~ c(1,0),
                UI_FreeSugarsg ~ c(0,0),
                UI_FreeSugars_PropFoodEnergy ~ c(0,0),
                UI_Carbohydrateg ~ c(0,0),
                UI_Carbs_PropFoodEnergy ~ c(0,0),
                UI_AOACFibreg ~ c(1,0),
                UI_Saltg ~ c(1,0),
                UI_FV_Portions_Total ~ c(1,0),
                UI_FV_g_Total ~ c(0,0),
                UI_RRPMg ~ c(0,0),
                all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(Sex, UI_Energykcal, SDG_Energy, UI_Fatg, UI_Fat_PropFoodEnergy, UI_SDG_Fat, UI_SatFatg, UI_SatFat_PropFoodEnergy, UI_SDG_SatFat, UI_TransFatg, UI_TransFat_PropFoodEnergy, UI_SDG_TransFat, 
              UI_FreeSugarsg, UI_FreeSugars_PropFoodEnergy, UI_SDG_FreeSugar, UI_Carbohydrateg, UI_Carbs_PropFoodEnergy, UI_SDG_Carbs, UI_AOACFibreg, UI_SDG_Fibre, UI_Saltg, UI_SDG_Salt, UI_FV_Portions_Total, 
              UI_FV_g_Total, UI_RRPMg, SDG_oilyfish)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  add_overall()

table.sdg.mean.age <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = age_cat,
  label = c(             
    UI_Energykcal ~ "Energy density, kcal/100g",
    SDG_Energy ~ "Energy density, % meeting goal",
    UI_Fatg ~ "Fat, g/day",
    UI_Fat_PropFoodEnergy ~ "Fat, % of energy excluding alcohol",
    UI_SDG_Fat ~ "Fat, % meeting goal",
    UI_SatFatg ~ "Saturated fat, g/day",
    UI_SatFat_PropFoodEnergy ~ "Saturated fat, % of energy excluding alcohol",
    UI_SDG_SatFat ~ "Saturated fat, % meeting goal",
    UI_TransFatg ~ "Trans fat, g/day",
    UI_TransFat_PropFoodEnergy ~ "Trans fat, % of energy excluding alcohol",
    UI_SDG_TransFat ~ "Trans fat, % meeting goal",
    UI_FreeSugarsg ~ "Free sugars, g/day",
    UI_FreeSugars_PropFoodEnergy ~ "Free sugars, % of energy excluding alcohol",
    UI_SDG_FreeSugar ~ "Free sugars, % meeting goal",
    UI_Carbohydrateg ~ "Carbohydrates, g/day",
    UI_Carbs_PropFoodEnergy ~ "Carbohydrates, % of energy excluding alcohol",
    UI_SDG_Carbs ~ "Carbohydrates, % meeting goal",
    UI_AOACFibreg ~ "Fibre, g/day",
    UI_SDG_Fibre ~ "Fibre, % meeting goal",
    UI_Saltg ~ "Salt, g/day",
    UI_SDG_Salt ~ "Salt, % meeting goal",
    UI_FV_Portions_Total ~ "Fruit and vegetables, portions/day",
    UI_FV_g_Total ~ "Fruit and vegetables, g/day",
    UI_RRPMg ~ "Red and red processed meat, g/day",
    SDG_oilyfish ~ "Oily fish"),
  statistic = list(UI_Energykcal ~ "{mean}", 
                   UI_Fatg ~ "{mean}", 
                   UI_Fat_PropFoodEnergy ~ "{mean}%",
                   UI_SatFatg ~ "{mean}", 
                   UI_SatFat_PropFoodEnergy ~ "{mean}%",
                   UI_TransFatg ~ "{mean}", 
                   UI_TransFat_PropFoodEnergy ~ "{mean}%",
                   UI_FreeSugarsg ~ "{mean}", 
                   UI_FreeSugars_PropFoodEnergy ~ "{mean}%",
                   UI_Carbohydrateg ~ "{mean}", 
                   UI_Carbs_PropFoodEnergy ~ "{mean}%",
                   UI_AOACFibreg ~ "{mean}",
                   UI_Saltg ~ "{mean}",
                   UI_FV_Portions_Total ~ "{mean}",
                   UI_FV_g_Total ~ "{mean}",
                   UI_RRPMg ~ "{mean}",
                   all_categorical() ~ "{p}%"),
  digits = list(UI_Energykcal ~ c(0,0), 
                UI_Fatg ~ c(0,0),
                UI_Fat_PropFoodEnergy ~ c(0,0),
                UI_SatFatg ~ c(0,0),
                UI_SatFat_PropFoodEnergy ~ c(0,0),
                UI_TransFatg ~ c(1,0),
                UI_TransFat_PropFoodEnergy ~ c(1,0),
                UI_FreeSugarsg ~ c(0,0),
                UI_FreeSugars_PropFoodEnergy ~ c(0,0),
                UI_Carbohydrateg ~ c(0,0),
                UI_Carbs_PropFoodEnergy ~ c(0,0),
                UI_AOACFibreg ~ c(1,0),
                UI_Saltg ~ c(1,0),
                UI_FV_Portions_Total ~ c(1,0),
                UI_FV_g_Total ~ c(0,0),
                UI_RRPMg ~ c(0,0),
                all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(age_cat, UI_Energykcal, SDG_Energy, UI_Fatg, UI_Fat_PropFoodEnergy, UI_SDG_Fat, UI_SatFatg, UI_SatFat_PropFoodEnergy, UI_SDG_SatFat, UI_TransFatg, UI_TransFat_PropFoodEnergy, UI_SDG_TransFat, 
              UI_FreeSugarsg, UI_FreeSugars_PropFoodEnergy, UI_SDG_FreeSugar, UI_Carbohydrateg, UI_Carbs_PropFoodEnergy, UI_SDG_Carbs, UI_AOACFibreg, UI_SDG_Fibre, UI_Saltg, UI_SDG_Salt, UI_FV_Portions_Total, 
              UI_FV_g_Total, UI_RRPMg, SDG_oilyfish)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()

table.sdg.mean.age.sex <- tbl_svysummary(
  svy.df.intake24_participant_fm, 
  by = age_sex_cat,
  label = c(             
    UI_Energykcal ~ "Energy density, kcal/100g",
    SDG_Energy ~ "Energy density, % meeting goal",
    UI_Fatg ~ "Fat, g/day",
    UI_Fat_PropFoodEnergy ~ "Fat, % of energy excluding alcohol",
    UI_SDG_Fat ~ "Fat, % meeting goal",
    UI_SatFatg ~ "Saturated fat, g/day",
    UI_SatFat_PropFoodEnergy ~ "Saturated fat, % of energy excluding alcohol",
    UI_SDG_SatFat ~ "Saturated fat, % meeting goal",
    UI_TransFatg ~ "Trans fat, g/day",
    UI_TransFat_PropFoodEnergy ~ "Trans fat, % of energy excluding alcohol",
    UI_SDG_TransFat ~ "Trans fat, % meeting goal",
    UI_FreeSugarsg ~ "Free sugars, g/day",
    UI_FreeSugars_PropFoodEnergy ~ "Free sugars, % of energy excluding alcohol",
    UI_SDG_FreeSugar ~ "Free sugars, % meeting goal",
    UI_Carbohydrateg ~ "Carbohydrates, g/day",
    UI_Carbs_PropFoodEnergy ~ "Carbohydrates, % of energy excluding alcohol",
    UI_SDG_Carbs ~ "Carbohydrates, % meeting goal",
    UI_AOACFibreg ~ "Fibre, g/day",
    UI_SDG_Fibre ~ "Fibre, % meeting goal",
    UI_Saltg ~ "Salt, g/day",
    UI_SDG_Salt ~ "Salt, % meeting goal",
    UI_FV_Portions_Total ~ "Fruit and vegetables, portions/day",
    UI_FV_g_Total ~ "Fruit and vegetables, g/day",
    UI_RRPMg ~ "Red and red processed meat, g/day",
    SDG_oilyfish ~ "Oily fish"),
  statistic = list(UI_Energykcal ~ "{mean}", 
                   UI_Fatg ~ "{mean}", 
                   UI_Fat_PropFoodEnergy ~ "{mean}%",
                   UI_SatFatg ~ "{mean}", 
                   UI_SatFat_PropFoodEnergy ~ "{mean}%",
                   UI_TransFatg ~ "{mean}", 
                   UI_TransFat_PropFoodEnergy ~ "{mean}%",
                   UI_FreeSugarsg ~ "{mean}", 
                   UI_FreeSugars_PropFoodEnergy ~ "{mean}%",
                   UI_Carbohydrateg ~ "{mean}", 
                   UI_Carbs_PropFoodEnergy ~ "{mean}%",
                   UI_AOACFibreg ~ "{mean}",
                   UI_Saltg ~ "{mean}",
                   UI_FV_Portions_Total ~ "{mean}",
                   UI_FV_g_Total ~ "{mean}",
                   UI_RRPMg ~ "{mean}",
                   all_categorical() ~ "{p}%"),
  digits = list(UI_Energykcal ~ c(0,0), 
                UI_Fatg ~ c(0,0),
                UI_Fat_PropFoodEnergy ~ c(0,0),
                UI_SatFatg ~ c(0,0),
                UI_SatFat_PropFoodEnergy ~ c(0,0),
                UI_TransFatg ~ c(1,0),
                UI_TransFat_PropFoodEnergy ~ c(1,0),
                UI_FreeSugarsg ~ c(0,0),
                UI_FreeSugars_PropFoodEnergy ~ c(0,0),
                UI_Carbohydrateg ~ c(0,0),
                UI_Carbs_PropFoodEnergy ~ c(0,0),
                UI_AOACFibreg ~ c(1,0),
                UI_Saltg ~ c(1,0),
                UI_FV_Portions_Total ~ c(1,0),
                UI_FV_g_Total ~ c(0,0),
                UI_RRPMg ~ c(0,0),
                all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(age_sex_cat, UI_Energykcal, SDG_Energy, UI_Fatg, UI_Fat_PropFoodEnergy, UI_SDG_Fat, UI_SatFatg, UI_SatFat_PropFoodEnergy, UI_SDG_SatFat, UI_TransFatg, UI_TransFat_PropFoodEnergy, UI_SDG_TransFat, 
              UI_FreeSugarsg, UI_FreeSugars_PropFoodEnergy, UI_SDG_FreeSugar, UI_Carbohydrateg, UI_Carbs_PropFoodEnergy, UI_SDG_Carbs, UI_AOACFibreg, UI_SDG_Fibre, UI_Saltg, UI_SDG_Salt, UI_FV_Portions_Total, 
              UI_FV_g_Total, UI_RRPMg, SDG_oilyfish)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()

table.sdg.mean.simd <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = simd_quintile,
  label = c(             
    UI_Energykcal ~ "Energy density, kcal/100g",
    SDG_Energy ~ "Energy density, % meeting goal",
    UI_Fatg ~ "Fat, g/day",
    UI_Fat_PropFoodEnergy ~ "Fat, % of energy excluding alcohol",
    UI_SDG_Fat ~ "Fat, % meeting goal",
    UI_SatFatg ~ "Saturated fat, g/day",
    UI_SatFat_PropFoodEnergy ~ "Saturated fat, % of energy excluding alcohol",
    UI_SDG_SatFat ~ "Saturated fat, % meeting goal",
    UI_TransFatg ~ "Trans fat, g/day",
    UI_TransFat_PropFoodEnergy ~ "Trans fat, % of energy excluding alcohol",
    UI_SDG_TransFat ~ "Trans fat, % meeting goal",
    UI_FreeSugarsg ~ "Free sugars, g/day",
    UI_FreeSugars_PropFoodEnergy ~ "Free sugars, % of energy excluding alcohol",
    UI_SDG_FreeSugar ~ "Free sugars, % meeting goal",
    UI_Carbohydrateg ~ "Carbohydrates, g/day",
    UI_Carbs_PropFoodEnergy ~ "Carbohydrates, % of energy excluding alcohol",
    UI_SDG_Carbs ~ "Carbohydrates, % meeting goal",
    UI_AOACFibreg ~ "Fibre, g/day",
    UI_SDG_Fibre ~ "Fibre, % meeting goal",
    UI_Saltg ~ "Salt, g/day",
    UI_SDG_Salt ~ "Salt, % meeting goal",
    UI_FV_Portions_Total ~ "Fruit and vegetables, portions/day",
    UI_FV_g_Total ~ "Fruit and vegetables, g/day",
    UI_RRPMg ~ "Red and red processed meat, g/day",
    SDG_oilyfish ~ "Oily fish"),
  statistic = list(UI_Energykcal ~ "{mean}", 
                   UI_Fatg ~ "{mean}", 
                   UI_Fat_PropFoodEnergy ~ "{mean}%",
                   UI_SatFatg ~ "{mean}", 
                   UI_SatFat_PropFoodEnergy ~ "{mean}%",
                   UI_TransFatg ~ "{mean}", 
                   UI_TransFat_PropFoodEnergy ~ "{mean}%",
                   UI_FreeSugarsg ~ "{mean}", 
                   UI_FreeSugars_PropFoodEnergy ~ "{mean}%",
                   UI_Carbohydrateg ~ "{mean}", 
                   UI_Carbs_PropFoodEnergy ~ "{mean}%",
                   UI_AOACFibreg ~ "{mean}",
                   UI_Saltg ~ "{mean}",
                   UI_FV_Portions_Total ~ "{mean}",
                   UI_FV_g_Total ~ "{mean}",
                   UI_RRPMg ~ "{mean}",
                   all_categorical() ~ "{p}%"),
  digits = list(UI_Energykcal ~ c(0,0), 
                UI_Fatg ~ c(0,0),
                UI_Fat_PropFoodEnergy ~ c(0,0),
                UI_SatFatg ~ c(0,0),
                UI_SatFat_PropFoodEnergy ~ c(0,0),
                UI_TransFatg ~ c(1,0),
                UI_TransFat_PropFoodEnergy ~ c(1,0),
                UI_FreeSugarsg ~ c(0,0),
                UI_FreeSugars_PropFoodEnergy ~ c(0,0),
                UI_Carbohydrateg ~ c(0,0),
                UI_Carbs_PropFoodEnergy ~ c(0,0),
                UI_AOACFibreg ~ c(1,0),
                UI_Saltg ~ c(1,0),
                UI_FV_Portions_Total ~ c(1,0),
                UI_FV_g_Total ~ c(0,0),
                UI_RRPMg ~ c(0,0),
                all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(simd_quintile, UI_Energykcal, SDG_Energy, UI_Fatg, UI_Fat_PropFoodEnergy, UI_SDG_Fat, UI_SatFatg, UI_SatFat_PropFoodEnergy, UI_SDG_SatFat, UI_TransFatg, UI_TransFat_PropFoodEnergy, UI_SDG_TransFat, 
              UI_FreeSugarsg, UI_FreeSugars_PropFoodEnergy, UI_SDG_FreeSugar, UI_Carbohydrateg, UI_Carbs_PropFoodEnergy, UI_SDG_Carbs, UI_AOACFibreg, UI_SDG_Fibre, UI_Saltg, UI_SDG_Salt, UI_FV_Portions_Total, 
              UI_FV_g_Total, UI_RRPMg, SDG_oilyfish)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()



### Oily fish ####
table.sdg.oilyfish.sex <- tbl_svysummary(
  svy.df.intake24_participant_fm, 
  by = Sex,
  label = c(             
    SDG_cat_oilyfish ~ "Oily fish"),
  statistic = list(all_categorical() ~ "{p}%"),
  digits = list(all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(Sex, SDG_cat_oilyfish)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars() %>%
  add_overall()


table.sdg.oilyfish.age <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = age_cat,
  label = c(             
    SDG_cat_oilyfish ~ "Oily fish"),
  statistic = list(
    all_categorical() ~ "{p}%"),
  digits = list(
    all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(age_cat, SDG_cat_oilyfish)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()

table.sdg.oilyfish.age.sex <- tbl_svysummary(
  svy.df.intake24_participant_fm, 
  by = age_sex_cat,
  label = c(             
    SDG_cat_oilyfish ~ "Oily fish"),
  statistic = list(
    all_categorical() ~ "{p}%"),
  digits = list(
    all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(age_sex_cat, SDG_cat_oilyfish)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()


table.sdg.oilyfish.simd <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = simd_quintile,
  label = c(             
    SDG_cat_oilyfish ~ "Oily fish"),
  statistic = list(
    all_categorical() ~ "{p}%"),
  digits = list(
    all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(simd_quintile, SDG_cat_oilyfish)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()

table.sdg.oilyfish.simd



# Convert to main text format


#function to convert tblsummary and create dataframes for each nutrient
process_tables <- function(tables, prefix) {
  processed_tables <- lapply(tables, function(table) {
    processed_table <- as.data.frame(t(as.data.frame(table)))
    colnames(processed_table) <- processed_table[1, ]
    processed_table <- processed_table[-1, ]
    processed_table <- clean_names(processed_table)
    processed_table %>% select(starts_with(prefix))
  })
  bind_rows(processed_tables)
}

# List of tables to process
tables <- list(
  sdg.sex = table.sdg.mean.sex,
  sdg.age = table.sdg.mean.age,
  sdg.age.sex = table.sdg.mean.age.sex,
  sdg.simd = table.sdg.mean.simd
)



# List of nutrients
prefixes <- c("energy", "fat", "saturated_fat", "trans_fat", "free_sugars", "carbohydrates", 
              "fibre", "salt", "fruit_and_vegetables", "red_and_red_processed", "oily_fish")


# Process tables and rename
for (prefix in prefixes) {
  assign(paste0("sdg.", gsub("_", ".", prefix)), process_tables(tables, prefix))
}


#list of dataframe
sdg_main_tables <- list(sdg.energy, sdg.fat, sdg.saturated.fat, sdg.trans.fat, sdg.free.sugars, sdg.carbohydrates, 
                        sdg.fibre, sdg.salt, sdg.fruit.and.vegetables, sdg.red.and.red.processed, sdg.oily.fish)


#convert row names to a column
for (i in seq_along(sdg_main_tables)) {
  sdg_main_tables[[i]] <- sdg_main_tables[[i]] %>%
    rownames_to_column(var = "Demographics")
}


###output Excel ####

#create workbook
wb1 <- createWorkbook()

#create lists for tabs
sdg_main_sheet_names <- c("energy", "fat", "saturated_fat", "trans_fat", "free_sugars", "carbohydrates", 
                          "fibre", "salt", "fruit_and_vegetables", "red_and_red_processed", "oily_fish")

#assign dataframes to tabs 
for (i in seq_along(sdg_main_sheet_names)) {
  addWorksheet(wb1, sheet = sdg_main_sheet_names[i])
  writeData(wb1, sheet = sdg_main_sheet_names[i], x = sdg_main_tables[[i]])
}

#save workbook
saveWorkbook(wb1, file = paste0("Output/Text_Tables_SDGs_", format(Sys.time(), "%d%m%Y"), ".xlsx"))


#Oily fish export 
# List of tables to process
tables <- list(
  sdg.oilyfish.sex = table.sdg.oilyfish.sex,
  sdg.oilyfish.age = table.sdg.oilyfish.age,
  sdg.oilyfish.age.sex = table.sdg.oilyfish.age.sex,
  sdg.oilyfish.simd = table.sdg.oilyfish.simd
)


# Process each table - not running full function as don't need to select prefixes here
processed_tables <- lapply(tables, function(table) {
  as.data.frame(t(as.data.frame(table)))
})

# Set the first row as column names and remove that row from each data frame
processed_tables <- lapply(processed_tables, function(df) {
  colnames(df) <- df[1, ]
  df[-1, ]
})

# Clean column names
processed_tables <- lapply(processed_tables, function(df) {
  clean_names(df)
})

# Combine and relocate columns 
sdg_oily_fish <- bind_rows(processed_tables) %>%
  rownames_to_column(var = "Demographics")


#output to excel

write_xlsx(sdg_oily_fish, file = paste0("Output/Table_SDGs_Oily_Fish_", format(Sys.time(), "%d%m%Y"), ".xlsx"))


## Fruit and Vegetables ####

# Mean portions
# age
table.sdgs.vegfruit_age <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(UI_FV_Portions_Beans ~ "Beans (portions/day)",
            UI_FV_Portions_JuiceSmoothie ~ "Fruit Juice / Smoothie (portions/day)",
            UI_FV_Portions_DriedFruit ~ "Dried fruit (portions/day)",
            UI_FV_Portions_FreshFruit ~ "Fresh fruit (portions/day)",
            UI_FV_Portions_Veg ~ "Vegetables (portions/day)",
            UI_FV_Portions_Total ~ "Total vegetables & fruit (portions/day)"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  missing = "ifany",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(UI_FV_Portions_Beans, UI_FV_Portions_JuiceSmoothie, UI_FV_Portions_DriedFruit, 
              UI_FV_Portions_FreshFruit, UI_FV_Portions_Veg, UI_FV_Portions_Total)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



# sex
table.sdgs.vegfruit_sex <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(UI_FV_Portions_Beans ~ "Beans (portions/day)",
            UI_FV_Portions_JuiceSmoothie ~ "Fruit Juice / Smoothie (portions/day)",
            UI_FV_Portions_DriedFruit ~ "Dried fruit (portions/day)",
            UI_FV_Portions_FreshFruit ~ "Fresh fruit (portions/day)",
            UI_FV_Portions_Veg ~ "Vegetables (portions/day)",
            UI_FV_Portions_Total ~ "Total vegetables & fruit (portions/day)"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  missing = "ifany",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(UI_FV_Portions_Beans, UI_FV_Portions_JuiceSmoothie, UI_FV_Portions_DriedFruit, 
              UI_FV_Portions_FreshFruit, UI_FV_Portions_Veg, UI_FV_Portions_Total)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

# age/sex
table.sdgs.vegfruit_age_sex <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(UI_FV_Portions_Beans ~ "Beans (portions/day)",
            UI_FV_Portions_JuiceSmoothie ~ "Fruit Juice / Smoothie (portions/day)",
            UI_FV_Portions_DriedFruit ~ "Dried fruit (portions/day)",
            UI_FV_Portions_FreshFruit ~ "Fresh fruit (portions/day)",
            UI_FV_Portions_Veg ~ "Vegetables (portions/day)",
            UI_FV_Portions_Total ~ "Total vegetables & fruit (portions/day)"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  missing = "ifany",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(UI_FV_Portions_Beans, UI_FV_Portions_JuiceSmoothie, UI_FV_Portions_DriedFruit, 
              UI_FV_Portions_FreshFruit, UI_FV_Portions_Veg, UI_FV_Portions_Total)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#simd
table.sdgs.vegfruit_simd <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(UI_FV_Portions_Beans ~ "Beans (portions/day)",
            UI_FV_Portions_JuiceSmoothie ~ "Fruit Juice / Smoothie (portions/day)",
            UI_FV_Portions_DriedFruit ~ "Dried fruit (portions/day)",
            UI_FV_Portions_FreshFruit ~ "Fresh fruit (portions/day)",
            UI_FV_Portions_Veg ~ "Vegetables (portions/day)",
            UI_FV_Portions_Total ~ "Total vegetables & fruit (portions/day)"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  missing = "ifany",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(UI_FV_Portions_Beans, UI_FV_Portions_JuiceSmoothie, UI_FV_Portions_DriedFruit, 
              UI_FV_Portions_FreshFruit, UI_FV_Portions_Veg, UI_FV_Portions_Total)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


# mean grams
#Sex
table.sdgs.vegfruit_g_sex <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(UI_FV_Beansg ~ "Beans (g/day)",
            UI_FV_JuiceSmoothieg ~ "Fruit Juice / Smoothie (g/day)",
            UI_FV_DriedFruitg ~ "Dried fruit (g/day)",
            UI_FV_FreshFruitg ~ "Fresh fruit (g/day)",
            UI_FV_Vegg ~ "Vegetables (g/day)",
            UI_FV_g_Total ~ "Total vegetables & fruit (g/day)",
            UI_FV_g_Total_NoJuiceSmoothie ~ "Total vegetables & fruit - no juice (g/day)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(UI_FV_Beansg, UI_FV_JuiceSmoothieg, UI_FV_DriedFruitg, 
              UI_FV_FreshFruitg, UI_FV_Vegg, UI_FV_g_Total, UI_FV_g_Total_NoJuiceSmoothie)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() 

#Age
table.sdgs.vegfruit_g_age <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(UI_FV_Beansg ~ "Beans (g/day)",
            UI_FV_JuiceSmoothieg ~ "Fruit Juice / Smoothie (g/day)",
            UI_FV_DriedFruitg ~ "Dried fruit (g/day)",
            UI_FV_FreshFruitg ~ "Fresh fruit (g/day)",
            UI_FV_Vegg ~ "Vegetables (g/day)",
            UI_FV_g_Total ~ "Total vegetables & fruit (g/day)",
            UI_FV_g_Total_NoJuiceSmoothie ~ "Total vegetables & fruit - no juice (g/day)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(UI_FV_Beansg, UI_FV_JuiceSmoothieg, UI_FV_DriedFruitg, 
              UI_FV_FreshFruitg, UI_FV_Vegg, UI_FV_g_Total, UI_FV_g_Total_NoJuiceSmoothie)) %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() 


#age and sex
table.sdgs.vegfruit_g_age_sex <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(UI_FV_Beansg ~ "Beans (g/day)",
            UI_FV_JuiceSmoothieg ~ "Fruit Juice / Smoothie (g/day)",
            UI_FV_DriedFruitg ~ "Dried fruit (g/day)",
            UI_FV_FreshFruitg ~ "Fresh fruit (g/day)",
            UI_FV_Vegg ~ "Vegetables (g/day)",
            UI_FV_g_Total ~ "Total vegetables & fruit (g/day)",
            UI_FV_g_Total_NoJuiceSmoothie ~ "Total vegetables & fruit - no juice (g/day)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(UI_FV_Beansg, UI_FV_JuiceSmoothieg, UI_FV_DriedFruitg, 
              UI_FV_FreshFruitg, UI_FV_Vegg, UI_FV_g_Total, UI_FV_g_Total_NoJuiceSmoothie)) %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() 


#simd
table.sdgs.vegfruit_g_simd <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(UI_FV_Beansg ~ "Beans (g/day)",
            UI_FV_JuiceSmoothieg ~ "Fruit Juice / Smoothie (g/day)",
            UI_FV_DriedFruitg ~ "Dried fruit (g/day)",
            UI_FV_FreshFruitg ~ "Fresh fruit (g/day)",
            UI_FV_Vegg ~ "Vegetables (g/day)",
            UI_FV_g_Total ~ "Total vegetables & fruit (g/day)",
            UI_FV_g_Total_NoJuiceSmoothie ~ "Total vegetables & fruit - no juice (g/day)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(UI_FV_Beansg, UI_FV_JuiceSmoothieg, UI_FV_DriedFruitg, 
              UI_FV_FreshFruitg, UI_FV_Vegg, UI_FV_g_Total, UI_FV_g_Total_NoJuiceSmoothie)) %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() 

table.sdgs.vegfruit_g_sex

###output Excel ####

#Reformat for main text
# List of tables to process
tables <- list(
  sdg.fv.sex = table.sdgs.vegfruit_g_sex,
  sdg.fv.age = table.sdgs.vegfruit_g_age,
  sdg.fv.age.sex = table.sdgs.vegfruit_g_age_sex,
  sdg.fv.simd = table.sdgs.vegfruit_g_simd
)


# Process each table - not running full function as don't need to select prefixes here
processed_tables <- lapply(tables, function(table) {
  as.data.frame(t(as.data.frame(table)))
})

# Set the first row as column names and remove that row from each data frame
processed_tables <- lapply(processed_tables, function(df) {
  colnames(df) <- df[1, ]
  df[-1, ]
})

# Clean column names
processed_tables <- lapply(processed_tables, function(df) {
  clean_names(df)
})

# Combine and relocate columns 
sdg_FV_main_text <- bind_rows(processed_tables) %>%
  relocate(dried_fruit_g_day, .after = fresh_fruit_g_day) %>%
  relocate(fruit_juice_smoothie_g_day, .after = dried_fruit_g_day) %>%
  relocate(beans_g_day, .after = fruit_juice_smoothie_g_day) %>%
  rownames_to_column(var = "Demographics")


#output to excel

#Portions
#create workbook
wb2 <- wb_workbook()

#create lists for tabs
sdg_FV_sheet_names <- c("Age", "Sex", "Age_Sex", "SIMD")
sdg_FV_tables <- list(table.sdgs.vegfruit_age, table.sdgs.vegfruit_sex, table.sdgs.vegfruit_age_sex, table.sdgs.vegfruit_simd)

#assign flextables to tabs 
for (i in seq_along(sdg_FV_sheet_names)) {
  wb2 <- wb_add_worksheet(wb2, sheet = sdg_FV_sheet_names[i])
  wb2 <- wb_add_flextable(wb2, sheet = sdg_FV_sheet_names[i], sdg_FV_tables[[i]])
}

#save workbook
wb_save(wb2, file = paste0("Output/Table_SDGs_FV_Portions_", format(Sys.time(), "%d%m%Y"), ".xlsx"))

#grams
write_xlsx(sdg_FV_main_text, file = paste0("Output/Table_SDGs_FV_Grams_", format(Sys.time(), "%d%m%Y"), ".xlsx"))


  ## Most Commonly Reported Vegetables and Fruit ####
## Vegetables ####
Vegetables <- df.intake24_item %>%
  filter(MainFoodGroupCode %in% c(37, 36)) #36 is raw veg, 37 is cooked veg

Veg_sub <- Vegetables %>%
  select(UserID, SearchTerm, Description_English, Description_Local, FoodGroupCode, FoodGroupEnglish, ReadyMeal, RecallNo, NumberOfRecalls, NumberOfItems, Sex, Age, age_cat, simd_quintile, age_cat,age_sex_cat, NewMealName, MainFoodGroupCode, MainFoodGroupDesc)

Veg_sub$age_cat <- factor(Veg_sub$age_cat, levels = c("2-4y", "5-10y", "11-15y"))

#filter out non-single items
Veg_sub_cooked <- Veg_sub %>%
  filter(Description_English %in% c("Asparagus, cooked", "Aubergine, cooked", "Aubergine, fried",
                                    "Baby carrots, cooked", "Beansprouts", "Beetroot, cooked", "Beetroot, pickled in vinegar",
                                    "Black eyed beans", "Black eyed beans, canned", "Broad beans", "Broccoli puree", "Broccoli, boiled",
                                    "Broccoli, steamed/microwaved", "Brussels sprouts, cooked","Brussels sprouts, cooked",
                                    "Brussels sprouts, cooked from frozen", "Butter beans", "Butter beans, canned", "Butter beans",
                                    "Butternut squash mash", "Butternut squash, cooked/roasted with oil", "Butternut squash, cooked/roasted without oil",
                                    "Carrot puree", "Carrots", "Carrots, canned", "Carrots, cooked from frozen", "Carrots, fried",
                                    "Carrots, roasted", "Cauliflower puree", "Cauliflower, cooked", "Celeriac","Celery, cooked", "Chard","Chickpeas",
                                    "Chickpeas, canned", "Chinese leaves (cabbage), cooked", "Chinese mushrooms, dried", "Corn on the cob (sweetcorn)",
                                    "Courgette (zucchini), cooked", "Courgette (zucchini), fried", "Courgette (zucchini), fried in olive oil", "Crispy seaweed",
                                    "Fried cabbage", "Garlic Mushrooms (not breaded)", "Green beans/French beans", "Green beans/French beans, canned", "Green beans/French beans, fried",
                                    "Green pepper, cooked", "Green pepper, roasted", "Green/savoy cabbage", "Kale, cooked", "Kidney beans", "Kidney beans, canned",
                                    "Leek puree", "Leek, cooked", "Lentils", "Mange tout, cooked", "Mini sweetcorn (babycorn), cooked","Mini sweetcorn (babycorn), raw",
                                    "Mini sweetcorn / sweet corn (babycorn), canned", "Mixed peppers", "Mixed peppers, fried", "Mixed vegetables, canned", "Mixed vegetables stir fry (cooked from frozen)",
                                    "Mixed vegetables, cooked from fresh", "Mixed vegetables, cooked from frozen", "Mushrooms", "Mushrooms, fried", "Mushrooms, fried in butter",
                                    "Mushrooms, fried in olive oil", "Okra puree", "Okra, fried", "Onions, baked/roasted", "Onions, cooked", "Onions, fried", "Pak choi / Bok choi, cooked",
                                    "Parsnip, cooked", "Parsnip, roasted", "Peas", "Peas puree", "Peas, canned", "Peas, cooked from frozen", "Plantain", "Plantain, fried",
                                    "Pumpkin", "Red cabbage, cooked", "Red cabbage, pickled", "Red/yellow/orange pepper, cooked", "Red/yellow/orange pepper, fried",
                                    "Red/yellow/orange pepper, roasted", "Refried beans", "Runner beans, cooked", "Samphire, cooked", "Soya beans / edamame beans", "Spinach",
                                    "Spinach, fried", "Split peas", "Spring greens (cabbage), cooked", "Sprouting broccoli, cooked", "Sugar-snap peas", "Swede", "Sundried tomatoes",
                                    "Swede, mashed", "Sweet potato", "Sweet potato fries", "Sweet potato mash", "Sweet potato puree", "Sweet potato wedges", "Sweet potato, baked/raosted",
                                    "Sweet potato, baked/roasted, with oil", "Sweetcorn / sweet corn, canned", "Sweetcorn / sweet corn, canned, puree", "Sweetcorn / sweet corn, cooked", 
                                    "Tenderstem broccoli, cooked", "Tomato, fresh (pureed)", "Tomato, grilled", "Tomatoes, canned", "Tomatoes, canned (pureed)", "Turnip", "Turnip, mashed",
                                    "Water chestnuts", "White cabbage"))

#combine with raw veg
Veg_sub_raw <- Veg_sub %>%
  filter(MainFoodGroupCode == 36)

Veg_combo <- rbind(Veg_sub_raw, Veg_sub_cooked)

#create variable for linked items
Veg_combo <- Veg_combo %>%
  mutate(Veg_description = case_when(
    grepl("asparagus", Description_English, ignore.case = TRUE) ~ "Asparagus",
    grepl("Aubergine", Description_English, ignore.case = TRUE) ~ "Aubergine",
    grepl("carrots|carrot", Description_English, ignore.case = TRUE) ~ "Carrots",
    grepl("beans", Description_English, ignore.case = TRUE) ~ "Beans",
    grepl("beetroot", Description_English, ignore.case = TRUE) ~ "Beetroot",
    grepl("mushroom", Description_English, ignore.case = TRUE) ~ "Mushrooms",
    grepl("broccoli", Description_English, ignore.case = TRUE) ~ "Broccoli",
    grepl("brussel sprouts", Description_English, ignore.case = TRUE) ~ "Brussel sprouts",
    grepl("butternut", Description_English, ignore.case = TRUE) ~ "Butternut squash",
    grepl("cauliflower", Description_English, ignore.case = TRUE) ~ "Cauliflower",
    grepl("celery", Description_English, ignore.case = TRUE) ~ "Celery",
    grepl("chickpeas", Description_English, ignore.case = TRUE) ~ "Chickpeas",
    grepl("cabbage|pak choi", Description_English, ignore.case = TRUE) ~ "Cabbage",
    grepl("corn", Description_English, ignore.case = TRUE) ~ "Corn",
    grepl("pepper", Description_English, ignore.case = TRUE) ~ "Peppers",
    grepl("leek", Description_English, ignore.case = TRUE) ~ "Leek",
    grepl("parsnip", Description_English, ignore.case = TRUE) ~ "Parsnip",
    grepl("peas", Description_English, ignore.case = TRUE) ~ "Peas",
    grepl("spinach", Description_English, ignore.case = TRUE) ~ "Spinach",
    grepl("tomato", Description_English, ignore.case = TRUE) ~ "Tomatoes",
    grepl("potato", Description_English, ignore.case = TRUE) ~ "Potatoes",
    grepl("turnip", Description_English, ignore.case = TRUE) ~ "Turnip",
    grepl("courgette", Description_English, ignore.case = TRUE) ~ "Courgette",
    grepl("kale", Description_English, ignore.case = TRUE) ~ "Kale",
    grepl("onion", Description_English, ignore.case = TRUE) ~ "Onions",
    TRUE ~ Description_English
  ),)

#demographics

#sex
table.veg_sex <- Veg_combo %>%
  select(Sex, Veg_description) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = Sex,
              label = Veg_description ~ ""
  ) %>%
  add_overall() %>%
  modify_header(label ~ "**Vegetables**") %>%
  bold_labels()

#age
table.veg_age <- Veg_combo %>%
  select(age_cat, Veg_description) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = age_cat,
              label = Veg_description ~ ""
  ) %>%
  modify_header(label ~ "**Vegetables**") %>%
  bold_labels() 


#age/sex
table.veg_age_sex <- Veg_combo %>%
  select(age_sex_cat, Veg_description) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = age_sex_cat,
              label = Veg_description ~ ""
  ) %>%
  modify_header(label ~ "**Vegetables**") %>%
  bold_labels() 


#simd
table.veg_simd <- Veg_combo %>%
  select(simd_quintile, Veg_description) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = simd_quintile,
              label = Veg_description ~ ""
  ) %>%
  modify_header(label ~ "**Vegetables**") %>%
  bold_labels() 

#Reformat for main text
# List of tables to process
tables <- list(
  veg.sex = table.veg_sex,
  veg.age = table.veg_age,
  veg.age.sex = table.veg_age_sex,
  veg.simd = table.veg_simd
)


# Process each table - not running full function as don't need to select prefixes here
processed_tables <- lapply(tables, function(table) {
  as.data.frame(t(as.data.frame(table)))
})

# Set the first row as column names and remove that row from each data frame
processed_tables <- lapply(processed_tables, function(df) {
  colnames(df) <- df[1, ]
  df[-1, ]
})

# Clean column names
processed_tables <- lapply(processed_tables, function(df) {
  clean_names(df)
})

# Combine and relocate columns 
veg_main_text <- bind_rows(processed_tables) %>%
  rownames_to_column(var = "Demographics")

#select columns for export
Veg_main_text_export <- veg_main_text %>%
  select(Demographics, broccoli, carrots, corn, cucumber, peas, peppers, tomatoes)

#save workbook
write_xlsx(Veg_main_text_export, file = paste0("Output/Table_Vegetable_Freq_", format(Sys.time(), "%d%m%Y"), ".xlsx"))



##Fruit ####
Fruit <- df.intake24_item %>%
  filter(MainFoodGroupCode == 40)

Fruit_sub <- Fruit %>%
  select(UserID, SearchTerm, Description_English, Description_Local, FoodGroupCode, FoodGroupEnglish, RecallNo, NumberOfRecalls, NumberOfItems, Sex, Age, age_cat, simd_quintile, age_cat, age_sex_cat, NewMealName, MainFoodGroupCode, MainFoodGroupDesc)

#create new variable with linked items
Fruit_sub <- Fruit_sub %>%
  mutate(Fruit_description = case_when(
    grepl("apple", Description_English, ignore.case = TRUE) ~ "Apple",
    grepl("banana", Description_English, ignore.case = TRUE) ~ "Banana",
    grepl("avocado", Description_English, ignore.case = TRUE) ~ "Avocado",
    grepl("blackberries", Description_English, ignore.case = TRUE) ~ "Blackberries",
    grepl("blackcurrants", Description_English, ignore.case = TRUE) ~ "Blackcurrants",
    grepl("dates", Description_English, ignore.case = TRUE) ~ "Dates",
    grepl("apricots", Description_English, ignore.case = TRUE) ~ "Apricots",
    grepl("cocktail", Description_English, ignore.case = TRUE) ~ "Fruit cocktail, in syrup or juice",
    grepl("mango", Description_English, ignore.case = TRUE) ~ "Mango",
    grepl("melon", Description_English, ignore.case = TRUE) ~ "Melons",
    grepl("peach", Description_English, ignore.case = TRUE) ~ "Peaches",
    grepl("pear", Description_English, ignore.case = TRUE) ~ "Pear",
    grepl("pineapple", Description_English, ignore.case = TRUE) ~ "Pineapple",
    grepl("pomegranate", Description_English, ignore.case = TRUE) ~ "Pomegranate",
    grepl("prunes", Description_English, ignore.case = TRUE) ~ "Prunes",
    grepl("raspberries", Description_English, ignore.case = TRUE) ~ "Raspberries",
    grepl("strawberries|strawberry", Description_English, ignore.case = TRUE) ~ "Strawberries",
    grepl("grapes", Description_English, ignore.case = TRUE) ~ "Grapes",
    TRUE ~ Description_English
  ),)

#demographics


#sex
table.fruit_sex <- Fruit_sub %>%
  select(Sex, Fruit_description) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = Sex,
              label = Fruit_description ~ ""
  ) %>%
  modify_header(label ~ "**Fruit**") %>%
  add_overall()%>%
  bold_labels() 


#age
table.fruit_age <- Fruit_sub %>%
  select(age_cat, Fruit_description) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = age_cat,
              label = Fruit_description ~ ""
  ) %>%
  modify_header(label ~ "**Fruit**") %>%
  bold_labels() 

#age/sex
table.fruit_age_sex <- Fruit_sub %>%
  select(age_sex_cat, Fruit_description) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = age_sex_cat,
              label = Fruit_description ~ ""
  ) %>%
  modify_header(label ~ "**Fruit**") %>%
  bold_labels() 

#simd
table.fruit_simd <- Fruit_sub %>%
  select(simd_quintile, Fruit_description) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = simd_quintile,
              label = Fruit_description ~ ""
  ) %>%
  modify_header(label ~ "**Fruit**") %>%
  bold_labels() 

#Reformat for main text
# List of tables to process
tables <- list(
  fruit.sex = table.fruit_sex,
  fruit.age = table.fruit_age,
  fruit.age.sex = table.fruit_age_sex,
  fruit.simd = table.fruit_simd
)


# Process each table - not running full function as don't need to select prefixes here
processed_tables <- lapply(tables, function(table) {
  as.data.frame(t(as.data.frame(table)))
})

# Set the first row as column names and remove that row from each data frame
processed_tables <- lapply(processed_tables, function(df) {
  colnames(df) <- df[1, ]
  df[-1, ]
})

# Clean column names
processed_tables <- lapply(processed_tables, function(df) {
  clean_names(df)
})

# Combine and relocate columns 
fruit_main_text <- bind_rows(processed_tables) %>%
  rownames_to_column(var = "Demographics")

#select columns for export
Fruit_main_text_export <- fruit_main_text %>%
  select(Demographics, apple, banana, blueberries_bilberries, grapes, melons, strawberries)

#save workbook
write_xlsx(Fruit_main_text_export, file = paste0("Output/Table_Fruit_Freq_", format(Sys.time(), "%d%m%Y"), ".xlsx"))





# Energy and Nutrient Intake -Chpt 5 ####

## Tables - Annexe ####

##Energy####
#age
age.energy <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = UI_Energykcal ~ "Total energy (kcal/day)",
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(UI_Energykcal ~ c(0,0), all_categorical() ~ c(0,0)),
  include = UI_Energykcal) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.energy <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = UI_Energykcal ~ "Total energy (kcal/day)",
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(UI_Energykcal ~ c(0,0), all_categorical() ~ c(0,0)),
  include = UI_Energykcal) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.energy <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = UI_Energykcal ~ "Total energy (kcal/day)",
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(UI_Energykcal ~ c(0,0), all_categorical() ~ c(0,0)),
  include = UI_Energykcal) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.energy <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = UI_Energykcal ~ "Total energy (kcal/day)",
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(UI_Energykcal ~ c(0,0), all_categorical() ~ c(0,0)),
  include = UI_Energykcal) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

##Energy as % EAR####
#age
age.energy.ear <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = UI_EAR_prct_Energy ~ "Energy (kcal) as % of EAR",
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(UI_EAR_prct_Energy ~ c(0,0), all_categorical() ~ c(0,0)),
  include = UI_EAR_prct_Energy) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.energy.ear <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = UI_EAR_prct_Energy ~ "Energy (kcal) as % of EAR",
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(UI_EAR_prct_Energy ~ c(0,0), all_categorical() ~ c(0,0)),
  include = UI_EAR_prct_Energy) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.energy.ear <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = UI_EAR_prct_Energy ~ "Energy (kcal) as % of EAR",
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(UI_EAR_prct_Energy ~ c(0,0), all_categorical() ~ c(0,0)),
  include = UI_EAR_prct_Energy) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.energy.ear <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = UI_EAR_prct_Energy ~ "Energy (kcal) as % of EAR",
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(UI_EAR_prct_Energy ~ c(0,0), all_categorical() ~ c(0,0)),
  include = UI_EAR_prct_Energy) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()




##Protein####
#age
age.protein <-tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Proteing ~ "Protein (g/day)",
    UI_RNI_prct_Protein ~ "Protein, mean as % RNI",
    UI_RNI_Protein ~ "Protein, % below RNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Proteing ~ c(0,0),
    UI_RNI_prct_Protein ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Proteing, UI_RNI_prct_Protein, UI_RNI_Protein)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.protein <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Proteing ~ "Protein (g/day)",
    UI_RNI_prct_Protein ~ "Protein, mean as % RNI",
    UI_RNI_Protein ~ "Protein, % below RNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Proteing ~ c(0,0),
    UI_RNI_prct_Protein ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Proteing, UI_RNI_prct_Protein, UI_RNI_Protein)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.protein <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Proteing ~ "Protein (g/day)",
    UI_RNI_prct_Protein ~ "Protein, mean as % RNI",
    UI_RNI_Protein ~ "Protein, % below RNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Proteing ~ c(0,0),
    UI_RNI_prct_Protein ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Proteing, UI_RNI_prct_Protein, UI_RNI_Protein)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.protein <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Proteing ~ "Protein (g/day)",
    UI_RNI_prct_Protein ~ "Protein, mean as % RNI",
    UI_RNI_Protein ~ "Protein, % below RNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Proteing ~ c(0,0),
    UI_RNI_prct_Protein ~ c(0,0),
    
    all_categorical() ~ c(0,0)),
  include = c(UI_Proteing, UI_RNI_prct_Protein, UI_RNI_Protein)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


##Vitamin A####
#age
age.VitA <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_VitaminAug ~ "Vitamin A (µg/day)",
    UI_RNI_prct_VitaminA ~ "Vitamin A, mean as % RNI",
    UI_RNI_VitaminA ~ "Vitamin A, % below RNI",
    UI_LRNI_VitaminA ~ "Vitamin A, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminAug ~ c(0,0),
    UI_RNI_prct_VitaminA ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminAug,UI_RNI_prct_VitaminA, UI_RNI_VitaminA, UI_LRNI_VitaminA)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.VitA <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_VitaminAug ~ "Vitamin A (µg/day)",
    UI_RNI_prct_VitaminA ~ "Vitamin A, mean as % RNI",
    UI_RNI_VitaminA ~ "Vitamin A, % below RNI",
    UI_LRNI_VitaminA ~ "Vitamin A, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminAug ~ c(0,0),
    UI_RNI_prct_VitaminA ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminAug,UI_RNI_prct_VitaminA, UI_RNI_VitaminA, UI_LRNI_VitaminA)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


#age and sex
age.sex.VitA <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_VitaminAug ~ "Vitamin A (µg/day)",
    UI_RNI_prct_VitaminA ~ "Vitamin A, mean as % RNI",
    UI_RNI_VitaminA ~ "Vitamin A, % below RNI",
    UI_LRNI_VitaminA ~ "Vitamin A, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminAug ~ c(0,0),
    UI_RNI_prct_VitaminA ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminAug,UI_RNI_prct_VitaminA, UI_RNI_VitaminA, UI_LRNI_VitaminA)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


#simd
simd.VitA <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_VitaminAug ~ "Vitamin A (µg/day)",
    UI_RNI_prct_VitaminA ~ "Vitamin A, mean as % RNI",
    UI_RNI_VitaminA ~ "Vitamin A, % below RNI",
    UI_LRNI_VitaminA ~ "Vitamin A, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminAug ~ c(0,0),
    UI_RNI_prct_VitaminA ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminAug,UI_RNI_prct_VitaminA, UI_RNI_VitaminA, UI_LRNI_VitaminA)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


##Riboflavin####
#age
age.riboflavin <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Riboflavinmg ~ "Riboflavin (mg/day)",
    UI_RNI_prct_Riboflavin ~ "Riboflavin, mean as % RNI",
    UI_RNI_Riboflavin ~ "Riboflavin, % below RNI",
    UI_LRNI_Riboflavin ~ "Riboflavin, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Riboflavinmg ~ c(1,0),
    UI_RNI_prct_Riboflavin ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Riboflavinmg,UI_RNI_prct_Riboflavin, UI_RNI_Riboflavin, UI_LRNI_Riboflavin)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.riboflavin <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Riboflavinmg ~ "Riboflavin (mg/day)",
    UI_RNI_prct_Riboflavin ~ "Riboflavin, mean as % RNI",
    UI_RNI_Riboflavin ~ "Riboflavin, % below RNI",
    UI_LRNI_Riboflavin ~ "Riboflavin, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Riboflavinmg ~ c(1,0),
    UI_RNI_prct_Riboflavin ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Riboflavinmg,UI_RNI_prct_Riboflavin, UI_RNI_Riboflavin, UI_LRNI_Riboflavin)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.riboflavin <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Riboflavinmg ~ "Riboflavin (mg/day)",
    UI_RNI_prct_Riboflavin ~ "Riboflavin, mean as % RNI",
    UI_RNI_Riboflavin ~ "Riboflavin, % below RNI",
    UI_LRNI_Riboflavin ~ "Riboflavin, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Riboflavinmg ~ c(1,0),
    UI_RNI_prct_Riboflavin ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Riboflavinmg,UI_RNI_prct_Riboflavin, UI_RNI_Riboflavin, UI_LRNI_Riboflavin)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.riboflavin <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Riboflavinmg ~ "Riboflavin (mg/day)",
    UI_RNI_prct_Riboflavin ~ "Riboflavin, mean as % RNI",
    UI_RNI_Riboflavin ~ "Riboflavin, % below RNI",
    UI_LRNI_Riboflavin ~ "Riboflavin, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Riboflavinmg ~ c(1,0),
    UI_RNI_prct_Riboflavin ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Riboflavinmg,UI_RNI_prct_Riboflavin, UI_RNI_Riboflavin, UI_LRNI_Riboflavin)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

##Folate####
#age
age.folate <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Folateug ~ "Folate (µg/day)",
    UI_RNI_prct_Folate ~ "Folate, mean as % RNI",
    UI_RNI_Folate ~ "Folate, % below RNI",
    UI_LRNI_Folate ~ "Folate, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Folateug ~ c(0,0),
    UI_RNI_prct_Folate ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Folateug, UI_RNI_prct_Folate, UI_RNI_Folate, UI_LRNI_Folate)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.folate <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Folateug ~ "Folate (µg/day)",
    UI_RNI_prct_Folate ~ "Folate, mean as % RNI",
    UI_RNI_Folate ~ "Folate, % below RNI",
    UI_LRNI_Folate ~ "Folate, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Folateug ~ c(0,0),
    UI_RNI_prct_Folate ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Folateug, UI_RNI_prct_Folate, UI_RNI_Folate, UI_LRNI_Folate)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.folate <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Folateug ~ "Folate (µg/day)",
    UI_RNI_prct_Folate ~ "Folate, mean as % RNI",
    UI_RNI_Folate ~ "Folate, % below RNI",
    UI_LRNI_Folate ~ "Folate, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Folateug ~ c(0,0),
    UI_RNI_prct_Folate ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Folateug, UI_RNI_prct_Folate, UI_RNI_Folate, UI_LRNI_Folate)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.folate <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Folateug ~ "Folate (µg/day)",
    UI_RNI_prct_Folate ~ "Folate, mean as % RNI",
    UI_RNI_Folate ~ "Folate, % below RNI",
    UI_LRNI_Folate ~ "Folate, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Folateug ~ c(0,0),
    UI_RNI_prct_Folate ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Folateug, UI_RNI_prct_Folate, UI_RNI_Folate, UI_LRNI_Folate)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


##Vitamin D####
#age
age.VitD <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_VitaminDug ~ "Vitamin D (µg/day)",
    UI_RNI_prct_VitaminD ~ "Vitamin D, mean as % RNI",
    UI_RNI_VitaminD ~ "Vitamin D, % below RNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminDug ~ c(0,0),
    UI_RNI_prct_VitaminD ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminDug, UI_RNI_prct_VitaminD,UI_RNI_VitaminD)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.VitD <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_VitaminDug ~ "Vitamin D (µg/day)",
    UI_RNI_prct_VitaminD ~ "Vitamin D, mean as % RNI",
    UI_RNI_VitaminD ~ "Vitamin D, % below RNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminDug ~ c(0,0),
    UI_RNI_prct_VitaminD ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminDug, UI_RNI_prct_VitaminD,UI_RNI_VitaminD)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.VitD <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_VitaminDug ~ "Vitamin D (µg/day)",
    UI_RNI_prct_VitaminD ~ "Vitamin D, mean as % RNI",
    UI_RNI_VitaminD ~ "Vitamin D, % below RNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminDug ~ c(0,0),
    UI_RNI_prct_VitaminD ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminDug, UI_RNI_prct_VitaminD,UI_RNI_VitaminD)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.VitD <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_VitaminDug ~ "Vitamin D (µg/day)",
    UI_RNI_prct_VitaminD ~ "Vitamin D, mean as % RNI",
    UI_RNI_VitaminD ~ "Vitamin D, % below RNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminDug ~ c(0,0),
    UI_RNI_prct_VitaminD ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminDug, UI_RNI_prct_VitaminD,UI_RNI_VitaminD)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


##Vitamin B12####
#age
age.VitB12 <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_VitaminB12ug ~ "Vitamin B12 (µg/day)",
    UI_RNI_prct_VitaminB12 ~ "Vitamin B12, mean as % RNI",
    UI_RNI_VitaminB12 ~ "Vitamin B12, % below RNI",
    UI_LRNI_VitaminB12 ~ "Vitamin B12, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminB12ug ~ c(1,0),
    UI_RNI_prct_VitaminB12 ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminB12ug, UI_RNI_prct_VitaminB12, UI_RNI_VitaminB12, UI_LRNI_VitaminB12)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.VitB12 <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_VitaminB12ug ~ "Vitamin B12 (µg/day)",
    UI_RNI_prct_VitaminB12 ~ "Vitamin B12, mean as % RNI",
    UI_RNI_VitaminB12 ~ "Vitamin B12, % below RNI",
    UI_LRNI_VitaminB12 ~ "Vitamin B12, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminB12ug ~ c(1,0),
    UI_RNI_prct_VitaminB12 ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminB12ug, UI_RNI_prct_VitaminB12, UI_RNI_VitaminB12, UI_LRNI_VitaminB12)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.VitB12 <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_VitaminB12ug ~ "Vitamin B12 (µg/day)",
    UI_RNI_prct_VitaminB12 ~ "Vitamin B12, mean as % RNI",
    UI_RNI_VitaminB12 ~ "Vitamin B12, % below RNI",
    UI_LRNI_VitaminB12 ~ "Vitamin B12, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminB12ug ~ c(1,0),
    UI_RNI_prct_VitaminB12 ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminB12ug, UI_RNI_prct_VitaminB12, UI_RNI_VitaminB12, UI_LRNI_VitaminB12)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.VitB12 <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_VitaminB12ug ~ "Vitamin B12 (µg/day)",
    UI_RNI_prct_VitaminB12 ~ "Vitamin B12, mean as % RNI",
    UI_RNI_VitaminB12 ~ "Vitamin B12, % below RNI",
    UI_LRNI_VitaminB12 ~ "Vitamin B12, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminB12ug ~ c(1,0),
    UI_RNI_prct_VitaminB12 ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminB12ug, UI_RNI_prct_VitaminB12, UI_RNI_VitaminB12, UI_LRNI_VitaminB12)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


##Vitamin C####
#age
age.VitC <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_VitaminCmg ~ "Vitamin C (mg/day)",
    UI_RNI_prct_VitaminC ~ "Vitamin C, mean as % RNI",
    UI_RNI_VitaminC ~ "Vitamin C, % below RNI",
    UI_LRNI_VitaminC ~ "Vitamin C, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminCmg ~ c(0,0),
    UI_RNI_prct_VitaminC ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminCmg, UI_RNI_prct_VitaminC, UI_RNI_VitaminC, UI_LRNI_VitaminC)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.VitC <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_VitaminCmg ~ "Vitamin C (mg/day)",
    UI_RNI_prct_VitaminC ~ "Vitamin C, mean as % RNI",
    UI_RNI_VitaminC ~ "Vitamin C, % below RNI",
    UI_LRNI_VitaminC ~ "Vitamin C, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminCmg ~ c(0,0),
    UI_RNI_prct_VitaminC ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminCmg, UI_RNI_prct_VitaminC, UI_RNI_VitaminC, UI_LRNI_VitaminC)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.VitC <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_VitaminCmg ~ "Vitamin C (mg/day)",
    UI_RNI_prct_VitaminC ~ "Vitamin C, mean as % RNI",
    UI_RNI_VitaminC ~ "Vitamin C, % below RNI",
    UI_LRNI_VitaminC ~ "Vitamin C, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminCmg ~ c(0,0),
    UI_RNI_prct_VitaminC ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminCmg, UI_RNI_prct_VitaminC, UI_RNI_VitaminC, UI_LRNI_VitaminC)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.VitC <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_VitaminCmg ~ "Vitamin C (mg/day)",
    UI_RNI_prct_VitaminC ~ "Vitamin C, mean as % RNI",
    UI_RNI_VitaminC ~ "Vitamin C, % below RNI",
    UI_LRNI_VitaminC ~ "Vitamin C, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_VitaminCmg ~ c(0,0),
    UI_RNI_prct_VitaminC ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_VitaminCmg, UI_RNI_prct_VitaminC, UI_RNI_VitaminC, UI_LRNI_VitaminC)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


##Iron####
#age
age.iron <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Ironmg ~ "Iron (mg/day)",
    UI_RNI_prct_Iron ~ "Iron, mean as % RNI",
    UI_RNI_Iron ~ "Iron, % below RNI",
    UI_LRNI_Iron ~ "Iron, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Ironmg ~ c(1,0),
    UI_RNI_prct_Iron ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Ironmg, UI_RNI_prct_Iron, UI_RNI_Iron, UI_LRNI_Iron)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.iron <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Ironmg ~ "Iron (mg/day)",
    UI_RNI_prct_Iron ~ "Iron, mean as % RNI",
    UI_RNI_Iron ~ "Iron, % below RNI",
    UI_LRNI_Iron ~ "Iron, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Ironmg ~ c(1,0),
    UI_RNI_prct_Iron ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Ironmg, UI_RNI_prct_Iron, UI_RNI_Iron, UI_LRNI_Iron)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.iron <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Ironmg ~ "Iron (mg/day)",
    UI_RNI_prct_Iron ~ "Iron, mean as % RNI",
    UI_RNI_Iron ~ "Iron, % below RNI",
    UI_LRNI_Iron ~ "Iron, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Ironmg ~ c(1,0),
    UI_RNI_prct_Iron ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Ironmg, UI_RNI_prct_Iron, UI_RNI_Iron, UI_LRNI_Iron)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.iron <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Ironmg ~ "Iron (mg/day)",
    UI_RNI_prct_Iron ~ "Iron, mean as % RNI",
    UI_RNI_Iron ~ "Iron, % below RNI",
    UI_LRNI_Iron ~ "Iron, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Ironmg ~ c(1,0),
    UI_RNI_prct_Iron ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Ironmg, UI_RNI_prct_Iron, UI_RNI_Iron, UI_LRNI_Iron)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


##Calcium####
#age
age.calcium <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Calciummg ~ "Calcium (mg/day)",
    UI_RNI_prct_Calcium ~ "Calcium, mean as % RNI",
    UI_RNI_Calcium ~ "Calcium, % below RNI",
    UI_LRNI_Calcium ~ "Calcium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Calciummg ~ c(0,0),
    UI_RNI_prct_Calcium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c( UI_Calciummg, UI_RNI_prct_Calcium, UI_RNI_Calcium, UI_LRNI_Calcium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.calcium <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Calciummg ~ "Calcium (mg/day)",
    UI_RNI_prct_Calcium ~ "Calcium, mean as % RNI",
    UI_RNI_Calcium ~ "Calcium, % below RNI",
    UI_LRNI_Calcium ~ "Calcium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Calciummg ~ c(0,0),
    UI_RNI_prct_Calcium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c( UI_Calciummg, UI_RNI_prct_Calcium, UI_RNI_Calcium, UI_LRNI_Calcium)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.calcium <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Calciummg ~ "Calcium (mg/day)",
    UI_RNI_prct_Calcium ~ "Calcium, mean as % RNI",
    UI_RNI_Calcium ~ "Calcium, % below RNI",
    UI_LRNI_Calcium ~ "Calcium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Calciummg ~ c(0,0),
    UI_RNI_prct_Calcium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c( UI_Calciummg, UI_RNI_prct_Calcium, UI_RNI_Calcium, UI_LRNI_Calcium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.calcium <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Calciummg ~ "Calcium (mg/day)",
    UI_RNI_prct_Calcium ~ "Calcium, mean as % RNI",
    UI_RNI_Calcium ~ "Calcium, % below RNI",
    UI_LRNI_Calcium ~ "Calcium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Calciummg ~ c(0,0),
    UI_RNI_prct_Calcium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c( UI_Calciummg, UI_RNI_prct_Calcium, UI_RNI_Calcium, UI_LRNI_Calcium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()



##Magnesium####
#age
age.magnesium <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Magnesiummg ~ "Magnesium (mg/day)",
    UI_RNI_prct_Magnesium ~ "Magnesium, mean as % RNI",
    UI_RNI_Magnesium ~ "Magnesium, % below RNI",
    UI_LRNI_Magnesium ~ "Magnesium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Magnesiummg ~ c(0,0),
    UI_RNI_prct_Magnesium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Magnesiummg, UI_RNI_prct_Magnesium, UI_RNI_Magnesium, UI_LRNI_Magnesium)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.magnesium <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Magnesiummg ~ "Magnesium (mg/day)",
    UI_RNI_prct_Magnesium ~ "Magnesium, mean as % RNI",
    UI_RNI_Magnesium ~ "Magnesium, % below RNI",
    UI_LRNI_Magnesium ~ "Magnesium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Magnesiummg ~ c(0,0),
    UI_RNI_prct_Magnesium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Magnesiummg, UI_RNI_prct_Magnesium, UI_RNI_Magnesium, UI_LRNI_Magnesium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.magnesium <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Magnesiummg ~ "Magnesium (mg/day)",
    UI_RNI_prct_Magnesium ~ "Magnesium, mean as % RNI",
    UI_RNI_Magnesium ~ "Magnesium, % below RNI",
    UI_LRNI_Magnesium ~ "Magnesium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Magnesiummg ~ c(0,0),
    UI_RNI_prct_Magnesium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Magnesiummg, UI_RNI_prct_Magnesium, UI_RNI_Magnesium, UI_LRNI_Magnesium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.magnesium <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Magnesiummg ~ "Magnesium (mg/day)",
    UI_RNI_prct_Magnesium ~ "Magnesium, mean as % RNI",
    UI_RNI_Magnesium ~ "Magnesium, % below RNI",
    UI_LRNI_Magnesium ~ "Magnesium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Magnesiummg ~ c(0,0),
    UI_RNI_prct_Magnesium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Magnesiummg, UI_RNI_prct_Magnesium, UI_RNI_Magnesium, UI_LRNI_Magnesium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


##Potassium####
#age
age.potassium <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Potassiummg ~ "Potassium (mg/day)",
    UI_RNI_prct_Potassium ~ "Potassium, mean as % RNI",
    UI_RNI_Potassium ~ "Potassium, % below RNI",
    UI_LRNI_Potassium ~ "Potassium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Potassiummg ~ c(0,0),
    UI_RNI_prct_Potassium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Potassiummg,UI_RNI_prct_Potassium, UI_RNI_Potassium, UI_LRNI_Potassium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.potassium <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Potassiummg ~ "Potassium (mg/day)",
    UI_RNI_prct_Potassium ~ "Potassium, mean as % RNI",
    UI_RNI_Potassium ~ "Potassium, % below RNI",
    UI_LRNI_Potassium ~ "Potassium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Potassiummg ~ c(0,0),
    UI_RNI_prct_Potassium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Potassiummg,UI_RNI_prct_Potassium, UI_RNI_Potassium, UI_LRNI_Potassium)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.potassium <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Potassiummg ~ "Potassium (mg/day)",
    UI_RNI_prct_Potassium ~ "Potassium, mean as % RNI",
    UI_RNI_Potassium ~ "Potassium, % below RNI",
    UI_LRNI_Potassium ~ "Potassium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Potassiummg ~ c(0,0),
    UI_RNI_prct_Potassium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Potassiummg,UI_RNI_prct_Potassium, UI_RNI_Potassium, UI_LRNI_Potassium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.potassium <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Potassiummg ~ "Potassium (mg/day)",
    UI_RNI_prct_Potassium ~ "Potassium, mean as % RNI",
    UI_RNI_Potassium ~ "Potassium, % below RNI",
    UI_LRNI_Potassium ~ "Potassium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Potassiummg ~ c(0,0),
    UI_RNI_prct_Potassium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Potassiummg,UI_RNI_prct_Potassium, UI_RNI_Potassium, UI_LRNI_Potassium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


##Iodine####
#age
age.iodine <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Iodineug ~ "Iodine (µg/day)",
    UI_RNI_prct_Iodine ~ "Iodine, mean as % RNI",
    UI_RNI_Iodine ~ "Iodine, % below RNI",
    UI_LRNI_Iodine ~ "Iodine, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Iodineug ~ c(0,0),
    UI_RNI_prct_Iodine ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Iodineug, UI_RNI_prct_Iodine, UI_RNI_Iodine, UI_LRNI_Iodine)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.iodine <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Iodineug ~ "Iodine (µg/day)",
    UI_RNI_prct_Iodine ~ "Iodine, mean as % RNI",
    UI_RNI_Iodine ~ "Iodine, % below RNI",
    UI_LRNI_Iodine ~ "Iodine, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Iodineug ~ c(0,0),
    UI_RNI_prct_Iodine ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Iodineug, UI_RNI_prct_Iodine, UI_RNI_Iodine, UI_LRNI_Iodine)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.iodine <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Iodineug ~ "Iodine (µg/day)",
    UI_RNI_prct_Iodine ~ "Iodine, mean as % RNI",
    UI_RNI_Iodine ~ "Iodine, % below RNI",
    UI_LRNI_Iodine ~ "Iodine, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Iodineug ~ c(0,0),
    UI_RNI_prct_Iodine ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Iodineug, UI_RNI_prct_Iodine, UI_RNI_Iodine, UI_LRNI_Iodine)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.iodine <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Iodineug ~ "Iodine (µg/day)",
    UI_RNI_prct_Iodine ~ "Iodine, mean as % RNI",
    UI_RNI_Iodine ~ "Iodine, % below RNI",
    UI_LRNI_Iodine ~ "Iodine, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Iodineug ~ c(0,0),
    UI_RNI_prct_Iodine ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Iodineug, UI_RNI_prct_Iodine, UI_RNI_Iodine, UI_LRNI_Iodine)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

##Selenium####
#age
age.selenium <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Seleniumug ~ "Selenium (µg/day)",
    UI_RNI_prct_Selenium ~ "Selenium, mean as % RNI",
    UI_RNI_Selenium ~ "Selenium, % below RNI",
    UI_LRNI_Selenium ~ "Selenium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Seleniumug ~ c(0,0),
    UI_RNI_prct_Selenium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Seleniumug, UI_RNI_prct_Selenium, UI_RNI_Selenium, 
              UI_LRNI_Selenium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.selenium <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Seleniumug ~ "Selenium (µg/day)",
    UI_RNI_prct_Selenium ~ "Selenium, mean as % RNI",
    UI_RNI_Selenium ~ "Selenium, % below RNI",
    UI_LRNI_Selenium ~ "Selenium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Seleniumug ~ c(0,0),
    UI_RNI_prct_Selenium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Seleniumug, UI_RNI_prct_Selenium, UI_RNI_Selenium, 
              UI_LRNI_Selenium)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


#age and sex
age.sex.selenium <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Seleniumug ~ "Selenium (µg/day)",
    UI_RNI_prct_Selenium ~ "Selenium, mean as % RNI",
    UI_RNI_Selenium ~ "Selenium, % below RNI",
    UI_LRNI_Selenium ~ "Selenium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Seleniumug ~ c(0,0),
    UI_RNI_prct_Selenium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Seleniumug, UI_RNI_prct_Selenium, UI_RNI_Selenium, 
              UI_LRNI_Selenium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#simd
simd.selenium <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Seleniumug ~ "Selenium (µg/day)",
    UI_RNI_prct_Selenium ~ "Selenium, mean as % RNI",
    UI_RNI_Selenium ~ "Selenium, % below RNI",
    UI_LRNI_Selenium ~ "Selenium, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Seleniumug ~ c(0,0),
    UI_RNI_prct_Selenium ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Seleniumug, UI_RNI_prct_Selenium, UI_RNI_Selenium, 
              UI_LRNI_Selenium)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()



##Zinc####
#age
age.zinc <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(
    UI_Zincmg ~ "Zinc (mg/day)",
    UI_RNI_prct_Zinc ~ "Zinc, mean as % RNI",
    UI_RNI_Zinc ~ "Zinc, % below RNI",
    UI_LRNI_Zinc ~ "Zinc, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Zincmg ~ c(1,0),
    UI_RNI_prct_Zinc ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Zincmg, UI_RNI_prct_Zinc, UI_RNI_Zinc, UI_LRNI_Zinc)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#sex
sex.zinc <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_Zincmg ~ "Zinc (mg/day)",
    UI_RNI_prct_Zinc ~ "Zinc, mean as % RNI",
    UI_RNI_Zinc ~ "Zinc, % below RNI",
    UI_LRNI_Zinc ~ "Zinc, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Zincmg ~ c(1,0),
    UI_RNI_prct_Zinc ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Zincmg, UI_RNI_prct_Zinc, UI_RNI_Zinc, UI_LRNI_Zinc)) %>%
  bold_labels() %>% 
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()

#age and sex
age.sex.zinc <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    UI_Zincmg ~ "Zinc (mg/day)",
    UI_RNI_prct_Zinc ~ "Zinc, mean as % RNI",
    UI_RNI_Zinc ~ "Zinc, % below RNI",
    UI_LRNI_Zinc ~ "Zinc, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Zincmg ~ c(1,0),
    UI_RNI_prct_Zinc ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Zincmg, UI_RNI_prct_Zinc, UI_RNI_Zinc, UI_LRNI_Zinc)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()


#simd
simd.zinc <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    UI_Zincmg ~ "Zinc (mg/day)",
    UI_RNI_prct_Zinc ~ "Zinc, mean as % RNI",
    UI_RNI_Zinc ~ "Zinc, % below RNI",
    UI_LRNI_Zinc ~ "Zinc, % below LRNI"),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median} 
                                       {sd}
                                       {p25}
                                       {p75}", 
                   all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(
    UI_Zincmg ~ c(1,0),
    UI_RNI_prct_Zinc ~ c(0,0),
    all_categorical() ~ c(0,0)),
  include = c(UI_Zincmg, UI_RNI_prct_Zinc, UI_RNI_Zinc, UI_LRNI_Zinc)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()%>%
  as_flex_table()



###output Excel ####

#create workbook
wb3 <- wb_workbook()


nutrient_sheet_names <- c("energy", "energy as % EAR", "protein", "Vitamin A", "riboflavin", "folate", "Vitamin D", "Vitamin B12", 
                          "Vitamin C", "iron", "calcium", "magnesium", "potassium", "iodine", "selenium", "zinc")

# list of flextables
nutrient_flextables <- list(
  energy = list(age = age.energy, sex = sex.energy, age_sex = age.sex.energy, simd = simd.energy),
  `energy as % EAR` = list(age = age.energy.ear, sex = sex.energy.ear, age_sex = age.sex.energy.ear, simd = simd.energy.ear),
  protein = list(age = age.protein, sex = sex.protein, age_sex = age.sex.protein, simd = simd.protein),
  `Vitamin A` = list(age = age.VitA, sex = sex.VitA, age_sex = age.sex.VitA, simd = simd.VitA),
  riboflavin = list(age = age.riboflavin, sex = sex.riboflavin, age_sex = age.sex.riboflavin, simd = simd.riboflavin),
  folate = list(age = age.folate, sex = sex.folate, age_sex = age.sex.folate, simd = simd.folate),
  `Vitamin D` = list(age = age.VitD, sex = sex.VitD, age_sex = age.sex.VitD, simd = simd.VitD),
  `Vitamin B12` = list(age = age.VitB12, sex = sex.VitB12, age_sex = age.sex.VitB12, simd = simd.VitB12),
  `Vitamin C` = list(age = age.VitC, sex = sex.VitC, age_sex = age.sex.VitC, simd = simd.VitC),
  iron = list(age = age.iron, sex = sex.iron, age_sex = age.sex.iron, simd = simd.iron),
  calcium = list(age = age.calcium, sex = sex.calcium, age_sex = age.sex.calcium, simd = simd.calcium),
  magnesium = list(age = age.magnesium, sex = sex.magnesium, age_sex = age.sex.magnesium, simd = simd.magnesium),
  potassium = list(age = age.potassium, sex = sex.potassium, age_sex = age.sex.potassium, simd = simd.potassium),
  iodine = list(age = age.iodine, sex = sex.iodine, age_sex = age.sex.iodine, simd = simd.iodine),
  selenium = list(age = age.selenium, sex = sex.selenium, age_sex = age.sex.selenium, simd = simd.selenium),
  zinc = list(age = age.zinc, sex = sex.zinc, age_sex = age.sex.zinc, simd = simd.zinc)
)

# list with location for excel sheet
dims_list <- c(age = "A1", sex = "G1", age_sex = "M1", simd = "V1")

# Add worksheets and flextables
for (nutrient in nutrient_sheet_names) {
  wb3 <- wb_add_worksheet(wb3, sheet = nutrient)
  
  if (nutrient %in% names(nutrient_flextables)) {
    tables <- nutrient_flextables[[nutrient]]
    
    for (table_type in names(tables)) {
      wb3 <- wb_add_flextable(wb3, sheet = nutrient, tables[[table_type]], dims = dims_list[[table_type]])
    }
  }
}


#save workbook
wb_save(wb3, file = paste0("Output/Table_Nutrients_Annexure_", format(Sys.time(), "%d%m%Y"), ".xlsx"))



#Word
save_as_docx("Energy by Age"= age.energy, "Energy by Sex"= sex.energy,"Energy by Age and Sex"= age.sex.energy, "Energy by SIMD"= simd.energy,
             "protein by Age"= age.protein, "protein by Sex"= sex.protein,"protein by Age and Sex"= age.sex.protein, "protein by SIMD"= simd.protein,
             "vitA by Age"= age.VitA, "vitA by Sex"= sex.VitA,"vitA by Age and Sex"= age.sex.VitA, "vitA by SIMD"= simd.VitA,
             "riboflavin by Age"= age.riboflavin, "riboflavin by Sex"= sex.riboflavin,"riboflavin by Age and Sex"= age.sex.riboflavin, "riboflavin by SIMD"= simd.riboflavin,
             "folate by Age"= age.folate, "folate by Sex"= sex.folate,"folate by Age and Sex"= age.sex.folate, "folate by SIMD"= simd.folate,
             "VitD by Age"= age.VitD, "VitD by Sex"= sex.VitD,"VitD by Age and Sex"= age.sex.VitD, "VitD by SIMD"= simd.VitD,
             "VitB12 by Age"= age.VitB12, "VitB12 by Sex"= sex.VitB12,"VitB12 by Age and Sex"= age.sex.VitB12, "VitB12 by SIMD"= simd.VitB12,
             "VitC by Age"= age.VitC, "VitC by Sex"= sex.VitC,"VitC by Age and Sex"= age.sex.VitC, "VitC by SIMD"= simd.VitC,
             "iron by Age"= age.iron, "iron by Sex"= sex.iron,"iron by Age and Sex"= age.sex.iron, "iron by SIMD"= simd.iron,
             "calcium by Age"= age.calcium, "calcium by Sex"= sex.calcium,"calcium by Age and Sex"= age.sex.calcium, "calcium by SIMD"= simd.calcium,
             "magnesium by Age"= age.magnesium, "magnesium by Sex"= sex.magnesium,"magnesium by Age and Sex"= age.sex.magnesium, "magnesium by SIMD"= simd.magnesium,
             "potassium by Age"= age.potassium, "potassium by Sex"= sex.potassium,"potassium by Age and Sex"= age.sex.potassium, "potassium by SIMD"= simd.potassium,
             "iodine by Age"= age.iodine, "iodine by Sex"= sex.iodine,"iodine by Age and Sex"= age.sex.iodine, "iodine by SIMD"= simd.iodine,
             "selenium by Age"= age.selenium, "selenium by Sex"= sex.selenium,"selenium by Age and Sex"= age.sex.selenium, "selenium by SIMD"= simd.selenium,
             "zinc by Age"= age.zinc, "zinc by Sex"= sex.zinc,"zinc by Age and Sex"= age.sex.zinc, "zinc by SIMD"= simd.zinc,
             path=paste0("Output/Table_Nutrients_Annexure_Separate_", format(Sys.time(), "%d%m%Y"),".docx"))


## Tables - Main Text ####

table.rnis.sex <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(UI_Energykcal ~ "Total energy (kcal/day)",
            
            UI_Proteing ~ "Protein (g/day)",
            UI_RNI_prct_Protein ~ "Protein, mean as % RNI",
            
            UI_VitaminAug ~ "Vitamin A (µg/day)",
            UI_RNI_prct_VitaminA ~ "Vitamin A, mean as % RNI",
            
            UI_Riboflavinmg ~ "Riboflavin (mg/day)",
            UI_RNI_prct_Riboflavin ~ "Riboflavin, mean as % RNI",
            
            UI_Folateug ~ "Folate (µg/day)",
            UI_RNI_prct_Folate ~ "Folate, mean as % RNI",
            
            UI_VitaminDug ~ "Vitamin D (µg/day)",
            UI_RNI_prct_VitaminD ~ "Vitamin D, mean as % RNI",
            
            UI_VitaminB12ug ~ "Vitamin B12 (µg/day)",
            UI_RNI_prct_VitaminB12 ~ "Vitamin B12, mean as % RNI",
            
            UI_VitaminCmg ~ "Vitamin C (mg/day)",
            UI_RNI_prct_VitaminC ~ "Vitamin C, mean as % RNI",
            
            UI_Ironmg ~ "Iron (mg/day)",
            UI_RNI_prct_Iron ~ "Iron, mean as % RNI",
            
            UI_Calciummg ~ "Calcium (mg/day)",
            UI_RNI_prct_Calcium ~ "Calcium, mean as % RNI",
            
            UI_Magnesiummg ~ "Magnesium (mg/day)",
            UI_RNI_prct_Magnesium ~ "Magnesium, mean as % RNI",
            
            UI_Potassiummg ~ "Potassium (mg/day)",
            UI_RNI_prct_Potassium ~ "Potassium, mean as % RNI",
            
            UI_Iodineug ~ "Iodine (µg/day)",
            UI_RNI_prct_Iodine ~ "Iodine, mean as % RNI",
            
            UI_Seleniumug ~ "Selenium (µg/day)",
            UI_RNI_prct_Selenium ~ "Selenium, mean as % RNI",
            
            UI_Zincmg ~ "Zinc (mg/day)",
            UI_RNI_prct_Zinc ~ "Zinc, mean as % RNI"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(UI_Energykcal ~ c(0,0),
                UI_Proteing ~ c(0,0),
                UI_RNI_prct_Protein ~ c(0,0),
                UI_VitaminAug ~ c(0,0),
                UI_RNI_prct_VitaminA ~ c(0,0),
                UI_Riboflavinmg ~ c(1,0),
                UI_RNI_prct_Riboflavin ~ c(0,0),
                UI_Folateug ~ c(0,0),
                UI_RNI_prct_Folate ~ c(0,0),
                UI_VitaminDug ~ c(0,0),
                UI_RNI_prct_VitaminD ~ c(0,0),
                UI_VitaminB12ug ~ c(1,0),
                UI_RNI_prct_VitaminB12 ~ c(0,0),
                UI_VitaminCmg ~ c(0,0),
                UI_RNI_prct_VitaminC ~ c(0,0),
                UI_Ironmg ~ c(1,0),
                UI_RNI_prct_Iron ~ c(0,0),
                UI_Calciummg ~ c(0,0),
                UI_RNI_prct_Calcium ~ c(0,0),
                UI_Magnesiummg ~ c(0,0),
                UI_RNI_prct_Magnesium ~ c(0,0),
                UI_Potassiummg ~ c(0,0),
                UI_RNI_prct_Potassium ~ c(0,0),
                UI_Iodineug ~ c(0,0),
                UI_RNI_prct_Iodine ~ c(0,0),
                UI_Seleniumug ~ c(0,0),
                UI_RNI_prct_Selenium ~ c(0,0),
                UI_Zincmg ~ c(1,0),
                UI_RNI_prct_Zinc ~ c(0,0)),
  include = c(UI_Energykcal, UI_Proteing, UI_RNI_prct_Protein, 
              UI_VitaminAug, UI_RNI_prct_VitaminA, 
              UI_Riboflavinmg, UI_RNI_prct_Riboflavin, 
              UI_Folateug, UI_RNI_prct_Folate, 
              UI_VitaminDug, UI_RNI_prct_VitaminD,
              UI_VitaminB12ug, UI_RNI_prct_VitaminB12, 
              UI_VitaminCmg, UI_RNI_prct_VitaminC, 
              UI_Ironmg, UI_RNI_prct_Iron,
              UI_Calciummg, UI_RNI_prct_Calcium, 
              UI_Magnesiummg, UI_RNI_prct_Magnesium,
              UI_Potassiummg, UI_RNI_prct_Potassium, 
              UI_Iodineug, UI_RNI_prct_Iodine, 
              UI_Seleniumug, UI_RNI_prct_Selenium, 
              UI_Zincmg, UI_RNI_prct_Zinc, )) %>%
  bold_labels() %>%
  add_p()%>%
  add_significance_stars()%>%
  add_overall()

table.rnis.age <- tbl_svysummary(
  svy.df.intake24_participant,
  by = age_cat,
  label = c(UI_Energykcal ~ "Total energy (kcal/day)",
            
            UI_Proteing ~ "Protein (g/day)",
            UI_RNI_prct_Protein ~ "Protein, mean as % RNI",
            
            UI_VitaminAug ~ "Vitamin A (µg/day)",
            UI_RNI_prct_VitaminA ~ "Vitamin A, mean as % RNI",
            
            UI_Riboflavinmg ~ "Riboflavin (mg/day)",
            UI_RNI_prct_Riboflavin ~ "Riboflavin, mean as % RNI",
            
            UI_Folateug ~ "Folate (µg/day)",
            UI_RNI_prct_Folate ~ "Folate, mean as % RNI",
            
            UI_VitaminDug ~ "Vitamin D (µg/day)",
            UI_RNI_prct_VitaminD ~ "Vitamin D, mean as % RNI",
            
            UI_VitaminB12ug ~ "Vitamin B12 (µg/day)",
            UI_RNI_prct_VitaminB12 ~ "Vitamin B12, mean as % RNI",
            
            UI_VitaminCmg ~ "Vitamin C (mg/day)",
            UI_RNI_prct_VitaminC ~ "Vitamin C, mean as % RNI",
            
            UI_Ironmg ~ "Iron (mg/day)",
            UI_RNI_prct_Iron ~ "Iron, mean as % RNI",
            
            UI_Calciummg ~ "Calcium (mg/day)",
            UI_RNI_prct_Calcium ~ "Calcium, mean as % RNI",
            
            UI_Magnesiummg ~ "Magnesium (mg/day)",
            UI_RNI_prct_Magnesium ~ "Magnesium, mean as % RNI",
            
            UI_Potassiummg ~ "Potassium (mg/day)",
            UI_RNI_prct_Potassium ~ "Potassium, mean as % RNI",
            
            UI_Iodineug ~ "Iodine (µg/day)",
            UI_RNI_prct_Iodine ~ "Iodine, mean as % RNI",
            
            UI_Seleniumug ~ "Selenium (µg/day)",
            UI_RNI_prct_Selenium ~ "Selenium, mean as % RNI",
            
            UI_Zincmg ~ "Zinc (mg/day)",
            UI_RNI_prct_Zinc ~ "Zinc, mean as % RNI"),
  digits = list(UI_Energykcal ~ c(0,0),
                UI_Proteing ~ c(0,0),
                UI_RNI_prct_Protein ~ c(0,0),
                UI_VitaminAug ~ c(0,0),
                UI_RNI_prct_VitaminA ~ c(0,0),
                UI_Riboflavinmg ~ c(1,0),
                UI_RNI_prct_Riboflavin ~ c(0,0),
                UI_Folateug ~ c(0,0),
                UI_RNI_prct_Folate ~ c(0,0),
                UI_VitaminDug ~ c(0,0),
                UI_RNI_prct_VitaminD ~ c(0,0),
                UI_VitaminB12ug ~ c(1,0),
                UI_RNI_prct_VitaminB12 ~ c(0,0),
                UI_VitaminCmg ~ c(0,0),
                UI_RNI_prct_VitaminC ~ c(0,0),
                UI_Ironmg ~ c(1,0),
                UI_RNI_prct_Iron ~ c(0,0),
                UI_Calciummg ~ c(0,0),
                UI_RNI_prct_Calcium ~ c(0,0),
                UI_Magnesiummg ~ c(0,0),
                UI_RNI_prct_Magnesium ~ c(0,0),
                UI_Potassiummg ~ c(0,0),
                UI_RNI_prct_Potassium ~ c(0,0),
                UI_Iodineug ~ c(0,0),
                UI_RNI_prct_Iodine ~ c(0,0),
                UI_Seleniumug ~ c(0,0),
                UI_RNI_prct_Selenium ~ c(0,0),
                UI_Zincmg ~ c(1,0),
                UI_RNI_prct_Zinc ~ c(0,0)),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  include = c(UI_Energykcal, UI_Proteing, UI_RNI_prct_Protein, UI_VitaminAug, UI_RNI_prct_VitaminA, UI_Riboflavinmg, UI_RNI_prct_Riboflavin, UI_Folateug, 
              UI_RNI_prct_Folate, 
              UI_VitaminDug, UI_RNI_prct_VitaminD, UI_VitaminB12ug, UI_RNI_prct_VitaminB12, UI_VitaminCmg, UI_RNI_prct_VitaminC, UI_Ironmg, UI_RNI_prct_Iron, 
              UI_Calciummg, UI_RNI_prct_Calcium, UI_Magnesiummg, UI_RNI_prct_Magnesium, UI_Potassiummg, UI_RNI_prct_Potassium, UI_Iodineug, UI_RNI_prct_Iodine, UI_Seleniumug, UI_RNI_prct_Selenium, 
              UI_Zincmg, UI_RNI_prct_Zinc)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()

table.rnis.age.sex <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(UI_Energykcal ~ "Total energy (kcal/day)",
            
            UI_Proteing ~ "Protein (g/day)",
            UI_RNI_prct_Protein ~ "Protein, mean as % RNI",
            
            UI_VitaminAug ~ "Vitamin A (µg/day)",
            UI_RNI_prct_VitaminA ~ "Vitamin A, mean as % RNI",
            
            UI_Riboflavinmg ~ "Riboflavin (mg/day)",
            UI_RNI_prct_Riboflavin ~ "Riboflavin, mean as % RNI",
            
            UI_Folateug ~ "Folate (µg/day)",
            UI_RNI_prct_Folate ~ "Folate, mean as % RNI",
            
            UI_VitaminDug ~ "Vitamin D (µg/day)",
            UI_RNI_prct_VitaminD ~ "Vitamin D, mean as % RNI",
            
            UI_VitaminB12ug ~ "Vitamin B12 (µg/day)",
            UI_RNI_prct_VitaminB12 ~ "Vitamin B12, mean as % RNI",
            
            UI_VitaminCmg ~ "Vitamin C (mg/day)",
            UI_RNI_prct_VitaminC ~ "Vitamin C, mean as % RNI",
            
            UI_Ironmg ~ "Iron (mg/day)",
            UI_RNI_prct_Iron ~ "Iron, mean as % RNI",
            
            UI_Calciummg ~ "Calcium (mg/day)",
            UI_RNI_prct_Calcium ~ "Calcium, mean as % RNI",
            
            UI_Magnesiummg ~ "Magnesium (mg/day)",
            UI_RNI_prct_Magnesium ~ "Magnesium, mean as % RNI",
            
            UI_Potassiummg ~ "Potassium (mg/day)",
            UI_RNI_prct_Potassium ~ "Potassium, mean as % RNI",
            
            UI_Iodineug ~ "Iodine (µg/day)",
            UI_RNI_prct_Iodine ~ "Iodine, mean as % RNI",
            
            UI_Seleniumug ~ "Selenium (µg/day)",
            UI_RNI_prct_Selenium ~ "Selenium, mean as % RNI",
            
            UI_Zincmg ~ "Zinc (mg/day)",
            UI_RNI_prct_Zinc ~ "Zinc, mean as % RNI"),
  digits = list(UI_Energykcal ~ c(0,0),
                UI_Proteing ~ c(0,0),
                UI_RNI_prct_Protein ~ c(0,0),
                UI_VitaminAug ~ c(0,0),
                UI_RNI_prct_VitaminA ~ c(0,0),
                UI_Riboflavinmg ~ c(1,0),
                UI_RNI_prct_Riboflavin ~ c(0,0),
                UI_Folateug ~ c(0,0),
                UI_RNI_prct_Folate ~ c(0,0),
                UI_VitaminDug ~ c(0,0),
                UI_RNI_prct_VitaminD ~ c(0,0),
                UI_VitaminB12ug ~ c(1,0),
                UI_RNI_prct_VitaminB12 ~ c(0,0),
                UI_VitaminCmg ~ c(0,0),
                UI_RNI_prct_VitaminC ~ c(0,0),
                UI_Ironmg ~ c(1,0),
                UI_RNI_prct_Iron ~ c(0,0),
                UI_Calciummg ~ c(0,0),
                UI_RNI_prct_Calcium ~ c(0,0),
                UI_Magnesiummg ~ c(0,0),
                UI_RNI_prct_Magnesium ~ c(0,0),
                UI_Potassiummg ~ c(0,0),
                UI_RNI_prct_Potassium ~ c(0,0),
                UI_Iodineug ~ c(0,0),
                UI_RNI_prct_Iodine ~ c(0,0),
                UI_Seleniumug ~ c(0,0),
                UI_RNI_prct_Selenium ~ c(0,0),
                UI_Zincmg ~ c(1,0),
                UI_RNI_prct_Zinc ~ c(0,0)),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  include = c(UI_Energykcal, UI_Proteing, UI_RNI_prct_Protein, UI_VitaminAug, UI_RNI_prct_VitaminA, UI_Riboflavinmg, UI_RNI_prct_Riboflavin, UI_Folateug, UI_RNI_prct_Folate, 
              UI_VitaminDug, UI_RNI_prct_VitaminD, UI_VitaminB12ug, UI_RNI_prct_VitaminB12, UI_VitaminCmg, UI_RNI_prct_VitaminC, UI_Ironmg, UI_RNI_prct_Iron, 
              UI_Calciummg, UI_RNI_prct_Calcium, UI_Magnesiummg, UI_RNI_prct_Magnesium, UI_Potassiummg, UI_RNI_prct_Potassium, UI_Iodineug, UI_RNI_prct_Iodine, UI_Seleniumug, UI_RNI_prct_Selenium, 
              UI_Zincmg, UI_RNI_prct_Zinc)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()

table.rnis.simd <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(UI_Energykcal ~ "Total energy (kcal/day)",
            
            UI_Proteing ~ "Protein (g/day)",
            UI_RNI_prct_Protein ~ "Protein, mean as % RNI",
            
            UI_VitaminAug ~ "Vitamin A (µg/day)",
            UI_RNI_prct_VitaminA ~ "Vitamin A, mean as % RNI",
            
            UI_Riboflavinmg ~ "Riboflavin (mg/day)",
            UI_RNI_prct_Riboflavin ~ "Riboflavin, mean as % RNI",
            
            UI_Folateug ~ "Folate (µg/day)",
            UI_RNI_prct_Folate ~ "Folate, mean as % RNI",
            
            UI_VitaminDug ~ "Vitamin D (µg/day)",
            UI_RNI_prct_VitaminD ~ "Vitamin D, mean as % RNI",
            
            UI_VitaminB12ug ~ "Vitamin B12 (µg/day)",
            UI_RNI_prct_VitaminB12 ~ "Vitamin B12, mean as % RNI",
            
            UI_VitaminCmg ~ "Vitamin C (mg/day)",
            UI_RNI_prct_VitaminC ~ "Vitamin C, mean as % RNI",
            
            UI_Ironmg ~ "Iron (mg/day)",
            UI_RNI_prct_Iron ~ "Iron, mean as % RNI",
            
            UI_Calciummg ~ "Calcium (mg/day)",
            UI_RNI_prct_Calcium ~ "Calcium, mean as % RNI",
            
            UI_Magnesiummg ~ "Magnesium (mg/day)",
            UI_RNI_prct_Magnesium ~ "Magnesium, mean as % RNI",
            
            UI_Potassiummg ~ "Potassium (mg/day)",
            UI_RNI_prct_Potassium ~ "Potassium, mean as % RNI",
            
            UI_Iodineug ~ "Iodine (µg/day)",
            UI_RNI_prct_Iodine ~ "Iodine, mean as % RNI",
            
            UI_Seleniumug ~ "Selenium (µg/day)",
            UI_RNI_prct_Selenium ~ "Selenium, mean as % RNI",
            
            UI_Zincmg ~ "Zinc (mg/day)",
            UI_RNI_prct_Zinc ~ "Zinc, mean as % RNI"),
  digits = list(UI_Energykcal ~ c(0,0),
                UI_Proteing ~ c(0,0),
                UI_RNI_prct_Protein ~ c(0,0),
                UI_VitaminAug ~ c(0,0),
                UI_RNI_prct_VitaminA ~ c(0,0),
                UI_Riboflavinmg ~ c(1,0),
                UI_RNI_prct_Riboflavin ~ c(0,0),
                UI_Folateug ~ c(0,0),
                UI_RNI_prct_Folate ~ c(0,0),
                UI_VitaminDug ~ c(0,0),
                UI_RNI_prct_VitaminD ~ c(0,0),
                UI_VitaminB12ug ~ c(1,0),
                UI_RNI_prct_VitaminB12 ~ c(0,0),
                UI_VitaminCmg ~ c(0,0),
                UI_RNI_prct_VitaminC ~ c(0,0),
                UI_Ironmg ~ c(1,0),
                UI_RNI_prct_Iron ~ c(0,0),
                UI_Calciummg ~ c(0,0),
                UI_RNI_prct_Calcium ~ c(0,0),
                UI_Magnesiummg ~ c(0,0),
                UI_RNI_prct_Magnesium ~ c(0,0),
                UI_Potassiummg ~ c(0,0),
                UI_RNI_prct_Potassium ~ c(0,0),
                UI_Iodineug ~ c(0,0),
                UI_RNI_prct_Iodine ~ c(0,0),
                UI_Seleniumug ~ c(0,0),
                UI_RNI_prct_Selenium ~ c(0,0),
                UI_Zincmg ~ c(1,0),
                UI_RNI_prct_Zinc ~ c(0,0)),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  include = c(UI_Energykcal, UI_Proteing, UI_RNI_prct_Protein, UI_VitaminAug, UI_RNI_prct_VitaminA, UI_Riboflavinmg, UI_RNI_prct_Riboflavin, UI_Folateug, UI_RNI_prct_Folate, 
              UI_VitaminDug, UI_RNI_prct_VitaminD, UI_VitaminB12ug, UI_RNI_prct_VitaminB12, UI_VitaminCmg, UI_RNI_prct_VitaminC, UI_Ironmg, UI_RNI_prct_Iron, 
              UI_Calciummg, UI_RNI_prct_Calcium, UI_Magnesiummg, UI_RNI_prct_Magnesium, UI_Potassiummg, UI_RNI_prct_Potassium, UI_Iodineug, UI_RNI_prct_Iodine, UI_Seleniumug, UI_RNI_prct_Selenium, 
              UI_Zincmg, UI_RNI_prct_Zinc)) %>%
  bold_labels() %>% 
  add_p()%>%
  add_significance_stars()


#Convert to main text format

# List of tables to process
tables_nutrients <- list(
  rni.sex = table.rnis.sex,
  rni.age = table.rnis.age,
  rni.age.sex = table.rnis.age.sex,
  rni.simd = table.rnis.simd
)


# List of nutrients
prefixes <- c("total_energy", "protein", "vitamin_a", "riboflavin", "folate", "vitamin_d", 
              "vitamin_b12", "vitamin_c", "iron", "calcium", "magnesium", "potassium", "iodine", "selenium", "zinc")

# Process tables and rename
for (prefix in prefixes) {
  assign(paste0("rni.", gsub("_", ".", prefix)), process_tables(tables_nutrients, prefix)) #calls function defined in chpt 4
}


nutrient_main_tables <- list(rni.total.energy, rni.protein, rni.vitamin.a, rni.riboflavin, rni.folate, rni.vitamin.d,
                             rni.vitamin.b12, rni.vitamin.c, rni.iron, rni.calcium, rni.magnesium, rni.potassium, rni.iodine,
                             rni.selenium, rni.zinc)

for (i in seq_along(nutrient_main_tables)) {
  nutrient_main_tables[[i]] <- nutrient_main_tables[[i]] %>%
    rownames_to_column(var = "Demographics")
}




###output Excel ####
#create workbook
wb4 <- createWorkbook()

#create lists for tabs
nutrient_text_sheet_names <- c("energy", "protein", "vitamin_a", "riboflavin", "folate", "vitamin_d", 
                               "vitamin_b12", "vitamin_c", "iron", "calcium", "magnesium", "potassium", "iodine", "selenium", "zinc")

#assign flextables to tabs 
for (i in seq_along(nutrient_text_sheet_names)) {
  addWorksheet(wb4, sheet = nutrient_text_sheet_names[i])
  writeData(wb4, sheet = nutrient_text_sheet_names[i], x = nutrient_main_tables[[i]])
}

#save workbook
saveWorkbook(wb4, file = paste0("Output/Table_Nutrients_Main_Text_", format(Sys.time(), "%d%m%Y"), ".xlsx"))



# Supplements ####
df.supplements <- df.supplements %>%
  mutate(Supp_vitA = 
           case_when(grepl("vitamin A", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_vitB2 = 
           case_when(grepl("vitamin b2", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_vitB6 = 
           case_when(grepl("vitamin b6", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_vitB12 = 
           case_when(grepl("vitamin b12", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_vitC = 
           case_when(grepl("vitamin c", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_vitD = 
           case_when(grepl("vitamin D", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_multivitamin = 
           case_when(grepl("multivitamin | multi vitamin", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_vitE = 
           case_when(grepl("vitamin E", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_fishOil = 
           case_when(grepl("Omega |cod liver |fish oil", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_iron = 
           case_when(grepl("iron", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_calcium = 
           case_when(grepl("calcium", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_copper = 
           case_when(grepl("copper", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_thiamin = 
           case_when(grepl("Thiamin |Thiamine", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
         Supp_magneisum = 
           case_when(grepl("magnesium", Description_English, ignore.case = TRUE) ~ "1",
                     TRUE ~ "0"),
  )

#pivot to long
Sub_supplements <- df.supplements %>%
  select("UserID", starts_with("supp_")) %>%
  pivot_longer(
    cols = starts_with("supp_"),
    names_to = "Supplement_Type",
    values_to = "Value"
  )


#counts dataframe
Supp_counts <- Sub_supplements %>%
  group_by(Supplement_Type, Value) %>%
  summarize(Count = n(), .groups = "drop") %>%
  filter(Value == 1)

#counts table - item level
Supp_counts.tbl <- Sub_supplements %>%
  filter(Value == 1) %>%
  tbl_summary(by = Value,
              statistic = all_categorical() ~ c("{n}")) %>%
  bold_labels() %>%
  as_flex_table()  

#count of distinct participants reporting each supplement. Each participant is only counted once 
Sub_supplements_counts <- df.supplements %>%
  select("UserID", starts_with("supp_")) %>%
  pivot_longer(
    cols = starts_with("supp_"),
    names_to = "Supplement_Type",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  group_by(Supplement_Type) %>%
  summarize(Unique_UserID_Count = n_distinct(UserID), .groups = "drop") %>%
  flextable() %>%
  theme_booktabs()

n_distinct(df.supplements$UserID)

save_as_docx(Supp_counts.tbl, Sub_supplements_counts, path= paste0("Output/Supplement counts_", format(Sys.time(), "%d%m%Y"), ".docx"))

# Food Categories ####
##Consumers ####

table.consumers <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Consumer_foodcat_1 ~ "Cereals and Cereal Products",
    Consumer_foodcat_2 ~ "Milk and Milk Products",
    Consumer_foodcat_3 ~ "Eggs and Egg Dishes",
    Consumer_foodcat_4 ~ "Fat Spreads",
    Consumer_foodcat_5 ~ "Meat and Meat Products",
    Consumer_foodcat_6 ~ "Fish and Fish Dishes",
    Consumer_foodcat_7 ~ "Sandwiches",
    Consumer_foodcat_8 ~ "Vegetables, potatoes",
    Consumer_foodcat_9 ~ "Fruit",
    Consumer_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Consumer_foodcat_11 ~ "Savoury Snacks",
    Consumer_foodcat_12 ~ "Nuts and Seeds",
    Consumer_foodcat_13 ~ "Non-alcoholic beverages",
    Consumer_foodcat_15 ~ "Miscellaneous",
    Consumer_foodcat_16 ~ "Toddler foods",
    Consumer_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(Consumer_foodcat_1:Consumer_foodcat_13,Consumer_foodcat_15,Consumer_foodcat_16,Consumer_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#output to Excel
df.table.consumers <- table.consumers$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)


wb <- createWorkbook()
addWorksheet(wb, "Consumers")
writeData(wb, sheet = "Consumers", df.table.consumers)

# Save the workbook
saveWorkbook(wb, paste0("Output/Table_FoodCategory_Consumers_", format(Sys.time(), "%d%m%Y"), ".xlsx"))


##Contributions to Energy and Nutrients ####

##Energy
table.energy.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Energykcal_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Energykcal_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Energykcal_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Energykcal_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Energykcal_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Energykcal_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Energykcal_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Energykcal_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Energykcal_foodcat_9 ~ "Fruit",
    Avg_Prop_Energykcal_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Energykcal_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Energykcal_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Energykcal_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Energykcal_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Energykcal_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Energykcal_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Energykcal_foodcat_1:Avg_Prop_Energykcal_foodcat_13,Avg_Prop_Energykcal_foodcat_15,Avg_Prop_Energykcal_foodcat_16,Avg_Prop_Energykcal_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Carbohydrates
table.carbohydrate.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Carbohydrateg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Carbohydrateg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Carbohydrateg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Carbohydrateg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Carbohydrateg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Carbohydrateg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Carbohydrateg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Carbohydrateg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Carbohydrateg_foodcat_9 ~ "Fruit",
    Avg_Prop_Carbohydrateg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Carbohydrateg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Carbohydrateg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Carbohydrateg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Carbohydrateg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Carbohydrateg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Carbohydrateg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Carbohydrateg_foodcat_1:Avg_Prop_Carbohydrateg_foodcat_13,Avg_Prop_Carbohydrateg_foodcat_15,Avg_Prop_Carbohydrateg_foodcat_16,Avg_Prop_Carbohydrateg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Free sugars
table.freesugars.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_FreeSugarsg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_FreeSugarsg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_FreeSugarsg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_FreeSugarsg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_FreeSugarsg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_FreeSugarsg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_FreeSugarsg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_FreeSugarsg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_FreeSugarsg_foodcat_9 ~ "Fruit",
    Avg_Prop_FreeSugarsg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_FreeSugarsg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_FreeSugarsg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_FreeSugarsg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_FreeSugarsg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_FreeSugarsg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_FreeSugarsg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_FreeSugarsg_foodcat_1:Avg_Prop_FreeSugarsg_foodcat_13,Avg_Prop_FreeSugarsg_foodcat_15,Avg_Prop_FreeSugarsg_foodcat_16,Avg_Prop_FreeSugarsg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


##Fibre
table.fibre.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_AOACFibreg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_AOACFibreg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_AOACFibreg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_AOACFibreg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_AOACFibreg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_AOACFibreg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_AOACFibreg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_AOACFibreg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_AOACFibreg_foodcat_9 ~ "Fruit",
    Avg_Prop_AOACFibreg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_AOACFibreg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_AOACFibreg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_AOACFibreg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_AOACFibreg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_AOACFibreg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_AOACFibreg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_AOACFibreg_foodcat_1:Avg_Prop_AOACFibreg_foodcat_13,Avg_Prop_AOACFibreg_foodcat_15,Avg_Prop_AOACFibreg_foodcat_16,Avg_Prop_AOACFibreg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Fat
table.fat.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Fatg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Fatg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Fatg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Fatg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Fatg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Fatg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Fatg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Fatg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Fatg_foodcat_9 ~ "Fruit",
    Avg_Prop_Fatg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Fatg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Fatg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Fatg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Fatg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Fatg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Fatg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Fatg_foodcat_1:Avg_Prop_Fatg_foodcat_13,Avg_Prop_Fatg_foodcat_15,Avg_Prop_Fatg_foodcat_16,Avg_Prop_Fatg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Saturated fat
table.satfat.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_SatFatg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_SatFatg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_SatFatg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_SatFatg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_SatFatg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_SatFatg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_SatFatg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_SatFatg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_SatFatg_foodcat_9 ~ "Fruit",
    Avg_Prop_SatFatg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_SatFatg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_SatFatg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_SatFatg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_SatFatg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_SatFatg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_SatFatg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_SatFatg_foodcat_1:Avg_Prop_SatFatg_foodcat_13,Avg_Prop_SatFatg_foodcat_15,Avg_Prop_SatFatg_foodcat_16,Avg_Prop_SatFatg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Trans fat
table.transfat.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_TransFatg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_TransFatg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_TransFatg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_TransFatg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_TransFatg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_TransFatg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_TransFatg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_TransFatg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_TransFatg_foodcat_9 ~ "Fruit",
    Avg_Prop_TransFatg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_TransFatg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_TransFatg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_TransFatg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_TransFatg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_TransFatg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_TransFatg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_TransFatg_foodcat_1:Avg_Prop_TransFatg_foodcat_13,Avg_Prop_TransFatg_foodcat_15,Avg_Prop_TransFatg_foodcat_16,Avg_Prop_TransFatg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Protein
table.protein.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Proteing_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Proteing_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Proteing_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Proteing_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Proteing_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Proteing_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Proteing_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Proteing_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Proteing_foodcat_9 ~ "Fruit",
    Avg_Prop_Proteing_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Proteing_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Proteing_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Proteing_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Proteing_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Proteing_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Proteing_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Proteing_foodcat_1:Avg_Prop_Proteing_foodcat_13,Avg_Prop_Proteing_foodcat_15,Avg_Prop_Proteing_foodcat_16,Avg_Prop_Proteing_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Vitamin A
table.vitaminA.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_VitaminAug_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_VitaminAug_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_VitaminAug_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_VitaminAug_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_VitaminAug_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_VitaminAug_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_VitaminAug_foodcat_7 ~ "Sandwiches",
    Avg_Prop_VitaminAug_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_VitaminAug_foodcat_9 ~ "Fruit",
    Avg_Prop_VitaminAug_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_VitaminAug_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_VitaminAug_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_VitaminAug_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_VitaminAug_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_VitaminAug_foodcat_16 ~ "Toddler foods",
    Avg_Prop_VitaminAug_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_VitaminAug_foodcat_1:Avg_Prop_VitaminAug_foodcat_13,Avg_Prop_VitaminAug_foodcat_15,Avg_Prop_VitaminAug_foodcat_16,Avg_Prop_VitaminAug_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Riboflavin
table.riboflavin.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Riboflavinmg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Riboflavinmg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Riboflavinmg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Riboflavinmg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Riboflavinmg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Riboflavinmg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Riboflavinmg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Riboflavinmg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Riboflavinmg_foodcat_9 ~ "Fruit",
    Avg_Prop_Riboflavinmg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Riboflavinmg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Riboflavinmg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Riboflavinmg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Riboflavinmg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Riboflavinmg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Riboflavinmg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Riboflavinmg_foodcat_1:Avg_Prop_Riboflavinmg_foodcat_13,Avg_Prop_Riboflavinmg_foodcat_15,Avg_Prop_Riboflavinmg_foodcat_16,Avg_Prop_Riboflavinmg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Folate
table.folate.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Folateug_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Folateug_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Folateug_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Folateug_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Folateug_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Folateug_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Folateug_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Folateug_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Folateug_foodcat_9 ~ "Fruit",
    Avg_Prop_Folateug_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Folateug_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Folateug_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Folateug_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Folateug_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Folateug_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Folateug_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Folateug_foodcat_1:Avg_Prop_Folateug_foodcat_13,Avg_Prop_Folateug_foodcat_15,Avg_Prop_Folateug_foodcat_16,Avg_Prop_Folateug_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Vitamin D
table.vitaminD.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_VitaminDug_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_VitaminDug_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_VitaminDug_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_VitaminDug_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_VitaminDug_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_VitaminDug_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_VitaminDug_foodcat_7 ~ "Sandwiches",
    Avg_Prop_VitaminDug_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_VitaminDug_foodcat_9 ~ "Fruit",
    Avg_Prop_VitaminDug_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_VitaminDug_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_VitaminDug_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_VitaminDug_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_VitaminDug_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_VitaminDug_foodcat_16 ~ "Toddler foods",
    Avg_Prop_VitaminDug_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_VitaminDug_foodcat_1:Avg_Prop_VitaminDug_foodcat_13,Avg_Prop_VitaminDug_foodcat_15,Avg_Prop_VitaminDug_foodcat_16,Avg_Prop_VitaminDug_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Vitamin B12
table.vitaminB12.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_VitaminB12ug_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_VitaminB12ug_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_VitaminB12ug_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_VitaminB12ug_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_VitaminB12ug_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_VitaminB12ug_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_VitaminB12ug_foodcat_7 ~ "Sandwiches",
    Avg_Prop_VitaminB12ug_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_VitaminB12ug_foodcat_9 ~ "Fruit",
    Avg_Prop_VitaminB12ug_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_VitaminB12ug_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_VitaminB12ug_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_VitaminB12ug_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_VitaminB12ug_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_VitaminB12ug_foodcat_16 ~ "Toddler foods",
    Avg_Prop_VitaminB12ug_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_VitaminB12ug_foodcat_1:Avg_Prop_VitaminB12ug_foodcat_13,Avg_Prop_VitaminB12ug_foodcat_15,Avg_Prop_VitaminB12ug_foodcat_16,Avg_Prop_VitaminB12ug_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Vitamin C
table.vitaminC.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_VitaminCmg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_VitaminCmg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_VitaminCmg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_VitaminCmg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_VitaminCmg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_VitaminCmg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_VitaminCmg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_VitaminCmg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_VitaminCmg_foodcat_9 ~ "Fruit",
    Avg_Prop_VitaminCmg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_VitaminCmg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_VitaminCmg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_VitaminCmg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_VitaminCmg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_VitaminCmg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_VitaminCmg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_VitaminCmg_foodcat_1:Avg_Prop_VitaminCmg_foodcat_13,Avg_Prop_VitaminCmg_foodcat_15,Avg_Prop_VitaminCmg_foodcat_16,Avg_Prop_VitaminCmg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Iron
table.iron.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Ironmg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Ironmg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Ironmg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Ironmg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Ironmg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Ironmg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Ironmg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Ironmg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Ironmg_foodcat_9 ~ "Fruit",
    Avg_Prop_Ironmg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Ironmg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Ironmg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Ironmg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Ironmg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Ironmg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Ironmg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Ironmg_foodcat_1:Avg_Prop_Ironmg_foodcat_13,Avg_Prop_Ironmg_foodcat_15,Avg_Prop_Ironmg_foodcat_16,Avg_Prop_Ironmg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Calcium
table.calcium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Calciummg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Calciummg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Calciummg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Calciummg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Calciummg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Calciummg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Calciummg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Calciummg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Calciummg_foodcat_9 ~ "Fruit",
    Avg_Prop_Calciummg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Calciummg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Calciummg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Calciummg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Calciummg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Calciummg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Calciummg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Calciummg_foodcat_1:Avg_Prop_Calciummg_foodcat_13,Avg_Prop_Calciummg_foodcat_15,Avg_Prop_Calciummg_foodcat_16,Avg_Prop_Calciummg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Magnesium
table.magnesium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Magnesiummg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Magnesiummg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Magnesiummg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Magnesiummg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Magnesiummg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Magnesiummg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Magnesiummg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Magnesiummg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Magnesiummg_foodcat_9 ~ "Fruit",
    Avg_Prop_Magnesiummg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Magnesiummg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Magnesiummg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Magnesiummg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Magnesiummg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Magnesiummg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Magnesiummg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Magnesiummg_foodcat_1:Avg_Prop_Magnesiummg_foodcat_13,Avg_Prop_Magnesiummg_foodcat_15,Avg_Prop_Magnesiummg_foodcat_16,Avg_Prop_Magnesiummg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Potassium
table.potassium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Potassiummg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Potassiummg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Potassiummg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Potassiummg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Potassiummg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Potassiummg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Potassiummg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Potassiummg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Potassiummg_foodcat_9 ~ "Fruit",
    Avg_Prop_Potassiummg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Potassiummg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Potassiummg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Potassiummg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Potassiummg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Potassiummg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Potassiummg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Potassiummg_foodcat_1:Avg_Prop_Potassiummg_foodcat_13,Avg_Prop_Potassiummg_foodcat_15,Avg_Prop_Potassiummg_foodcat_16,Avg_Prop_Potassiummg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Iodine
table.iodine.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Iodineug_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Iodineug_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Iodineug_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Iodineug_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Iodineug_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Iodineug_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Iodineug_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Iodineug_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Iodineug_foodcat_9 ~ "Fruit",
    Avg_Prop_Iodineug_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Iodineug_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Iodineug_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Iodineug_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Iodineug_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Iodineug_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Iodineug_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Iodineug_foodcat_1:Avg_Prop_Iodineug_foodcat_13,Avg_Prop_Iodineug_foodcat_15,Avg_Prop_Iodineug_foodcat_16,Avg_Prop_Iodineug_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Selenium
table.selenium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Seleniumug_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Seleniumug_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Seleniumug_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Seleniumug_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Seleniumug_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Seleniumug_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Seleniumug_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Seleniumug_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Seleniumug_foodcat_9 ~ "Fruit",
    Avg_Prop_Seleniumug_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Seleniumug_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Seleniumug_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Seleniumug_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Seleniumug_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Seleniumug_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Seleniumug_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Seleniumug_foodcat_1:Avg_Prop_Seleniumug_foodcat_13,Avg_Prop_Seleniumug_foodcat_15,Avg_Prop_Seleniumug_foodcat_16,Avg_Prop_Seleniumug_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Zinc
table.zinc.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Zincmg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Zincmg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Zincmg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Zincmg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Zincmg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Zincmg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Zincmg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Zincmg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Zincmg_foodcat_9 ~ "Fruit",
    Avg_Prop_Zincmg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Zincmg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Zincmg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Zincmg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Zincmg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Zincmg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Zincmg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Zincmg_foodcat_1:Avg_Prop_Zincmg_foodcat_13,Avg_Prop_Zincmg_foodcat_15,Avg_Prop_Zincmg_foodcat_16,Avg_Prop_Zincmg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Sodium
table.sodium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Sodiummg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Sodiummg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Sodiummg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Sodiummg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Sodiummg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Sodiummg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Sodiummg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Sodiummg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Sodiummg_foodcat_9 ~ "Fruit",
    Avg_Prop_Sodiummg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Sodiummg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Sodiummg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Sodiummg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Sodiummg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Sodiummg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Sodiummg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Sodiummg_foodcat_1:Avg_Prop_Sodiummg_foodcat_13,Avg_Prop_Sodiummg_foodcat_15,Avg_Prop_Sodiummg_foodcat_16,Avg_Prop_Sodiummg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


##Output Contributions to Excel ####

# Convert flextables to data frames
df.table.energy <- table.energy.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.carbohydrate <- table.carbohydrate.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.freesugars <- table.freesugars.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.fibre <- table.fibre.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.fat <- table.fat.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.satfat <- table.satfat.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.transfat <- table.transfat.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.protein <- table.protein.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.vitaminA <- table.vitaminA.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.riboflavin <- table.riboflavin.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.folate <- table.folate.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.vitaminD <- table.vitaminD.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.vitaminB12 <- table.vitaminB12.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.vitaminC <- table.vitaminC.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.iron <- table.iron.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.calcium <- table.calcium.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.magnesium <- table.magnesium.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.potassium <- table.potassium.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.iodine <- table.iodine.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.selenium <- table.selenium.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.zinc <- table.zinc.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

df.table.sodium <- table.sodium.fg$body$dataset %>%
  rename("Food Group" = label,
         Overall = stat_0,
         "Female, 2-4y" = stat_1,
         "Female, 5-10y" = stat_2,
         "Female, 11-15y" = stat_3,
         "Male, 2-4y" = stat_4,
         "Male, 5-10y" = stat_5,
         "Male, 11-15y" = stat_6)

write_xlsx(
  list(
    "Energy" = df.table.energy,
    "Carbohydrate" = df.table.carbohydrate,
    "Free sugars" = df.table.freesugars,
    "Fibre" = df.table.fibre,
    "Fat" = df.table.fat,
    "Saturated fat" = df.table.satfat,
    "Trans fat" = df.table.transfat,
    "Protein" = df.table.protein,
    
    "Vitamin A" = df.table.vitaminA,
    "Riboflavin" = df.table.riboflavin,
    "Folate" = df.table.folate,
    "Vitamin D" = df.table.vitaminD,
    "Vitamin B12" = df.table.vitaminB12,
    "Vitamin C" = df.table.vitaminC,
    
    "Iron" = df.table.iron,
    "Calcium" = df.table.calcium,
    "Magnesium" = df.table.magnesium,
    "Potassium" = df.table.potassium,
    "Iodine" = df.table.iodine,
    "Selenium" = df.table.selenium,
    "Zinc" = df.table.zinc,
    "Sodium" = df.table.sodium
  ),
  path = paste0("Output/Table_FoodCat_", format(Sys.time(), "%d%m%Y"), ".xlsx")
)


# Food Groups - NDNS - Chpt 6 ####

##Consumers ####
table.ndns.consumers <- tbl_svysummary(
  svy.df.intake24_participant, 
  label = c(
    Consumer_ndns_1 ~ "Pasta, rice and other miscellaneous cereals",
    Consumer_ndns_2 ~ "White bread",
    Consumer_ndns_3 ~ "Wholemeal bread",
    Consumer_ndns_4 ~ "Other breads",
    Consumer_ndns_5 ~ "High fibre breakfast cereals",
    Consumer_ndns_6 ~ "Other breakfast cereals",
    Consumer_ndns_7 ~ "Biscuits",
    Consumer_ndns_8 ~ "Buns, cakes, pastries and fruit pies",
    Consumer_ndns_9 ~ "Puddings",
    Consumer_ndns_10 ~ "Whole milk",
    Consumer_ndns_11 ~ "Semi-skimmed milk",
    Consumer_ndns_12 ~ "Skimmed milk",
    Consumer_ndns_13 ~ "Other milk and cream",
    Consumer_ndns_14 ~ "Cheese",
    Consumer_ndns_15 ~ "Yogurt, fromage frais and other dairy desserts",
    Consumer_ndns_16 ~ "Eggs and egg dishes",
    Consumer_ndns_17 ~ "Butter",
    Consumer_ndns_18 ~ "Polyunsaturated margarine and oils",
    Consumer_ndns_19 ~ "Low fat spread",
    Consumer_ndns_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
    Consumer_ndns_21 ~ "Reduced fat spread",
    Consumer_ndns_22 ~ "Bacon and ham",
    Consumer_ndns_23 ~ "Beef, veal and dishes",
    Consumer_ndns_24 ~ "Lamb and dishes",
    Consumer_ndns_25 ~ "Pork and dishes",
    Consumer_ndns_26 ~ "Coated chicken and turkey manufactured",
    Consumer_ndns_27 ~ "Chicken and turkey dishes",
    Consumer_ndns_28 ~ "Liver, products and dishes",
    Consumer_ndns_29 ~ "Burgers and kebabs",
    Consumer_ndns_30 ~ "Sausages",
    Consumer_ndns_31 ~ "Meat pies and pastries",
    Consumer_ndns_32 ~ "Other meat and meat products",
    Consumer_ndns_33 ~ "White fish coated or fried",
    Consumer_ndns_34 ~ "Other white fish, shellfish and fish dishes",
    Consumer_ndns_35 ~ "Oily fish",
    Consumer_ndns_36 ~ "Salad and other raw vegetables",
    Consumer_ndns_37 ~ "Vegetables (not raw)",
    Consumer_ndns_38 ~ "Chips, fried and roast potatoes and potato products",
    Consumer_ndns_39 ~ "Other potatoes, potato salads and dishes",
    Consumer_ndns_40 ~ "Fruit",
    Consumer_ndns_41 ~ "Sugars, preserves and sweet spreads",
    Consumer_ndns_42 ~ "Crisps and savoury snacks",
    Consumer_ndns_43 ~ "Sugar confectionery",
    Consumer_ndns_44 ~ "Chocolate confectionery",
    Consumer_ndns_45 ~ "Fruit juice",
    Consumer_ndns_47 ~ "Spirits and liqueurs",
    Consumer_ndns_48 ~ "Wine",
    Consumer_ndns_49 ~ "Beer lager cider and perry",
    Consumer_ndns_50 ~ "Miscellaneous",
    Consumer_ndns_51 ~ "Tea, coffee and water",
    Consumer_ndns_52 ~ "Commercial toddlers foods and drinks",
    Consumer_ndns_53 ~ "Ice cream",
    Consumer_ndns_54 ~ "Dietary supplements",
    Consumer_ndns_55 ~ "Artificial sweeteners",
    Consumer_ndns_56 ~ "Nuts and seeds",
    Consumer_ndns_57 ~ "Soft drinks, not diet",
    Consumer_ndns_58 ~ "Soft drinks, diet",
    Consumer_ndns_59 ~ "Brown, granary and wheatgerm bread",
    Consumer_ndns_60 ~ "1% Milk",
    Consumer_ndns_62 ~ "Sandwiches",
    Consumer_ndns_63 ~ "Other milk and cream DF",                         
    Consumer_ndns_64 ~ "Cheese DF",
    Consumer_ndns_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
    Consumer_ndns_66 ~ "Ice cream DF"),
  statistic = list(all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(Consumer_ndns_1:Consumer_ndns_45,Consumer_ndns_47:Consumer_ndns_60,Consumer_ndns_62:Consumer_ndns_66)) %>%
  bold_labels() %>% 
  as_flex_table()

table.ndns.consumers.sex <- tbl_svysummary(
  svy.df.intake24_participant_fm, 
  by = age_sex_cat,
  label = c(
    Consumer_ndns_1 ~ "Pasta, rice and other miscellaneous cereals",
    Consumer_ndns_2 ~ "White bread",
    Consumer_ndns_3 ~ "Wholemeal bread",
    Consumer_ndns_4 ~ "Other breads",
    Consumer_ndns_5 ~ "High fibre breakfast cereals",
    Consumer_ndns_6 ~ "Other breakfast cereals",
    Consumer_ndns_7 ~ "Biscuits",
    Consumer_ndns_8 ~ "Buns, cakes, pastries and fruit pies",
    Consumer_ndns_9 ~ "Puddings",
    Consumer_ndns_10 ~ "Whole milk",
    Consumer_ndns_11 ~ "Semi-skimmed milk",
    Consumer_ndns_12 ~ "Skimmed milk",
    Consumer_ndns_13 ~ "Other milk and cream",
    Consumer_ndns_14 ~ "Cheese",
    Consumer_ndns_15 ~ "Yogurt, fromage frais and other dairy desserts",
    Consumer_ndns_16 ~ "Eggs and egg dishes",
    Consumer_ndns_17 ~ "Butter",
    Consumer_ndns_18 ~ "Polyunsaturated margarine and oils",
    Consumer_ndns_19 ~ "Low fat spread",
    Consumer_ndns_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
    Consumer_ndns_21 ~ "Reduced fat spread",
    Consumer_ndns_22 ~ "Bacon and ham",
    Consumer_ndns_23 ~ "Beef, veal and dishes",
    Consumer_ndns_24 ~ "Lamb and dishes",
    Consumer_ndns_25 ~ "Pork and dishes",
    Consumer_ndns_26 ~ "Coated chicken and turkey manufactured",
    Consumer_ndns_27 ~ "Chicken and turkey dishes",
    Consumer_ndns_28 ~ "Liver, products and dishes",
    Consumer_ndns_29 ~ "Burgers and kebabs",
    Consumer_ndns_30 ~ "Sausages",
    Consumer_ndns_31 ~ "Meat pies and pastries",
    Consumer_ndns_32 ~ "Other meat and meat products",
    Consumer_ndns_33 ~ "White fish coated or fried",
    Consumer_ndns_34 ~ "Other white fish, shellfish and fish dishes",
    Consumer_ndns_35 ~ "Oily fish",
    Consumer_ndns_36 ~ "Salad and other raw vegetables",
    Consumer_ndns_37 ~ "Vegetables (not raw)",
    Consumer_ndns_38 ~ "Chips, fried and roast potatoes and potato products",
    Consumer_ndns_39 ~ "Other potatoes, potato salads and dishes",
    Consumer_ndns_40 ~ "Fruit",
    Consumer_ndns_41 ~ "Sugars, preserves and sweet spreads",
    Consumer_ndns_42 ~ "Crisps and savoury snacks",
    Consumer_ndns_43 ~ "Sugar confectionery",
    Consumer_ndns_44 ~ "Chocolate confectionery",
    Consumer_ndns_45 ~ "Fruit juice",
    Consumer_ndns_47 ~ "Spirits and liqueurs",
    Consumer_ndns_48 ~ "Wine",
    Consumer_ndns_49 ~ "Beer lager cider and perry",
    Consumer_ndns_50 ~ "Miscellaneous",
    Consumer_ndns_51 ~ "Tea, coffee and water",
    Consumer_ndns_52 ~ "Commercial toddlers foods and drinks",
    Consumer_ndns_53 ~ "Ice cream",
    Consumer_ndns_54 ~ "Dietary supplements",
    Consumer_ndns_55 ~ "Artificial sweeteners",
    Consumer_ndns_56 ~ "Nuts and seeds",
    Consumer_ndns_57 ~ "Soft drinks, not diet",
    Consumer_ndns_58 ~ "Soft drinks, diet",
    Consumer_ndns_59 ~ "Brown, granary and wheatgerm bread",
    Consumer_ndns_60 ~ "1% Milk",
    Consumer_ndns_62 ~ "Sandwiches",
    Consumer_ndns_63 ~ "Other milk and cream DF",                         
    Consumer_ndns_64 ~ "Cheese DF",
    Consumer_ndns_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
    Consumer_ndns_66 ~ "Ice cream DF"),
  statistic = list(all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(Consumer_ndns_1:Consumer_ndns_45,Consumer_ndns_47:Consumer_ndns_60,Consumer_ndns_62:Consumer_ndns_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

table.ndns.consumers.simd <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = simd_quintile,
  label = c(
    Consumer_ndns_1 ~ "Pasta, rice and other miscellaneous cereals",
    Consumer_ndns_2 ~ "White bread",
    Consumer_ndns_3 ~ "Wholemeal bread",
    Consumer_ndns_4 ~ "Other breads",
    Consumer_ndns_5 ~ "High fibre breakfast cereals",
    Consumer_ndns_6 ~ "Other breakfast cereals",
    Consumer_ndns_7 ~ "Biscuits",
    Consumer_ndns_8 ~ "Buns, cakes, pastries and fruit pies",
    Consumer_ndns_9 ~ "Puddings",
    Consumer_ndns_10 ~ "Whole milk",
    Consumer_ndns_11 ~ "Semi-skimmed milk",
    Consumer_ndns_12 ~ "Skimmed milk",
    Consumer_ndns_13 ~ "Other milk and cream",
    Consumer_ndns_14 ~ "Cheese",
    Consumer_ndns_15 ~ "Yogurt, fromage frais and other dairy desserts",
    Consumer_ndns_16 ~ "Eggs and egg dishes",
    Consumer_ndns_17 ~ "Butter",
    Consumer_ndns_18 ~ "Polyunsaturated margarine and oils",
    Consumer_ndns_19 ~ "Low fat spread",
    Consumer_ndns_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
    Consumer_ndns_21 ~ "Reduced fat spread",
    Consumer_ndns_22 ~ "Bacon and ham",
    Consumer_ndns_23 ~ "Beef, veal and dishes",
    Consumer_ndns_24 ~ "Lamb and dishes",
    Consumer_ndns_25 ~ "Pork and dishes",
    Consumer_ndns_26 ~ "Coated chicken and turkey manufactured",
    Consumer_ndns_27 ~ "Chicken and turkey dishes",
    Consumer_ndns_28 ~ "Liver, products and dishes",
    Consumer_ndns_29 ~ "Burgers and kebabs",
    Consumer_ndns_30 ~ "Sausages",
    Consumer_ndns_31 ~ "Meat pies and pastries",
    Consumer_ndns_32 ~ "Other meat and meat products",
    Consumer_ndns_33 ~ "White fish coated or fried",
    Consumer_ndns_34 ~ "Other white fish, shellfish and fish dishes",
    Consumer_ndns_35 ~ "Oily fish",
    Consumer_ndns_36 ~ "Salad and other raw vegetables",
    Consumer_ndns_37 ~ "Vegetables (not raw)",
    Consumer_ndns_38 ~ "Chips, fried and roast potatoes and potato products",
    Consumer_ndns_39 ~ "Other potatoes, potato salads and dishes",
    Consumer_ndns_40 ~ "Fruit",
    Consumer_ndns_41 ~ "Sugars, preserves and sweet spreads",
    Consumer_ndns_42 ~ "Crisps and savoury snacks",
    Consumer_ndns_43 ~ "Sugar confectionery",
    Consumer_ndns_44 ~ "Chocolate confectionery",
    Consumer_ndns_45 ~ "Fruit juice",
    Consumer_ndns_47 ~ "Spirits and liqueurs",
    Consumer_ndns_48 ~ "Wine",
    Consumer_ndns_49 ~ "Beer lager cider and perry",
    Consumer_ndns_50 ~ "Miscellaneous",
    Consumer_ndns_51 ~ "Tea, coffee and water",
    Consumer_ndns_52 ~ "Commercial toddlers foods and drinks",
    Consumer_ndns_53 ~ "Ice cream",
    Consumer_ndns_54 ~ "Dietary supplements",
    Consumer_ndns_55 ~ "Artificial sweeteners",
    Consumer_ndns_56 ~ "Nuts and seeds",
    Consumer_ndns_57 ~ "Soft drinks, not diet",
    Consumer_ndns_58 ~ "Soft drinks, diet",
    Consumer_ndns_59 ~ "Brown, granary and wheatgerm bread",
    Consumer_ndns_60 ~ "1% Milk",
    Consumer_ndns_62 ~ "Sandwiches",
    Consumer_ndns_63 ~ "Other milk and cream DF",                         
    Consumer_ndns_64 ~ "Cheese DF",
    Consumer_ndns_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
    Consumer_ndns_66 ~ "Ice cream DF"),
  statistic = list(all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(Consumer_ndns_1:Consumer_ndns_45,Consumer_ndns_47:Consumer_ndns_60,Consumer_ndns_62:Consumer_ndns_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

save_as_docx("Overall"=table.ndns.consumers, "By Sex/Age" = table.ndns.consumers.sex, "By SIMD" = table.ndns.consumers.simd, path="Output/Table_NDNS_Consumers.docx")


## Grams ####
#sex
table.ndns_grams_sex <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = Sex,
  label = c(
    UI_ndnsg_1 ~ "Pasta, rice and other miscellaneous cereals",
    UI_ndnsg_2 ~ "White bread",
    UI_ndnsg_3 ~ "Wholemeal bread",
    # UI_ndnsg_4 ~ "Other breads", 
    UI_ndnsg_5 ~ "High fibre breakfast cereals",
    UI_ndnsg_6 ~ "Other breakfast cereals",
    UI_ndnsg_7 ~ "Biscuits",
    UI_ndnsg_8 ~ "Buns, cakes, pastries and fruit pies",
    UI_ndnsg_9 ~ "Puddings",
    UI_ndnsg_10 ~ "Whole milk",
    UI_ndnsg_11 ~ "Semi-skimmed milk",
    # UI_ndnsg_12 ~ "Skimmed milk",
    UI_ndnsg_13 ~ "Other milk and cream",
    UI_ndnsg_14 ~ "Cheese",
    UI_ndnsg_15 ~ "Yogurt, fromage frais and other dairy desserts",
    UI_ndnsg_16 ~ "Eggs and egg dishes",
    UI_ndnsg_17 ~ "Butter",
    # UI_ndnsg_18 ~ "Polyunsaturated margarine and oils",
    # UI_ndnsg_19 ~ "Low fat spread",
    # UI_ndnsg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
    UI_ndnsg_21 ~ "Reduced fat spread",
    # UI_ndnsg_22 ~ "Bacon and ham",
    UI_ndnsg_23 ~ "Beef, veal and dishes",
    #  UI_ndnsg_24 ~ "Lamb and dishes",
    #  UI_ndnsg_25 ~ "Pork and dishes",
    UI_ndnsg_26 ~ "Coated chicken and turkey manufactured",
    UI_ndnsg_27 ~ "Chicken and turkey dishes",
    #  UI_ndnsg_28 ~ "Liver, products and dishes",
    #  UI_ndnsg_29 ~ "Burgers and kebabs",
    UI_ndnsg_30 ~ "Sausages",
    UI_ndnsg_31 ~ "Meat pies and pastries",
    #  UI_ndnsg_32 ~ "Other meat and meat products",
    UI_ndnsg_33 ~ "White fish coated or fried",
    #  UI_ndnsg_34 ~ "Other white fish, shellfish and fish dishes",
    #  UI_ndnsg_35 ~ "Oily fish",
    UI_ndnsg_36 ~ "Salad and other raw vegetables",
    UI_ndnsg_37 ~ "Vegetables (not raw)",
    UI_ndnsg_38 ~ "Chips, fried and roast potatoes and potato products",
    UI_ndnsg_39 ~ "Other potatoes, potato salads and dishes",
    UI_ndnsg_40 ~ "Fruit",
    UI_ndnsg_41 ~ "Sugars, preserves and sweet spreads",
    UI_ndnsg_42 ~ "Crisps and savoury snacks",
    UI_ndnsg_43 ~ "Sugar confectionery",
    UI_ndnsg_44 ~ "Chocolate confectionery",
    UI_ndnsg_45 ~ "Fruit juice",
    #  UI_ndnsg_47 ~ "Spirits and liqueurs",
    #  UI_ndnsg_48 ~ "Wine",
    #  UI_ndnsg_49 ~ "Beer lager cider and perry",
    UI_ndnsg_50 ~ "Miscellaneous",
    #  UI_ndnsg_51 ~ "Tea, coffee and water",
    #  UI_ndnsg_52 ~ "Commercial toddlers foods and drinks",
    UI_ndnsg_53 ~ "Ice cream",
    #  UI_ndnsg_54 ~ "Dietary supplements",
    #  UI_ndnsg_55 ~ "Artificial sweeteners",
    #  UI_ndnsg_56 ~ "Nuts and seeds",
    UI_ndnsg_57 ~ "Soft drinks, not diet",
    UI_ndnsg_58 ~ "Soft drinks, diet",
    #  UI_ndnsg_59 ~ "Brown, granary and wheatgerm bread",
    #  UI_ndnsg_60 ~ "1% Milk",
    UI_ndnsg_62 ~ "Sandwiches"
    #  UI_ndnsg_63 ~ "Other milk and cream DF",                         
    #  UI_ndnsg_64 ~ "Cheese DF",
    #  UI_ndnsg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
    #  UI_ndnsg_66 ~ "Ice cream DF"
  ),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(UI_ndnsg_1:UI_ndnsg_3,UI_ndnsg_5:UI_ndnsg_11, UI_ndnsg_13:UI_ndnsg_17, UI_ndnsg_21, UI_ndnsg_23,UI_ndnsg_26, 
              UI_ndnsg_27, UI_ndnsg_30, UI_ndnsg_31, UI_ndnsg_33, UI_ndnsg_36:UI_ndnsg_45, UI_ndnsg_50, UI_ndnsg_53, UI_ndnsg_57, 
              UI_ndnsg_58, UI_ndnsg_62)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#age
table.ndns_grams_age <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = age_cat,
  label = c(
    UI_ndnsg_1 ~ "Pasta, rice and other miscellaneous cereals",
    UI_ndnsg_2 ~ "White bread",
    UI_ndnsg_3 ~ "Wholemeal bread",
    # UI_ndnsg_4 ~ "Other breads", 
    UI_ndnsg_5 ~ "High fibre breakfast cereals",
    UI_ndnsg_6 ~ "Other breakfast cereals",
    UI_ndnsg_7 ~ "Biscuits",
    UI_ndnsg_8 ~ "Buns, cakes, pastries and fruit pies",
    UI_ndnsg_9 ~ "Puddings",
    UI_ndnsg_10 ~ "Whole milk",
    UI_ndnsg_11 ~ "Semi-skimmed milk",
    # UI_ndnsg_12 ~ "Skimmed milk",
    UI_ndnsg_13 ~ "Other milk and cream",
    UI_ndnsg_14 ~ "Cheese",
    UI_ndnsg_15 ~ "Yogurt, fromage frais and other dairy desserts",
    UI_ndnsg_16 ~ "Eggs and egg dishes",
    UI_ndnsg_17 ~ "Butter",
    # UI_ndnsg_18 ~ "Polyunsaturated margarine and oils",
    # UI_ndnsg_19 ~ "Low fat spread",
    # UI_ndnsg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
    UI_ndnsg_21 ~ "Reduced fat spread",
    # UI_ndnsg_22 ~ "Bacon and ham",
    UI_ndnsg_23 ~ "Beef, veal and dishes",
    #  UI_ndnsg_24 ~ "Lamb and dishes",
    #  UI_ndnsg_25 ~ "Pork and dishes",
    UI_ndnsg_26 ~ "Coated chicken and turkey manufactured",
    UI_ndnsg_27 ~ "Chicken and turkey dishes",
    #  UI_ndnsg_28 ~ "Liver, products and dishes",
    #  UI_ndnsg_29 ~ "Burgers and kebabs",
    UI_ndnsg_30 ~ "Sausages",
    UI_ndnsg_31 ~ "Meat pies and pastries",
    #  UI_ndnsg_32 ~ "Other meat and meat products",
    UI_ndnsg_33 ~ "White fish coated or fried",
    #  UI_ndnsg_34 ~ "Other white fish, shellfish and fish dishes",
    #  UI_ndnsg_35 ~ "Oily fish",
    UI_ndnsg_36 ~ "Salad and other raw vegetables",
    UI_ndnsg_37 ~ "Vegetables (not raw)",
    UI_ndnsg_38 ~ "Chips, fried and roast potatoes and potato products",
    UI_ndnsg_39 ~ "Other potatoes, potato salads and dishes",
    UI_ndnsg_40 ~ "Fruit",
    UI_ndnsg_41 ~ "Sugars, preserves and sweet spreads",
    UI_ndnsg_42 ~ "Crisps and savoury snacks",
    UI_ndnsg_43 ~ "Sugar confectionery",
    UI_ndnsg_44 ~ "Chocolate confectionery",
    UI_ndnsg_45 ~ "Fruit juice",
    #  UI_ndnsg_47 ~ "Spirits and liqueurs",
    #  UI_ndnsg_48 ~ "Wine",
    #  UI_ndnsg_49 ~ "Beer lager cider and perry",
    UI_ndnsg_50 ~ "Miscellaneous",
    #  UI_ndnsg_51 ~ "Tea, coffee and water",
    #  UI_ndnsg_52 ~ "Commercial toddlers foods and drinks",
    UI_ndnsg_53 ~ "Ice cream",
    #  UI_ndnsg_54 ~ "Dietary supplements",
    #  UI_ndnsg_55 ~ "Artificial sweeteners",
    #  UI_ndnsg_56 ~ "Nuts and seeds",
    UI_ndnsg_57 ~ "Soft drinks, not diet",
    UI_ndnsg_58 ~ "Soft drinks, diet",
    #  UI_ndnsg_59 ~ "Brown, granary and wheatgerm bread",
    #  UI_ndnsg_60 ~ "1% Milk",
    UI_ndnsg_62 ~ "Sandwiches"
    #  UI_ndnsg_63 ~ "Other milk and cream DF",                         
    #  UI_ndnsg_64 ~ "Cheese DF",
    #  UI_ndnsg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
    #  UI_ndnsg_66 ~ "Ice cream DF"
  ),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(UI_ndnsg_1:UI_ndnsg_3,UI_ndnsg_5:UI_ndnsg_11, UI_ndnsg_13:UI_ndnsg_17, UI_ndnsg_21, UI_ndnsg_23,UI_ndnsg_26, 
              UI_ndnsg_27, UI_ndnsg_30, UI_ndnsg_31, UI_ndnsg_33, UI_ndnsg_36:UI_ndnsg_45, UI_ndnsg_50, UI_ndnsg_53, UI_ndnsg_57, 
              UI_ndnsg_58, UI_ndnsg_62)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#age/sex
table.ndns_grams_age_sex <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    UI_ndnsg_1 ~ "Pasta, rice and other miscellaneous cereals",
    UI_ndnsg_2 ~ "White bread",
    UI_ndnsg_3 ~ "Wholemeal bread",
    # UI_ndnsg_4 ~ "Other breads", 
    UI_ndnsg_5 ~ "High fibre breakfast cereals",
    UI_ndnsg_6 ~ "Other breakfast cereals",
    UI_ndnsg_7 ~ "Biscuits",
    UI_ndnsg_8 ~ "Buns, cakes, pastries and fruit pies",
    UI_ndnsg_9 ~ "Puddings",
    UI_ndnsg_10 ~ "Whole milk",
    UI_ndnsg_11 ~ "Semi-skimmed milk",
    # UI_ndnsg_12 ~ "Skimmed milk",
    UI_ndnsg_13 ~ "Other milk and cream",
    UI_ndnsg_14 ~ "Cheese",
    UI_ndnsg_15 ~ "Yogurt, fromage frais and other dairy desserts",
    UI_ndnsg_16 ~ "Eggs and egg dishes",
    UI_ndnsg_17 ~ "Butter",
    # UI_ndnsg_18 ~ "Polyunsaturated margarine and oils",
    # UI_ndnsg_19 ~ "Low fat spread",
    # UI_ndnsg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
    UI_ndnsg_21 ~ "Reduced fat spread",
    # UI_ndnsg_22 ~ "Bacon and ham",
    UI_ndnsg_23 ~ "Beef, veal and dishes",
    #  UI_ndnsg_24 ~ "Lamb and dishes",
    #  UI_ndnsg_25 ~ "Pork and dishes",
    UI_ndnsg_26 ~ "Coated chicken and turkey manufactured",
    UI_ndnsg_27 ~ "Chicken and turkey dishes",
    #  UI_ndnsg_28 ~ "Liver, products and dishes",
    #  UI_ndnsg_29 ~ "Burgers and kebabs",
    UI_ndnsg_30 ~ "Sausages",
    UI_ndnsg_31 ~ "Meat pies and pastries",
    #  UI_ndnsg_32 ~ "Other meat and meat products",
    UI_ndnsg_33 ~ "White fish coated or fried",
    #  UI_ndnsg_34 ~ "Other white fish, shellfish and fish dishes",
    #  UI_ndnsg_35 ~ "Oily fish",
    UI_ndnsg_36 ~ "Salad and other raw vegetables",
    UI_ndnsg_37 ~ "Vegetables (not raw)",
    UI_ndnsg_38 ~ "Chips, fried and roast potatoes and potato products",
    UI_ndnsg_39 ~ "Other potatoes, potato salads and dishes",
    UI_ndnsg_40 ~ "Fruit",
    UI_ndnsg_41 ~ "Sugars, preserves and sweet spreads",
    UI_ndnsg_42 ~ "Crisps and savoury snacks",
    UI_ndnsg_43 ~ "Sugar confectionery",
    UI_ndnsg_44 ~ "Chocolate confectionery",
    UI_ndnsg_45 ~ "Fruit juice",
    #  UI_ndnsg_47 ~ "Spirits and liqueurs",
    #  UI_ndnsg_48 ~ "Wine",
    #  UI_ndnsg_49 ~ "Beer lager cider and perry",
    UI_ndnsg_50 ~ "Miscellaneous",
    #  UI_ndnsg_51 ~ "Tea, coffee and water",
    #  UI_ndnsg_52 ~ "Commercial toddlers foods and drinks",
    UI_ndnsg_53 ~ "Ice cream",
    #  UI_ndnsg_54 ~ "Dietary supplements",
    #  UI_ndnsg_55 ~ "Artificial sweeteners",
    #  UI_ndnsg_56 ~ "Nuts and seeds",
    UI_ndnsg_57 ~ "Soft drinks, not diet",
    UI_ndnsg_58 ~ "Soft drinks, diet",
    #  UI_ndnsg_59 ~ "Brown, granary and wheatgerm bread",
    #  UI_ndnsg_60 ~ "1% Milk",
    UI_ndnsg_62 ~ "Sandwiches"
    #  UI_ndnsg_63 ~ "Other milk and cream DF",                         
    #  UI_ndnsg_64 ~ "Cheese DF",
    #  UI_ndnsg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
    #  UI_ndnsg_66 ~ "Ice cream DF"
  ),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(UI_ndnsg_1:UI_ndnsg_3,UI_ndnsg_5:UI_ndnsg_11, UI_ndnsg_13:UI_ndnsg_17, UI_ndnsg_21, UI_ndnsg_23,UI_ndnsg_26, 
              UI_ndnsg_27, UI_ndnsg_30, UI_ndnsg_31, UI_ndnsg_33, UI_ndnsg_36:UI_ndnsg_45, UI_ndnsg_50, UI_ndnsg_53, UI_ndnsg_57, 
              UI_ndnsg_58, UI_ndnsg_62)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

table.ndns_grams_simd <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = simd_quintile,
  label = c(
    UI_ndnsg_1 ~ "Pasta, rice and other miscellaneous cereals",
    UI_ndnsg_2 ~ "White bread",
    UI_ndnsg_3 ~ "Wholemeal bread",
    # UI_ndnsg_4 ~ "Other breads", 
    UI_ndnsg_5 ~ "High fibre breakfast cereals",
    UI_ndnsg_6 ~ "Other breakfast cereals",
    UI_ndnsg_7 ~ "Biscuits",
    UI_ndnsg_8 ~ "Buns, cakes, pastries and fruit pies",
    UI_ndnsg_9 ~ "Puddings",
    UI_ndnsg_10 ~ "Whole milk",
    UI_ndnsg_11 ~ "Semi-skimmed milk",
    # UI_ndnsg_12 ~ "Skimmed milk",
    UI_ndnsg_13 ~ "Other milk and cream",
    UI_ndnsg_14 ~ "Cheese",
    UI_ndnsg_15 ~ "Yogurt, fromage frais and other dairy desserts",
    UI_ndnsg_16 ~ "Eggs and egg dishes",
    UI_ndnsg_17 ~ "Butter",
    # UI_ndnsg_18 ~ "Polyunsaturated margarine and oils",
    # UI_ndnsg_19 ~ "Low fat spread",
    # UI_ndnsg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
    UI_ndnsg_21 ~ "Reduced fat spread",
    # UI_ndnsg_22 ~ "Bacon and ham",
    UI_ndnsg_23 ~ "Beef, veal and dishes",
    #  UI_ndnsg_24 ~ "Lamb and dishes",
    #  UI_ndnsg_25 ~ "Pork and dishes",
    UI_ndnsg_26 ~ "Coated chicken and turkey manufactured",
    UI_ndnsg_27 ~ "Chicken and turkey dishes",
    #  UI_ndnsg_28 ~ "Liver, products and dishes",
    #  UI_ndnsg_29 ~ "Burgers and kebabs",
    UI_ndnsg_30 ~ "Sausages",
    UI_ndnsg_31 ~ "Meat pies and pastries",
    #  UI_ndnsg_32 ~ "Other meat and meat products",
    UI_ndnsg_33 ~ "White fish coated or fried",
    #  UI_ndnsg_34 ~ "Other white fish, shellfish and fish dishes",
    #  UI_ndnsg_35 ~ "Oily fish",
    UI_ndnsg_36 ~ "Salad and other raw vegetables",
    UI_ndnsg_37 ~ "Vegetables (not raw)",
    UI_ndnsg_38 ~ "Chips, fried and roast potatoes and potato products",
    UI_ndnsg_39 ~ "Other potatoes, potato salads and dishes",
    UI_ndnsg_40 ~ "Fruit",
    UI_ndnsg_41 ~ "Sugars, preserves and sweet spreads",
    UI_ndnsg_42 ~ "Crisps and savoury snacks",
    UI_ndnsg_43 ~ "Sugar confectionery",
    UI_ndnsg_44 ~ "Chocolate confectionery",
    UI_ndnsg_45 ~ "Fruit juice",
    #  UI_ndnsg_47 ~ "Spirits and liqueurs",
    #  UI_ndnsg_48 ~ "Wine",
    #  UI_ndnsg_49 ~ "Beer lager cider and perry",
    UI_ndnsg_50 ~ "Miscellaneous",
    #  UI_ndnsg_51 ~ "Tea, coffee and water",
    #  UI_ndnsg_52 ~ "Commercial toddlers foods and drinks",
    UI_ndnsg_53 ~ "Ice cream",
    #  UI_ndnsg_54 ~ "Dietary supplements",
    #  UI_ndnsg_55 ~ "Artificial sweeteners",
    #  UI_ndnsg_56 ~ "Nuts and seeds",
    UI_ndnsg_57 ~ "Soft drinks, not diet",
    UI_ndnsg_58 ~ "Soft drinks, diet",
    #  UI_ndnsg_59 ~ "Brown, granary and wheatgerm bread",
    #  UI_ndnsg_60 ~ "1% Milk",
    UI_ndnsg_62 ~ "Sandwiches"
    #  UI_ndnsg_63 ~ "Other milk and cream DF",                         
    #  UI_ndnsg_64 ~ "Cheese DF",
    #  UI_ndnsg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
    #  UI_ndnsg_66 ~ "Ice cream DF"
  ),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(UI_ndnsg_1:UI_ndnsg_3,UI_ndnsg_5:UI_ndnsg_11, UI_ndnsg_13:UI_ndnsg_17, UI_ndnsg_21, UI_ndnsg_23,UI_ndnsg_26, 
              UI_ndnsg_27, UI_ndnsg_30, UI_ndnsg_31, UI_ndnsg_33, UI_ndnsg_36:UI_ndnsg_45, UI_ndnsg_50, UI_ndnsg_53, UI_ndnsg_57, 
              UI_ndnsg_58, UI_ndnsg_62)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


###output Excel ####

#create workbook
wb5 <- wb_workbook()

#create lists for tabs
ndns_sheet_names <- c("Grams by Sex", "Grams by Age", "Grams by Age and Sex", "Grams by SIMD")
ndns_tables <- list(table.ndns_grams_sex, table.ndns_grams_age, table.ndns_grams_age_sex, table.ndns_grams_simd)

#assign flextables to tabs 
for (i in seq_along(ndns_sheet_names)) {
  wb5 <- wb_add_worksheet(wb5, sheet = ndns_sheet_names[i])
  wb5 <- wb_add_flextable(wb5, sheet = ndns_sheet_names[i], ndns_tables[[i]])
}

#save workbook
wb_save(wb5, file = paste0("Output/Table_NDNS_Grams_", format(Sys.time(), "%d%m%Y"), ".xlsx"))



##Energy ####

table.energy.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_Energykcal_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Energykcal_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Energykcal_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Energykcal_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Energykcal_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Energykcal_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Energykcal_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Energykcal_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Energykcal_foodcat_9 ~ "Fruit",
    Avg_Prop_Energykcal_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Energykcal_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Energykcal_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Energykcal_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Energykcal_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Energykcal_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Energykcal_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Energykcal_foodcat_1:Avg_Prop_Energykcal_foodcat_13,Avg_Prop_Energykcal_foodcat_15,Avg_Prop_Energykcal_foodcat_16,Avg_Prop_Energykcal_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#age/sex

table.energy.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Energykcal_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Energykcal_fg_2 ~ "White bread",
            Avg_Prop_Energykcal_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Energykcal_fg_4 ~ "Other breads",
            Avg_Prop_Energykcal_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Energykcal_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Energykcal_fg_7 ~ "Biscuits",
            Avg_Prop_Energykcal_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Energykcal_fg_9 ~ "Puddings",
            Avg_Prop_Energykcal_fg_10 ~ "Whole milk",
            Avg_Prop_Energykcal_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Energykcal_fg_12 ~ "Skimmed milk",
            Avg_Prop_Energykcal_fg_13 ~ "Other milk and cream",
            Avg_Prop_Energykcal_fg_14 ~ "Cheese",
            Avg_Prop_Energykcal_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Energykcal_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Energykcal_fg_17 ~ "Butter",
            # Avg_Prop_Energykcal_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Energykcal_fg_19 ~ "Low fat spread",
            Avg_Prop_Energykcal_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Energykcal_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Energykcal_fg_22 ~ "Bacon and ham",
            Avg_Prop_Energykcal_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Energykcal_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Energykcal_fg_25 ~ "Pork and dishes",
            Avg_Prop_Energykcal_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Energykcal_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Energykcal_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Energykcal_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Energykcal_fg_30 ~ "Sausages",
            Avg_Prop_Energykcal_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Energykcal_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Energykcal_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Energykcal_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Energykcal_fg_35 ~ "Oily fish",
            Avg_Prop_Energykcal_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Energykcal_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Energykcal_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Energykcal_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Energykcal_fg_40 ~ "Fruit",
            Avg_Prop_Energykcal_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Energykcal_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Energykcal_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Energykcal_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Energykcal_fg_45 ~ "Fruit juice",
            # Avg_Prop_Energykcal_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Energykcal_fg_48 ~ "Wine",
            # Avg_Prop_Energykcal_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Energykcal_fg_50 ~ "Miscellaneous",
            Avg_Prop_Energykcal_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Energykcal_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Energykcal_fg_53 ~ "Ice cream",
            #Avg_Prop_Energykcal_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Energykcal_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Energykcal_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Energykcal_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Energykcal_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Energykcal_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Energykcal_fg_60 ~ "1% Milk",
            Avg_Prop_Energykcal_fg_62 ~ "Sandwiches",
            Avg_Prop_Energykcal_fg_63 ~ "Other milk and cream DF",                         
            Avg_Prop_Energykcal_fg_64 ~ "Cheese DF",
            Avg_Prop_Energykcal_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Energykcal_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Energykcal_fg_1:Avg_Prop_Energykcal_fg_17, Avg_Prop_Energykcal_fg_19:Avg_Prop_Energykcal_fg_45, 
              Avg_Prop_Energykcal_fg_50:Avg_Prop_Energykcal_fg_53, Avg_Prop_Energykcal_fg_56:Avg_Prop_Energykcal_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD
table.energy.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Energykcal_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Energykcal_fg_2 ~ "White bread",
            Avg_Prop_Energykcal_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Energykcal_fg_4 ~ "Other breads",
            Avg_Prop_Energykcal_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Energykcal_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Energykcal_fg_7 ~ "Biscuits",
            Avg_Prop_Energykcal_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Energykcal_fg_9 ~ "Puddings",
            Avg_Prop_Energykcal_fg_10 ~ "Whole milk",
            Avg_Prop_Energykcal_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Energykcal_fg_12 ~ "Skimmed milk",
            Avg_Prop_Energykcal_fg_13 ~ "Other milk and cream",
            Avg_Prop_Energykcal_fg_14 ~ "Cheese",
            Avg_Prop_Energykcal_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Energykcal_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Energykcal_fg_17 ~ "Butter",
            # Avg_Prop_Energykcal_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Energykcal_fg_19 ~ "Low fat spread",
            Avg_Prop_Energykcal_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Energykcal_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Energykcal_fg_22 ~ "Bacon and ham",
            Avg_Prop_Energykcal_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Energykcal_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Energykcal_fg_25 ~ "Pork and dishes",
            Avg_Prop_Energykcal_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Energykcal_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Energykcal_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Energykcal_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Energykcal_fg_30 ~ "Sausages",
            Avg_Prop_Energykcal_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Energykcal_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Energykcal_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Energykcal_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Energykcal_fg_35 ~ "Oily fish",
            Avg_Prop_Energykcal_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Energykcal_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Energykcal_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Energykcal_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Energykcal_fg_40 ~ "Fruit",
            Avg_Prop_Energykcal_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Energykcal_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Energykcal_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Energykcal_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Energykcal_fg_45 ~ "Fruit juice",
            # Avg_Prop_Energykcal_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Energykcal_fg_48 ~ "Wine",
            # Avg_Prop_Energykcal_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Energykcal_fg_50 ~ "Miscellaneous",
            Avg_Prop_Energykcal_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Energykcal_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Energykcal_fg_53 ~ "Ice cream",
            #Avg_Prop_Energykcal_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Energykcal_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Energykcal_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Energykcal_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Energykcal_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Energykcal_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Energykcal_fg_60 ~ "1% Milk",
            Avg_Prop_Energykcal_fg_62 ~ "Sandwiches",
            Avg_Prop_Energykcal_fg_63 ~ "Other milk and cream DF",                         
            Avg_Prop_Energykcal_fg_64 ~ "Cheese DF",
            Avg_Prop_Energykcal_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Energykcal_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Energykcal_fg_1:Avg_Prop_Energykcal_fg_17, Avg_Prop_Energykcal_fg_19:Avg_Prop_Energykcal_fg_45, 
              Avg_Prop_Energykcal_fg_50:Avg_Prop_Energykcal_fg_53, Avg_Prop_Energykcal_fg_56:Avg_Prop_Energykcal_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


##Carbs ####

#age/sex

table.carbs.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Carbohydrateg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Carbohydrateg_fg_2 ~ "White bread",
            Avg_Prop_Carbohydrateg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Carbohydrateg_fg_4 ~ "Other breads",
            Avg_Prop_Carbohydrateg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Carbohydrateg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Carbohydrateg_fg_7 ~ "Biscuits",
            Avg_Prop_Carbohydrateg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Carbohydrateg_fg_9 ~ "Puddings",
            Avg_Prop_Carbohydrateg_fg_10 ~ "Whole milk",
            Avg_Prop_Carbohydrateg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Carbohydrateg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Carbohydrateg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Carbohydrateg_fg_14 ~ "Cheese",
            Avg_Prop_Carbohydrateg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Carbohydrateg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Carbohydrateg_fg_17 ~ "Butter",
            # Avg_Prop_Carbohydrateg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Carbohydrateg_fg_19 ~ "Low fat spread",
            # Avg_Prop_Carbohydrateg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Carbohydrateg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Carbohydrateg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Carbohydrateg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Carbohydrateg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Carbohydrateg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Carbohydrateg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Carbohydrateg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Carbohydrateg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Carbohydrateg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Carbohydrateg_fg_30 ~ "Sausages",
            Avg_Prop_Carbohydrateg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Carbohydrateg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Carbohydrateg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Carbohydrateg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Carbohydrateg_fg_35 ~ "Oily fish",
            Avg_Prop_Carbohydrateg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Carbohydrateg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Carbohydrateg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Carbohydrateg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Carbohydrateg_fg_40 ~ "Fruit",
            Avg_Prop_Carbohydrateg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Carbohydrateg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Carbohydrateg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Carbohydrateg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Carbohydrateg_fg_45 ~ "Fruit juice",
            # Avg_Prop_Carbohydrateg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Carbohydrateg_fg_48 ~ "Wine",
            # Avg_Prop_Carbohydrateg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Carbohydrateg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Carbohydrateg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Carbohydrateg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Carbohydrateg_fg_53 ~ "Ice cream",
            # Avg_Prop_Carbohydrateg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Carbohydrateg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Carbohydrateg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Carbohydrateg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Carbohydrateg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Carbohydrateg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Carbohydrateg_fg_60 ~ "1% Milk",
            Avg_Prop_Carbohydrateg_fg_62 ~ "Sandwiches",
            Avg_Prop_Carbohydrateg_fg_63 ~ "Other milk and cream DF",                         
            Avg_Prop_Carbohydrateg_fg_64 ~ "Cheese DF",
            Avg_Prop_Carbohydrateg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Carbohydrateg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Carbohydrateg_fg_1:Avg_Prop_Carbohydrateg_fg_17, Avg_Prop_Carbohydrateg_fg_19, Avg_Prop_Carbohydrateg_fg_21:Avg_Prop_Carbohydrateg_fg_45, 
              Avg_Prop_Carbohydrateg_fg_50:Avg_Prop_Carbohydrateg_fg_53, Avg_Prop_Carbohydrateg_fg_56:Avg_Prop_Carbohydrateg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.carbs.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Carbohydrateg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Carbohydrateg_fg_2 ~ "White bread",
            Avg_Prop_Carbohydrateg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Carbohydrateg_fg_4 ~ "Other breads",
            Avg_Prop_Carbohydrateg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Carbohydrateg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Carbohydrateg_fg_7 ~ "Biscuits",
            Avg_Prop_Carbohydrateg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Carbohydrateg_fg_9 ~ "Puddings",
            Avg_Prop_Carbohydrateg_fg_10 ~ "Whole milk",
            Avg_Prop_Carbohydrateg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Carbohydrateg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Carbohydrateg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Carbohydrateg_fg_14 ~ "Cheese",
            Avg_Prop_Carbohydrateg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Carbohydrateg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Carbohydrateg_fg_17 ~ "Butter",
            # Avg_Prop_Carbohydrateg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Carbohydrateg_fg_19 ~ "Low fat spread",
            # Avg_Prop_Carbohydrateg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Carbohydrateg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Carbohydrateg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Carbohydrateg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Carbohydrateg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Carbohydrateg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Carbohydrateg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Carbohydrateg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Carbohydrateg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Carbohydrateg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Carbohydrateg_fg_30 ~ "Sausages",
            Avg_Prop_Carbohydrateg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Carbohydrateg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Carbohydrateg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Carbohydrateg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Carbohydrateg_fg_35 ~ "Oily fish",
            Avg_Prop_Carbohydrateg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Carbohydrateg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Carbohydrateg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Carbohydrateg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Carbohydrateg_fg_40 ~ "Fruit",
            Avg_Prop_Carbohydrateg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Carbohydrateg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Carbohydrateg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Carbohydrateg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Carbohydrateg_fg_45 ~ "Fruit juice",
            # Avg_Prop_Carbohydrateg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Carbohydrateg_fg_48 ~ "Wine",
            # Avg_Prop_Carbohydrateg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Carbohydrateg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Carbohydrateg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Carbohydrateg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Carbohydrateg_fg_53 ~ "Ice cream",
            # Avg_Prop_Carbohydrateg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Carbohydrateg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Carbohydrateg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Carbohydrateg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Carbohydrateg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Carbohydrateg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Carbohydrateg_fg_60 ~ "1% Milk",
            Avg_Prop_Carbohydrateg_fg_62 ~ "Sandwiches",
            Avg_Prop_Carbohydrateg_fg_63 ~ "Other milk and cream DF",                         
            Avg_Prop_Carbohydrateg_fg_64 ~ "Cheese DF",
            Avg_Prop_Carbohydrateg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Carbohydrateg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Carbohydrateg_fg_1:Avg_Prop_Carbohydrateg_fg_17, Avg_Prop_Carbohydrateg_fg_19, Avg_Prop_Carbohydrateg_fg_21:Avg_Prop_Carbohydrateg_fg_45, 
              Avg_Prop_Carbohydrateg_fg_50:Avg_Prop_Carbohydrateg_fg_53, Avg_Prop_Carbohydrateg_fg_56:Avg_Prop_Carbohydrateg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Free Sugars ####

table.freesugars.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_FreeSugarsg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_FreeSugarsg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_FreeSugarsg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_FreeSugarsg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_FreeSugarsg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_FreeSugarsg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_FreeSugarsg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_FreeSugarsg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_FreeSugarsg_foodcat_9 ~ "Fruit",
    Avg_Prop_FreeSugarsg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_FreeSugarsg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_FreeSugarsg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_FreeSugarsg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_FreeSugarsg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_FreeSugarsg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_FreeSugarsg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_FreeSugarsg_foodcat_1:Avg_Prop_FreeSugarsg_foodcat_13,Avg_Prop_FreeSugarsg_foodcat_15,Avg_Prop_FreeSugarsg_foodcat_16,Avg_Prop_FreeSugarsg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#age/sex

table.FreeSugars.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_FreeSugarsg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_FreeSugarsg_fg_2 ~ "White bread",
            Avg_Prop_FreeSugarsg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_FreeSugarsg_fg_4 ~ "Other breads",
            Avg_Prop_FreeSugarsg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_FreeSugarsg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_FreeSugarsg_fg_7 ~ "Biscuits",
            Avg_Prop_FreeSugarsg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_FreeSugarsg_fg_9 ~ "Puddings",
            # Avg_Prop_FreeSugarsg_fg_10 ~ "Whole milk",
            # Avg_Prop_FreeSugarsg_fg_11 ~ "Semi-skimmed milk",
            # Avg_Prop_FreeSugarsg_fg_12 ~ "Skimmed milk",
            Avg_Prop_FreeSugarsg_fg_13 ~ "Other milk and cream",
            Avg_Prop_FreeSugarsg_fg_14 ~ "Cheese",
            Avg_Prop_FreeSugarsg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_FreeSugarsg_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_FreeSugarsg_fg_17 ~ "Butter",
            # Avg_Prop_FreeSugarsg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_FreeSugarsg_fg_19 ~ "Low fat spread",
            # Avg_Prop_FreeSugarsg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            # Avg_Prop_FreeSugarsg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_FreeSugarsg_fg_22 ~ "Bacon and ham",
            Avg_Prop_FreeSugarsg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_FreeSugarsg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_FreeSugarsg_fg_25 ~ "Pork and dishes",
            Avg_Prop_FreeSugarsg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_FreeSugarsg_fg_27 ~ "Chicken and turkey dishes",
            # Avg_Prop_FreeSugarsg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_FreeSugarsg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_FreeSugarsg_fg_30 ~ "Sausages",
            Avg_Prop_FreeSugarsg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_FreeSugarsg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_FreeSugarsg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_FreeSugarsg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_FreeSugarsg_fg_35 ~ "Oily fish",
            Avg_Prop_FreeSugarsg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_FreeSugarsg_fg_37 ~ "Vegetables (not raw)",
            # Avg_Prop_FreeSugarsg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            # Avg_Prop_FreeSugarsg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_FreeSugarsg_fg_40 ~ "Fruit",
            Avg_Prop_FreeSugarsg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_FreeSugarsg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_FreeSugarsg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_FreeSugarsg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_FreeSugarsg_fg_45 ~ "Fruit juice",
            # Avg_Prop_FreeSugarsg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_FreeSugarsg_fg_48 ~ "Wine",
            # Avg_Prop_FreeSugarsg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_FreeSugarsg_fg_50 ~ "Miscellaneous",
            # Avg_Prop_FreeSugarsg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_FreeSugarsg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_FreeSugarsg_fg_53 ~ "Ice cream",
            #  Avg_Prop_FreeSugarsg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_FreeSugarsg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_FreeSugarsg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_FreeSugarsg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_FreeSugarsg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_FreeSugarsg_fg_59 ~ "Brown, granary and wheatgerm bread",
            #  Avg_Prop_FreeSugarsg_fg_60 ~ "1% Milk",
            Avg_Prop_FreeSugarsg_fg_62 ~ "Sandwiches",
            Avg_Prop_FreeSugarsg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_FreeSugarsg_fg_64 ~ "Cheese DF",
            Avg_Prop_FreeSugarsg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_FreeSugarsg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_FreeSugarsg_fg_1:Avg_Prop_FreeSugarsg_fg_9,Avg_Prop_FreeSugarsg_fg_13:Avg_Prop_FreeSugarsg_fg_16,
              Avg_Prop_FreeSugarsg_fg_22:Avg_Prop_FreeSugarsg_fg_27, Avg_Prop_FreeSugarsg_fg_29:Avg_Prop_FreeSugarsg_fg_37,
              Avg_Prop_FreeSugarsg_fg_40:Avg_Prop_FreeSugarsg_fg_45, Avg_Prop_FreeSugarsg_fg_50, Avg_Prop_FreeSugarsg_fg_52, 
              Avg_Prop_FreeSugarsg_fg_53, Avg_Prop_FreeSugarsg_fg_56:Avg_Prop_FreeSugarsg_fg_59,Avg_Prop_FreeSugarsg_fg_62,
              Avg_Prop_FreeSugarsg_fg_63, Avg_Prop_FreeSugarsg_fg_65, Avg_Prop_FreeSugarsg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.FreeSugars.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_FreeSugarsg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_FreeSugarsg_fg_2 ~ "White bread",
            Avg_Prop_FreeSugarsg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_FreeSugarsg_fg_4 ~ "Other breads",
            Avg_Prop_FreeSugarsg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_FreeSugarsg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_FreeSugarsg_fg_7 ~ "Biscuits",
            Avg_Prop_FreeSugarsg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_FreeSugarsg_fg_9 ~ "Puddings",
            # Avg_Prop_FreeSugarsg_fg_10 ~ "Whole milk",
            # Avg_Prop_FreeSugarsg_fg_11 ~ "Semi-skimmed milk",
            # Avg_Prop_FreeSugarsg_fg_12 ~ "Skimmed milk",
            Avg_Prop_FreeSugarsg_fg_13 ~ "Other milk and cream",
            Avg_Prop_FreeSugarsg_fg_14 ~ "Cheese",
            Avg_Prop_FreeSugarsg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_FreeSugarsg_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_FreeSugarsg_fg_17 ~ "Butter",
            # Avg_Prop_FreeSugarsg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_FreeSugarsg_fg_19 ~ "Low fat spread",
            # Avg_Prop_FreeSugarsg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            # Avg_Prop_FreeSugarsg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_FreeSugarsg_fg_22 ~ "Bacon and ham",
            Avg_Prop_FreeSugarsg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_FreeSugarsg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_FreeSugarsg_fg_25 ~ "Pork and dishes",
            Avg_Prop_FreeSugarsg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_FreeSugarsg_fg_27 ~ "Chicken and turkey dishes",
            # Avg_Prop_FreeSugarsg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_FreeSugarsg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_FreeSugarsg_fg_30 ~ "Sausages",
            Avg_Prop_FreeSugarsg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_FreeSugarsg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_FreeSugarsg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_FreeSugarsg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_FreeSugarsg_fg_35 ~ "Oily fish",
            Avg_Prop_FreeSugarsg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_FreeSugarsg_fg_37 ~ "Vegetables (not raw)",
            # Avg_Prop_FreeSugarsg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            # Avg_Prop_FreeSugarsg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_FreeSugarsg_fg_40 ~ "Fruit",
            Avg_Prop_FreeSugarsg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_FreeSugarsg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_FreeSugarsg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_FreeSugarsg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_FreeSugarsg_fg_45 ~ "Fruit juice",
            # Avg_Prop_FreeSugarsg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_FreeSugarsg_fg_48 ~ "Wine",
            # Avg_Prop_FreeSugarsg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_FreeSugarsg_fg_50 ~ "Miscellaneous",
            # Avg_Prop_FreeSugarsg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_FreeSugarsg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_FreeSugarsg_fg_53 ~ "Ice cream",
            #  Avg_Prop_FreeSugarsg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_FreeSugarsg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_FreeSugarsg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_FreeSugarsg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_FreeSugarsg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_FreeSugarsg_fg_59 ~ "Brown, granary and wheatgerm bread",
            #  Avg_Prop_FreeSugarsg_fg_60 ~ "1% Milk",
            Avg_Prop_FreeSugarsg_fg_62 ~ "Sandwiches",
            Avg_Prop_FreeSugarsg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_FreeSugarsg_fg_64 ~ "Cheese DF",
            Avg_Prop_FreeSugarsg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_FreeSugarsg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_FreeSugarsg_fg_1:Avg_Prop_FreeSugarsg_fg_9,Avg_Prop_FreeSugarsg_fg_13:Avg_Prop_FreeSugarsg_fg_16,
              Avg_Prop_FreeSugarsg_fg_22:Avg_Prop_FreeSugarsg_fg_27, Avg_Prop_FreeSugarsg_fg_29:Avg_Prop_FreeSugarsg_fg_37,
              Avg_Prop_FreeSugarsg_fg_40:Avg_Prop_FreeSugarsg_fg_45, Avg_Prop_FreeSugarsg_fg_50, Avg_Prop_FreeSugarsg_fg_52, 
              Avg_Prop_FreeSugarsg_fg_53, Avg_Prop_FreeSugarsg_fg_56:Avg_Prop_FreeSugarsg_fg_59,Avg_Prop_FreeSugarsg_fg_62,
              Avg_Prop_FreeSugarsg_fg_63, Avg_Prop_FreeSugarsg_fg_65, Avg_Prop_FreeSugarsg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

##Fat ####

#age/sex

table.fat.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Fatg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Fatg_fg_2 ~ "White bread",
            Avg_Prop_Fatg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Fatg_fg_4 ~ "Other breads",
            Avg_Prop_Fatg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Fatg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Fatg_fg_7 ~ "Biscuits",
            Avg_Prop_Fatg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Fatg_fg_9 ~ "Puddings",
            Avg_Prop_Fatg_fg_10 ~ "Whole milk",
            Avg_Prop_Fatg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Fatg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Fatg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Fatg_fg_14 ~ "Cheese",
            Avg_Prop_Fatg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Fatg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Fatg_fg_17 ~ "Butter",
            #  Avg_Prop_Fatg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Fatg_fg_19 ~ "Low fat spread",
            Avg_Prop_Fatg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Fatg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Fatg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Fatg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Fatg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Fatg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Fatg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Fatg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Fatg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Fatg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Fatg_fg_30 ~ "Sausages",
            Avg_Prop_Fatg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Fatg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Fatg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Fatg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Fatg_fg_35 ~ "Oily fish",
            Avg_Prop_Fatg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Fatg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Fatg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Fatg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Fatg_fg_40 ~ "Fruit",
            Avg_Prop_Fatg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Fatg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Fatg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Fatg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Fatg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Fatg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Fatg_fg_48 ~ "Wine",
            # Avg_Prop_Fatg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Fatg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Fatg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Fatg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Fatg_fg_53 ~ "Ice cream",
            # Avg_Prop_Fatg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Fatg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Fatg_fg_56 ~ "Nuts and seeds",
            # Avg_Prop_Fatg_fg_57 ~ "Soft drinks, not diet",
            #Avg_Prop_Fatg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Fatg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Fatg_fg_60 ~ "1% Milk",
            Avg_Prop_Fatg_fg_62 ~ "Sandwiches",
            Avg_Prop_Fatg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Fatg_fg_64 ~ "Cheese DF",
            Avg_Prop_Fatg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Fatg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Fatg_fg_1:Avg_Prop_Fatg_fg_17, Avg_Prop_Fatg_fg_19:Avg_Prop_Fatg_fg_45, Avg_Prop_Fatg_fg_50:Avg_Prop_Fatg_fg_53,
              Avg_Prop_Fatg_fg_56,Avg_Prop_Fatg_fg_59:Avg_Prop_Fatg_fg_63, Avg_Prop_Fatg_fg_65, Avg_Prop_Fatg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#SIMD

table.fat.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Fatg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Fatg_fg_2 ~ "White bread",
            Avg_Prop_Fatg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Fatg_fg_4 ~ "Other breads",
            Avg_Prop_Fatg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Fatg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Fatg_fg_7 ~ "Biscuits",
            Avg_Prop_Fatg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Fatg_fg_9 ~ "Puddings",
            Avg_Prop_Fatg_fg_10 ~ "Whole milk",
            Avg_Prop_Fatg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Fatg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Fatg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Fatg_fg_14 ~ "Cheese",
            Avg_Prop_Fatg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Fatg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Fatg_fg_17 ~ "Butter",
            #  Avg_Prop_Fatg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Fatg_fg_19 ~ "Low fat spread",
            Avg_Prop_Fatg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Fatg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Fatg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Fatg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Fatg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Fatg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Fatg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Fatg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Fatg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Fatg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Fatg_fg_30 ~ "Sausages",
            Avg_Prop_Fatg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Fatg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Fatg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Fatg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Fatg_fg_35 ~ "Oily fish",
            Avg_Prop_Fatg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Fatg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Fatg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Fatg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Fatg_fg_40 ~ "Fruit",
            Avg_Prop_Fatg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Fatg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Fatg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Fatg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Fatg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Fatg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Fatg_fg_48 ~ "Wine",
            # Avg_Prop_Fatg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Fatg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Fatg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Fatg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Fatg_fg_53 ~ "Ice cream",
            # Avg_Prop_Fatg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Fatg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Fatg_fg_56 ~ "Nuts and seeds",
            # Avg_Prop_Fatg_fg_57 ~ "Soft drinks, not diet",
            #Avg_Prop_Fatg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Fatg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Fatg_fg_60 ~ "1% Milk",
            Avg_Prop_Fatg_fg_62 ~ "Sandwiches",
            Avg_Prop_Fatg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Fatg_fg_64 ~ "Cheese DF",
            Avg_Prop_Fatg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Fatg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Fatg_fg_1:Avg_Prop_Fatg_fg_17, Avg_Prop_Fatg_fg_19:Avg_Prop_Fatg_fg_45, Avg_Prop_Fatg_fg_50:Avg_Prop_Fatg_fg_53,
              Avg_Prop_Fatg_fg_56,Avg_Prop_Fatg_fg_59:Avg_Prop_Fatg_fg_63, Avg_Prop_Fatg_fg_65, Avg_Prop_Fatg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


##Sat Fat ####

table.satfat.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_SatFatg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_SatFatg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_SatFatg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_SatFatg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_SatFatg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_SatFatg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_SatFatg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_SatFatg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_SatFatg_foodcat_9 ~ "Fruit",
    Avg_Prop_SatFatg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_SatFatg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_SatFatg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_SatFatg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_SatFatg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_SatFatg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_SatFatg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_SatFatg_foodcat_1:Avg_Prop_SatFatg_foodcat_13,Avg_Prop_SatFatg_foodcat_15,Avg_Prop_SatFatg_foodcat_16,Avg_Prop_SatFatg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#age/sex

table.SatFat.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_SatFatg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_SatFatg_fg_2 ~ "White bread",
            Avg_Prop_SatFatg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_SatFatg_fg_4 ~ "Other breads",
            Avg_Prop_SatFatg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_SatFatg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_SatFatg_fg_7 ~ "Biscuits",
            Avg_Prop_SatFatg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_SatFatg_fg_9 ~ "Puddings",
            Avg_Prop_SatFatg_fg_10 ~ "Whole milk",
            Avg_Prop_SatFatg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_SatFatg_fg_12 ~ "Skimmed milk",
            Avg_Prop_SatFatg_fg_13 ~ "Other milk and cream",
            Avg_Prop_SatFatg_fg_14 ~ "Cheese",
            Avg_Prop_SatFatg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_SatFatg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_SatFatg_fg_17 ~ "Butter",
            # Avg_Prop_SatFatg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_SatFatg_fg_19 ~ "Low fat spread",
            Avg_Prop_SatFatg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_SatFatg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_SatFatg_fg_22 ~ "Bacon and ham",
            Avg_Prop_SatFatg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_SatFatg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_SatFatg_fg_25 ~ "Pork and dishes",
            Avg_Prop_SatFatg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_SatFatg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_SatFatg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_SatFatg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_SatFatg_fg_30 ~ "Sausages",
            Avg_Prop_SatFatg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_SatFatg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_SatFatg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_SatFatg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_SatFatg_fg_35 ~ "Oily fish",
            Avg_Prop_SatFatg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_SatFatg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_SatFatg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_SatFatg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_SatFatg_fg_40 ~ "Fruit",
            Avg_Prop_SatFatg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_SatFatg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_SatFatg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_SatFatg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_SatFatg_fg_45 ~ "Fruit juice",
            #   Avg_Prop_SatFatg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_SatFatg_fg_48 ~ "Wine",
            #  Avg_Prop_SatFatg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_SatFatg_fg_50 ~ "Miscellaneous",
            Avg_Prop_SatFatg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_SatFatg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_SatFatg_fg_53 ~ "Ice cream",
            #  Avg_Prop_SatFatg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_SatFatg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_SatFatg_fg_56 ~ "Nuts and seeds",
            # Avg_Prop_SatFatg_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_SatFatg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_SatFatg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_SatFatg_fg_60 ~ "1% Milk",
            Avg_Prop_SatFatg_fg_62 ~ "Sandwiches",
            Avg_Prop_SatFatg_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_SatFatg_fg_64 ~ "Cheese DF",
            Avg_Prop_SatFatg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_SatFatg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_SatFatg_fg_1:Avg_Prop_SatFatg_fg_17, Avg_Prop_SatFatg_fg_19:Avg_Prop_SatFatg_fg_45, Avg_Prop_SatFatg_fg_50:Avg_Prop_SatFatg_fg_53,
              Avg_Prop_SatFatg_fg_56,Avg_Prop_SatFatg_fg_59:Avg_Prop_SatFatg_fg_63, Avg_Prop_SatFatg_fg_65, Avg_Prop_SatFatg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.SatFat.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_SatFatg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_SatFatg_fg_2 ~ "White bread",
            Avg_Prop_SatFatg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_SatFatg_fg_4 ~ "Other breads",
            Avg_Prop_SatFatg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_SatFatg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_SatFatg_fg_7 ~ "Biscuits",
            Avg_Prop_SatFatg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_SatFatg_fg_9 ~ "Puddings",
            Avg_Prop_SatFatg_fg_10 ~ "Whole milk",
            Avg_Prop_SatFatg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_SatFatg_fg_12 ~ "Skimmed milk",
            Avg_Prop_SatFatg_fg_13 ~ "Other milk and cream",
            Avg_Prop_SatFatg_fg_14 ~ "Cheese",
            Avg_Prop_SatFatg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_SatFatg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_SatFatg_fg_17 ~ "Butter",
            # Avg_Prop_SatFatg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_SatFatg_fg_19 ~ "Low fat spread",
            Avg_Prop_SatFatg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_SatFatg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_SatFatg_fg_22 ~ "Bacon and ham",
            Avg_Prop_SatFatg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_SatFatg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_SatFatg_fg_25 ~ "Pork and dishes",
            Avg_Prop_SatFatg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_SatFatg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_SatFatg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_SatFatg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_SatFatg_fg_30 ~ "Sausages",
            Avg_Prop_SatFatg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_SatFatg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_SatFatg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_SatFatg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_SatFatg_fg_35 ~ "Oily fish",
            Avg_Prop_SatFatg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_SatFatg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_SatFatg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_SatFatg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_SatFatg_fg_40 ~ "Fruit",
            Avg_Prop_SatFatg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_SatFatg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_SatFatg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_SatFatg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_SatFatg_fg_45 ~ "Fruit juice",
            #   Avg_Prop_SatFatg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_SatFatg_fg_48 ~ "Wine",
            #  Avg_Prop_SatFatg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_SatFatg_fg_50 ~ "Miscellaneous",
            Avg_Prop_SatFatg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_SatFatg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_SatFatg_fg_53 ~ "Ice cream",
            #  Avg_Prop_SatFatg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_SatFatg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_SatFatg_fg_56 ~ "Nuts and seeds",
            # Avg_Prop_SatFatg_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_SatFatg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_SatFatg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_SatFatg_fg_60 ~ "1% Milk",
            Avg_Prop_SatFatg_fg_62 ~ "Sandwiches",
            Avg_Prop_SatFatg_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_SatFatg_fg_64 ~ "Cheese DF",
            Avg_Prop_SatFatg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_SatFatg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_SatFatg_fg_1:Avg_Prop_SatFatg_fg_17, Avg_Prop_SatFatg_fg_19:Avg_Prop_SatFatg_fg_45, Avg_Prop_SatFatg_fg_50:Avg_Prop_SatFatg_fg_53,
              Avg_Prop_SatFatg_fg_56,Avg_Prop_SatFatg_fg_59:Avg_Prop_SatFatg_fg_63, Avg_Prop_SatFatg_fg_65, Avg_Prop_SatFatg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


##Trans Fat ####

#age/sex

table.TransFat.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_TransFatg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_TransFatg_fg_2 ~ "White bread",
            Avg_Prop_TransFatg_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_TransFatg_fg_4 ~ "Other breads",
            Avg_Prop_TransFatg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_TransFatg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_TransFatg_fg_7 ~ "Biscuits",
            Avg_Prop_TransFatg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_TransFatg_fg_9 ~ "Puddings",
            Avg_Prop_TransFatg_fg_10 ~ "Whole milk",
            Avg_Prop_TransFatg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_TransFatg_fg_12 ~ "Skimmed milk",
            Avg_Prop_TransFatg_fg_13 ~ "Other milk and cream",
            Avg_Prop_TransFatg_fg_14 ~ "Cheese",
            Avg_Prop_TransFatg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_TransFatg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_TransFatg_fg_17 ~ "Butter",
            # Avg_Prop_TransFatg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_TransFatg_fg_19 ~ "Low fat spread",
            # Avg_Prop_TransFatg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_TransFatg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_TransFatg_fg_22 ~ "Bacon and ham",
            Avg_Prop_TransFatg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_TransFatg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_TransFatg_fg_25 ~ "Pork and dishes",
            Avg_Prop_TransFatg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_TransFatg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_TransFatg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_TransFatg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_TransFatg_fg_30 ~ "Sausages",
            Avg_Prop_TransFatg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_TransFatg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_TransFatg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_TransFatg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_TransFatg_fg_35 ~ "Oily fish",
            Avg_Prop_TransFatg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_TransFatg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_TransFatg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_TransFatg_fg_39 ~ "Other potatoes, potato salads and dishes",
            # Avg_Prop_TransFatg_fg_40 ~ "Fruit",
            Avg_Prop_TransFatg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_TransFatg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_TransFatg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_TransFatg_fg_44 ~ "Chocolate confectionery",
            # Avg_Prop_TransFatg_fg_45 ~ "Fruit juice",
            # Avg_Prop_TransFatg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_TransFatg_fg_48 ~ "Wine",
            # Avg_Prop_TransFatg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_TransFatg_fg_50 ~ "Miscellaneous",
            Avg_Prop_TransFatg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_TransFatg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_TransFatg_fg_53 ~ "Ice cream",
            # Avg_Prop_TransFatg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_TransFatg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_TransFatg_fg_56 ~ "Nuts and seeds",
            # Avg_Prop_TransFatg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_TransFatg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_TransFatg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_TransFatg_fg_60 ~ "1% Milk",
            Avg_Prop_TransFatg_fg_62 ~ "Sandwiches",
            #  Avg_Prop_TransFatg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_TransFatg_fg_64 ~ "Cheese DF",
            Avg_Prop_TransFatg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_TransFatg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_TransFatg_fg_1:Avg_Prop_TransFatg_fg_3, Avg_Prop_TransFatg_fg_5:Avg_Prop_TransFatg_fg_17,
              Avg_Prop_TransFatg_fg_19, Avg_Prop_TransFatg_fg_21:Avg_Prop_TransFatg_fg_39, Avg_Prop_TransFatg_fg_41:Avg_Prop_TransFatg_fg_44,
              Avg_Prop_TransFatg_fg_50:Avg_Prop_TransFatg_fg_53, Avg_Prop_TransFatg_fg_56,
              Avg_Prop_TransFatg_fg_58:Avg_Prop_TransFatg_fg_62, Avg_Prop_TransFatg_fg_65, Avg_Prop_TransFatg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.TransFat.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_TransFatg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_TransFatg_fg_2 ~ "White bread",
            Avg_Prop_TransFatg_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_TransFatg_fg_4 ~ "Other breads",
            Avg_Prop_TransFatg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_TransFatg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_TransFatg_fg_7 ~ "Biscuits",
            Avg_Prop_TransFatg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_TransFatg_fg_9 ~ "Puddings",
            Avg_Prop_TransFatg_fg_10 ~ "Whole milk",
            Avg_Prop_TransFatg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_TransFatg_fg_12 ~ "Skimmed milk",
            Avg_Prop_TransFatg_fg_13 ~ "Other milk and cream",
            Avg_Prop_TransFatg_fg_14 ~ "Cheese",
            Avg_Prop_TransFatg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_TransFatg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_TransFatg_fg_17 ~ "Butter",
            # Avg_Prop_TransFatg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_TransFatg_fg_19 ~ "Low fat spread",
            # Avg_Prop_TransFatg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_TransFatg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_TransFatg_fg_22 ~ "Bacon and ham",
            Avg_Prop_TransFatg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_TransFatg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_TransFatg_fg_25 ~ "Pork and dishes",
            Avg_Prop_TransFatg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_TransFatg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_TransFatg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_TransFatg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_TransFatg_fg_30 ~ "Sausages",
            Avg_Prop_TransFatg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_TransFatg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_TransFatg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_TransFatg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_TransFatg_fg_35 ~ "Oily fish",
            Avg_Prop_TransFatg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_TransFatg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_TransFatg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_TransFatg_fg_39 ~ "Other potatoes, potato salads and dishes",
            # Avg_Prop_TransFatg_fg_40 ~ "Fruit",
            Avg_Prop_TransFatg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_TransFatg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_TransFatg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_TransFatg_fg_44 ~ "Chocolate confectionery",
            # Avg_Prop_TransFatg_fg_45 ~ "Fruit juice",
            # Avg_Prop_TransFatg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_TransFatg_fg_48 ~ "Wine",
            # Avg_Prop_TransFatg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_TransFatg_fg_50 ~ "Miscellaneous",
            Avg_Prop_TransFatg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_TransFatg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_TransFatg_fg_53 ~ "Ice cream",
            # Avg_Prop_TransFatg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_TransFatg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_TransFatg_fg_56 ~ "Nuts and seeds",
            # Avg_Prop_TransFatg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_TransFatg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_TransFatg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_TransFatg_fg_60 ~ "1% Milk",
            Avg_Prop_TransFatg_fg_62 ~ "Sandwiches",
            #  Avg_Prop_TransFatg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_TransFatg_fg_64 ~ "Cheese DF",
            Avg_Prop_TransFatg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_TransFatg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_TransFatg_fg_1:Avg_Prop_TransFatg_fg_3, Avg_Prop_TransFatg_fg_5:Avg_Prop_TransFatg_fg_17,
              Avg_Prop_TransFatg_fg_19, Avg_Prop_TransFatg_fg_21:Avg_Prop_TransFatg_fg_39, Avg_Prop_TransFatg_fg_41:Avg_Prop_TransFatg_fg_44,
              Avg_Prop_TransFatg_fg_50:Avg_Prop_TransFatg_fg_53, Avg_Prop_TransFatg_fg_56,
              Avg_Prop_TransFatg_fg_58:Avg_Prop_TransFatg_fg_62, Avg_Prop_TransFatg_fg_65, Avg_Prop_TransFatg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Protein ####

#age/sex

table.protein.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Proteing_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Proteing_fg_2 ~ "White bread",
            Avg_Prop_Proteing_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Proteing_fg_4 ~ "Other breads",
            Avg_Prop_Proteing_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Proteing_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Proteing_fg_7 ~ "Biscuits",
            Avg_Prop_Proteing_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Proteing_fg_9 ~ "Puddings",
            Avg_Prop_Proteing_fg_10 ~ "Whole milk",
            Avg_Prop_Proteing_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Proteing_fg_12 ~ "Skimmed milk",
            Avg_Prop_Proteing_fg_13 ~ "Other milk and cream",
            Avg_Prop_Proteing_fg_14 ~ "Cheese",
            Avg_Prop_Proteing_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Proteing_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Proteing_fg_17 ~ "Butter",
            # Avg_Prop_Proteing_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Proteing_fg_19 ~ "Low fat spread",
            #  Avg_Prop_Proteing_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Proteing_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Proteing_fg_22 ~ "Bacon and ham",
            Avg_Prop_Proteing_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Proteing_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Proteing_fg_25 ~ "Pork and dishes",
            Avg_Prop_Proteing_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Proteing_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Proteing_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Proteing_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Proteing_fg_30 ~ "Sausages",
            Avg_Prop_Proteing_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Proteing_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Proteing_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Proteing_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Proteing_fg_35 ~ "Oily fish",
            Avg_Prop_Proteing_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Proteing_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Proteing_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Proteing_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Proteing_fg_40 ~ "Fruit",
            Avg_Prop_Proteing_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Proteing_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Proteing_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Proteing_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Proteing_fg_45 ~ "Fruit juice",
            # Avg_Prop_Proteing_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Proteing_fg_48 ~ "Wine",
            # Avg_Prop_Proteing_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Proteing_fg_50 ~ "Miscellaneous",
            Avg_Prop_Proteing_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Proteing_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Proteing_fg_53 ~ "Ice cream",
            # Avg_Prop_Proteing_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Proteing_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Proteing_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Proteing_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Proteing_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Proteing_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Proteing_fg_60 ~ "1% Milk",
            Avg_Prop_Proteing_fg_62 ~ "Sandwiches",
            Avg_Prop_Proteing_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Proteing_fg_64 ~ "Cheese DF",
            Avg_Prop_Proteing_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Proteing_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Proteing_fg_1:Avg_Prop_Proteing_fg_17, Avg_Prop_Proteing_fg_19,
              Avg_Prop_Proteing_fg_21:Avg_Prop_Proteing_fg_45, Avg_Prop_Proteing_fg_50:Avg_Prop_Proteing_fg_53,
              Avg_Prop_Proteing_fg_56:Avg_Prop_Proteing_fg_63, Avg_Prop_Proteing_fg_65,Avg_Prop_Proteing_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.protein.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Proteing_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Proteing_fg_2 ~ "White bread",
            Avg_Prop_Proteing_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Proteing_fg_4 ~ "Other breads",
            Avg_Prop_Proteing_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Proteing_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Proteing_fg_7 ~ "Biscuits",
            Avg_Prop_Proteing_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Proteing_fg_9 ~ "Puddings",
            Avg_Prop_Proteing_fg_10 ~ "Whole milk",
            Avg_Prop_Proteing_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Proteing_fg_12 ~ "Skimmed milk",
            Avg_Prop_Proteing_fg_13 ~ "Other milk and cream",
            Avg_Prop_Proteing_fg_14 ~ "Cheese",
            Avg_Prop_Proteing_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Proteing_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Proteing_fg_17 ~ "Butter",
            # Avg_Prop_Proteing_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Proteing_fg_19 ~ "Low fat spread",
            #  Avg_Prop_Proteing_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Proteing_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Proteing_fg_22 ~ "Bacon and ham",
            Avg_Prop_Proteing_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Proteing_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Proteing_fg_25 ~ "Pork and dishes",
            Avg_Prop_Proteing_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Proteing_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Proteing_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Proteing_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Proteing_fg_30 ~ "Sausages",
            Avg_Prop_Proteing_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Proteing_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Proteing_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Proteing_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Proteing_fg_35 ~ "Oily fish",
            Avg_Prop_Proteing_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Proteing_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Proteing_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Proteing_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Proteing_fg_40 ~ "Fruit",
            Avg_Prop_Proteing_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Proteing_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Proteing_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Proteing_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Proteing_fg_45 ~ "Fruit juice",
            # Avg_Prop_Proteing_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Proteing_fg_48 ~ "Wine",
            # Avg_Prop_Proteing_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Proteing_fg_50 ~ "Miscellaneous",
            Avg_Prop_Proteing_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Proteing_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Proteing_fg_53 ~ "Ice cream",
            # Avg_Prop_Proteing_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Proteing_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Proteing_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Proteing_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Proteing_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Proteing_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Proteing_fg_60 ~ "1% Milk",
            Avg_Prop_Proteing_fg_62 ~ "Sandwiches",
            Avg_Prop_Proteing_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Proteing_fg_64 ~ "Cheese DF",
            Avg_Prop_Proteing_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Proteing_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Proteing_fg_1:Avg_Prop_Proteing_fg_17, Avg_Prop_Proteing_fg_19,
              Avg_Prop_Proteing_fg_21:Avg_Prop_Proteing_fg_45, Avg_Prop_Proteing_fg_50:Avg_Prop_Proteing_fg_53,
              Avg_Prop_Proteing_fg_56:Avg_Prop_Proteing_fg_63, Avg_Prop_Proteing_fg_65,Avg_Prop_Proteing_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Fibre ####

table.fibre.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_AOACFibreg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_AOACFibreg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_AOACFibreg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_AOACFibreg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_AOACFibreg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_AOACFibreg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_AOACFibreg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_AOACFibreg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_AOACFibreg_foodcat_9 ~ "Fruit",
    Avg_Prop_AOACFibreg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_AOACFibreg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_AOACFibreg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_AOACFibreg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_AOACFibreg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_AOACFibreg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_AOACFibreg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_AOACFibreg_foodcat_1:Avg_Prop_AOACFibreg_foodcat_13,Avg_Prop_AOACFibreg_foodcat_15,Avg_Prop_AOACFibreg_foodcat_16,Avg_Prop_AOACFibreg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#age/sex

table.fibre.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_AOACFibreg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_AOACFibreg_fg_2 ~ "White bread",
            Avg_Prop_AOACFibreg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_AOACFibreg_fg_4 ~ "Other breads",
            Avg_Prop_AOACFibreg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_AOACFibreg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_AOACFibreg_fg_7 ~ "Biscuits",
            Avg_Prop_AOACFibreg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_AOACFibreg_fg_9 ~ "Puddings",
            # Avg_Prop_AOACFibreg_fg_10 ~ "Whole milk",
            #  Avg_Prop_AOACFibreg_fg_11 ~ "Semi-skimmed milk",
            #  Avg_Prop_AOACFibreg_fg_12 ~ "Skimmed milk",
            Avg_Prop_AOACFibreg_fg_13 ~ "Other milk and cream",
            Avg_Prop_AOACFibreg_fg_14 ~ "Cheese",
            Avg_Prop_AOACFibreg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_AOACFibreg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_AOACFibreg_fg_17 ~ "Butter",
            # Avg_Prop_AOACFibreg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_AOACFibreg_fg_19 ~ "Low fat spread",
            # Avg_Prop_AOACFibreg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_AOACFibreg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_AOACFibreg_fg_22 ~ "Bacon and ham",
            Avg_Prop_AOACFibreg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_AOACFibreg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_AOACFibreg_fg_25 ~ "Pork and dishes",
            Avg_Prop_AOACFibreg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_AOACFibreg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_AOACFibreg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_AOACFibreg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_AOACFibreg_fg_30 ~ "Sausages",
            Avg_Prop_AOACFibreg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_AOACFibreg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_AOACFibreg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_AOACFibreg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_AOACFibreg_fg_35 ~ "Oily fish",
            Avg_Prop_AOACFibreg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_AOACFibreg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_AOACFibreg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_AOACFibreg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_AOACFibreg_fg_40 ~ "Fruit",
            Avg_Prop_AOACFibreg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_AOACFibreg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_AOACFibreg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_AOACFibreg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_AOACFibreg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_AOACFibreg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_AOACFibreg_fg_48 ~ "Wine",
            #  Avg_Prop_AOACFibreg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_AOACFibreg_fg_50 ~ "Miscellaneous",
            #  Avg_Prop_AOACFibreg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_AOACFibreg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_AOACFibreg_fg_53 ~ "Ice cream",
            # Avg_Prop_AOACFibreg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_AOACFibreg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_AOACFibreg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_AOACFibreg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_AOACFibreg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_AOACFibreg_fg_59 ~ "Brown, granary and wheatgerm bread",
            #  Avg_Prop_AOACFibreg_fg_60 ~ "1% Milk",
            Avg_Prop_AOACFibreg_fg_62 ~ "Sandwiches",
            Avg_Prop_AOACFibreg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_AOACFibreg_fg_64 ~ "Cheese DF",
            Avg_Prop_AOACFibreg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_AOACFibreg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_AOACFibreg_fg_1:Avg_Prop_AOACFibreg_fg_9, Avg_Prop_AOACFibreg_fg_13:Avg_Prop_AOACFibreg_fg_17,
              Avg_Prop_AOACFibreg_fg_21:Avg_Prop_AOACFibreg_fg_45, Avg_Prop_AOACFibreg_fg_50, 
              Avg_Prop_AOACFibreg_fg_52, Avg_Prop_AOACFibreg_fg_53, Avg_Prop_AOACFibreg_fg_56:Avg_Prop_AOACFibreg_fg_59,
              Avg_Prop_AOACFibreg_fg_62,Avg_Prop_AOACFibreg_fg_63, Avg_Prop_AOACFibreg_fg_65,Avg_Prop_AOACFibreg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.fibre.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_AOACFibreg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_AOACFibreg_fg_2 ~ "White bread",
            Avg_Prop_AOACFibreg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_AOACFibreg_fg_4 ~ "Other breads",
            Avg_Prop_AOACFibreg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_AOACFibreg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_AOACFibreg_fg_7 ~ "Biscuits",
            Avg_Prop_AOACFibreg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_AOACFibreg_fg_9 ~ "Puddings",
            # Avg_Prop_AOACFibreg_fg_10 ~ "Whole milk",
            #  Avg_Prop_AOACFibreg_fg_11 ~ "Semi-skimmed milk",
            #  Avg_Prop_AOACFibreg_fg_12 ~ "Skimmed milk",
            Avg_Prop_AOACFibreg_fg_13 ~ "Other milk and cream",
            Avg_Prop_AOACFibreg_fg_14 ~ "Cheese",
            Avg_Prop_AOACFibreg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_AOACFibreg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_AOACFibreg_fg_17 ~ "Butter",
            # Avg_Prop_AOACFibreg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_AOACFibreg_fg_19 ~ "Low fat spread",
            # Avg_Prop_AOACFibreg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_AOACFibreg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_AOACFibreg_fg_22 ~ "Bacon and ham",
            Avg_Prop_AOACFibreg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_AOACFibreg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_AOACFibreg_fg_25 ~ "Pork and dishes",
            Avg_Prop_AOACFibreg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_AOACFibreg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_AOACFibreg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_AOACFibreg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_AOACFibreg_fg_30 ~ "Sausages",
            Avg_Prop_AOACFibreg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_AOACFibreg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_AOACFibreg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_AOACFibreg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_AOACFibreg_fg_35 ~ "Oily fish",
            Avg_Prop_AOACFibreg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_AOACFibreg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_AOACFibreg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_AOACFibreg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_AOACFibreg_fg_40 ~ "Fruit",
            Avg_Prop_AOACFibreg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_AOACFibreg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_AOACFibreg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_AOACFibreg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_AOACFibreg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_AOACFibreg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_AOACFibreg_fg_48 ~ "Wine",
            #  Avg_Prop_AOACFibreg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_AOACFibreg_fg_50 ~ "Miscellaneous",
            #  Avg_Prop_AOACFibreg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_AOACFibreg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_AOACFibreg_fg_53 ~ "Ice cream",
            # Avg_Prop_AOACFibreg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_AOACFibreg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_AOACFibreg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_AOACFibreg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_AOACFibreg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_AOACFibreg_fg_59 ~ "Brown, granary and wheatgerm bread",
            #  Avg_Prop_AOACFibreg_fg_60 ~ "1% Milk",
            Avg_Prop_AOACFibreg_fg_62 ~ "Sandwiches",
            Avg_Prop_AOACFibreg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_AOACFibreg_fg_64 ~ "Cheese DF",
            Avg_Prop_AOACFibreg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_AOACFibreg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_AOACFibreg_fg_1:Avg_Prop_AOACFibreg_fg_9, Avg_Prop_AOACFibreg_fg_13:Avg_Prop_AOACFibreg_fg_17,
              Avg_Prop_AOACFibreg_fg_21:Avg_Prop_AOACFibreg_fg_45, Avg_Prop_AOACFibreg_fg_50, 
              Avg_Prop_AOACFibreg_fg_52, Avg_Prop_AOACFibreg_fg_53, Avg_Prop_AOACFibreg_fg_56:Avg_Prop_AOACFibreg_fg_59,
              Avg_Prop_AOACFibreg_fg_62,Avg_Prop_AOACFibreg_fg_63, Avg_Prop_AOACFibreg_fg_65,Avg_Prop_AOACFibreg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Vitamin A ####

#age/sex

table.VitA.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_VitaminAug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_VitaminAug_fg_2 ~ "White bread",
            #  Avg_Prop_VitaminAug_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_VitaminAug_fg_4 ~ "Other breads",
            Avg_Prop_VitaminAug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_VitaminAug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_VitaminAug_fg_7 ~ "Biscuits",
            Avg_Prop_VitaminAug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_VitaminAug_fg_9 ~ "Puddings",
            Avg_Prop_VitaminAug_fg_10 ~ "Whole milk",
            Avg_Prop_VitaminAug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_VitaminAug_fg_12 ~ "Skimmed milk",
            Avg_Prop_VitaminAug_fg_13 ~ "Other milk and cream",
            Avg_Prop_VitaminAug_fg_14 ~ "Cheese",
            Avg_Prop_VitaminAug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_VitaminAug_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_VitaminAug_fg_17 ~ "Butter",
            #  Avg_Prop_VitaminAug_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_VitaminAug_fg_19 ~ "Low fat spread",
            # Avg_Prop_VitaminAug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_VitaminAug_fg_21 ~ "Reduced fat spread",
            # Avg_Prop_VitaminAug_fg_22 ~ "Bacon and ham",
            Avg_Prop_VitaminAug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_VitaminAug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_VitaminAug_fg_25 ~ "Pork and dishes",
            Avg_Prop_VitaminAug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_VitaminAug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_VitaminAug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_VitaminAug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_VitaminAug_fg_30 ~ "Sausages",
            Avg_Prop_VitaminAug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_VitaminAug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_VitaminAug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_VitaminAug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_VitaminAug_fg_35 ~ "Oily fish",
            Avg_Prop_VitaminAug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_VitaminAug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_VitaminAug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_VitaminAug_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_VitaminAug_fg_40 ~ "Fruit",
            Avg_Prop_VitaminAug_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_VitaminAug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_VitaminAug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_VitaminAug_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_VitaminAug_fg_45 ~ "Fruit juice",
            # Avg_Prop_VitaminAug_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_VitaminAug_fg_48 ~ "Wine",
            # Avg_Prop_VitaminAug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_VitaminAug_fg_50 ~ "Miscellaneous",
            Avg_Prop_VitaminAug_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_VitaminAug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_VitaminAug_fg_53 ~ "Ice cream",
            #  Avg_Prop_VitaminAug_fg_54 ~ "Dietary supplements",
            # Avg_Prop_VitaminAug_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_VitaminAug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_VitaminAug_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_VitaminAug_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_VitaminAug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_VitaminAug_fg_60 ~ "1% Milk",
            Avg_Prop_VitaminAug_fg_62 ~ "Sandwiches",
            #   Avg_Prop_VitaminAug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_VitaminAug_fg_64 ~ "Cheese DF",
            Avg_Prop_VitaminAug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF"
            # Avg_Prop_VitaminAug_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_VitaminAug_fg_1, Avg_Prop_VitaminAug_fg_2, Avg_Prop_VitaminAug_fg_5:Avg_Prop_VitaminAug_fg_17,
              Avg_Prop_VitaminAug_fg_19,Avg_Prop_VitaminAug_fg_21, Avg_Prop_VitaminAug_fg_23:Avg_Prop_VitaminAug_fg_45,
              Avg_Prop_VitaminAug_fg_50:Avg_Prop_VitaminAug_fg_53, Avg_Prop_VitaminAug_fg_56:Avg_Prop_VitaminAug_fg_62,
              Avg_Prop_VitaminAug_fg_65)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.VitA.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_VitaminAug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_VitaminAug_fg_2 ~ "White bread",
            #  Avg_Prop_VitaminAug_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_VitaminAug_fg_4 ~ "Other breads",
            Avg_Prop_VitaminAug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_VitaminAug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_VitaminAug_fg_7 ~ "Biscuits",
            Avg_Prop_VitaminAug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_VitaminAug_fg_9 ~ "Puddings",
            Avg_Prop_VitaminAug_fg_10 ~ "Whole milk",
            Avg_Prop_VitaminAug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_VitaminAug_fg_12 ~ "Skimmed milk",
            Avg_Prop_VitaminAug_fg_13 ~ "Other milk and cream",
            Avg_Prop_VitaminAug_fg_14 ~ "Cheese",
            Avg_Prop_VitaminAug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_VitaminAug_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_VitaminAug_fg_17 ~ "Butter",
            #  Avg_Prop_VitaminAug_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_VitaminAug_fg_19 ~ "Low fat spread",
            # Avg_Prop_VitaminAug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_VitaminAug_fg_21 ~ "Reduced fat spread",
            # Avg_Prop_VitaminAug_fg_22 ~ "Bacon and ham",
            Avg_Prop_VitaminAug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_VitaminAug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_VitaminAug_fg_25 ~ "Pork and dishes",
            Avg_Prop_VitaminAug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_VitaminAug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_VitaminAug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_VitaminAug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_VitaminAug_fg_30 ~ "Sausages",
            Avg_Prop_VitaminAug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_VitaminAug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_VitaminAug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_VitaminAug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_VitaminAug_fg_35 ~ "Oily fish",
            Avg_Prop_VitaminAug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_VitaminAug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_VitaminAug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_VitaminAug_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_VitaminAug_fg_40 ~ "Fruit",
            Avg_Prop_VitaminAug_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_VitaminAug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_VitaminAug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_VitaminAug_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_VitaminAug_fg_45 ~ "Fruit juice",
            # Avg_Prop_VitaminAug_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_VitaminAug_fg_48 ~ "Wine",
            # Avg_Prop_VitaminAug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_VitaminAug_fg_50 ~ "Miscellaneous",
            Avg_Prop_VitaminAug_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_VitaminAug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_VitaminAug_fg_53 ~ "Ice cream",
            #  Avg_Prop_VitaminAug_fg_54 ~ "Dietary supplements",
            # Avg_Prop_VitaminAug_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_VitaminAug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_VitaminAug_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_VitaminAug_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_VitaminAug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_VitaminAug_fg_60 ~ "1% Milk",
            Avg_Prop_VitaminAug_fg_62 ~ "Sandwiches",
            #   Avg_Prop_VitaminAug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_VitaminAug_fg_64 ~ "Cheese DF",
            Avg_Prop_VitaminAug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF"
            # Avg_Prop_VitaminAug_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_VitaminAug_fg_1, Avg_Prop_VitaminAug_fg_2, Avg_Prop_VitaminAug_fg_5:Avg_Prop_VitaminAug_fg_17,
              Avg_Prop_VitaminAug_fg_19,Avg_Prop_VitaminAug_fg_21, Avg_Prop_VitaminAug_fg_23:Avg_Prop_VitaminAug_fg_45,
              Avg_Prop_VitaminAug_fg_50:Avg_Prop_VitaminAug_fg_53, Avg_Prop_VitaminAug_fg_56:Avg_Prop_VitaminAug_fg_62,
              Avg_Prop_VitaminAug_fg_65)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Riboflavin ####

#age/sex

table.riboflavin.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Riboflavinmg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Riboflavinmg_fg_2 ~ "White bread",
            Avg_Prop_Riboflavinmg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Riboflavinmg_fg_4 ~ "Other breads",
            Avg_Prop_Riboflavinmg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Riboflavinmg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Riboflavinmg_fg_7 ~ "Biscuits",
            Avg_Prop_Riboflavinmg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Riboflavinmg_fg_9 ~ "Puddings",
            Avg_Prop_Riboflavinmg_fg_10 ~ "Whole milk",
            Avg_Prop_Riboflavinmg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Riboflavinmg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Riboflavinmg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Riboflavinmg_fg_14 ~ "Cheese",
            Avg_Prop_Riboflavinmg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Riboflavinmg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Riboflavinmg_fg_17 ~ "Butter",
            #  Avg_Prop_Riboflavinmg_fg_18 ~ "Polyunsaturated margarine and oils",
            #  Avg_Prop_Riboflavinmg_fg_19 ~ "Low fat spread",
            # Avg_Prop_Riboflavinmg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Riboflavinmg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Riboflavinmg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Riboflavinmg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Riboflavinmg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Riboflavinmg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Riboflavinmg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Riboflavinmg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Riboflavinmg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Riboflavinmg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Riboflavinmg_fg_30 ~ "Sausages",
            Avg_Prop_Riboflavinmg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Riboflavinmg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Riboflavinmg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Riboflavinmg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Riboflavinmg_fg_35 ~ "Oily fish",
            Avg_Prop_Riboflavinmg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Riboflavinmg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Riboflavinmg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Riboflavinmg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Riboflavinmg_fg_40 ~ "Fruit",
            Avg_Prop_Riboflavinmg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Riboflavinmg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Riboflavinmg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Riboflavinmg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Riboflavinmg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Riboflavinmg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Riboflavinmg_fg_48 ~ "Wine",
            #  Avg_Prop_Riboflavinmg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Riboflavinmg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Riboflavinmg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Riboflavinmg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Riboflavinmg_fg_53 ~ "Ice cream",
            # Avg_Prop_Riboflavinmg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Riboflavinmg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Riboflavinmg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Riboflavinmg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Riboflavinmg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Riboflavinmg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Riboflavinmg_fg_60 ~ "1% Milk",
            Avg_Prop_Riboflavinmg_fg_62 ~ "Sandwiches",
            Avg_Prop_Riboflavinmg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Riboflavinmg_fg_64 ~ "Cheese DF",
            Avg_Prop_Riboflavinmg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF"
            # Avg_Prop_Riboflavinmg_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Riboflavinmg_fg_1:Avg_Prop_Riboflavinmg_fg_17, Avg_Prop_Riboflavinmg_fg_21:Avg_Prop_Riboflavinmg_fg_45,
              Avg_Prop_Riboflavinmg_fg_50:Avg_Prop_Riboflavinmg_fg_53,Avg_Prop_Riboflavinmg_fg_56:Avg_Prop_Riboflavinmg_fg_63, Avg_Prop_Riboflavinmg_fg_65)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



#SIMD

table.riboflavin.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Riboflavinmg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Riboflavinmg_fg_2 ~ "White bread",
            Avg_Prop_Riboflavinmg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Riboflavinmg_fg_4 ~ "Other breads",
            Avg_Prop_Riboflavinmg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Riboflavinmg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Riboflavinmg_fg_7 ~ "Biscuits",
            Avg_Prop_Riboflavinmg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Riboflavinmg_fg_9 ~ "Puddings",
            Avg_Prop_Riboflavinmg_fg_10 ~ "Whole milk",
            Avg_Prop_Riboflavinmg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Riboflavinmg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Riboflavinmg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Riboflavinmg_fg_14 ~ "Cheese",
            Avg_Prop_Riboflavinmg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Riboflavinmg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Riboflavinmg_fg_17 ~ "Butter",
            #  Avg_Prop_Riboflavinmg_fg_18 ~ "Polyunsaturated margarine and oils",
            #  Avg_Prop_Riboflavinmg_fg_19 ~ "Low fat spread",
            # Avg_Prop_Riboflavinmg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Riboflavinmg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Riboflavinmg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Riboflavinmg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Riboflavinmg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Riboflavinmg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Riboflavinmg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Riboflavinmg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Riboflavinmg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Riboflavinmg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Riboflavinmg_fg_30 ~ "Sausages",
            Avg_Prop_Riboflavinmg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Riboflavinmg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Riboflavinmg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Riboflavinmg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Riboflavinmg_fg_35 ~ "Oily fish",
            Avg_Prop_Riboflavinmg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Riboflavinmg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Riboflavinmg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Riboflavinmg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Riboflavinmg_fg_40 ~ "Fruit",
            Avg_Prop_Riboflavinmg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Riboflavinmg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Riboflavinmg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Riboflavinmg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Riboflavinmg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Riboflavinmg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Riboflavinmg_fg_48 ~ "Wine",
            #  Avg_Prop_Riboflavinmg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Riboflavinmg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Riboflavinmg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Riboflavinmg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Riboflavinmg_fg_53 ~ "Ice cream",
            # Avg_Prop_Riboflavinmg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Riboflavinmg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Riboflavinmg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Riboflavinmg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Riboflavinmg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Riboflavinmg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Riboflavinmg_fg_60 ~ "1% Milk",
            Avg_Prop_Riboflavinmg_fg_62 ~ "Sandwiches",
            Avg_Prop_Riboflavinmg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Riboflavinmg_fg_64 ~ "Cheese DF",
            Avg_Prop_Riboflavinmg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF"
            # Avg_Prop_Riboflavinmg_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Riboflavinmg_fg_1:Avg_Prop_Riboflavinmg_fg_17, Avg_Prop_Riboflavinmg_fg_21:Avg_Prop_Riboflavinmg_fg_45,
              Avg_Prop_Riboflavinmg_fg_50:Avg_Prop_Riboflavinmg_fg_53,Avg_Prop_Riboflavinmg_fg_56:Avg_Prop_Riboflavinmg_fg_63, Avg_Prop_Riboflavinmg_fg_65)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()




##Folate ####

#age/sex

table.folate.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Folateug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Folateug_fg_2 ~ "White bread",
            Avg_Prop_Folateug_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Folateug_fg_4 ~ "Other breads",
            Avg_Prop_Folateug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Folateug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Folateug_fg_7 ~ "Biscuits",
            Avg_Prop_Folateug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Folateug_fg_9 ~ "Puddings",
            Avg_Prop_Folateug_fg_10 ~ "Whole milk",
            Avg_Prop_Folateug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Folateug_fg_12 ~ "Skimmed milk",
            Avg_Prop_Folateug_fg_13 ~ "Other milk and cream",
            Avg_Prop_Folateug_fg_14 ~ "Cheese",
            Avg_Prop_Folateug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Folateug_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_Folateug_fg_17 ~ "Butter",
            #  Avg_Prop_Folateug_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_Folateug_fg_19 ~ "Low fat spread",
            # Avg_Prop_Folateug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Folateug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Folateug_fg_22 ~ "Bacon and ham",
            Avg_Prop_Folateug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Folateug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Folateug_fg_25 ~ "Pork and dishes",
            Avg_Prop_Folateug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Folateug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Folateug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Folateug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Folateug_fg_30 ~ "Sausages",
            Avg_Prop_Folateug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Folateug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Folateug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Folateug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Folateug_fg_35 ~ "Oily fish",
            Avg_Prop_Folateug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Folateug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Folateug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Folateug_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Folateug_fg_40 ~ "Fruit",
            Avg_Prop_Folateug_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Folateug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Folateug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Folateug_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Folateug_fg_45 ~ "Fruit juice",
            # Avg_Prop_Folateug_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Folateug_fg_48 ~ "Wine",
            # Avg_Prop_Folateug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Folateug_fg_50 ~ "Miscellaneous",
            Avg_Prop_Folateug_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Folateug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Folateug_fg_53 ~ "Ice cream",
            #  Avg_Prop_Folateug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Folateug_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Folateug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Folateug_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Folateug_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Folateug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Folateug_fg_60 ~ "1% Milk",
            Avg_Prop_Folateug_fg_62 ~ "Sandwiches",
            Avg_Prop_Folateug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Folateug_fg_64 ~ "Cheese DF",
            Avg_Prop_Folateug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Folateug_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Folateug_fg_1:Avg_Prop_Folateug_fg_16, Avg_Prop_Folateug_fg_21:Avg_Prop_Folateug_fg_45,
              Avg_Prop_Folateug_fg_50:Avg_Prop_Folateug_fg_53, Avg_Prop_Folateug_fg_56:Avg_Prop_Folateug_fg_63, Avg_Prop_Folateug_fg_65,Avg_Prop_Folateug_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



#SIMD

table.folate.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Folateug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Folateug_fg_2 ~ "White bread",
            Avg_Prop_Folateug_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Folateug_fg_4 ~ "Other breads",
            Avg_Prop_Folateug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Folateug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Folateug_fg_7 ~ "Biscuits",
            Avg_Prop_Folateug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Folateug_fg_9 ~ "Puddings",
            Avg_Prop_Folateug_fg_10 ~ "Whole milk",
            Avg_Prop_Folateug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Folateug_fg_12 ~ "Skimmed milk",
            Avg_Prop_Folateug_fg_13 ~ "Other milk and cream",
            Avg_Prop_Folateug_fg_14 ~ "Cheese",
            Avg_Prop_Folateug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Folateug_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_Folateug_fg_17 ~ "Butter",
            #  Avg_Prop_Folateug_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_Folateug_fg_19 ~ "Low fat spread",
            # Avg_Prop_Folateug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Folateug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Folateug_fg_22 ~ "Bacon and ham",
            Avg_Prop_Folateug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Folateug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Folateug_fg_25 ~ "Pork and dishes",
            Avg_Prop_Folateug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Folateug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Folateug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Folateug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Folateug_fg_30 ~ "Sausages",
            Avg_Prop_Folateug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Folateug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Folateug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Folateug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Folateug_fg_35 ~ "Oily fish",
            Avg_Prop_Folateug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Folateug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Folateug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Folateug_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Folateug_fg_40 ~ "Fruit",
            Avg_Prop_Folateug_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Folateug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Folateug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Folateug_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Folateug_fg_45 ~ "Fruit juice",
            # Avg_Prop_Folateug_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Folateug_fg_48 ~ "Wine",
            # Avg_Prop_Folateug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Folateug_fg_50 ~ "Miscellaneous",
            Avg_Prop_Folateug_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Folateug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Folateug_fg_53 ~ "Ice cream",
            #  Avg_Prop_Folateug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Folateug_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Folateug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Folateug_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Folateug_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Folateug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Folateug_fg_60 ~ "1% Milk",
            Avg_Prop_Folateug_fg_62 ~ "Sandwiches",
            Avg_Prop_Folateug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Folateug_fg_64 ~ "Cheese DF",
            Avg_Prop_Folateug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Folateug_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Folateug_fg_1:Avg_Prop_Folateug_fg_16, Avg_Prop_Folateug_fg_21:Avg_Prop_Folateug_fg_45,
              Avg_Prop_Folateug_fg_50:Avg_Prop_Folateug_fg_53, Avg_Prop_Folateug_fg_56:Avg_Prop_Folateug_fg_63, Avg_Prop_Folateug_fg_65,Avg_Prop_Folateug_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Vitamin D ####

#age/sex

table.VitD.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_VitaminDug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_VitaminDug_fg_2 ~ "White bread",
            # Avg_Prop_VitaminDug_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_VitaminDug_fg_4 ~ "Other breads",
            Avg_Prop_VitaminDug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_VitaminDug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_VitaminDug_fg_7 ~ "Biscuits",
            Avg_Prop_VitaminDug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_VitaminDug_fg_9 ~ "Puddings",
            Avg_Prop_VitaminDug_fg_10 ~ "Whole milk",
            #   Avg_Prop_VitaminDug_fg_11 ~ "Semi-skimmed milk",
            #  Avg_Prop_VitaminDug_fg_12 ~ "Skimmed milk",
            Avg_Prop_VitaminDug_fg_13 ~ "Other milk and cream",
            Avg_Prop_VitaminDug_fg_14 ~ "Cheese",
            Avg_Prop_VitaminDug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_VitaminDug_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_VitaminDug_fg_17 ~ "Butter",
            # Avg_Prop_VitaminDug_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_VitaminDug_fg_19 ~ "Low fat spread",
            #  Avg_Prop_VitaminDug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_VitaminDug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_VitaminDug_fg_22 ~ "Bacon and ham",
            Avg_Prop_VitaminDug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_VitaminDug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_VitaminDug_fg_25 ~ "Pork and dishes",
            Avg_Prop_VitaminDug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_VitaminDug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_VitaminDug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_VitaminDug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_VitaminDug_fg_30 ~ "Sausages",
            Avg_Prop_VitaminDug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_VitaminDug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_VitaminDug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_VitaminDug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_VitaminDug_fg_35 ~ "Oily fish",
            # Avg_Prop_VitaminDug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_VitaminDug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_VitaminDug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_VitaminDug_fg_39 ~ "Other potatoes, potato salads and dishes",
            # Avg_Prop_VitaminDug_fg_40 ~ "Fruit",
            Avg_Prop_VitaminDug_fg_41 ~ "Sugars, preserves and sweet spreads",
            #  Avg_Prop_VitaminDug_fg_42 ~ "Crisps and savoury snacks",
            #  Avg_Prop_VitaminDug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_VitaminDug_fg_44 ~ "Chocolate confectionery",
            # Avg_Prop_VitaminDug_fg_45 ~ "Fruit juice",
            # Avg_Prop_VitaminDug_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_VitaminDug_fg_48 ~ "Wine",
            # Avg_Prop_VitaminDug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_VitaminDug_fg_50 ~ "Miscellaneous",
            # Avg_Prop_VitaminDug_fg_51 ~ "Tea, coffee and water",
            # Avg_Prop_VitaminDug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_VitaminDug_fg_53 ~ "Ice cream",
            # Avg_Prop_VitaminDug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_VitaminDug_fg_55 ~ "Artificial sweeteners",
            # Avg_Prop_VitaminDug_fg_56 ~ "Nuts and seeds",
            #  Avg_Prop_VitaminDug_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_VitaminDug_fg_58 ~ "Soft drinks, diet",
            # Avg_Prop_VitaminDug_fg_59 ~ "Brown, granary and wheatgerm bread",
            # Avg_Prop_VitaminDug_fg_60 ~ "1% Milk",
            Avg_Prop_VitaminDug_fg_62 ~ "Sandwiches",
            Avg_Prop_VitaminDug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_VitaminDug_fg_64 ~ "Cheese DF",
            Avg_Prop_VitaminDug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF"
            #  Avg_Prop_VitaminDug_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_VitaminDug_fg_1, Avg_Prop_VitaminDug_fg_2,Avg_Prop_VitaminDug_fg_5:Avg_Prop_VitaminDug_fg_10,
              Avg_Prop_VitaminDug_fg_13:Avg_Prop_VitaminDug_fg_17, Avg_Prop_VitaminDug_fg_19,
              Avg_Prop_VitaminDug_fg_21:Avg_Prop_VitaminDug_fg_35, Avg_Prop_VitaminDug_fg_37:Avg_Prop_VitaminDug_fg_39,
              Avg_Prop_VitaminDug_fg_41, Avg_Prop_VitaminDug_fg_44, Avg_Prop_VitaminDug_fg_50,Avg_Prop_VitaminDug_fg_53,
              Avg_Prop_VitaminDug_fg_62, Avg_Prop_VitaminDug_fg_63, Avg_Prop_VitaminDug_fg_65)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



#SIMD

table.VitD.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_VitaminDug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_VitaminDug_fg_2 ~ "White bread",
            # Avg_Prop_VitaminDug_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_VitaminDug_fg_4 ~ "Other breads",
            Avg_Prop_VitaminDug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_VitaminDug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_VitaminDug_fg_7 ~ "Biscuits",
            Avg_Prop_VitaminDug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_VitaminDug_fg_9 ~ "Puddings",
            Avg_Prop_VitaminDug_fg_10 ~ "Whole milk",
            #   Avg_Prop_VitaminDug_fg_11 ~ "Semi-skimmed milk",
            #  Avg_Prop_VitaminDug_fg_12 ~ "Skimmed milk",
            Avg_Prop_VitaminDug_fg_13 ~ "Other milk and cream",
            Avg_Prop_VitaminDug_fg_14 ~ "Cheese",
            Avg_Prop_VitaminDug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_VitaminDug_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_VitaminDug_fg_17 ~ "Butter",
            # Avg_Prop_VitaminDug_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_VitaminDug_fg_19 ~ "Low fat spread",
            #  Avg_Prop_VitaminDug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_VitaminDug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_VitaminDug_fg_22 ~ "Bacon and ham",
            Avg_Prop_VitaminDug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_VitaminDug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_VitaminDug_fg_25 ~ "Pork and dishes",
            Avg_Prop_VitaminDug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_VitaminDug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_VitaminDug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_VitaminDug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_VitaminDug_fg_30 ~ "Sausages",
            Avg_Prop_VitaminDug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_VitaminDug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_VitaminDug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_VitaminDug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_VitaminDug_fg_35 ~ "Oily fish",
            # Avg_Prop_VitaminDug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_VitaminDug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_VitaminDug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_VitaminDug_fg_39 ~ "Other potatoes, potato salads and dishes",
            # Avg_Prop_VitaminDug_fg_40 ~ "Fruit",
            Avg_Prop_VitaminDug_fg_41 ~ "Sugars, preserves and sweet spreads",
            #  Avg_Prop_VitaminDug_fg_42 ~ "Crisps and savoury snacks",
            #  Avg_Prop_VitaminDug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_VitaminDug_fg_44 ~ "Chocolate confectionery",
            # Avg_Prop_VitaminDug_fg_45 ~ "Fruit juice",
            # Avg_Prop_VitaminDug_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_VitaminDug_fg_48 ~ "Wine",
            # Avg_Prop_VitaminDug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_VitaminDug_fg_50 ~ "Miscellaneous",
            # Avg_Prop_VitaminDug_fg_51 ~ "Tea, coffee and water",
            # Avg_Prop_VitaminDug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_VitaminDug_fg_53 ~ "Ice cream",
            # Avg_Prop_VitaminDug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_VitaminDug_fg_55 ~ "Artificial sweeteners",
            # Avg_Prop_VitaminDug_fg_56 ~ "Nuts and seeds",
            #  Avg_Prop_VitaminDug_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_VitaminDug_fg_58 ~ "Soft drinks, diet",
            # Avg_Prop_VitaminDug_fg_59 ~ "Brown, granary and wheatgerm bread",
            # Avg_Prop_VitaminDug_fg_60 ~ "1% Milk",
            Avg_Prop_VitaminDug_fg_62 ~ "Sandwiches",
            Avg_Prop_VitaminDug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_VitaminDug_fg_64 ~ "Cheese DF",
            Avg_Prop_VitaminDug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF"
            #  Avg_Prop_VitaminDug_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_VitaminDug_fg_1, Avg_Prop_VitaminDug_fg_2,Avg_Prop_VitaminDug_fg_5:Avg_Prop_VitaminDug_fg_10,
              Avg_Prop_VitaminDug_fg_13:Avg_Prop_VitaminDug_fg_17, Avg_Prop_VitaminDug_fg_19,
              Avg_Prop_VitaminDug_fg_21:Avg_Prop_VitaminDug_fg_35, Avg_Prop_VitaminDug_fg_37:Avg_Prop_VitaminDug_fg_39,
              Avg_Prop_VitaminDug_fg_41, Avg_Prop_VitaminDug_fg_44, Avg_Prop_VitaminDug_fg_50,Avg_Prop_VitaminDug_fg_53,
              Avg_Prop_VitaminDug_fg_62, Avg_Prop_VitaminDug_fg_63, Avg_Prop_VitaminDug_fg_65)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()




##Vitamin B12 ####

#age/sex

table.VitB12.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_VitaminB12ug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_VitaminB12ug_fg_2 ~ "White bread",
            #  Avg_Prop_VitaminB12ug_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_VitaminB12ug_fg_4 ~ "Other breads",
            Avg_Prop_VitaminB12ug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_VitaminB12ug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_VitaminB12ug_fg_7 ~ "Biscuits",
            Avg_Prop_VitaminB12ug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_VitaminB12ug_fg_9 ~ "Puddings",
            Avg_Prop_VitaminB12ug_fg_10 ~ "Whole milk",
            Avg_Prop_VitaminB12ug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_VitaminB12ug_fg_12 ~ "Skimmed milk",
            Avg_Prop_VitaminB12ug_fg_13 ~ "Other milk and cream",
            Avg_Prop_VitaminB12ug_fg_14 ~ "Cheese",
            Avg_Prop_VitaminB12ug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_VitaminB12ug_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_VitaminB12ug_fg_17 ~ "Butter",
            #  Avg_Prop_VitaminB12ug_fg_18 ~ "Polyunsaturated margarine and oils",
            #  Avg_Prop_VitaminB12ug_fg_19 ~ "Low fat spread",
            # Avg_Prop_VitaminB12ug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_VitaminB12ug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_VitaminB12ug_fg_22 ~ "Bacon and ham",
            Avg_Prop_VitaminB12ug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_VitaminB12ug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_VitaminB12ug_fg_25 ~ "Pork and dishes",
            Avg_Prop_VitaminB12ug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_VitaminB12ug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_VitaminB12ug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_VitaminB12ug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_VitaminB12ug_fg_30 ~ "Sausages",
            Avg_Prop_VitaminB12ug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_VitaminB12ug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_VitaminB12ug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_VitaminB12ug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_VitaminB12ug_fg_35 ~ "Oily fish",
            Avg_Prop_VitaminB12ug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_VitaminB12ug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_VitaminB12ug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            # Avg_Prop_VitaminB12ug_fg_39 ~ "Other potatoes, potato salads and dishes",
            #  Avg_Prop_VitaminB12ug_fg_40 ~ "Fruit",
            Avg_Prop_VitaminB12ug_fg_41 ~ "Sugars, preserves and sweet spreads",
            #   Avg_Prop_VitaminB12ug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_VitaminB12ug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_VitaminB12ug_fg_44 ~ "Chocolate confectionery",
            # Avg_Prop_VitaminB12ug_fg_45 ~ "Fruit juice",
            #  Avg_Prop_VitaminB12ug_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_VitaminB12ug_fg_48 ~ "Wine",
            #  Avg_Prop_VitaminB12ug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_VitaminB12ug_fg_50 ~ "Miscellaneous",
            Avg_Prop_VitaminB12ug_fg_51 ~ "Tea, coffee and water",
            # Avg_Prop_VitaminB12ug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_VitaminB12ug_fg_53 ~ "Ice cream",
            # Avg_Prop_VitaminB12ug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_VitaminB12ug_fg_55 ~ "Artificial sweeteners",
            # Avg_Prop_VitaminB12ug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_VitaminB12ug_fg_57 ~ "Soft drinks, not diet",
            #  Avg_Prop_VitaminB12ug_fg_58 ~ "Soft drinks, diet",
            #  Avg_Prop_VitaminB12ug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_VitaminB12ug_fg_60 ~ "1% Milk",
            Avg_Prop_VitaminB12ug_fg_62 ~ "Sandwiches",
            Avg_Prop_VitaminB12ug_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_VitaminB12ug_fg_64 ~ "Cheese DF",
            Avg_Prop_VitaminB12ug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF"
            # Avg_Prop_VitaminB12ug_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_VitaminB12ug_fg_1,Avg_Prop_VitaminB12ug_fg_2, Avg_Prop_VitaminB12ug_fg_5:Avg_Prop_VitaminB12ug_fg_17,
              Avg_Prop_VitaminB12ug_fg_21:Avg_Prop_VitaminB12ug_fg_38, Avg_Prop_VitaminB12ug_fg_41, Avg_Prop_VitaminB12ug_fg_43, Avg_Prop_VitaminB12ug_fg_44,
              Avg_Prop_VitaminB12ug_fg_50,Avg_Prop_VitaminB12ug_fg_51, Avg_Prop_VitaminB12ug_fg_53, Avg_Prop_VitaminB12ug_fg_57, 
              Avg_Prop_VitaminB12ug_fg_60:Avg_Prop_VitaminB12ug_fg_63, Avg_Prop_VitaminB12ug_fg_65)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



#SIMD
table.VitB12.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_VitaminB12ug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_VitaminB12ug_fg_2 ~ "White bread",
            #  Avg_Prop_VitaminB12ug_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_VitaminB12ug_fg_4 ~ "Other breads",
            Avg_Prop_VitaminB12ug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_VitaminB12ug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_VitaminB12ug_fg_7 ~ "Biscuits",
            Avg_Prop_VitaminB12ug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_VitaminB12ug_fg_9 ~ "Puddings",
            Avg_Prop_VitaminB12ug_fg_10 ~ "Whole milk",
            Avg_Prop_VitaminB12ug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_VitaminB12ug_fg_12 ~ "Skimmed milk",
            Avg_Prop_VitaminB12ug_fg_13 ~ "Other milk and cream",
            Avg_Prop_VitaminB12ug_fg_14 ~ "Cheese",
            Avg_Prop_VitaminB12ug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_VitaminB12ug_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_VitaminB12ug_fg_17 ~ "Butter",
            #  Avg_Prop_VitaminB12ug_fg_18 ~ "Polyunsaturated margarine and oils",
            #  Avg_Prop_VitaminB12ug_fg_19 ~ "Low fat spread",
            # Avg_Prop_VitaminB12ug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_VitaminB12ug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_VitaminB12ug_fg_22 ~ "Bacon and ham",
            Avg_Prop_VitaminB12ug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_VitaminB12ug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_VitaminB12ug_fg_25 ~ "Pork and dishes",
            Avg_Prop_VitaminB12ug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_VitaminB12ug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_VitaminB12ug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_VitaminB12ug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_VitaminB12ug_fg_30 ~ "Sausages",
            Avg_Prop_VitaminB12ug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_VitaminB12ug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_VitaminB12ug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_VitaminB12ug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_VitaminB12ug_fg_35 ~ "Oily fish",
            Avg_Prop_VitaminB12ug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_VitaminB12ug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_VitaminB12ug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            # Avg_Prop_VitaminB12ug_fg_39 ~ "Other potatoes, potato salads and dishes",
            #  Avg_Prop_VitaminB12ug_fg_40 ~ "Fruit",
            Avg_Prop_VitaminB12ug_fg_41 ~ "Sugars, preserves and sweet spreads",
            #   Avg_Prop_VitaminB12ug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_VitaminB12ug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_VitaminB12ug_fg_44 ~ "Chocolate confectionery",
            # Avg_Prop_VitaminB12ug_fg_45 ~ "Fruit juice",
            #  Avg_Prop_VitaminB12ug_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_VitaminB12ug_fg_48 ~ "Wine",
            #  Avg_Prop_VitaminB12ug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_VitaminB12ug_fg_50 ~ "Miscellaneous",
            Avg_Prop_VitaminB12ug_fg_51 ~ "Tea, coffee and water",
            # Avg_Prop_VitaminB12ug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_VitaminB12ug_fg_53 ~ "Ice cream",
            # Avg_Prop_VitaminB12ug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_VitaminB12ug_fg_55 ~ "Artificial sweeteners",
            # Avg_Prop_VitaminB12ug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_VitaminB12ug_fg_57 ~ "Soft drinks, not diet",
            #  Avg_Prop_VitaminB12ug_fg_58 ~ "Soft drinks, diet",
            #  Avg_Prop_VitaminB12ug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_VitaminB12ug_fg_60 ~ "1% Milk",
            Avg_Prop_VitaminB12ug_fg_62 ~ "Sandwiches",
            Avg_Prop_VitaminB12ug_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_VitaminB12ug_fg_64 ~ "Cheese DF",
            Avg_Prop_VitaminB12ug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF"
            # Avg_Prop_VitaminB12ug_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_VitaminB12ug_fg_1,Avg_Prop_VitaminB12ug_fg_2, Avg_Prop_VitaminB12ug_fg_5:Avg_Prop_VitaminB12ug_fg_17,
              Avg_Prop_VitaminB12ug_fg_21:Avg_Prop_VitaminB12ug_fg_38, Avg_Prop_VitaminB12ug_fg_41, Avg_Prop_VitaminB12ug_fg_43, Avg_Prop_VitaminB12ug_fg_44,
              Avg_Prop_VitaminB12ug_fg_50,Avg_Prop_VitaminB12ug_fg_51, Avg_Prop_VitaminB12ug_fg_53, Avg_Prop_VitaminB12ug_fg_57, 
              Avg_Prop_VitaminB12ug_fg_60:Avg_Prop_VitaminB12ug_fg_63, Avg_Prop_VitaminB12ug_fg_65)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()




##Vitamin C ####

#age/sex

table.VitC.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_VitaminCmg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_VitaminCmg_fg_2 ~ "White bread",
            Avg_Prop_VitaminCmg_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_VitaminCmg_fg_4 ~ "Other breads",
            Avg_Prop_VitaminCmg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_VitaminCmg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_VitaminCmg_fg_7 ~ "Biscuits",
            Avg_Prop_VitaminCmg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_VitaminCmg_fg_9 ~ "Puddings",
            Avg_Prop_VitaminCmg_fg_10 ~ "Whole milk",
            Avg_Prop_VitaminCmg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_VitaminCmg_fg_12 ~ "Skimmed milk",
            Avg_Prop_VitaminCmg_fg_13 ~ "Other milk and cream",
            Avg_Prop_VitaminCmg_fg_14 ~ "Cheese",
            Avg_Prop_VitaminCmg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_VitaminCmg_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_VitaminCmg_fg_17 ~ "Butter",
            #  Avg_Prop_VitaminCmg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_VitaminCmg_fg_19 ~ "Low fat spread",
            #  Avg_Prop_VitaminCmg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_VitaminCmg_fg_21 ~ "Reduced fat spread",
            # Avg_Prop_VitaminCmg_fg_22 ~ "Bacon and ham",
            Avg_Prop_VitaminCmg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_VitaminCmg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_VitaminCmg_fg_25 ~ "Pork and dishes",
            Avg_Prop_VitaminCmg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_VitaminCmg_fg_27 ~ "Chicken and turkey dishes",
            # Avg_Prop_VitaminCmg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_VitaminCmg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_VitaminCmg_fg_30 ~ "Sausages",
            Avg_Prop_VitaminCmg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_VitaminCmg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_VitaminCmg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_VitaminCmg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            #  Avg_Prop_VitaminCmg_fg_35 ~ "Oily fish",
            Avg_Prop_VitaminCmg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_VitaminCmg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_VitaminCmg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_VitaminCmg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_VitaminCmg_fg_40 ~ "Fruit",
            Avg_Prop_VitaminCmg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_VitaminCmg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_VitaminCmg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_VitaminCmg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_VitaminCmg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_VitaminCmg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_VitaminCmg_fg_48 ~ "Wine",
            #  Avg_Prop_VitaminCmg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_VitaminCmg_fg_50 ~ "Miscellaneous",
            Avg_Prop_VitaminCmg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_VitaminCmg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_VitaminCmg_fg_53 ~ "Ice cream",
            #  Avg_Prop_VitaminCmg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_VitaminCmg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_VitaminCmg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_VitaminCmg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_VitaminCmg_fg_58 ~ "Soft drinks, diet",
            #  Avg_Prop_VitaminCmg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_VitaminCmg_fg_60 ~ "1% Milk",
            Avg_Prop_VitaminCmg_fg_62 ~ "Sandwiches",
            Avg_Prop_VitaminCmg_fg_63 ~ "Other milk and cream DF"                     
            #  Avg_Prop_VitaminCmg_fg_64 ~ "Cheese DF",
            #  Avg_Prop_VitaminCmg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            #  Avg_Prop_VitaminCmg_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_VitaminCmg_fg_1:Avg_Prop_VitaminCmg_fg_3, Avg_Prop_VitaminCmg_fg_5:Avg_Prop_VitaminCmg_fg_16,
              Avg_Prop_VitaminCmg_fg_21, Avg_Prop_VitaminCmg_fg_23:Avg_Prop_VitaminCmg_fg_27, Avg_Prop_VitaminCmg_fg_29:Avg_Prop_VitaminCmg_fg_34,
              Avg_Prop_VitaminCmg_fg_36:Avg_Prop_VitaminCmg_fg_45, Avg_Prop_VitaminCmg_fg_50:Avg_Prop_VitaminCmg_fg_53,
              Avg_Prop_VitaminCmg_fg_56:Avg_Prop_VitaminCmg_fg_58, Avg_Prop_VitaminCmg_fg_60:Avg_Prop_VitaminCmg_fg_63)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



#SIMD

table.VitC.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_VitaminCmg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_VitaminCmg_fg_2 ~ "White bread",
            Avg_Prop_VitaminCmg_fg_3 ~ "Wholemeal bread",
            # Avg_Prop_VitaminCmg_fg_4 ~ "Other breads",
            Avg_Prop_VitaminCmg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_VitaminCmg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_VitaminCmg_fg_7 ~ "Biscuits",
            Avg_Prop_VitaminCmg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_VitaminCmg_fg_9 ~ "Puddings",
            Avg_Prop_VitaminCmg_fg_10 ~ "Whole milk",
            Avg_Prop_VitaminCmg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_VitaminCmg_fg_12 ~ "Skimmed milk",
            Avg_Prop_VitaminCmg_fg_13 ~ "Other milk and cream",
            Avg_Prop_VitaminCmg_fg_14 ~ "Cheese",
            Avg_Prop_VitaminCmg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_VitaminCmg_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_VitaminCmg_fg_17 ~ "Butter",
            #  Avg_Prop_VitaminCmg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_VitaminCmg_fg_19 ~ "Low fat spread",
            #  Avg_Prop_VitaminCmg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_VitaminCmg_fg_21 ~ "Reduced fat spread",
            # Avg_Prop_VitaminCmg_fg_22 ~ "Bacon and ham",
            Avg_Prop_VitaminCmg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_VitaminCmg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_VitaminCmg_fg_25 ~ "Pork and dishes",
            Avg_Prop_VitaminCmg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_VitaminCmg_fg_27 ~ "Chicken and turkey dishes",
            # Avg_Prop_VitaminCmg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_VitaminCmg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_VitaminCmg_fg_30 ~ "Sausages",
            Avg_Prop_VitaminCmg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_VitaminCmg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_VitaminCmg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_VitaminCmg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            #  Avg_Prop_VitaminCmg_fg_35 ~ "Oily fish",
            Avg_Prop_VitaminCmg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_VitaminCmg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_VitaminCmg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_VitaminCmg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_VitaminCmg_fg_40 ~ "Fruit",
            Avg_Prop_VitaminCmg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_VitaminCmg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_VitaminCmg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_VitaminCmg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_VitaminCmg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_VitaminCmg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_VitaminCmg_fg_48 ~ "Wine",
            #  Avg_Prop_VitaminCmg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_VitaminCmg_fg_50 ~ "Miscellaneous",
            Avg_Prop_VitaminCmg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_VitaminCmg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_VitaminCmg_fg_53 ~ "Ice cream",
            #  Avg_Prop_VitaminCmg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_VitaminCmg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_VitaminCmg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_VitaminCmg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_VitaminCmg_fg_58 ~ "Soft drinks, diet",
            #  Avg_Prop_VitaminCmg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_VitaminCmg_fg_60 ~ "1% Milk",
            Avg_Prop_VitaminCmg_fg_62 ~ "Sandwiches",
            Avg_Prop_VitaminCmg_fg_63 ~ "Other milk and cream DF"                     
            #  Avg_Prop_VitaminCmg_fg_64 ~ "Cheese DF",
            #  Avg_Prop_VitaminCmg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            #  Avg_Prop_VitaminCmg_fg_66 ~ "Ice cream DF"
  ),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_VitaminCmg_fg_1:Avg_Prop_VitaminCmg_fg_3, Avg_Prop_VitaminCmg_fg_5:Avg_Prop_VitaminCmg_fg_16,
              Avg_Prop_VitaminCmg_fg_21, Avg_Prop_VitaminCmg_fg_23:Avg_Prop_VitaminCmg_fg_27, Avg_Prop_VitaminCmg_fg_29:Avg_Prop_VitaminCmg_fg_34,
              Avg_Prop_VitaminCmg_fg_36:Avg_Prop_VitaminCmg_fg_45, Avg_Prop_VitaminCmg_fg_50:Avg_Prop_VitaminCmg_fg_53,
              Avg_Prop_VitaminCmg_fg_56:Avg_Prop_VitaminCmg_fg_58, Avg_Prop_VitaminCmg_fg_60:Avg_Prop_VitaminCmg_fg_63)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


##Iron ####


table.iron.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_Ironmg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Ironmg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Ironmg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Ironmg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Ironmg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Ironmg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Ironmg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Ironmg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Ironmg_foodcat_9 ~ "Fruit",
    Avg_Prop_Ironmg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Ironmg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Ironmg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Ironmg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Ironmg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Ironmg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Ironmg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Ironmg_foodcat_1:Avg_Prop_Ironmg_foodcat_13,Avg_Prop_Ironmg_foodcat_15,Avg_Prop_Ironmg_foodcat_16,Avg_Prop_Ironmg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#age/sex

table.iron.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Ironmg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Ironmg_fg_2 ~ "White bread",
            Avg_Prop_Ironmg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Ironmg_fg_4 ~ "Other breads",
            Avg_Prop_Ironmg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Ironmg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Ironmg_fg_7 ~ "Biscuits",
            Avg_Prop_Ironmg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Ironmg_fg_9 ~ "Puddings",
            Avg_Prop_Ironmg_fg_10 ~ "Whole milk",
            # Avg_Prop_Ironmg_fg_11 ~ "Semi-skimmed milk",
            # Avg_Prop_Ironmg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Ironmg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Ironmg_fg_14 ~ "Cheese",
            Avg_Prop_Ironmg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Ironmg_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_Ironmg_fg_17 ~ "Butter",
            # Avg_Prop_Ironmg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_Ironmg_fg_19 ~ "Low fat spread",
            Avg_Prop_Ironmg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Ironmg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Ironmg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Ironmg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Ironmg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Ironmg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Ironmg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Ironmg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Ironmg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Ironmg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Ironmg_fg_30 ~ "Sausages",
            Avg_Prop_Ironmg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Ironmg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Ironmg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Ironmg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Ironmg_fg_35 ~ "Oily fish",
            Avg_Prop_Ironmg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Ironmg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Ironmg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Ironmg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Ironmg_fg_40 ~ "Fruit",
            Avg_Prop_Ironmg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Ironmg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Ironmg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Ironmg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Ironmg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Ironmg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Ironmg_fg_48 ~ "Wine",
            #  Avg_Prop_Ironmg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Ironmg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Ironmg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Ironmg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Ironmg_fg_53 ~ "Ice cream",
            #  Avg_Prop_Ironmg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Ironmg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Ironmg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Ironmg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Ironmg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Ironmg_fg_59 ~ "Brown, granary and wheatgerm bread",
            #  Avg_Prop_Ironmg_fg_60 ~ "1% Milk",
            Avg_Prop_Ironmg_fg_62 ~ "Sandwiches",
            Avg_Prop_Ironmg_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_Ironmg_fg_64 ~ "Cheese DF",
            Avg_Prop_Ironmg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Ironmg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Ironmg_fg_1:Avg_Prop_Ironmg_fg_10, Avg_Prop_Ironmg_fg_13:Avg_Prop_Ironmg_fg_16,
              Avg_Prop_Ironmg_fg_20:Avg_Prop_Ironmg_fg_45, Avg_Prop_Ironmg_fg_50:Avg_Prop_Ironmg_fg_53,
              Avg_Prop_Ironmg_fg_56:Avg_Prop_Ironmg_fg_59, Avg_Prop_Ironmg_fg_62, Avg_Prop_Ironmg_fg_63, 
              Avg_Prop_Ironmg_fg_65, Avg_Prop_Ironmg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.iron.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Ironmg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Ironmg_fg_2 ~ "White bread",
            Avg_Prop_Ironmg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Ironmg_fg_4 ~ "Other breads",
            Avg_Prop_Ironmg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Ironmg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Ironmg_fg_7 ~ "Biscuits",
            Avg_Prop_Ironmg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Ironmg_fg_9 ~ "Puddings",
            Avg_Prop_Ironmg_fg_10 ~ "Whole milk",
            # Avg_Prop_Ironmg_fg_11 ~ "Semi-skimmed milk",
            # Avg_Prop_Ironmg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Ironmg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Ironmg_fg_14 ~ "Cheese",
            Avg_Prop_Ironmg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Ironmg_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_Ironmg_fg_17 ~ "Butter",
            # Avg_Prop_Ironmg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_Ironmg_fg_19 ~ "Low fat spread",
            Avg_Prop_Ironmg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Ironmg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Ironmg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Ironmg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Ironmg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Ironmg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Ironmg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Ironmg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Ironmg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Ironmg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Ironmg_fg_30 ~ "Sausages",
            Avg_Prop_Ironmg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Ironmg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Ironmg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Ironmg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Ironmg_fg_35 ~ "Oily fish",
            Avg_Prop_Ironmg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Ironmg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Ironmg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Ironmg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Ironmg_fg_40 ~ "Fruit",
            Avg_Prop_Ironmg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Ironmg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Ironmg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Ironmg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Ironmg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Ironmg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Ironmg_fg_48 ~ "Wine",
            #  Avg_Prop_Ironmg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Ironmg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Ironmg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Ironmg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Ironmg_fg_53 ~ "Ice cream",
            #  Avg_Prop_Ironmg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Ironmg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Ironmg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Ironmg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Ironmg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Ironmg_fg_59 ~ "Brown, granary and wheatgerm bread",
            #  Avg_Prop_Ironmg_fg_60 ~ "1% Milk",
            Avg_Prop_Ironmg_fg_62 ~ "Sandwiches",
            Avg_Prop_Ironmg_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_Ironmg_fg_64 ~ "Cheese DF",
            Avg_Prop_Ironmg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Ironmg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Ironmg_fg_1:Avg_Prop_Ironmg_fg_10, Avg_Prop_Ironmg_fg_13:Avg_Prop_Ironmg_fg_16,
              Avg_Prop_Ironmg_fg_20:Avg_Prop_Ironmg_fg_45, Avg_Prop_Ironmg_fg_50:Avg_Prop_Ironmg_fg_53,
              Avg_Prop_Ironmg_fg_56:Avg_Prop_Ironmg_fg_59, Avg_Prop_Ironmg_fg_62, Avg_Prop_Ironmg_fg_63, 
              Avg_Prop_Ironmg_fg_65, Avg_Prop_Ironmg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Calcium ####

#age/sex

table.calcium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Calciummg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Calciummg_fg_2 ~ "White bread",
            Avg_Prop_Calciummg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Calciummg_fg_4 ~ "Other breads",
            Avg_Prop_Calciummg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Calciummg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Calciummg_fg_7 ~ "Biscuits",
            Avg_Prop_Calciummg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Calciummg_fg_9 ~ "Puddings",
            Avg_Prop_Calciummg_fg_10 ~ "Whole milk",
            Avg_Prop_Calciummg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Calciummg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Calciummg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Calciummg_fg_14 ~ "Cheese",
            Avg_Prop_Calciummg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Calciummg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Calciummg_fg_17 ~ "Butter",
            #   Avg_Prop_Calciummg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Calciummg_fg_19 ~ "Low fat spread",
            #  Avg_Prop_Calciummg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Calciummg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Calciummg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Calciummg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Calciummg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Calciummg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Calciummg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Calciummg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Calciummg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Calciummg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Calciummg_fg_30 ~ "Sausages",
            Avg_Prop_Calciummg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Calciummg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Calciummg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Calciummg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Calciummg_fg_35 ~ "Oily fish",
            Avg_Prop_Calciummg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Calciummg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Calciummg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Calciummg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Calciummg_fg_40 ~ "Fruit",
            Avg_Prop_Calciummg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Calciummg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Calciummg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Calciummg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Calciummg_fg_45 ~ "Fruit juice",
            #   Avg_Prop_Calciummg_fg_47 ~ "Spirits and liqueurs",
            #   Avg_Prop_Calciummg_fg_48 ~ "Wine",
            #  Avg_Prop_Calciummg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Calciummg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Calciummg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Calciummg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Calciummg_fg_53 ~ "Ice cream",
            #   Avg_Prop_Calciummg_fg_54 ~ "Dietary supplements",
            #   Avg_Prop_Calciummg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Calciummg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Calciummg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Calciummg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Calciummg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Calciummg_fg_60 ~ "1% Milk",
            Avg_Prop_Calciummg_fg_62 ~ "Sandwiches",
            Avg_Prop_Calciummg_fg_63 ~ "Other milk and cream DF",                         
            #   Avg_Prop_Calciummg_fg_64 ~ "Cheese DF",
            Avg_Prop_Calciummg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Calciummg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Calciummg_fg_1:Avg_Prop_Calciummg_fg_17, Avg_Prop_Calciummg_fg_19,
              Avg_Prop_Calciummg_fg_21:Avg_Prop_Calciummg_fg_45, Avg_Prop_Calciummg_fg_50:Avg_Prop_Calciummg_fg_53,
              Avg_Prop_Calciummg_fg_56:Avg_Prop_Calciummg_fg_63, Avg_Prop_Calciummg_fg_65, Avg_Prop_Calciummg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.calcium.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Calciummg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Calciummg_fg_2 ~ "White bread",
            Avg_Prop_Calciummg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Calciummg_fg_4 ~ "Other breads",
            Avg_Prop_Calciummg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Calciummg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Calciummg_fg_7 ~ "Biscuits",
            Avg_Prop_Calciummg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Calciummg_fg_9 ~ "Puddings",
            Avg_Prop_Calciummg_fg_10 ~ "Whole milk",
            Avg_Prop_Calciummg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Calciummg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Calciummg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Calciummg_fg_14 ~ "Cheese",
            Avg_Prop_Calciummg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Calciummg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Calciummg_fg_17 ~ "Butter",
            #   Avg_Prop_Calciummg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Calciummg_fg_19 ~ "Low fat spread",
            #  Avg_Prop_Calciummg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Calciummg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Calciummg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Calciummg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Calciummg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Calciummg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Calciummg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Calciummg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Calciummg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Calciummg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Calciummg_fg_30 ~ "Sausages",
            Avg_Prop_Calciummg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Calciummg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Calciummg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Calciummg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Calciummg_fg_35 ~ "Oily fish",
            Avg_Prop_Calciummg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Calciummg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Calciummg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Calciummg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Calciummg_fg_40 ~ "Fruit",
            Avg_Prop_Calciummg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Calciummg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Calciummg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Calciummg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Calciummg_fg_45 ~ "Fruit juice",
            #   Avg_Prop_Calciummg_fg_47 ~ "Spirits and liqueurs",
            #   Avg_Prop_Calciummg_fg_48 ~ "Wine",
            #  Avg_Prop_Calciummg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Calciummg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Calciummg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Calciummg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Calciummg_fg_53 ~ "Ice cream",
            #   Avg_Prop_Calciummg_fg_54 ~ "Dietary supplements",
            #   Avg_Prop_Calciummg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Calciummg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Calciummg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Calciummg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Calciummg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Calciummg_fg_60 ~ "1% Milk",
            Avg_Prop_Calciummg_fg_62 ~ "Sandwiches",
            Avg_Prop_Calciummg_fg_63 ~ "Other milk and cream DF",                         
            #   Avg_Prop_Calciummg_fg_64 ~ "Cheese DF",
            Avg_Prop_Calciummg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Calciummg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Calciummg_fg_1:Avg_Prop_Calciummg_fg_17, Avg_Prop_Calciummg_fg_19,
              Avg_Prop_Calciummg_fg_21:Avg_Prop_Calciummg_fg_45, Avg_Prop_Calciummg_fg_50:Avg_Prop_Calciummg_fg_53,
              Avg_Prop_Calciummg_fg_56:Avg_Prop_Calciummg_fg_63, Avg_Prop_Calciummg_fg_65, Avg_Prop_Calciummg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Sodium ####

table.sodium.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_Sodiummg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Sodiummg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Sodiummg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Sodiummg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Sodiummg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Sodiummg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Sodiummg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Sodiummg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Sodiummg_foodcat_9 ~ "Fruit",
    Avg_Prop_Sodiummg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Sodiummg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Sodiummg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Sodiummg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Sodiummg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Sodiummg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Sodiummg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Sodiummg_foodcat_1:Avg_Prop_Sodiummg_foodcat_13,Avg_Prop_Sodiummg_foodcat_15,Avg_Prop_Sodiummg_foodcat_16,Avg_Prop_Sodiummg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#age/sex

table.sodium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Sodiummg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Sodiummg_fg_2 ~ "White bread",
            Avg_Prop_Sodiummg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Sodiummg_fg_4 ~ "Other breads",
            Avg_Prop_Sodiummg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Sodiummg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Sodiummg_fg_7 ~ "Biscuits",
            Avg_Prop_Sodiummg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Sodiummg_fg_9 ~ "Puddings",
            Avg_Prop_Sodiummg_fg_10 ~ "Whole milk",
            Avg_Prop_Sodiummg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Sodiummg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Sodiummg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Sodiummg_fg_14 ~ "Cheese",
            Avg_Prop_Sodiummg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Sodiummg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Sodiummg_fg_17 ~ "Butter",
            #  Avg_Prop_Sodiummg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Sodiummg_fg_19 ~ "Low fat spread",
            #   Avg_Prop_Sodiummg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Sodiummg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Sodiummg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Sodiummg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Sodiummg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Sodiummg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Sodiummg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Sodiummg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Sodiummg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Sodiummg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Sodiummg_fg_30 ~ "Sausages",
            Avg_Prop_Sodiummg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Sodiummg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Sodiummg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Sodiummg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Sodiummg_fg_35 ~ "Oily fish",
            Avg_Prop_Sodiummg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Sodiummg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Sodiummg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Sodiummg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Sodiummg_fg_40 ~ "Fruit",
            Avg_Prop_Sodiummg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Sodiummg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Sodiummg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Sodiummg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Sodiummg_fg_45 ~ "Fruit juice",
            # Avg_Prop_Sodiummg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Sodiummg_fg_48 ~ "Wine",
            # Avg_Prop_Sodiummg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Sodiummg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Sodiummg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Sodiummg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Sodiummg_fg_53 ~ "Ice cream",
            # Avg_Prop_Sodiummg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Sodiummg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Sodiummg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Sodiummg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Sodiummg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Sodiummg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Sodiummg_fg_60 ~ "1% Milk",
            Avg_Prop_Sodiummg_fg_62 ~ "Sandwiches",
            Avg_Prop_Sodiummg_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_Sodiummg_fg_64 ~ "Cheese DF",
            Avg_Prop_Sodiummg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Sodiummg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Sodiummg_fg_1:Avg_Prop_Sodiummg_fg_17, Avg_Prop_Sodiummg_fg_19,
              Avg_Prop_Sodiummg_fg_21:Avg_Prop_Sodiummg_fg_45, Avg_Prop_Sodiummg_fg_50:Avg_Prop_Sodiummg_fg_53,
              Avg_Prop_Sodiummg_fg_56:Avg_Prop_Sodiummg_fg_63, Avg_Prop_Sodiummg_fg_65, Avg_Prop_Sodiummg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD
table.sodium.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Sodiummg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Sodiummg_fg_2 ~ "White bread",
            Avg_Prop_Sodiummg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Sodiummg_fg_4 ~ "Other breads",
            Avg_Prop_Sodiummg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Sodiummg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Sodiummg_fg_7 ~ "Biscuits",
            Avg_Prop_Sodiummg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Sodiummg_fg_9 ~ "Puddings",
            Avg_Prop_Sodiummg_fg_10 ~ "Whole milk",
            Avg_Prop_Sodiummg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Sodiummg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Sodiummg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Sodiummg_fg_14 ~ "Cheese",
            Avg_Prop_Sodiummg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Sodiummg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Sodiummg_fg_17 ~ "Butter",
            #  Avg_Prop_Sodiummg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Sodiummg_fg_19 ~ "Low fat spread",
            #   Avg_Prop_Sodiummg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Sodiummg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Sodiummg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Sodiummg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Sodiummg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Sodiummg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Sodiummg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Sodiummg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Sodiummg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Sodiummg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Sodiummg_fg_30 ~ "Sausages",
            Avg_Prop_Sodiummg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Sodiummg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Sodiummg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Sodiummg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Sodiummg_fg_35 ~ "Oily fish",
            Avg_Prop_Sodiummg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Sodiummg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Sodiummg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Sodiummg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Sodiummg_fg_40 ~ "Fruit",
            Avg_Prop_Sodiummg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Sodiummg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Sodiummg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Sodiummg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Sodiummg_fg_45 ~ "Fruit juice",
            # Avg_Prop_Sodiummg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Sodiummg_fg_48 ~ "Wine",
            # Avg_Prop_Sodiummg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Sodiummg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Sodiummg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Sodiummg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Sodiummg_fg_53 ~ "Ice cream",
            # Avg_Prop_Sodiummg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Sodiummg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Sodiummg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Sodiummg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Sodiummg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Sodiummg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Sodiummg_fg_60 ~ "1% Milk",
            Avg_Prop_Sodiummg_fg_62 ~ "Sandwiches",
            Avg_Prop_Sodiummg_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_Sodiummg_fg_64 ~ "Cheese DF",
            Avg_Prop_Sodiummg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Sodiummg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Sodiummg_fg_1:Avg_Prop_Sodiummg_fg_17, Avg_Prop_Sodiummg_fg_19,
              Avg_Prop_Sodiummg_fg_21:Avg_Prop_Sodiummg_fg_45, Avg_Prop_Sodiummg_fg_50:Avg_Prop_Sodiummg_fg_53,
              Avg_Prop_Sodiummg_fg_56:Avg_Prop_Sodiummg_fg_63, Avg_Prop_Sodiummg_fg_65, Avg_Prop_Sodiummg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Magnesium ####

table.magnesium.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_Magnesiummg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Magnesiummg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Magnesiummg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Magnesiummg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Magnesiummg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Magnesiummg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Magnesiummg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Magnesiummg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Magnesiummg_foodcat_9 ~ "Fruit",
    Avg_Prop_Magnesiummg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Magnesiummg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Magnesiummg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Magnesiummg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Magnesiummg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Magnesiummg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Magnesiummg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Magnesiummg_foodcat_1:Avg_Prop_Magnesiummg_foodcat_13,Avg_Prop_Magnesiummg_foodcat_15,Avg_Prop_Magnesiummg_foodcat_16,Avg_Prop_Magnesiummg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#age/sex

table.magnesium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Magnesiummg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Magnesiummg_fg_2 ~ "White bread",
            Avg_Prop_Magnesiummg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Magnesiummg_fg_4 ~ "Other breads",
            Avg_Prop_Magnesiummg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Magnesiummg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Magnesiummg_fg_7 ~ "Biscuits",
            Avg_Prop_Magnesiummg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Magnesiummg_fg_9 ~ "Puddings",
            Avg_Prop_Magnesiummg_fg_10 ~ "Whole milk",
            Avg_Prop_Magnesiummg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Magnesiummg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Magnesiummg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Magnesiummg_fg_14 ~ "Cheese",
            Avg_Prop_Magnesiummg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Magnesiummg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Magnesiummg_fg_17 ~ "Butter",
            # Avg_Prop_Magnesiummg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Magnesiummg_fg_19 ~ "Low fat spread",
            # Avg_Prop_Magnesiummg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Magnesiummg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Magnesiummg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Magnesiummg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Magnesiummg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Magnesiummg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Magnesiummg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Magnesiummg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Magnesiummg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Magnesiummg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Magnesiummg_fg_30 ~ "Sausages",
            Avg_Prop_Magnesiummg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Magnesiummg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Magnesiummg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Magnesiummg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Magnesiummg_fg_35 ~ "Oily fish",
            Avg_Prop_Magnesiummg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Magnesiummg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Magnesiummg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Magnesiummg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Magnesiummg_fg_40 ~ "Fruit",
            Avg_Prop_Magnesiummg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Magnesiummg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Magnesiummg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Magnesiummg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Magnesiummg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Magnesiummg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Magnesiummg_fg_48 ~ "Wine",
            #  Avg_Prop_Magnesiummg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Magnesiummg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Magnesiummg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Magnesiummg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Magnesiummg_fg_53 ~ "Ice cream",
            #  Avg_Prop_Magnesiummg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Magnesiummg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Magnesiummg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Magnesiummg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Magnesiummg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Magnesiummg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Magnesiummg_fg_60 ~ "1% Milk",
            Avg_Prop_Magnesiummg_fg_62 ~ "Sandwiches",
            Avg_Prop_Magnesiummg_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_Magnesiummg_fg_64 ~ "Cheese DF",
            Avg_Prop_Magnesiummg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Magnesiummg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Magnesiummg_fg_1:Avg_Prop_Magnesiummg_fg_17, Avg_Prop_Magnesiummg_fg_19,
              Avg_Prop_Magnesiummg_fg_21:Avg_Prop_Magnesiummg_fg_45, Avg_Prop_Magnesiummg_fg_50:Avg_Prop_Magnesiummg_fg_53,
              Avg_Prop_Magnesiummg_fg_56:Avg_Prop_Magnesiummg_fg_63, Avg_Prop_Magnesiummg_fg_65,Avg_Prop_Magnesiummg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



#SIMD

table.magnesium.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Magnesiummg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Magnesiummg_fg_2 ~ "White bread",
            Avg_Prop_Magnesiummg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Magnesiummg_fg_4 ~ "Other breads",
            Avg_Prop_Magnesiummg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Magnesiummg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Magnesiummg_fg_7 ~ "Biscuits",
            Avg_Prop_Magnesiummg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Magnesiummg_fg_9 ~ "Puddings",
            Avg_Prop_Magnesiummg_fg_10 ~ "Whole milk",
            Avg_Prop_Magnesiummg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Magnesiummg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Magnesiummg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Magnesiummg_fg_14 ~ "Cheese",
            Avg_Prop_Magnesiummg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Magnesiummg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Magnesiummg_fg_17 ~ "Butter",
            # Avg_Prop_Magnesiummg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Magnesiummg_fg_19 ~ "Low fat spread",
            # Avg_Prop_Magnesiummg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Magnesiummg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Magnesiummg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Magnesiummg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Magnesiummg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Magnesiummg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Magnesiummg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Magnesiummg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Magnesiummg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Magnesiummg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Magnesiummg_fg_30 ~ "Sausages",
            Avg_Prop_Magnesiummg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Magnesiummg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Magnesiummg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Magnesiummg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Magnesiummg_fg_35 ~ "Oily fish",
            Avg_Prop_Magnesiummg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Magnesiummg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Magnesiummg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Magnesiummg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Magnesiummg_fg_40 ~ "Fruit",
            Avg_Prop_Magnesiummg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Magnesiummg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Magnesiummg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Magnesiummg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Magnesiummg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Magnesiummg_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Magnesiummg_fg_48 ~ "Wine",
            #  Avg_Prop_Magnesiummg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Magnesiummg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Magnesiummg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Magnesiummg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Magnesiummg_fg_53 ~ "Ice cream",
            #  Avg_Prop_Magnesiummg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Magnesiummg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Magnesiummg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Magnesiummg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Magnesiummg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Magnesiummg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Magnesiummg_fg_60 ~ "1% Milk",
            Avg_Prop_Magnesiummg_fg_62 ~ "Sandwiches",
            Avg_Prop_Magnesiummg_fg_63 ~ "Other milk and cream DF",                         
            #  Avg_Prop_Magnesiummg_fg_64 ~ "Cheese DF",
            Avg_Prop_Magnesiummg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Magnesiummg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Magnesiummg_fg_1:Avg_Prop_Magnesiummg_fg_17, Avg_Prop_Magnesiummg_fg_19,
              Avg_Prop_Magnesiummg_fg_21:Avg_Prop_Magnesiummg_fg_45, Avg_Prop_Magnesiummg_fg_50:Avg_Prop_Magnesiummg_fg_53,
              Avg_Prop_Magnesiummg_fg_56:Avg_Prop_Magnesiummg_fg_63, Avg_Prop_Magnesiummg_fg_65,Avg_Prop_Magnesiummg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Potassium ####


table.potassium.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_Potassiummg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Potassiummg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Potassiummg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Potassiummg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Potassiummg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Potassiummg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Potassiummg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Potassiummg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Potassiummg_foodcat_9 ~ "Fruit",
    Avg_Prop_Potassiummg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Potassiummg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Potassiummg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Potassiummg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Potassiummg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Potassiummg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Potassiummg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Potassiummg_foodcat_1:Avg_Prop_Potassiummg_foodcat_13,Avg_Prop_Potassiummg_foodcat_15,Avg_Prop_Potassiummg_foodcat_16,Avg_Prop_Potassiummg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#age/sex

table.potassium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Potassiummg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Potassiummg_fg_2 ~ "White bread",
            Avg_Prop_Potassiummg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Potassiummg_fg_4 ~ "Other breads",
            Avg_Prop_Potassiummg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Potassiummg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Potassiummg_fg_7 ~ "Biscuits",
            Avg_Prop_Potassiummg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Potassiummg_fg_9 ~ "Puddings",
            Avg_Prop_Potassiummg_fg_10 ~ "Whole milk",
            Avg_Prop_Potassiummg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Potassiummg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Potassiummg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Potassiummg_fg_14 ~ "Cheese",
            Avg_Prop_Potassiummg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Potassiummg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Potassiummg_fg_17 ~ "Butter",
            #   Avg_Prop_Potassiummg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Potassiummg_fg_19 ~ "Low fat spread",
            #  Avg_Prop_Potassiummg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Potassiummg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Potassiummg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Potassiummg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Potassiummg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Potassiummg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Potassiummg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Potassiummg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Potassiummg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Potassiummg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Potassiummg_fg_30 ~ "Sausages",
            Avg_Prop_Potassiummg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Potassiummg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Potassiummg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Potassiummg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Potassiummg_fg_35 ~ "Oily fish",
            Avg_Prop_Potassiummg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Potassiummg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Potassiummg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Potassiummg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Potassiummg_fg_40 ~ "Fruit",
            Avg_Prop_Potassiummg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Potassiummg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Potassiummg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Potassiummg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Potassiummg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Potassiummg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Potassiummg_fg_48 ~ "Wine",
            # Avg_Prop_Potassiummg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Potassiummg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Potassiummg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Potassiummg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Potassiummg_fg_53 ~ "Ice cream",
            #  Avg_Prop_Potassiummg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Potassiummg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Potassiummg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Potassiummg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Potassiummg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Potassiummg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Potassiummg_fg_60 ~ "1% Milk",
            Avg_Prop_Potassiummg_fg_62 ~ "Sandwiches",
            Avg_Prop_Potassiummg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Potassiummg_fg_64 ~ "Cheese DF",
            Avg_Prop_Potassiummg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Potassiummg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Potassiummg_fg_1:Avg_Prop_Potassiummg_fg_17, Avg_Prop_Potassiummg_fg_19,
              Avg_Prop_Potassiummg_fg_21:Avg_Prop_Potassiummg_fg_45, Avg_Prop_Potassiummg_fg_50:Avg_Prop_Potassiummg_fg_53,
              Avg_Prop_Potassiummg_fg_56:Avg_Prop_Potassiummg_fg_63,Avg_Prop_Potassiummg_fg_65,Avg_Prop_Potassiummg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



#SIMD

table.potassium.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Potassiummg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Potassiummg_fg_2 ~ "White bread",
            Avg_Prop_Potassiummg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Potassiummg_fg_4 ~ "Other breads",
            Avg_Prop_Potassiummg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Potassiummg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Potassiummg_fg_7 ~ "Biscuits",
            Avg_Prop_Potassiummg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Potassiummg_fg_9 ~ "Puddings",
            Avg_Prop_Potassiummg_fg_10 ~ "Whole milk",
            Avg_Prop_Potassiummg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Potassiummg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Potassiummg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Potassiummg_fg_14 ~ "Cheese",
            Avg_Prop_Potassiummg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Potassiummg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Potassiummg_fg_17 ~ "Butter",
            #   Avg_Prop_Potassiummg_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Potassiummg_fg_19 ~ "Low fat spread",
            #  Avg_Prop_Potassiummg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Potassiummg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Potassiummg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Potassiummg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Potassiummg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Potassiummg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Potassiummg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Potassiummg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Potassiummg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Potassiummg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Potassiummg_fg_30 ~ "Sausages",
            Avg_Prop_Potassiummg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Potassiummg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Potassiummg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Potassiummg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Potassiummg_fg_35 ~ "Oily fish",
            Avg_Prop_Potassiummg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Potassiummg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Potassiummg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Potassiummg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Potassiummg_fg_40 ~ "Fruit",
            Avg_Prop_Potassiummg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Potassiummg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Potassiummg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Potassiummg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Potassiummg_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Potassiummg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Potassiummg_fg_48 ~ "Wine",
            # Avg_Prop_Potassiummg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Potassiummg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Potassiummg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Potassiummg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Potassiummg_fg_53 ~ "Ice cream",
            #  Avg_Prop_Potassiummg_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Potassiummg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Potassiummg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Potassiummg_fg_57 ~ "Soft drinks, not diet",
            Avg_Prop_Potassiummg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Potassiummg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Potassiummg_fg_60 ~ "1% Milk",
            Avg_Prop_Potassiummg_fg_62 ~ "Sandwiches",
            Avg_Prop_Potassiummg_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Potassiummg_fg_64 ~ "Cheese DF",
            Avg_Prop_Potassiummg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Potassiummg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Potassiummg_fg_1:Avg_Prop_Potassiummg_fg_17, Avg_Prop_Potassiummg_fg_19,
              Avg_Prop_Potassiummg_fg_21:Avg_Prop_Potassiummg_fg_45, Avg_Prop_Potassiummg_fg_50:Avg_Prop_Potassiummg_fg_53,
              Avg_Prop_Potassiummg_fg_56:Avg_Prop_Potassiummg_fg_63,Avg_Prop_Potassiummg_fg_65,Avg_Prop_Potassiummg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


##Iodine ####

table.iodine.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_Iodineug_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Iodineug_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Iodineug_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Iodineug_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Iodineug_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Iodineug_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Iodineug_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Iodineug_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Iodineug_foodcat_9 ~ "Fruit",
    Avg_Prop_Iodineug_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Iodineug_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Iodineug_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Iodineug_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Iodineug_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Iodineug_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Iodineug_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Iodineug_foodcat_1:Avg_Prop_Iodineug_foodcat_13,Avg_Prop_Iodineug_foodcat_15,Avg_Prop_Iodineug_foodcat_16,Avg_Prop_Iodineug_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#age/sex

table.iodine.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Iodineug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Iodineug_fg_2 ~ "White bread",
            Avg_Prop_Iodineug_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Iodineug_fg_4 ~ "Other breads",
            Avg_Prop_Iodineug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Iodineug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Iodineug_fg_7 ~ "Biscuits",
            Avg_Prop_Iodineug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Iodineug_fg_9 ~ "Puddings",
            Avg_Prop_Iodineug_fg_10 ~ "Whole milk",
            Avg_Prop_Iodineug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Iodineug_fg_12 ~ "Skimmed milk",
            Avg_Prop_Iodineug_fg_13 ~ "Other milk and cream",
            Avg_Prop_Iodineug_fg_14 ~ "Cheese",
            Avg_Prop_Iodineug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Iodineug_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Iodineug_fg_17 ~ "Butter",
            # Avg_Prop_Iodineug_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Iodineug_fg_19 ~ "Low fat spread",
            #Avg_Prop_Iodineug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Iodineug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Iodineug_fg_22 ~ "Bacon and ham",
            Avg_Prop_Iodineug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Iodineug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Iodineug_fg_25 ~ "Pork and dishes",
            Avg_Prop_Iodineug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Iodineug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Iodineug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Iodineug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Iodineug_fg_30 ~ "Sausages",
            Avg_Prop_Iodineug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Iodineug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Iodineug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Iodineug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Iodineug_fg_35 ~ "Oily fish",
            Avg_Prop_Iodineug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Iodineug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Iodineug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Iodineug_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Iodineug_fg_40 ~ "Fruit",
            Avg_Prop_Iodineug_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Iodineug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Iodineug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Iodineug_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Iodineug_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Iodineug_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Iodineug_fg_48 ~ "Wine",
            #  Avg_Prop_Iodineug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Iodineug_fg_50 ~ "Miscellaneous",
            Avg_Prop_Iodineug_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Iodineug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Iodineug_fg_53 ~ "Ice cream",
            # Avg_Prop_Iodineug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Iodineug_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Iodineug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Iodineug_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_Iodineug_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Iodineug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Iodineug_fg_60 ~ "1% Milk",
            Avg_Prop_Iodineug_fg_62 ~ "Sandwiches",
            Avg_Prop_Iodineug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Iodineug_fg_64 ~ "Cheese DF",
            Avg_Prop_Iodineug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Iodineug_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Iodineug_fg_1:Avg_Prop_Iodineug_fg_17, Avg_Prop_Iodineug_fg_19, 
              Avg_Prop_Iodineug_fg_21:Avg_Prop_Iodineug_fg_45,Avg_Prop_Iodineug_fg_50:Avg_Prop_Iodineug_fg_53,
              Avg_Prop_Iodineug_fg_56,Avg_Prop_Iodineug_fg_57, Avg_Prop_Iodineug_fg_59:Avg_Prop_Iodineug_fg_63,
              Avg_Prop_Iodineug_fg_65,Avg_Prop_Iodineug_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



#SIMD

table.iodine.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Iodineug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Iodineug_fg_2 ~ "White bread",
            Avg_Prop_Iodineug_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Iodineug_fg_4 ~ "Other breads",
            Avg_Prop_Iodineug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Iodineug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Iodineug_fg_7 ~ "Biscuits",
            Avg_Prop_Iodineug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Iodineug_fg_9 ~ "Puddings",
            Avg_Prop_Iodineug_fg_10 ~ "Whole milk",
            Avg_Prop_Iodineug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Iodineug_fg_12 ~ "Skimmed milk",
            Avg_Prop_Iodineug_fg_13 ~ "Other milk and cream",
            Avg_Prop_Iodineug_fg_14 ~ "Cheese",
            Avg_Prop_Iodineug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Iodineug_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Iodineug_fg_17 ~ "Butter",
            # Avg_Prop_Iodineug_fg_18 ~ "Polyunsaturated margarine and oils",
            Avg_Prop_Iodineug_fg_19 ~ "Low fat spread",
            #Avg_Prop_Iodineug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Iodineug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Iodineug_fg_22 ~ "Bacon and ham",
            Avg_Prop_Iodineug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Iodineug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Iodineug_fg_25 ~ "Pork and dishes",
            Avg_Prop_Iodineug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Iodineug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Iodineug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Iodineug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Iodineug_fg_30 ~ "Sausages",
            Avg_Prop_Iodineug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Iodineug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Iodineug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Iodineug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Iodineug_fg_35 ~ "Oily fish",
            Avg_Prop_Iodineug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Iodineug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Iodineug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Iodineug_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Iodineug_fg_40 ~ "Fruit",
            Avg_Prop_Iodineug_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Iodineug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Iodineug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Iodineug_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Iodineug_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Iodineug_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Iodineug_fg_48 ~ "Wine",
            #  Avg_Prop_Iodineug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Iodineug_fg_50 ~ "Miscellaneous",
            Avg_Prop_Iodineug_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Iodineug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Iodineug_fg_53 ~ "Ice cream",
            # Avg_Prop_Iodineug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Iodineug_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Iodineug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Iodineug_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_Iodineug_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Iodineug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Iodineug_fg_60 ~ "1% Milk",
            Avg_Prop_Iodineug_fg_62 ~ "Sandwiches",
            Avg_Prop_Iodineug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Iodineug_fg_64 ~ "Cheese DF",
            Avg_Prop_Iodineug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Iodineug_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Iodineug_fg_1:Avg_Prop_Iodineug_fg_17, Avg_Prop_Iodineug_fg_19, 
              Avg_Prop_Iodineug_fg_21:Avg_Prop_Iodineug_fg_45,Avg_Prop_Iodineug_fg_50:Avg_Prop_Iodineug_fg_53,
              Avg_Prop_Iodineug_fg_56,Avg_Prop_Iodineug_fg_57, Avg_Prop_Iodineug_fg_59:Avg_Prop_Iodineug_fg_63,
              Avg_Prop_Iodineug_fg_65,Avg_Prop_Iodineug_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Selenium ####

table.selenium.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_Seleniumug_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Seleniumug_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Seleniumug_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Seleniumug_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Seleniumug_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Seleniumug_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Seleniumug_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Seleniumug_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Seleniumug_foodcat_9 ~ "Fruit",
    Avg_Prop_Seleniumug_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Seleniumug_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Seleniumug_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Seleniumug_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Seleniumug_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Seleniumug_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Seleniumug_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Seleniumug_foodcat_1:Avg_Prop_Seleniumug_foodcat_13,Avg_Prop_Seleniumug_foodcat_15,Avg_Prop_Seleniumug_foodcat_16,Avg_Prop_Seleniumug_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#age/sex

table.selenium.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Seleniumug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Seleniumug_fg_2 ~ "White bread",
            Avg_Prop_Seleniumug_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Seleniumug_fg_4 ~ "Other breads",
            Avg_Prop_Seleniumug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Seleniumug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Seleniumug_fg_7 ~ "Biscuits",
            Avg_Prop_Seleniumug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Seleniumug_fg_9 ~ "Puddings",
            Avg_Prop_Seleniumug_fg_10 ~ "Whole milk",
            Avg_Prop_Seleniumug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Seleniumug_fg_12 ~ "Skimmed milk",
            Avg_Prop_Seleniumug_fg_13 ~ "Other milk and cream",
            Avg_Prop_Seleniumug_fg_14 ~ "Cheese",
            Avg_Prop_Seleniumug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Seleniumug_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_Seleniumug_fg_17 ~ "Butter",
            # Avg_Prop_Seleniumug_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_Seleniumug_fg_19 ~ "Low fat spread",
            # Avg_Prop_Seleniumug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Seleniumug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Seleniumug_fg_22 ~ "Bacon and ham",
            Avg_Prop_Seleniumug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Seleniumug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Seleniumug_fg_25 ~ "Pork and dishes",
            Avg_Prop_Seleniumug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Seleniumug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Seleniumug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Seleniumug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Seleniumug_fg_30 ~ "Sausages",
            Avg_Prop_Seleniumug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Seleniumug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Seleniumug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Seleniumug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Seleniumug_fg_35 ~ "Oily fish",
            Avg_Prop_Seleniumug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Seleniumug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Seleniumug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Seleniumug_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Seleniumug_fg_40 ~ "Fruit",
            Avg_Prop_Seleniumug_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Seleniumug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Seleniumug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Seleniumug_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Seleniumug_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Seleniumug_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Seleniumug_fg_48 ~ "Wine",
            #  Avg_Prop_Seleniumug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Seleniumug_fg_50 ~ "Miscellaneous",
            Avg_Prop_Seleniumug_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Seleniumug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Seleniumug_fg_53 ~ "Ice cream",
            #  Avg_Prop_Seleniumug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Seleniumug_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Seleniumug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Seleniumug_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_Seleniumug_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Seleniumug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Seleniumug_fg_60 ~ "1% Milk",
            Avg_Prop_Seleniumug_fg_62 ~ "Sandwiches",
            Avg_Prop_Seleniumug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Seleniumug_fg_64 ~ "Cheese DF",
            Avg_Prop_Seleniumug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Seleniumug_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Seleniumug_fg_1:Avg_Prop_Seleniumug_fg_16,Avg_Prop_Seleniumug_fg_21:Avg_Prop_Seleniumug_fg_45,
              Avg_Prop_Seleniumug_fg_50:Avg_Prop_Seleniumug_fg_53, Avg_Prop_Seleniumug_fg_56,Avg_Prop_Seleniumug_fg_57,
              Avg_Prop_Seleniumug_fg_59:Avg_Prop_Seleniumug_fg_63, Avg_Prop_Seleniumug_fg_65, Avg_Prop_Seleniumug_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



#SIMD

table.selenium.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Seleniumug_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Seleniumug_fg_2 ~ "White bread",
            Avg_Prop_Seleniumug_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Seleniumug_fg_4 ~ "Other breads",
            Avg_Prop_Seleniumug_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Seleniumug_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Seleniumug_fg_7 ~ "Biscuits",
            Avg_Prop_Seleniumug_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Seleniumug_fg_9 ~ "Puddings",
            Avg_Prop_Seleniumug_fg_10 ~ "Whole milk",
            Avg_Prop_Seleniumug_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Seleniumug_fg_12 ~ "Skimmed milk",
            Avg_Prop_Seleniumug_fg_13 ~ "Other milk and cream",
            Avg_Prop_Seleniumug_fg_14 ~ "Cheese",
            Avg_Prop_Seleniumug_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Seleniumug_fg_16 ~ "Eggs and egg dishes",
            # Avg_Prop_Seleniumug_fg_17 ~ "Butter",
            # Avg_Prop_Seleniumug_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_Seleniumug_fg_19 ~ "Low fat spread",
            # Avg_Prop_Seleniumug_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Seleniumug_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Seleniumug_fg_22 ~ "Bacon and ham",
            Avg_Prop_Seleniumug_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Seleniumug_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Seleniumug_fg_25 ~ "Pork and dishes",
            Avg_Prop_Seleniumug_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Seleniumug_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Seleniumug_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Seleniumug_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Seleniumug_fg_30 ~ "Sausages",
            Avg_Prop_Seleniumug_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Seleniumug_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Seleniumug_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Seleniumug_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Seleniumug_fg_35 ~ "Oily fish",
            Avg_Prop_Seleniumug_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Seleniumug_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Seleniumug_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Seleniumug_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Seleniumug_fg_40 ~ "Fruit",
            Avg_Prop_Seleniumug_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Seleniumug_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Seleniumug_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Seleniumug_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Seleniumug_fg_45 ~ "Fruit juice",
            #  Avg_Prop_Seleniumug_fg_47 ~ "Spirits and liqueurs",
            #  Avg_Prop_Seleniumug_fg_48 ~ "Wine",
            #  Avg_Prop_Seleniumug_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Seleniumug_fg_50 ~ "Miscellaneous",
            Avg_Prop_Seleniumug_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Seleniumug_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Seleniumug_fg_53 ~ "Ice cream",
            #  Avg_Prop_Seleniumug_fg_54 ~ "Dietary supplements",
            #  Avg_Prop_Seleniumug_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Seleniumug_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Seleniumug_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_Seleniumug_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Seleniumug_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Seleniumug_fg_60 ~ "1% Milk",
            Avg_Prop_Seleniumug_fg_62 ~ "Sandwiches",
            Avg_Prop_Seleniumug_fg_63 ~ "Other milk and cream DF",                         
            # Avg_Prop_Seleniumug_fg_64 ~ "Cheese DF",
            Avg_Prop_Seleniumug_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Seleniumug_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Seleniumug_fg_1:Avg_Prop_Seleniumug_fg_16,Avg_Prop_Seleniumug_fg_21:Avg_Prop_Seleniumug_fg_45,
              Avg_Prop_Seleniumug_fg_50:Avg_Prop_Seleniumug_fg_53, Avg_Prop_Seleniumug_fg_56,Avg_Prop_Seleniumug_fg_57,
              Avg_Prop_Seleniumug_fg_59:Avg_Prop_Seleniumug_fg_63, Avg_Prop_Seleniumug_fg_65, Avg_Prop_Seleniumug_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##Zinc ####

table.zinc.foodcat <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = age_sex_cat,
  label = c(
    Avg_Prop_Zincmg_foodcat_1 ~ "Cereals and Cereal Products",
    Avg_Prop_Zincmg_foodcat_2 ~ "Milk and Milk Products",
    Avg_Prop_Zincmg_foodcat_3 ~ "Eggs and Egg Dishes",
    Avg_Prop_Zincmg_foodcat_4 ~ "Fat Spreads",
    Avg_Prop_Zincmg_foodcat_5 ~ "Meat and Meat Products",
    Avg_Prop_Zincmg_foodcat_6 ~ "Fish and Fish Dishes",
    Avg_Prop_Zincmg_foodcat_7 ~ "Sandwiches",
    Avg_Prop_Zincmg_foodcat_8 ~ "Vegetables, potatoes",
    Avg_Prop_Zincmg_foodcat_9 ~ "Fruit",
    Avg_Prop_Zincmg_foodcat_10 ~ "Sugar, Preserves and Confectionery",
    Avg_Prop_Zincmg_foodcat_11 ~ "Savoury Snacks",
    Avg_Prop_Zincmg_foodcat_12 ~ "Nuts and Seeds",
    Avg_Prop_Zincmg_foodcat_13 ~ "Non-alcoholic beverages",
    Avg_Prop_Zincmg_foodcat_15 ~ "Miscellaneous",
    Avg_Prop_Zincmg_foodcat_16 ~ "Toddler foods",
    Avg_Prop_Zincmg_foodcat_18 ~ "Milk and Milk Products (dairy-free)"),
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Zincmg_foodcat_1:Avg_Prop_Zincmg_foodcat_13,Avg_Prop_Zincmg_foodcat_15,Avg_Prop_Zincmg_foodcat_16,Avg_Prop_Zincmg_foodcat_18)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#age/sex

table.zinc.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(Avg_Prop_Zincmg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Zincmg_fg_2 ~ "White bread",
            Avg_Prop_Zincmg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Zincmg_fg_4 ~ "Other breads",
            Avg_Prop_Zincmg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Zincmg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Zincmg_fg_7 ~ "Biscuits",
            Avg_Prop_Zincmg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Zincmg_fg_9 ~ "Puddings",
            Avg_Prop_Zincmg_fg_10 ~ "Whole milk",
            Avg_Prop_Zincmg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Zincmg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Zincmg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Zincmg_fg_14 ~ "Cheese",
            Avg_Prop_Zincmg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Zincmg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Zincmg_fg_17 ~ "Butter",
            #Avg_Prop_Zincmg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_Zincmg_fg_19 ~ "Low fat spread",
            #  Avg_Prop_Zincmg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Zincmg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Zincmg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Zincmg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Zincmg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Zincmg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Zincmg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Zincmg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Zincmg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Zincmg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Zincmg_fg_30 ~ "Sausages",
            Avg_Prop_Zincmg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Zincmg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Zincmg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Zincmg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Zincmg_fg_35 ~ "Oily fish",
            Avg_Prop_Zincmg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Zincmg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Zincmg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Zincmg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Zincmg_fg_40 ~ "Fruit",
            Avg_Prop_Zincmg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Zincmg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Zincmg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Zincmg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Zincmg_fg_45 ~ "Fruit juice",
            # Avg_Prop_Zincmg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Zincmg_fg_48 ~ "Wine",
            # Avg_Prop_Zincmg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Zincmg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Zincmg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Zincmg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Zincmg_fg_53 ~ "Ice cream",
            # Avg_Prop_Zincmg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Zincmg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Zincmg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Zincmg_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_Zincmg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Zincmg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Zincmg_fg_60 ~ "1% Milk",
            Avg_Prop_Zincmg_fg_62 ~ "Sandwiches",
            Avg_Prop_Zincmg_fg_63 ~ "Other milk and cream DF",                         
            #Avg_Prop_Zincmg_fg_64 ~ "Cheese DF",
            Avg_Prop_Zincmg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Zincmg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Zincmg_fg_1:Avg_Prop_Zincmg_fg_17, Avg_Prop_Zincmg_fg_21:Avg_Prop_Zincmg_fg_45, 
              Avg_Prop_Zincmg_fg_50:Avg_Prop_Zincmg_fg_53, Avg_Prop_Zincmg_fg_56, Avg_Prop_Zincmg_fg_57,
              Avg_Prop_Zincmg_fg_59:Avg_Prop_Zincmg_fg_63, Avg_Prop_Zincmg_fg_65,Avg_Prop_Zincmg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#SIMD

table.zinc.fg.simd <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = simd_quintile,
  label = c(Avg_Prop_Zincmg_fg_1 ~ "Pasta, rice and other miscellaneous cereals",
            Avg_Prop_Zincmg_fg_2 ~ "White bread",
            Avg_Prop_Zincmg_fg_3 ~ "Wholemeal bread",
            Avg_Prop_Zincmg_fg_4 ~ "Other breads",
            Avg_Prop_Zincmg_fg_5 ~ "High fibre breakfast cereals",
            Avg_Prop_Zincmg_fg_6 ~ "Other breakfast cereals",
            Avg_Prop_Zincmg_fg_7 ~ "Biscuits",
            Avg_Prop_Zincmg_fg_8 ~ "Buns, cakes, pastries and fruit pies",
            Avg_Prop_Zincmg_fg_9 ~ "Puddings",
            Avg_Prop_Zincmg_fg_10 ~ "Whole milk",
            Avg_Prop_Zincmg_fg_11 ~ "Semi-skimmed milk",
            Avg_Prop_Zincmg_fg_12 ~ "Skimmed milk",
            Avg_Prop_Zincmg_fg_13 ~ "Other milk and cream",
            Avg_Prop_Zincmg_fg_14 ~ "Cheese",
            Avg_Prop_Zincmg_fg_15 ~ "Yogurt, fromage frais and other dairy desserts",
            Avg_Prop_Zincmg_fg_16 ~ "Eggs and egg dishes",
            Avg_Prop_Zincmg_fg_17 ~ "Butter",
            #Avg_Prop_Zincmg_fg_18 ~ "Polyunsaturated margarine and oils",
            # Avg_Prop_Zincmg_fg_19 ~ "Low fat spread",
            #  Avg_Prop_Zincmg_fg_20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
            Avg_Prop_Zincmg_fg_21 ~ "Reduced fat spread",
            Avg_Prop_Zincmg_fg_22 ~ "Bacon and ham",
            Avg_Prop_Zincmg_fg_23 ~ "Beef, veal and dishes",
            Avg_Prop_Zincmg_fg_24 ~ "Lamb and dishes",
            Avg_Prop_Zincmg_fg_25 ~ "Pork and dishes",
            Avg_Prop_Zincmg_fg_26 ~ "Coated chicken and turkey manufactured",
            Avg_Prop_Zincmg_fg_27 ~ "Chicken and turkey dishes",
            Avg_Prop_Zincmg_fg_28 ~ "Liver, products and dishes",
            Avg_Prop_Zincmg_fg_29 ~ "Burgers and kebabs",
            Avg_Prop_Zincmg_fg_30 ~ "Sausages",
            Avg_Prop_Zincmg_fg_31 ~ "Meat pies and pastries",
            Avg_Prop_Zincmg_fg_32 ~ "Other meat and meat products",
            Avg_Prop_Zincmg_fg_33 ~ "White fish coated or fried",
            Avg_Prop_Zincmg_fg_34 ~ "Other white fish, shellfish and fish dishes",
            Avg_Prop_Zincmg_fg_35 ~ "Oily fish",
            Avg_Prop_Zincmg_fg_36 ~ "Salad and other raw vegetables",
            Avg_Prop_Zincmg_fg_37 ~ "Vegetables (not raw)",
            Avg_Prop_Zincmg_fg_38 ~ "Chips, fried and roast potatoes and potato products",
            Avg_Prop_Zincmg_fg_39 ~ "Other potatoes, potato salads and dishes",
            Avg_Prop_Zincmg_fg_40 ~ "Fruit",
            Avg_Prop_Zincmg_fg_41 ~ "Sugars, preserves and sweet spreads",
            Avg_Prop_Zincmg_fg_42 ~ "Crisps and savoury snacks",
            Avg_Prop_Zincmg_fg_43 ~ "Sugar confectionery",
            Avg_Prop_Zincmg_fg_44 ~ "Chocolate confectionery",
            Avg_Prop_Zincmg_fg_45 ~ "Fruit juice",
            # Avg_Prop_Zincmg_fg_47 ~ "Spirits and liqueurs",
            # Avg_Prop_Zincmg_fg_48 ~ "Wine",
            # Avg_Prop_Zincmg_fg_49 ~ "Beer lager cider and perry",
            Avg_Prop_Zincmg_fg_50 ~ "Miscellaneous",
            Avg_Prop_Zincmg_fg_51 ~ "Tea, coffee and water",
            Avg_Prop_Zincmg_fg_52 ~ "Commercial toddlers foods and drinks",
            Avg_Prop_Zincmg_fg_53 ~ "Ice cream",
            # Avg_Prop_Zincmg_fg_54 ~ "Dietary supplements",
            # Avg_Prop_Zincmg_fg_55 ~ "Artificial sweeteners",
            Avg_Prop_Zincmg_fg_56 ~ "Nuts and seeds",
            Avg_Prop_Zincmg_fg_57 ~ "Soft drinks, not diet",
            # Avg_Prop_Zincmg_fg_58 ~ "Soft drinks, diet",
            Avg_Prop_Zincmg_fg_59 ~ "Brown, granary and wheatgerm bread",
            Avg_Prop_Zincmg_fg_60 ~ "1% Milk",
            Avg_Prop_Zincmg_fg_62 ~ "Sandwiches",
            Avg_Prop_Zincmg_fg_63 ~ "Other milk and cream DF",                         
            #Avg_Prop_Zincmg_fg_64 ~ "Cheese DF",
            Avg_Prop_Zincmg_fg_65 ~ "Yogurt, fromage frais and other dairy desserts DF",
            Avg_Prop_Zincmg_fg_66 ~ "Ice cream DF"),
  statistic = list(all_continuous() ~ 
                     "{mean}"),
  missing = "no",
  digits = list(all_continuous() ~ c(1,0)),
  include = c(Avg_Prop_Zincmg_fg_1:Avg_Prop_Zincmg_fg_17, Avg_Prop_Zincmg_fg_21:Avg_Prop_Zincmg_fg_45, 
              Avg_Prop_Zincmg_fg_50:Avg_Prop_Zincmg_fg_53, Avg_Prop_Zincmg_fg_56, Avg_Prop_Zincmg_fg_57,
              Avg_Prop_Zincmg_fg_59:Avg_Prop_Zincmg_fg_63, Avg_Prop_Zincmg_fg_65,Avg_Prop_Zincmg_fg_66)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


###output Excel ####

#create workbook
wb6 <- wb_workbook()

#create lists for tabs
ndns_fg_sheet_names <- c("Energy", "Carbs","Free_Sugars","Fat", "Sat_Fat", 
                         "Trans_Fat","Protein","Fibre","Vit_A","Riboflavin","Folate",
                         "Vit_D","Vit_B12", "Vit_C","Iron","Calcium","Sodium","Magnesium",
                         "Potassium","Iodine" ,"Selenium","Zinc")
ndns_fg_tables <- list(
  Energy = list(age.sex = table.energy.fg, simd = table.energy.fg.simd),
  Carbs = list(age.sex = table.carbs.fg, simd = table.carbs.fg.simd),
  Free_Sugars = list(age.sex = table.FreeSugars.fg, simd = table.FreeSugars.fg.simd),
  Fat = list(age.sex = table.fat.fg, simd = table.fat.fg.simd),
  Sat_Fat = list(age.sex = table.SatFat.fg,simd = table.SatFat.fg.simd),
  Trans_Fat = list(age.sex = table.TransFat.fg, simd = table.TransFat.fg.simd),
  Protein = list(age.sex = table.protein.fg, simd = table.protein.fg.simd),
  Fibre = list(age.sex = table.fibre.fg, simd = table.fibre.fg.simd),
  Vit_A = list(age.sex = table.VitA.fg, simd = table.VitA.fg.simd),
  Riboflavin = list(age.sex = table.riboflavin.fg, simd = table.riboflavin.fg.simd),
  Folate = list(age.sex = table.folate.fg, simd = table.folate.fg.simd),
  Vit_D = list(age.sex = table.VitD.fg, simd = table.VitD.fg.simd),
  Vit_B12 = list(age.sex = table.VitB12.fg, simd = table.VitB12.fg.simd),
  Vit_C = list(age.sex = table.VitC.fg, simd = table.VitC.fg.simd),
  Iron = list(age.sex = table.iron.fg, simd = table.iron.fg.simd),
  Calcium = list(age.sex = table.calcium.fg, simd = table.calcium.fg.simd),
  Sodium = list(age.sex = table.sodium.fg, simd = table.sodium.fg.simd),
  Magnesium = list(age.sex = table.magnesium.fg, simd = table.magnesium.fg.simd),
  Potassium = list(age.sex = table.potassium.fg, simd = table.potassium.fg.simd),
  Iodine = list(age.sex = table.iodine.fg, simd = table.iodine.fg.simd),
  Selenium = list(age.sex = table.selenium.fg, simd = table.selenium.fg.simd),
  Zinc = list(age.sex = table.zinc.fg, simd = table.zinc.fg.simd))

# list with location for excel sheet
dims_list <- c(age.sex = "A1", simd = "L1")

#assign flextables to tabs 
for (i in seq_along(ndns_fg_sheet_names)) {
  sheet_name <- ndns_fg_sheet_names[i]
  wb6 <- wb_add_worksheet(wb6, sheet = sheet_name)
  
  if (sheet_name %in% names(ndns_fg_tables)) {
    tables <- ndns_fg_tables[[sheet_name]]
    
    for (table_type in names(tables)) {
      if (table_type %in% names(dims_list)) {
        wb6 <- wb_add_flextable(wb6, sheet = sheet_name, tables[[table_type]], dims = dims_list[[table_type]])
      }
    }
  }
}


#wb6 <- wb_add_flextable(wb6, sheet = ndns_fg_sheet_names[i], ndns_fg_tables[[i]])

#save workbook
wb_save(wb6, file = paste0("Output/Annexe_Table_3_", format(Sys.time(), "%d%m%Y"), ".xlsx"))


#save text tables of Food Category Level
save_as_docx("Energy"=table.energy.foodcat, "Free_Sugars" = table.freesugars.foodcat, "Fibre" = table.fibre.foodcat, 
             "Sat_Fat"=table.satfat.foodcat, "Sodium"=table.sodium.foodcat,"Iron"=table.iron.foodcat,
             "Magnesium"=table.magnesium.foodcat,"Potassium"=table.potassium.foodcat,"Iodine"=table.iodine.foodcat,
             "Selenium"=table.selenium.foodcat,"Zinc"=table.zinc.foodcat,
             path="Output/Text_NDNS_Food_Categories.docx")


## Toddler milks ####
table(df.intake24_participant$AFreqToddlerMilk)

table.toddlerMilk <- tbl_svysummary(
  svy.df.intake24_participant,
  statistic = list(all_categorical() ~ "{p}% ({n_unweighted})"),
  missing = "ifany",
  include = c(AFreqToddlerMilk)) %>%
  as_flex_table()

save_as_docx("Toddler Milks" = table.toddlerMilk, path="Output/Table_ToddlerMilks.docx")


# Alcohol ####
table.alcohol <- tbl_svysummary(
  svy.df.intake24_participant,
  statistic = list(all_continuous() ~ "{mean}"),
  missing = "ifany",
  include = c(Avg_Alcohol_kcal)) %>%
  as_flex_table()

table.alcohol



#Food Groups - Discretionary - Chpt 7 ####
##Consumers ####

table.consumers <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Consumer_discr_1 ~ "Sweet biscuits",
    Consumer_discr_2 ~ "Cakes, sweet pastries & puddings",
    Consumer_discr_3 ~ "Crisps and savoury snacks",
    Consumer_discr_4 ~ "Confectionery",
    Consumer_discr_5 ~ "Ice creams & ice lollies",
    Consumer_discr_6 ~ "Sugar-containing soft drinks",
    Consumer_discr_7 ~ "Breakfast cerealss",
    Consumer_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Consumer_discr_9 ~ "Pizza",
    Consumer_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Consumer_discr_11 ~ "Ready meals"),
  statistic = list(all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(Consumer_discr_1:Consumer_discr_11)) %>%
  add_overall() %>%
  add_p() %>%
  bold_labels() %>% 
  as_flex_table()

#by simd
table.consumers_simd <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = simd_quintile,
  label = c(
    Consumer_discr_1 ~ "Sweet biscuits",
    Consumer_discr_2 ~ "Cakes, sweet pastries & puddings",
    Consumer_discr_3 ~ "Crisps and savoury snacks",
    Consumer_discr_4 ~ "Confectionery",
    Consumer_discr_5 ~ "Ice creams & ice lollies",
    Consumer_discr_6 ~ "Sugar-containing soft drinks",
    Consumer_discr_7 ~ "Breakfast cerealss",
    Consumer_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Consumer_discr_9 ~ "Pizza",
    Consumer_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Consumer_discr_11 ~ "Ready meals"),
  statistic = list(all_categorical() ~ "{p}%"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(Consumer_discr_1:Consumer_discr_11)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


##Grams ####

#sex
table.disc_grams_sex <- tbl_svysummary(
  svy.df.intake24_participant_fm,
  by = Sex,
  label = c(
    UI_sweetbiscuitsg ~ "Sweet biscuits",
    UI_cakespastriespuddingsg ~ "Cakes, sweet pastries & puddings",
    UI_crispsg ~ "Crisps and savoury snacks",
    UI_confectioneryg ~ "Confectionery",
    UI_icecreamg ~ "Ice creams & ice lollies",
    UI_sugarydrinksg ~ "Sugar-containing soft drinks",
    UI_bfastcerealsg ~ "Breakfast cerealss",
    UI_potatoesg ~ "Roast potatoes, chips and similar roasted potato products",
    UI_pizzag ~ "Pizza",
    UI_dairydessertsg ~ "Yoghurts, fromage frais and dairy desserts",
    UI_readymealsg ~ "Ready meals"
  ),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(UI_sweetbiscuitsg, UI_cakespastriespuddingsg, UI_crispsg, UI_confectioneryg , UI_icecreamg, UI_sugarydrinksg,
              UI_bfastcerealsg, UI_potatoesg, UI_pizzag, UI_dairydessertsg, UI_readymealsg
  )) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

#age
table.disc_grams_age <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = age_cat,
  label = c(
    UI_sweetbiscuitsg ~ "Sweet biscuits",
    UI_cakespastriespuddingsg ~ "Cakes, sweet pastries & puddings",
    UI_crispsg ~ "Crisps and savoury snacks",
    UI_confectioneryg ~ "Confectionery",
    UI_icecreamg ~ "Ice creams & ice lollies",
    UI_sugarydrinksg ~ "Sugar-containing soft drinks",
    UI_bfastcerealsg ~ "Breakfast cerealss",
    UI_potatoesg ~ "Roast potatoes, chips and similar roasted potato products",
    UI_pizzag ~ "Pizza",
    UI_dairydessertsg ~ "Yoghurts, fromage frais and dairy desserts",
    UI_readymealsg ~ "Ready meals"
  ),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(UI_sweetbiscuitsg, UI_cakespastriespuddingsg, UI_crispsg, UI_confectioneryg , UI_icecreamg, UI_sugarydrinksg,
              UI_bfastcerealsg, UI_potatoesg, UI_pizzag, UI_dairydessertsg, UI_readymealsg
  )) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#age/sex
table.disc_grams_age_sex <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    UI_sweetbiscuitsg ~ "Sweet biscuits",
    UI_cakespastriespuddingsg ~ "Cakes, sweet pastries & puddings",
    UI_crispsg ~ "Crisps and savoury snacks",
    UI_confectioneryg ~ "Confectionery",
    UI_icecreamg ~ "Ice creams & ice lollies",
    UI_sugarydrinksg ~ "Sugar-containing soft drinks",
    UI_bfastcerealsg ~ "Breakfast cerealss",
    UI_potatoesg ~ "Roast potatoes, chips and similar roasted potato products",
    UI_pizzag ~ "Pizza",
    UI_dairydessertsg ~ "Yoghurts, fromage frais and dairy desserts",
    UI_readymealsg ~ "Ready meals"
  ),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(UI_sweetbiscuitsg, UI_cakespastriespuddingsg, UI_crispsg, UI_confectioneryg , UI_icecreamg, UI_sugarydrinksg,
              UI_bfastcerealsg, UI_potatoesg, UI_pizzag, UI_dairydessertsg, UI_readymealsg
  )) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


#simd
table.disc_grams_simd <- tbl_svysummary(
  svy.df.intake24_participant, 
  by = simd_quintile,
  label = c(
    UI_sweetbiscuitsg ~ "Sweet biscuits",
    UI_cakespastriespuddingsg ~ "Cakes, sweet pastries & puddings",
    UI_crispsg ~ "Crisps and savoury snacks",
    UI_confectioneryg ~ "Confectionery",
    UI_icecreamg ~ "Ice creams & ice lollies",
    UI_sugarydrinksg ~ "Sugar-containing soft drinks",
    UI_bfastcerealsg ~ "Breakfast cerealss",
    UI_potatoesg ~ "Roast potatoes, chips and similar roasted potato products",
    UI_pizzag ~ "Pizza",
    UI_dairydessertsg ~ "Yoghurts, fromage frais and dairy desserts",
    UI_readymealsg ~ "Ready meals"
  ),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ "{mean}
                                       {median}
                                       {sd}
                                       {p25}
                                       {p75}"),
  missing = "no",
  digits = list(all_categorical() ~ c(0,0)),
  include = c(UI_sweetbiscuitsg, UI_cakespastriespuddingsg, UI_crispsg, UI_confectioneryg , UI_icecreamg, UI_sugarydrinksg,
              UI_bfastcerealsg, UI_potatoesg, UI_pizzag, UI_dairydessertsg, UI_readymealsg
  )) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


###output Excel ####

#GRAMS
#create workbook
wb7 <- wb_workbook()

#create lists for tabs
disc_sheet_names <- c("Grams by Sex", "Grams by Age", "Grams by Age and Sex", "Grams by SIMD")
disc_tables <- list(table.disc_grams_sex, table.disc_grams_age, table.disc_grams_age_sex,
                    table.disc_grams_simd)

#assign flextables to tabs 
for (i in seq_along(disc_sheet_names)) {
  wb7 <- wb_add_worksheet(wb7, sheet = disc_sheet_names[i])
  wb7 <- wb_add_flextable(wb7, sheet = disc_sheet_names[i], disc_tables[[i]])
}

#save workbook
wb_save(wb7, file = paste0("Output/Table_Discretionary_Grams_", format(Sys.time(), "%d%m%Y"), ".xlsx"))

#CONSUMERS
#create workbook
wb7.1 <- wb_workbook()

#create lists for tabs
disc_sheet_names.1 <- c("Consumers by Age and Sex", "Consumers by SIMD")
disc_tables.1 <- list(table.consumers, table.consumers_simd)

#assign flextables to tabs 
for (i in seq_along(disc_sheet_names.1)) {
  wb7.1 <- wb_add_worksheet(wb7.1, sheet = disc_sheet_names.1[i])
  wb7.1 <- wb_add_flextable(wb7.1, sheet = disc_sheet_names.1[i], disc_tables.1[[i]])
}

#save workbook
wb_save(wb7.1, file = paste0("Output/Table_Discretionary_Consumers_", format(Sys.time(), "%d%m%Y"), ".xlsx"))



## Generate Tables ####
##age_sex

# Energy
table.energy.disc.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Energykcal_discr_1 ~ "Sweet biscuits",
    Avg_Prop_Energykcal_discr_2 ~ "Cakes, sweet pastries & puddings",
    Avg_Prop_Energykcal_discr_3 ~ "Crisps and savoury snacks",
    Avg_Prop_Energykcal_discr_4 ~ "Confectionery",
    Avg_Prop_Energykcal_discr_5 ~ "Ice creams & ice lollies",
    Avg_Prop_Energykcal_discr_6 ~ "Sugar-containing soft drinks",
    Avg_Prop_Energykcal_discr_7 ~ "Breakfast cereals",
    Avg_Prop_Energykcal_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Avg_Prop_Energykcal_discr_9 ~ "Pizza",
    Avg_Prop_Energykcal_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Avg_Prop_Energykcal_discr_11 ~ "Ready meals"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Energykcal_discr_1:Avg_Prop_Energykcal_discr_11)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


## Fat
table.fat.disc.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_Fatg_discr_1 ~ "Sweet biscuits",
    Avg_Prop_Fatg_discr_2 ~ "Cakes, sweet pastries & puddings",
    Avg_Prop_Fatg_discr_3 ~ "Crisps and savoury snacks",
    Avg_Prop_Fatg_discr_4 ~ "Confectionery",
    Avg_Prop_Fatg_discr_5 ~ "Ice creams & ice lollies",
    Avg_Prop_Fatg_discr_6 ~ "Sugar-containing soft drinks",
    Avg_Prop_Fatg_discr_7 ~ "Breakfast cereals",
    Avg_Prop_Fatg_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Avg_Prop_Fatg_discr_9 ~ "Pizza",
    Avg_Prop_Fatg_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Avg_Prop_Fatg_discr_11 ~ "Ready meals"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Fatg_discr_1:Avg_Prop_Fatg_discr_11)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

## Sat Fat
table.SatFat.disc.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_SatFatg_discr_1 ~ "Sweet biscuits",
    Avg_Prop_SatFatg_discr_2 ~ "Cakes, sweet pastries & puddings",
    Avg_Prop_SatFatg_discr_3 ~ "Crisps and savoury snacks",
    Avg_Prop_SatFatg_discr_4 ~ "Confectionery",
    Avg_Prop_SatFatg_discr_5 ~ "Ice creams & ice lollies",
    Avg_Prop_SatFatg_discr_6 ~ "Sugar-containing soft drinks",
    Avg_Prop_SatFatg_discr_7 ~ "Breakfast cereals",
    Avg_Prop_SatFatg_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Avg_Prop_SatFatg_discr_9 ~ "Pizza",
    Avg_Prop_SatFatg_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Avg_Prop_SatFatg_discr_11 ~ "Ready meals"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_SatFatg_discr_1:Avg_Prop_SatFatg_discr_11)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

## Free Sugars
table.FreeSugars.disc.fg <- tbl_svysummary(
  svy.df.intake24_participant_fm, #updated with age_sex weights
  by = age_sex_cat,
  label = c(
    Avg_Prop_FreeSugarsg_discr_1 ~ "Sweet biscuits",
    Avg_Prop_FreeSugarsg_discr_2 ~ "Cakes, sweet pastries & puddings",
    Avg_Prop_FreeSugarsg_discr_3 ~ "Crisps and savoury snacks",
    Avg_Prop_FreeSugarsg_discr_4 ~ "Confectionery",
    Avg_Prop_FreeSugarsg_discr_5 ~ "Ice creams & ice lollies",
    Avg_Prop_FreeSugarsg_discr_6 ~ "Sugar-containing soft drinks",
    Avg_Prop_FreeSugarsg_discr_7 ~ "Breakfast cereals",
    Avg_Prop_FreeSugarsg_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Avg_Prop_FreeSugarsg_discr_9 ~ "Pizza",
    Avg_Prop_FreeSugarsg_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Avg_Prop_FreeSugarsg_discr_11 ~ "Ready meals"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_FreeSugarsg_discr_1:Avg_Prop_FreeSugarsg_discr_11)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()



##simd

# Energy
table.energy.disc.fg_simd <- tbl_svysummary(
  svy.df.intake24_participant,
  by = simd_quintile,
  label = c(
    Avg_Prop_Energykcal_discr_1 ~ "Sweet biscuits",
    Avg_Prop_Energykcal_discr_2 ~ "Cakes, sweet pastries & puddings",
    Avg_Prop_Energykcal_discr_3 ~ "Crisps and savoury snacks",
    Avg_Prop_Energykcal_discr_4 ~ "Confectionery",
    Avg_Prop_Energykcal_discr_5 ~ "Ice creams & ice lollies",
    Avg_Prop_Energykcal_discr_6 ~ "Sugar-containing soft drinks",
    Avg_Prop_Energykcal_discr_7 ~ "Breakfast cereals",
    Avg_Prop_Energykcal_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Avg_Prop_Energykcal_discr_9 ~ "Pizza",
    Avg_Prop_Energykcal_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Avg_Prop_Energykcal_discr_11 ~ "Ready meals"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Energykcal_discr_1:Avg_Prop_Energykcal_discr_11)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


## Fat
table.fat.disc.fg_simd <- tbl_svysummary(
  svy.df.intake24_participant, #updated with age_sex weights
  by = simd_quintile,
  label = c(
    Avg_Prop_Fatg_discr_1 ~ "Sweet biscuits",
    Avg_Prop_Fatg_discr_2 ~ "Cakes, sweet pastries & puddings",
    Avg_Prop_Fatg_discr_3 ~ "Crisps and savoury snacks",
    Avg_Prop_Fatg_discr_4 ~ "Confectionery",
    Avg_Prop_Fatg_discr_5 ~ "Ice creams & ice lollies",
    Avg_Prop_Fatg_discr_6 ~ "Sugar-containing soft drinks",
    Avg_Prop_Fatg_discr_7 ~ "Breakfast cereals",
    Avg_Prop_Fatg_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Avg_Prop_Fatg_discr_9 ~ "Pizza",
    Avg_Prop_Fatg_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Avg_Prop_Fatg_discr_11 ~ "Ready meals"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_Fatg_discr_1:Avg_Prop_Fatg_discr_11)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

## Sat Fat
table.SatFat.disc.fg_simd <- tbl_svysummary(
  svy.df.intake24_participant, #updated with age_sex weights
  by = simd_quintile,
  label = c(
    Avg_Prop_SatFatg_discr_1 ~ "Sweet biscuits",
    Avg_Prop_SatFatg_discr_2 ~ "Cakes, sweet pastries & puddings",
    Avg_Prop_SatFatg_discr_3 ~ "Crisps and savoury snacks",
    Avg_Prop_SatFatg_discr_4 ~ "Confectionery",
    Avg_Prop_SatFatg_discr_5 ~ "Ice creams & ice lollies",
    Avg_Prop_SatFatg_discr_6 ~ "Sugar-containing soft drinks",
    Avg_Prop_SatFatg_discr_7 ~ "Breakfast cereals",
    Avg_Prop_SatFatg_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Avg_Prop_SatFatg_discr_9 ~ "Pizza",
    Avg_Prop_SatFatg_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Avg_Prop_SatFatg_discr_11 ~ "Ready meals"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_SatFatg_discr_1:Avg_Prop_SatFatg_discr_11)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()

## Free Sugars
table.FreeSugars.disc.fg_simd <- tbl_svysummary(
  svy.df.intake24_participant, #updated with age_sex weights
  by = simd_quintile,
  label = c(
    Avg_Prop_FreeSugarsg_discr_1 ~ "Sweet biscuits",
    Avg_Prop_FreeSugarsg_discr_2 ~ "Cakes, sweet pastries & puddings",
    Avg_Prop_FreeSugarsg_discr_3 ~ "Crisps and savoury snacks",
    Avg_Prop_FreeSugarsg_discr_4 ~ "Confectionery",
    Avg_Prop_FreeSugarsg_discr_5 ~ "Ice creams & ice lollies",
    Avg_Prop_FreeSugarsg_discr_6 ~ "Sugar-containing soft drinks",
    Avg_Prop_FreeSugarsg_discr_7 ~ "Breakfast cereals",
    Avg_Prop_FreeSugarsg_discr_8 ~ "Roast potatoes, chips and similar roasted potato products",
    Avg_Prop_FreeSugarsg_discr_9 ~ "Pizza",
    Avg_Prop_FreeSugarsg_discr_10 ~ "Yoghurts, fromage frais and dairy desserts",
    Avg_Prop_FreeSugarsg_discr_11 ~ "Ready meals"),
  statistic = list(all_continuous() ~ "{mean}%"),
  missing = "no",
  digits = list(all_continuous() ~ c(0,0)),
  include = c(Avg_Prop_FreeSugarsg_discr_1:Avg_Prop_FreeSugarsg_discr_11)) %>%
  add_overall() %>%
  add_p()%>%
  add_significance_stars()%>%
  bold_labels() %>% 
  as_flex_table()


###output Excel ####

#create workbook
wb8 <- wb_workbook()

#create lists for tabs
disc_fg_sheet_names <- c("Energy_age_sex","Fat_age_sex","Sat_Fat_age_sex", "Free_Sugars_age_sex",
                         "Energy_simd", "Fat_simd", "Sat_Fat_simd", "Free_Sugars_simd")
disc_fg_tables <- list(table.energy.disc.fg, table.fat.disc.fg, table.SatFat.disc.fg, table.FreeSugars.disc.fg,
                       table.energy.disc.fg_simd, table.fat.disc.fg_simd, table.SatFat.disc.fg_simd,
                       table.FreeSugars.disc.fg_simd)

#assign flextables to tabs 
for (i in seq_along(disc_fg_sheet_names)) {
  wb8 <- wb_add_worksheet(wb8, sheet = disc_fg_sheet_names[i])
  wb8 <- wb_add_flextable(wb8, sheet = disc_fg_sheet_names[i], disc_fg_tables[[i]])
}

#save workbook
wb_save(wb8, file = paste0("Output/Table_Discretionary_Foods_Nutrients_", format(Sys.time(), "%d%m%Y"), ".xlsx"))



# Beyond the School Gate ####

table.school <- tbl_svysummary(
  svy.df.survey_fm, # updated for sex weights
  by = Sex,
  label = c(YPFreqOffSchlGrounds ~ "Frequency of consumption beyond the school gate",
            AgreeOSG1 ~ "Because my friends do",
            AgreeOSG2 ~ "Because I can’t get the food I want in school",
            AgreeOSG3 ~ "Because the canteen queue is too long",
            AgreeOSG4 ~ "Because I want to get out of school",
            AgreeOSG5 ~ "Because I don’t like school lunches",
            AgreeOSG6 ~ "Because I don’t like the canteen",
            AgreeOSG7 ~ "Because it’s my right to choose where I go and buy food",
            UseFoodApps ~ "Use food delivery apps",
            FoodAppsFirstOrderAge ~ "Age first used food delivery app",
            FreqFoodApps ~ "Frequency of food delivery app use"),
  statistic = list(all_categorical() ~ "{p}% ({n_unweighted})"),
  digits = list(all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(YPFreqOffSchlGrounds,AgreeOSG1:AgreeOSG7,UseFoodApps,FoodAppsFirstOrderAge,FreqFoodApps)) %>%
  bold_labels() %>% 
  add_overall()%>%
  as_flex_table()

save_as_docx(table.school, path=here("Output/Table_School_Gate.docx"))

# Additional questions
table.demo.secondaryschool <- tbl_svysummary(
  svy.df.survey,
  by = Education,
  label = c(Age ~ "Age, years",
            Sex ~ "Sex",
            simd_quintile ~ "Scottish Index of Multiple Deprivation",
            adults_cat ~ "Number of adults in household",
            children_cat ~ "Number of children in household"),
  statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  missing = "ifany",
  include = c(Age, Sex, Ethnicity, simd_quintile, adults_cat, children_cat)) %>%
  bold_labels() %>%
  as_flex_table()



table.demo.additionalquestions <- tbl_svysummary(
  svy.df.survey,
  by = ChildCompletedFoodDiary,
  label = c(Age ~ "Age, years",
            Sex ~ "Sex",
            simd_quintile ~ "Scottish Index of Multiple Deprivation",
            adults_cat ~ "Number of adults in household",
           children_cat ~ "Number of children in household"),
  statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                   all_categorical() ~ "{p}% ({n_unweighted})"),
  missing = "ifany",
  include = c(Age, Sex, Ethnicity, simd_quintile, adults_cat, children_cat)) %>%
  bold_labels() %>%
  as_flex_table()



save_as_docx("Secondary school" = table.demo.secondaryschool, "Answered additional q" = table.demo.additionalquestions, path="Output/Table_Demographics_AdditionalQuestions.docx")

# Energy Drinks ####
table.energy <- tbl_svysummary(
  svy.df.survey_fm, #updated for sex weights
  by = Sex,
  label = c(FreqEnergyDrinks ~ "Frequency of energy drink consumption",
            ImportanceED1 ~ "The taste",
            ImportanceED2 ~ "To not feel tired or sleepy / For an energy boost",
            ImportanceED3 ~ "To study better",
            ImportanceED4 ~ "Because my friends drink them",
            ImportanceED5 ~ "Because they cost less than other drinks "),
  statistic = list(all_categorical() ~ "{p}% ({n_unweighted})"),
  digits = list(all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(FreqEnergyDrinks,ImportanceED1:ImportanceED5)) %>%
  bold_labels() %>% 
  add_overall()%>%
  as_flex_table()

save_as_docx(table.energy, path="Output/Table_Energy_Drinks.docx")


# Food insecurity - Chpt 9 ####
table.foodinsec <- tbl_svysummary(
  svy.df.survey,
  label = c(HHFOOD1 ~ "Worried about running out of food",
            HHFOOD2 ~ "Ate less",
            HHFOOD3 ~ "Ran out of food",
            HHFOOD4 ~ "At less to leave more for your child / children"),
  statistic = list(all_categorical() ~ "{p}% ({n_unweighted})"),
  digits = list(all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(HHFOOD1:HHFOOD4)) %>%
  bold_labels() %>% 
  as_flex_table()

table.foodinsec.sex <- tbl_svysummary(
  svy.df.survey_fm, 
  by = age_sex_cat,
  label = c(HHFOOD1 ~ "Worried about running out of food",
            HHFOOD2 ~ "Ate less",
            HHFOOD3 ~ "Ran out of food",
            HHFOOD4 ~ "At less to leave more for your child / children"),
  statistic = list(all_categorical() ~ "{p}% ({n_unweighted})"),
  digits = list(all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(age_sex_cat,HHFOOD1:HHFOOD4)) %>%
  bold_labels() %>% 
  add_p()%>%
  as_flex_table()

table.foodinsec.simd <- tbl_svysummary(
  svy.df.survey, 
  by = simd_quintile,
  label = c(HHFOOD1 ~ "Worried about running out of food",
            HHFOOD2 ~ "Ate less",
            HHFOOD3 ~ "Ran out of food",
            HHFOOD4 ~ "At less to leave more for your child / children"),
  statistic = list(all_categorical() ~ "{p}% ({n_unweighted})"),
  digits = list(all_categorical() ~ c(0,0)),
  missing = "no",
  include = c(simd_quintile,HHFOOD1:HHFOOD4)) %>%
  bold_labels() %>% 
  add_p()%>%
  as_flex_table()

save_as_docx("Overall"=table.foodinsec, "By Sex/Age" = table.foodinsec.sex, "By SIMD" = table.foodinsec.simd, path="Output/Table_Food_Insecurity.docx")

# End of code ####
