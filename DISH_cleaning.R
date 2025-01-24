
# DISH Cleaning
# Date: 2024-10-11

# Prepare R Environment ####

#clear R environment
rm(list = ls())

#load required packages
library(dplyr)
library(tidyr)
library(rio)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(flextable)
library(gtsummary)
library(writexl)
library(tibble)
library(readxl)
library(janitor)
library(openxlsx)
library(forcats)

#set working directory
#setwd("C:/Users/ljaacks/OneDrive - University of Edinburgh/Scottish Diets/_National Monitoring of Diet Scotland/3_DISH/Analysis/")



# Import the data ####
df.survey_full <- read_excel("Data/survey_26.08.24.xlsx")
df.intake24_full <- read_excel("Data/intake24_26.08.24.xlsx")

df.full_sample <- read_excel("Data/full_sample_health_boards.xlsx")


# Clean names ####

#set all column names to lower case and replace spaces and special characters with underscores
df.survey <- clean_names(df.survey_full)
df.intake24_item <- clean_names(df.intake24_full)
df.full_sample <- clean_names(df.full_sample)

#remove dataframes that are no longer needed
rm(df.survey_full, df.intake24_full)


## Rename variables - survey ####

#rename simd column to match previous data update files
df.survey <- df.survey %>%
  rename(simd_quintile = simd)

#select only the variables of interest
df.survey <- df.survey %>%
  select(household_id, simd_quintile, la_code, id_case, id_name, id_date, id_start, id_end_date, 
         id_end, id_time, q1, q5, q6, q7, q7a, q8, q9,q10, q11, q12, q13, q14, q15, q16, q17, q18, 
         q19, q20, q21, q24, q33, q34, q42, q43, q43a, q43b, q43c, q43d, q43e, q43f, q44, q45, q46, 
         q47, q48, q49, q49a, q49b, q49c, q49d, q50, q51)

#rename variable columns
df.survey <- df.survey %>%
  rename(HouseholdID = household_id, 
         simd_quintile = simd_quintile,  
         psu = la_code,
         SurveyID = id_case,
         UserID = id_name, 
         StartDate = id_date, 
         StartTime = id_start, 
         FinishDate = id_end_date, 
         FinishTime = id_end, 
         SurveyDuration = id_time, 
         NumberOfChildren = q1, 
         DOB = q5, 
         Sex = q6, 
         Ethnicity = q7, 
         EthnicityOther = q7a, 
         Education = q8, 
         FoodParcels = q9,
         HhldAdults = q10, 
         HhldChildren = q11, 
         HHFOOD1 = q12, 
         HHFOOD2 = q13, 
         HHFOOD3 = q14, 
         HHFOOD4 = q15, 
         AFreqWhiteMeat = q16, 
         AFreqRRPM = q17, 
         AFreqWhiteFish = q18, 
         AFreqOilyFish = q19, 
         AFreqOtherFish = q20, 
         AFreqToddlerMilk = q21, 
         ChildCompletedFoodDiary = q24, 
         FurtherSurveyContact = q33,
         FurtherSurveyContact2 = q34,
         YPFreqOffSchlGrounds = q42, 
         AgreeOSG1 = q43, 
         AgreeOSG2 = q43a, 
         AgreeOSG3 = q43b, 
         AgreeOSG4 = q43c, 
         AgreeOSG5 = q43d, 
         AgreeOSG6 = q43e, 
         AgreeOSG7 = q43f, 
         AgreeOSG_Other = q44, 
         UseFoodApps = q45, 
         FoodAppsFirstOrderAge = q46, 
         FreqFoodApps = q47, 
         FreqEnergyDrinks = q48, 
         ImportanceED1 = q49, 
         ImportanceED2 = q49a, 
         ImportanceED3 = q49b, 
         ImportanceED4 = q49c,
         ImportanceED5 = q49d, 
         ImportanceED_Other = q50, 
         SurveyConsent = q51)

#convert to factor
df.survey <- df.survey %>%
  mutate(across(c(psu, Sex),as.factor))

df.survey <- df.survey %>%
  mutate(psu=fct_recode(psu,"Aberdeen City"="100","Aberdeenshire"="110","Angus"="120","Argyll and Bute"="130",
                        "City of Edinburgh"="230","Clackmannanshire"="150","Dumfries and Galloway"="170",
                        "Dundee City"="180","East Ayrshire"="190","East Dunbartonshire"="200",
                        "East Lothian"="210","East Renfrewshire"="220","Falkirk"="240","Fife"="250",
                        "Glasgow City"="260","Highland"="270","Inverclyde"="280","Midlothian"="290",
                        "Moray"="300","Na h-Eileanan Siar"="235","North Ayrshire"="310",
                        "North Lanarkshire"="320","Orkney Islands"="330","Perth and Kinross"="340",
                        "Renfrewshire"="350","Scottish Borders"="355","Shetland Islands"="360",
                        "South Ayrshire"="370","South Lanarkshire"="380","Stirling"="390",
                        "West Dunbartonshire"="395","West Lothian"="400"))

## Rename variables - intake24 ####

#select only the variables of interest
df.intake24_item <- df.intake24_item %>%
  select(-proxy, -proxy_issues, -serving_image, -leftovers_image, -food_group_local, -brand, -meal_index, 
         -meal_id, -food_index, -nutrient_table_name, -reasonable_amount, -water, -total_nitrogen, 
         -nitrogen_conversion_factor, -non_milk_extrinsic_sugars, -intrinsic_and_milk_sugars, -starch, 
         -englyst_fibre, -glucose, -fructose, -maltose, -lactose, -sucrose, -total_sugars, -other_sugars_uk, 
         -fs_table_sugar, -fs_other_added_sugar, -fs_honey, -fs_fruit_juice, -fs_dried_fruit, -fs_fruit_puree, 
         -fs_stewed_fruit, -fs_vegetable_puree, -cis_mon_fa, -cis_n3_fa, -cis_n6_fa, -cholesterol, -retinol, 
         -total_carotene, -alpha_carotene, -beta_carotene, -beta_cryptoxanthin, -tryptophan_60, -niacin_equivalent, 
         -copper, -chloride, -manganese, -thiamin, -vitamin_e, -niacin, -vitamin_b6, -pantothenic_acid, -biotin, 
         -phosphorus) 

#rename variable columns
df.intake24_item <- df.intake24_item %>%
  rename(SurveyID = survey_id, 
         UserID = user_id, 
         Device = device_information_user_agent,
         StartTime = start_time, 
         FinishTime = submission_time, 
         SurveyDuration = time_to_complete, 
         FoodAmount = food_amount,
         FoodAmountReason = reason_for_unusual_food_amount,
         MealName = meal_name,
         MealTime = meal_time,
         FoodSource = food_source,
         SearchTerm = search_term,
         FoodID = food_id,
         Intake24FoodCode= intake24_food_code,
         Description_English = description_en,
         Description_Local = description_local,
         NutrientTableCode= nutrient_table_code,
         FoodGroupCode = food_group_code,
         FoodGroupEnglish= food_group_en,
         ReadyMeal= ready_meal,
         AsServedWeightFactor= as_served_weight_factor,
         ServingSizeGrams = serving_size_g_ml,
         LeftoversGrams = leftovers_g_ml,
         TotalGrams = portion_size_g_ml,
         MissingFoodID = missing_food_id, 
         MissingFoodDescription = missing_food_description, 
         MissingFoodPortionSize = missing_food_portion_size,  
         MissingFoodLeftovers = missing_food_leftovers,
         SubFoodGroupCode = sub_group_code,
         Energykcal = energy_kcal,
         Energykj= energy_k_j,
         Carbohydrateg = carbohydrate,
         Proteing = protein,
         Fatg = fat,
         Alcoholg = alcohol,
         Saturatedfattyacidsg = satd_fa,
         Transfattyacidsg = trans_fa,
         Freesugarsg = total_fs,
         AOACg = aoac,
         VitaminAug = vitamin_a,
         Riboflavinmg = riboflavin,
         Folateug = folate,
         VitaminDug = vitamin_d,
         VitaminB12ug = vitamin_b12,
         VitaminCmg = vitamin_c,
         Ironmg = iron,
         haemironmg = haem_iron,
         Nonhaemironmg = non_haem_iron,
         Calciummg = calcium,
         Iodineug = iodine,
         Sodiummg = sodium,
         Magnesiummg = magnesium,
         Potassiummg = potassium,
         Seleniumug = selenium,
         Zincmg = zinc,
         Fruitg = fruit,
         DriedFruitg = dried_fruit,
         FruitJuiceg = fruit_juice,
         SmoothieFruitg = smoothie_fruit,
         Tomatoesg = tomatoes,
         TomatoPureeg = tomato_puree,
         Brassicaceaeg = brassicaceae,
         YellowRedGreeng = yellow_red_green,
         Beansg = beans,
         Nutsg = nuts,
         OtherVegg = other_vegetables,
         Beefg = beef,
         Lambg = lamb,
         Porkg = pork,
         ProcessedRedMeatg = processed_red_meat,
         OtherRedMeatg = other_red_meat,
         Burgersg = burgers,
         Sausagesg = sausages,
         Offalg = offal,
         Poultryg = poultry,
         ProcessedPoultryg = processed_poultry,
         GameBirdsg = game_birds,
         WhiteFishg = white_fish,
         OilyFishg = oily_fish,
         CannedTunag = canned_tuna,
         Shellfishg = shellfish,
         CottageCheeseg= cottage_cheese,
         CheddarCheeseg = cheddar_cheese,
         OtherCheeseg = other_cheese) 

## Merge health board with survey ####

# select respondents and health board data
df.health_boards <- df.full_sample %>%
  filter(!is.na(health_board)) %>%
  select(user_id, health_board) %>%
  rename(UserID = user_id)

# join to survey data
df.survey <- left_join(df.survey, df.health_boards, by = "UserID")

#relocate

df.survey <- df.survey %>%
  relocate(health_board, .after = psu)

# Clean Data ####

## Remove ineligibles ####

#calculate the age of the participant. This calculates the interval between two columns, the birth date and the survey completion date and then divides by a function setting the years to 1
df.survey$CalcAge <- interval(df.survey$DOB, df.survey$StartDate) / years(1)
df.survey$Age <- trunc(df.survey$CalcAge)

#relocate column in data frame
df.survey <- df.survey %>%
  relocate(c(CalcAge, Age), .after = DOB)

#check those removed for being outside the age range
age_removed <- df.survey[which(df.survey$CalcAge<2 | df.survey$CalcAge>16),]

#keep only the participants that are between 2-15 years old
df.survey <- df.survey[which(df.survey$CalcAge>=2 & df.survey$CalcAge<16),]

#remove age-ineligible recalls
df.intake24_item <- df.intake24_item %>%
  filter(!UserID %in% c(10007139, 10006439, 10004147)) 

#remove participant with incomplete data
df.intake24_item <- df.intake24_item %>%
  filter(!UserID %in% c(10008296))

df.survey <- df.survey %>%
  filter(!UserID %in% c(10008296))

#remove surveys that have no recalls
participant_ids <- df.intake24_item %>%
  select(UserID) %>%
  distinct()

df.survey <- df.survey %>%
  semi_join(participant_ids, by = "UserID")

rm(age_removed, participant_ids)


## Re-code NA food insecurity responses ####
df.survey <- df.survey %>%
  mutate(HHFOOD2 = case_when(
    is.na(HHFOOD2) ~ "No",
    TRUE ~ HHFOOD2
  ))

df.survey <- df.survey %>%
  mutate(HHFOOD3 = case_when(
    is.na(HHFOOD3) ~ "No",
    TRUE ~ HHFOOD3
  ))

df.survey <- df.survey %>%
  mutate(HHFOOD4 = case_when(
    is.na(HHFOOD4) ~ "No",
    TRUE ~ HHFOOD4
  ))


## Re-code Not sure food parcels responses ####
df.survey <- df.survey %>%
  mutate(FoodParcels = case_when(
    FoodParcels =="Not sure" ~ "No",
    TRUE ~ FoodParcels
  ))


## Categorise 'other' text ####

# ethnicity
subset_Ethnicity_Other <- df.survey %>% 
  filter(!is.na(EthnicityOther))

#code not included to prevent identifiability

# remove dataframes that are no longer needed
rm(subset_Ethnicity_Other)



# Create New Variables ####

## Age groups ####

# report age group
df.survey <- df.survey %>%
  mutate(
    age_cat = case_when(
      CalcAge >= 2 & CalcAge < 5 ~ "2-4y",
      CalcAge >= 5 & CalcAge < 11 ~ "5-10y", 
      CalcAge >= 11 & CalcAge < 16 ~ "11-15y",
      TRUE ~ NA)) %>%
  relocate(age_cat, .after = Age)

# DRVs age group
df.survey <- df.survey %>%
  mutate(
    age_cat_drv = case_when(
      CalcAge >= 2 & CalcAge < 4 ~ "2-3y",
      CalcAge >= 4 & CalcAge < 7 ~ "4-6y",
      CalcAge >= 7 & CalcAge < 11 ~ "7-10y", 
      CalcAge >= 11 & CalcAge < 15 ~ "11-14y",
      CalcAge >= 15 ~ "15y",
      TRUE ~ NA)) %>%
  relocate(age_cat_drv, .after = age_cat)


## Age and sex groups ####

# report age group
df.survey <- df.survey %>%
  mutate(
    age_sex_cat = case_when(
      Sex == "Male" & CalcAge >= 2 & CalcAge < 5 ~ "Male, 2-4y",
      Sex == "Male" & CalcAge >= 5 & CalcAge < 11 ~ "Male, 5-10y", 
      Sex == "Male" & CalcAge >= 11 & CalcAge < 16 ~ "Male, 11-15y",
      Sex == "Female" & CalcAge >= 2 & CalcAge < 5 ~ "Female, 2-4y",
      Sex == "Female" & CalcAge >= 5 & CalcAge < 11 ~ "Female, 5-10y", 
      Sex == "Female" & CalcAge >= 11 & CalcAge < 16 ~ "Female, 11-15y",
      TRUE ~ NA)) %>%
  relocate(age_sex_cat, .after = Sex)

# DRVs age group
df.survey <- df.survey %>%
  mutate(
    age_sex_cat_drv = case_when(
      Sex == "Male" & CalcAge >= 2 & CalcAge < 4 ~ "Male, 2-3y",
      Sex == "Male" & CalcAge >= 4 & CalcAge < 7 ~ "Male, 4-6y",
      Sex == "Male" & CalcAge >= 7 & CalcAge < 11 ~ "Male, 7-10y", 
      Sex == "Male" & CalcAge >= 11 & CalcAge < 15 ~ "Male, 11-14y",
      Sex == "Male" & CalcAge >= 15 ~ "Male, 15y",
      Sex == "Female" & CalcAge >= 2 & CalcAge < 4 ~ "Female, 2-3y",
      Sex == "Female" & CalcAge >= 4 & CalcAge < 7 ~ "Female, 4-6y",
      Sex == "Female" & CalcAge >= 7 & CalcAge < 11 ~ "Female, 7-10y", 
      Sex == "Female" & CalcAge >= 11 & CalcAge < 15 ~ "Female, 11-14y",
      Sex == "Female" & CalcAge >= 15 ~ "Female, 15y",
      TRUE ~ NA)) %>%
  relocate(age_sex_cat_drv, .after = age_cat_drv)


## Number of adults in household group ####
df.survey <- df.survey %>%
  mutate(
    adults_cat = case_when(
      HhldAdults == 1 ~ "1",
      HhldAdults == 2  ~ "2", 
      HhldAdults >= 3 ~ "3 or more",
      TRUE ~ NA)) %>%
  relocate(adults_cat, .after = HhldAdults)


## Number of children in household group ####
df.survey <- df.survey %>%
  mutate(
    children_cat = case_when(
      HhldChildren == 1 ~ "1",
      HhldChildren == 2  ~ "2", 
      HhldChildren >= 3 ~ "3 or more",
      TRUE ~ NA)) %>%
  relocate(children_cat, .after = HhldChildren)


## Recall number and number of recalls ####

# RecallNo

# Separate date from date and time variable
df.intake24_item$date<-lubridate::date(df.intake24_item$'FinishTime')

# Drop duplicate dates for each participant so we have one line per recall date per participant
df.intake24_uniquetime <- df.intake24_item %>%
  distinct(UserID, date, .keep_all = TRUE)

# Index the row numbers to create RecallNo variable
df.intake24_uniquetime <- df.intake24_uniquetime %>%
  group_by(UserID) %>% 
  arrange(UserID, date) %>%
  mutate(RecallNo = row_number())%>%
  ungroup()

df.intake24_uniquetime$RecallNo <- as.numeric(df.intake24_uniquetime$RecallNo)

# NumberOfRecalls

# Participant's max RecallNo is their number of recalls
df.intake24_uniquetime <- df.intake24_uniquetime %>%
  group_by(UserID) %>%
  mutate(NumberOfRecalls = max(RecallNo, na.rm = TRUE)) %>%
  ungroup()

# Pull out newly created variables and join with intake24 dataset
df.intake24_recallvariables <-  df.intake24_uniquetime  %>%
  select(UserID, date, RecallNo, NumberOfRecalls)

df.intake24_item <- left_join(df.intake24_item, df.intake24_recallvariables, by= c("UserID", "date"))

# relocate variables 
df.intake24_item <- df.intake24_item %>%
  relocate(RecallNo, NumberOfRecalls, date, .after = FinishTime)

# remove dataframes that are no longer needed
rm(df.intake24_uniquetime, df.intake24_recallvariables)


## Number of items in each recall ####
df.intake24_item <- df.intake24_item %>%
  group_by(UserID, RecallNo) %>%
  mutate(NumberOfItems = n()) %>%
  ungroup()

df.intake24_item <- df.intake24_item %>%
  relocate(NumberOfItems, .after = NumberOfRecalls)


## Day of the week recall completed ####
df.intake24_item <- df.intake24_item %>%
  mutate(DayofWeek = weekdays(date - 1)) #take one day from the date, as the date value is the day of submission, not date of recall

df.intake24_item <- df.intake24_item %>%
  relocate(DayofWeek, .after = date)

#set factor levels so order is easy to read
df.intake24_item$DayofWeek <- factor(df.intake24_item$DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


## Meal types ####

# update meal time variable from excel format to date and time
df.intake24_item$MealTime <- as.POSIXct(df.intake24_item$MealTime, origin = "1970-01-01", tz = "UTC")

df.intake24_item$MealTime_hour <- hour(df.intake24_item$'MealTime')
df.intake24_item$MealTime_minute <- minute(df.intake24_item$'MealTime')

df.intake24_item <- df.intake24_item %>%
  mutate(NewMealTime = paste0(MealTime_hour,":", MealTime_minute))

df.intake24_item <- df.intake24_item %>%
  relocate(NewMealTime, .after = MealTime)

df.intake24_item$MealTime_hour <- as.numeric(df.intake24_item$MealTime_hour)
df.intake24_item$MealTime_minute <- as.numeric(df.intake24_item$MealTime_minute)

## Month ####
# create month variable
df.intake24_item$Month <- as.POSIXct(df.intake24_item$FinishTime, origin = "1970-01-01", tz = "UTC")

df.intake24_item$Month <- month(df.intake24_item$Month, label = TRUE, abbr = FALSE)

df.intake24_item <- df.intake24_item %>%
  relocate(Month, .after = date)


# re-categorise meals into meal categories. If mealname is not one of Breakfast, Lunch, Dinner, or Snack, rename based on meal time 
df.intake24_item <- df.intake24_item %>%
  mutate(NewMealName = case_when(
    grepl("snack", MealName, ignore.case = TRUE) ~ 'Snack', 
    grepl("evening meal", MealName, ignore.case = TRUE) ~ 'Dinner', 
    grepl("dietary supplements", MealName, ignore.case = TRUE) ~ 'Other',
    MealName %in% c('Breakfast', 'Lunch', 'Dinner', 'Snack') ~ MealName, #keep these categories the same
    between(MealTime_hour, 5,9)  ~ "Breakfast", #re-assign meals to categories based on time
    (MealTime_hour == 12 | (MealTime_hour == 13 & MealTime_minute <= 30)) ~ "Lunch", 
    between(MealTime_hour, 17,19)  ~ "Dinner",
    between(MealTime_hour, 10,11) ~ "Snack",
    ((MealTime_hour == 13 & MealTime_minute > 30) | between(MealTime_hour, 14,16)) ~ "Snack",
    (MealTime_hour >= 20 & MealTime_hour <=23) | (MealTime_hour >= 0 & MealTime_hour <= 4) ~ "Snack",
    TRUE ~ MealName)) 

df.intake24_item <- df.intake24_item %>%
  relocate(NewMealName, .after = MealName) %>%
  select(-MealTime_hour, -MealTime_minute)


## Main Food Groups ####
df.intake24_item <- df.intake24_item %>%
  mutate(MainFoodGroupCode = case_when(
    SubFoodGroupCode %in% c("1C", "1D", "1E", "1F", "1G", "1R") ~ 1,
    SubFoodGroupCode=="2R" ~ 2,
    SubFoodGroupCode=="3R" ~ 3,
    SubFoodGroupCode=="59R" ~ 59,
    SubFoodGroupCode %in% c("4A", "4R") ~ 4,
    SubFoodGroupCode=="5R" ~ 5,
    SubFoodGroupCode=="6R" ~ 6,
    SubFoodGroupCode %in% c("7A", "7B") ~ 7,
    SubFoodGroupCode %in% c("8B", "8C", "8D", "8E") ~ 8,
    SubFoodGroupCode %in% c("9C", "9D", "9E", "9F", "9G", "9H") ~ 9,
    SubFoodGroupCode=="10R" ~ 10,
    SubFoodGroupCode=="11R" ~ 11,
    SubFoodGroupCode=="60R" ~ 60,
    SubFoodGroupCode=="12R" ~ 12,
    SubFoodGroupCode %in% c("13A", "13B", "13R") ~ 13,
    SubFoodGroupCode %in% c("14A", "14B", "14R") ~ 14,
    SubFoodGroupCode %in% c("15B", "15C", "15D") ~ 15,
    SubFoodGroupCode=="53R" ~ 53,
    SubFoodGroupCode %in% c("16C", "16D") ~ 16,
    SubFoodGroupCode=="17R" ~ 17,
    SubFoodGroupCode %in% c("18A", "18B") ~ 18,
    SubFoodGroupCode %in% c("19A", "19R") ~ 19,
    SubFoodGroupCode %in% c("20A", "20B", "20C") ~ 20,
    SubFoodGroupCode %in% c("21A", "21B") ~ 21,
    SubFoodGroupCode %in% c("22A", "22B") ~ 22,
    SubFoodGroupCode %in% c("23A", "23B") ~ 23,
    SubFoodGroupCode %in% c("24A", "24B") ~ 24,
    SubFoodGroupCode %in% c("25A", "25B") ~ 25,
    SubFoodGroupCode %in% c("26A", "26B") ~ 26,
    SubFoodGroupCode %in% c("27A", "27B") ~ 27,
    SubFoodGroupCode=="28R" ~ 28,
    SubFoodGroupCode=="29R" ~ 29,
    SubFoodGroupCode %in% c("30A", "30B") ~ 30,
    SubFoodGroupCode %in% c("31A", "31B") ~ 31,
    SubFoodGroupCode %in% c("32A", "32B") ~ 32,
    SubFoodGroupCode=="33R" ~ 33,
    SubFoodGroupCode %in% c("34C", "34D", "34E", "34F", "34G", "34H") ~ 34,
    SubFoodGroupCode %in% c("35A", "35B") ~ 35,
    SubFoodGroupCode %in% c("36A", "36B", "36C") ~ 36,
    SubFoodGroupCode %in% c("37A", "37B", "37C", "37D", "37E", "37F", "37I", "37K", "37L", "37M") ~ 37,
    SubFoodGroupCode %in% c("38A", "38C", "38D") ~ 38,
    SubFoodGroupCode %in% c("39A", "39B") ~ 39,
    SubFoodGroupCode=="42R" ~ 42,
    SubFoodGroupCode=="56R" ~ 56,
    SubFoodGroupCode %in% c("40A", "40B", "40C", "40D", "40E", "40R") ~ 40,
    SubFoodGroupCode  %in% c("41A", "41B", "41R") ~ 41,
    SubFoodGroupCode=="43R" ~ 43,
    SubFoodGroupCode=="44R" ~ 44,
    SubFoodGroupCode  %in% c("45R", "61R") ~ 45,
    SubFoodGroupCode %in% c("57A", "57B", "57C") ~ 57,
    SubFoodGroupCode %in% c("58A", "58B", "58C") ~ 58,
    SubFoodGroupCode %in% c("51A", "51B", "51C", "51D", "51R") ~ 51,
    SubFoodGroupCode %in% c("47A", "47B") ~ 47,
    SubFoodGroupCode %in% c("48A", "48B", "48C") ~ 48,
    SubFoodGroupCode %in% c("49A", "49B", "49C", "49D", "49E") ~ 49,
    SubFoodGroupCode %in% c("50A", "50C", "50D", "50E", "50R") ~ 50,
    SubFoodGroupCode %in% c("52A", "52R") ~ 52,
    SubFoodGroupCode %in% c("54A", "54B", "54C", "54D", "54E", "54F", "54G", "54H", "54I", "54J", "54K", "54L", "54M", "54N", "54P") ~ 54,
    SubFoodGroupCode=="55R" ~ 55,
    SubFoodGroupCode=="62R" ~ 62,
    TRUE ~ NA)) %>%
  relocate(MainFoodGroupCode, .after = FoodGroupEnglish)

# Create Main Food Group description variable
df.intake24_item <- df.intake24_item %>%
  mutate(MainFoodGroupDesc = case_when(
    MainFoodGroupCode == 1 ~ "Pasta, rice and other miscellaneous cereals",
    MainFoodGroupCode == 2 ~ "White bread",
    MainFoodGroupCode == 3 ~ "Wholemeal bread",
    MainFoodGroupCode == 4 ~ "Other breads",
    MainFoodGroupCode == 5 ~ "High fibre breakfast cereals",
    MainFoodGroupCode == 6 ~ "Other breakfast cereals",
    MainFoodGroupCode == 7 ~ "Biscuits",
    MainFoodGroupCode == 8 ~ "Buns, cakes, pastries and fruit pies",
    MainFoodGroupCode == 9 ~ "Puddings",
    MainFoodGroupCode == 10 ~ "Whole milk",
    MainFoodGroupCode == 11 ~ "Semi-skimmed milk",
    MainFoodGroupCode == 12 ~ "Skimmed milk",
    MainFoodGroupCode == 13 ~ "Other milk and cream",
    MainFoodGroupCode == 14 ~ "Cheese",
    MainFoodGroupCode == 15 ~ "Yogurt, fromage frais and other dairy desserts",
    MainFoodGroupCode == 16 ~ "Eggs and egg dishes",
    MainFoodGroupCode == 17 ~ "Butter",
    MainFoodGroupCode == 18 ~ "Polyunsaturated margarine and oils",
    MainFoodGroupCode == 19 ~ "Low fat spread",
    MainFoodGroupCode == 20 ~ "Margarine and other cooking fats and oils NOT polyunsaturated",
    MainFoodGroupCode == 21 ~ "Reduced fat spread",
    MainFoodGroupCode == 22 ~ "Bacon and ham",
    MainFoodGroupCode == 23 ~ "Beef, veal and dishes",
    MainFoodGroupCode == 24 ~ "Lamb and dishes",
    MainFoodGroupCode == 25 ~ "Pork and dishes",
    MainFoodGroupCode == 26 ~ "Coated chicken and turkey manufactured",
    MainFoodGroupCode == 27 ~ "Chicken and turkey dishes",
    MainFoodGroupCode == 28 ~ "Liver, products and dishes",
    MainFoodGroupCode == 29 ~ "Burgers and kebabs",
    MainFoodGroupCode == 30 ~ "Sausages",
    MainFoodGroupCode == 31 ~ "Meat pies and pastries",
    MainFoodGroupCode == 32 ~ "Other meat and meat products",
    MainFoodGroupCode == 33 ~ "White fish coated or fried",
    MainFoodGroupCode == 34 ~ "Other white fish, shellfish and fish dishes",
    MainFoodGroupCode == 35 ~ "Oily fish",
    MainFoodGroupCode == 36 ~ "Salad and other raw vegetables",
    MainFoodGroupCode == 37 ~ "Vegetables (not raw)",
    MainFoodGroupCode == 38 ~ "Chips, fried and roast potatoes and potato products",
    MainFoodGroupCode == 39 ~ "Other potatoes, potato salads and dishes",
    MainFoodGroupCode == 40 ~ "Fruit",
    MainFoodGroupCode == 41 ~ "Sugars, preserves and sweet spreads",
    MainFoodGroupCode == 42 ~ "Crisps and savoury snacks",
    MainFoodGroupCode == 43 ~ "Sugar confectionery",
    MainFoodGroupCode == 44 ~ "Chocolate confectionery",
    MainFoodGroupCode == 45 ~ "Fruit juice",
    MainFoodGroupCode == 47 ~ "Spirits and liqueurs",
    MainFoodGroupCode == 48 ~ "Wine",
    MainFoodGroupCode == 49 ~ "Beer lager cider and perry",
    MainFoodGroupCode == 50 ~ "Miscellaneous",
    MainFoodGroupCode == 51 ~ "Tea, coffee and water",
    MainFoodGroupCode == 52 ~ "Commercial toddlers foods and drinks",
    MainFoodGroupCode == 53 ~ "Ice cream",
    MainFoodGroupCode == 54 ~ "Dietary supplements",
    MainFoodGroupCode == 55 ~ "Artificial sweeteners",
    MainFoodGroupCode == 56 ~ "Nuts and seeds",
    MainFoodGroupCode == 57 ~ "Soft drinks, not diet",
    MainFoodGroupCode == 58 ~ "Soft drinks, diet",
    MainFoodGroupCode == 59 ~ "Brown, granary and wheatgerm bread",
    MainFoodGroupCode == 60 ~ "1% Milk",
    MainFoodGroupCode == 62 ~ "Sandwiches",
    MainFoodGroupCode == 63 ~ "Other milk and cream DF",                         
    MainFoodGroupCode == 64 ~ "Cheese DF",
    MainFoodGroupCode == 65 ~ "Yogurt, fromage frais and other dairy desserts DF",
    MainFoodGroupCode == 66 ~ "Ice cream DF",
    TRUE ~ NA)) %>%
  relocate(MainFoodGroupDesc, .after = MainFoodGroupCode)

## Device intake24 completed on ####
df.intake24_item <- df.intake24_item %>%
  mutate(Device_group = ifelse(grepl("Android|iPhone", Device), "phone", ifelse(grepl("iPad", Device), "ipad", "computer"))
  )

df.intake24_item <- df.intake24_item %>%
  relocate(Device_group, .after = Device)



# Convert variables to numeric and replace NAs with 0s ####

#convert to numeric from characters

#list of variables to not convert
char_not_to_convert <- c("SurveyID", "UserID", "Device", "Device_group", "StartTime", "FinishTime", "SurveyDuration","Month", "date", "DayofWeek", "FoodAmount", "FoodAmountReason", 
                         "MealName", "NewMealName", "MealTime", "NewMealTime", "FoodSource", "SearchTerm", "Intake24FoodCode", "Description_English", "Description_Local", "FoodGroupCode", 
                         "FoodGroupEnglish", "MainFoodGroupDesc", "AsServedWeightFactor", "SubFoodGroupCode", "ReadyMeal",  "MissingFoodID", "MissingFoodDescription", "MissingFoodPortionSize", "MissingFoodLeftovers")

df.intake24_item <- df.intake24_item %>%
  mutate(across(-all_of(char_not_to_convert), as.numeric))

#change NA values to 0
df.intake24_item <- df.intake24_item %>%
  mutate(across(-all_of(char_not_to_convert), ~ifelse(is.na(.), 0, .)))



# Assign missing food codes ####

#import manually missing food code best-match data
df.missing_foods <- read_excel("Data/Missing_foods_matched.xlsx") %>%
  select(-Description_English, -NutrientTableCode, -Intake24FoodCode, -Description_Local, -NDB_match_description) %>%
  rename(NutrientTableCode = NDB_match_food_code)

#import nutrient databank
df.ndb <- read_excel("Data/UK_NDB_1.2 2024.12.16.xlsx")
df.ndb <- clean_names(df.ndb)



df.ndb <- df.ndb %>%
  rename(
         Description_English = foodname,
         NutrientTableCode= foodnumber)

#select the first food code match and remove duplicate items
df.ndb <- df.ndb %>%
  group_by(NutrientTableCode) %>%
  slice(1)

#merge with matched food codes
df.missing_foods <- left_join(df.missing_foods, df.ndb, by = "NutrientTableCode", relationship = "many-to-many") %>%
  filter(!is.na(Intake24FoodCode)) 

#calculate nutrients based on portion size 
df.missing_foods <- df.missing_foods %>%
  mutate(across(45:142, ~ as.numeric(.)))

df.missing_foods <- df.missing_foods %>%
  mutate(across(45:142, ~ . * (Estimated_Portion_Size_g / 100)))

df.missing_foods <- df.missing_foods %>%
  select(-TotalGrams) %>%
  rename(
    TotalGrams = Estimated_Portion_Size_g,
    SubFoodGroupCode = sub_group_code,
    Energykcal = energy_kcal_kcal,
    Energykj= energy_k_j_k_j,
    Carbohydrateg = carbohydrate_g,
    Proteing = protein_g,
    Fatg = fat_g,
    Alcoholg = alcohol_g,
    Saturatedfattyacidsg = satd_fa_g,
    Transfattyacidsg = trans_fa_g,
    Freesugarsg = total_fs_g,
    AOACg = aoac_g,
    VitaminAug = vitamin_a_g,
    Riboflavinmg = riboflavin_g,
    Folateug = folate_g,
    VitaminDug = vitamin_d_g,
    VitaminB12ug = vitamin_b12_g,
    VitaminCmg = vitamin_c_g,
    Ironmg = iron_g,
    haemironmg = haem_iron_g,
    Nonhaemironmg = non_haem_iron_g,
    Calciummg = calcium_g,
    Iodineug = iodine_g,
    Sodiummg = sodium_g,
    Magnesiummg = magnesium_g,
    Potassiummg = potassium_g,
    Seleniumug = selenium_g,
    Zincmg = zinc_g,
    Fruitg = fruit_g,
    DriedFruitg = dried_fruit_g,
    FruitJuiceg = fruit_juice_g,
    SmoothieFruitg = smoothie_fruit_g,
    Tomatoesg = tomatoes_g,
    TomatoPureeg = tomato_puree_g,
    Brassicaceaeg = brassicaceae_g,
    YellowRedGreeng = yellow_red_green_g,
    Beansg = beans_g,
    Nutsg = nuts_g,
    OtherVegg = other_vegetables_g,
    Beefg = beef_g,
    Lambg = lamb_g,
    Porkg = pork_g,
    ProcessedRedMeatg = processed_red_meat_g,
    OtherRedMeatg = other_red_meat_g,
    Burgersg = burgers_g,
    Sausagesg = sausages_g,
    Offalg = offal_g,
    Poultryg = poultry_g,
    ProcessedPoultryg = processed_poultry_g,
    GameBirdsg = game_birds_g,
    WhiteFishg = white_fish_g,
    OilyFishg = oily_fish_g,
    CannedTunag = canned_tuna_g,
    Shellfishg = shellfish_g,
    CottageCheeseg= cottage_cheese_g,
    CheddarCheeseg = cheddar_cheese_g,
    OtherCheeseg = other_cheese_g)

df.missing_foods <- df.missing_foods %>%
  select(-ReportingFoodGroupCode, -ReportingFoodGroupDesc, -source_locale, -fct, -water_g, -total_nitrogen_g, -nitrogen_conversion_factor_g, -non_milk_extrinsic_sugars_g, -intrinsic_and_milk_sugars_g, -starch_g, -englyst_fibre_g, 
         -glucose_g, -fructose_g, -maltose_g, -lactose_g, -sucrose_g, -total_sugars_g, -other_sugars_uk_g, -fs_table_sugar_g, -fs_other_added_sugar_g, -fs_honey_g, -fs_fruit_juice_g, 
         -fs_dried_fruit_g, -fs_fruit_puree_g, -fs_stewed_fruit_g, -fs_vegetable_puree_g, -cis_mon_fa_g, -cis_n3_fa_g, -cis_n6_fa_g, -cholesterol_g, -retinol_g, -total_carotene_g, 
         -alpha_carotene_g, -beta_carotene_g, -beta_cryptoxanthin_g, -tryptophan_60_g, -niacin_equivalent_g, -copper_g, -chloride_g, -manganese_g, -thiamin_g, -vitamin_e_g, -niacin_g, 
         -vitamin_b6_g, -pantothenic_acid_g, -biotin_g, -phosphorus_g) %>%
  relocate(Intake24FoodCode, Description_English, Description_Local, NutrientTableCode, .after = FoodID) %>%
  relocate(TotalGrams, .after = LeftoversGrams) %>%
  relocate(Proteing, Fatg, Carbohydrateg, .before = Energykcal) %>%
  relocate(AOACg, .before = Saturatedfattyacidsg)

df.missing_foods$MealTime <- as.POSIXct(df.missing_foods$MealTime, origin = "1970-01-01", tz = "UTC")
df.missing_foods$NewMealTime <- as.character(df.missing_foods$NewMealTime)
df.missing_foods$FoodID <- as.numeric(df.missing_foods$FoodID)
df.missing_foods$MainFoodGroupCode <- as.numeric(df.missing_foods$MainFoodGroupCode)
df.missing_foods$ServingSizeGrams <- as.numeric(df.missing_foods$ServingSizeGrams)
df.missing_foods$LeftoversGrams <- as.numeric(df.missing_foods$LeftoversGrams)
df.missing_foods$MissingFoodID <- as.character(df.missing_foods$MissingFoodID)

# merge back to intake24

#first, drop those in intake24_item missing food code
df.missing <- df.intake24_item %>%
  subset(Intake24FoodCode == "MISSING")

df.intake24_item <- df.intake24_item %>%
  subset(Intake24FoodCode != "MISSING")

#then bind
df.intake24_item <- bind_rows(df.intake24_item, df.missing_foods)


# pull out breast milk and supplements 
df.breast_milk_missing <- df.missing %>%
  filter(grepl("breast milk|breastmilk|breastfeed", MissingFoodDescription, ignore.case = TRUE))

df.supplements_missing <- df.missing %>%
  filter(grepl("supplement|multivitamin|vitamin|iron", MissingFoodDescription, ignore.case = TRUE))

rm(df.missing, df.ndb, df.missing_foods)


# Remove diet supplements and breast milk  ####

# Remove diet supplements
df.supplements <- df.intake24_item %>%
  filter(MainFoodGroupCode == "54")

# all supplements reported in sample
df.supplements <- rbind(df.supplements, df.supplements_missing)

df.intake24_item <- df.intake24_item %>%
  filter(MainFoodGroupCode != "54")

# Remove breast milk

df.breast_milk <- df.intake24_item %>%
  filter(Description_English == "Breast milk")

# all breast milk reported in sample
df.breast_milk <- rbind(df.breast_milk, df.breast_milk_missing)


df.intake24_item <- df.intake24_item %>%
  filter(Description_English != "Breast milk") 



# Remove dairy-free items from milk product food groups ####

# Sub food group level
# Other milk
df.intake24_item <- df.intake24_item %>%  
  mutate(SubFoodGroupCode = case_when(
    SubFoodGroupCode == "13R" &
      (str_detect(Description_English, regex("Almond", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Alpro", ignore_case = TRUE)) |
         str_detect(Description_English, regex("soya", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Hemp", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Oat milk", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Rice", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Coconut", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Dairy free", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Hazelnut", ignore_case = TRUE))) ~ "13R_DF",
    SubFoodGroupCode == "13A" &
      (str_detect(Description_English, regex("Soya", ignore_case = TRUE))) ~ "13A_DF",
    TRUE ~ SubFoodGroupCode))

# Cream
df.intake24_item <- df.intake24_item %>% 
  mutate(SubFoodGroupCode = case_when(
    SubFoodGroupCode == "13B" & 
      str_detect(Description_English, regex("Alpro", ignore_case = TRUE)) ~ "13B_DF",
    TRUE ~ SubFoodGroupCode))

# Other cheese
df.intake24_item <- df.intake24_item %>% 
  mutate(SubFoodGroupCode = case_when(
    SubFoodGroupCode == "14R" & 
      (str_detect(Description_English, regex("Tofu", ignore_case = TRUE))|
         str_detect(Description_English, regex("Coconut", ignore_case = TRUE))) ~ "14R_DF",
    TRUE ~ SubFoodGroupCode))

# Yogurt
df.intake24_item <- df.intake24_item %>% 
  mutate(SubFoodGroupCode = case_when(
    SubFoodGroupCode == "15B" & 
      (str_detect(Description_English, regex("Soya", ignore_case = TRUE)) | 
         str_detect(Description_English, regex("Coconut", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Oat-based", ignore_case = TRUE))  ) ~ "15B_DF",
    TRUE ~ SubFoodGroupCode))

# Fromage frais and other dairy desserts 
df.intake24_item <- df.intake24_item %>% 
  mutate(SubFoodGroupCode = case_when(
    SubFoodGroupCode == "15C" & 
      str_detect(Description_English, regex("Soya", ignore_case = TRUE)) ~ "15C_DF",
    TRUE ~ SubFoodGroupCode))

# Ice cream
df.intake24_item <- df.intake24_item %>% 
  mutate(SubFoodGroupCode = case_when(
    SubFoodGroupCode == "53R" &
      str_detect(Description_English, regex("Dairy free", ignore_case = TRUE)) ~ "53R_DF",
    TRUE ~ SubFoodGroupCode))

# Hot chocolate made with water
df.intake24_item <- df.intake24_item %>% 
  mutate(SubFoodGroupCode = case_when(
    str_detect(Description_English, regex("Hot chocolate, made with water", ignore_case=TRUE)) ~ "50A",
    TRUE ~ SubFoodGroupCode))

# Main food group level
df.intake24_item <- df.intake24_item %>% 
  mutate(MainFoodGroupCode = case_when(
    SubFoodGroupCode == "13R_DF" ~ 63,
    SubFoodGroupCode == "13B_DF" ~ 63,
    SubFoodGroupCode == "13A_DF" ~ 63,
    SubFoodGroupCode == "14R_DF" ~ 64,
    SubFoodGroupCode == "15B_DF" ~ 65,
    SubFoodGroupCode == "15C_DF" ~ 65,
    SubFoodGroupCode == "53R_DF" ~ 66,
    SubFoodGroupCode == "50A" ~ 50,
    TRUE ~ MainFoodGroupCode))

# Move milky coffees into 'other milk' ####
df.intake24_item <- df.intake24_item %>% 
  mutate(SubFoodGroupCode = case_when(
    SubFoodGroupCode == "51A" &
      (str_detect(Description_English, regex("Cappuccino", ignore_case = TRUE)) | 
         str_detect(Description_English, regex("Latte", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Flat white", ignore_case = TRUE)) |
         str_detect(Description_English, regex("Mocha", ignore_case = TRUE))) ~ "13R",
    TRUE ~ SubFoodGroupCode))


# Create Discretionary Food Groups ####

#re-categorise discretionary foods 
biscuits_exclude_list <- c("7649","251","252","3973","7650","8330","258","256","255","273","3267","266","267","3236","279","10062","11242","274","7652")
crisps_exclude_list <- c("8033","8155","7876")
confectionary_include_list <- c("8853") #poptarts
confectionery_exclude_list <- c("7762", "2262")
lollies_include_list <- c("7762", "2262")
bfastcereals_exclude_list <- c("8853") #poptarts
potatoes_include_list <- c("5065","11493")
readymeal_include_list <- c("8049","9245","3187","1347","9386","9244","8666","4283","6393","5321","7094","9318","9270","1116","5627","8286","9726","5313", "41") #41 is code for Pasta in tomato sauce, canned, reduced sugar/salt (e.g. Heinz Hoops No Added Sugar)

# Create discretionary food groups 
df.intake24_item <- df.intake24_item %>% 
  mutate(DiscretionaryFoodGroupCode = case_when (
    # Sweet biscuits
    MainFoodGroupCode == 7 & !NutrientTableCode %in% c(biscuits_exclude_list) ~ 1,
    # Cakes, sweet pastries & puddings
    MainFoodGroupCode == 8 | MainFoodGroupCode == 9 ~ 2,
    # Crisps and savoury snacks
    MainFoodGroupCode == 42  & 
      !NutrientTableCode %in% c(crisps_exclude_list) ~ 3,
    # Confectionery
    (MainFoodGroupCode == 43 | MainFoodGroupCode == 44 | NutrientTableCode %in% c(confectionary_include_list))  & !NutrientTableCode %in% c(confectionery_exclude_list) ~ 4,
    # Ice creams & ice lollies
    MainFoodGroupCode == 53 | NutrientTableCode %in% c(lollies_include_list) ~ 5,
    # Sugar-containing soft drinks
    MainFoodGroupCode == 57 ~ 6,
    # Breakfast cereals
    MainFoodGroupCode == 5 | MainFoodGroupCode == 6 ~ 7,
    # Roast potatoes, chips and similar roasted potato products
    MainFoodGroupCode == 38 | NutrientTableCode %in% c(potatoes_include_list) ~ 8,
    # Pizza
    SubFoodGroupCode == "1C" ~ 9,
    # Yoghurts, fromage frais and dairy desserts
    MainFoodGroupCode == 15 ~ 10,
    # Ready meals
    NutrientTableCode %in% c(readymeal_include_list) ~ 11,
    TRUE ~ NA))

# Add description variable
df.intake24_item <- df.intake24_item %>%
  mutate(DiscretionaryFoodGroupDesc = case_when(
    DiscretionaryFoodGroupCode == 1 ~ "Sweet biscuits",
    DiscretionaryFoodGroupCode == 2 ~ "Cakes, sweet pastries & puddings",
    DiscretionaryFoodGroupCode == 3 ~ "Crisps and savoury snacks",
    DiscretionaryFoodGroupCode == 4 ~ "Confectionery",
    DiscretionaryFoodGroupCode == 5 ~ "Ice creams & ice lollies",
    DiscretionaryFoodGroupCode == 6 ~ "Sugar-containing soft drinks",
    DiscretionaryFoodGroupCode == 7 ~ "Breakfast cereals",
    DiscretionaryFoodGroupCode == 8 ~ "Roast potatoes, chips and similar roasted potato products",
    DiscretionaryFoodGroupCode == 9 ~ "Pizza",
    DiscretionaryFoodGroupCode == 10 ~ "Yoghurts, fromage frais and dairy desserts",
    DiscretionaryFoodGroupCode == 11 ~ "Ready meals",
    TRUE ~ "Other")) %>%
  
  relocate(DiscretionaryFoodGroupCode, DiscretionaryFoodGroupDesc, .after = MainFoodGroupDesc)



# Create Food Categories ####
df.intake24_item <- df.intake24_item %>%
  mutate(FoodCatCode = case_when(
    MainFoodGroupCode %in% c(1:9,59) ~ 1,
    MainFoodGroupCode %in% c(10:15,17,53,60) ~ 2,
    MainFoodGroupCode == 16 ~ 3,
    MainFoodGroupCode %in% c(18:21) ~ 4,
    MainFoodGroupCode %in% c(22:32) ~ 5,
    MainFoodGroupCode %in% c(33:35) ~ 6,
    MainFoodGroupCode == 62 ~ 7,
    MainFoodGroupCode %in% c(36:39) ~ 8,
    MainFoodGroupCode == 40 ~ 9,
    MainFoodGroupCode %in% c(41,43,44) ~ 10,
    MainFoodGroupCode == 42 ~ 11,
    MainFoodGroupCode ==56 ~ 12,
    MainFoodGroupCode %in% c(45,51,57,58,61) ~ 13,
    MainFoodGroupCode %in% c(47:49) ~ 14,
    MainFoodGroupCode == 50 ~ 15,
    MainFoodGroupCode == 52 ~ 16,
    MainFoodGroupCode == 55 ~ 17,
    MainFoodGroupCode %in% c(63:66) ~ 18,
    TRUE ~ NA)) %>%
  relocate(FoodCatCode, .after = MainFoodGroupDesc)

# Create Food Category description variable
df.intake24_item <- df.intake24_item %>%
  mutate(FoodCatDesc = case_when(
    FoodCatCode == 1 ~ "Cereals and Cereal Products",
    FoodCatCode == 2 ~ "Milk and Milk Products",
    FoodCatCode == 3 ~ "Eggs and Egg Dishes",
    FoodCatCode == 4 ~ "Fat Spreads",
    FoodCatCode == 5 ~ "Meat and Meat Products",
    FoodCatCode == 6 ~ "Fish and Fish Dishes",
    FoodCatCode == 7 ~ "Sandwiches",
    FoodCatCode == 8 ~ "Vegetables, potatoes",
    FoodCatCode == 9 ~ "Fruit",
    FoodCatCode == 10 ~ "Sugar, Preserves and Confectionery",
    FoodCatCode == 11 ~ "Savoury Snacks",
    FoodCatCode == 12 ~ "Nuts and Seeds",
    FoodCatCode == 13 ~ "Non-alcoholic beverages",
    FoodCatCode == 14 ~ "Alcoholic beverages",
    FoodCatCode == 15 ~ "Miscellaneous",
    FoodCatCode == 16 ~ "Toddler foods",
    FoodCatCode == 17 ~ "Artificial sweeteners",
    FoodCatCode == 18 ~ "Milk and Milk Products (dairy-free)",
    TRUE ~ NA)) %>%
  relocate(FoodCatDesc, .after = FoodCatCode)



# Create new nutrient variables ####

#food energy (kcal) 
df.intake24_item <- df.intake24_item %>%
  mutate(FoodEnergy = Energykcal - (Alcoholg*7)) %>%
  relocate(FoodEnergy, .after = Energykcal)

#food energy milk (kcal)  - removes all kcals from non-milk beverages
df.intake24_item <- df.intake24_item %>%                
  mutate(FoodMilkEnergy = case_when(
    MainFoodGroupCode %in% c(45, 51, 57, 58) ~  0,
    TRUE ~ FoodEnergy)) %>%
  relocate(FoodMilkEnergy, .after = FoodEnergy)

#food grams 
df.intake24_item <- df.intake24_item %>%
  mutate(FoodGrams = TotalGrams - Alcoholg) %>%
  relocate(FoodGrams, .after = TotalGrams)

#food grams milk 
df.intake24_item <- df.intake24_item %>%                
  mutate(FoodMilkGrams = case_when(
    FoodMilkEnergy == 0 ~ 0,
    TRUE ~ FoodGrams
  )) %>%
  relocate(FoodMilkGrams, .after = FoodGrams)

#calories from macronutrients (kcal)
df.intake24_item <- df.intake24_item %>%
  mutate(
    Carbs_kcal = (Carbohydrateg*3.75),
    Freesugars_kcal = (Freesugarsg*3.75), 
    Fat_kcal = (Fatg*9),
    SatFat_Kcal = (Saturatedfattyacidsg*9),
    TransFat_Kcal = (Transfattyacidsg*9),
    Protein_kcal = (Proteing*4),
    Alcohol_kcal = (Alcoholg*7),)  %>%
  relocate(Carbs_kcal, .after = Carbohydrateg) %>%
  relocate(Freesugars_kcal, .after = Freesugarsg) %>%
  relocate(Fat_kcal, .after = Fatg) %>%
  relocate(SatFat_Kcal, .after = Saturatedfattyacidsg) %>%
  relocate(TransFat_Kcal, .after = Transfattyacidsg) %>%
  relocate(Protein_kcal, .after = Proteing) %>%
  relocate(Alcohol_kcal, .after = Alcoholg)

#salt from sodium 
df.intake24_item <- df.intake24_item %>%
  mutate(
    Saltg = ((Sodiummg/1000)*2.498)
  ) %>%
  relocate(Saltg, .after = Sodiummg)



# Recalls completed under 2 minutes ####

# remove recalls that didn't pass quality check
df.intake24_item <- df.intake24_item %>%
  filter(!(UserID == 10008833 & RecallNo == 2))

df.intake24_item <- df.intake24_item %>%
  filter(!(UserID == 10000459 & RecallNo == 3))

#fix number of recallno and numberofrecalls variables
df.intake24_item <- df.intake24_item %>%
  mutate(NumberOfRecalls = case_when(
    UserID == 10008833 ~ 1, 
    UserID == 10000459 ~ 2,
    TRUE ~ NumberOfRecalls)
  )



# Recalls with fewer than 5 items ####

# remove recalls that didn't pass quality check
df.intake24_item <- df.intake24_item %>%
  filter(!(UserID == 10013179 & RecallNo == 2))

df.intake24_item <- df.intake24_item %>%
  filter(!(UserID == 10012849 & RecallNo == 1)) 

#fix number of recallno and numberofrecalls variables
df.intake24_item <- df.intake24_item %>%
  mutate(NumberOfRecalls = case_when(
    UserID == 10013179 ~ 2, 
    UserID == 10012849 ~ 1,
    TRUE ~ NumberOfRecalls),
    RecallNo = case_when(
      UserID == 10013179 & RecallNo == 3 ~ 2,
      UserID == 10012849 ~ 1,
      TRUE ~ RecallNo
    ))



# High and low energy recalls ####

#remove low energy outliers that didn't pass quality check
df.intake24_item <- df.intake24_item %>%
  filter(!(UserID == 10002909 & RecallNo == 1)) 

#fix number of recallno and numberofrecalls variables
df.intake24_item <- df.intake24_item %>%
  mutate(NumberOfRecalls = case_when(
    UserID == 10002909 ~ 1, 
    TRUE ~ NumberOfRecalls),
    RecallNo = case_when(
      UserID == 10002909 ~ 1,
      TRUE ~ RecallNo
    ))

#remove high energy outliers that didn't pass quality check
df.intake24_item <- df.intake24_item %>%
  filter(!(UserID == 10009887 & RecallNo == 1) &
           !(UserID == 10000459 & RecallNo == 1) &
           !UserID == 20002885 & #removing participant as both recalls did not pass quality checks
           !(UserID == 30000069 & RecallNo == 4)) 

df.survey <- df.survey %>% #removing participant as all recalls did not pass quality checks
  filter(!UserID == 20002885)

#fix number of recallno and numberofrecalls variables
df.intake24_item <- df.intake24_item %>%
  mutate(NumberOfRecalls = case_when(
    UserID == 10009887 ~ 1, 
    UserID == 10000459 ~ 1,
    UserID == 30000069 ~ 3,
    TRUE ~ NumberOfRecalls),
    RecallNo = case_when(
      UserID == 10009887 ~ 1,
      UserID == 10000459 ~ 1,
      TRUE ~ RecallNo
    ))

#clean portion sizes that didn't pass quality check
nutrients <- c("ServingSizeGrams","TotalGrams", "FoodGrams", "FoodMilkGrams" ,"Proteing", "Protein_kcal", "Fatg","Fat_kcal","Carbohydrateg","Carbs_kcal" , "Energykcal", "FoodEnergy","FoodMilkEnergy","Energykj","Alcoholg", "Alcohol_kcal","AOACg","Saturatedfattyacidsg","SatFat_Kcal","Transfattyacidsg", "TransFat_Kcal","VitaminAug","VitaminDug", "Riboflavinmg","VitaminCmg","VitaminB12ug","Folateug" ,"Sodiummg", "Potassiummg", "Calciummg","Magnesiummg","Ironmg","haemironmg", "Nonhaemironmg","Zincmg","Iodineug","Seleniumug" ,"Freesugarsg", "Freesugars_kcal", "Fruitg", "DriedFruitg" ,"FruitJuiceg", "SmoothieFruitg", "Tomatoesg", "TomatoPureeg", "Brassicaceaeg", "YellowRedGreeng", "Beansg", "Nutsg", "OtherVegg", "Beefg", "Lambg","Porkg","ProcessedRedMeatg","OtherRedMeatg","Burgersg", "Sausagesg","Offalg", "Poultryg", "ProcessedPoultryg", "GameBirdsg","WhiteFishg", "OilyFishg", "CannedTunag", "Shellfishg", "CottageCheeseg", "CheddarCheeseg", "OtherCheeseg")

df.intake24_item <- df.intake24_item %>%
  mutate(across(all_of(nutrients), ~ case_when(
    #change from 500g to 50g of crisps
    UserID == 10001114 & RecallNo == 1 & FoodID == 2805837 ~ . * 0.1,
    # change from 2884g to 28.4g of coconut milk
    UserID == 10002649 & RecallNo == 1 & FoodID == 2810175 ~ . * 0.0098474341,
    # change from 375g to 37.5g of crisps
    UserID == 10000205 & RecallNo == 1 & FoodID == 2814318 ~ . * 0.1,
    # change from 1000g to 100g of puff pastry
    UserID == 10003890 & RecallNo == 2 & FoodID == 2828464 ~ . * 0.1,
    # change from 4000g to 400g of pringles and 3780g to 378g of maltesers
    UserID == 10000820 & RecallNo == 1 & (FoodID == 2830538 |FoodID == 2830539) ~ . * 0.1,
    # change from 1750g to 175g of pretzels
    UserID == 10007577 & RecallNo == 4 & FoodID == 2851695 ~ . * 0.1,
    # change from 1050g to 105g of milk chocolate buttons
    UserID == 10002539 & RecallNo == 2 & FoodID == 2862182 ~ . * 0.1,
    # change from 810g to 81g of chocolate dairy dessert
    UserID == 10003190 & RecallNo == 3 & FoodID == 2879146 ~ . * 0.1,
    # change from 1480g to 148g of sausage roll
    UserID == 20000108 & RecallNo == 1 & FoodID == 3206454 ~ . * 0.1,
    # change from 920g to 92g of sausage roll
    UserID == 20000533 & RecallNo == 2 & FoodID == 3216171 ~ . * 0.1,
    # change from 487.5g to 48.75g of crisps
    UserID == 20002338 & RecallNo == 1 & FoodID == 3216580 ~ . * 0.1,
    # change from 800g to 80g of takeaway fries
    UserID == 20000723 & RecallNo == 3 & FoodID == 3227460 ~ . * 0.1,
    # change from crisps entries to match 25g snack size reported by sibling
    UserID == 30000187 & RecallNo == 1 & FoodID == 3281284 ~ . * (25/357.5),
    UserID == 30000187 & RecallNo == 1 & FoodID == 3281288 ~ . * (25/349.38),
    UserID == 30000187 & RecallNo == 1 & FoodID == 3281297 ~ . * (25/56),
    UserID == 30000187 & RecallNo == 2 & FoodID == 3284726 ~ . * (25/455),
    UserID == 30000187 & RecallNo == 2 & FoodID == 3284735 ~ . * (25/422.5),
    # change from 1720g to 172g of haribo sweets
    UserID == 30000387 & RecallNo == 3 & FoodID == 3293093 ~ . * 0.1,
    
    TRUE ~ .  # keeps original value in columns for all other cases
  ))) 



# Create sample characteristics data frame ####
df.char <- df.survey %>%
  select("UserID", "HouseholdID", "psu", "health_board", "Age", "age_cat", "Sex", "age_sex_cat", "age_cat_drv", "age_sex_cat_drv", "Ethnicity", "simd_quintile", "Education", "adults_cat", "children_cat", 
         "AFreqOilyFish", "AFreqToddlerMilk", "ChildCompletedFoodDiary","FoodParcels", "HHFOOD1", "HHFOOD2", "HHFOOD3", "HHFOOD4")


# Merge sample characteristics with intake24 ####

# Join intake24 data and survey data
df.intake24_item <- left_join(df.intake24_item, df.char, by = c('UserID')) %>%
  relocate(c("HouseholdID", "psu", "health_board", "Age", "age_cat", "Sex", "age_sex_cat", "age_cat_drv", "age_sex_cat_drv", "Ethnicity", "simd_quintile", "Education", "adults_cat", "children_cat", 
             "AFreqOilyFish", "AFreqToddlerMilk", "ChildCompletedFoodDiary", "HHFOOD1", "HHFOOD2", "HHFOOD3", "HHFOOD4"), .after = UserID) %>%
  select(-MealTime, -Device)

# Merge with supplement data
df.supplements <- left_join(df.supplements, df.char, by = "UserID") %>%
  relocate(c("HouseholdID", "psu", "health_board", "Age", "age_cat", "Sex", "age_sex_cat", "age_cat_drv", "age_sex_cat_drv", "Ethnicity", "simd_quintile", "Education", "adults_cat", "children_cat", 
             "AFreqOilyFish", "AFreqToddlerMilk", "ChildCompletedFoodDiary", "HHFOOD1", "HHFOOD2", "HHFOOD3", "HHFOOD4"), .after = UserID)


# Write data frames to CSV files ####
write.csv(df.intake24_item, file = "Data/intake24_item.csv")
write.csv(df.survey, file = "Data/survey_clean.csv")
write.csv(df.char, file = "Data/participant_characteristics.csv")
write.csv(df.supplements, file = "Data/supplements.csv")
write.csv(df.breast_milk, file = "Data/breast_milk.csv")
