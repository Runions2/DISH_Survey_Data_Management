
# Title: DISH Data Management
# Author: Ricki Runions
# Date created: May 17, 2024
# Description: This code preps and manages data for the DISH survey and calculates adherence to the Scottish Dietary Goals and the frequency of individual vegetable consumption

LINDSAY WAS HERE

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
  #library(XLConnect)
  library(flextable)
  library(gtsummary)
  library(writexl)
  library(forcats)
  
  # Set working directory
  setwd('C:/Users/ljaacks/OneDrive - University of Edinburgh/Scottish Diets/_National Monitoring of Diet Scotland/3_DISH/Analysis')

  # Import the data
  
  df.survey <- read.csv( "Data/Survey_clean_for_Analysis.csv") %>%
    select(-X)
  
  df.intake24_item <- read.csv("Data/intake24_clean_for_Analysis.csv") %>%
    select(-X)
  
  df.recall <- read.csv("Data/Recall_Dataset_clean_for_Analysis.csv") %>%
    select(-X)
  
  user_char <- read.csv("Data/user_characteristics.csv") %>%
    select(-X)

  

# Keep variables needed for analysis ####
  df.intake24_item <- df.intake24_item %>%
    select(SurveyID,UserID,SurveyDuration,date,,RecallNo,NumberOfRecalls,NumberOfItems,Sex,CalcAge,age_cat,simd_quintile,
           FoodAmount,FoodAmountReason,MealIndex,MealID,MealName,MealTime,MealTime_hour,MealTime_minute,FoodSource,FoodIndex,SearchTerm,FoodID,
           Intake24FoodCode,Description_English,Description_Local,FoodGroupCode,FoodGroupEnglish,SubFoodGroupCode,ReadyMeal,
           TotalGrams,ReasonableAmount,MissingFoodID,MissingFoodDescription,MissingFoodPortionSize,MissingFoodLeftovers,
           Energykcal,Proteing,Fatg,Saturatedfattyacidsg,Transfattyacidsg,Carbohydrateg,Alcoholg,Totalsugarsg,NONMILKSUGARS,AOACg,Sodiummg,
           VitaminAug,Riboflavinmg,Folateug,VitaminDug,VitaminB12ug,VitaminCmg,Ironmg,Calciummg,Iodineug,Magnesiummg,Potassiummg,Seleniumug,Zincmg,
           Fruitg,FruitJuiceg,DriedFruitg,SmoothieFruitg,Tomatoesg,TomatoPureeg,Brassicaceaeg,YellowRedGreeng,Beansg,Nutsg,OtherVegg,
           Beefg,Lambg,Porkg,ProcessedRedMeatg,OtherRedMeatg,Burgersg,Sausagesg,Offalg,Poultryg,ProcessedPoultryg,GameBirdsg,
           WhiteFishg,OilyFishg,CannedTunag,Shellfishg) 
  
  
# Convert to numeric from characters ####
  
  # List of variables to not convert
    char_not_to_convert <- c("SurveyID", "UserID", "SurveyDuration", "FoodAmount", "FoodAmountReason", "MealIndex", "MealID", "MealName", 
                            "MealTime", "FoodSource", "FoodIndex", "SearchTerm", "FoodID", "Intake24FoodCode", 
                            "Description_English", "Description_Local", "FoodGroupEnglish", "ReadyMeal", "NumberOfRecalls", 
                            "NumberOfItems", "date", "Sex", "CalcAge", "age_cat","ReasonableAmount", 
                            "MissingFoodID", "MissingFoodDescription", "MissingFoodPortionSize",
                            "MissingFoodLeftovers", "SubFoodGroupCode")
    
    df.intake24_item <- df.intake24_item %>%
      mutate(across(-all_of(char_not_to_convert), as.numeric))
    
  # Change NA values to 0
    df.intake24_item <- df.intake24_item %>%
      mutate_all(~ifelse(is.na(.), 0, .))

  
  
# Create new nutrient variables ####
   
  # Create food energy (kcal) variable
    df.intake24_item <- df.intake24_item %>%
      mutate(FoodEnergy = Energykcal-(Alcoholg*7)) %>%
    relocate(FoodEnergy, .after = Energykcal)
    
  # Create food grams variable
    df.intake24_item <- df.intake24_item %>%
      mutate(FoodGrams = TotalGrams - Alcoholg) %>%
      relocate(FoodGrams, .after = TotalGrams)
    
  # Calculate intakes as percent of calories (kcal)
    df.intake24_item <- df.intake24_item %>%
      mutate(
        Carbs_kcal = (Carbohydrateg*3.75),
        TotalSugars_kcal = (NONMILKSUGARS*3.75), 
        Fat_kcal = (Fatg*9),
        SatFat_Kcal = (Saturatedfattyacidsg*9),
        TransFat_Kcal = (Transfattyacidsg*9)) %>%
      relocate(Carbs_kcal, .after = Carbohydrateg) %>%
      relocate(TotalSugars_kcal, .after = NONMILKSUGARS) %>%
      relocate(Fat_kcal, .after = Fatg) %>%
      relocate(SatFat_Kcal, .after = Saturatedfattyacidsg) %>%
      relocate(TransFat_Kcal, .after = Transfattyacidsg)
    
  # Calculate salt from sodium 
    df.intake24_item <- df.intake24_item %>%
      mutate(
        Saltg = ((Sodiummg/1000)*2.498)
      ) %>%
      relocate(Saltg, .after = Sodiummg)
    

  
# Calculate mean daily intakes of nutrients ####
  
  # Recall level
  df.intake24_recall <- df.intake24_item %>% 
      group_by(UserID, RecallNo) %>%
      summarise(
        CalcAge = first(CalcAge), 
        NumberOfRecalls = first(NumberOfRecalls), 
        Day_Energykcal = sum(Energykcal),
        Day_FoodEnergy = sum(FoodEnergy),
        Day_FoodGrams = sum(FoodGrams),
        Day_Carbohydrate = sum(Carbohydrateg),
        Day_Carbs_kcal=sum(Carbs_kcal),
        Day_TotalSugars = sum(NONMILKSUGARS), 
        Day_TotalSugars_kcal=sum(TotalSugars_kcal),
        Day_TotalFatg = sum(Fatg),
        Day_Fat_kcal=sum(Fat_kcal),
        Day_SatFatg = sum(Saturatedfattyacidsg),
        Day_SatFat_Kcal=sum(SatFat_Kcal),
        Day_TransFatg = sum(Transfattyacidsg),
        Day_TransFat_Kcal=sum(TransFat_Kcal),
        Day_Protein = sum(Proteing),
        Day_AOACFibre = sum(AOACg),
        Day_VitaminA = sum(VitaminAug),
        Day_Riboflavin = sum(Riboflavinmg),
        Day_Folate = sum(Folateug),
        Day_VitaminD = sum(VitaminDug),
        Day_VitaminB12 = sum(VitaminB12ug),
        Day_VitaminC = sum(VitaminCmg),
        Day_Iron = sum(Ironmg),
        Day_Calcium = sum(Calciummg),
        Day_Sodium = sum(Sodiummg),
        Day_Salt = sum(Saltg), #salt
        Day_Iodine = sum(Iodineug),
        Day_Magnesium = sum(Magnesiummg),
        Day_Potassium = sum(Potassiummg),
        Day_Selenium = sum(Seleniumug),
        Day_Zinc = sum(Zincmg)) %>%
      ungroup() 
    
  # Participant level
  df.intake24_participant <- df.intake24_recall %>% 
      group_by(UserID) %>%
      summarise(
        CalcAge = first(CalcAge),
        NumberOfRecalls = first(NumberOfRecalls),
        Avg_Energykcal = mean(Day_Energykcal),
        Avg_FoodEnergy = mean(Day_FoodEnergy),
        Avg_FoodGrams = mean(Day_FoodGrams),
        Avg_Carbohydrate = mean(Day_Carbohydrate),
        Avg_Carbs_kcal = mean(Day_Carbs_kcal),
        Avg_TotalSugars = mean(Day_TotalSugars),
        Avg_TotalSugars_kcal = mean(Day_TotalSugars_kcal),
        Avg_TotalFatg = mean(Day_TotalFatg),
        Avg_Fat_kcal = mean(Day_Fat_kcal),
        Avg_SatFatg = mean(Day_SatFatg),
        Avg_SatFat_Kcal = mean(Day_SatFat_Kcal),
        Avg_TransFatg = mean(Day_TransFatg),
        Avg_TransFat_Kcal = mean(Day_TransFat_Kcal),
        Avg_Protein = mean(Day_Protein),
        Avg_AOACFibre = mean(Day_AOACFibre),
        Avg_VitaminA = mean(Day_VitaminA),
        Avg_Riboflavin = mean(Day_Riboflavin),
        Avg_Folate = mean(Day_Folate),
        Avg_VitaminD = mean(Day_VitaminD),
        Avg_VitaminB12 = mean(Day_VitaminB12),
        Avg_VitaminC = mean(Day_VitaminC),
        Avg_Iron = mean(Day_Iron),
        Avg_Calcium = mean(Day_Calcium),
        Avg_Sodium = mean(Day_Sodium),
        Avg_Salt = mean(Day_Salt), #Salt
        Avg_Iodine = mean(Day_Iodine),
        Avg_Magnesium = mean(Day_Magnesium),
        Avg_Potassium = mean(Day_Potassium),
        Avg_Selenium = mean(Day_Selenium),
        Avg_Zinc = mean(Day_Zinc)) %>%
      ungroup() 
    
    # Check no duplicates
     n_distinct(df.intake24_participant$UserID)
  
  # Mean daily intakes as % of food energy
    df.intake24_participant <- df.intake24_participant %>%
      group_by(UserID) %>%
      mutate(
        Carbs_PropFoodEnergy = (Avg_Carbs_kcal/Avg_FoodEnergy)*100,
        TotalSugars_PropFoodEnergy = (Avg_TotalSugars_kcal/Avg_FoodEnergy)*100,
        Fat_PropFoodEnergy = (Avg_Fat_kcal/Avg_FoodEnergy)*100,
        SatFat_PropFoodEnergy = (Avg_SatFat_Kcal/Avg_FoodEnergy)*100,
        TransFat_PropFoodEnergy = (Avg_TransFat_Kcal/Avg_FoodEnergy)*100) %>%
      ungroup() %>%
      relocate(Carbs_PropFoodEnergy, .after = Avg_Carbs_kcal) %>%
      relocate(TotalSugars_PropFoodEnergy, .after = Avg_TotalSugars_kcal) %>%
      relocate(Fat_PropFoodEnergy, .after = Avg_Fat_kcal) %>%
      relocate(SatFat_PropFoodEnergy, .after = Avg_SatFat_Kcal) %>%
      relocate(TransFat_PropFoodEnergy, .after = Avg_TransFat_Kcal)
    

    
# Create Main Food Groups ####
    df.intake24_item <- df.intake24_item %>%
      mutate(MainFoodGroupCode= case_when(
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
        TRUE ~ NA))
    
    # Create main food group description variables
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
        MainFoodGroupCode == 20 ~ "Margarine and other cooking fats and oils",
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
        MainFoodGroupCode == 62 ~ "Sandwiches",
        TRUE ~ NA))
    
    

# Remove dairy-free items from milk product sub food groups ####

    #Sub food group level
    # Other milk
    df.intake24 <- df.intake24 %>%  
      mutate(SubFoodGroupCode = case_when(
        SubFoodGroupCode == "13R" &
          (str_detect(Description_English, regex("Almond", ignore_case = TRUE)) |
             str_detect(Description_English, regex("Alpro", ignore_case = TRUE)) |
             str_detect(Description_English, regex("soya", ignore_case = TRUE)) |
             str_detect(Description_English, regex("Hemp", ignore_case = TRUE)) |
             str_detect(Description_English, regex("Oat", ignore_case = TRUE)) |
             str_detect(Description_English, regex("Rice", ignore_case = TRUE))) ~ "13R_DF",
        TRUE ~ SubFoodGroupCode))
    
    # Cream
    df.intake24 <- df.intake24 %>% 
      mutate(SubFoodGroupCode = case_when(
        SubFoodGroupCode == "13B" & 
          str_detect(Description_English, regex("Alpro", ignore_case = TRUE)) ~ "13B_DF",
        TRUE ~ SubFoodGroupCode))
    
    # Other cheese
    df.intake24 <- df.intake24 %>% 
      mutate(SubFoodGroupCode = case_when(
        SubFoodGroupCode == "14R" & 
          str_detect(Description_English, regex("Tofu", ignore_case = TRUE)) ~ "14R_DF",
        TRUE ~ SubFoodGroupCode))
    
    # Yogurt
    df.intake24 <- df.intake24 %>% 
      mutate(SubFoodGroupCode = case_when(
        SubFoodGroupCode == "15B" & 
          str_detect(Description_English, regex("Soya", ignore_case = TRUE)) ~ "15B_DF",
        TRUE ~ SubFoodGroupCode))
    
    # Fromage frais and other dairy desserts 
    df.intake24 <- df.intake24 %>% 
      mutate(SubFoodGroupCode = case_when(
        SubFoodGroupCode == "15C" & 
          str_detect(Description_English, regex("Soya", ignore_case = TRUE)) ~ "15C_DF",
        TRUE ~ SubFoodGroupCode))
    
    # Ice cream
    df.intake24 <- df.intake24 %>% 
      mutate(SubFoodGroupCode = case_when(
        SubFoodGroupCode == "53R" &
          str_detect(Description_English, regex("Dairy free", ignore_case = TRUE)) ~ "53R_DF",
        TRUE ~ SubFoodGroupCode))
    
    # Main food group level
    df.intake24<-df.intake24 %>% 
      mutate(MainFoodGroupCode = case_when(
        SubFoodGroupCode == "13R_DF" ~ 63,
        SubFoodGroupCode == "13B_DF" ~ 63,
        SubFoodGroupCode == "14R_DF" ~ 64,
        SubFoodGroupCode == "15B_DF" ~ 65,
        SubFoodGroupCode == "15C_DF" ~ 65,
        SubFoodGroupCode == "53R_DF" ~ 66,
        TRUE ~ MainFoodGroupCode))
    
    
    # Remove hot chocolate made with water
    
      # Sub food group
      df.intake24 <- df.intake24 %>% 
        mutate(SubFoodGroupCode = case_when(
          str_detect(Description_English, regex("Hot chocolate, made with water", ignore_case=TRUE)) ~ "50A",
          TRUE ~ SubFoodGroupCode)) 
      
      # Main food group
      df.intake24 <- df.intake24 %>% 
        mutate(MainFoodGroupCode = case_when(
          SubFoodGroupCode == "50A" ~ 50,
          TRUE ~ MainFoodGroupCode))
    
      
    # Move milky coffees into 'other milk'
 
      #Sub food group
      df.intake24 <- df.intake24 %>% 
        mutate(SubFoodGroupCode = case_when(
          SubFoodGroupCode == "51A" &
            (str_detect(Description_English, regex("Cappuccino", ignore_case = TRUE)) | 
               str_detect(Description_English, regex("Latte", ignore_case = TRUE)) |
               str_detect(Description_English, regex("Flat white", ignore_case = TRUE)) |
               str_detect(Description_English, regex("Mocha", ignore_case = TRUE))) ~ "13R",
          TRUE ~ SubFoodGroupCode))
    
    # Re-categorise dairy-free coffees out of 'other milk'
    
      # Sub food group
      df.intake24 <- df.intake24 %>% 
        mutate(SubFoodGroupCode = case_when(
          SubFoodGroupCode == "13R" &
            str_detect(Description_English, regex("soya milk", ignore_case = TRUE)) ~ "13R_DF",
          TRUE ~ SubFoodGroupCode))
      
      # Main food group
      df.intake24 <- df.intake24 %>% 
        mutate(MainFoodGroupCode = case_when(
          SubFoodGroupCode == "13R" ~ 13,
          TRUE ~ MainFoodGroupCode))

      
  ## Calculate mean energy density ####
    df.intake24_participant <- df.intake24_participant %>%
      group_by(UserID) %>%
      mutate(Avg_EnergyDensity = (Avg_FoodEnergy/Avg_FoodGrams)*100) %>%
      ungroup ()
    
    # needs updating to remove non-milk beverages @Ricki
    
    
    
# Re-code discretionary foods ####
      
    savoury_biscuits_list <- c("7649","251","252","3973","7650","8330","258","256","255","273","3267","266","267","3236","279","10062","11242","274","7652")
    
    lollies <- c("7762", "2262")
    
    pizza <- c("11511", "10029","10028","10030","10032","11514","11508","10026","11516","11512","11509","11510","11512", "10031", "11511","2743","10033","10034") #11512 has 2 different descriptions, same code
    
    ready_meal <- c("8049","8011","9245","3187","1347","9386","9244","8666","10298","4283","3730","10755","6393","9328","1356","8588","5320","2734","9300","5321","7094","9318","9270","1116","5627","8038","1352")
    
    #biscuits_sav <- df.intake24 %>%
    # filter(NutrientTableCode == c(savoury_biscuits_list))

    
    
# Re-code Main Food Groups into summary categories for reporting ####

#We're reporting intakes of the following food groups:

#Red meat
#Red processed meat
#Red meat and red processed meat combined
#Oily fish
#Fruit
#Vegetables
#Fruit and vegetables combined
#Fruit juice
#Sugar-sweetened beverages
#Sugar confectionery
#Chocolate confectionery
#Sweet biscuits
#Savoury biscuits
#Cakes, pastries and fruit pies
#Crisps and savoury snacks
#Pizzas and ready meals


#I've created these food categories by re-coding main food groups below - definitely double check this all makes sense, you can look at NDNS food category definitions to confirm.
    
#    Some points to highlight:
      
#      1) Below estimates are from food groups (i.e. don't include disaggregated estimates). This is the only way we can look at nutritional contributions.

# 2) Do FSS want to include pork within red meat definitions? The below code captures pork, but I know there's been some discussion on this back and forth.
                                               
# Create binary variable for red meat
                                        
#create binary variable and categorise all red and red processed meat based on main food group codes
df.intake24 <- df.intake24 %>%
mutate(RedMeat = case_when(
MainFoodGroupCode %in% c(23, 24, 25, 28, 22, 29, 30, 31, 32) ~ 1, 
 TRUE ~ 0
))
                                               
                                               #check 'other meat' category to determine what needs to be re-coded
                                               check <- df.intake24 %>%
                                                 filter(MainFoodGroupCode == "32")
                                               
                                               #remove any meat that is not red meat from binary variable
                                               df.intake24 <- df.intake24 %>%
                                                 mutate(RedMeat = case_when(
                                                   MainFoodGroupCode == "32" &
                                                     (str_detect(Description_English, regex("chicken", ignore_case = TRUE))) ~ 0,
                                                   TRUE ~ RedMeat
                                                 ))
                                             
                                               
                                               check <- df.intake24 %>%
                                                 filter(MainFoodGroupCode == 35)
                                               
                                               # Create reporting food groups 
                                               df.intake24 <- df.intake24 %>% 
                                                 mutate(ReportingFoodGroupCode = case_when (
                                                   #Red meat
                                                   MainFoodGroupCode %in% c(23, 24, 25, 28) & RedMeat == 1 ~ 1,
                                                   # Red processed meat
                                                   MainFoodGroupCode %in% c(22, 29, 30, 31, 32) & RedMeat == 1 ~ 2,
                                                   # Oily fish
                                                   MainFoodGroupCode == 35 ~ 3,
                                                   # Fruit
                                                   MainFoodGroupCode == 40 ~ 4,
                                                   # Vegetables
                                                   MainFoodGroupCode == 36 ~ 5,
                                                   # Fruit juice - sometimes you need to specify the subfoodgroup instead of main food group - this captures only fruit juice and not smoothies
                                                   SubFoodGroupCode == "45R" ~ 6,
                                                   # Sugar-sweetened beverages
                                                   MainFoodGroupCode == 57 ~ 7,
                                                   # Sugar confectionery
                                                   MainFoodGroupCode == 43 & !NutrientTableCode %in% c(lollies) ~ 8,
                                                   # Chocolate confectionery
                                                   MainFoodGroupCode == 44 ~ 9,
                                                   # Sweet biscuits
                                                   MainFoodGroupCode == 7 & !NutrientTableCode %in% c(savoury_biscuits_list) ~ 10,
                                                   # Savoury biscuits
                                                   MainFoodGroupCode == 7 & NutrientTableCode %in% c(savoury_biscuits_list) ~ 11,
                                                   # Cakes, pastries and fruit pies
                                                   MainFoodGroupCode == 8 ~ 12,
                                                   # Crisps and savoury snacks
                                                   MainFoodGroupCode == 42 ~ 13,
                                                   #Pizzas and ready meals
                                                   NutrientTableCode %in% c(pizza, ready_meal) ~ 14,
                                                   #Ice creams
                                                   MainFoodGroupCode == 53 | NutrientTableCode %in% c(lollies) ~ 15,
                                                   TRUE ~ NA)) 
                                               
                                               # Add description variable
                                               df.intake24 <- df.intake24 %>%
                                                 mutate(ReportingFoodGroupDesc = case_when(
                                                   ReportingFoodGroupCode == 1 ~ "Red meat",
                                                   ReportingFoodGroupCode == 2 ~ "Red processed meat",
                                                   ReportingFoodGroupCode == 3 ~ "Oily fish",
                                                   ReportingFoodGroupCode == 4 ~ "Fruit",
                                                   ReportingFoodGroupCode == 5 ~ "Vegetables",
                                                   ReportingFoodGroupCode == 6 ~ "Fruit juice",
                                                   ReportingFoodGroupCode == 7 ~ "Sugar-sweetened beverages",
                                                   ReportingFoodGroupCode == 8 ~ "Sugar confectionery",
                                                   ReportingFoodGroupCode == 9 ~ "Chocolate confectionery",
                                                   ReportingFoodGroupCode == 10 ~ "Sweet biscuits",
                                                   ReportingFoodGroupCode == 11 ~ "Savoury biscuits",
                                                   ReportingFoodGroupCode == 12 ~ "Cakes, pastries and fruit pies",
                                                   ReportingFoodGroupCode == 13 ~ "Crisps and savoury snacks",
                                                   ReportingFoodGroupCode == 14 ~ "Pizzas and ready meals",
                                                   ReportingFoodGroupCode == 15 ~ "Ice cream and lollies",
                                                   TRUE ~ NA_character_))
                                              
                                                #Check food groups for foods of interest that may not be reported in the above
                                                df.intake24 %>%
                                                 count(MainFoodGroupDesc, MainFoodGroupCode, SubFoodGroupCode, ReportingFoodGroupDesc, ReportingFoodGroupCode) %>%
                                                 View()
                                               
                                               
                                               check <- df.intake24 %>%
                                                 filter(SubFoodGroupCode == "61R")
                                            
                                               
                                               
                                               
                                               
#Food Groups: Calculate Mean Daily Intakes (g) of Reporting Food Groups ####
                                                 
#1) Daily intakes (g)
df.intake24_foodgroup_day <- df.intake24 %>%
  group_by(UserID, RecallNo) %>%
  summarise(
    NumberOfRecalls = first(NumberOfRecalls),
    Day_redmeat = sum(TotalGrams[ReportingFoodGroupCode == 1 ], na.rm = TRUE),
    Day_processedmeat = sum(TotalGrams[ReportingFoodGroupCode == 2], na.rm = TRUE),
    Day_redprocessedmeat = sum(TotalGrams[ReportingFoodGroupCode %in% c(1, 2)], na.rm = TRUE),
    Day_oilyfish = sum(TotalGrams[ReportingFoodGroupCode == 3], na.rm = TRUE),
    Day_fruit = sum(TotalGrams[ReportingFoodGroupCode == 4], na.rm = TRUE),
    Day_veg = sum(TotalGrams[ReportingFoodGroupCode == 5], na.rm = TRUE),
    Day_fruitveg = sum(TotalGrams[ReportingFoodGroupCode %in% c(4, 5)], na.rm = TRUE),
    Day_fruitjuice = sum(TotalGrams[ReportingFoodGroupCode == 6], na.rm = TRUE),
    Day_ssb = sum(TotalGrams[ReportingFoodGroupCode == 7], na.rm = TRUE),
    Day_sugarconfec = sum(TotalGrams[ReportingFoodGroupCode == 8], na.rm = TRUE),
    Day_chocolateconfec = sum(TotalGrams[ReportingFoodGroupCode == 9], na.rm = TRUE),
    Day_sweetbiscuits = sum(TotalGrams[ReportingFoodGroupCode == 10], na.rm = TRUE),
    Day_savourybiscuits = sum(TotalGrams[ReportingFoodGroupCode == 11], na.rm = TRUE),
    Day_cakespies = sum(TotalGrams[ReportingFoodGroupCode == 12], na.rm = TRUE),
    Day_savourysnacks = sum(TotalGrams[ReportingFoodGroupCode == 13], na.rm = TRUE),
    Day_pizzareadymeals = sum(TotalGrams[ReportingFoodGroupCode == 14], na.rm = TRUE),
    Day_icecream = sum(TotalGrams[ReportingFoodGroupCode == 15], na.rm = TRUE))%>%
  ungroup()

# Join with day level intake24 dataset
df.intake24_day_merge <- left_join(df.intake24_day, df.intake24_foodgroup_day, by=c("UserID", "RecallNo"))  %>%
  rename(NumberOfRecalls="NumberOfRecalls.x")

# Mean daily intakes (g)
df.intake24_foodgroup_participant <- df.intake24_foodgroup_day %>%
  group_by(UserID) %>%
  summarise(
    NumberOfRecalls = first(NumberOfRecalls),
    Avg_redmeat = mean(Day_redmeat),
    Avg_processedmeat = mean(Day_processedmeat),
    Avg_redprocessedmeat = mean(Day_redprocessedmeat),
    Avg_oilyfish = mean(Day_oilyfish),
    Avg_fruit = mean(Day_fruit),
    Avg_veg = mean(Day_veg),
    Avg_fruitveg = mean(Day_fruitveg),
    Avg_fruitjuice = mean(Day_fruitjuice),
    Avg_ssb = mean(Day_ssb),
    Avg_sugarconfec = mean(Day_sugarconfec),
    Avg_chocolateconfec = mean(Day_chocolateconfec),
    Avg_sweetbiscuits = mean(Day_sweetbiscuits),
    Avg_savourybiscuits = mean(Day_savourybiscuits),
    Avg_cakespies = mean(Day_cakespies),
    Avg_savourysnacks = mean(Day_savourysnacks),
    Avg_pizzareadymeals = mean(Day_pizzareadymeals),
    Avg_icecream = mean(Day_icecream)) %>%
  ungroup()

# Join with participant level intake24 dataset
df.intake24_participant <- left_join(df.intake24_participant, df.intake24_foodgroup_participant, by=c("UserID")) %>%
  select(-NumberOfRecalls.y) %>%
  rename(NumberOfRecalls="NumberOfRecalls.x") 

rm(df.intake24_foodgroup_day, df.intake24_foodgroup_participant)



#Food groups - contributions to energy and free sugar intake ####
  
# Calculate daily intakes of energy and free sugars from food groups
df.intake24_day_fg <- df.intake24 %>%
  group_by(UserID, RecallNo, ReportingFoodGroupCode) %>% 
  summarise(
    NumberOfRecalls = first(NumberOfRecalls),
    Day_Energy_fg=sum(Energykcal),
    Day_FreeSugars_fg=sum(NONMILKSUGARS)) #sugars

# Create a grid with all possible food category and recall number combinations for each participant
# Need to do this separately for different recall numbers

# 1 recall
df.onerecall <- df.intake24_day_merge %>%
  filter(NumberOfRecalls == 1)

foodgrid_onerecall <- expand.grid( #create a grid of each combination of the below:
  UserID = unique(df.onerecall$UserID), #take each unique UserID
  RecallNo = unique(df.onerecall$RecallNo), #take each unique RecallNo
  ReportingFoodGroupCode = factor(1:15)) #create a column of with 1-12 for reporting food groups

# 2 recalls
df.tworecalls <- df.intake24_day_merge %>%
  filter(NumberOfRecalls == 2)

foodgrid_tworecalls <- expand.grid(
  UserID = unique(df.tworecalls$UserID),
  RecallNo = unique(df.tworecalls$RecallNo),
  ReportingFoodGroupCode = factor(1:15))

# 3 recalls
df.threerecalls <- df.intake24_day_merge %>%
  filter(NumberOfRecalls == 3)

foodgrid_threerecalls <- expand.grid(
  UserID = unique(df.threerecalls$UserID),
  RecallNo = unique(df.threerecalls$RecallNo),
  ReportingFoodGroupCode = factor(1:15))

#4 recalls 
df.fourrecalls <- df.intake24_day_merge %>%
  filter(NumberOfRecalls == 4)

foodgrid_fourrecalls <- expand.grid(
  UserID = unique(df.fourrecalls$UserID),
  RecallNo = unique(df.threerecalls$RecallNo),
  ReportingFoodGroupCode = factor(1:15))

# Combine expanded grids
foodgroup_grid <- bind_rows(foodgrid_onerecall, foodgrid_tworecalls, foodgrid_threerecalls) %>%
  arrange(UserID, RecallNo) %>%
  mutate(ReportingFoodGroupCode = as.numeric(ReportingFoodGroupCode))

# Join daily intakes with expanded grids
df.intake24_day_fg <- left_join(foodgroup_grid, df.intake24_day_fg, 
                                by = c("UserID", "RecallNo", "ReportingFoodGroupCode")) %>%
  relocate(NumberOfRecalls, .after=UserID)

# Change NA values to 0 
df.intake24_day_fg <- df.intake24_day_fg %>%
  mutate_all(~ifelse(is.na(.), 0, .))

# Remove datasets no longer needed
rm(foodgrid_onerecall, foodgrid_tworecalls, foodgrid_threerecalls, foodgroup_grid)


# Calculate mean daily intakes of energy and free sugars from food groups

# Participant level dataset
df.intake24_participant_fg <- df.intake24_day_fg %>%
  group_by(UserID, ReportingFoodGroupCode) %>% 
  mutate(
    Avg_Energy_fg = ifelse(NumberOfRecalls != 0,  sum(Day_Energy_fg)/NumberOfRecalls, NA),
    Avg_FreeSugars_fg = ifelse(NumberOfRecalls != 0,  sum(Day_FreeSugars_fg)/NumberOfRecalls, NA))%>%
  ungroup()

# Change NA values to 0 
df.intake24_participant_fg <- df.intake24_participant_fg %>%
  mutate_all(~ifelse(is.na(.), 0, .))

# Fill in values across food groups in all recalls
df.intake24_participant_fg <- df.intake24_participant_fg %>%
  group_by(UserID, ReportingFoodGroupCode) %>%
  mutate(across(starts_with("Avg_"), ~max(., na.rm = TRUE))) %>%
  ungroup()

# Drop day level variables (as participant level dataset)
df.intake24_participant_fg <- df.intake24_participant_fg %>%
  select(-starts_with("Day_"))

#Keep one line per food group per participant
df.intake24_participant_fg <- df.intake24_participant_fg %>%
  distinct(UserID, ReportingFoodGroupCode, .keep_all = TRUE)


# Calculate energy and free sugar contributions from food groups

# Pull out mean daily intakes of energy and free sugars
NutrientIntakes <- subset(df.intake24_participant, select=c(UserID, Avg_Energykcal, Avg_TotalSugars))

# Join with mean daily intakes of energy and free sugars from food groups 
df.intake24_participant_fg <- left_join(NutrientIntakes, df.intake24_participant_fg, by=c("UserID"))

#Calculate nutritional contributions from food categories
df.intake24_participant_fg <- df.intake24_participant_fg %>%
  group_by(UserID, ReportingFoodGroupCode) %>%
  mutate(
    Prop_Energy_fg=(Avg_Energy_fg/Avg_Energykcal)*100,
    Prop_FreeSugars_fg=(Avg_FreeSugars_fg/Avg_TotalSugars)*100,
  ) %>%
  ungroup() %>%
  relocate(ReportingFoodGroupCode, .after=UserID) %>% 
  select(-RecallNo, -NumberOfRecalls, -Avg_Energykcal, -Avg_TotalSugars) %>%
  mutate_at(vars(starts_with("Prop_")), ~round(., 2))

# Change NA values to 0 
df.intake24_participant_fg <- df.intake24_participant_fg %>%
  mutate_all(~ifelse(is.na(.), 0, .))

#add in description of reporting food codes
df.intake24_participant_fg <- df.intake24_participant_fg %>%
  mutate(ReportingFoodGroupDesc = case_when(
    ReportingFoodGroupCode == 1 ~ "Red meat",
    ReportingFoodGroupCode == 2 ~ "Red processed meat",
    ReportingFoodGroupCode == 3 ~ "Oily fish",
    ReportingFoodGroupCode == 4 ~ "Fruit",
    ReportingFoodGroupCode == 5 ~ "Vegetables",
    ReportingFoodGroupCode == 6 ~ "Fruit juice",
    ReportingFoodGroupCode == 7 ~ "Sugar-sweetened beverages",
    ReportingFoodGroupCode == 8 ~ "Sugar confectionery",
    ReportingFoodGroupCode == 9 ~ "Chocolate confectionery",
    ReportingFoodGroupCode == 10 ~ "Sweet biscuits",
    ReportingFoodGroupCode == 11 ~ "Savoury biscuits",
    ReportingFoodGroupCode == 12 ~ "Cakes, pastries and fruit pies",
    ReportingFoodGroupCode == 13 ~ "Crisps and savoury snacks",
    ReportingFoodGroupCode == 14 ~ "Pizzas and ready meals",
    ReportingFoodGroupCode == 15 ~ "Ice cream and lollies",
    TRUE ~ NA_character_))



# Adherence to Scottish Dietary Goals ####

# Energy density to be max 125kcal/100g
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_Energy = case_when(
      Avg_EnergyDensity <= 125 ~ 1, 
      Avg_EnergyDensity > 125 ~ 0,
      TRUE ~ NA)) 

# Total fat (max 35% of food energy)
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_Fat = case_when(
    Fat_PropFoodEnergy <= 35 ~ 1,
    Fat_PropFoodEnergy > 35 ~ 0,
    TRUE ~ NA)) 

# Saturated fat (max 11% of food energy)
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_SatFat = case_when(
    SatFat_PropFoodEnergy <= 11 ~ 1,
    SatFat_PropFoodEnergy > 11 ~ 0,
    TRUE ~ NA)) 

# Trans fat (<1% of food energy)
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_TransFat = case_when(
    TransFat_PropFoodEnergy < 1 ~ 1,
    TransFat_PropFoodEnergy >= 1 ~ 0,
    TRUE ~ NA)) 

# Free sugars (max 5% of total energy)
df.intake24_participant <- df.intake24_participant %>%
  mutate(
    SDG_Sugar = case_when(
    TotalSugars_PropFoodEnergy <= 5 ~ 1,
    TotalSugars_PropFoodEnergy > 5 ~ 0,
    TRUE ~ NA)) 

# Carbohydrates (~50% of total energy; created upper/lower limits of 45-55%)
df.intake24_participant <- df.intake24_participant %>%
  mutate(SDG_Carbs = case_when(
    Carbs_PropFoodEnergy >= 45 & Carbs_PropFoodEnergy <= 55 ~ 1,
    Carbs_PropFoodEnergy < 45 | Carbs_PropFoodEnergy > 55 ~ 0,
    TRUE ~ NA)) %>%
  ungroup()

# Fibre (min 15g/d for 2-5y; min 20g/d for 5-11y; min 25g/d for 11-16y)
df.intake24_participant <- df.intake24_participant %>%
  mutate(SDG_Fibre = case_when(
    age >= 2 & age < 6 & Avg_AOACFibre >= 15 ~ 1,
    age >= 2 & age < 6 & Avg_AOACFibre < 15 ~ 0,
    age >= 5 & age < 12 & Avg_AOACFibre >= 20 ~ 1,
    age >= 5 & age < 12 & Avg_AOACFibre < 20 ~ 0,
    age >= 11 & age < 17 & Avg_AOACFibre >= 25 ~ 1,
    age >= 11 & age < 17 & Avg_AOACFibre < 25 ~ 0,
    age >= 17 & Avg_AOACFibre >= 30 ~ 1,
    age >= 17 & Avg_AOACFibre < 30 ~ 0,
    TRUE ~ NA)) 

# Salt (2g / day for 1-3 year olds; 3g / day for 4-6 year olds; 5g / day for 7-10 year olds; 6g / day for 11+ year olds)
df.intake24_participant <- df.intake24_participant%>%
  group_by(UserID) %>%
  mutate(SDG_Salt = case_when(
    age >= 1 & age < 4 & Avg_Salt >= 2 ~ 1,
    age >= 1 & age < 4 & Avg_Salt < 2 ~ 0,
    age >= 4 & age < 7 & Avg_Salt >= 3 ~ 1,
    age >= 4 & age < 7 & Avg_Salt < 3 ~ 0,
    age >= 7 & age < 11 & Avg_Salt >= 5 ~ 1,
    age >= 7 & age < 11 & Avg_Salt < 5 ~ 0,
    age >= 11 & Avg_Salt >= 6 ~ 1,
    age >= 11 & Avg_Salt < 6 ~ 0,
    TRUE ~ NA)) %>%
  ungroup()

# Fruits and vegetables (at least 5 portions/day (≥200 g/day))

  # Calculate daily intakes of beans 
  SDG_Fruitveg <- df.intake24 %>%
    group_by(UserID, RecallNo) %>%
    mutate(
      FV_Beans = sum(Beansg)) %>%
    ungroup()
  
  # Cap at 40g (can only count as max one portion)
  SDG_Fruitveg <- SDG_Fruitveg %>%
    group_by(UserID, RecallNo) %>%
    mutate(
      FV_Beans = ifelse(
        FV_Beans > 40, 40, FV_Beans)) %>%
    ungroup()
  
  # Calculate daily intakes of fruit juice 
  SDG_Fruitveg <- SDG_Fruitveg %>%
    group_by(UserID, RecallNo) %>%
    mutate(
      FV_FruitJuice = sum(FruitJuiceg)) %>%
    ungroup()
  
  # Cap at 150g (can only count as max one portion) - School regs don't allow any fruit juice - so we will report as 150 and zero
  SDG_Fruitveg <- SDG_Fruitveg %>%
    group_by(UserID, RecallNo) %>%
    mutate(
      FV_FruitJuice = ifelse(
        FV_FruitJuice > 150, 150, FV_FruitJuice)) %>%
    ungroup()

  # Calculate daily intakes of dried fruit (1 portion is 15g)
  SDG_Fruitveg <- SDG_Fruitveg %>%
    group_by(UserID, RecallNo) %>%
    mutate(
      FV_Dried = sum(DriedFruitg)) %>%
    ungroup()
  
  # Calculate daily intakes of remaining fruit and veg
  SDG_Fruitveg <- SDG_Fruitveg %>%
    group_by(UserID, RecallNo) %>%
    mutate(
      FV_FreshFruit = sum(Fruitg, SmoothieFruitg),
      FV_Veg = sum(Tomatoesg, TomatoPureeg, Brassicaceaeg, YellowRedGreeng, OtherVegg)) %>%
    ungroup()

  # Calculate daily intakes of total fruit and veg
  SDG_Fruitveg <- SDG_Fruitveg %>%
    distinct(UserID, RecallNo, .keep_all = TRUE) %>%
    group_by(UserID, RecallNo) %>%
    mutate(
      FV_Total = sum(FV_Beans, FV_FruitJuice, FV_Dried, FV_FreshFruit, FV_Veg, na.rm = TRUE)
    ) %>%
    ungroup()

  # Create a smaller day level dataset
  SDG_Fruitveg_sub <- SDG_Fruitveg %>%
    select(UserID, RecallNo, NumberOfRecalls, starts_with("FV_"))
  
  # Calculate mean daily intakes (grams)
  SDG_Fruitveg_sub <- SDG_Fruitveg_sub %>%
    group_by(UserID) %>%
    mutate(Avg_FV_Beans = sum(FV_Beans)/NumberOfRecalls,
           Avg_FV_FruitJuice = sum(FV_FruitJuice)/NumberOfRecalls,
           Avg_FV_Dried = sum(FV_Dried)/NumberOfRecalls,
           Avg_FV_FreshFruit = sum(FV_FreshFruit)/NumberOfRecalls,
           Avg_FV_Veg = sum(FV_Veg)/NumberOfRecalls,
           Avg_FV_Total = sum(FV_Total)/NumberOfRecalls) %>%
    ungroup()
  
  # Calculate mean daily intakes (portions)
  SDG_Fruitveg_sub <- SDG_Fruitveg_sub %>%
    group_by(UserID) %>%
    mutate(Avg_FV_Beans_Prtn = (Avg_FV_Beans/40),
           Avg_FV_FruitJuice_Prtn = (Avg_FV_FruitJuice/150),
           Avg_FV_Dried_Prtn = (Avg_FV_Dried/15),
           Avg_FV_FreshFruit_Prtn = (Avg_FV_FreshFruit/40),
           Avg_FV_Veg_Prtn = (Avg_FV_Veg/40)) %>%
    ungroup()
  
  SDG_Fruitveg_sub <- SDG_Fruitveg_sub %>%
    mutate(
      Avg_FV_TotalPrtn = rowSums(select(., ends_with("Prtn")), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Create variable for meeting fruit and veg SDG 
  SDG_Fruitveg_sub <- SDG_Fruitveg_sub %>%
    group_by(UserID) %>%
    mutate(SDG_FV = case_when(
      Avg_FV_Total >= 200 ~ 1,
      Avg_FV_Total < 200 ~ 0,
      TRUE ~ NA)) %>%
    ungroup()

  # Create participant level SDG dataset
  SDG_Fruitveg_participant <- SDG_Fruitveg_sub %>%
    distinct(UserID, .keep_all=TRUE) %>%
    select(UserID, starts_with("Avg_"), SDG_FV)
  
  # Join with intake24_participant dataset
  df.intake24_participant <- left_join(df.intake24_participant, SDG_Fruitveg_participant, by = c("UserID"))
  
  df.intake24_participant <- df.intake24_participant %>%
    relocate(starts_with("Avg_FV"), .before = "SDG_Energy") %>%
    mutate_at(vars(starts_with("Avg_")), ~round(., 1))
  
  rm(SDG_Fruitveg, SDG_Fruitveg_participant, NutrientIntakes)

#add age buckets back in 
df.intake24_participant <- inner_join(df.intake24_participant, user_char, by = "UserID")

#relocate columns for ease in management
df.intake24_participant <- df.intake24_participant %>%
  relocate(c("Sex","simd_quintile", "CalcAge","Age", "age_cat", "Ethnicity"), .after = age)

#set factor levels and make NA a level
df.intake24_participant$age_cat <- factor(df.intake24_participant$age_cat, levels = c("2-4y", "5-10y", "11-15y"))
df.intake24_participant$age_cat <- fct_explicit_na(df.intake24_participant$age_cat)

df.intake24_participant %>% 
  select(age_cat, SDG_Energy, SDG_Fat, SDG_SatFat, SDG_TransFat, SDG_Sugar, SDG_Carbs, SDG_Fibre, SDG_Salt, SDG_FV) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = age_cat
  ) %>%
  add_overall() %>%
  modify_header(label ~ "**SDG **") %>%
  bold_labels() %>%  
  as_flex_table()

check <- df.intake24_participant %>%
  filter(SDG_Energy == 1)

# SDGs for Oily fish and Red and red processed meat

#set factor levels and make NA a level
df.survey$age_cat <- factor(df.survey$age_cat, levels = c("2-4y", "5-10y", "11-15y"))
df.survey$age_cat <- fct_explicit_na(df.survey$age_cat)

#Oily fish
df.survey <- df.survey %>%
  mutate(SDG_oilyfish = case_when(
    AFreqOilyFish %in% c("Never", "Not sure", "Less than once a week") ~ 0,
    TRUE ~ 1
  ))

#Red and red processed meat
df.survey <- df.survey %>%
  mutate(SDG_RRPM = case_when(
    AFreqRRPM %in% c("Never", "Not sure", "Less than once a week") ~ 0,
    TRUE ~ 1
  ))

#set factor levels
df.survey$AFreqRRPM <- factor(df.survey$AFreqRRPM, levels = c("Every day", "4-6 times a week", "2-3 times a week", "Once a week", "Less than once a week", "Never", "Not sure" ))
df.survey$AFreqRRPM <- fct_explicit_na(df.survey$AFreqRRPM)

df.survey$AFreqOilyFish <- factor(df.survey$AFreqOilyFish, levels = c("Every day", "4-6 times a week", "2-3 times a week", "Once a week", "Less than once a week", "Never", "Not sure" ))
df.survey$AFreqOilyFish <- fct_explicit_na(df.survey$AFreqOilyFish)

df.survey %>% 
  select(age_cat, AFreqRRPM, AFreqOilyFish, SDG_RRPM, SDG_oilyfish) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = age_cat
  ) %>%
  add_overall() %>%
  modify_header(label ~ "**SDG **") %>%
  bold_labels() %>%  
  as_flex_table()



# Most commonly reported vegetables ####
Vegetables <- df.intake24 %>%
  filter(MainFoodGroupCode == c(37, 36))

Veg_sub <- Vegetables %>%
  select(UserID, SearchTerm, Description_English, Description_Local, FoodGroupCode, FoodGroupEnglish, ReadyMeal, RecallNo, NumberOfRecalls, NumberOfItems, Sex, CalcAge, simd_quintile, age_cat, NewMealName, MainFoodGroupCode, MainFoodGroupDesc, ReportingFoodGroupCode, ReportingFoodGroupDesc)

Veg_sub$age_cat <- factor(Veg_sub$age_cat, levels = c("2-4y", "5-10y", "11-15y"))



Veg_sub %>%
  filter(ReportingFoodGroupCode == 5) %>%
  select(age_cat, Description_English) %>%
  tbl_summary(missing = "no",
              digits = list(all_categorical() ~ c(1,0)),
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}, {max}")),
              by = age_cat
  ) %>%
  add_overall() %>%
  modify_header(label ~ "**Vegetables**") %>%
  bold_labels() %>%  
  as_flex_table() 


# Files for analysis ####
write.csv(df.intake24_participant_fg, file = "/Users/ricki/Desktop/DISH_data_cleaning/Output_data_analysis/intake24_FS_Energy_14_05_24.csv")
write.csv(df.intake24_participant, file = "/Users/ricki/Desktop/DISH_data_cleaning/Output_data_analysis/intake24_intake_foodgroups_SDG_14_05_24.csv")
