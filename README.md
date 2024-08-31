# Dietary Intake in Scotland’s CHildren (DISH)
## Achievement of the Scottish Dietary Goals, Nutrient and Food Group Intake, Food Beyond the School Gate and Consumption of Energy Drinks

This repository processes and analyses data from a national survey of children and young people aged 2 to 15 years living in Scotland. 

The main outcomes were:
1. Achievement of the Scottish Dietary Goals
2. Nutrient intake including adherence to the RNIs and LRNIs
3. Food group intake
4. Consumption of food beyond the school gates
5. Consumption of energy drinks

Data were collected using a survey and up to four 24-hour dietary recalls (herein 'intake24').

# R scripts
## DISH_cleaning

This R script processes the raw survey and intake24 data: 
- Renames variables 
- Removes ineligibles and incompletes
- Categorises 'other' responses
- Creates new variables: age groups (2-4y, 5-10y, 11-15y), recall number, number of recalls, number of items per recall, day of the week recall completed, meal types, main food groups, reporting food groups, device intake24 completed on
- Re-codes missing food codes
- Removes diet supplements and breast milk
- Removes dairy-free items from dairy food groups
- Converts intake24 nutrient variables to numeric and replaces NAs with 0s
- Creates new nutrient variables: food energy, food and milk energy, food weight, food and milk weight, percent of calories, salt


## DISH_intake24

This R script calculates:
- total daily intakes of nutrients
- total daily intakes of food groups
- mean participant intakes of nutrients (across 1-4 recalls)
- mean participant intakes of food groups (across 1-4 recalls)
- adherence to the Scottish Dietary Goals
- meeting the RNIs
- meeting the LRNIs

It also cleans or drops energy intakes <400 kcal or >4,000 kcal.


## DISH_results

This script creates report tables for:
- participant characteristics
- Scottish Dietary Goals
- nutrient intakes
- meeting the RNIs
- meeting the LRNIs
- food group intakes
- consumption of food beyond the school gates
- consumption of energy drinks 


# Data Files
## Raw

survey_26.08.24.xlsx
  - Raw survey data
    
intake24_26.08.24.xlsx 
  - Raw Intake24 24-hour dietary recall data
    

## Clean

survey_clean_280824.csv
- Cleaned survey data

intake24_clean_280824.csv
- Cleaned Intake24 24-hour dietary recall data
