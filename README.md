# Dietary Intake in Scotland’s CHildren (DISH)
## Achievement of the Scottish Dietary Goals, Nutrient and Food Group Intake, and Consumption of Food Beyond the School Gate and Energy Drinks

This repository processes and analyses data from a national survey of children and young people aged 2 to 15 years living in Scotland. 

The main outcomes were:
1. Achievement of the Scottish Dietary Goals
2. Nutrient intake including adherence to the RNIs and LRNIs
3. Food group intake
4. Consumption of food beyond the school gates
5. Consumption of energy drinks

Data were collected using a survey and up to four 24-hour dietary recalls (herein 'intake24').

# Data Management Files
## DISH_cleaning

This R script processes the raw survey and intake24 data: 
- renames variables 
- removes ineligibles and incompletes
- categorises 'other' responses
- creates new variables
- re-codes missing food codes
- removes diet supplements and breast milk
- removes dairy-free items from dairy food groups
- converts intake24 nutrient variables to numeric and replaces NAs with 0s
- removes or cleans recalls completed in <2 minutes or with <=5 items
- cleans or drops energy intakes <400 kcal or >4,000 kcal

## DISH_intake24

This R script calculates:
- total daily intakes of nutrients
- total daily intakes of food groups
- mean participant intakes of nutrients (across 1-4 recalls)
- mean participant intakes of food groups (across 1-4 recalls)
- runs the Multiple Source Method to estimate usual dietary intakes
- adherence to the Scottish Dietary Goals
- meeting the RNIs
- meeting the LRNIs
- consumers of food categories, food groups, and discretionary and additional food groups

### Output
- intake24_participant.csv : Participant level data
- intake24_recall.csv : Recall (daily) level data
- intake24_item.csv : Item level data

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

- survey_26.08.24.xlsx : raw survey data
    
- intake24_26.08.24.xlsx : raw Intake24 24-hour dietary recall data
    

## Clean

- survey_clean.csv : cleaned survey data

- intake24_participant.csv : cleaned Intake24 24-hour dietary recall data at the participant level
- intake24_recall.csv : Recall (daily) level data
- intake24_item.csv : Item level data
