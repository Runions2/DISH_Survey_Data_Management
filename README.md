# Dietary Intake in Scotland’s CHildren (DISH)

These R scripts process and analyse data from a cross-sectional representative survey of children and young people aged 2 to 15 years living in Scotland collected in 2024. 

In some instances, code has been removed to prevent identifiability, for example, in re-coding text responses for 'Other' ethnicity. 

The final processed data files and meta data are available for download at https://www.data-archive.ac.uk/. 

The main outcomes of the survey were:
1. Achievement of the Scottish Dietary Goals
2. Nutrient intake including adherence to the Reference Nutrient Intakes (RNIs) and Lower Reference Nutrient Intakes (LRNIs)
3. Food group intake and contribution to energy and nutrient intake
4. Consumption of food beyond the school gates in secondary school pupils
5. Consumption of energy drinks in secondary school pupils
6. Food insecurity 

Data were collected using an online survey and up to four 24-hour dietary recalls (herein 'intake24').

More information about the survey is available at www.edin.ac/intake24. 

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

### Output
- survey_clean.csv : Participant level survey data for analysis (available for download at https://www.data-archive.ac.uk/)
  
## DISH_intake24

This R script calculates:
- total daily intakes of nutrients
- total daily intakes of food groups
- mean participant intakes of nutrients (across 1-4 recalls)
- mean participant intakes of food groups (across 1-4 recalls)
- runs the Multiple Source Method to estimate usual dietary intakes (more information about this method is available here: https://msm.dife.de/)
- adherence to the Scottish Dietary Goals
- meeting the RNIs
- meeting the LRNIs
- consumers of food categories, food groups, and discretionary and additional food groups

### Output
- intake24_participant.csv : Participant level diet data for analysis (available for download at https://www.data-archive.ac.uk/)
- intake24_recall.csv : Recall (daily) level diet data for analysis (available for download at https://www.data-archive.ac.uk/)
- intake24_item.csv : Item level diet data for analysis (available for download at https://www.data-archive.ac.uk/)

## DISH_results

This R script exports the following survey-weighted outputs into Excel tables overall and for population subgroups (age, sex, Scottish Index of Multiple Deprivation):
- participant characteristics
- Scottish Dietary Goals
- nutrient intakes
- meeting the RNIs
- meeting the LRNIs
- food group intakes
- consumption of food beyond the school gates
- consumption of energy drinks
- food insecurity


# Data Files
## Raw

- survey_26.08.24.xlsx : raw survey data
- intake24_26.08.24.xlsx : raw Intake24 24-hour dietary recall data
- full_sample_health_boards.xlsx : raw health boards
- UK_NDB_1.2 2023.09 : Nutrient Databank used for Intake24 data
- sample_survey_weights_081024 : sample survey weights

Raw data files are not available for download as some variables together could potentially be used to identify an individual respondent. 

## Clean

- survey_clean.csv : cleaned survey data
- intake24_participant.csv : cleaned Intake24 24-hour dietary recall data at the participant level
- intake24_recall.csv : Recall (daily) level data
- intake24_item.csv : Item level data

Clean data files are available for download at https://www.data-archive.ac.uk/
