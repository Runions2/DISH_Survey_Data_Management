# DISH Data Management 

This repository preps and manages the DISH Data, cleaning the data and computing additional variables if interest for Scottish Dietary Goals and Adherence to RNI and LRNIs for nutrients of interest. 

# Data Management Files
Cleaning Script: This script cleans and preps the data for further analysis.

The following is completed within this script: 
- Rename variables
- Checks for NAs and incompletes
- Remove ineligible participants
- Checks for extreme reporting
- Recode missing food codes
- Categorises 'other' responses
- Creates new variables for meal times and meal categories.
- A user characteristic dataset is created including (UserID, HouseholdID, Age, Sex, Ethnicity, SIMD)

Data Management Script: This script creates a recall-level dataset, calculates the daily intakes and mean daily intakes of nutrients. Food groups are created based on NDNS food groups and additional reporting food groups are created based on FSS guidelines and discretionary foods are re-categorised based on FSS discretionary food reporting. 

The following are calculated within this script:
- Daily intakes of nutrients
- Mean daily intakes of nutrients
- Categorisation of food groups
- Categorisation of reporting food groups
- Re-categorisation of milk-containing items and discretionary foods

SDG and RNI/LRNI Adherence Script: This script creates binary variables for adherence to Scottish Dietary Goals and Adherence to RNI and LRNIs. 

- Binary variables for SDG adherence
- Binary variables for RNI and LRNI adherence
- Tables of SDG adherence by age group and SIMD
- Frequency of Vegetables consumed in DISH survey

### Data Files
## Data Cleaning
Initial_survey_05.08.24.xlsx
  - Participant Survey
    
intake24_05.08.24.xlsx 
  - Intake24 Diet Data
    


### Output
## Data Cleaning Output

survey_clean_for_management_200824.csv
- Participant Survey

intake24_clean_for_management_200824.csv
- Intake 24 diet data

user_characteristics.csv 
  - Participant Characteristics from Survey


## Data Management Output
intake24_FS_Energy_20_08_24.csv 
   - Recall level of average daily energy and free sugars and proportion of energy and free sugars by reporting food group

intake24_participant_20_08_24.csv
 - Participant level dataset with mean daily intakes of nutrients and mean daily intakes of discretionary food groups

intake24_recall_20_08_24.csv
- Recall level dataset with daily intakes of nutrients and daily intakes of discretionary food groups

intake24_item_20_08_24.csv
- Intake24 dataset at the food item level with reporting food groups 

## SDG and RNI/LRNI Output


