# Scottish Dietary Goals in DISH Survey (2024)

Analysis of the Scottish Dietary Goals among children 2-15y in the DISH Survey

# Data Management Files
This script calculates the daily intakes and average daily intakes of nutrients for participants in the DISH survey. Food groups are created based on NDNS food groups and further reporting food groups are created based on FSS guidelines and discretionary foods are re-categorised based on FSS discretionary food reporting. 

The following are calculated within this script:
- Daily intakes of nutrients
- Mean daily intakes of nutrients
- Categorisation of food groups
- Categorisation of reporting food groups
- Re-categorisation of milk-containing items and discretionary foods
- Binary variables for SDG adherence
- Tables of SDG adherence by age group and SIMD
- Frequency of Vegetables consumed in DISH survey

### Data Files

Survey_clean_for_Analysis.csv 
  - Participant Survey
    
intake24_clean_for_Analysis.csv 
  - Intake24 Diet Data
    
user_characteristics.csv 
  - Participant Characteristics from Survey

### Output

intake24_FS_Energy_14_05_24.csv 
   - Average daily energy and free sugars and proportion of energy and free sugars by reporting food group

intake24_intake_foodgroups_SDG_14_05_24.csv
 - Recall level dataset with food groups and SDG adherence binary variables



