# GEMINI

This repository provides visuallization to compare data from some tables of two CDM.

# TABLE LIST

PERSON

DEATH

VISIT OCCURRENCE

CONDITION OCCURRENCE

DRUG EXPOSURE

DRUG ERA

# HOW TO USE

1. Run SaveData.R to get Standard RDS files
  * Set CDM name
  * Set server infomation and database user infomation

2. Request RDS files to other institution.
  * This RDS files must be stored in 'Target RDS' folder.
  
3. Run GetData.R

4. Run DrawChart.R

5. Run gemini_md.Rmd
