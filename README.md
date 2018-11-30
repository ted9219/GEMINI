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

0. Before SaveData.R, you should make server_info.cfg file.

You should create file in work directory by using terminal.

```
touch server_info.cfg
```

or you can create server_info.txt file and then convert .txt to .cfg.

In file, you should write down DB server information and DB user information.

> dbName=<sql server name>
> server=<server IP>
> schemaName=<schema name>
> user=<ID>
> password=<password>

1. Run SaveData.R to get Standard RDS files

This will be create 'Standard RDS', 'target RDS' folder. And if server_info.cfg is correctly able to access DB, create RDS files in 'standard RDS' folder 

2. Request RDS files to other institution.

This RDS files must be stored in 'Target RDS' folder.

> if want to test 'gemini', by copying RDS files in standard RDS folder to target RDS folder.
  
3. Run GetData.R

4. Run DrawChart.R

This process will create gemini_md.md and gemini_md.html file and automatically execute html file.

> If R studio encoding is CP949 (Window default), don't open gemini_md.Rmd file until change R studio encoding to UTF-8. It occurs .Rmd file crash.
