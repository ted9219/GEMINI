# GEMINI

This repository provides visuallization to compare data from some tables of two CDM.

# TABLE LIST

PERSON

DEATH

VISIT OCCURRENCE

CONDITION OCCURRENCE

DRUG EXPOSURE

DRUG ERA

(OBSERVATION_PERIOD)

# HOW TO USE

1. Install GEMINI package using install_github().

```
install.packages("devtools")
library(devtools)
install_github("https://github.com/ABMI/GEMINI.git")
library(gemini)
```

> Until merging, you should be use this code when install package.

```
install_github("https://github.com/ABMI/GEMINI.git", ref="gemini_temp")
```

2. After install, create_rds() to create RDS files.

You can set your own work directory path and create server_info.cfg file to connect DB server.

What you type down in Rstudio the server_info.cfg file in the following format.


```
dbName=<sql server name>
server=<server IP>
schemaName=<schema name>
user=<ID>
password=<password>
```

If you already have RDS files, put them in 'Standard RDS' folder and follow next process.

3. Request RDS files to other institution.

This RDS files must be stored in 'Target RDS' folder.

Just standard rds files can run GEMINI but line graph will be shown single institution rds data.
  
4. Run Gemini()

You should set path where 'Standard RDS' folder and 'Target RDS' folder exist.

Then GEMINI will create images folder(with image files), markdown file and html file and will appear browser to show you a report(html file) what GEMINI made.

If data is NULL (or image file shows 'No Data'), check the CDM data really it NULL.

> If R studio encoding is CP949 (Window default), don't open gemini_md.Rmd file until change R studio encoding to UTF-8. It occurs .Rmd file crash.
