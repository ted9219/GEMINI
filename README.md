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

0. In R, install_github() will need auth_token.

Require to wnsuddlsla@gmail.com to get token.

1. Install GEMINI package using install_github().

2. After install, create_rds() to create RDS files.

You can set your own work directory path and create server_info.cfg file to connect DB server.

In server_info.cfg file, you should write down DB server information and DB user information.

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
