@echo off
C:
set "PATH=C:\Users\CMADSEN\AppData\Local\Programs\Python\Python311;%PATH%"
set "PATH=C:\Program Files\R\R-4.4.1\bin\x64;%PATH%"
cd C:\Users\CMADSEN\Downloads\LocalR\long_term_projects\ZQMussels

REM Run Python Script
python download_annual_metabase_csv.py

REM Run the P
Rscript update_imdp_data_automatically.R
pause