# Opioid_Medicare_D_2014

USE THE DEV BRANCH FOR GOD'S SAKE.  

Ridge_and_HeatMap.R - Creates the images.  
ReportFromDB.Rmd - Creates the code needed.  


This was the original read.me from Dr. Jenny Listman:
Explore and visualize data on Opioid prescriptions covered by Medicare part D in 2014, by MD, State, Zip. Data set includes, per MD, total number of prescriptions & number of opiate prescriptions.

Read a blog post with a summary of results focusing on Dentist prescribers [here](https://towardsdatascience.com/mississippi-dental-opioid-epicenter-of-2014-ab4d7f68fa49).

Read more about my work on my [website](https://jenny-listman.netlify.com).

File "Medicare_Part_D_Opioid_Prescriber_Summary_File_2014.csv" downloaded from https://data.cms.gov/Medicare-Claims/Medicare-Part-D-Opioid-Prescriber-Summary-File-201/e4ka-3ncx/data.

Description of the file:

"The Centers for Medicare & Medicaid Services (CMS) has prepared a public data set, the Medicare Part D Opioid Prescriber Summary File, which presents information on the individual opioid prescribing rates of health providers that participate in Medicare Part D program. This file is a prescriber-level data set that provides data on the number and percentage of prescription claims (includes new prescriptions and refills) for opioid drugs, and contains information on each provider’s name, specialty, state, and ZIP code. This summary file was derived from the 2014 Part D Prescriber Summary Table (Documentation available at: https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Prescriber_Methods.pdf)"

# Info on geom_density_ridges
The geom geom_density_ridges calculates density estimates from the provided data and then plots those, using the ridgeline visualization. The height aesthetic does not need to be specified in this case.

ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges()

# vroom
I used the vroom package to read in all the drug prescription data for the four different years.  It worked like a charm.  Wow!

# Working through the Dermatology Paper for this.  
Opioid Prescribing Pattenrs and Complications in the Dermatology Medicare Population

# Data 
* NUCC - https://www.nucc.org/index.php/code-sets-mainmenu-41/provider-taxonomy-mainmenu-40/csv-mainmenu-57, National uniform claim committee
* NPI data - https://www.dropbox.com/s/x5bduw6aqx8ht1p/npidata_pfile_20050523-20200913.csv?dl=0
* List of female pelvic medicine and reconstructive surgeons with matched NPI codes - FPMRS_1269_doctors.csv
* Part D Prescriber PUF NPI Drug, Folder containing the 2013, 2014, 2015, and 2016 listings - https://www.dropbox.com/sh/w44i42jncp86ybi/AABVGkzE2vAtRww3Td_k_Woma?dl=0.  These were originally available from CMS.
* Data Dictionary for Part D Prescriber PUF NPI Drug - https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Prescriber_Methods.pdf

* Part D Prescriber data that highlights the total numbers of drug classes prescribed - https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Part-D-Prescriber
* Physicians Shared Patient Data - https://www.nber.org/research/data/physician-shared-patient-patterns-data.  We used the 180 day time windows.  

* Very good review - https://github.com/jasonbxu/psql

# How to plut NPI file into a postgres database
https://github.com/semerj/nppes-postgres

# Clean Data
#### Replace empty "" integer fields in NPI CSV file
$ sed 's/""//g' npidata_pfile_20050523-20201213.csv > npi.csv

### Install csvformat
$ sudo pip install csvkit

#### Convert taxonomy data to utf-8 and tab delimited
$ iconv -c -t utf8 nucc_taxonomy_201.csv | csvformat -T > taxonomy.tab

#### Create npi Database and Import Data
$ createdb -O [USERNAME] [DBNAME]
$ createdb -O tylermuffly npi

$ chmod +x ./create_npi_db.sh
$ chmod a+rX /Users/tylermuffly/Dropbox\ \(Personal\)/obgyn_surg/data/NPI /Users/tylermuffly/Dropbox\ \(Personal\)/obgyn_surg/data/NPI/create_npi_db.sh /Users/tylermuffly/Dropbox\ \(Personal\)/obgyn_surg/data/NPI/npidata_pfile_20050523-20201213.csv /Users/tylermuffly/Dropbox\ \(Personal\)/obgyn_surg/data/NPI/nucc_taxonomy_201.csv /Users/tylermuffly/Dropbox\ \(Personal\)/obgyn_surg/data/NPI/taxonomy.tab /Users/tylermuffly/Dropbox\ \(Personal\)/obgyn_surg/data/NPI/taxonomy.tab /Users/tylermuffly/Dropbox\ \(Personal\)/obgyn_surg/data/NPI/npi.csv

$ ./create_npi_db.sh tylermuffly referral /Users/tylermuffly/Dropbox\ \(Personal\)/obgyn_surg/data/NPI/npi.csv /Users/tylermuffly/Dropbox\ \(Personal\)/obgyn_surg/data/NPI/taxonomy.tab

```r
install.packages("DBI")
install.packages("dbplyr")
install.packages("RSQLite")
install.packages("odbc")

library(DBI) # main DB interface
library(dplyr) 
library(dbplyr) # dplyr back-end for DBs
library(RSQLite)
library(odbc)

########################
#devtools::install_github("r-dbi/DBI")
#remotes::install_github("r-dbi/RPostgres")
library(RPostgres)
library(tidyverse)
library(bakeoff)


# Download a PostgreSQL Database engine
# https://www.postgresql.org/

# Open PG Admin using the username + pswd you set-up
# when installing
# Create a new DB called great_brit_bakeoff_pg

# For the command line:  https://www.codementor.io/@engineerapart/getting-started-with-postgresql-on-mac-osx-are8jcopb
# sudo -u postgres
# postgres=# \du
# CREATE ROLE tylermuffly WITH LOGIN PASSWORD ''; 
# ALTER ROLE tylermuffly CREATEDB;

# psql postgres -U tylermuffly
# CREATE DATABASE great_brit_bakeoff_pg;
# GRANT ALL PRIVILEGES ON DATABASE great_brit_bakeoff_pg TO tylermuffly; 
# \list

# Login as postgres and change tylermuffly privileges
# ALTER ROLE tylermuffly WITH SUPERUSER;

# Log on via terminal:
# psql postgres -U tylermuffly -d referral
# Passwd B1
# \c referral;  #connects to the correct database
# DROP TABLE IF EXISTS npi;
# CREATE TABLE npi
# (
#   NPI integer,
#   Entity_Type_Code varchar(150),
#   Replacement_NPI varchar(150),
#   Employer_Identification_Number varchar(150),
#   Provider_Organization_Name_Legal_Business_Name varchar(150),
#   Provider_Last_Name_Legal_Name varchar(150),
#   Provider_First_Name varchar(150),
#   Provider_Middle_Name varchar(150),
#   Provider_Name_Prefix_Text varchar(150),
#   Provider_Name_Suffix_Text varchar(150),
#   Provider_Credential_Text varchar(150),
#   Provider_Other_Organization_Name varchar(150),
#   Provider_Other_Organization_Name_Type_Code varchar(150),
#   Provider_Other_Last_Name varchar(150),
#   Provider_Other_First_Name varchar(150),
#   Provider_Other_Middle_Name varchar(150),
#   Provider_Other_Name_Prefix_Text varchar(150),
#   Provider_Other_Name_Suffix_Text varchar(150),
#   Provider_Other_Credential_Text varchar(150),
#   Provider_Other_Last_Name_Type_Code varchar(150),
#   Provider_First_Line_Business_Mailing_Address varchar(150),
#   Provider_Second_Line_Business_Mailing_Address varchar(150),
#   Provider_Business_Mailing_Address_City_Name varchar(150),
#   Provider_Business_Mailing_Address_State_Name varchar(150),
#   Provider_Business_Mailing_Address_Postal_Code varchar(150),
#   Provider_Business_Mailing_Address_Country_Code_If_outside_US varchar(150),
#   Provider_Business_Mailing_Address_Telephone_Number varchar(150),
#   Provider_Business_Mailing_Address_Fax_Number varchar(150),
#   Provider_First_Line_Business_Practice_Location_Address varchar(150),
#   Provider_Second_Line_Business_Practice_Location_Address varchar(150),
#   Provider_Business_Practice_Location_Address_City_Name varchar(150),
#   Provider_Business_Practice_Location_Address_State_Name varchar(150),
#   Provider_Business_Practice_Location_Address_Postal_Code varchar(150),
#   Provider_Business_Practice_Location_Address_Country_Code_If_outside_US varchar(150),
#   Provider_Business_Practice_Location_Address_Telephone_Number varchar(150),
#   Provider_Business_Practice_Location_Address_Fax_Number varchar(150),
#   Provider_Enumeration_Date varchar(150),
#   Last_Update_Date varchar(150),
#   NPI_Deactivation_Reason_Code varchar(150),
#   NPI_Deactivation_Date varchar(150),
#   NPI_Reactivation_Date varchar(150),
#   Provider_Gender_Code varchar(150),
#   Authorized_Official_Last_Name varchar(150),
#   Authorized_Official_First_Name varchar(150),
#   Authorized_Official_Middle_Name varchar(150),
#   Authorized_Official_Title_or_Position varchar(150),
#   Authorized_Official_Telephone_Number varchar(150),
#   Healthcare_Provider_Taxonomy_Code_1 varchar(150),
#   Provider_License_Number_1 varchar(150),
#   Provider_License_Number_State_Code_1 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_1 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_2 varchar(150),
#   Provider_License_Number_2 varchar(150),
#   Provider_License_Number_State_Code_2 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_2 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_3 varchar(150),
#   Provider_License_Number_3 varchar(150),
#   Provider_License_Number_State_Code_3 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_3 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_4 varchar(150),
#   Provider_License_Number_4 varchar(150),
#   Provider_License_Number_State_Code_4 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_4 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_5 varchar(150),
#   Provider_License_Number_5 varchar(150),
#   Provider_License_Number_State_Code_5 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_5 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_6 varchar(150),
#   Provider_License_Number_6 varchar(150),
#   Provider_License_Number_State_Code_6 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_6 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_7 varchar(150),
#   Provider_License_Number_7 varchar(150),
#   Provider_License_Number_State_Code_7 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_7 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_8 varchar(150),
#   Provider_License_Number_8 varchar(150),
#   Provider_License_Number_State_Code_8 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_8 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_9 varchar(150),
#   Provider_License_Number_9 varchar(150),
#   Provider_License_Number_State_Code_9 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_9 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_10 varchar(150),
#   Provider_License_Number_10 varchar(150),
#   Provider_License_Number_State_Code_10 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_10 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_11 varchar(150),
#   Provider_License_Number_11 varchar(150),
#   Provider_License_Number_State_Code_11 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_11 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_12 varchar(150),
#   Provider_License_Number_12 varchar(150),
#   Provider_License_Number_State_Code_12 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_12 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_13 varchar(150),
#   Provider_License_Number_13 varchar(150),
#   Provider_License_Number_State_Code_13 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_13 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_14 varchar(150),
#   Provider_License_Number_14 varchar(150),
#   Provider_License_Number_State_Code_14 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_14 varchar(150),
#   Healthcare_Provider_Taxonomy_Code_15 varchar(150),
#   Provider_License_Number_15 varchar(150),
#   Provider_License_Number_State_Code_15 varchar(150),
#   Healthcare_Provider_Primary_Taxonomy_Switch_15 varchar(150),
#   Other_Provider_Identifier_1 varchar(150),
#   Other_Provider_Identifier_Type_Code_1 varchar(150),
#   Other_Provider_Identifier_State_1 varchar(150),
#   Other_Provider_Identifier_Issuer_1 varchar(100),
#   Other_Provider_Identifier_2 varchar(150),
#   Other_Provider_Identifier_Type_Code_2 varchar(100),
#   Other_Provider_Identifier_State_2 varchar(150),
#   Other_Provider_Identifier_Issuer_2 varchar(100),
#   Other_Provider_Identifier_3 varchar(150),
#   Other_Provider_Identifier_Type_Code_3 varchar(100),
#   Other_Provider_Identifier_State_3 varchar(150),
#   Other_Provider_Identifier_Issuer_3 varchar(100),
#   Other_Provider_Identifier_4 varchar(150),
#   Other_Provider_Identifier_Type_Code_4 varchar(150),
#   Other_Provider_Identifier_State_4 varchar(150),
#   Other_Provider_Identifier_Issuer_4 varchar(150),
#   Other_Provider_Identifier_5 varchar(150),
#   Other_Provider_Identifier_Type_Code_5 varchar(150),
#   Other_Provider_Identifier_State_5 varchar(150),
#   Other_Provider_Identifier_Issuer_5 varchar(150),
#   Other_Provider_Identifier_6 varchar(150),
#   Other_Provider_Identifier_Type_Code_6 varchar(150),
#   Other_Provider_Identifier_State_6 varchar(150),
#   Other_Provider_Identifier_Issuer_6 varchar(150),
#   Other_Provider_Identifier_7 varchar(150),
#   Other_Provider_Identifier_Type_Code_7 varchar(150),
#   Other_Provider_Identifier_State_7 varchar(150),
#   Other_Provider_Identifier_Issuer_7 varchar(150),
#   Other_Provider_Identifier_8 varchar(150),
#   Other_Provider_Identifier_Type_Code_8 varchar(150),
#   Other_Provider_Identifier_State_8 varchar(150),
#   Other_Provider_Identifier_Issuer_8 varchar(150),
#   Other_Provider_Identifier_9 varchar(150),
#   Other_Provider_Identifier_Type_Code_9 varchar(150),
#   Other_Provider_Identifier_State_9 varchar(150),
#   Other_Provider_Identifier_Issuer_9 varchar(150),
#   Other_Provider_Identifier_10 varchar(150),
#   Other_Provider_Identifier_Type_Code_10 varchar(150),
#   Other_Provider_Identifier_State_10 varchar(150),
#   Other_Provider_Identifier_Issuer_10 varchar(150),
#   Other_Provider_Identifier_11 varchar(150),
#   Other_Provider_Identifier_Type_Code_11 varchar(150),
#   Other_Provider_Identifier_State_11 varchar(150),
#   Other_Provider_Identifier_Issuer_11 varchar(150),
#   Other_Provider_Identifier_12 varchar(150),
#   Other_Provider_Identifier_Type_Code_12 varchar(150),
#   Other_Provider_Identifier_State_12 varchar(150),
#   Other_Provider_Identifier_Issuer_12 varchar(150),
#   Other_Provider_Identifier_13 varchar(150),
#   Other_Provider_Identifier_Type_Code_13 varchar(150),
#   Other_Provider_Identifier_State_13 varchar(150),
#   Other_Provider_Identifier_Issuer_13 varchar(150),
#   Other_Provider_Identifier_14 varchar(150),
#   Other_Provider_Identifier_Type_Code_14 varchar(150),
#   Other_Provider_Identifier_State_14 varchar(150),
#   Other_Provider_Identifier_Issuer_14 varchar(150),
#   Other_Provider_Identifier_15 varchar(150),
#   Other_Provider_Identifier_Type_Code_15 varchar(150),
#   Other_Provider_Identifier_State_15 varchar(150),
#   Other_Provider_Identifier_Issuer_15 varchar(150),
#   Other_Provider_Identifier_16 varchar(150),
#   Other_Provider_Identifier_Type_Code_16 varchar(150),
#   Other_Provider_Identifier_State_16 varchar(150),
#   Other_Provider_Identifier_Issuer_16 varchar(150),
#   Other_Provider_Identifier_17 varchar(150),
#   Other_Provider_Identifier_Type_Code_17 varchar(150),
#   Other_Provider_Identifier_State_17 varchar(150),
#   Other_Provider_Identifier_Issuer_17 varchar(150),
#   Other_Provider_Identifier_18 varchar(150),
#   Other_Provider_Identifier_Type_Code_18 varchar(150),
#   Other_Provider_Identifier_State_18 varchar(150),
#   Other_Provider_Identifier_Issuer_18 varchar(150),
#   Other_Provider_Identifier_19 varchar(150),
#   Other_Provider_Identifier_Type_Code_19 varchar(150),
#   Other_Provider_Identifier_State_19 varchar(150),
#   Other_Provider_Identifier_Issuer_19 varchar(150),
#   Other_Provider_Identifier_20 varchar(150),
#   Other_Provider_Identifier_Type_Code_20 varchar(150),
#   Other_Provider_Identifier_State_20 varchar(150),
#   Other_Provider_Identifier_Issuer_20 varchar(150),
#   Other_Provider_Identifier_21 varchar(150),
#   Other_Provider_Identifier_Type_Code_21 varchar(150),
#   Other_Provider_Identifier_State_21 varchar(150),
#   Other_Provider_Identifier_Issuer_21 varchar(150),
#   Other_Provider_Identifier_22 varchar(150),
#   Other_Provider_Identifier_Type_Code_22 varchar(150),
#   Other_Provider_Identifier_State_22 varchar(150),
#   Other_Provider_Identifier_Issuer_22 varchar(150),
#   Other_Provider_Identifier_23 varchar(150),
#   Other_Provider_Identifier_Type_Code_23 varchar(150),
#   Other_Provider_Identifier_State_23 varchar(150),
#   Other_Provider_Identifier_Issuer_23 varchar(150),
#   Other_Provider_Identifier_24 varchar(150),
#   Other_Provider_Identifier_Type_Code_24 varchar(150),
#   Other_Provider_Identifier_State_24 varchar(150),
#   Other_Provider_Identifier_Issuer_24 varchar(150),
#   Other_Provider_Identifier_25 varchar(150),
#   Other_Provider_Identifier_Type_Code_25 varchar(150),
#   Other_Provider_Identifier_State_25 varchar(150),
#   Other_Provider_Identifier_Issuer_25 varchar(150),
#   Other_Provider_Identifier_26 varchar(150),
#   Other_Provider_Identifier_Type_Code_26 varchar(150),
#   Other_Provider_Identifier_State_26 varchar(150),
#   Other_Provider_Identifier_Issuer_26 varchar(150),
#   Other_Provider_Identifier_27 varchar(150),
#   Other_Provider_Identifier_Type_Code_27 varchar(150),
#   Other_Provider_Identifier_State_27 varchar(150),
#   Other_Provider_Identifier_Issuer_27 varchar(150),
#   Other_Provider_Identifier_28 varchar(150),
#   Other_Provider_Identifier_Type_Code_28 varchar(150),
#   Other_Provider_Identifier_State_28 varchar(150),
#   Other_Provider_Identifier_Issuer_28 varchar(150),
#   Other_Provider_Identifier_29 varchar(150),
#   Other_Provider_Identifier_Type_Code_29 varchar(150),
#   Other_Provider_Identifier_State_29 varchar(150),
#   Other_Provider_Identifier_Issuer_29 varchar(150),
#   Other_Provider_Identifier_30 varchar(150),
#   Other_Provider_Identifier_Type_Code_30 varchar(150),
#   Other_Provider_Identifier_State_30 varchar(150),
#   Other_Provider_Identifier_Issuer_30 varchar(150),
#   Other_Provider_Identifier_31 varchar(150),
#   Other_Provider_Identifier_Type_Code_31 varchar(150),
#   Other_Provider_Identifier_State_31 varchar(150),
#   Other_Provider_Identifier_Issuer_31 varchar(150),
#   Other_Provider_Identifier_32 varchar(150),
#   Other_Provider_Identifier_Type_Code_32 varchar(150),
#   Other_Provider_Identifier_State_32 varchar(150),
#   Other_Provider_Identifier_Issuer_32 varchar(150),
#   Other_Provider_Identifier_33 varchar(150),
#   Other_Provider_Identifier_Type_Code_33 varchar(150),
#   Other_Provider_Identifier_State_33 varchar(150),
#   Other_Provider_Identifier_Issuer_33 varchar(150),
#   Other_Provider_Identifier_34 varchar(150),
#   Other_Provider_Identifier_Type_Code_34 varchar(150),
#   Other_Provider_Identifier_State_34 varchar(150),
#   Other_Provider_Identifier_Issuer_34 varchar(150),
#   Other_Provider_Identifier_35 varchar(150),
#   Other_Provider_Identifier_Type_Code_35 varchar(150),
#   Other_Provider_Identifier_State_35 varchar(150),
#   Other_Provider_Identifier_Issuer_35 varchar(150),
#   Other_Provider_Identifier_36 varchar(150),
#   Other_Provider_Identifier_Type_Code_36 varchar(150),
#   Other_Provider_Identifier_State_36 varchar(150),
#   Other_Provider_Identifier_Issuer_36 varchar(150),
#   Other_Provider_Identifier_37 varchar(150),
#   Other_Provider_Identifier_Type_Code_37 varchar(150),
#   Other_Provider_Identifier_State_37 varchar(150),
#   Other_Provider_Identifier_Issuer_37 varchar(150),
#   Other_Provider_Identifier_38 varchar(150),
#   Other_Provider_Identifier_Type_Code_38 varchar(150),
#   Other_Provider_Identifier_State_38 varchar(150),
#   Other_Provider_Identifier_Issuer_38 varchar(150),
#   Other_Provider_Identifier_39 varchar(150),
#   Other_Provider_Identifier_Type_Code_39 varchar(150),
#   Other_Provider_Identifier_State_39 varchar(150),
#   Other_Provider_Identifier_Issuer_39 varchar(150),
#   Other_Provider_Identifier_40 varchar(150),
#   Other_Provider_Identifier_Type_Code_40 varchar(150),
#   Other_Provider_Identifier_State_40 varchar(150),
#   Other_Provider_Identifier_Issuer_40 varchar(150),
#   Other_Provider_Identifier_41 varchar(150),
#   Other_Provider_Identifier_Type_Code_41 varchar(150),
#   Other_Provider_Identifier_State_41 varchar(150),
#   Other_Provider_Identifier_Issuer_41 varchar(150),
#   Other_Provider_Identifier_42 varchar(150),
#   Other_Provider_Identifier_Type_Code_42 varchar(150),
#   Other_Provider_Identifier_State_42 varchar(150),
#   Other_Provider_Identifier_Issuer_42 varchar(150),
#   Other_Provider_Identifier_43 varchar(150),
#   Other_Provider_Identifier_Type_Code_43 varchar(150),
#   Other_Provider_Identifier_State_43 varchar(150),
#   Other_Provider_Identifier_Issuer_43 varchar(150),
#   Other_Provider_Identifier_44 varchar(150),
#   Other_Provider_Identifier_Type_Code_44 varchar(150),
#   Other_Provider_Identifier_State_44 varchar(150),
#   Other_Provider_Identifier_Issuer_44 varchar(150),
#   Other_Provider_Identifier_45 varchar(150),
#   Other_Provider_Identifier_Type_Code_45 varchar(150),
#   Other_Provider_Identifier_State_45 varchar(150),
#   Other_Provider_Identifier_Issuer_45 varchar(150),
#   Other_Provider_Identifier_46 varchar(150),
#   Other_Provider_Identifier_Type_Code_46 varchar(150),
#   Other_Provider_Identifier_State_46 varchar(150),
#   Other_Provider_Identifier_Issuer_46 varchar(150),
#   Other_Provider_Identifier_47 varchar(150),
#   Other_Provider_Identifier_Type_Code_47 varchar(150),
#   Other_Provider_Identifier_State_47 varchar(150),
#   Other_Provider_Identifier_Issuer_47 varchar(150),
#   Other_Provider_Identifier_48 varchar(150),
#   Other_Provider_Identifier_Type_Code_48 varchar(150),
#   Other_Provider_Identifier_State_48 varchar(150),
#   Other_Provider_Identifier_Issuer_48 varchar(150),
#   Other_Provider_Identifier_49 varchar(150),
#   Other_Provider_Identifier_Type_Code_49 varchar(150),
#   Other_Provider_Identifier_State_49 varchar(150),
#   Other_Provider_Identifier_Issuer_49 varchar(150),
#   Other_Provider_Identifier_50 varchar(150),
#   Other_Provider_Identifier_Type_Code_50 varchar(150),
#   Other_Provider_Identifier_State_50 varchar(150),
#   Other_Provider_Identifier_Issuer_50 varchar(150),
#   Is_Sole_Proprietor varchar(1),
#   Is_Organization_Subpart varchar(1),
#   Parent_Organization_LBN varchar(150),
#   Parent_Organization_TIN varchar(150),
#   Authorized_Official_Name_Prefix_Text varchar(150),
#   Authorized_Official_Name_Suffix_Text varchar(150),
#   Authorized_Official_Credential_Text varchar(150),
#   Healthcare_Provider_Taxonomy_Group_1 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_2 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_3 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_4 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_5 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_6 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_7 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_8 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_9 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_10 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_11 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_12 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_13 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_14 varchar(150),
#   Healthcare_Provider_Taxonomy_Group_15 varchar(150),
#   Certification_Date varchar(10));
#\d
#\d npi
#\copy npi FROM '/Users/tylermuffly/Movies/npi.csv' DELIMITER ',' CSV HEADER;




# Connect to an existing database 
# to write our data
con <- dbConnect(drv = RPostgres::Postgres(), 
                 host = "localhost", 
                 port = "5432",
                 user = "tylermuffly", 
                 password = "",
                 dbname = "referral")

# Verify Tables were created
DBI::dbListTables(con)

tbl(con, "npi")

dbListFields(con, # Reach into my connection and ...
             "npi")   # tell me what fields does the `bakers` table have?

res <- dbSendQuery(con, "SELECT * FROM npi LIMIT 3") # Execute a query
dbFetch(res) # get the result

#using dplyr
tbl(con, "npi") # let's have a look - works like glimpse()
tbl(con, "npi") %>% 
  head(3) # "SELECT * FROM bakers LIMIT 3"
dbListFields(con, "npi")

tbl(con, "npi") %>% 
  select(npi, entity_type_code, employer_identification_number, provider_last_name_legal_name) # normal dplyr select

tbl(con, "npi") %>% 
  select(series, baker, hometown, series_winner) %>% 
  filter(series_winner == 1) %>% # normal dplyr filter
  count(hometown, sort = TRUE)   # normal dplyr count

tbl(con, "npi") %>% # Reach into my connection, and "talk" to results table
  head(10) %>%          # get me a subset of the data
  # sometimes if there are many columns, some columns are hidden, 
  # this option prints all columns for us
  print(width = Inf)   

tbl(con, "npi") %>% # Reach in and "talk" to baker_results
  head() %>%                  # get a glimpse of data
  collect() %>%               # bring that glimpsed data into R 
  DT::datatable(options = list(scrollX = TRUE)) # force DT horizontal scrollbar

baker_results$series <- as.factor(baker_results$series)

set.seed(42)
head <- tbl(con, "npi") %>% # use connection to "talk" to baker_results
  #inner_join(tbl(con, "results"), # use connection to "talk" to results and join both tables 
             #by = c('baker' = 'baker','series' = 'series')) %>% # join criteria 
  head() %>%
  collect() 
head

#%>% # get it into R
 # sample_n(size = 3) %>% # take a random sample
  #print(width = Inf) # print all columns

class(results$baker)
class(baker_results$baker)

class(results$series)
class(baker_results$series)


dbDisconnect(con)
```
