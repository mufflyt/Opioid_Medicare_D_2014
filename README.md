# Opioid_Medicare_D_2014

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
* 
