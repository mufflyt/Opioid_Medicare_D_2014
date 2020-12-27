#############################################################3
# Brings in NPI file that contains demographics of physicians: gender, location with zip code, specialty, start date based on Provider.Enumeration.Date.  


# Set libPaths.
.libPaths("/Users/tylermuffly/.exploratory/R/4.0")

# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(bit64)
library(exploratory)
library(english)

# National Uniform Claim Committee
# Crosswalk of all taxonomy codes to their text descriptions.  
# https://www.nucc.org/index.php/code-sets-mainmenu-41/provider-taxonomy-mainmenu-40/csv-mainmenu-57
# Steps to produce nucc_taxonomy_201
`nucc_taxonomy_201` <- exploratory::read_delim_file("data/nucc_taxonomy_201.csv" , ",", quote = "\"", skip = 0, 
                                                    col_names = TRUE , na = c('', 'NA'), locale = readr::locale(
                                                      encoding = "UTF-8",
                                                      decimal_mark = ".",
                                                      tz = "America/Denver",
                                                      grouping_mark = ","
                                                    ),
                                                    trim_ws = TRUE ,
                                                    progress = FALSE) %>% 
  readr::type_convert(.) %>%
  exploratory::clean_data_frame(.) %>%
  filter(Grouping == "Allopathic & Osteopathic Physicians") %>%
  select(-Grouping, -Definition)



# NPPES Data Dissemination file - Listof every physician in the USA with a unique ID number called an NPI (National Provider Identification number).   
# https://download.cms.gov/nppes/NPI_Files.html
# NPPES ----
cls_end <- replicate(270, "NULL")
cls_begin <- replicate(60, NA)
cls <- append(cls_begin, cls_end)

invisible(gc())
NPPES <- vroom("Data/npidata_pfile_20050523-20200913.csv", delim = ",")
NPPES <- read.csv("Data/npidata_pfile_20050523-20200913.csv", stringsAsFactors = FALSE, colClasses = cls) 
invisible(gc())

colnames(NPPES)
# install.packages("beepr")
library(beepr)
beepr::beep(3) # to access exported functions you need to use a ::

#####
NPPES1 <- NPPES %>%
  dplyr::filter("Entity Type Code" == "1") %>% #Remove all hospitals and nursing homes
  dplyr::filter(Provider.Business.Practice.Location.Address.State.Name %nin% c("ZZ", "AS", "FM", "GU", "MH", "MP", "PW", "VI")) %>% #Take out the places are not in the United States
  distinct(NPI, .keep_all = TRUE) %>%
  mutate(Provider.Credential.Text = str_remove_all(Provider.Credential.Text, "[[:punct:]]+")) %>%
  mutate(Provider.Credential.Text = exploratory::str_clean(Provider.Credential.Text)) %>%
  filter(Provider.Business.Practice.Location.Address.Country.Code..If.outside.U.S.. == "US" & Provider.Business.Mailing.Address.Country.Code..If.outside.U.S.. == "US") %>%
  mutate_at(vars(Provider.Last.Name..Legal.Name., Provider.First.Name, Provider.Middle.Name, Provider.Other.Last.Name, Provider.Other.First.Name, Provider.Other.Middle.Name), funs(str_to_title)) %>% 
  tidyr::drop_na(Provider.Last.Name..Legal.Name., Provider.First.Name) %>%
  mutate(Provider.Middle.Name = impute_na(Provider.Middle.Name, type = "value", val = ""), Provider.Middle.Name = exploratory::str_clean(Provider.Middle.Name)) %>%
  mutate(Provider.Business.Mailing.Address.State.Name = str_remove_all(Provider.Business.Mailing.Address.State.Name, "[[:punct:]]+")) %>% 
  filter(Provider.Business.Mailing.Address.State.Name %in% 
           c("AK", "AR", "AL", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", 
             "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", 
             "NC", "ND", "NE", "NH", "NJ", "NV", "NY", "OH", "NM", "OK", "OR", "PA", "PR", "RI", 
             "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV")) %>%
  filter(Provider.Business.Practice.Location.Address.State.Name %in% 
           c("AK", "AR", "AL", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA",
             "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", 
             "NC", "ND", "NE", "NH", "NJ", "NV", "NY", "OH", "NM", "OK", "OR", "PA", "PR", "RI", 
             "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV")) %>%
  distinct(NPI, .keep_all = TRUE) %>%
  filter(Provider.Credential.Text %in% c("MD", "DO")) %>%
  mutate(
    Provider.Name.Suffix.Text = impute_na(Provider.Name.Suffix.Text, type = "value", val = ""),
    Provider.Other.Last.Name = impute_na(Provider.Other.Last.Name, type = "value", val = ""),
    Provider.Other.First.Name = impute_na(Provider.Other.First.Name, type = "value", val = ""),
    Provider.Other.Middle.Name = impute_na(Provider.Other.Middle.Name, type = "value", val = "")
  ) %>% 
  unite(nppes.full.name.1, Provider.First.Name, Provider.Middle.Name, Provider.Last.Name..Legal.Name., sep = " ", remove = FALSE, na.rm = FALSE) %>%
  unite(nppes.full.name.2, Provider.First.Name, Provider.Middle.Name, Provider.Last.Name..Legal.Name., Provider.Name.Suffix.Text, sep = " ", remove = FALSE, na.rm = FALSE) %>%
  unite(nppes.full.name.3, Provider.Other.First.Name, Provider.Other.Middle.Name, Provider.Other.Last.Name, sep = " ", remove = FALSE, na.rm = FALSE) %>%
  unite(nppes.full.name.state, Provider.First.Name, Provider.Middle.Name, Provider.Last.Name..Legal.Name., Provider.Business.Mailing.Address.State.Name, sep = " ", remove = FALSE, na.rm = FALSE) %>%
  mutate(nppes.full.name.state = str_clean(nppes.full.name.state)) %>%
  select(-Entity.Type.Code, -Replacement.NPI, -Employer.Identification.Number..EIN., -Provider.Organization.Name..Legal.Business.Name., -nppes.full.name.state, -Provider.Other.Organization.Name, -Provider.Other.Organization.Name.Type.Code, -Provider.Second.Line.Business.Mailing.Address, -Provider.Business.Mailing.Address.Country.Code..If.outside.U.S.., -Provider.Second.Line.Business.Practice.Location.Address, -Provider.Business.Practice.Location.Address.Country.Code..If.outside.U.S.., -Last.Update.Date, -NPI.Deactivation.Reason.Code, -NPI.Deactivation.Date, -NPI.Reactivation.Date, -Authorized.Official.Last.Name, -Authorized.Official.First.Name, -Authorized.Official.Middle.Name, -Authorized.Official.Title.or.Position, -Authorized.Official.Telephone.Number, -Provider.License.Number_1, -Provider.License.Number.State.Code_1, -Healthcare.Provider.Primary.Taxonomy.Switch_1, -Provider.License.Number_2, -Provider.License.Number.State.Code_2, -Healthcare.Provider.Primary.Taxonomy.Switch_2, -Provider.License.Number_3, -Provider.License.Number.State.Code_3, -Healthcare.Provider.Primary.Taxonomy.Switch_3) %>%
  mutate(Provider.Other.Credential.Text = recode(Provider.Other.Credential.Text, ".D." = "DO", ".D.O." = "DO", ".M.D." = "MD", "(MD)" = "MD", "D,O." = "DO", "D O PA" = "DO", "DO" = "DO", "DO  PA" = "DO", "DO PA" = "DO", "DO PC" = "DO", "DO, MHA" = "DO", "DO, MPH" = "DO", "DO, MS, RPH, FAIHM" = "DO", "DO, PA" = "DO", "DO, PA-C" = "DO", "DO, PHD" = "DO", "DO." = "DO", "M,D," = "MD", "M,D." = "MD", "M. D." = "MD", "M. D.  LLC" = "MD", "M. D., P .A." = "MD", "M.,D." = "MD", "M..D." = "MD", "M.D.,  P.A.," = "MD", "M.D., A.P.M.C." = "MD", "M.D., C.C.D." = "MD", "M.D., F.A.A.P." = "MD", "M.D., F.A.C.O.G" = "MD", "M.D., F.A.C.O.G." = "MD", "M.D., F.A.C.P., P.C." = "MD", "M.D., F.A.C.S." = "MD", "M.B, B.CH., M.H.S." = "MD", "M.B,.B.S." = "MD", "M.B,B.S" = "MD", "M.B." = "MD", "M.B. CH.B." = "MD", "M.B., B.S." = "MD", "M.B.,B.S" = "MD", "M.B.,B.S." = "MD", "M.B.,CH.B." = "MD", "M.B.B.CH" = "MD", "M.B.B.S" = "MD", "M.B.B.S." = "MD", "M.B.B.S. , M.D." = "MD", "M.B.B.S.," = "MD", "M.B.B.S., D.G.O." = "MD", "M.B.B.S.,D.G.O." = "MD", "M.B.CH.B." = "MD", "M.C." = "MD", "M.D" = "MD", "M.D ( PSYCHIATRY)" = "MD", "M.D INC" = "MD", "M.D," = "MD", "M.D, FAAP" = "MD", "M.D, P.A." = "MD", "M.D;" = "MD", "M.D." = "MD", "M.D.  INC" = "MD", "M.D. , INC." = "MD", "M.D. DERMATOLOGY" = "MD", "M.D. FACC" = "MD", "M.D. FACS" = "MD", "M.D. INC" = "MD", "M.D. INC." = "MD", "M.D. LLC" = "MD", "M.D. P.A." = "MD", "M.D. P.C." = "MD", "M.D. PA" = "MD", "M.D. PH.D." = "MD", "M.D. PROFESSION CORP" = "MD", "M.D. PSYCHIATRIST" = "MD", "M.D. S.C." = "MD", "M.D.," = "MD", .default = "MD")) %>%
  mutate_at(vars(Provider.First.Line.Business.Mailing.Address, Provider.Business.Mailing.Address.City.Name, Provider.Business.Practice.Location.Address.City.Name, Provider.Business.Mailing.Address.Postal.Code, Provider.First.Line.Business.Practice.Location.Address), funs(str_to_title)) %>%
  mutate_at(vars(Provider.Business.Mailing.Address.Postal.Code, Provider.Business.Practice.Location.Address.Postal.Code), funs(str_sub(.,1,5))) %>%
  rename(Fax = Provider.Business.Mailing.Address.Fax.Number, Telephone2 = Provider.Business.Practice.Location.Address.Telephone.Number, Fax2 = Provider.Business.Practice.Location.Address.Fax.Number, Telephone1 = Provider.Business.Mailing.Address.Telephone.Number) %>%
  left_join(nucc_taxonomy_201, by = c("Healthcare.Provider.Taxonomy.Code_1" = "Code"), ignorecase = TRUE) %>%
  select(-Healthcare.Provider.Taxonomy.Code_1) %>%
  rename(Taxonomy1.Classification = Classification, Taxonomy1.Specialization = Specialization, Taxonomy1.Notes = Notes) %>%
  left_join(nucc_taxonomy_201, by = c("Healthcare.Provider.Taxonomy.Code_2" = "Code"), ignorecase = TRUE) %>%
  select(-Healthcare.Provider.Taxonomy.Code_2) %>%
  rename(Taxonomy2.Classification = Classification, Taxonomy2.Specialization = Specialization, Taxonomy2.Notes = Notes) %>%
  select(-Healthcare.Provider.Taxonomy.Code_3, -Healthcare.Provider.Taxonomy.Code_4) %>%
  select(-nppes.full.name.2, -Provider.Last.Name..Legal.Name., -Provider.Name.Prefix.Text, -nppes.full.name.3, -Provider.Other.Last.Name, -Provider.Other.First.Name, -Provider.Other.Middle.Name, -Provider.Other.Name.Prefix.Text, -Provider.Other.Name.Suffix.Text, -Provider.Other.Credential.Text, -Provider.Other.Last.Name.Type.Code, -Telephone2, -Fax2) %>%
  filter(!is.na(Taxonomy1.Classification) & Taxonomy1.Classification %in% c("Colon & Rectal Surgery", "Orthopaedic Surgery", "Surgery", "Obstetrics & Gynecology", "Family Medicine", "Internal Medicine", "Emergency Medicine", "General Practice", "Otolaryngology", "Oral & Maxillofacial Surgery", "Urology")) %>%
  mutate(Taxonomy1.Classification = recode(Taxonomy1.Classification, "General Practice" = "Family Medicine")) %>%
  mutate(Provider.Enumeration.Date = mdy(Provider.Enumeration.Date)) %>%
  readr::write_rds("Data/npidata_pfile_20050523-20200913_cleaned.rds")

rm(NPPES)

dim(NPPES1)
colnames(NPPES1)
head(NPPES1)

############################################################################
# Drug Prescriptions ---------------
# Read in all FPMRS
fpmrs_1269 <- read_csv("data/FPMRS_1269_doctors.csv") %>%
  dplyr::select(fullname1, complete_address, Specialty, NPI, Credentials_match, gender, Age, `Year Of Birth`, ACOG_Regions, `Graduation year`, `Zip Code`)
dim(fpmrs_1269)

############################################################################
# Drug Prescriptions ---------------
# Bring in drug data for FPMRS physicians
# Need to inner join the FPMRS NPI numbers with the NPI numbers for the drug prescribers 

#devtools::install_dev("vroom")
library(vroom)

#install.packages("connections")
library(connections)

#Read in all the drug files at once because they are in one directory
library(fs)
fs::dir_ls()
file_paths <- fs::dir_ls("Data/Drugs") #Location of the PUF Drug files downloaded, PartD_Prescriber_PUF_NPI_Drug_13.txt
fs::file_size(file_paths) 

# Read in all years and then do minor cleaning
drugs_all_years <- 
  vroom::vroom(file_paths, delim = "\t", id = "path") %>%
  dplyr::filter(nppes_provider_state %in% c("AR", "AZ", "AL", "AK", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "RI", "PA", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC", "PR")) %>%
  #dplyr::filter(specialty_description %in% c("Obstetrics & Gynecology", "Obstetrics/Gynecology", "Urology")) %>%
  dplyr::mutate_at(vars(drug_name), funs(stringr::str_to_title)) %>%
  dplyr::filter(stringr::str_to_lower(drug_name) %in% stringr::str_to_lower(c("Hydrocodone Bit-Ibuprofen", "Hydrocodone-Acetaminophen", "Oxycodone Hcl-Acetaminophen", "Tramadol Hcl", "Acetaminophen-Codeine", "Ascomp With Codeine", "Butalbital Compound-Codeine", "Fioricet With Codeine", "Codeine Sulfate", "Hydrocodone Bt-Homatropine Mbr", "Hydrocodone-Chlorpheniramine", "Hydrocodone-Homatropine Mbr", "Hydrocodone-Ibuprofen", "Hydromorphone Hcl", "Dilaudid", "Dilaudid-Hp", "Ms Contin", "Oxycontin", "Fentanyl", "Fentanyl Citrate", "Morphine Sulfate", "Morphine Sulfate Er", "Butorphanol Tartrate", "Methadone Hcl", "Oxycodone-Acetaminophen", "Acetaminoph-Caff-Dihydrocodein", "Oxymorphone Hcl", "Oxymorphone Hcl Er", "Belladonna-Opium", "Buprenorphine Hcl", "Carisoprodol Compound-Codeine", "Carisoprodol-Aspirin-Codeine", "Meperidine Hcl", "Butalb-Caff-Acetaminoph-Codein", "Butalbital-Acetaminophen-Caffe", "Acetaminophen-Butalbital", "Butalbital-Aspirin-Caffeine", "Levorphanol Tartrate", "Nalbuphine Hcl", "Oxycodone Concentrate", "Oxycodone Hcl", "Oxycodone Hcl-Aspirin", "Oxycodone Hcl-Ibuprofen", "Pentazocine-Acetaminophen", "Pentazocine-Naloxone Hcl"))) %>%
  dplyr::mutate(specialty_description = dplyr::recode(specialty_description, "Obstetrics & Gynecology" = "Obstetrics/Gynecology", "Obstetrics/Gynecology" = "Obstetrics/Gynecology")) %>%
  #dplyr::inner_join(fpmrs_1269, by = c("npi" = "NPI"))  %>%  inner_join(FPMRS_1269_doctors, by = c("npi" = "NPI"))
  dplyr::arrange(desc(npi)) 
beepr::beep(2)

readr::write_rds(drugs_all_years, "Data/drugs_all_years_filtered.rds")
#readr::read_rds("Data/drugs_all_years_filtered.rds")

colnames(drugs_all_years)
dim(drugs_all_years)


###########
# Inner join with FPMRS only
drugs_all_years$npi <- as.factor(drugs_all_years$npi)
fpmrs_1269$NPI <- as.factor(fpmrs_1269$NPI)

drugs_all_years_fpmrs_only <- drugs_all_years %>%
  dplyr::inner_join(fpmrs_1269, by = c("npi" = "NPI")) %>%
  mutate(ACOG_Regions = recode(ACOG_Regions, "KA" = "District XII"))
dim(drugs_all_years_fpmrs_only)

readr::write_rds(drugs_all_years_fpmrs_only, "Data/drugs_all_years_fpmrs_only.rds")

dim(drugs_all_years_fpmrs_only)
colnames(drugs_all_years_fpmrs_only)
View(head(drugs_all_years_fpmrs_only))


#############################################################################################3
# Start the results section here please.  

#Number of FPMRS who prescribed narcotics.  
dim(opioidRX)
opioidRX$NPI <- as.factor(opioidRX$NPI)
fpmrs_1269$NPI <- as.factor(fpmrs_1269$NPI)
opioidRX$`Opioid Claim Count` <- as.numeric(opioidRX$`Opioid Claim Count`)

opioidRX_fpmrs_only <- opioidRX %>%
  dplyr::inner_join(fpmrs_1269, by = c("NPI" = "NPI")) 
dim(opioidRX_fpmrs_only)

# Number of unique FPMRS in database
(number_of_unique_fpmrs_opioid_prescribers <- nrow(opioidRX_fpmrs_only %>% distinct(NPI, .keep_all = TRUE)))

# Number of unique FPMRS who prescribed ZERO opioids
zero_prescribed_opioids <- nrow(opioidRX_fpmrs_only %>% 
                                  distinct(NPI, .keep_all = TRUE) %>%
                                  filter(`Opioid Claim Count` == 0))

percent_zero_prescribed_opioids = round(zero_prescribed_opioids/number_of_unique_fpmrs_opioid_prescribers, 2)*100

# Number of unique FPMRS who at least one prescribed opioids
at_least_ten_prescribed_opioids <- nrow(opioidRX_fpmrs_only %>% 
                                          distinct(NPI, .keep_all = TRUE) %>%
                                          filter(`Opioid Claim Count` >= 10))

percent_at_least_ten_prescribed_opioids = round(at_least_ten_prescribed_opioids/number_of_unique_fpmrs_opioid_prescribers, 2)*100
  
#For manuscript:  
print(paste0("A total of ", format(number_of_unique_fpmrs_opioid_prescribers, big.mark = ","), " female pelvic medicine and reconstructive surgeons were identified using the Part D Opioid Prescriber Summary File:", zero_prescribed_opioids, " (",percent_zero_prescribed_opioids,"%) prescribed 0 opioid claims, and ", at_least_ten_prescribed_opioids," (", percent_at_least_ten_prescribed_opioids,"%) prescribed more than 10 opioid claims."))


######################
#Number of FPMRS in drugs database
number_of_FPMRS <- nrow(drugs_all_years_fpmrs_only %>%
  distinct(npi, .keep_all = TRUE))

#Number of beneficiaries prescribed opioids
beneficiaries <- sum(drugs_all_years_fpmrs_only$bene_count, na.rm = TRUE)
beneficiaries

#Number of opioid claims
number_of_opioid_claims <- sum(drugs_all_years_fpmrs_only$total_claim_count, na.rm = TRUE)
number_of_opioid_claims

#Opioid claims per FPMRS, mean [sd]
number_of_opioid_claims_per_FPMRS <- round((number_of_opioid_claims / number_of_FPMRS), 2)
number_of_opioid_claims_per_FPMRS
mean(number_of_opioid_claims_per_FPMRS) # I think I screwed this up here.  Can't get SD.  ???????????????
sd(number_of_opioid_claims_per_FPMRS)

#Number of beneficiaries per FPMRS
beneficiaries_per_FPMRS <- round((beneficiaries / number_of_FPMRS), 1)
sd(drugs$Number_of_bene_per_FPMRS) #Not right.  ????????????????

#Mean days’ supply per opioid claim
Mean_days_supply_per_opioid_claim = round(mean(drugs_all_years_fpmrs_only$total_day_supply/drugs_all_years_fpmrs_only$bene_count, na.rm = TRUE), 1)
Mean_days_supply_per_opioid_claim

#Mean opioid claims per beneficiary
Mean_opioid_claims_per_beneficiary = round((number_of_opioid_claims/beneficiaries),1)
Mean_opioid_claims_per_beneficiary

# Type of opioid, No. 
# Hydrocodone bitartrate and acetaminophen
Number_of_opioids_Hydrocodone_Acetaminophen = sum(nrow(drugs_all_years_fpmrs_only %>% filter(drug_name == "Hydrocodone-Acetaminophen")))
class(Number_of_opioids_Hydrocodone_Acetaminophen)
percent_of_opioids_Hydrocodone_Acetaminophen = round(Number_of_opioids_Hydrocodone_Acetaminophen / (sum(nrow(drugs_all_years_fpmrs_only))),3)*100
percent_of_opioids_Hydrocodone_Acetaminophen

# Type of opioid, No. 
# Oxycodone Hcl-Acetaminophen
Number_of_opioids_Oxycodone_Hcl_Acetaminophen = sum(nrow(drugs_all_years_fpmrs_only %>% filter(drug_name %in% c("Oxycodone-Acetaminophen", "Oxycodone Hcl-Acetaminophen"))))
Number_of_opioids_Oxycodone_Hcl_Acetaminophen
percent_of_opioids_Oxycodone_Hcl_Acetaminophen = round(Number_of_opioids_Oxycodone_Hcl_Acetaminophen / (sum(nrow(drugs_all_years_fpmrs_only))),3)*100
percent_of_opioids_Oxycodone_Hcl_Acetaminophen

# Type of opioid, No. 
# Tramadol Hcl
Number_of_opioids_Tramadol = sum(nrow(drugs_all_years_fpmrs_only %>% filter(drug_name == "Tramadol Hcl")))
Number_of_opioids_Tramadol
percent_of_opioids_Tramadol = round(Number_of_opioids_Tramadol / (sum(nrow(drugs_all_years_fpmrs_only))),3)*100
percent_of_opioids_Tramadol

# Type of opioid, No. 
# Hydrocodone Bit-Ibuprofen
Number_of_opioids_Oxycodone_Hcl = sum(nrow(drugs_all_years_fpmrs_only %>% filter(drug_name == "Oxycodone Hcl")))
Number_of_opioids_Oxycodone_Hcl
percent_of_opioids_Oxycodone_Hcl = round(Number_of_opioids_Oxycodone_Hcl / (sum(nrow(drugs_all_years_fpmrs_only))),3)*100
percent_of_opioids_Oxycodone_Hcl

######
#For manuscript:  
print(paste0("Among FPMRS prescribing at least 10 opioid claims in the Part D Prescriber PUF, a total of ", format(number_of_opioid_claims, big.mark = ","), " opioid claims were prescribed to ", format(beneficiaries, big.mark = ","), " beneficiaries (Table 1). Each FPMRS prescribed a mean (SD) of ", format(number_of_opioid_claims_per_FPMRS, big.mark = ",")," (SD???) opioid claims to ", beneficiaries_per_FPMRS," (??) beneficiaries. Each beneficiary received a mean of", Mean_opioid_claims_per_beneficiary," opioid claims, with a supply lasting a mean of ", Mean_days_supply_per_opioid_claim," days. Combination hydrocodone and acetaminophen accounted for ", format(Number_of_opioids_Hydrocodone_Acetaminophen, big.mark = ",")," opioid claims (", percent_of_opioids_Hydrocodone_Acetaminophen,"%), oxycodone acetaminophen ", format(Number_of_opioids_Oxycodone_Hcl_Acetaminophen, big.mark = ","),"  opioid claims (", percent_of_opioids_Oxycodone_Hcl_Acetaminophen,"%), followed by tramadol hydrochloride ",Number_of_opioids_Tramadol," opioid claims (", percent_of_opioids_Tramadol,"%), and lastly oxycodone ", Number_of_opioids_Oxycodone_Hcl," opioid claims (", percent_of_opioids_Oxycodone_Hcl,"%)."))

###
#Top 10 FPMRS prescribing the most opioids
top10_opioid_prescribers <- drugs_all_years_fpmrs_only %>% top_n(10, total_claim_count)
top10_opioid_prescribers

percent_claims_top10_opioid_prescribers <- round(((sum(nrow(top10_opioid_prescribers)) / number_of_opioid_claims) * 100), 2)
percent_claims_top10_opioid_prescribers

#Top 10 Number of opioid claims
top_10_number_of_opioid_claims <- sum(top10_opioid_prescribers$total_claim_count, na.rm = TRUE)
top_10_number_of_opioid_claims

#Top 10 Number of beneficiaries
top_10_beneficiaries <- sum(top10_opioid_prescribers$bene_count, na.rm = TRUE)
top_10_beneficiaries

#Top 10 Mean number of opioid claims per beneficiary
top_10_mean_claims <- round(mean(top10_opioid_prescribers$bene_count, na.rm = TRUE),1)
top_10_mean_claims

#Top 10 Each beneficiary received a mean of x opioid claims
Top_10_Mean_opioid_claims_per_beneficiary = round(top_10_beneficiaries/top_10_mean_claims,1)
Top_10_Mean_opioid_claims_per_beneficiary

#Top 10 Mean days’ supply per opioid claim
Top_10_Mean_days_supply_per_opioid_claim = mean(top10_opioid_prescribers$total_day_supply)
####???????????????
Top_10_Mean_days_supply_per_opioid_claim

#For manuscript:
print(paste0("We next identified the top 10 FPMRS prescribing opioids. These FPMRS accounted for ", percent_claims_top10_opioid_prescribers, "% of opioid claims and prescribed a total of ", format(top_10_number_of_opioid_claims, big.mark = ",")," opioid claims. Each FPMRS prescribed a mean (SD) of ", top_10_mean_claims," (SD?????????) opioid claims, and each beneficiary received a mean of ", Top_10_Mean_opioid_claims_per_beneficiary," opioid claims, with a supply of ", format(Top_10_Mean_days_supply_per_opioid_claim, big.mark = ",")," days."))

##########################################################################
#For Manuscript:
top10_opioid_prescribers

colnames(top10_opioid_prescribers)
colnames(NPPES1)

NPPES1$NPI <- as.factor(NPPES1$NPI)
top10_opioid_prescribers$npi <- as.factor(top10_opioid_prescribers$npi)

#Inner join between top10_opioid_prescribers and NPI with unique NPI number for one 
demographics_top_10 <- top10_opioid_prescribers %>%
  inner_join(NPPES1, by = c("npi" = "NPI")) %>%
  distinct(npi, .keep_all = TRUE)
dim(demographics_top_10)

top_10_male <- (sum(nrow(demographics_top_10 %>% filter(Provider.Gender.Code == "M"))))
top_10_male

perc_top_10_male <- ((sum(nrow(demographics_top_10 %>% filter(Provider.Gender.Code == "M"))) / top_10_male)) * 100
perc_top_10_male

perc_other_90 <- drugs_all_years_fpmrs_only %>% top_n(-90, bene_count) %>% sample_rows(5, replace = TRUE, seed = 123456)

perc_other_90_gender_denominator <- sum(nrow(perc_other_90))
perc_other_90_gender_male <- sum(nrow(perc_other_90 %>% filter(gender == "Male")))
perc_perc_male_gender <- (perc_other_90_gender_male/perc_other_90_gender_denominator) * 100
perc_perc_male_gender

print(paste0("Among the top 1% of FPMRS prescribing opioids, ", top_10_male," (", perc_top_10_male,"%) were male physicians, compared with ", perc_other_90_gender_male," of ", perc_other_90_gender_denominator," (",perc_perc_male_gender,"%) in a random sample of the same size from the remaining FPMRS prescribing more than 10 opioid claims (P= 0.???? Diego I'm not sure how to do this????) (Table 2)."))



##########################################################################
# ANOVA of ACOG region and total claim count
two.way <- stats::aov(total_claim_count ~ ACOG_Regions, data = drugs_all_years_fpmrs_only)
summary(two.way)

#From the post-hoc test results, we see that there are significant differences (p < 0.05)
tukey.two.way <- TukeyHSD(two.way)
tukey.two.way

#The significant groupwise differences are any where the 95% confidence interval doesn’t include zero.
tukey.plot.aov <- aov(total_claim_count ~ ACOG_Regions, data = drugs_all_years_fpmrs_only)
tukey.plot.test <- TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#For Manuscript:
print(paste0("FPMRS in ACOG district XII prescribed higher numbers of opioid claims per 1000 Medicare beneficiaries, compared with FPMRS in the other ACOG districts (p < 0.01, Figure). ????Diego I'm not sure how to do this??????  Aggregated by region, FPMRS in the South prescribed 2.77 opioid claims per 1000 Medicare beneficiaries, compared with 1.60 in the West, 0.89 in the Midwest, and 0.83 in the Northeast (eTable 2 in the Supplement).????Diego I'm not sure how to do this??????"))

###########################
continue_opioids_past_one_year <- format(round(beneficiaries * 0.051, digits = 0), big.mark = ",") 
# Table 3 Opioid use at 1 y, %
continue_opioids_past_one_year

continue_opioids_paste_three_year_low <- format(round(beneficiaries * 0.024, digits = 0), big.mark = ",") 
# Table 3 Opioid use at 1 y, %
continue_opioids_paste_three_year_low

continue_opioids_paste_three_year_high <- format(round(beneficiaries * 0.053, digits = 0), big.mark = ",") 
# Table 3 Opioid use at 1 y, %
continue_opioids_paste_three_year_high


gi_opioids_side_effects_low <-
  format(round(beneficiaries * 0.13, digits = 0), big.mark = ",")
gi_opioids_side_effects_low
gi_opioids_side_effects_high <-
  format(round(beneficiaries * 0.30, digits = 0), big.mark = ",")
gi_opioids_side_effects_high

fracture_opioids_side_effects_low <-
  format(round(beneficiaries * (531 / (1000 * 4)), digits = 0), big.mark =
           ",") #531 per person-years, The calculation can be accomplished by adding the number of patients in the group and multiplying that number times the years that patients are in a study in order to calculate the patient-years (denominator). Then divide the number of events (numerator) by the denominator
# ???????????I don't think this is right Diego.  I've got to figure out person-years.  ????????
fracture_opioids_side_effects_low

fracture_opioids_side_effects_high <-
  format(round(beneficiaries * (902 / 1000 * 4), digits = 0), big.mark = ",") #902 per person-years
# ?????????????????????????

leftover_opioid_pills <-
  format(round(
    sum(drugs_all_years_fpmrs_only$total_claim_count) * 0.054,
    digits = 0
  ), big.mark = ",") #Leftover pills, 5.4% No. of leftover pills per claim

#For Manuscript:
print(
  paste0(
    "The frequencies of several risks associated with opioid use in the elderly, including addiction, gastrointestinal tract adverse effects (including constipation, nausea, and vomiting), central nervous system adverse effects (including dizziness, somnolence, and unsteadiness), and fracture, were identified from the literature (Table 3). We estimate that ",
    continue_opioids_past_one_year,
    " beneficiaries could continue to use opioids one year after their prescription and ",
    continue_opioids_paste_three_year_low,
    " to ",
    continue_opioids_paste_three_year_high,
    " may continue to use them three years after their prescription. A total of ",
    gi_opioids_side_effects_low,
    " to ",
    gi_opioids_side_effects_high ,
    " beneficiaries could experience gastrointestinal tract or central nervous system adverse effects and ???",
    fracture_opioids_side_effects_low,
    " to ?????",
    fracture_opioids_side_effects_high,
    " ?????could experience fractures. We also estimate a remainder of ",
    leftover_opioid_pills,
    " unused opioid pills in the community."
  )
)

