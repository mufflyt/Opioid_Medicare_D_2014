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
library(ggplot2)
library(ggridges)

# Steps to produce Medicare_Part_D_Opioid_Prescriber_Summary_File_2017
`Medicare_Part_D_Opioid_Prescriber_Summary_File_2017` <- exploratory::read_delim_file("data/Medicare Part D Opioid Prescriber Summary Files/Medicare_Part_D_Opioid_Prescriber_Summary_File_2017.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale = readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/New_York", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert(.) %>% 
  rename(`Extended-Release Opioid Prescribing Rate` = `Long-Acting Opioid Prescribing Rate`) %>%
  rename(`Extended-Release Opioid Claims` = `Long-Acting Opioid Claim Count`) %>%
  exploratory::clean_data_frame(.)

# Steps to produce Medicare_Part_D_Opioid_Prescriber_Summary_File_2016
`Medicare_Part_D_Opioid_Prescriber_Summary_File_2016` <- exploratory::read_delim_file("data/Medicare Part D Opioid Prescriber Summary Files/Medicare_Part_D_Opioid_Prescriber_Summary_File_2016.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale = readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/New_York", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert(.) %>%
  exploratory::clean_data_frame(.)

# Steps to produce Medicare_Part_D_Opioid_Prescriber_Summary_File_2015
`Medicare_Part_D_Opioid_Prescriber_Summary_File_2015` <- exploratory::read_delim_file("data/Medicare Part D Opioid Prescriber Summary Files/Medicare_Part_D_Opioid_Prescriber_Summary_File_2015.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale = readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/New_York", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert(.) %>%
  exploratory::clean_data_frame(.)

# Steps to produce Medicare_Part_D_Opioid_Prescriber_Summary_File_2014
`Medicare_Part_D_Opioid_Prescriber_Summary_File_2014` <- exploratory::read_delim_file("data/Medicare Part D Opioid Prescriber Summary Files/Medicare_Part_D_Opioid_Prescriber_Summary_File_2014.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale = readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Denver", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert(.) %>%
  exploratory::clean_data_frame(.)

# Steps to produce Medicare_Part_D_Opioid_Prescriber_Summary_File_2013
`Medicare_Part_D_Opioid_Prescriber_Summary_File_2013` <- exploratory::read_delim_file("data/Medicare Part D Opioid Prescriber Summary Files/Medicare_Part_D_Opioid_Prescriber_Summary_File_2013.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale = readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/New_York", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert(.) %>%
  exploratory::clean_data_frame(.)

# I separated the last file input just to make it easier to understand
# Steps to produce Medicare_Part_D_Opioid_Prescriber_Summary_File_ALL
Medicare_Part_D_Opioid_Prescriber_Summary_File_ALL <-
  Medicare_Part_D_Opioid_Prescriber_Summary_File_2014 %>%
  rename(`NPPES Provider ZIP Code` = `NPPES Provider Zip Code`) %>%
  rename(`NPPES Provider Last Name` = `NPPES Provider Last/Org Name`) %>%
  bind_rows(
    Medicare_Part_D_Opioid_Prescriber_Summary_File_2013,
    Medicare_Part_D_Opioid_Prescriber_Summary_File_2015,
    Medicare_Part_D_Opioid_Prescriber_Summary_File_2016,
    Medicare_Part_D_Opioid_Prescriber_Summary_File_2017,
    id_column_name = "ID", 
    current_df_name = "Medicare_Part_D_Opioid_Prescriber_Summary_File_2014",
    force_data_type = TRUE
  )

# Steps to produce the output
opioidRX <-  Medicare_Part_D_Opioid_Prescriber_Summary_File_ALL %>%
  mutate_at(vars(`Opioid Prescribing Rate`, `Extended-Release Opioid Prescribing Rate`), funs(parse_number)) %>%
  mutate_at(vars(ID, NPI, `NPPES Provider Last Name`, `NPPES Provider First Name`, `NPPES Provider ZIP Code`, `NPPES Provider State`, `Specialty Description`), funs(factor)) %>%
  arrange(desc(NPI)) %>%
  mutate(`Specialty Description` = factor(`Specialty Description`)) %>%
  filter(`Specialty Description` %in% c("Gynecological/Oncology", "Emergency Medicine", "Geriatric Medicine", "Family Medicine",
                                        "Family Practice", "Internal Medicine", "Orthopaedic Surgery", "Orthopedic Surgery",
                                        "Urology", "Obstetrics & Gynecology", "Obstetrics/Gynecology", 
                                        "Female Pelvic Medicine and Reconstructive Surgery", "Urology", 
                                        "Colon & Rectal Surgery", "Otolaryngy", "Surgical Oncology", "Gynecological/Oncology", 
                                        "General Surgery")) %>%
  mutate(`Specialty Description` = recode(`Specialty Description`, `Family Practice` = "Family Medicine", 
                                          `Orthopaedic Surgery` = "Orthopedic Surgery", 
                                          `Obstetrics/Gynecology` = "Obstetrics & Gynecology")) %>%
  mutate(RXRate = `Opioid Claim Count`/`Total Claim Count`) %>%
  filter(!is.na(RXRate)) %>%
  rename(Specialty = `Specialty Description`) %>%
  mutate(Specialty = factor(Specialty)) %>%
  rename(State = `NPPES Provider State`) %>%
  mutate(State = factor(State)) %>%
  mutate(`NPPES Provider ZIP Code` = factor(`NPPES Provider ZIP Code`)) %>%
  mutate(Specialty = recode(Specialty, `Obstetrics/Gynecology` = "FPMRS", Urology = "FPMRS", 
                            `Gynecological/Oncology` = "FPMRS", `Unknown Physician Specialty Code` = "FPMRS", 
                            `Student in an Organized Health Care Education/Training Program` = "FPMRS", 
                            Specialist = "FPMRS", `Osteopathic Manipulative Medicine` = "FPMRS")) %>%
  rename(Zip = `NPPES Provider ZIP Code`) %>%
  filter(State %nin% c("AA", "AE", "AP", "AS", "GU", "MP", "VI", "ZZ", "XX")) %>%
  mutate(State = fct_drop(State)) %>%
  group_by(Specialty) %>%
  mutate(MedianBySpecialty = median(RXRate, na.rm = TRUE), 
         `Extended-Release Opioid Claims` = impute_na(`Extended-Release Opioid Claims`, type = "value", val = "0"), 
         `Extended-Release Opioid Prescribing Rate` = impute_na(`Extended-Release Opioid Prescribing Rate`,
                                                                type = "value", val = "0")) %>%
  mutate(ID = recode(ID, "Medicare_Part_D_Opioid_Prescriber_Summary_File_2014" = "2014", 
                     "Medicare_Part_D_Opioid_Prescriber_Summary_File_2013" = "2013", 
                     "Medicare_Part_D_Opioid_Prescriber_Summary_File_2015" = "2015", 
                     "Medicare_Part_D_Opioid_Prescriber_Summary_File_2016" = "2016", 
                     "Medicare_Part_D_Opioid_Prescriber_Summary_File_2017" = "2017")) %>%
  rename(Year = ID) %>%
  #mutate(`Opioid Claim Count` = impute_na(`Opioid Claim Count`, type = "value", val = "0"), 
  #`Opioid Prescribing Rate` = impute_na(`Opioid Prescribing Rate`, type = "value", val = "0")) %>%
  mutate_at(vars(`Opioid Claim Count`, `Opioid Prescribing Rate`,
                 `Extended-Release Opioid Claims`, 
                 `Extended-Release Opioid Prescribing Rate`), funs(parse_number)) %>%
  ungroup() 
  # I removed this filter from here because it is only useful for the ridges
  # The heatmap need all specialties
  #  %>% filter(Specialty == "FPMRS")
write_rds(opioidRX, "Data/opioidRX.rds")
#opioidRX <- readr::read_rds("Data/opioidRX.rds")
dim(opioidRX)


### State by State Ridge Plot  ----
# Then I added the filter only when calling the ridge data
FPMRS_OpioidRXState <- opioidRX %>% filter(Specialty == "FPMRS") %>%
  group_by(State) %>% mutate(Median = median(RXRate, na.rm = TRUE))

FPMRS_OpioidRidgePlot <- ggplot(FPMRS_OpioidRXState,  aes(x = RXRate, y = reorder(State, -Median), fill = Median)) + 
  geom_density_ridges(scale = 4, alpha = 0.7, rel_min_height = 0.01, size = .5,
                      # stat = "binline", binwidth=.015, # Uncomment this line to see a squared alternative
                      draw_baseline = F) +
  # theme(axis.title.y = element_blank()) +
  viridis::scale_fill_viridis(discrete = F, option = "B", direction = -1, begin = .1, end = .9, 
                              name = "Median Opioid \nPrescribing Rate") +
  ylab("State") + scale_y_discrete(expand = c(0.01, 0)) + scale_x_continuous(limits = c(0.000001, 0.30)) +
  xlab("Fraction of FPMRS Prescriptions That Were Opioid") + 
  ggtitle("Medicare Part D Opioid Prescription Rate by State (2013 to 2017)\n") +
  guides(fill = guide_colourbar(title.position = "top")) +
  theme_ridges(
    font_size = 14,
    font_family = "",
    line_size = 0.5,
    grid = TRUE,
    center_axis_labels = TRUE
  ) +
  theme(legend.text = element_text(size = 8),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title.align = 0.5, 
        legend.key.width = unit(1, "cm"),
        legend.justification = "center",
        legend.title = element_text(hjust = 0.5, angle = 0, size = 10),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(margin = margin(5, 0, 5, 5), size = 10),
        axis.text.x = element_text(margin = margin(5, 5, 25, 5), hjust = 0, vjust = 0))
FPMRS_OpioidRidgePlot

### FPMRS Opioid boxplot ----
OpioidBoxPlot <- ggplot(FPMRS_OpioidRXState, aes(x = reorder(State, -Median), y = RXRate)) +
  geom_boxplot(outlier.colour = "NA") +
  labs(x = "State", y = "Median Fraction of Dentist's RX That Are Opioid")
OpioidBoxPlot

# Fraction of FPMRS that are Opioids
DentistOpioidMeanMedianPlot <- ggplot(FPMRS_OpioidRXState, aes(x = reorder(State, -Median), y = RXRate)) +
  stat_summary(fun = "median", geom = "point", color = "red") +
  stat_summary(fun = "mean", geom = "point", color = "black") +
  labs(x = "State", y = "Fraction of Dentist's RX That Are Opioid")

DentistOpioidMeanMedianPlot


# Heat map ----
# Read in data with HEAT MAP
OD_DeathRates <- read.csv("data/Drug Overdose Mortality by State.csv", header = TRUE)
OD_DeathRates <- OD_DeathRates %>% filter(YEAR <= 2017 & YEAR >= 2013) %>% group_by(STATE) %>% 
  summarise(Rate = mean(RATE), .groups = 'drop')

# head(OD_DeathRates)
colnames(OD_DeathRates)[colnames(OD_DeathRates) == "STATE"] <- "State"

opioidRX <- merge(opioidRX, OD_DeathRates, by = "State")
table(opioidRX$State)


totalRX <- sum(opioidRX$`Opioid Claim Count`, na.rm = TRUE)

opioidRX$MD_RXRate <- (opioidRX$`Opioid Claim Count`)/(opioidRX$`Total Claim Count`)


opioidRX <- opioidRX %>%
  mutate(MedianBySpecialty = median(MD_RXRate, na.rm = TRUE)) %>% 
  mutate(RXnumberBySpecialty = sum(`Opioid Claim Count`, na.rm = TRUE)) %>% 
  mutate(RXPercentBySpecialty = RXnumberBySpecialty/totalRX, na.rm = TRUE)

names(opioidRX)[19] <- "StateODRate"
# colnames(opioidRX)
# View(opioidRX)

opioidRX$Specialty <- as.factor(opioidRX$Specialty)
opioidRX$State <- as.factor(opioidRX$State)

TidyOpioidRX <- opioidRX %>% group_by(State, Specialty) %>% summarise(Rate, SpecialtyMedian = median(RXRate), .groups = 'drop') %>%
  distinct(State, Specialty, SpecialtyMedian, Rate) %>%
  complete(Specialty, State, fill = list(SpecialtyMedian = 0, Rate = 0)) 

TidyOpioidRX <- TidyOpioidRX %>% group_by(State) %>% mutate(Rate = max(Rate)) %>% filter(State %nin% c('PR', 'DC'))

# TidyOpioidRX <- merge(TidyOpioidRX,  OD_DeathRates, by = "State")
TidyOpioidRX$xlabel <- paste(TidyOpioidRX$State, round(TidyOpioidRX$Rate, 0), sep = ":  ")
# TidyOpioidRX <- TidyOpioidRX %>% arrange(State, SpecialtyMedian)
TidyOpioidRX <- TidyOpioidRX[order(TidyOpioidRX$SpecialtyMedian), ]


OpioidHeatMap <- ggplot(TidyOpioidRX, aes(x = reorder(Specialty, SpecialtyMedian, na.rm = TRUE), 
                                          y = reorder(xlabel, Rate, mean, na.rm = TRUE))) +
  geom_tile(aes(fill = SpecialtyMedian), color = "gray") +
  viridis::scale_fill_viridis(direction = -1, option = "B") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(axis.text.y = element_text(hjust = 0.25, size = 10)) +
  labs(fill = "% Opioid of Total Prescriptions",
       title = "Medicare Opioid Prescription Rates By Specialty & State (2013 to 2017)\n",
       x = "",
       y = "State Overdose Death Rate Per 100,000") +
  coord_fixed(ratio = .30) +
  theme(text = element_text(size = 10), 
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title.align = 0.5, legend.key.width = unit(2, "cm"),
        legend.title = element_text(hjust = 0.5, angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(margin = margin(0, 5, 5, 15))) + guides(fill = guide_colourbar(title.position = "top"))

OpioidHeatMap



