# Load required packages.
library(dplyr)
library(dbplyr)
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(RcppRoll)
library(tibble)
library(bit64)
library(exploratory)
library(ggplot2)
library(ggridges)
library(knitr)
library(DBI)

`%nin%` <- Negate(`%in%`)
db <-  '/home/dfcoelho/Downloads/tyler.db'
con <- dbConnect(RSQLite::SQLite(), db)
PUF_tb <- tbl(con, "PUF")
SF_tb <- tbl(con, "SummaryFiles")
FPMRS_tb <- tbl(con, "FPMRS")
ACOG_tb <- tbl(con, "Crosswalk_ACOG_Districts")
DOMS_tb <- tbl(con, "DrugOverdoseMortalityByState")
NPPES_tb <- tbl(con, "NPPES")
OPIOIDS_tb <- tbl(con, "OPIOIDS")

# Steps to produce the output

opioidRX <-
  SF_tb %>% collect() %>%
  mutate_at(vars(Year, NPI, NPPESProviderLastName,NPPESProviderFirstName, Zip, 
                 State, Specialty), as.factor) %>%
  filter(Specialty %in% c("Gynecological/Oncology", "Emergency Medicine",
                          "Geriatric Medicine", "Family Medicine",
                          "Family Practice", "Internal Medicine",
                          "Orthopaedic Surgery", "Orthopedic Surgery",
                          "Urology", "Obstetrics & Gynecology",
                          "Obstetrics/Gynecology",
                          "Female Pelvic Medicine and Reconstructive Surgery",
                          "Urology", "Colon & Rectal Surgery", "Otolaryngy",
                          "Surgical Oncology", "Gynecological/Oncology",
                          "General Surgery")) %>% 
  mutate(Specialty = recode(Specialty,
                            `Family Practice` = "Family Medicine",   
                            `Orthopaedic Surgery` = "Orthopedic Surgery",  
                            `Obstetrics/Gynecology` = "Obstetrics & Gynecology")) %>%
  mutate(Specialty = recode(Specialty, `Obstetrics/Gynecology` = "FPMRS", 
                            Urology = "FPMRS", 
                            `Gynecological/Oncology` = "FPMRS", 
                            `Unknown Physician Specialty Code` = "FPMRS", 
                            `Student in an Organized Health Care Education/Training Program` = "FPMRS", 
                            Specialist = "FPMRS", `Osteopathic Manipulative Medicine` = "FPMRS")) %>%
  filter(State %nin% c("AA", "AE", "AP", "AS", "GU", "MP", "VI", "ZZ", "XX")) %>%
  mutate(State = fct_drop(State), Specialty = factor(Specialty)) %>%
  group_by(Specialty) %>%
  mutate(MedianBySpecialty = median(RXRATE, na.rm = TRUE)) %>% ungroup() 


write_rds(opioidRX, "Data/opioidRX.rds")
#opioidRX <- readr::read_rds("Data/opioidRX.rds")
dim(opioidRX)


### State by State Ridge Plot  ----
# Then I added the filter only when calling the ridge data
FPMRS_OpioidRXState <- opioidRX %>% filter(Specialty == "FPMRS") %>%
  group_by(State) %>% mutate(Median = median(RXRATE, na.rm = TRUE))

FPMRS_OpioidRidgePlot <- ggplot(FPMRS_OpioidRXState,  
                                aes(x = RXRATE, y = reorder(State, -Median), 
                                    fill = Median)) + 
  geom_density_ridges(scale = 5, alpha = 0.7, 
                      rel_min_height = 0.01, size = .5) +
  viridis::scale_fill_viridis(discrete = F, option = "B", direction = -1, 
                              begin = .1, end = .9, 
                              name = "Median Opioid \nPrescribing Rate") +
  ylab("State") + scale_y_discrete(expand = c(0.01, 0)) + 
  scale_x_continuous(limits = c(0.000001, 0.30)) +
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
        axis.text.x = element_text(margin = margin(5, 5, 25, 5), hjust = 0, 
                                   vjust = 0))

FPMRS_OpioidRidgePlot

ggsave(path = getwd(), filename = "FPMRS_OpioidRidgePlot.tiff", width = 15, 
       height = 12, device = 'tiff', dpi = 600, bg='white')

### FPMRS Opioid boxplot ----
OpioidBoxPlot <- ggplot(FPMRS_OpioidRXState, aes(x = reorder(State, -Median), y = RXRATE)) +
  geom_boxplot(outlier.colour = "NA") +
  labs(x = "State", y = "Median Fraction of Dentist's RX That Are Opioid") +
  theme_bw()

# OpioidBoxPlot
ggsave(path = getwd(), filename = "OpioidBoxPlot.tiff", width = 15, height = 12, 
       device = 'tiff', dpi = 600)

# Fraction of FPMRS that are Opioids
DentistOpioidMeanMedianPlot <- ggplot(FPMRS_OpioidRXState, 
                                      aes(x = reorder(State, -Median), 
                                          y = RXRATE)) +
  stat_summary(fun = "median", geom = "point", color = "red") +
  stat_summary(fun = "mean", geom = "point", color = "black") +
  labs(x = "State", y = "Fraction of Dentist's RX That Are Opioid") +
  theme_bw()

# DentistOpioidMeanMedianPlot
ggsave(path = getwd(), filename = "DentistOpioidMeanMedianPlot.tiff", width = 15, 
       height = 12, device = 'tiff', dpi = 600)

# Heat map ----
# Read in data with HEAT MAP
OD_DeathRates <- read.csv("data/Drug Overdose Mortality by State.csv", header = TRUE)
OD_DeathRates <- OD_DeathRates %>% filter(YEAR <= 2017 & YEAR >= 2013) %>% group_by(STATE) %>% 
  summarise(ODRate = mean(RATE), .groups = 'drop') %>% rename(State = STATE)

opioidRX <- readr::read_rds("Data/opioidRX.rds")
opioidRX <- merge(opioidRX, OD_DeathRates, by = "State")
table(opioidRX$State)

# TotalRX is the total amount of opioid claims across all specialties, 
# prescribers and years
totalRX <- sum(opioidRX$OpioidClaimCount, na.rm = TRUE)
# MD_RXRATE is the median opioid prescription rate per NPI across all years
opioidRX <- opioidRX %>% mutate(MD_RXRATE = OpioidClaimCount/TotalClaimCount)

opioidRX <- opioidRX %>% group_by(Specialty) %>%
  mutate(OpioidClaimCountBySpecialty = sum(OpioidClaimCount, na.rm = TRUE)) %>% 
  mutate(RXPercentBySpecialty = OpioidClaimCountBySpecialty/totalRX)

opioidRX <- opioidRX %>% mutate(Specialty = as.factor(Specialty)) %>%
  mutate(State = as.factor(State))

TidyOpioidRX <- opioidRX %>% group_by(State, Specialty) %>% 
  summarise(ODRate, SpecialtyMedian = median(RXRATE), .groups = 'drop') %>%
  distinct(State, Specialty, SpecialtyMedian, ODRate) %>%
  complete(Specialty, State, fill = list(SpecialtyMedian = 0, ODRate = 0)) 

TidyOpioidRX <- TidyOpioidRX %>% group_by(State) %>% 
  mutate(ODRate = max(ODRate)) %>% filter(State %nin% c('PR', 'DC'))

TidyOpioidRX$xlabel <- paste0(TidyOpioidRX$State, ":  ",
                              round(TidyOpioidRX$ODRate, 1), " %")
TidyOpioidRX <- TidyOpioidRX[order(TidyOpioidRX$SpecialtyMedian), ]

OpioidHeatMap <- ggplot(TidyOpioidRX, 
                        aes(x = reorder(Specialty, SpecialtyMedian, 
                                        na.rm = TRUE), 
                            y = reorder(xlabel, ODRate, mean, 
                                        na.rm = TRUE))) +
  geom_tile(aes(fill = SpecialtyMedian), color = "gray") +
  viridis::scale_fill_viridis(direction = -1, option = "B") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(axis.text.y = element_text(hjust = 0.25, size = 8)) +
  labs(fill = "% Opioid of Total Prescriptions",
       title = "Medicare Opioid Prescription Rates By Specialty & State (2013 to 2017)\n",
       x = "",
       y = "State Overdose Death Rate Per 100,000") +
  coord_fixed(ratio = .5) +
  theme(text = element_text(size = 10), 
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title.align = 0.5, legend.key.width = unit(2, "cm"),
        legend.title = element_text(hjust = 0.5, angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(margin = margin(0, 5, 5, 15))) + 
  guides(fill = guide_colourbar(title.position = "top"))

# OpioidHeatMap

ggsave(path = getwd(), filename = "OpioidHeatMap.tiff", width = 16, height = 11, 
       device = 'tiff', dpi = 600)

# ggsave(path = getwd(), filename = "OpioidHeatMap_B.tiff", width = 10, 
#        height = 10, device = 'tiff', dpi = 600)

erin_data <- opioidRX %>% group_by(Specialty) %>% 
  mutate(OpioidPrescribingRate = as.numeric(OpioidPrescribingRate)) %>%
  summarise(specialty_median = round(median(OpioidPrescribingRate), 2), 
            specialty_mean = round(mean(OpioidPrescribingRate),2))

write.csv(erin_data, "erin_data.csv")

# erin_data %>% kable()
