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
#db <-  '/home/dfcoelho/Downloads/tyler.db'
db <-'/Volumes/Video Projects Muffly 1/Opioids/tyler.db'
con <- dbConnect(RSQLite::SQLite(), db)
PUF_tb <- tbl(con, "PUF")
SF_tb <- tbl(con, "SummaryFiles")
FPMRS_tb <- tbl(con, "FPMRS")
ACOG_tb <- tbl(con, "Crosswalk_ACOG_Districts")
DOMS_tb <- tbl(con, "DrugOverdoseMortalityByState")
NPPES_tb <- tbl(con, "NPPES")
OPIOIDS_tb <- tbl(con, "OPIOIDS")

# Steps to produce the output

FPMRS <- FPMRS_tb %>% collect()
FPMRS_NPIs <- unlist(FPMRS$NPI)
OPIOIDS <- OPIOIDS_tb %>% collect()
OPIOIDS_list <-  as.list(OPIOIDS) %>% unlist(use.names = FALSE) 

opioidRX <-
  PUF_tb %>% 
  filter(!is.na(bene_count)) %>% filter(!is.na(total_claim_count)) %>% 
  collect() %>%
  rename(Specialty = specialty_description) %>%
  rename(State = nppes_provider_state) %>%
  rename(NPPESProviderLastName = nppes_provider_last_org_name) %>%
  rename(NPPESProviderFirstName = nppes_provider_first_name) %>%
  mutate_at(vars(Year, NPI, NPPESProviderLastName,
                 NPPESProviderFirstName,  
                 State, Specialty), as.factor) %>%
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
  mutate(opioids = ifelse(drug_name %in% OPIOIDS_list | generic_name %in% OPIOIDS_list, total_claim_count, 0)) 

opioidRX <- opioidRX %>%
  group_by(NPI, State, Specialty) %>%
  summarise(OpioidClaimCount = sum(opioids, na.rm = TRUE),
            bene_count = sum(bene_count, na.rm = TRUE),
            TotalClaimCount = sum(total_claim_count, na.rm = TRUE)) %>%
  mutate(RXRATE = OpioidClaimCount/TotalClaimCount) %>%
  group_by(Specialty) %>%
  mutate(MedianBySpecialty = median(RXRATE, na.rm = TRUE)) %>% ungroup() 

write_rds(opioidRX, "Data/opioidRX.rds")
opioidRX <- opioidRX %>% filter(NPI %in% FPMRS_NPIs) 
#opioidRX <- readr::read_rds("Data/opioidRX.rds")
dim(opioidRX)

### State by State Ridge Plot  ----
# Then I added the filter only when calling the ridge data
FPMRS_OpioidRXState <- opioidRX %>% #filter(Specialty == "FPMRS") %>%
  group_by(State) %>% mutate(Median = median(RXRATE, na.rm = TRUE)) %>%
  mutate(N=n())

FPMRS_OpioidRidgePlot <- ggplot(FPMRS_OpioidRXState,  
                                aes(x = RXRATE, y = reorder(State, -Median), 
                                    fill = Median)) + 
  geom_density_ridges(scale = 15, alpha = 0.7, jittered_points = TRUE, 
                      point_alpha=1, point_shape=21, 
                      rel_min_height = 0.001, size = .5) +
  viridis::scale_fill_viridis(discrete = F, option = "B", direction = -1, 
                              begin = .1, end = .9, 
                              name = "Median Opioid \nPrescribing Rate") +
  ylab("State") +
  # geom_point(data=subset(FPMRS_OpioidRXState, N < 3), 
  #            aes(fill = Median, color=Median), shape=13) +
  scale_y_discrete(expand = c(0.01, 0)) + 
  scale_x_continuous(limits = c(-.1, 1)) +
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

opioidRX <- opioidRX %>% filter(Specialty %in% c("Gynecological/Oncology", "Emergency Medicine",
                        "Geriatric Medicine", "Family Medicine",
                        "Family Practice", "Internal Medicine",
                        "Orthopaedic Surgery", "Orthopedic Surgery",
                        "Urology", "Obstetrics & Gynecology",
                        "Obstetrics/Gynecology", "FPMRS",
                        "Female Pelvic Medicine and Reconstructive Surgery",
                        "Urology", "Colon & Rectal Surgery", "Otolaryngy",
                        "Surgical Oncology", "Gynecological/Oncology",
                        "General Surgery")) 

# TotalRX is the total amount of opioid claims across all specialties, 
# prescribers and years
# totalRX <- sum(opioidRX$OpioidClaimCount, na.rm = TRUE)
# MD_RXRATE is the median opioid prescription rate per NPI across all years
# opioidRX <- opioidRX %>% mutate(MD_RXRATE = OpioidClaimCount/TotalClaimCount)

# opioidRX <- opioidRX %>% group_by(Specialty) %>%
#   mutate(OpioidClaimCountBySpecialty = sum(OpioidClaimCount, na.rm = TRUE)) %>% 
#   mutate(RXPercentBySpecialty = OpioidClaimCountBySpecialty/totalRX)

opioidRX <- opioidRX %>% mutate(Specialty = as.factor(Specialty)) %>%
  mutate(State = as.factor(State))

TidyOpioidRX <- opioidRX %>% group_by(State, Specialty) %>% 
  filter(State %nin% c('PR', 'DC')) %>% 
  summarise(ODRate = max(ODRate), SpecialtyMedian = median(RXRATE)) %>%
  mutate(Specialty = as.factor(as.character(Specialty))) %>% ungroup() 

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

OpioidHeatMap
ggsave(path = getwd(), filename = "OpioidHeatMap.tiff", width = 16, height = 11, 
       device = 'tiff', dpi = 600)

# ggsave(path = getwd(), filename = "OpioidHeatMap_B.tiff", width = 10, 
#        height = 10, device = 'tiff', dpi = 600)

erin_data <- opioidRX %>% group_by(Specialty) %>% 
  summarise(specialty_median = round(median(RXRATE), 2), 
            specialty_mean = round(mean(RXRATE),2))

erin_data_b <- opioidRX %>%
  group_by(Specialty, NPI) %>%
  summarise(.groups = "drop",
            TotalClaimCount = sum(TotalClaimCount),
            OpioidClaimCount = sum(OpioidClaimCount),
            opioidPR = 100*OpioidClaimCount/TotalClaimCount,
            RXRATE = mean(RXRATE)) %>%
  group_by(Specialty) %>% 
  summarise(
    MEDIAN_opioidPR = 100*median(RXRATE),
    #WEIGHTED_MEDIAN_opioidPR = 100*weighted.median(x=RXRATE, w=TotalClaimCount),
    MEAN_opioidPR = 100*mean(RXRATE),
    MEDIAN_TotalClaim = 100*median(TotalClaimCount),
    MEDIAN_TotalOpioidClaim = 100*median(OpioidClaimCount),
    TotalClaim = sum(TotalClaimCount),
    TotalOpioidClaim = sum(OpioidClaimCount)) %>% 
  mutate(across(where(is.numeric), round, 2))

write.csv(erin_data, "Data/erin_data.csv")
write.csv(erin_data_b, "Data/erin_data_b.csv")

# erin_data %>% kable()
