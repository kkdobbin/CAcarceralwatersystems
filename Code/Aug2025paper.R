#Data compilation for Aug 2025

#Load libraries
library(tidyverse)

#Read in data
CASDWIS <- read_csv("Data/CASDWIS_WaterSystemSummary_20250812.csv")
System_types <- read_csv("Data/binary_usepa_outcomes_081125_KD.csv") #Jenny data sent Aug 11 with manual updates to make some nonexclusive carceral systems exclusive carceral based on name matching (13 mostly non CWS CC systems). Also changed to noncarceral a few based on facility closures. 

#Abbreviate data and change column names then join
CASDWIS <- CASDWIS %>% rename(pwsid = 'Water System No.')
CASDWIS <- CASDWIS[,c(1,6,7)]
CASDWIS <- CASDWIS %>% rename(CASDWIS_County = 'Principal County Served')
CASDWIS <- CASDWIS %>% rename(CASDWIS_SourceType = 'Primary Source Water Type')
CASDWIS <- CASDWIS %>% mutate(across(c(pwsid, CASDWIS_County, CASDWIS_SourceType), as.factor))

System_types <- System_types %>% mutate(across(c(pwsid, carceral_sys), as.factor))

Data <- left_join(System_types, CASDWIS)

#Get rid of 4 NA systems no longer in SDWIS (assume closed)
Data <- Data %>% filter(!is.na(CASDWIS_SourceType))

#Create variables for source type and whether source purchased or not
Data$watersource <- as.factor(ifelse(Data$CASDWIS_SourceType == "GU" | Data$CASDWIS_SourceType == "GUP" |
                           Data$CASDWIS_SourceType == "GW" | Data$CASDWIS_SourceType == "GWP", "GW", "SW"))

Data$purchased <- as.factor(ifelse(Data$CASDWIS_SourceType == "GUP" | Data$CASDWIS_SourceType == "GWP" |
                        Data$CASDWIS_SourceType == "SWP", "Purchased", "Not purchased"))

Data$state_classification <- as.factor(Data$state_classification)

#Add in additional output measures

#Needs assessment 2025
NA2025 <- read_csv("Data/SaferNA_2025_consolidated.csv")
NA2025 <- NA2025 %>% mutate(across(c(Failing_Status, Absence_of_Interties, Source_Capacity_Violations, Bottled_Water_or_Hauled_Water_Reliance, Significant_Deficiencies, Number_of_Water_Sources), as.factor))
Data <- left_join(Data, NA2025, join_by("pwsid" == "PWSID"))

#DWR water shortage vulnerability tool
DWRvulnerability <- read_csv("Data/i07_Water_Shortage_Vulnerability_Small_Water_Systems.csv")
DWRvulnerability <- DWRvulnerability[,c(2,43,57)]
DWRvulnerability$Distributiondis_any <- ifelse(DWRvulnerability$SC3i_Count_Distribution > 0, 1, 0)
Data <- left_join(Data, DWRvulnerability,  join_by("pwsid" == "WATER_SYSTEM_NUMBER"))

#CCR
CCR <- read_csv("Data/NCCR_full.csv")
Data <- left_join(Data, CCR, join_by("pwsid" == "id"))

#Save full paper data
write_csv(Data, "Data/Fullpaperdata.csv")

