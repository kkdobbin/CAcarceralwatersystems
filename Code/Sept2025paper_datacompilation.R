#Data compilation for Sept 2025 - subbing NA risk assessment category scores for levels

#Load libraries
library(tidyverse)
library(sf)

#Read in data and reduce
CASDWIS <- read_csv("Data/CASDWIS_WaterSystemSummary_20250812.csv")
System_types <- read_csv("Data/acr_df_for_analysis_aug292025.csv") 
System_types <- System_types[,c(2,13:19,10,5,12,7,6,11)]

#Abbreviate data and change column names then join
CASDWIS <- CASDWIS %>% rename(pwsid = 'Water System No.')
CASDWIS <- CASDWIS[,c(1,3,6,7)]
CASDWIS <- CASDWIS %>% rename(CASDWIS_County = 'Principal County Served')
CASDWIS <- CASDWIS %>% rename(CASDWIS_SourceType = 'Primary Source Water Type')
CASDWIS <- CASDWIS %>% mutate(across(c(pwsid, CASDWIS_County, CASDWIS_SourceType), as.factor))
CASDWIS <- CASDWIS %>% rename(CASDWIS_Type = 'Type')

System_types <- System_types %>% mutate(across(c(pwsid, carceral_sys), as.factor))

Data <- left_join(System_types, CASDWIS)

#switch type to state classification, get rid of any that aren't NTNC and CWS ######

#Get rid of 42 NA systems no longer in SDWIS (assume closed) 
Data <- Data %>% filter(!is.na(CASDWIS_SourceType))

#Change carceral system type on two more system #######

#Create variables for source type and whether source purchased or not
Data$watersource <- as.factor(ifelse(Data$CASDWIS_SourceType == "GU" | Data$CASDWIS_SourceType == "GUP" |
                                       Data$CASDWIS_SourceType == "GW" | Data$CASDWIS_SourceType == "GWP", "GW", "SW"))

Data$purchased <- as.factor(ifelse(Data$CASDWIS_SourceType == "GUP" | Data$CASDWIS_SourceType == "GWP" |
                                     Data$CASDWIS_SourceType == "SWP", "Purchased", "Not purchased"))

Data$pws_type_code <- as.factor(Data$pws_type_code)
Data$CASDWIS_Type <- as.factor(Data$CASDWIS_Type)

#Remove NC system types (state designation)
Data <- Data %>% filter(CASDWIS_Type != "NC")
Data$pwsid <- droplevels(Data$pwsid) #Make sure all unique systems

#create variable for hydro region
Systemboundarypolygons <- st_read("Data/California_Drinking_Water_System_Area_Boundaries.geojson") #From Jenny 072325
Systemboundarypolygons <- st_make_valid(Systemboundarypolygons)
hydroregionpolygons <- st_read("Data/i03_Hydrologic_Regions/i03_Hydrologic_Regions.shp")
hydroregionpolygons <- st_transform(hydroregionpolygons, crs = 4326)

intersection <- st_intersection(Systemboundarypolygons, hydroregionpolygons)
intersection$area <- st_area(intersection)

majority_overlap_join <- intersection %>%
  group_by(SABL_PWSID) %>% 
  slice_max(order_by = area, n = 1) %>%
  ungroup() %>%
  select(SABL_PWSID, HR_NAME) 

majority_overlap_join <- st_drop_geometry(majority_overlap_join) #get rid of spatial data

Data <- left_join(Data, majority_overlap_join, by = c("pwsid" = "SABL_PWSID"))
Data$HR_NAME <- as.factor(Data$HR_NAME)
summary(Data$HR_NAME) #811 don't have boundaires so are NA

#Add in additional output measures

#Needs assessment 2025
NA2025 <- read_csv("Data/2025riskassessment_categorysummarylevels.csv") #downloaded from the data dashboard September 24th
NA2025$SAFER_STATUS <- as.factor(NA2025$SAFER_STATUS)
NA2025$Water_Quality_Risk_Level_Display <- as.factor(NA2025$Water_Quality_Risk_Level_Display)
NA2025$Accessability_Risk_Level_Display <- as.factor(NA2025$Accessability_Risk_Level_Display)
NA2025$TMF_Capacity_Risk_Level_Display <- as.factor(NA2025$TMF_Capacity_Risk_Level_Display)
NA2025$Affordability_Risk_Level_Display <- as.factor(NA2025$Affordability_Risk_Level_Display)

Data <- left_join(Data, NA2025, join_by("pwsid" == "PWSID"))

Data <- Data %>%
  mutate(across(c(Water_Quality_Risk_Level_Display, SAFER_STATUS, Accessability_Risk_Level_Display, Affordability_Risk_Level_Display, TMF_Capacity_Risk_Level_Display), ~replace_na(., "Not Assessed")))

#collapse levels into a smaller number

library(forcats)
Data$Water_Quality_Risk_Level_Display <- fct_collapse(Data$Water_Quality_Risk_Level_Display,
                                 "High to medium risk" = c("High Risk", "Medium Risk"),
                                 "Low to no risk" = c("Low Risk", "No Risk"),
                                 "Not Assessed" = "Not Assessed")
Data$Accessability_Risk_Level_Display <- fct_collapse(Data$Accessability_Risk_Level_Display,
                                                      "High to medium risk" = c("High Risk", "Medium Risk"),
                                                      "Low to no risk" = c("Low Risk", "No Risk"),
                                                      "Not Assessed" = "Not Assessed")
Data$TMF_Capacity_Risk_Level_Display <- fct_collapse(Data$TMF_Capacity_Risk_Level_Display,
                                                      "High to medium risk" = c("High Risk", "Medium Risk"),
                                                      "Low to no risk" = c("Low Risk", "No Risk"),
                                                      "Not Assessed" = "Not Assessed")

#DWR water shortage vulnerability tool
DWRvulnerability <- read_csv("Data/i07_Water_Shortage_Vulnerability_Small_Water_Systems.csv")
DWRvulnerability <- DWRvulnerability[,c(2,43,57)]
DWRvulnerability$Distributiondis_any <- ifelse(DWRvulnerability$SC3i_Count_Distribution > 0, 1, 0)
Data <- left_join(Data, DWRvulnerability,  join_by("pwsid" == "WATER_SYSTEM_NUMBER"))

#CCR
CCR <- read_csv("Data/NCCR_full_master.csv")
Data <- left_join(Data, CCR, join_by("pwsid" == "id"))

#create a final indicator for which systems had CCRs pulled
PulledCCR_first <- read_csv("Data/binary_usepa_outcomes_081125_KD.csv")
PulledCCR_first <- PulledCCR_first[,1]
PulledCCR_second <- read_csv("Data/additionalCCRpullsneeded.csv")
PulledCCR_second <- PulledCCR_second %>% rename(pwsid = PWSID)
PulledCCR_combined <- rbind(PulledCCR_first, PulledCCR_second)
PulledCCR_combined$CCR_pulled <- "Yes"
Data <- left_join(Data, PulledCCR_combined)
Data <- Data %>% distinct(pwsid, .keep_all = TRUE)

#Save full paper data
write_csv(Data, "Data/Fullpaperdata_Sept24.csv")

