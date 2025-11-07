#Analysis script by Kristin Dobbin

#Load libraries
library(tidyverse)
library(gtsummary)
library(kableExtra)

#Read in and prep full paper data
Data <- read_csv("Data/Fullpaperdata_Sept24.csv")
Data <- Data %>% mutate(across(c(pwsid, CASDWIS_Type, carceral_sys, health_5yr_no2ndary_or_lsl_binary, mon_only_5yr_binary, mr_5yr_binary, CASDWIS_County, watersource, purchased, SAFER_STATUS, SC5e_Drought_Impact, Distributiondis_any, HR_NAME, Water_Quality_Risk_Level_Display, Accessability_Risk_Level_Display, Affordability_Risk_Level_Display, TMF_Capacity_Risk_Level_Display), as.factor))
Data$Missing_ccr_any <- as.factor(ifelse(Data$n_ccr_missing > 0, "Yes", "No"))
Data$Missing_ccr_twoplus <- as.factor(ifelse(Data$n_ccr_missing >= 2, "Yes", "No"))


# Table 1 -----------------------------------------------------------------

#Provide basic descriptive on systems across typology
System_descriptives <- Data[, c(6,8,15,18,19,20)]
Table <- System_descriptives %>% tbl_summary(by = carceral_sys)
kable_Table <- Table %>% as_kable_extra(); kable_Table


# Figure 1 (map) ----------------------------------------------------------

#Map water systems by type

#Load more libraries
library(ggmap)
library(sf)
library(sp)

#load in boundary polygons for CWS
PWS_boundary <- st_read("Data/California_Drinking_Water_System_Area_Boundaries.geojson") #SABL download from Jenny 072325
str(PWS_boundary)
PWS_boundary <- st_make_valid(PWS_boundary)
PWS_boundary$SABL_PWSID <- as.factor(PWS_boundary$SABL_PWSID)
PWS_boundary$WATER_SYSTEM_NAME <- as.factor(PWS_boundary$WATER_SYSTEM_NAME)
PWS_boundary <- PWS_boundary %>% filter(BOUNDARY_TYPE != "Jurisdictional") #remove jurisdictional boundaries to get rid of duplicates

#Create a new version of the master data with geo attributes
Geodata <- Data
Geodata <- left_join(Geodata, PWS_boundary, by = c("pwsid" = "SABL_PWSID"))
Geodata <- Geodata %>% distinct(pwsid, .keep_all = TRUE) #get rid of one duplicate

#get centroids
Geodata$Centroid <- NA
Geodata$Centroid <- st_centroid(Geodata$geometry) #Get centroids to service area boundaries

#Basemaps
caCountiesTmp <- tigris::counties(state = 06) %>%
  st_as_sf()

Geodata <- st_as_sf(Geodata, sf_column_name = "Centroid")
Geodata$carceral_sys <- factor(Geodata$carceral_sys, levels = c("carceral_water_system", "carceral_serving_water_system", "other_water_systems"))

#map
Map <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), size = .35, data = Geodata, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", "other_water_systems" = "azure3", "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", "other_water_systems" = "Other water system", "carceral_serving_water_system" = "Carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

#save map
ggsave('Figures/Map.png', dpi = 720, bg='#ffffff')


# Table 2 -----------------------------------------------------------------

#Provide basic descriptive for outcome measures by typology
Outcomes <- Data[,c(8,9,11,24,25,27,29,38)]
Table2 <- Outcomes %>% tbl_summary(by = carceral_sys, missing = "no")
kable_Table2 <- Table2 %>% as_kable_extra(); kable_Table2


# Chi-squared tests -------------------------------------------------------

#Run Chi-squared tests for outcomes
MCL <- xtabs(~ health_5yr_no2ndary_or_lsl_binary + carceral_sys , data = Data)
chisq.test(MCL, correct = FALSE) #no sign dif between any groups

MON <- xtabs(~ mon_only_5yr_binary + carceral_sys , data = Data); MON
chisq.test(MON, correct = FALSE) #sig p=0.01163
chisq.test(MON[,c(2,1)]) #carceral serving and carceral sig different p=0.008
chisq.test(MON[,c(2,3)]) # carceral and other not sig (p=0.1342)
chisq.test(MON[,c(1,3)]) #carceral serving and other sig different p=0.01793

CCR <- xtabs(~ Missing_ccr_any + carceral_sys , data = Data); CCR
chisq.test(CCR, correct = FALSE) # Sig differences p<0.001
chisq.test(CCR[,c(2,1)]) # Carceral serving and carceral not different
chisq.test(CCR[,c(2,3)]) # Carceral and other systems not different
chisq.test(CCR[,c(1,3)]) #carceral serving and other systems sig dif p <0.001

#create dataset with NAs (not asssessed) removed to do the same for needs assessment data
Data_notassessedremoved <- Data
Data_notassessedremoved$Water_Quality_Risk_Level_Display[Data$Water_Quality_Risk_Level_Display == "Not Assessed"] <- NA
Data_notassessedremoved$Accessability_Risk_Level_Display[Data$Accessability_Risk_Level_Display == "Not Assessed"] <- NA
Data_notassessedremoved$TMF_Capacity_Risk_Level_Display[Data$TMF_Capacity_Risk_Level_Display == "Not Assessed"] <- NA
Data_notassessedremoved$SAFER_STATUS[Data$SAFER_STATUS == "Not Assessed"] <- NA
Outcomes_notassessedremoved <- Data_notassessedremoved[,c(8,9,11,24,25,27,29,38)]
Outcomes_notassessedremoved <- na.omit(Outcomes_notassessedremoved)
Outcomes_notassessedremoved <- droplevels(Outcomes_notassessedremoved)

QR <- xtabs(~ Water_Quality_Risk_Level_Display + carceral_sys , data = Outcomes_notassessedremoved); QR
chisq.test(QR, correct = FALSE) # not sig different p=0.4513
chisq.test(QR[,c(2,1)]) # not sig p=0.62
chisq.test(QR[,c(2,3)]) # not sig p=0.3091
chisq.test(QR[,c(1,3)]) #not sig p=0.7328

AR <- xtabs(~ Accessability_Risk_Level_Display + carceral_sys , data = Outcomes_notassessedremoved); AR
chisq.test(AR, correct = FALSE) # there are sig differneces, p<0.001
chisq.test(AR[,c(2,1)]) #Carceral serving and carceral sig different p<0.001
chisq.test(AR[,c(2,3)]) # carceral and other not sig different p=0.99
chisq.test(AR[,c(1,3)]) #carceral serving and other sig p<0.01

TMFR <- xtabs(~ TMF_Capacity_Risk_Level_Display + carceral_sys , data = Outcomes_notassessedremoved); TMFR
chisq.test(TMFR, correct = FALSE) # there are sig differneces, p<0.001
chisq.test(TMFR[,c(2,1)]) #Carceral serving and carceral sig different p<0.001
chisq.test(TMFR[,c(2,3)]) # carceral and other are sig different p<0.01
chisq.test(TMFR[,c(1,3)]) #carceral serving and other not sig p=0.1519

Outcomes_notassessedremoved$failingoratrisk <- as.factor(ifelse(Outcomes_notassessedremoved$SAFER_STATUS == "At-Risk" | Outcomes_notassessedremoved$SAFER_STATUS == "Failing", "Yes", "No"))

FAILORRISK <- xtabs(~ failingoratrisk + carceral_sys , data = Outcomes_notassessedremoved); FAILORRISK
chisq.test(FAILORRISK, correct = FALSE) # sig difference p=0.02
chisq.test(FAILORRISK[,c(2,1)]) #Carceral serving and carceral sig different p=0.013
chisq.test(FAILORRISK[,c(2,3)]) # carceral and other are not sig different p=0.1584
chisq.test(FAILORRISK[,c(1,3)]) #carceral serving and other  sig different p=0.032

STATUS <- xtabs(~ SAFER_STATUS + carceral_sys , data = Outcomes_notassessedremoved); STATUS
chisq.test(STATUS, correct = FALSE) # there are sig differneces, p=0.00364
chisq.test(STATUS[,c(2,1)]) #Carceral serving and carceral sig different p=0.01139
chisq.test(STATUS[,c(2,3)]) # carceral and other are sig different p=004117
chisq.test(STATUS[,c(1,3)]) #carceral serving and other not sig different p=0.1166


#Make a pairwise chi-squared table for paper
library(jgsbook)
mcl <- pairwise.chisq.test(Outcomes_notassessedremoved$health_5yr_no2ndary_or_lsl_binary, Outcomes_notassessedremoved$carceral_sys)
mcl$Outcome <- "One or more health-based violation 2020-2024"
mcl <- mcl %>% relocate(Outcome, .before = group1)

mon <- pairwise.chisq.test(Outcomes_notassessedremoved$mon_only_5yr_binary, Outcomes_notassessedremoved$carceral_sys)
mon$Outcome <- "One or more monitoring violation 2020-2024"
mon <- mon %>% relocate(Outcome, .before = group1)

qr <- pairwise.chisq.test(Outcomes_notassessedremoved$Water_Quality_Risk_Level_Display, Outcomes_notassessedremoved$carceral_sys)
qr$Outcome <- "Water quality risk level"
qr <- qr %>% relocate(Outcome, .before = group1)

ar <- pairwise.chisq.test(Outcomes_notassessedremoved$Accessability_Risk_Level_Display, Outcomes_notassessedremoved$carceral_sys)
ar$Outcome <- "Water accessibility risk level"
ar <- ar %>% relocate(Outcome, .before = group1)

tmfr <-  pairwise.chisq.test(Outcomes_notassessedremoved$TMF_Capacity_Risk_Level_Display, Outcomes_notassessedremoved$carceral_sys)
tmfr$Outcome <- "Technical, Managerial Financial risk level"
tmfr <- tmfr %>% relocate(Outcome, .before = group1)

status <- pairwise.chisq.test(Outcomes_notassessedremoved$failingoratrisk, Outcomes_notassessedremoved$carceral_sys)
status$Outcome <- "Designated failing or at-risk"
status <- status %>% relocate(Outcome, .before = group1)

ccr <- pairwise.chisq.test(Outcomes_notassessedremoved$Missing_ccr_any, Outcomes_notassessedremoved$carceral_sys)
ccr$Outcome <- "Missing one or more Consumer Confidence Report 2020-2023"
ccr <- ccr %>% relocate(Outcome, .before = group1)

pairwise <- rbind(mcl, mon, qr, ar, tmfr, status, ccr)

pairwise <- pairwise %>% mutate(across(c(xsquare, pvalue, padjust), as.numeric))

pairwise_rounded <- data.frame(lapply(pairwise, function(x) {
  if (is.numeric(x)) {
    round(x, digits = 2)
  } else {
    x 
  }
}))


# Supplemental Figure 1 ---------------------------------------------------

#Make several version of Figure 1 (statewide map of typology) showing only systems by type that are at risk for each outcome measure

Geodata_MCL <- Geodata %>% filter(health_5yr_no2ndary_or_lsl_binary == 1)

Map_MCL <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), size = .35, data = Geodata_MCL, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", "other_water_systems" = "azure3", "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = "Carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

Geodata_MON <- Geodata %>% filter(mon_only_5yr_binary == 1)

Map_MON <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), size = .35, data = Geodata_MCL, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", "other_water_systems" = "azure3", "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = "Carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

Geodata_QR <- Geodata %>% filter(Water_Quality_Risk_Level_Display == "High to medium risk")

Map_QR <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), size = .35, data = Geodata_QR, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", "other_water_systems" = "azure3", "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = "Carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))


Geodata_AR <- Geodata %>% filter(Accessability_Risk_Level_Display == "High to medium risk")

Map_AR <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), size = .35, data = Geodata_AR, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", "other_water_systems" = "azure3", "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = "Carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

Geodata_TMFR <- Geodata %>% filter(TMF_Capacity_Risk_Level_Display == "High to medium risk")

Map_TMFR <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), size = .35, data = Geodata_TMFR, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", "other_water_systems" = "azure3", "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = "Carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))

Geodata_STATUS <- Geodata %>% filter(SAFER_STATUS == "Failing" | SAFER_STATUS == "At-Risk")

Map_STATUS <- ggplot() +
  geom_sf(data = caCountiesTmp, fill = "white") +
  geom_sf(mapping = aes(colour = carceral_sys, geometry = Centroid), size = .35, data = Geodata_STATUS, inherit.aes = FALSE) +
  scale_color_manual(values = c("carceral_water_system" = "goldenrod1", "other_water_systems" = "azure3", "carceral_serving_water_system" = "hotpink4"),
                     labels = c("carceral_water_system" = "Carceral water system", "other_water_systems" = "Other water system",
                                "carceral_serving_water_system" = "Carceral-serving water system")) +
  labs(color = "System Type") +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(), 
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=10))


library(ggpubr)
Appendixplots <- ggarrange(Map_MCL, Map_MON, Map_QR, Map_AR, Map_TMFR, Map_STATUS,nrow = 3, ncol = 2, common.legend = TRUE, legend = "right", labels = "auto")
ggsave('Figures/Appendixplots.png', dpi = 720, width = 7.5, height = 10, bg='#ffffff')
