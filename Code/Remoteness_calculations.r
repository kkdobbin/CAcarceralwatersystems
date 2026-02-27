#Remoteness calculation
library(tidyverse)
library(sf)
library(ggmap)
library(nngeo)

Urbanized_areas <- st_read("Data/2020_Adjusted_Urban_Area/") #From CalTrans. Dowloaded from https://gisdata-caltrans.opendata.arcgis.com/datasets/71930cd25e6f4683ba2ee18511ac21ef_0/about on January 3rd. Last updated Dec 11, 2024. 
str(Urbanized_areas)
plot(st_geometry(Urbanized_areas)) 

PWS_boundary <- st_read("Data/California_Drinking_Water_System_Area_Boundaries.geojson") #SABL download from Jenny 072325
str(PWS_boundary)
PWS_boundary <- PWS_boundary %>% filter(BOUNDARY_TYPE == "Water Service Area") #Filter out duplicates
PWS_boundary <- st_transform(PWS_boundary,"EPSG:4326") #transform

sf_use_s2(FALSE)
PWS_boundary$centroid <- st_centroid(PWS_boundary$geometry) #works but I get an error
PWS_boundary$centroid_lat <- st_coordinates(PWS_boundary$centroid)[, "Y"]
PWS_boundary$centroid_lon <- st_coordinates(PWS_boundary$centroid)[, "X"]

plot(st_geometry(PWS_boundary$geometry))
plot(PWS_boundary$centroid, add = T, col = 'red', pch = 19) #Check - looks right

library(nngeo)
Distance <- st_nn(PWS_boundary$centroid, Urbanized_areas$geometry, k=1, returnDist = T) %>% 
  set_names("NN", "NN_distance_meters") %>% 
  map_df(., unlist)

PWS <- PWS_boundary[2]

Final <- cbind(PWS, Distance)

Notzero <- Final %>% filter(NN_distance_meters !=0)
Notzero$NN_distance_miles <- Notzero$NN_distance_meters/1609.34
Zero <- Final %>% filter(NN_distance_meters == 0)
Zero$NN_distance_miles <- 0

Final <- rbind(Notzero, Zero)

Final$Remote <- ifelse(Final$NN_distance_miles>5, "Yes", "No") #using 5 miles as a threshold for plurality. Corresponds with census designation for rural- distant (although definition of urban area for measuring to is different, theirs is 50k people plus whereas the cal trans layer here is all urbanized areas with more thank 5k people. 

#re-do the above distance calculation to only calculate distance from large urban areas (50k plus areas)
Urbanized_areas_large <- Urbanized_areas %>% filter(UrbanAreas == 2)
Distance_largeurban <- st_nn(PWS_boundary$centroid, Urbanized_areas_large$geometry, k=1, returnDist = T) %>% 
  set_names("NN", "NN_distance_meters") %>% 
  map_df(., unlist)

PWS <- PWS_boundary[2]

Final_largeurban <- cbind(PWS, Distance_largeurban)

Notzero_largeurban <- Final_largeurban %>% filter(NN_distance_meters !=0)
Notzero_largeurban$NN_distance_miles_largeurban <- Notzero_largeurban$NN_distance_meters/1609.34
Zero_largeurban <- Final_largeurban %>% filter(NN_distance_meters == 0)
Zero_largeurban$NN_distance_miles_largeurban <- 0

Final_largeurban <- rbind(Notzero_largeurban, Zero_largeurban)

Final_largeurban$Remote_largeurban <- ifelse(Final_largeurban$NN_distance_miles_largeurban>5, "Yes", "No") 

Final_largeurban2 <- st_drop_geometry(Final_largeurban)

Final2 <- st_drop_geometry(Final)

Final_combined <- left_join(Final_largeurban2, Final2, by = "SABL_PWSID")

write.csv(Final_combined, file = "Data/Remoteness.csv")

