# adding district numbers to the raw files
# av <- available.packages(filters=list())
# av[av[, "Package"] == 'rgdal']
#install.packages('rgdal')
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(readxl)
library(tidygeocoder)

#library(rgdal)

# MHVillage
mhvillage_df <- read.csv("MHVillage_IL_Parks.csv")

house_districts <- read_sf("illinois_HouseShapefile")
senate_districts <- read_sf("illinois_senateShapefile")

colnames(house_districts)
colnames(senate_districts)


mhvillage_df <- mhvillage_df %>%
  mutate(full_address = paste(Address,City_State_ZIP, ZIP))

geocoded_df <- mhvillage_df %>%
  geocode(address = full_address, method = "osm")

success <- geocoded_df %>% filter(!(is.na(lat) | is.na(long)))
failed <- geocoded_df %>% filter(is.na(lat) | is.na(long))

results_retry <- failed %>%
  select(-lat,-long) %>%
  geocode(address = full_address, method = "arcgis")  # or "census"

# Combine results
combined <- bind_rows(success,results_retry)


# pt 2

house_districts <- st_make_valid(house_districts)
senate_districts <- st_make_valid(senate_districts)

# Ensure the districts use the same "GPS language" (CRS 4326) as your points
house_districts  <- st_transform(house_districts, 4326)
senate_districts <- st_transform(senate_districts, 4326)

geocoded_sf <- combined %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)  # WGS 84

# 3. Join EVERYTHING into one single object
geocoded_final <- geocoded_sf %>%
  st_join(house_districts %>% select(House.district = DISTRICT)) %>%
  st_join(senate_districts %>% select(Senate.district = DISTRICT)) 


#pt 3

final <- geocoded_final %>%
  st_drop_geometry()

final_with_county <- reverse_geocode(
  .tbl = final,
  lat = lat,
  long = long,
  method = "osm",
  full_results = TRUE
)

csvOut <- final_with_county %>%
  select(Name, Address, City_State_ZIP, ZIP, 
         Number_of_Sites, Url, 
         House.district, Senate.district, 
         full_address, lat, long, 
         County = county)


write.csv(csvOut, "MHvillage_IL_addedDistricts.csv", row.names = FALSE)
