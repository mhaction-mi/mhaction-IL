# trying to figure out home rule communities...
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(readxl)
library(tidygeocoder)

# MHVillage
mhvillage_df <- read.csv("MHVillage_IL_addedDistricts.csv")

boundaries <- read_sf('il_homeRuleMunicipalities.geojson')
colnames(boundaries)

boundaries <- st_make_valid(boundaries)

# Ensure the districts use the same "GPS language" (CRS 4326) as your points
boundaries  <- st_transform(boundaries, 4326)

geocoded_sf <- mhvillage_df %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)  # WGS 84

geocoded_final <- geocoded_sf %>%
  st_join(boundaries %>% select(HomeRuleMunicipality = NAME), join = st_intersects) 

final <- geocoded_final %>%
  st_drop_geometry()

write.csv(final, "MHvillage_IL_addedDistricts_addedHomeRuleMunicipalities.csv", row.names = FALSE)

