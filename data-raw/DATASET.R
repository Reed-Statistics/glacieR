## code to prepare `glacier` dataset goes here

#load libraries
library(tidyverse)
library(here)
library(rvest)
library(sf)
library(magrittr)

# upload files
glacier <- read_csv(here("data-raw","wgi_feb2012.csv"))

#  create new column with country names
country_name_crosswalk <- read_csv("data-raw/country_name_crosswalk.csv",
                                   col_names = c("political_unit", "country_name"))
glacier %<>%
  left_join(country_name_crosswalk) %>%
  mutate(country_name = as.factor(country_name))

# change continent to factor
continent_crosswalk <-tribble(
  ~continent_code, ~continent_name,
  1, "South America",
  2, "North America",
  3, "Africa",
  4, "Europe",
  5, "Asia",
  6, "New Zealand and Antarctic Islands",
  7, "Antartica"
)

glacier %<>%
  left_join(continent_crosswalk) %>%
  mutate(continent_name = as.factor(continent_name),
         political_unit = as.factor(political_unit))

# change columns to factor and title case
glacier %<>%
  mutate(data_contributor = as.factor(data_contributor),
         orientation_acc = as.factor(orientation_acc),
         orientation_abl = as.factor(orientation_abl), 
         glacier_name = str_to_title(glacier_name), 
         country_name = str_to_title(country_name),
         snow_line_date = if_else(snow_line_date == 1951, 19510000, snow_line_date)) %>%
  rename(country_code = political_unit, area_in_state = area_in_state)


# remove duplicated observations
glacier %<>%
  unique(glacier)

# reorder columns
glacier %<>%
  select(-free_position_code, -local_glacier_code, -country_code, -continent_code,
         -data_contributor, -drainage_code, -remarks, -coordinate_description) %>%
  select(wgi_glacier_id, country_name, continent_name, everything())

# recode observations into characters
glacier$primary_class <- case_when(
            glacier$primary_class == 0 ~ "Miscellaneous",
            glacier$primary_class == 1 ~ "Continental Ice Sheet",
            glacier$primary_class == 2 ~ "Ice Field",
            glacier$primary_class == 3 ~ "Ice Cap",
            glacier$primary_class == 4 ~ "Outlet Glacier",
            glacier$primary_class == 5 ~ "Valley Glacier",
            glacier$primary_class == 6 ~ "Mountain Glacier",
            glacier$primary_class == 7 ~ "Glacieret and Snowfield",
            glacier$primary_class == 8 ~ "Ice Shelf",
            glacier$primary_class == 9 ~ "Rock Shelf")
            
glacier$form <- case_when(
            glacier$form == 0 ~ "Miscellaneous",
            glacier$form == 1 ~ "Compound Valley Basins",
            glacier$form == 2 ~ "Compound Accumulation Basins",
            glacier$form == 3 ~ "Simple Basin",
            glacier$form == 4 ~ "Cirque",
            glacier$form == 5 ~ "Niche",
            glacier$form == 6 ~ "Crater",
            glacier$form == 7 ~ "Ice Apron",
            glacier$form == 8 ~ "Group",
            glacier$form == 9 ~ "Remnant") 
            
glacier$frontal_char <- case_when(            
            glacier$frontal_char == 0 ~ "Miscellaneous",
            glacier$frontal_char == 1 ~ "Piedmont",
            glacier$frontal_char == 2 ~ "Expended Foot",
            glacier$frontal_char == 3 ~ "Lobed",
            glacier$frontal_char == 4 ~ "Calving",
            glacier$frontal_char == 5 ~ "Confluent",
            glacier$frontal_char == 6 ~ "Irregular Debris Covered",
            glacier$frontal_char == 7 ~ "Irregular Clean Ice",
            glacier$frontal_char == 8 ~ "Single Lobe Clean Ice",
            glacier$frontal_char == 9 ~ "Single Lobe Debris Covered")

glacier$longi_profile <- case_when(
            glacier$longi_profile == 0 ~ "Miscellaneous",
            glacier$longi_profile == 1 ~ "Even/Regular",
            glacier$longi_profile == 2 ~ "Hanging",
            glacier$longi_profile == 3 ~ "Cascading",
            glacier$longi_profile == 4 ~ "Ice Fall",
            glacier$longi_profile == 5 ~ "Interrupted")
          
glacier$source_nourish <- case_when(            
            glacier$source_nourish == 0 ~ "Unknown",
            glacier$source_nourish == 1 ~ "Snow",
            glacier$source_nourish == 2 ~ "Avalanches",
            glacier$source_nourish == 3 ~ "Superimposed Ice")
            
glacier$tongue_activity <- case_when(
            glacier$tongue_activity == 0 ~ "Uncertain",
            glacier$tongue_activity == 1 ~ "Marked Retreat",
            glacier$tongue_activity == 2 ~ "Slight Retreat",
            glacier$tongue_activity == 3 ~ "Stationary",
            glacier$tongue_activity == 4 ~ "Slight Advance",
            glacier$tongue_activity == 5 ~ "Marked Advance",
            glacier$tongue_activity == 6 ~ "Possible Surge",
            glacier$tongue_activity == 7 ~ "Known Surge",
            glacier$tongue_activity == 8 ~ "Oscillating")
            
glacier$moraines1 <- case_when(
            glacier$moraines1 == 0 ~ "No Moraine",
            glacier$moraines1 == 1 ~ "Terminal Moraine",
            glacier$moraines1 == 2 ~ "Lateral/Medial Moraine",
            glacier$moraines1 == 3 ~ "Push Moraine",
            glacier$moraines1 == 4 ~ "Terminal and Lateral/Medial Moraine",
            glacier$moraines1 == 5 ~ "Terminal and Push Moraine",
            glacier$moraines1 == 6 ~ "Lateral/Medial and Push Moraine",
            glacier$moraines1 == 7 ~ "Terminal and Lateral/Medial and Push Moraine",
            glacier$moraines1 == 8 ~ "Debris",
            glacier$moraines1 == 9 ~ "Uncertain Type")
            
glacier$moraines2 <- case_when(
            glacier$moraines2 == 0 ~ "No Moraine",
            glacier$moraines2 == 1 ~ "Terminal Moraine",
            glacier$moraines2 == 2 ~ "Lateral/Medial Moraine",
            glacier$moraines2 == 3 ~ "Push Moraine",
            glacier$moraines2 == 4 ~ "Terminal and Lateral/Medial Moraine",
            glacier$moraines2 == 5 ~ "Terminal and Push Moraine",
            glacier$moraines2 == 6 ~ "Lateral/Medial and Push Moraine",
            glacier$moraines2 == 7 ~ "Terminal and Lateral/Medial and Push Moraine",
            glacier$moraines2 == 8 ~ "Debris",
            glacier$moraines2 == 9 ~ "Uncertain Type")
            
glacier$snow_line_acy <- case_when(
            glacier$snow_line_acy == 1 ~ "0 - 25",
            glacier$snow_line_acy == 2 ~ "25 - 50",
            glacier$snow_line_acy == 3 ~ "50 - 100",
            glacier$snow_line_acy == 4 ~ "100 - 200",
            glacier$snow_line_acy == 5 ~ ">200")
            
glacier$depth_acy <- case_when(
            glacier$depth_acy == 1 ~ "0 - 5",
            glacier$depth_acy == 2 ~ "5 - 10",
            glacier$depth_acy == 3 ~ "10 - 20",
            glacier$depth_acy == 4 ~ "20 - 30",
            glacier$depth_acy == 5 ~ ">30")
            
glacier$area_acy <- case_when(
            glacier$area_acy == 1 ~ "0 - 5",
            glacier$area_acy == 2 ~ "5 - 10",
            glacier$area_acy == 3 ~ "10 - 15",
            glacier$area_acy == 4 ~ "15 - 30",
            glacier$area_acy == 5 ~ ">30")


# clean up environment
rm(continent_crosswalk, country_name_crosswalk)
  
# overwriting dataset wrangling
usethis::use_data(glacier, overwrite = TRUE)
