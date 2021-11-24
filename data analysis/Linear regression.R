
library(tidyverse)
library(dplyr)
library(here)

#read file
raw_data <- read.csv(here::here('data/realtor.csv'))

#look at number of rows and columns
n_rows <- nrow(raw_data) %>%
  print()
n_cols <- ncol(raw_data) %>%
  print()
#look at summary of the data
summary(raw_data)

# count how many missing value
count_missing <- function(x){
  number_missing <- sum(is.na(x))
  return(number_missing)
}

#have a look on missing value
raw_data %>%
  summarize_all(count_missing) %>%
  glimpse()

#standardize the column names
raw_data <- raw_data %>% 
  dplyr::rename(.,property_type = propertyType,
                building_type = buildingType,
                parking_type = parkingType,
                building_amenities = buildingAmenities,
                has_photo = hasphoto,
                multi_media_features = multimediaFeatures,
                has_CMHCFTHBBanner = hasCMHCFTHBBanner,
                community_name = Community.Name,
                time_length = Time.on.REALTOR.ca,
                above_grade = Above.Grade,
                heating_type = Heating.Type,
                exterior_finish = Exterior.Finish,
                maintenance_fees = Maintenance.Fees,
                total_parking = Total.Parking.Spaces,
                annual_property_tax = Annual.Property.Taxes,
                features = Features,
                cooling = Cooling
                )

#discard attributes that missing too much value and not useful in linear regression
realtor_data <- raw_data %>%
  dplyr::select(price, building_type, parking_type,  
                bathrooms, bedrooms, building_amenities, has_photo, photos, 
                has_CMHCFTHBBanner, features, cooling, heating_type, community_name, 
                time_length, above_grade, maintenance_fees, annual_property_tax,
                exterior_finish
                )

#correct building type
realtor_data <- realtor_data %>%
  mutate(building_type = case_when(
    building_type %in% c("House", "Parking", "Other") ~ "",
    TRUE ~ building_type
  ))

#group heating type
realtor_data <- realtor_data %>%
  mutate(heating_type = case_when(
    heating_type %in% c("Baseboard heaters", 
                        "Baseboard heaters (Electric)",
                        "Baseboard heaters (Natural gas)") ~ "Baseboard heaters",
    heating_type %in% c("Forced air", "Forced air (Electric)",
                        "Forced air (Natural gas)",
                        "Forced air, (Natural gas)",
                        "Forced air, Hot water radiator heat") ~ "Forced air",
    heating_type %in% c("Heat Pump","Heat Pump (Electric)",
                        "Heat Pump (Natural gas)",
                        "Heat Pump, (Natural gas)") ~ "Heat Pump",
    heating_type %in% c("Hot water radiator heat (Natural gas)",
                        "Radiant heat",
                        "Radiant heat (Electric)",
                        "Radiant heat (Natural gas)") ~ "Radiant heat",
    TRUE ~ heating_type
  ))

#get numerical time length
realtor_data <- realtor_data %>%
  mutate(time_length = as.numeric(stringr::str_sub(time_length, 1, 2))
  )

#get numerical maintenance fees
realtor_data <- realtor_data %>%
  mutate(maintenance_fees = as.numeric
         (gsub('[$, (CAD)Monthly]', '', maintenance_fees))) 


#get numerical annual property tax
realtor_data <- realtor_data %>%
  mutate(annual_property_tax = as.numeric
         (gsub('[$, (CAD)]', '', annual_property_tax))) 

full_model <- lm(price~., data=na.omit(realtor_data))

backward_selection <- stats::step(full_model) 

summary(backward_selection)





