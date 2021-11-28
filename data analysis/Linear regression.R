
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
                cooling = Cooling,
                address2 = Address2,
                amenities_nearby = Amenities.Nearby
                )

#discard attributes that missing too much value and not useful in linear regression
realtor_data <- raw_data %>%
  dplyr::select(price, building_type, parking_type,  
                bathrooms, bedrooms, building_amenities, has_photo, photos, 
                features, cooling, heating_type, community_name, 
                time_length, above_grade, maintenance_fees, annual_property_tax,
                exterior_finish, amenities_nearby
                )

#correct building type
realtor_data <- realtor_data %>%
  mutate(building_type = case_when(
    building_type %in% c("House", "Parking", "Other") ~ "",
    TRUE ~ building_type
  ))

realtor_data <- realtor_data %>%
  filter(price<2999000, price>100000, building_type == 'Apartment')

#group heating type
realtor_data <- realtor_data %>%
  mutate(heating_type = case_when(
    str_detect(heating_type, 'Baseboard')~ "Baseboard heaters",
    str_detect(heating_type, 'Forced air')~ "Forced air",
    str_detect(heating_type, 'Heat Pump')~ "Heat Pump",
    str_detect(heating_type, 'Radia|radia')~ "Radiant heat",
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


#extract exterior_finish
realtor_data <- realtor_data %>%
  mutate(exterior_brick = case_when(
    str_detect(exterior_finish, 'Brick')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(exterior_concrete = case_when(
    str_detect(exterior_finish, 'Concrete')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))


realtor_data <- realtor_data %>%
  mutate(exterior_other = case_when(
    str_detect(exterior_finish, "Aluminum|Stone|Metal|Other")~ 'TRUE',
    TRUE ~ 'FALSE'
  ))


#extract building_amenities attributes
realtor_data <- realtor_data %>%
  mutate(building_amenities_car_wash = case_when(
    str_detect(building_amenities, 'Car')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(building_amenities_exercise_center = case_when(
    str_detect(building_amenities, 'Exercise')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(building_amenities_storge = case_when(
    str_detect(building_amenities, 'Storage')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(building_amenities_security = case_when(
    str_detect(building_amenities, 'Security')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(building_amenities_party_room = case_when(
    str_detect(building_amenities, 'Party')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(building_amenities_recreation_centre = case_when(
    str_detect(building_amenities, 'Recreation')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(building_amenities_sauna = case_when(
    str_detect(building_amenities, 'Sauna')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(building_guest_suit = case_when(
    str_detect(building_amenities, 'Guest')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))



#extract parking type attributes

realtor_data <- realtor_data %>%
  mutate(parking_type_garage = case_when(
    str_detect(parking_type, 'Garage')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(parking_type_visitor_parking = case_when(
    str_detect(parking_type, 'Visitor')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(parking_type_undergroung = case_when(
    str_detect(parking_type, 'Underground')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

#extract feature attributes

realtor_data <- realtor_data %>%
  mutate(feature_balcony = case_when(
    str_detect(features, 'Balcony')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))


#extract amenities nearby attributes
realtor_data <- realtor_data %>%
  mutate(amenities_nearby_hospital = case_when(
    str_detect(amenities_nearby, 'Hospital')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(amenities_nearby_park = case_when(
    str_detect(amenities_nearby, 'Park')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(amenities_nearby_public_transit = case_when(
    str_detect(amenities_nearby, 'Transit')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(amenities_nearby_schools = case_when(
    str_detect(amenities_nearby, ' Schools')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(amenities_nearby_marina = case_when(
    str_detect(amenities_nearby, ' Marina')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))


#extract significant community name attributes
realtor_data <- realtor_data %>%
  mutate(cname_black_creek = case_when(
    str_detect(community_name, 'Black')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(cname_downsview_roding-CFB = case_when(
    str_detect(community_name, 'Downsview')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(cname_l_amoreaux = case_when(
    str_detect(community_name, 'Amoreaux')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))

realtor_data <- realtor_data %>%
  mutate(cname_steeles = case_when(
    str_detect(community_name, 'Steeles')~ 'TRUE',
    TRUE ~ 'FALSE'
  ))


#fit linear regression model

realtor_data <- realtor_data %>%
  mutate(annual_property_tax = ifelse(annual_property_tax > 2, annual_property_tax, NA)) %>%
  mutate(maintenance_fees = ifelse(maintenance_fees > 2, maintenance_fees, NA))


realtor_data <- realtor_data %>%
  dplyr::select(-building_type, -building_amenities, -features, 
                -exterior_finish, -parking_type, -amenities_nearby,
                -above_grade, -community_name
                )


#realtor_data<- realtor_data[-360,]

full_model <- glm(price~., data=na.omit(realtor_data), family = "gaussian")

backward_selection <- stats::step(full_model) 

summary(backward_selection)



log_model <- glm(price~., data=na.omit(realtor_data), family = Gamma(link="log"))

log_backward_selection <- stats::step(log_model)





#test model assumptions
best_model <- backward_selection

print("Non data transformation linear regression model")
summary(backward_selection)
print("Model assumption for non data transformation model")
test <- data.frame(
  R2 = modelr::rsquare(best_model, data = na.omit(realtor_data)),
  RMSE = modelr::rmse(best_model, data = na.omit(realtor_data)),
  MAE = modelr::mae(best_model, data = na.omit(realtor_data)),
  AIC = AIC(best_model),
  BIC = BIC(best_model)
)
glimpse(test)


print("Log data transformation linear regression model")
summary(log_backward_selection)
print("Model assumption for non data transformation model")
test2 <- data.frame(
  R2 = modelr::rsquare(log_backward_selection, data = na.omit(realtor_data)),
  RMSE = modelr::rmse(log_backward_selection, data = na.omit(realtor_data)),
  MAE = modelr::mae(log_backward_selection, data = na.omit(realtor_data)),
  AIC = AIC(log_backward_selection),
  BIC = BIC(log_backward_selection)
)
glimpse(test2)


realtor_data %>%
  ggplot(aes(x = price, na.rm = TRUE))+
  xlab("Total earning") +
  ylab("Count") +
  ggtitle("Distribution of Total earning") +
  geom_histogram(binwidth = 30000, colour="black", fill="white")+
  scale_x_continuous(labels = scales::comma)


par(mfrow=c(2,2))
plot(best_model)


par(mfrow=c(2,2))
plot(log_backward_selection)
with(summary(log_backward_selection), 1 - deviance/null.deviance)


#simple model
simple_model <- glm(price~bathrooms+maintenance_fees+annual_property_tax, data=na.omit(realtor_data), family = "gaussian")
test3 <- data.frame(
  R2 = modelr::rsquare(simple_model, data = na.omit(realtor_data)),
  RMSE = modelr::rmse(simple_model, data = na.omit(realtor_data)),
  MAE = modelr::mae(simple_model, data = na.omit(realtor_data)),
  AIC = AIC(simple_model),
  BIC = BIC(simple_model)
)
glimpse(test3)

