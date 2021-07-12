# Data from ebird is cleaned and tidied
# Loading packages
library(auk)
library(dplyr)
library(lubridate)

# Reading the ebird data using auk data
acdata <- read_ebd("./ebd_cremyn_relMay-2018.txt")
head(acdata)
summary(acdata)

# Exploring data and checking if are there missing data

# Is all the data about the Crested Myna or are there any other species?
unique(acdata$common_name) # "Crested Myna"
unique(acdata$scientific_name) # "Acridotheres cristatellus"

# Are there any missing data of date, latitude, longitude or country?
any(is.na(acdata$observation_date))
any(is.na(acdata$latitude))
any(is.na(acdata$longitude))
any(is.na(acdata$longitude))

# Observation date as Timestamp 
acdata$observation_date <- as.Date(acdata$observation_date)

# Adding year column 
acdata$year <- year(acdata$observation_date)

# Time observations started as DateTime
acdata <- acdata %>%
  mutate(., starting_time = as.numeric(as_datetime(paste(observation_date, time_observations_started, sep = " "))))

# Observations count as numeric
acdata$observation_count <- as.numeric(acdata$observation_count)

# Which countries do the observations belong to?
acdata$country <- as.factor(acdata$country)
unique(acdata$country)
unique(acdata$country_code)

# Countries of America
countries_america <- c("Argentina", "Canada", "United States", "Uruguay")

# Filtering data for American Countries
amdata <- acdata %>% 
  filter(., country %in% countries_america)

unique(amdata$country)
nrow(amdata) # 1747
summary(amdata)

# Are there observations registered in e-bird as the same sampling event?
observers_group <- as.factor(amdata$group_identifier)
observers_group <- observers_group[!is.na(observers_group)]
length(observers_group) == length(unique(observers_group)) # TRUE

table(observers_group)

############ Looking for records that are probably the same, but they are registered as different observations
# Are there any observations from the same day and for the same country?
amdata_more_samplings <- amdata %>%
  group_by(., country) %>%
  add_count(., observation_date) %>%
  rename(sampling_frequency = n)  %>%
  filter(., sampling_frequency > 1)

# If they were on the same day, were they in the same place?
amdata_same_place <- amdata_more_samplings %>%
  group_by(., country, observation_date) %>%
  mutate(., latlon=paste(longitude, latitude, sep = "")) %>%
  add_count(., latlon, name = 'latlon_count') %>%
  filter(., latlon_count>1)

# Where did observations started?
amdata_same_sampling_time <- amdata_same_place %>%
  group_by(., country, observation_date, latlon) %>%
  arrange(., starting_time) %>%
  mutate(., duration_first_sampling = first(duration_minutes)) %>%
  mutate(., diff_time = abs(c(NA,diff(starting_time)))/60)

amdata_same_sampling_time <- amdata_same_sampling_time %>% 
  mutate(., same_sampling_event=case_when(diff_time<duration_first_sampling ~ TRUE, 
                                          diff_time >= duration_first_sampling ~ FALSE,
                                          diff_time == NA ~ NA))

amdata_same_sampling_time <- amdata_same_sampling_time %>% 
  filter(., same_sampling_event==TRUE)

# Finally, the ids from duplicated observations were obtained
id_duplicated_obs <- amdata_same_sampling_time$global_unique_identifier

# The rows with those ids are removed from the table
cleaned_amdata <- amdata %>%
  filter(., !global_unique_identifier %in% id_duplicated_obs)

# Only some colums are kept 
dfcleaned_amdata <- as.data.frame(cleaned_amdata)
colnames(dfcleaned_amdata)
dfamdata_main_colums <- dfcleaned_amdata[ ,c("country","latitude", "longitude","year")]
head(dfamdata_main_colums)

# The final table can be writen as .csv
write.csv(dfamdata_main_colums, "./crested_myna_records.csv")