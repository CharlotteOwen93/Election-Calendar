# Load packages
library(tidyverse)
library(parlitools)
library(readxl)
polls <- read_excel("Scheduled Polls 2017-2025 (1).xlsx")

ons_codes_LAs <- la_codes
ons_codes_const <- bes_2015 %>% select(ons_const_id,constituency_name,country,region,constituency_type)
colnames(ons_codes_LAs)
ons_codes_LAs %>% filter(type!="Scotland"&type!="Wales"&type!="Northern Ireland") #20 too many for England Apr 2020?
colnames(ons_codes_const)
ons_codes_const <- ons_codes_const %>% rename(name=constituency_name, code=ons_const_id, type=constituency_type)
colnames(ons_codes_const)
ons_codes_LAs <- ons_codes_LAs %>% rename(code=la_code)
colnames(ons_codes_const)
colnames(ons_codes_LAs)

temp <- polls$`Authority Name`
temp <- gsub("Ards and North Down", "North Down and Ards", temp)
temp <- gsub("Armagh, Banbridge and Craigavon", "Armagh City, Banbridge and Craigavon", temp)
temp <- gsub("Bristol, City of", "City of Bristol", temp)
temp <- gsub("Edinburgh", "City of Edinburgh", temp)
temp <- gsub("Glasgow", "Glasgow City", temp) 
temp <- gsub("Herefordshire, County of", "Herefordshire", temp)
temp <- gsub("Kingston upon Hull, City of", "Kingston upon Hull", temp)
temp <- gsub("Kingston Upon Thames", "Kingston upon Thames", temp)
temp <- gsub("Newcastle Upon Tyne", "Newcastle upon Tyne", temp)
temp <- gsub("Orkney", "Orkney Islands", temp)
temp <- gsub("Rhondda Cynon Taf", "Rhondda Cynon Taff", temp)
temp <- gsub("Richmond Upon Thames", "Richmond upon Thames", temp)
temp <- gsub("Rotheram", "Rotherham", temp)
temp <- gsub("Shepway", "Folkestone and Hythe", temp)
temp <- gsub("Shetland", "Shetland Islands", temp)
temp <- gsub("St. Albans", "St Albans", temp)
temp <- gsub("St. Edmundsbury", "St Edmundsbury", temp)
temp <- gsub("St. Helens", "St Helens", temp)
polls$`Authority Name` <- temp	

ons_codes_LAs <- ons_codes_LAs %>% 
	mutate(country = ifelse(str_detect(ons_codes_LAs$code,"W")==TRUE, "Wales", ifelse(str_detect(ons_codes_LAs$code,"E")==TRUE, "England", ifelse(str_detect(ons_codes_LAs$code,"S")==TRUE, "Scotland","Northern Ireland"))))

colnames(ons_codes_const)

test <- left_join(polls,ons_codes_LAs, by = c("Authority Name"="name"))

view(test[is.na(test$code)==TRUE,])	
class(polls$Year)

write_csv(polls,"Scheduled_polls.csv")

