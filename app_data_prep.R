# Load packages
library(tidyverse)
library(parlitools)
library(readxl)
library(pdftools)
library(tabulizer)
library(skimr)

"The English election timetable can be accessed here: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/792138/Election_Timetable_in_England_2019.pdf"
"The parlitools package contains a list of all local government authorities in the UK and their unique codes. Most helpfully 
here, it contains what type of local area they are too."

df <- la_codes %>% filter(type!="Sui Generis"&type!="Scotland"&type!="Wales"&type!="Northern Ireland")

skim(df)

"There are 3 districts different to the rest, Doncaster, Rotherham and Birmingham"
temp <- c("Doncaster","Rotherham","Birmingham")
met_whole <- df %>% filter(name %in% temp) #every year except council year

`%notin%` <- Negate(`%in%`)
met_thirds <- df %>% filter(type=="Metropolitan"&name %notin% temp) #once every 4 years. from 16/17/18 for Rotherham, Doncaster, Birmingham

rm(temp)

london <- df %>% filter(type=="London") #every 4 years from 2022
county <- df %>% filter(type=='County') #online list doesn't include Dorset, has it as a UA instead
#every 4 years from 2021

district_whole_A <- c("") #4 years from 2023
district_whole_B <- c("") #4 years from 2020
district_whole_C <- c("") #4 years from 2022
district_thirds <- c("") #every year except council year 
district_halves <- c("") #every 2 years from 2020


#met districts are mainly elected by thirds, those that are not:
# Doncaster, Rotherham, Birmingham all elect whole council
# Districts are weird and will need lot of separate info
# Unitary Authorities
# 

### Correct modulo input shown below, fix - then nearly ready!
which_elections <- function(year){
  elec.c = ifelse((year-2021) %% 4 == 0, "TRUE","FALSE") #county
  elec.b = ifelse((year-2022) %% 4 == 0, "TRUE","FALSE") #london boroughs
  elec.doncaster = ifelse((year-2021) %% 4 == 0, "TRUE","FALSE") 
  elec.rotherham = ifelse((year-2020) %% 4 == 0, "TRUE","FALSE")
  elec.birmingham = ifelse((year-2022) %% 4 == 0, "TRUE","FALSE")
  elec.dwA = ifelse((year-2023) %% 4 == 0, "TRUE","FALSE") #district whole, group A
  elec.dwB = ifelse((year-2020) %% 4 == 0, "TRUE","FALSE") #district whole, group B
  elec.dwC = ifelse((year-2022) %% 4 == 0, "TRUE","FALSE") #district whole, group C
  elec.d2 = ifelse(year %% 2 == 0,"FALSE","TRUE") #district by halves
  elec.d3 = ifelse(elec.c == "TRUE","FALSE","TRUE") #district by thirds
  elec.met3 = ifelse(elec.c == "TRUE","FALSE","TRUE") #metropolitan thirds
  elec.u3 = ifelse(elec.c == "TRUE","FALSE","TRUE") #unitary thirds
  elec.uwA = ifelse((year-2023) %% 4 == 0, "TRUE","FALSE") #unitary whole, group A
  elec.uwB = ifelse((year-2021) %% 4 == 0, "TRUE","FALSE") #unitary whole, group A
  elec.uwC = ifelse((year-2020) %% 4 == 0, "TRUE","FALSE") #unitary whole, group A
  elec.sandc = ifelse((year-2021) %% 4 == 0, "TRUE","FALSE") #scilly and city
  elecs <- list(elec.c,elec.b,elec.doncaster)
  print("The areas that have elections this year are:")
  for(e in elecs){
    if(e == TRUE){
      print(deparse(substitute(e)))
    }
  }
}

which_elections(2034)

test_year <- 2025
test_out <- ifelse((test_year-2021)%%4 == 0, "TRUE","FALSE")
test_out
