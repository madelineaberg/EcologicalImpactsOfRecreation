##### R Script for cleaning & combining LBCU data ########
################# December 17, 2021 ######################

#### packages needed ####
library(here)
library(dplyr)
library(tidyverse)

#### load data ####
LBCU2019 <- read.csv(here::here("datatemp/original/LBCU_FromStephanie/LBCU_2019_NestSuccess.csv"))
LBCU2020 <- read.csv(here::here("datatemp/original/LBCU_FromStephanie/LBCU_2020_NestSuccess.csv"))
LBCU2021 <- read.csv(here::here("datatemp/original/LBCU_FromStephanie/LBCU_2021_NestSuccess.csv"))
LBCUhab <- read.csv(here::here("datatemp/original/LBCU_FromStephanie/NestHabitatData_Compiled_2019-2021.csv"))

#### Remove rows with Mayfield calculations & extra columns ####
LBCU2019 <- LBCU2019[c(1:36), c(1:41)]
LBCU2020 <- LBCU2020[c(1:41),]
LBCU2021 <- LBCU2021[c(1:29), ]

#### Select and rename columns, then fix variables so we can combine years ####
LBCU2019 <- LBCU2019 %>% dplyr::select(ID, FirstFound_i, Exposure, 
                                projected.hatch, 
                                Fate, date.failed, how.failed, 
                                actual.hatch, Initiation)

LBCU2020 <- LBCU2020 %>% dplyr::select(nest_name, date_found, exp_startdate, exp_enddate, exposure_days, 
                                stage_found, how_found, What.is.the.projected.hatch.date.,
                                fate, date_failed, failure_cause, fate_notes,
                                hatch_date)

LBCU2021 <- LBCU2021 %>% dplyr::select(nest_name, date_found, exp_startdate, exp_enddate_UTC, exposure_days, 
                                stage_found, how_found, projected_hatch, 
                                fate, fate_binary, fail_date_UTC, fate_notes,
                                hatch_date, 
                                initiation_backcalculated_from_hatch_or_projHatch)

LBCU2019 <- LBCU2019 %>% rename(
  Nest.Name = ID, 
  date_found = FirstFound_i, 
  exposure_days = Exposure, 
  fate = Fate, 
  date_fail = date.failed, 
  fate_notes = how.failed, 
  hatch_date = actual.hatch, 
  initiation_date = Initiation,
  projected_hatch = projected.hatch)

LBCU2020 <- LBCU2020 %>% rename(
  Nest.Name = nest_name,
  projected_hatch = What.is.the.projected.hatch.date., 
  date_fail = date_failed)

LBCU2021 <- LBCU2021 %>% rename(
  Nest.Name = nest_name,
  exp_enddate = exp_enddate_UTC, 
  date_fail = fail_date_UTC, 
  initiation_date = initiation_backcalculated_from_hatch_or_projHatch)

# Fix dates before combining 
LBCU2019$projected_hatch <- as.Date(LBCU2019$projected_hatch, format = "%d-%b-%y")
LBCU2020$projected_hatch <- as.Date(LBCU2020$projected_hatch, format = "%B %d, %Y")
LBCU2021$projected_hatch <- as.Date(LBCU2021$projected_hatch, format = "%m/%d/%y %H:%M")

LBCU2019$date_found <- as.Date(LBCU2019$date_found, format = "%d-%b-%y")
LBCU2020$date_found <- as.Date(LBCU2020$date_found, format = "%B %d, %Y")
LBCU2021$date_found <- as.Date(LBCU2021$date_found,format='%m/%d/%y %H:%M')

LBCU2019$date_fail <- as.Date(LBCU2019$date_fail, format = "%d-%b-%y")
LBCU2020$date_fail <- as.Date(LBCU2020$date_fail, format = "%m/%d/%y %H:%M")
LBCU2021$date_fail <- as.Date(LBCU2021$date_found,format='%m/%d/%y %H:%M')

LBCU2019$hatch_date <- as.Date(LBCU2019$hatch_date, format = "%d-%b-%y")
LBCU2020$hatch_date <- as.Date(LBCU2020$hatch_date, format = "%m/%d/%y %H:%M")
LBCU2021$hatch_date <- as.Date(LBCU2021$hatch_date,format='%m/%d/%y %H:%M')

LBCU2019$initiation_date <- as.Date(LBCU2019$initiation_date, format = "%d-%b-%y")
LBCU2021$initiation_date <- as.Date(LBCU2021$initiation_date,format='%m/%d/%y %H:%M') 
LBCU2020 <- LBCU2020 %>% mutate(initiation_date = projected_hatch - 33)

#### Combine nest success datasets #### 
LBCUnests <- full_join(LBCU2019, LBCU2020)
LBCUnests <- full_join(LBCUnests, LBCU2021)
head(LBCUnests)

#### Combine with nesthab data ####
LBCUhab_nestsonly <- LBCUhab %>% filter(Type..nest.random. == "NEST" | Type..nest.random. == "Nest")

LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Dog Fight" = "Dogfight")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Lemonchello" = "Lemoncello")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Miss Business" = "Ms Business ")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Finally!" = "Finally")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "No Way! Joni" = "No Way Joni")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Born Free" = "Born Free ")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Oh Honey!" = "OhHoney")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Oh Yeah!" = "OhYeah")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Rhubarb Pie 2020" = "Rhubarb20")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Q-Tip" = "Qtip")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Oh Buddy!" = "OhBuddy")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Old McDonald" = "OldMcDonald")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Co-Angus" = "CovidAngusKH")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Princess Di" = "PrincessDi")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Labrynth" = "Labyrinth")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Too Hot To Handle" = "TooHotToHandle")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Jifog" = "JiFog")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "SUSPICIOUS LUMP" = "Suspicious Lump")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "LIMP" = "Limp")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "NINJA" = "Ninja")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "JONI JR." = "Joni Jr")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "VILLAGE" = "Village")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "JUJU" = "Juju")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "RHUBARB PIE" = "Rhubarb Pie")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "BONFIRE" = "Bonfire")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "FANDANGO" = "Fandango")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "ORANGE SQUARE" = "Orange Square")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "SHACKELTON" = "Shackleton")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "SNEAKY G" = "Sneaky G")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "HOUDINI" = "Houdini")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "PERISCOPE" = "Periscope")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "ROBIE" = "Robie (#1)")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "BINGO" = "Bingo")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "MANCHEGO" = "Manchego")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "DIJON" = "Dijon")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "MICHAEL JACKSON" = "Michael Jackson")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "MILK DUD" = "Milk Dud")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "ODD COUPLE" = "Odd Couple")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "GUNNIESS" = "Guinness")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "RELENTLESS" = "Relentless")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "ARTMESIA" = "Artemesia")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "SURPRISE 19" = "Surprise 2019")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "SPOTTY" = "Spotty")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "BOB" = "Bob")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "REDUX" = "Redux")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "RAG N' BONE" = "Rag n Bone")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "WATCHMAKER" = "Watch Maker")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "SKA" = "Ska")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "DAYTONA 36" = "Daytona")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "MAD HATTER" = "Mad Hatter")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Sweatbread" = "Sweetbread")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Jumper" = "JumpAround")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Bad Dog " = "Bad dog")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Oceana" = "Oceania")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Tuesday" = "Tuesday ")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "OldMcDonald" = "OldMcDonold")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Owyhee" = "Owyhee ")
LBCUhab_nestsonly$Nest.Name <- dplyr::recode(LBCUhab_nestsonly$Nest.Name, "Screecher" = "Screecher ")




LBCUnests <- inner_join(LBCUhab_nestsonly, LBCUnests, by = "Nest.Name") 








