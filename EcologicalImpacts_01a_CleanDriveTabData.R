# Bringing in tabular data only & cleaning it to combine with non-projected points -------------
# November 1, 2021

# Packages needed ---------------------------------------------------------
library(sf)
library(tidyverse)
library(raster)
library(here)
library(ggplot2)
library(viridis)

#############################################################################################
################################# OBS TAB ###################################################
#############################################################################################

# Bring in tabular data ---------------------------------------------------------
tab_data <- read.csv(here::here("datatemp/original/driving.obs.csv"))

# remove test obs
tab_data <- filter(tab_data, route != "test") # remove test obs

# remove obs from the week with missing points (2019-06-15) 
# has no GPS points, so don't want to include in weeks but depending on what doing might not want to remove this
tab_data <- filter(tab_data, date != "2019-06-15") 

# Check dataset & variables ------------------------------------------------
table(tab_data$NG.use.code)

# Put the date variable into the date format
tab_data$date <- as.Date(tab_data$date, "%d/%m/%Y") # want R to read this as a date

# Put time into time format 
tab_data$time1
tab_data <- mutate(tab_data, time1 = case_when(
  time <= 999 ~ paste0("0", time), 
  time >= 1000 ~ paste0(time))) 
tab_data$TimeDiff <- difftime(as.POSIXct(tab_data$time1, format = "%H%M"), 
                              as.POSIXct("0:00", format = "%H:%M"), units = "min")

# Route length - exact, calculated in QGIS
tab_data <- tab_data %>%
  mutate(rt_length = case_when(
    route == "BB1" ~ 19.99, 
    route == "BB2" ~ 13.56, 
    route == "CC" ~ 16.00,
    route == "GV" ~ 17.34,
    route == "PV1" ~ 12.91,
    route == "PV2" ~ 13.60,
    route == "PV3" ~ 14.20,
    route == "SC" ~ 16.21,
    route == "SF1" ~ 11.12,
    route == "SF2" ~ 18.69))

# add obs variable 
tab_data$obs <- "MA"

# trying to add a sequential week variable for each week through the study
tab_data <- tab_data %>% group_by(date) %>% dplyr::mutate(week = cur_group_id())
table(tab_data$week, tab_data$date) ## seems like this worked 10/17/21
table(tab_data$week)

# adding year and month variables
tab_data <- tab_data %>%
  mutate(
    year = case_when(
      grepl("2021", date) ~ "2021",
      grepl("2020", date) ~ "2020",
      grepl("2019", date) ~ "2019"), 
    month = case_when(
      grepl("-01-", date) ~ "January",
      grepl("-02-", date) ~ "February",
      grepl("-03-", date) ~ "March",
      grepl("-04-", date) ~ "April",
      grepl("-05-", date) ~ "May",
      grepl("-06-", date) ~ "June",
      grepl("-07-", date) ~ "July",
      grepl("-08-", date) ~ "August",
      grepl("-09-", date) ~ "September",
      grepl("-10-", date) ~ "October",
      grepl("-11-", date) ~ "November",
      grepl("-12-", date) ~ "December"),
    year.num = case_when(
      year == "2019" ~ 1,
      year == "2020" ~ 2, 
      year == "2021" ~ 3),
    month.num = case_when(
      grepl("-01-", date) ~ 1,
      grepl("-02-", date) ~ 2,
      grepl("-03-", date) ~ 3,
      grepl("-04-", date) ~ 4,
      grepl("-05-", date) ~ 5,
      grepl("-06-", date) ~ 6,
      grepl("-07-", date) ~ 7,
      grepl("-08-", date) ~ 8,
      grepl("-09-", date) ~ 9,
      grepl("-10-", date) ~ 10,
      grepl("-11-", date) ~ 11,
      grepl("-12-", date) ~ 12), 
    Day.of.Week = "S")

table(tab_data$survey.id)

table(tab_data$use.type) # the codes I used - more description than NG use codes
table(tab_data$NG.use.code)

table(tab_data$NG.habitat.over)
tab_data$NG.habitat.over <- recode(tab_data$NG.habitat.over, "road" = "none")

table(tab_data$NG.habitat.under)
tab_data$NG.habitat.under <- recode(tab_data$NG.habitat.under, "none" = "bare")
tab_data$NG.habitat.under <- recode(tab_data$NG.habitat.under, "Perrenial grass" = "perrenial grass")
tab_data$NG.habitat.under <- recode(tab_data$NG.habitat.under, " " = "bare")
tab_data$NG.habitat.under <- recode(tab_data$NG.habitat.under, "nA" = "unknown")
table(tab_data$NG.habitat.under)

table(tab_data$num.people)
table(tab_data$num.men)
tab_data$num.men <- as.integer(tab_data$num.men)
table(tab_data$num.women)
table(tab_data$num.kids)
tab_data$num.kids <- as.integer(tab_data$num.kids)

tab_data$distance <- recode(tab_data$distance, "500+"="500")
tab_data$distance <- as.integer(tab_data$distance)

str(tab_data$bearing) # looks good

# fix some route id names
tab_data$survey.id <- recode(tab_data$survey.id, 
                             "PV1-28918" = "PV1-28919",
                             "SF1-090521" = "SF1-090520", 
                             "SF1-090522" = "SF2-090520",
                             "SF2-090521" = "SF2-090520",
                             "SF2-090522" = "SF2-090520")

head(tab_data)
str(tab_data)

#############################################################################################
################################# Route TAB #################################################
#############################################################################################

# Bring in the route data -------------------------------------------------
route_tab_data <- read.csv(here::here("datatemp/original/driving.routes.csv"))

# Filtering out test routes
route_tab <- filter(route_tab_data, route != "CC-TEST" & route !="SF-TEST")

# Fix the mislabeled route 
route_tab$route <- recode(route_tab$route, "SF1 " = "SF1")
route_tab$route <- recode(route_tab$route, "PV1 " = "PV1")
route_tab$route <- recode(route_tab$route, "PV 3" = "PV3")
route_tab$route <- as.factor(route_tab$route)

# fix mislabeled survey id
route_tab$survey.id <- recode(route_tab$survey.id, 
                              "B1-040420" = "BB1-040420", 
                              "GV-030720" = "GV-070320", 
                              "PV1-030621" = "PV1-060321")

# check variables & make necessary changes
route_tab$numeric_num_obs <- as.numeric(route_tab$num.obs)
route_tab$numeric_num_shoot <- as.numeric(route_tab$num.shoot)

str(route_tab$date)
route_tab$Date <- as.Date(route_tab$date, "%d %B %Y")
str(route_tab$Date)


route_tab$start.time <- recode(route_tab$start.time, 
                               "1413 - 1432" = "1413",
                               "1440-1501" = "1440", 
                               "1420-1445" = "1420", 
                               "1144-1200" = "1144")
route_tab$start.time2 <- as.numeric(route_tab$start.time)
route_tab <- mutate(route_tab, time1 = case_when(
  start.time2 <= 999 ~ paste0("0", start.time2), 
  start.time2 >= 1000 ~ paste0(start.time2))) 
route_tab$TimeDiff <- difftime(as.POSIXct(route_tab$time1, format = "%H%M"), 
                              as.POSIXct("0:00", format = "%H:%M"), units = "min")

# Route length - exact, calculated in QGIS
route_tab <- route_tab %>%
  mutate(rt_length = case_when(
    route == "BB1" ~ 19.99, 
    route == "BB2" ~ 13.56, 
    route == "CC" ~ 16.00,
    route == "GV" ~ 17.34,
    route == "PV1" ~ 12.91,
    route == "PV2" ~ 13.60,
    route == "PV3" ~ 14.20,
    route == "SC" ~ 16.21,
    route == "SF1" ~ 11.12,
    route == "SF2" ~ 18.69))

route_tab <- route_tab %>%
  mutate(
    year = case_when(
      grepl("2021", date) ~ "2021",
      grepl("2020", date) ~ "2020",
      grepl("2019", date) ~ "2019")) 

library(weathermetrics)
route_tab$tempC <- fahrenheit.to.celsius(route_tab$temp)

table(route_tab$NG.weather)
table(route_tab$month)


write.csv(route_tab,"C:/Users/Madeline/Desktop/EcologicalImpactsManuscript/CleanData/DrivingRoutes_Clean_19Aug2024.csv")



