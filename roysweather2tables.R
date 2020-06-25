# Instructions
#   wget must be installed (based on OS)
#   install.packages(c("dplyr", "data.table", "rnoaa", "readr", "tidyr"))
#   delete file 2020.csv.gz to for update to latest dats
#   delete file coop-stations.txt to update station list (should not be necessary)
#   output is file roysdata.RDS  a tribble with FIPS, DATE, TMAX, TMIN, PRCP
#   NB: FIPS is stored as 

library(dplyr)
library(data.table)
library(rnoaa)
library(readr)
library(tidyr)

if (!file.exists("coop-stations.txt")) {                            # Weather stations
  download.file(
    "https://www.ncdc.noaa.gov/homr/file/coop-stations.txt",
    "coop-stations.txt",
    "wget"
  )
}

if (!file.exists("2020.csv.gz")) {                                  # Observations from 1/1/2020 til current date
  download.file(
    "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/2020.csv.gz",
    "2020.csv.gz",
    "wget"
  )
}

stations <-
  read_fwf("coop-stations.txt", fwf_cols(                          # Create county to FIPS table
    ID = c(26, 36),                                                # (I could not find station ID to FIPS
    STATE = c(94, 95),                                             # so went station ID to county name to FIPS
    COUNTY = c(97, 147)                                            # need to consider State because duplicate county names,
  ))                                                               # so need state name to state abbreviation map too)

weather <- read.csv("2020.csv.gz", header = FALSE)                 # YTD Weather by coop stations
weather <- weather[, -c(5:8)]
colnames(weather) = c("ID", "DATE", "ELEMENT", "VALUE")
weather <-
  filter(weather, ELEMENT == "TMAX" |
           ELEMENT == "TMIN" | ELEMENT == "PRCP")                  # Unpack weather, filter out all but Temp and Precipitation

# weather <-                                                        
#   weather[!(weather$ELEMENT == "PRCP" & weather$VALUE == 0), ]     # eliminate null  precipitation entries

codes <- fipscodes[, -c(3:4)]                                      # fipscodes is a dataset included in rnoaa - maps fips to counties
colnames(codes) = c("STATE", "COUNTY", "FIPS")

codes$STATE <- state.abb[match(codes$STATE, state.name)]           # state.abb is bult-in to R



codes$COUNTY <- toupper(codes$COUNTY)                              # clean names - all to upper and eliminate dashes
stations$COUNTY <- gsub("-", " ", stations$COUNTY)
stations <-
  merge(codes,
        stations,
        by = c("STATE", "COUNTY"),
        all = TRUE)
county_data <- na.omit(merge(stations, weather))                   # finally!  Weather, COunty, State, FIPS



county_data$VALUE <- county_data$VALUE / 10     # Weather data is stored as tenths of degrees C, and tenths of milimeters of precipitation


county_data %>%                                                   # Widen data by putting TMAX, TMIN, and PRCP in each entry
  pivot_wider(names_from = ELEMENT, values_from = VALUE) %>%
  group_by(FIPS, DATE) %>%                                        # Order by FIPS code, then date
  summarise(
    TMAX = mean(TMAX, na.omit = TRUE),                            # Take mean of each FIPS, Date readings
    TMIN = mean(TMIN, na.omit = TRUE)
#    ,PRCP = mean(PRCP, na.omit = TRUE)
  ) -> RoysDataTemp

county_data %>%                                                   # Widen data by putting TMAX, TMIN, and PRCP in each entry
  pivot_wider(names_from = ELEMENT, values_from = VALUE) %>%
  group_by(FIPS, DATE) %>%                                        # Order by FIPS code, then date
  summarise(
    # TMAX = mean(TMAX, na.omit = TRUE),                            # Take mean of each FIPS, Date readings
    # TMIN = mean(TMIN, na.omit = TRUE),
    PRCP = mean(PRCP, na.omit = TRUE)
  ) -> RoysDataPrcp



RoysDataTemp$FIPS <- sprintf("%05d",RoysData$FIPS)  # County FIPS must be 5 digit so make char with leading zero
RoysDataPrcp$FIPS <- sprintf("%05d",RoysData$FIPS)  # County FIPS must be 5 digit so make char with leading zero
saveRDS(RoysDataTemp, "roysdataTemp.RDS")               # Not sure how you want the data.  This is a tibble - could make time series
saveRDS(RoysDataPrcp, "roysdataPrcp.RDS")               # Not sure how you want the data.  This is a tibble - could make time series



