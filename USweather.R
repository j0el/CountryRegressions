library(dplyr)
# library(data.table)
# library(usmap)
library(rnoaa)
library(readr)
library(tidyr)


if (!file.exists("coop-stations.txt")) {
  download.file(
    "https://www.ncdc.noaa.gov/homr/file/coop-stations.txt",
    "coop-stations.txt",
    "wget"
  )
}

if (!file.exists("2020.csv.gz")) {
  download.file(
    "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/2020.csv.gz",
    "2020.csv.gz",
    "wget"
  )
}

stations <-
  read_fwf("coop-stations.txt", fwf_cols(
    ID = c(26, 36),
    STATE = c(94, 95),
    COUNTY = c(97, 147)
  ))

weather <- read.csv("2020.csv.gz", header = FALSE)
weather <- weather[,-c(5:8)]
colnames(weather) = c("ID", "DATE", "ELEMENT", "VALUE" )
weather <- filter(weather, ELEMENT == "TMAX" | ELEMENT == "TMIN" | ELEMENT == "PRCP")

weather <- weather[!(weather$ELEMENT == "PRCP" & weather$VALUE == 0),]
codes <- fipscodes[,-c(3:4)]
colnames(codes) = c( "STATE","COUNTY", "FIPS")

codes$STATE <- state.abb[match(codes$STATE, state.name)]



codes$COUNTY <- toupper(codes$COUNTY)
stations$COUNTY <- gsub("-"," ", stations$COUNTY)
stations <- merge(codes, stations, by = c("STATE", "COUNTY"), all = TRUE)
county_data <- na.omit(merge(stations, weather))

saveRDS(county_data,"county_data.RDS")

#county_data$VALUE <- county_data$VALUE/10

foo <- county_data %>%
  pivot_wider(names_from = ELEMENT, values_from = VALUE) %>%
  arrange( FIPS, DATE) %>%
  group_by(DATE) %>%
  summarize(TMAX_MEAN = mean(TMAX, na.rm = TRUE),
            TMIN_MEAN = mean(TMIN, na.rm = TRUE),
            PRCP_MEAN = mean(PRCP, na.rm = TRUE),
            FIPS)







