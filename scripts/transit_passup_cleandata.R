
library(here) 
library(tidyverse)
library(stringr)
library(lubridate)
library(hms)
library(sf)
library(units)
library(tmap)
library(snakecase)

# Reading data
#Transit pass up data reference: https://data.winnipeg.ca/Transit/Transit-Pass-ups/mer2-irmb
transitdata <- read_csv(here::here("data", "Transit_Pass-ups.csv"))

#shapefile (comes in zip format with 3 other files) reference: https://data.winnipeg.ca/City-Planning/Neighbourhood/fen6-iygi
wpg <- st_read(here::here("data", "wpg_neigh.shp"), quiet = TRUE)

#data cleaning for transit passup data
transitdataclean <- transitdata %>% 
  setNames(to_snake_case(names(.))) %>% ## sets all variable names as lowercase and adds an underscore to any spaces
  mutate_at(vars("route_destination", "route_name", "pass_up_type", "route_number"), as.factor) %>%
  mutate(time = lubridate::mdy_hms(time)) %>%
  separate(time, into = c("date", "time_of_day"), sep = " ") %>%
  mutate(time_of_day_num = as_hms(time_of_day)) %>%
  mutate_at(vars("time_of_day_num"), as.numeric) %>%
  mutate(year = year(date), 
         month = month(date), 
         day = day(date),
  ) %>%
  mutate_at(vars("year", "month", "day"), as.factor) %>%
  mutate(bus_speed = ifelse(route_number %in% c(137,160,161,162,163,170,180,181,183,185), 'RapidTransit',
                            ifelse(route_number %in% c(101,102,109,110), 'Dart',
                                   ifelse(route_number %in% c(1,2,3), 'DowntownSpirit',
                                          ifelse(route_number %in% c(21,22,24,28,30,31,32,40,41,42,46,48,54,57,58,59,64,65,67), 'Express',
                                                 ifelse(route_number %in% c(25,34,35,36), 'SuperExpress',
                                                        ifelse(route_number %in% c(635,641,642,649,650,662,671,672,676,677,690,691,693,694), 'RapidFeeder',
                                                               'Regular'))))))) %>%
  separate(location, into = c("extra", "long1", "lat1"), sep = " ") %>%
  separate(long1, into = c("extra1", "longitude"), sep = 1) %>%
  separate(lat1, into = c("latitude", "extra2"), sep = -1) %>%
  select(-extra, -extra1, -extra2) %>%
  mutate_at(vars("longitude", "latitude"), as.numeric) %>%
  mutate(route_destination = str_replace(route_destination, fixed("City Hall"), "To City Hall"),
         route_destination = str_replace(route_destination, fixed("To To City Hall"), "To City Hall"),
         route_destination = str_replace(route_destination, fixed("To Downtown only"), "To Downtown"),
         route_destination = str_replace(route_destination, fixed("To Downtwon"), "To Downtown"),
         route_destination = str_replace(route_destination, fixed("Downtown"), "To Downtown"),
         route_destination = str_replace(route_destination, fixed("To To Downtown"), "To Downtown"),
         route_destination = str_replace(route_destination, fixed("Red River College"), "To Red River College"),
         route_destination = str_replace(route_destination, fixed("To To Red River College"), "To Red River College"),
         route_destination = str_replace(route_destination, fixed("via Omands Creek"), "Via Omands Creek"),
         route_destination = str_replace(route_destination, fixed("via Mapleglen"), "Via Mapleglen"),
         route_destination = str_replace(route_destination, fixed("To U of Manitoba"), "To University of Manitoba"),
         route_destination = str_replace(route_destination, fixed("to University of Manitoba"), "To University of Manitoba"),
         route_destination = str_replace(route_destination, fixed("University of Manitoba"), "To University of Manitoba"),
         route_destination = str_replace(route_destination, fixed("To To University of Manitoba"), "To University of Manitoba"),
         route_destination = str_replace(route_destination, fixed("via Kenaston to Westdale"), "Via Kenaston to Westdale"),
         route_destination = str_replace(route_destination, fixed("Betournay & Speers"), "To Betournay & Speers"),
         route_destination = str_replace(route_destination, fixed("To To Betournay & Speers"), "To Betournay & Speers"),
         route_destination = str_replace(route_destination, fixed("To Betournay & Spears"), "To Betournay & Speers"),
         route_destination = str_replace(route_destination, fixed("via Kenaston to Westdale"), "Via Kenaston to Westdale"),
         route_destination = str_replace(route_destination, fixed("To Vaughan & Graham only"), "To Vaughan & Graham"),
         route_destination = str_replace(route_destination, fixed("University of Winnipeg"), "To U of Winnipeg"),
         route_destination = str_replace(route_destination, fixed("Speers- Elizabeth"), "Speers - Elizabeth"),
  ) %>%
  drop_na() 

# Creating sum stats mostly by route number  
sumstat_transit <- group_by(transitdataclean, year, month, day, route_number, route_name, route_destination) %>%
  filter(pass_up_type == 'Full Bus Pass-Up',
         route_destination != 'i') %>%
  summarise(
    count = n()  
  )

# Creating totals of passups per day
total_sumstat_transit_day <- sumstat_transit %>%
  group_by(year, month, day) %>%
  summarize(total_pu_per_day = sum(count))

# Creating totals of passups per month
total_sumstat_transit_month <- sumstat_transit %>%
  group_by(year, month) %>%
  summarize(total_pu_per_month = sum(count))

# Creating totals of passups per year
total_sumstat_transit_year <- sumstat_transit %>%
  group_by(year) %>%
  summarize(total_pu_per_year = sum(count))


# joining totals with counts
sumstat_transit <- left_join(sumstat_transit, total_sumstat_transit_day)
sumstat_transit <- left_join(sumstat_transit, total_sumstat_transit_month)
sumstat_transit <- left_join(sumstat_transit, total_sumstat_transit_year)

# joining totals to the transit data with counts
transitdataclean <- left_join(transitdataclean, total_sumstat_transit_day)
transitdataclean <- left_join(transitdataclean, total_sumstat_transit_month)
transitdataclean <- left_join(transitdataclean, total_sumstat_transit_year)

# convert passup data frame into sf object
transitclean_sf <- transitdataclean %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    agr = "constant",
    crs = 26914,        # nad83 / Manitoba Projection
    stringsAsFactors = FALSE,
    remove = TRUE
  ) 

wpg_sf <- wpg %>%
  st_transform(crs = st_crs(transitclean_sf)) %>%   # convert to same projection as above
  select(id, name) 






