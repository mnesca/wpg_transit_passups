
library(sf)
library(RSocrata)
library(stringr)
library(dplyr)

# read the data in
tran_src <- read.socrata("https://data.winnipeg.ca/resource/mer2-irmb.csv") 

wpg <- st_read("./data/Neighbourhood.shp")

# digest the data - some rows are missing the spatial 
transitdata <- tran_src %>% 
  filter(str_detect(location, "^POINT"))

# make transit data frame feel spatial & set CRS (the data seem to be in degrees = WGS84)
st_geometry(transitdata) <- st_as_sfc(transitdata$location)
st_crs(transitdata) <- 4326

# align neighbouroods & transit data on the same page
transitdata <- transitdata %>% 
  st_transform(crs = st_crs(wpg))

# now join the data!
result <- st_join(transitdata, wpg)

# check the result - all seems well; two points lost is OK result
sum(is.na(result$Name))