
library(here) 
library(tidyverse)
library(stringr)
library(lubridate)
library(hms)
library(shiny)
library(leaflet)
library(RSocrata)
library(sf)
library(units)
library(tmap)
library(snakecase)

wpg <- st_read(read.socrata("https://data.winnipeg.ca/resource/xaux-29zr.json"), quiet = TRUE) %>% 
  setNames(to_snake_case(names(.))) %>%
  select(-the_geom_type)

transitdata <- read.socrata("https://data.winnipeg.ca/resource/mer2-irmb.csv")




# Reading data
transitdata <- read_csv(here::here("data", "Transit_Pass-ups.csv"))

wpg <- st_read(here::here("data", "wpg_neigh.shp"), quiet = TRUE) %>%
  st_transform(26914) %>%   # convert to same projection as above
  select(id, name) 




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


class(wpg)
class(transitdataclean)

class(wpg_sf)
class(transitclean_sf)

# find points within polygons
latlong_in_wpg <- st_join(transitclean_sf, wpg_sf, join = st_within) 

latlong_wpg_count <- count(as_tibble(latlong_in_wpg), name) 







wpg$geometry %>% 
  ggplot() + 
  geom_sf(alpha = 0.3) +
  geom_point(data = transitdataclean, 
             aes(x = longitude, y = latitude), 
             size = 1, 
             shape = 23, 
             fill = "darkred")
  

##
## how to identify points in a polygon? with SF
## look at these websites
## https://mattherman.info/blog/point-in-poly/
## https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package

  


leaflet() %>% 
  addTiles() %>%
  addPolygons(data = wpg$geometry, 
              fillColor = "green",
              weight = 1.5,
              color = "black") # %>%
  # addMarkers(lng = transitdataclean$longitude, lat = transitdataclean$latitude)  lets not run this otherwise R seems to crash!



########################################################################
################# SHINY APP TIME !!!!!!!!! #############################
########################################################################

## This is the UI Section
ui <- bootstrapPage(
  
  # Application title
  titlePanel("Winnipeg Transit Pass-ups"),
  
  # this will be the side bar layout
  sidebarLayout(
    
    # this is the sidebar panel itself
    sidebarPanel(
      
      # the slider itself
      # sliderInput(inputId = "range", 
                  #label = "Water Level (Metres)", 
                  #value = 224, 
                  #min = 224, 
                  #max = 241),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # text for $ value affected
      # textOutput("text1"),
      
      
      br(),
      
      # text for population displaced
      # textOutput("text2"),
      
      
      br(),
      
      # Include clarifying text ----
      helpText("The purpose of this Shiny App is to portray the amount of passups under many different variables."
      ),
      # Include clarifying text ----
      #helpText("Assumption #2: We also summed up total dollar damage of properties in each neighbourhood affected, along with population."
      #),
      # Include clarifying text ----
      #helpText("Assumption #3: There is no flood mitigation variables included in this model such as dikes, the Red River Floodway, or pump stations."
      #),
      br(),
      
      #  adding the new div tag to the sidebar            
      tags$div(class="header", checked=NA,
               tags$p("References and datasources:"),
               tags$a(href="https://data.winnipeg.ca/City-Planning/Neighbourhood/fen6-iygi", "[DS] City of Winnipeg - Neighbourhood Polygon Shapefile"),
               br(),
               tags$a(href="https://winnipeg.ca/census/2016/", "[DS] City of Winnipeg - 2016 Census at the Neighbourhood level"),
               br(),
               tags$a(href="https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html", "Drawing beautiful maps programmatically with R, sf and ggplot2 Part 2: Layers"),
               br(),
               tags$a(href="https://r-spatial.github.io/sf/articles/sf1.html", "Simple Features for R"),
               br(),
               tags$a(href="https://rstudio.github.io/leaflet/shiny.html", "Using Leaflet with Shiny")
      ),
      br(),
      helpText("Created by: MNesca"
      )
      
      
    ),
    
    mainPanel(
      
      # the map itself
      leafletOutput("mymap", width = 1280, height = 1024)
      
    )
  )
)

## This is the Server Section
server <- function(input, output, session) {
  
  # Reactive expressions for the data subsetted to what the user selected
  #sliderdata_elevation <- reactive({
  #  is_flooded <- obj1$Elevation < input$range
  #})
  
  #reactive_dollar <- reactive({
  #  is_flooded <- sliderdata_elevation()
  #  tv_flooded <- obj1 %>% 
  #    summarize(sum(total*is_flooded))
  #})
  
  #reactive_population <- reactive({
  #  is_flooded <- sliderdata_elevation()
  #  tp_flooded <- obj1 %>% 
  #    summarize(sum(Population*is_flooded, na.rm = T))
  #})
  
  output$mymap <- renderLeaflet({
    # is_flooded = sliderdata_elevation()
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = wpg$geometry, #loading data
                  # fillColor = if_else(is_flooded, "darkblue", "darkgreen"), #colour of all the tiles
                  weight = 2, #weight of the lines (how thick it is)
                  color = "black" #changes colour of the polygon border
      )
  })
  
  #output$text1 <- renderText({
  #  paste("Total Flooded Dollar Value Potentially Affected: $",reactive_dollar())
  #})
  
  #output$text2 <- renderText({
  #  paste("Total Population Potentially Affected:",reactive_population())
  #})
}

## This is where the UI gets combined with the server
shinyApp(ui = ui, server = server)




