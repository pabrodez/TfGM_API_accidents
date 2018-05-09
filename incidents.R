# Script to make requests to TfGM API, store files and map locations
# this script is intended to request Incidents data, store it and map it.
# Contains National Statistics data Â© Crown copyright and database right 2018
# libraries
pckgs <- c("jsonlite", "tidyverse", "sf", "stringr", "httr", "ggthemes", "devtools", "ggmap", "lubridate", "viridis")
if (pckgs[!(pckgs %in% installed.packages())] > 0) {install.packages(pckgs[!(pckgs %in% installed.packages())])}
# devtools::install_github("tidyverse/ggplot2")

library(jsonlite); library(tidyverse); library(sf); library(stringr); library(httr); library(ggthemes); library(ggmap); library(lubridate); library(viridis)

# create folder to store requests of accidents
if (!dir.exists("./incidents")) {dir.create("./incidents")}

# Request
api_key <- "your_key"
request <- GET(
  "https://api.tfgm.com/odata/Incidents?$expand=Location&$top=6000",  # check https://developer.tfgm.com/docs/services to customize the request url
  add_headers("Ocp-Apim-Subscription-Key" = api_key))

if (status_code(request) != 200) {
  stop(paste("Client/server error.", "Status =", status_code(request)))
}

req_content <- httr::content(request, as = "text", encoding = "UTF-8") # Get the content as text. keep json structure

day_hour <- strftime(Sys.time(), format = "%Y-%m-%e,%H-%M")  # Create relative names to uniquely name each request text

write(req_content, file = paste0("./incidents/", day_hour, ".json"))  # Save request content

req_list <- list.files("./incidents", full.names = TRUE)
json_data <- lapply(req_list, function(x) fromJSON(txt = x, flatten = TRUE))  # to iterate over each, and get back unnested data frames

json_list_df <- function(x) {
  df <- data.frame()
  for (i in seq_along(json_data)) {
    df <- dplyr::bind_rows(df, json_data[[i]][["value"]])
  }
  df <- dplyr::distinct(df, Id, .keep_all = T)
  df_inc <<- df
}

json_list_df(json_data)

df_inc <- df_inc %>% rename(Location = Location.LocationSpatial.Geography.WellKnownText)  # rename

df_inc$Location <- gsub(pattern = "\\(", replacement = "", df_inc$Location) 
df_inc$Location <- gsub(pattern ="\\)", replacement = "", df_inc$Location)
df_inc$Location <- gsub(pattern = "POINT ", replacement ="", df_inc$Location)  # remove POINT and ()

long_lat <- str_split(df_inc$Location, "\\s+")  # split in two at the white space, get a list. Each element is a vector with long and lat

# get long and lat from each element of list and return as df
long_lat_df <- function(list) {
  long <- vector()
  lat <- vector()
  
  for (i in seq_along(list)) {
    long <- c(long, list[[i]][[1]])
    lat <- c(lat, list[[i]][[2]])
  }
  coordinates_df <<- data.frame(
    longitude = as.character(long),
    latitude = as.character(lat),
    stringsAsFactors = FALSE
  )
}

long_lat_df(long_lat)

options(digits = 17)  # to display the whole length of coordinates
df_inc$longitude <- coordinates_df$longitude  # convert to numeric
df_inc$latitude <- coordinates_df$latitude

df_inc$longitude <- as.double(df_inc$longitude)
df_inc$latitude <- as.double(df_inc$latitude)

# Convert dates to POSIXc and plot time
# Sys.setlocale("LC_TIME", "eng_GB")
df_inc$StartDate <- lubridate::ymd_hms(df_inc$StartDate)
# table(lubridate::day(df_inc$StartDate)) %>% as.tibble() %>% mutate(Var1 = as.numeric(Var1)) %>% rename(Day = Var1, Count = n)
ggplot(df_inc) +
  geom_freqpoly(
    aes(x = StartDate),
    bins = 6* 24 / 4  # 7 days, bin every 4 hours
  ) +
  scale_x_datetime(
    date_labels = "%D",
    breaks = as.POSIXct(seq(Sys.Date() - 7, Sys.Date(), "1 day")),
    limits = as.POSIXct(c(Sys.Date() - 7, Sys.Date())),
    expand = c(0, 0)
  ) +
  labs(title = "Count of incidents in the last week") +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_blank()
  )

# hour "heatmap"
day_hour_df <- df_inc %>%
  mutate(Weekday = lubridate::wday(StartDate, label = TRUE), Hour = lubridate::hour(StartDate)) %>%
  select(Weekday, Hour) %>%
  count(Weekday, Hour)

day_hour_df$Weekday <-  ordered(day_hour_df$Weekday, levels = c("Sun", "Sat", "Fri", "Thu", "Wed", "Tue", "Mon"))

  # group_by(Weekday, Hour) %>% 
  # summarize(Count = n())

# Plot to visualize basically this: table(lubridate::wday(df_inc$StartDate, label = TRUE), lubridate::hour(df_inc$StartDate), useNA = "ifany")
ggplot(day_hour_df, aes(Hour, Weekday)) +
  geom_tile(aes(fill = n), colour = "white") +
  scale_fill_viridis( 
    breaks = seq(min(day_hour_df$n), max(day_hour_df$n), by = 15),
    limits = c(min(day_hour_df$n), max(day_hour_df$n)),
    name = "Count"
  ) +
  scale_x_continuous(breaks = 00:23, labels = c(00:23)) +
  coord_equal() +
  labs(title = "Incidents per hour within a week",
       subtitle = paste("Since", min(df_inc$StartDate), sep = " ")
       ) +
  theme_fivethirtyeight() +
  theme(
    panel.grid = element_blank(),
    legend.key.height = unit(0.75, "cm"),
    legend.position = "right",
    legend.direction = "vertical",
    axis.text.y = element_text(margin = margin(r = -17))
  )
# might use scale_fill_gradient(colours = seq_gradient_pal())

# Get GM boundaries
uk_wards <- st_read("https://opendata.arcgis.com/datasets/afcc88affe5f450e9c03970b237a7999_3.geojson", quiet = TRUE, stringsAsFactors = FALSE)
gm_wards <- uk_wards %>% filter(lad16nm %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan"))
gm_wards <- gm_wards %>% select(code = wd16cd, ward_name = wd16nm, geometry = geometry, long, lat, District = lad16nm)

# convert to sf object and transform crs
incidents_sf <- st_as_sf(df_inc,
                         coords = c("longitude", "latitude"),
                         crs = 4326, agr = "constant"
)
incidents_sf <- st_transform(incidents_sf,
                             crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=375,-111,431,0,0,0,0 +units=m +no_defs"
)

gm_wards <- st_transform(gm_wards,
                         crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=375,-111,431,0,0,0,0 +units=m +no_defs"
)

ggplot() +
  geom_sf(data = incidents_sf, aes(colour = Type)) +
  geom_sf(data = gm_wards, fill = NA, size = 0.2, colour = "black") +
  labs(title = "Incidents reported in GM roads since April 2018") +
  theme_fivethirtyeight() +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    panel.background = element_blank(),
    line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(datum = NA)

# Road map
coordinates_df$longitude <- as.numeric(coordinates_df$longitude)
coordinates_df$latitude <- as.numeric(coordinates_df$latitude)
road_map <- get_map(location = "Manchester", maptype = "roadmap", color = "bw")

ggmap(road_map) + 
  geom_point(data = coordinates_df, aes(x = longitude, y = latitude)) +
  labs(title = "Incidents reported in GM roads since April 2018") +
  theme_fivethirtyeight(base_family) +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    panel.background = element_blank(),
    line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

