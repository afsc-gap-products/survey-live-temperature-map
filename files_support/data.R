
# Load data from FOSS API ------------------------------------------------------

# install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(dplyr)
options(scipen = 999)

# link to the Haul API
api_link_haul <- "https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_haul/"

dat_haul_api0 <- data.frame()
for (i in seq(0, 500000, 10000)){
  ## find how many iterations it takes to cycle through the data
  print(i)
  ## query the API link
  res <- httr::GET(url = paste0(api_link_haul, "?offset=",i,"&limit=10000"))
  ## convert from JSON format
  data <- jsonlite::fromJSON(base::rawToChar(res$content)) 
  
  ## if there are no data, stop the loop
  if (is.null(nrow(data$items))) {
    break
  }
  
  ## bind sub-pull to dat data.frame
  dat_haul_api0 <- dplyr::bind_rows(dat_haul_api0, data$items)
}
# Find how many rows and columns are in the data pull
# print(paste0("rows: ", dim(dat)[1], "; cols: ", dim(dat)[2]))

# save outputs for later comparison
dat_haul_api <- dat_haul_api0 %>% 
  dplyr::rename(SRVY = srvy, 
                survey_definition_id  = survey_name,
                survey_name = survey_definition_id) %>% 
  dplyr::mutate(data_type = "offical", 
                date = as.Date(date_time)) %>% 
  dplyr::select(-links)


pal <- colorFactor(viridis(
  option = "D", 
  n = length(unique(dat_haul_api$vessel_name)), 
  begin = .2, 
  end = .8), 
  ordered = FALSE,
  domain = levels(unique(dat_haul_api$vessel_name)),
  na.color = "black")

dat_survey_list <- dat_haul_api %>% 
  dplyr::select(survey_definition_id, SRVY, survey, survey_name) %>% 
  dplyr::distinct() %>% 
  dplyr::ungroup()

dat_surveys <- dat_haul_api %>% 
  dplyr::select(year, SRVY, survey, 
                survey_definition_id,
                survey_name, 
                date_time, vessel_name, vessel_id#, 
  # latitude_dd_start, longitude_dd_start, latitude_dd_end, longitude_dd_end, 
  # bottom_temperature_c, surface_temperature_c, depth_m
  ) %>% 
  dplyr::group_by(year, SRVY, survey, survey_definition_id, survey_name, vessel_name, vessel_id) %>% 
  dplyr::summarise(date_min = min(as.Date(date_time), na.rm = TRUE), 
                   date_max = max(as.Date(date_time), na.rm = TRUE))  %>% 
  dplyr::mutate(
                vessel_shape = substr(x = vessel_name, start = 1, stop = 1),
                vessel_ital = paste0("F/V *", stringr::str_to_title(vessel_name), "*"), 
                vessel_name = paste0("F/V ", stringr::str_to_title(vessel_name)), survey = survey_name, 
                survey_dates = paste0(format(date_min, "%B %d"), " - ", format(date_max, "%B %d, %Y"))
                ) %>% 
  dplyr::select(-date_min, -date_max) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(vessel_color = pal(vessel_name))

# Load new data ----------------------------------------------------------------

# a <- list.files(path = here::here("data"))
# for (i in 1:length(a)){
#   b <- read_csv(file = here::here("data", a[i]))
#   b <- janitor::clean_names(b)
#   if (names(b)[1] %in% "x1"){
#     b$x1<-NULL
#   }
#   assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
# }

# New data ---------------------------------------------------------------------

if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
  
  # I set up a ConnectToOracle.R that looks like this: 
  #   
  #   PKG <- c("RODBC")
  # for (p in PKG) {
  #   if(!require(p,character.only = TRUE)) {  
  #     install.packages(p)
  #     require(p,character.only = TRUE)}
  # }
  # 
  # channel<-odbcConnect(dsn = "AFSC",
  #                      uid = "USERNAME", # change
  #                      pwd = "PASSWORD", #change
  #                      believeNRows = FALSE)
  # 
  # odbcGetInfo(channel)
  
} else { # For those without a ConnectToOracle file
  # # library(devtools)
  # # devtools::install_github("afsc-gap-products/gapindex")
  # library(gapindex)
  # channel <- gapindex::get_connected()
  
  # or 
  
  library(rstudioapi)
  library(RODBC)
  channel <- odbcConnect(dsn = "AFSC", 
                         uid = rstudioapi::showPrompt(title = "Username", 
                                                      message = "Oracle Username", default = ""), 
                         pwd = rstudioapi::askForPassword("Enter Password"),
                         believeNRows = FALSE)
}


lastdl <- Sys.Date()

date_max <- RODBC::sqlQuery(channel, paste0("SELECT CREATE_DATE FROM RACE_DATA.EDIT_HAULS;"))
date_max <- sort(as.numeric(unique(unlist(format(date_max, format = "%Y")))))
date_max <- max(date_max[date_max<=format(Sys.Date(), format = "%Y")]) # sometimes there are dates that haven't happened yet b/c testing


# Convert from DDM to DD
ddm2dd <- function(xx){ 
  x <- strsplit(x = as.character(xx/100), split = ".", fixed = TRUE)
  min <- as.numeric(unlist(lapply(x, `[[`, 1)))
  deg <- as.numeric(paste0(".", unlist(lapply(x, `[[`, 2))))*100
  y <- min + deg/60
  return(y)
}

if (max(dat_surveys$year) < date_max) { # if this year's data hasn't been entered into the production data
  
dat_haul_oracleraw <- dplyr::inner_join(
  # Pull event data
  x = RODBC::sqlQuery(channel, paste0( 
    "SELECT HAUL_ID, 
EDIT_DATE_TIME, 
EDIT_LATITUDE AS LATITUDE_DD_START, 
EDIT_LONGITUDE AS LONGITUDE_DD_START 
FROM RACE_DATA.EDIT_EVENTS
WHERE EVENT_TYPE_ID = 3;")) %>%   # standard haul
    dplyr::filter(format(as.Date(EDIT_DATE_TIME), format = "%Y") == date_max) %>%
  dplyr::rename(date = EDIT_DATE_TIME) %>%
  dplyr::mutate(
    # date = format(as.Date(date), format = c("%Y-%m-%d %H:%M")),
    LONGITUDE_DD_START = ddm2dd(LONGITUDE_DD_START), 
    LATITUDE_DD_START = ddm2dd(LATITUDE_DD_START)),
  
  # Pull haul data
  y = RODBC::sqlQuery(channel, paste0( #  EDIT_GEAR_TEMPERATURE_UNITS, EDIT_SURFACE_TEMPERATURE_UNITS, ABUNDANCE_HAUL, CREATE_DATE, 
    "SELECT HAUL_ID, 
CRUISE_ID, 
HAUL, 
STATION, 
-- STRATUM, 
EDIT_BOTTOM_DEPTH as DEPTH_M, 
EDIT_SURFACE_TEMPERATURE AS surface_temperature_c, 
EDIT_GEAR_TEMPERATURE AS bottom_temperature_c
FROM RACE_DATA.EDIT_HAULS
WHERE PERFORMANCE >= 0;")), 
  
  by = "HAUL_ID")  %>% 
  
  # Get vessel info and SURVEY_ID
  dplyr::left_join(y = RODBC::sqlQuery(channel, paste0( 
    "SELECT CRUISE_ID, 
    SURVEY_ID, 
VESSEL_ID, 
START_DATE, 
END_DATE
FROM RACE_DATA.CRUISES;")) %>% 
      dplyr::mutate(survey_dates = paste0(format(START_DATE, "%B %d"), " - ", format(END_DATE, "%B %d, %Y"))) %>% 
      dplyr::select(-START_DATE, -END_DATE), 
    by = "CRUISE_ID") %>%
  
  # Add SURVEY_DEFINITION_ID
  dplyr::left_join(y = RODBC::sqlQuery(channel, paste0( 
    "SELECT SURVEY_DEFINITION_ID, 
    SURVEY_ID
FROM RACE_DATA.SURVEYS;")), 
    by = "SURVEY_ID") %>%  
  
  janitor::clean_names() %>% 
  dplyr::select(-survey_id, -cruise_id, -haul_id) %>%
  dplyr::left_join(x = ., 
                   y = dat_surveys %>% 
                     dplyr::select(-survey_dates, -year) %>% 
                     dplyr::distinct()) %>% 
    dplyr::mutate(data_type = "raw") %>% 
  dplyr::mutate(year = date_max) %>% 
  dplyr::ungroup()
  
} else {
  dat_haul_oracleraw <- data.frame()
}

# Combined haul data --------------------------------------------------------------------

dat <- dat_event <- dplyr::bind_rows(dat_haul_oracleraw, dat_haul_api)  %>% 
  dplyr::select(
    year, stratum, station, date, data_type, 
    SRVY, survey, survey_dates, 
    vessel_id, vessel_name, vessel_color, vessel_ital, vessel_shape, 
    depth_m,
    surface_temperature_c,
    bottom_temperature_c,
    latitude_dd_start, 
    longitude_dd_start
    # st = surface_temperature_c, 
    # bt = bottom_temperature_c, 
    # latitude = latitude_dd_start, 
    # longitude = longitude_dd_start
    ) %>% 
  dplyr::arrange(-year)
  # dplyr::filter(
  #   !(is.na(station)) &
  #     !is.na(surface_temperature_c) &
  #     !is.na(bottom_temperature_c) & 
  #     # there shouldn't be bottom temps of 0 in the AI or GOA
  #     ((SRVY %in% c("AI", "GOA") & surface_temperature_c != 0) | (SRVY %in% c("EBS", "NBS"))) & 
  #     ((SRVY %in% c("AI", "GOA") & bottom_temperature_c != 0) | (SRVY %in% c("EBS", "NBS")))) %>% 

# Shapefiles -------------------------------------------------------------------

load(file = here::here("data", "shp.rdata"))
