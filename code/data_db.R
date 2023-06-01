#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: download oracle data
#' ---------------------------


# Load data from Google Sheet --------------------------------------------------

if (googledrive_dl) {
  # https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
  googledrive::drive_download(file = googledrive::as_id(dir_googledrive_log),  #"gap_survey_progression.csv",
                              type = "xlsx", 
                              overwrite = TRUE, 
                              path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"))
}

# Download data from oracle saved locally: -------------------------------------

## Load stored data --------------------------------------------------------------------

a <- list.files(path = here::here("data"))
for (i in 1:length(a)){
  b <- read_csv(file = here::here("data", a[i]))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
}

## Cruise data -----------------------------------------------------------------

dat_survreg <- 
  dplyr::right_join( # get SRVY
    x = racebase_foss_join_foss_cpue_haul0 %>% 
      dplyr::select(SRVY = srvy, survey_definition_id, year, vessel_id) %>% 
      dplyr::distinct(),
    y = race_data_v_cruises0 %>% 
      dplyr::select(year, cruise_id, cruise, vessel_id, start_date, end_date, survey_definition_id, vessel_name) %>% 
      dplyr::distinct(),
    by = c("survey_definition_id", 'year', 'vessel_id')) %>% 
  dplyr::arrange(-year) %>%
  dplyr::mutate(
    vessel_shape = as.factor(substr(x = vessel_name, start = 1, stop = 1)), 
    vessel_ital = paste0("F/V *", stringr::str_to_title(vessel_name), "*"), 
    vessel_name = paste0("F/V ", stringr::str_to_title(vessel_name)),
    SRVY = dplyr::case_when(
      survey_definition_id == 98 ~ "EBS", 
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 47 ~ "GOA", 
      survey_definition_id == 52 ~ "AI"), 
    region_long = dplyr::case_when(
      SRVY == "EBS" ~ "Eastern Bering Sea", 
      SRVY == "NBS" ~ "Northern Bering Sea", 
      SRVY == "GOA" ~ "Gulf of Alaska", 
      SRVY == "AI" ~ "Aleutian Islands"), 
    reg_dates = paste0(
      format(x = min(as.Date(start_date), na.rm = TRUE), "%b %d"),
      " - ", 
      format(x = max(as.Date(end_date), na.rm = TRUE), "%b %d"))) %>% 
  dplyr::select(year, SRVY, cruise_id, cruise, reg_dates, vessel_id, vessel_shape, vessel_name, vessel_ital, region_long) %>% 
  dplyr::distinct()

# > dat_survreg
# # A tibble: 742 × 9
# year SRVY  cruise_id reg_dates       vessel_id vessel_shape vessel_name            vessel_ital              region_long
# <dbl> <chr>     <dbl> <chr>               <dbl> <fct>        <chr>                  <chr>                    <chr>      
#   1  2023 GOA         765 Mar 10 - Aug 24       148 O            F/V Ocean Explorer     F/V *Ocean Explorer*     Gulf of Al…
# 2  2023 GOA         766 Mar 10 - Aug 24       176 A            F/V Alaska Provider    F/V *Alaska Provider*    Gulf of Al…
# 3  2023 NBS         763 Mar 10 - Aug 24       134 N            F/V Northwest Explorer F/V *Northwest Explorer* Northern B…

## New haul data ---------------------------------------------------------------------

# This has a specific username and password because I DO NOT want people to have access to this!
source("https://raw.githubusercontent.com/afsc-gap-products/metadata/main/code/functions_oracle.R")

locations <- c("C:/Users/liz.dawson/Work/Projects/ConnectToOracle.R", 
               "Z:/Projects/ConnectToOracle.R", 
               "C:/Users/emily.markowitz/Work/projects/ConnectToOracle.R")

for (i in 1:length(locations)){
  if (file.exists(locations[i])) {source(locations[i])}
}

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

# Is there new data in the EDIT_HAULS that is not represented in HAUL?
lastdl <- Sys.Date()

date_max <- RODBC::sqlQuery(channel, paste0("SELECT CREATE_DATE FROM RACE_DATA.EDIT_HAULS;"))
date_max <- sort(as.numeric(unique(unlist(format(date_max, format = "%Y")))))
date_max <- max(date_max[date_max<=format(Sys.Date(), format = "%Y")]) # sometimes there are dates that haven't happened yet b/c testing

if (max(racebase_foss_join_foss_cpue_haul0$year) < date_max) { # if this year's data hasn't been entered into the production data
  
  temperature_raw <- dplyr::left_join( 
    
    x = RODBC::sqlQuery(channel, paste0( 
      "SELECT HAUL_ID, 
EDIT_DATE_TIME, 
EDIT_LATITUDE AS latitude_dd_start, 
EDIT_LONGITUDE AS longitude_dd_start 
FROM RACE_DATA.EDIT_EVENTS
WHERE EVENT_TYPE_ID = 3;")) %>%  # 3	On bottom time
      dplyr::rename(date = EDIT_DATE_TIME) %>%
      dplyr::filter(format(date, format = "%Y") == date_max),
    
    y = RODBC::sqlQuery(channel, paste0( #  EDIT_GEAR_TEMPERATURE_UNITS, EDIT_SURFACE_TEMPERATURE_UNITS, ABUNDANCE_HAUL, CREATE_DATE, 
      "SELECT HAUL_ID, CRUISE_ID, HAUL, STATION, STRATUM, 
EDIT_SURFACE_TEMPERATURE AS surface_temperature_c, 
EDIT_GEAR_TEMPERATURE AS bottom_temperature_c
FROM RACE_DATA.EDIT_HAULS;")), 
    
    by = "HAUL_ID")  %>% 
    janitor::clean_names() %>% 
    dplyr::left_join(
      y = dat_survreg %>% 
        dplyr::select(cruise_id, SRVY, vessel_id), 
      by = "cruise_id")
  
  temp0 <- temperature_raw %>% 
    dplyr::filter(SRVY %in% c("GOA")) %>% 
    unique()
  temp1 <- # add this year's allocated stations
    readxl::read_xlsx(path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"), 
                      sheet = "2023_GOA", 
                      skip = 1) %>%
    dplyr::select(SRVY, station, stratum) %>% 
    dplyr::filter(!(paste0(stratum, "-", station) %in% paste0(temp0$stratum, "-", temp0$station))) %>%
    unique() 
  
  temp2 <- # add this year's allocated stations
    readxl::read_xlsx(path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"), 
                      sheet = "2023_BS", 
                      skip = 1) %>%
    dplyr::select(SRVY, station, stratum) %>%
    unique()  %>% 
    dplyr::filter(!(station %in% temperature_raw$station[temperature_raw$SRVY %in% c("NBS", "EBS")])) # %>% 
    # dplyr::left_join(y = dat_survreg %>% 
    #                    dplyr::filter(year == date_max &
    #                                    SRVY %in% c("NBS", "EBS")) %>% 
    #                    dplyr::select(SRVY))
  
  temperature_raw <- temperature_raw %>% 
    dplyr::bind_rows(temp1) %>% 
    dplyr::bind_rows(temp2) %>% 
    # dplyr::left_join(x = ., 
    #                  y = dat_survreg %>% 
    #                    dplyr::filter(year == date_max)) %>% 
    dplyr::mutate(
      year = date_max,
      latitude_dd_start = latitude_dd_start/100, 
                  longitude_dd_start = longitude_dd_start/100, 
                  data_type = "raw") %>% 
    dplyr::select(-haul_id) 

} else {
  temperature_raw <- data.frame()
}

## Combined Previous and New haul data --------------------------------------------------------------------

dat <- haul <- racebase_foss_join_foss_cpue_haul0 %>% 
  dplyr::rename(SRVY = srvy, 
                date = date_time) %>%
  dplyr::left_join(x = ., # get cruise_id
                   y = dat_survreg %>%
                     dplyr::select(SRVY, year, cruise, cruise_id, vessel_id)) %>%
  dplyr::filter(
    !(is.na(station)) &
      !is.na(surface_temperature_c) &
      !is.na(bottom_temperature_c)) %>% 
  dplyr::mutate(data_type = "offical") %>% 
  dplyr::filter( 
    # there shouldn't be bottom temps of 0 in the AI or GOA
    ((SRVY %in% c("AI", "GOA") & surface_temperature_c != 0) | (SRVY %in% c("EBS", "NBS"))) & 
      ((SRVY %in% c("AI", "GOA") & bottom_temperature_c != 0) | (SRVY %in% c("EBS", "NBS")))) %>% 
  # dplyr::select(list(names(temperature_raw)))
  dplyr::bind_rows(temperature_raw) %>% # incorporate new data

  dplyr::mutate(date = as.Date(format(date, format = "%Y-%m-%d"))) %>%
  dplyr::select(
    SRVY, year, stratum, station, date, data_type, cruise_id, 
    vessel_id, 
    st = surface_temperature_c,
    bt = bottom_temperature_c,
    latitude = latitude_dd_start,
    longitude = longitude_dd_start ) %>%
  dplyr::left_join( # get SRVY info
    x = ., 
    y = dat_survreg %>% 
      dplyr::select(SRVY, region_long, reg_dates) %>% 
      unique()) %>%
  dplyr::left_join( # get vessel info
    x = ., 
    y = dat_survreg %>% 
      dplyr::select(vessel_name, vessel_id, vessel_shape, vessel_ital) %>% 
      unique()) 

# Shapefiles -------------------------------------------------------------------

# load(file = here::here("data", "shp.rdata"))
# shp_vess <- dplyr::left_join(
#   dat %>% 
#     dplyr::select(SRVY, year, date, station, stratum, vessel_id, vessel_shape, vessel_name, vessel_name), 
#   shp_stn)


# Load shape files -------------------------------------------------------------

## EBS + NBS  ------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "bs.all", set.crs = "auto")
survey_area$survey.grid <- survey_area$survey.grid %>% 
  sf::st_transform(x = ., survey_area$crs$input) %>%
  dplyr::rename(station = STATIONID) %>%
  dplyr::left_join(x = ., 
                   y = haul %>%
                     # dplyr::rename(station = stationid) %>% 
                     dplyr::select(station, stratum) %>% 
                     dplyr::distinct(), 
                   by = "station") %>% 
  dplyr::mutate(region = "Bering Sea")
survey_area$place.labels$y[survey_area$place.labels$lab == "200 m"] <- -60032.7
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(SRVY = ifelse(SURVEY == "EBS_SHELF", "EBS", "NBS"))
shp_bs <- survey_area

# ## EBS  ------------------------------------------------------------------------
# survey_area <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
# survey_area$survey.area <- shp_bs$survey.area %>% 
#   dplyr::filter(SRVY == "EBS")
# shp_ebs <- survey_area
# 
# ## NBS  ------------------------------------------------------------------------
# survey_area <- akgfmaps::get_base_layers(select.region = "bs.north", set.crs = "auto")
# survey_area$survey.area <- shp_bs$survey.area %>% 
#   dplyr::filter(SRVY == "NBS")
# shp_nbs <- survey_area

## AI  ------------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto")
survey_area$survey.grid <-
  dplyr::left_join(
    x = survey_area$survey.grid %>%
      dplyr::rename(station = ID,
                    stratum = STRATUM),
    y = goa_goa_strata0 %>%
      dplyr::filter(survey == "AI") %>%
      dplyr::mutate(SRVY = "AI",
                    region = stringr::str_to_title(inpfc_area),
                    region = dplyr::case_when(
                      region %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians",
                      TRUE ~ region)) %>%
      dplyr::select(SRVY, stratum, region) %>%
      dplyr::distinct(),
    by = "stratum")  %>%
  dplyr::arrange(region) %>%
  dplyr::filter(!is.na(region))
survey_area$survey.area <- survey_area$survey.area %>%
  dplyr::mutate(SURVEY = "AI",
                SRVY = "AI")
shp_ai <- survey_area

## GOA  ------------------------------------------------------------------------

survey_area <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
survey_area$survey.grid <-  
  dplyr::left_join(
    x = survey_area$survey.grid %>%
      dplyr::rename(station = ID, 
                    stratum = STRATUM),
    y = goa_goa_strata0 %>%
      dplyr::filter(survey == "GOA") %>%
      dplyr::mutate(SRVY = "GOA",
                    region = stringr::str_to_title(inpfc_area) ) %>%
      dplyr::select(SRVY, stratum, region) %>%
      dplyr::distinct(),
    by = "stratum")  %>% 
  dplyr::arrange(region) %>% 
  dplyr::filter(!is.na(region))
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(SURVEY = "GOA", 
                SRVY = "GOA")
shp_goa <- survey_area


## bsslope  ------------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "auto")
# survey_area$survey.grid <- survey_area$survey.grid %>% 
#   sf::st_transform(x = ., shp_bs$crs$input) %>%
#   dplyr::rename(station = STATIONID) %>%
#   dplyr::left_join(x = ., 
#                    y = haul %>%
#                      # dplyr::rename(station = stationid) %>% 
#                      dplyr::select(station, stratum) %>% 
#                      dplyr::distinct(), 
#                    by = "station") %>% 
#   dplyr::mutate(region = "Bering Sea")
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(SRVY = "BSS")
shp_bss <- survey_area