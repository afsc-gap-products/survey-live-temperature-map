#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' maintained: Emily Markowitz
#' purpose: download oracle data
#' ---------------------------

a <- list.files(path = paste0(dir_wd, "data/"))
# a <- a[grepl(pattern = ".", x = a, fixed = TRUE)] # remove folders
a <- a[grepl(pattern = ".csv", x = a, fixed = TRUE)] # remove xlxsx
for (i in 1:length(a)){
  if (grepl(pattern = ".csv", x = a[i], fixed = TRUE)) {
    b <- readr::read_csv(file = paste0(dir_wd, "data/", a[i]))
    # } else if (grepl(pattern = ".xlsx", x = a[i], fixed = TRUE)) {
    #   b <- readxl::read_xlsx(path = paste0(dir_wd, "data/")
  }
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(
    pattern = ifelse(grepl(pattern = ".csv", x = a[i], fixed = TRUE), ".csv", ".xlsx"), 
    replacement = "", x = paste0(a[i], "0")), 
    value = b)
}

# } else if (data_source == "oracle") {
# Load data new from Oracle edit tables -------------------------------------------------

if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
  if (istest == TRUE) { channel <- channel_test }
} else {
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
# date_max <- ifelse(istest, maxyr, date_max)

# if this year's data hasn't been entered into the production data
if (format(max(dat_foss0$date_time_start), format = "%Y") < date_max | istest) { 
  
  date_max0 <- ifelse(istest, 2023, date_max) # 2025 testing specific
  
  dat_race_data <- dplyr::left_join(
    
    x = RODBC::sqlQuery(channel, paste0( 
      "SELECT HAUL_ID, 
EDIT_DATE_TIME, 
EDIT_LATITUDE AS latitude_dd_start, 
EDIT_LONGITUDE AS longitude_dd_start 
FROM RACE_DATA.EDIT_EVENTS
WHERE EVENT_TYPE_ID = 3;")) %>%  # 3	On bottom time
      dplyr::rename(date = EDIT_DATE_TIME) %>% # cant define above because DATE is a function name in oracle
      dplyr::filter(format(date, format = "%Y") == date_max0),
    
    y = RODBC::sqlQuery(channel, paste0( #  EDIT_GEAR_TEMPERATURE_UNITS, EDIT_SURFACE_TEMPERATURE_UNITS, ABUNDANCE_HAUL, CREATE_DATE, 
      "SELECT HAUL_ID, 
CRUISE_ID, 
HAUL, 
STATION, 
STRATUM, 
EDIT_SURFACE_TEMPERATURE AS st, -- surface_temperature_c, 
EDIT_GEAR_TEMPERATURE AS bt -- bottom_temperature_c
FROM RACE_DATA.EDIT_HAULS
WHERE ABUNDANCE_HAUL = 'Y';")), 
    
    by = "HAUL_ID") %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!is.na(cruise_id)) %>%
  dplyr::mutate(
      year = date_max,
      date = format(as.Date(date), "%Y-%m-%d"), 
      latitude_dd_start = latitude_dd_start/100, 
      longitude_dd_start = longitude_dd_start/100, 
      source = "race_data") %>% 
    dplyr::left_join(race_data_cruises_mod0 %>%
                       dplyr::select(cruise, cruise_id, vessel_id, vessel_name, survey_definition_id) %>% 
                       dplyr::distinct())  %>%     
    dplyr::select(-cruise_id, -haul, -haul_id) %>% 
    dplyr::mutate(cruise = as.numeric(cruise))
  if (istest) {
    dat_race_data <- dat_race_data %>% 
      dplyr::mutate(
        cruise = as.numeric(paste0(maxyr, substr(start = 5, stop = 6, cruise))), 
        date = as.Date(paste0(maxyr, "-", format(as.Date(date), format = "%m-%d"))) )
  }
}
# }

# Load new data from Google Sheet -------------------------------------------------
# if (data_source == "gd") {

# if this year's data hasn't been entered into the production data
if (format(max(dat_foss0$date_time_start), format = "%Y") < date_max | istest) { 
  
  # if (googledrive_dl) {
  # https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
  googledrive::drive_download(file = googledrive::as_id(dir_googledrive_log),  #"gap_survey_progression.csv",
                              type = "xlsx", 
                              overwrite = TRUE, 
                              path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"))
  # }
  a <- readxl::excel_sheets(path = paste0(dir_wd, "data/gap_survey_progression.xlsx"))
  a <- a[grepl(pattern = ifelse(istest, "TEST", maxyr), x = a)]
  # a <- a[!grepl(pattern = "GOA_", x = a)]
  # a <- a[!grepl(pattern = "AI_", x = a)]
  # a <- a[!grepl(pattern = "BS_", x = a)]
  dat_googledrive <- data.frame()
  if (length(a)>0) { # if there are enteries for this year in the spreadsheet
    for (i in a) {
      b <- readxl::read_xlsx(path = paste0(dir_wd, "data/gap_survey_progression.xlsx"), sheet = i, skip = 1)
      # if (grepl(pattern = "AI", x = i) | grepl(pattern = "GOA", x = i)) {
      #   b <- b %>% 
      #     dplyr::filter(!is.na(gear_temperature_c))
      # }
      
      
      if (grepl(pattern = "BS_", x = i)) {
        b <- b %>% 
          dplyr::left_join(
          dat_foss0 %>% 
          dplyr::filter(srvy %in% c("EBS", "NBS")) %>% 
            dplyr::select(srvy = srvy, stratum, station) %>%
            dplyr::distinct()
          )
      }
      
      dat_googledrive <- dplyr::bind_rows( 
        dat_googledrive, 
        b %>% 
          dplyr::filter(!is.na(station))%>%
          dplyr::mutate(
            bt = as.numeric(bt), 
            station = as.character(station)))
    }
  }
  
  dat_googledrive <- dat_googledrive  %>% 
    dplyr::filter(!is.na(srvy)) %>% 
    dplyr::mutate(
      source = "googledrive", 
      cruise = as.numeric(paste0(maxyr, ifelse(srvy == "NBS", "02", "01"))), 
      stratum = as.numeric(stratum),
      year = maxyr # as.numeric(format(x = date, "%Y")), 
    ) %>% 
    dplyr::select(srvy, year, stratum, station, cruise, date, bt,  
                  vessel_name, source) 
}

# Combine all data sources -----------------------------------------------------

dat_survey <- 
  dplyr::left_join(
    dat_race_data, 
    dat_googledrive %>% 
      dplyr::filter(srvy %in% c("EBS", "NBS")) %>% 
      dplyr::mutate(
        survey_definition_id = dplyr::case_when(
          srvy == "EBS" ~ 98,
          srvy == "NBS" ~ 143,
          srvy == "BSS" ~ 78,
          srvy == "GOA" ~ 47,
          srvy == "AI" ~ 52)) %>% 
      dplyr::select(srvy, survey_definition_id, stratum, station, cruise, 
                    vessel_name_planned = vessel_name, 
                    date_planned = date, 
                    source_planned = source)) %>% 
  dplyr::bind_rows(
    dat_foss0 %>% 
      dplyr::filter(
        !(is.na(station)) &
          !is.na(st) &
          !is.na(bt)) %>% 
      dplyr::select(survey_definition_id, year, station, stratum, date, 
                    vessel_name, vessel_id, 
                    latitude_dd_start, longitude_dd_start, st, bt) ) %>% 
  dplyr::mutate(
  srvy = dplyr::case_when(
    survey_definition_id == 98 ~ "EBS",
    survey_definition_id == 143 ~ "NBS",
    survey_definition_id == 78 ~ "BSS",
    survey_definition_id == 47 ~ "GOA",
    survey_definition_id == 52 ~ "AI"),
  survey = dplyr::case_when(
    srvy == "EBS" ~ "Eastern Bering Sea",
    srvy == "NBS" ~ "Northern Bering Sea",
    srvy == "BSS" ~ "Bering Sea Slope",
    srvy == "GOA" ~ "Gulf of Alaska",
    srvy == "AI" ~ "Aleutian Islands"),  
  vessel_name_planned = ifelse(is.na(vessel_name_planned), NA, stringr::str_to_title(vessel_name_planned)), 
  vessel_ital_planned = ifelse(is.na(vessel_name_planned), NA, paste0("F/V *", stringr::str_to_title(vessel_name_planned), "*")),
  vessel_shape_planned = ifelse(is.na(vessel_name_planned), NA, substr(vessel_name_planned, start = 1, stop = 1)),          
  vessel_name_planned = ifelse(is.na(vessel_name_planned), NA, paste0("F/V ", vessel_name_planned)),           
  vessel_name = ifelse(is.na(vessel_name), NA, stringr::str_to_title(vessel_name)), 
  vessel_ital = ifelse(is.na(vessel_name), NA, paste0("F/V *", stringr::str_to_title(vessel_name), "*")),
  vessel_shape = ifelse(is.na(vessel_name), NA, substr(vessel_name, start = 1, stop = 1)),          
  vessel_name = ifelse(is.na(vessel_name), NA, paste0("F/V ", vessel_name))) # 

dat_survey <- race_data_cruises_mod0 %>% 
  dplyr::select(cruise, vessel_id, survey_definition_id, 
                date_start = start_date, date_end = end_date) %>% 
  dplyr::filter(!is.na(survey_definition_id)) %>% 
  dplyr::filter(!is.na(date_start)) %>%
  dplyr::group_by(survey_definition_id, cruise) %>%
  dplyr::summarise(
    date_start = min(date_start, na.rm = TRUE), 
    date_end = max(date_end, na.rm = TRUE)) %>% 
  dplyr::mutate(survey_dates = paste0(
    format(x = as.Date(date_start), "%b %d"),
    " - ", 
    format(x = as.Date(date_end), "%b %d"))) %>% 
  dplyr::ungroup() %>%
  dplyr::right_join(dat_survey)

# Load shapefiles --------------------------------------------------------------

load(file = here::here("data", "shp_all.rdata"), verbose = TRUE)
shp <- shp_all


