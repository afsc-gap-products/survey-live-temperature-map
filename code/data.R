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

# Load new data from Google Sheet -------------------------------------------------
# if (data_source == "gd") {

if (googledrive_dl) {
  # https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
  googledrive::drive_download(file = googledrive::as_id(dir_googledrive_log),  #"gap_survey_progression.csv",
                              type = "xlsx", 
                              overwrite = TRUE, 
                              path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"))
}
a <- readxl::excel_sheets(path = paste0(dir_wd, "data/gap_survey_progression.xlsx"))
a <- a[grepl(pattern = ifelse(istest, "TEST", maxyr), x = a)]
a <- a[!grepl(pattern = "GOA_", x = a)]
a <- a[!grepl(pattern = "AI_", x = a)]
a <- a[!grepl(pattern = "BS_", x = a)]
dat_googledrive <- data.frame()
if (length(a)>0) { # if there are enteries for this year in the spreadsheet
  for (i in a) {
    b <- readxl::read_xlsx(path = paste0(dir_wd, "data/gap_survey_progression.xlsx"), sheet = i, skip = 2)
    # if (grepl(pattern = "AI", x = i) | grepl(pattern = "GOA", x = i)) {
    #   b <- b %>% 
    #     dplyr::filter(!is.na(gear_temperature_c))
    # }
    
    dat_googledrive <- dplyr::bind_rows( 
      dat_googledrive, 
      b %>% 
        dplyr::filter(!is.na(station))%>%
        dplyr::mutate(
          gear_temperature_c = as.numeric(gear_temperature_c), 
          station = as.character(station), 
          stratum = as.numeric(stratum)))
  }
  dat_googledrive <- dat_googledrive %>% 
    dplyr::filter(!is.na(SRVY)) %>% 
    dplyr::mutate(
      data_type = "googledrive",
      year = maxyr, # as.numeric(format(x = date, "%Y")), 
      date = as.Date(date), 
      cruise = as.numeric(paste0(maxyr, ifelse(SRVY == "NBS", "02", "01"))), 
      survey_definition_id = dplyr::case_when(
        SRVY == "EBS" ~ 98, 
        SRVY == "NBS" ~ 143, 
        SRVY == "BSS" ~ 78,
        SRVY == "GOA" ~ 47, 
        SRVY == "AI" ~ 52)) %>% 
    dplyr::rename(date_time_start = date) 
}

# } else 
if (data_source == "oracle") {
  # Load data new from Oracle edit tables -------------------------------------------------
  
  if (file.exists("Z:/Projects/ConnectToOracle.R")) {
    source("Z:/Projects/ConnectToOracle.R")
    channel <- channel
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
  
  if (format(max(gap_products_akfin_haul0$date_time_start), format = "%Y") < date_max) { # if this year's data hasn't been entered into the production data
    
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
      dplyr::mutate(
        year = date_max,
        latitude_dd_start = latitude_dd_start/100, 
        longitude_dd_start = longitude_dd_start/100, 
        data_type = "raw") %>% 
      dplyr::select(-haul_id) %>% 
      dplyr::left_join(
        y = dat_googledrive %>%
          dplyr::select(SRVY, station, stratum)) %>% 
      dplyr::filter(!is.na(SRVY))
  }
}

# The official GAP_PRODUCTS data for previous years ----------------------------

dat_survey <- 
  dplyr::right_join(
    x = gap_products_akfin_haul0 %>% 
      dplyr::filter(
        !(is.na(station)) &
          !is.na(surface_temperature_c) &
          !is.na(gear_temperature_c)) %>% 
      dplyr::select(cruisejoin, hauljoin, station, stratum, date_time_start, 
                    latitude_dd_start, longitude_dd_start, surface_temperature_c, gear_temperature_c) %>% 
      dplyr::distinct(), 
    y = gap_products_akfin_cruise0 %>% 
      dplyr::select(cruisejoin, cruise, year, vessel_id, survey_definition_id, vessel_name) %>% # , date_start, date_end
      dplyr::distinct()) %>% 
  dplyr::mutate(data_type = "offical")

if (data_source == "gd" & 
    max(dat_survey$year) < max(dat_googledrive$year, na.rm = TRUE)) {
  
  dat_survey <- dat_survey %>%
    dplyr::filter(year < maxyr) %>%
    dplyr::bind_rows(
      dat_googledrive %>% # Combined previous and new haul data
        dplyr::mutate(vessel_name1 = toupper(vessel_name)) %>%
        dplyr::left_join( # join to a distinct list of vessels
          dat_survey %>%
            dplyr::mutate(vessel_name1 = toupper(vessel_name)) %>%
            dplyr::select(vessel_name1, vessel_id) %>%
            dplyr::filter(!is.na(vessel_id)) %>%
            dplyr::distinct())) %>%
    dplyr::select(-vessel_name1)
  
  # dat_survey <- dat_survey %>%
  #   dplyr::filter(year < maxyr) %>%
  #   dplyr::bind_rows(dat_googledrive) # Combined previous and new haul data
}

dat_survey <- race_data_cruises0 %>% 
  dplyr::select(survey_id, cruise, vessel_id, 
                date_start = start_date, date_end = end_date) %>% 
  dplyr::left_join(dat_survey %>% 
                     dplyr::select(cruise, vessel_id, survey_definition_id)) %>% 
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

dat_survey <- dat_survey %>% 
  dplyr::arrange(-year) %>%
  dplyr::mutate(
    vessel_shape = ifelse(is.na(vessel_name), NA, as.character(substr(x = vessel_name, start = 1, stop = 1))), 
    vessel_ital = ifelse(is.na(vessel_name), NA, paste0("F/V *", stringr::str_to_title(vessel_name), "*")), 
    vessel_name = ifelse(is.na(vessel_name), NA, paste0("F/V ", stringr::str_to_title(vessel_name))),
    SRVY = dplyr::case_when(
      survey_definition_id == 98 ~ "EBS", 
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 78 ~ "BSS",
      survey_definition_id == 47 ~ "GOA", 
      survey_definition_id == 52 ~ "AI"), 
    survey = dplyr::case_when(
      SRVY == "EBS" ~ "Eastern Bering Sea", 
      SRVY == "NBS" ~ "Northern Bering Sea", 
      SRVY == "BSS" ~ "Bering Sea Slope", 
      SRVY == "GOA" ~ "Gulf of Alaska", 
      SRVY == "AI" ~ "Aleutian Islands"), 
    date = as.Date(date_time_start)) %>% 
  # dplyr::filter( # there shouldn't be bottom temps of 0 in the AI or GOA
  #   ((survey_definition_id %in% c(52, 47) & surface_temperature_c != 0) | 
  #      (survey_definition_id %in% c(78, 98, 143))) & 
  #     ((survey_definition_id %in% c(52, 47) & gear_temperature_c != 0) | 
  #        (survey_definition_id %in% c(78, 98, 143)))) %>% 
  dplyr::rename(st = surface_temperature_c, 
                bt = gear_temperature_c) 
# dplyr::select(year, SRVY, survey_dates, vessel_id, vessel_shape, vessel_name, vessel_ital, survey) %>% 
# dplyr::distinct() 

# Load shapefiles --------------------------------------------------------------

load(file = here::here("data", "shp_all.rdata"))


