#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' maintained: Emily Markowitz
#' purpose: download oracle data
#' ---------------------------


# Load shapefiles --------------------------------------------------------------

load(file = paste0(dir_wd, "data/shp_all.rdata"), verbose = TRUE)
shp <- shp_all

# Load files --------------------------------------------------------------

a <- list.files(path = paste0(dir_wd, "data/"))
a <- a[grepl(pattern = ".csv", x = a, fixed = TRUE)] # remove xlxsx
for (i in 1:length(a)){
  if (grepl(pattern = ".csv", x = a[i], fixed = TRUE)) {
    b <- readr::read_csv(file = paste0(dir_wd, "data/", a[i]))
  }
  b <- b |> 
    rename_all(tolower)
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

# if this year's data hasn't been entered into the production data
# if (format(max(dat_foss0$date_time_start), format = "%Y") < date_max | istest) { 

dat_race_data <- dplyr::left_join(
  
  x = RODBC::sqlQuery(channel, paste0( 
    "SELECT HAUL_ID, 
EDIT_DATE_TIME, 
EDIT_LATITUDE AS latitude_dd_start, 
EDIT_LONGITUDE AS longitude_dd_start 
FROM RACE_DATA.EDIT_EVENTS
WHERE EVENT_TYPE_ID = 3;")) |>  # 3	On bottom time
    dplyr::rename(date = EDIT_DATE_TIME), # cant define above because DATE is a function name in oracle
  
  y = RODBC::sqlQuery(channel, paste0( #  EDIT_GEAR_TEMPERATURE_UNITS, EDIT_SURFACE_TEMPERATURE_UNITS, ABUNDANCE_HAUL, CREATE_DATE, 
    "SELECT HAUL_ID, 
CRUISE_ID, 
-- PERFORMANCE, 
HAUL, 
STATION, 
--- STRATUM, 
EDIT_SURFACE_TEMPERATURE AS st, -- surface_temperature_c, 
EDIT_GEAR_TEMPERATURE AS bt -- bottom_temperature_c
FROM RACE_DATA.EDIT_HAULS
WHERE PERFORMANCE >= 0
AND HAUL_TYPE  = 3;")), # WHERE ABUNDANCE_HAUL = 'Y'. Test tows are (7 GOA, 0 EBS)
  
  by = "HAUL_ID")  |> 
  rename_all(tolower) |> 
  dplyr::filter(!is.na(cruise_id))  |> 
  dplyr::filter(format(as.Date(date), "%Y") == date_max) |>
  dplyr::mutate(
    year = date_max,
    date = unlist(lapply(strsplit(x = as.character(date), split = " ", fixed = TRUE), '[[', 1)), 
    date = format(as.Date(date), "%Y-%m-%d", tz = ''), 
    date = as.Date(date), 
    latitude_dd_start = latitude_dd_start/100, 
    longitude_dd_start = longitude_dd_start/100, 
    source = "race_data") |> 
  dplyr::left_join(race_data_cruises_mod0 |>
                     dplyr::select(cruise, cruise_id, vessel_id, vessel_name, survey_definition_id) |> 
                     dplyr::distinct())  |>     
  dplyr::select(-cruise_id, -haul) |> # , -haul_id 
  dplyr::mutate(cruise = as.numeric(cruise), 
                vessel_name = stringr::str_to_title(vessel_name)#, 
                # date = as.Date(date, "%Y-%m-%d", tz = '') 
                ) |> 
  dplyr::select(-vessel_id, -latitude_dd_start, -longitude_dd_start)

if (istest) {
  dat_race_data <- dat_race_data |> 
    dplyr::mutate(
      cruise = as.numeric(paste0(maxyr, substr(start = 5, stop = 6, cruise))), 
      date = as.Date(paste0(maxyr, "-", format(as.Date(date), format = "%m-%d"))) ) |> 
    dplyr::filter(as.Date(date) < as.Date("2025-07-01"))
  # TOLEDO
  dat_race_data <- dat_race_data |> 
    dplyr::filter(date < "2025-07-01")
}

# Load new data from Google Sheet -------------------------------------------------
# if (data_source == "gd") {

# if this year's data hasn't been entered into the production data
# if (format(max(dat_foss0$date_time_start), format = "%Y") < date_max | istest) {

# if (googledrive_dl) {
# https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
googledrive::drive_download(file = googledrive::as_id(dir_googledrive_log),  #"gap_survey_progression.csv",
                            type = "xlsx", 
                            overwrite = TRUE, 
                            path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"))

a <- readxl::excel_sheets(path = paste0(dir_wd, "data/gap_survey_progression.xlsx"))
a <- a[grepl(pattern = ifelse(istest, "TEST", maxyr), x = a)]
dat_googledrive <- data.frame()
if (length(a)>0) { # if there are enteries for this year in the spreadsheet
  for (i in a) {
    b <- readxl::read_xlsx(path = paste0(dir_wd, "data/gap_survey_progression.xlsx"), sheet = i, skip = 1)
    
    if (grepl(pattern = "_BS", x = i)) {
      b <- b |> 
        dplyr::left_join(
          dat_foss0 |> 
            dplyr::filter(srvy %in% c("EBS", "NBS")) |> 
            dplyr::select(srvy, stratum, station) |>
            dplyr::distinct()
        )
    }
    
    dat_googledrive <- dplyr::bind_rows( 
      dat_googledrive, 
      b |> 
        # dplyr::filter(!is.na(date))|>
        dplyr::mutate(
          bt = as.numeric(bt), 
          station = as.character(station)))
  }
}

dat_googledrive <- dat_googledrive  |> 
  dplyr::filter(!is.na(srvy))  |>
  dplyr::mutate(
    source = "googledrive", 
    cruise = as.numeric(paste0(maxyr, ifelse(srvy == "NBS", "02", "01"))), 
    stratum = as.numeric(stratum),
    year = maxyr, # as.numeric(format(x = date, "%Y")), 
    date = as.Date(date, "%Y-%m-%d", tz = 'America/Anchorage')+1, 
    survey_definition_id = dplyr::case_when(
      srvy == "EBS" ~ 98,
      srvy == "NBS" ~ 143,
      srvy == "BSS" ~ 78,
      srvy == "GOA" ~ 47,
      srvy == "AI" ~ 52)
  ) |> 
  dplyr::select(srvy, survey_definition_id, year, stratum, station, cruise, date, bt,  
                vessel_name, source)

# dat_googledrive <- dplyr::bind_rows(
#   dat_googledrive |> 
#     dplyr::filter(srvy %in% c("GOA", "AI")) |> 
#     dplyr::filter(!is.na(bt) ), 
#   dat_googledrive  |> # remove data with no vessel for the EBS/NBS
#     dplyr::filter(srvy %in% c("EBS", "NBS")) |> 
#     dplyr::filter(!is.na(vessel_name)) )

# Combine all data sources -----------------------------------------------------

# prioritize data already in RACE_DATA
if (nrow(dat_race_data) == 0) {
  dat_googledrive <- dat_googledrive |> 
    dplyr::filter(date > as.Date(paste0(maxyr, "-12-31")) )
} else {
  dat_googledrive <- dplyr::left_join(dat_googledrive, 
                                      dat_race_data |> 
                                        dplyr::group_by(vessel_name, survey_definition_id) |> 
                                        dplyr::summarise(max_racedata_date = max(date, na.rm = TRUE)) |> 
                                        dplyr::ungroup()) |> 
    dplyr::filter(date > max_racedata_date) |> 
    dplyr::select(-max_racedata_date) |> 
    dplyr::bind_rows(dat_googledrive |> 
                       dplyr::filter(!is.na(station)) |> 
                       dplyr::filter(is.na(vessel_name)))
}

dat_survey <- 
  # bind race_data and google drive data
  dplyr::bind_rows(
    dat_race_data, 
    dat_googledrive) |> 
  dplyr::select(-srvy, -stratum) 

diff <- setdiff((unique(race_data_cruises_mod0$survey_definition_id[substr(race_data_cruises_mod0$cruise, 1, 4) == maxyr])), 
                unique(dat_survey$survey_definition_id) )

if (length(diff) > 0) {
  dat_survey <- dat_survey |> 
    dplyr::bind_rows(dat_survey |> 
                       head(length(diff)) |> 
                       dplyr::mutate(haul_id = NA, 
                                     station = NA, 
                                     cruise = as.numeric(paste0(year, "02")), 
                                     source = "fake", 
                                     vessel_name = NA, 
                                     survey_definition_id = diff))
}


dat_survey <- dat_survey |>
  # bind foss data
  dplyr::bind_rows(
    dat_foss0 |> 
      dplyr::filter(
        !(is.na(station)) &
          !is.na(st) &
          !is.na(bt)) |> 
      dplyr::select(survey_definition_id, year, station, stratum, date, 
                    vessel_name, vessel_id, 
                    latitude_dd_start, longitude_dd_start, st, bt) ) |> 
  dplyr::mutate(
    srvy = dplyr::case_when(
      survey_definition_id == 98 ~ "EBS",
      survey_definition_id == 143 ~ "NBS",
      survey_definition_id == 78 ~ "BSS",
      survey_definition_id == 47 ~ "GOA",
      survey_definition_id == 39 ~ "GOA",
      survey_definition_id == 52 ~ "AI"),
    survey = dplyr::case_when(
      srvy == "EBS" ~ "Eastern Bering Sea",
      srvy == "NBS" ~ "Northern Bering Sea",
      srvy == "BSS" ~ "Bering Sea Slope",
      srvy == "GOA" ~ "Gulf of Alaska",
      srvy == "AI" ~ "Aleutian Islands"),  
    vessel_name = ifelse(is.na(vessel_name), NA, stringr::str_to_title(vessel_name)), 
    vessel_ital = ifelse(is.na(vessel_name), NA, paste0("F/V *", stringr::str_to_title(vessel_name), "*")),
    vessel_shape = ifelse(is.na(vessel_name), NA, substr(vessel_name, start = 1, stop = 1)),          
    vessel_name = ifelse(is.na(vessel_name), NA, paste0("F/V ", vessel_name) )
  )

# join cruise data
dat_survey <- race_data_cruises_mod0 |> 
  dplyr::select(cruise, vessel_id, survey_definition_id, 
                date_start = start_date, date_end = end_date) |> 
  dplyr::filter(!is.na(survey_definition_id)) |> 
  dplyr::filter(!is.na(date_start)) |>
  dplyr::group_by(survey_definition_id, cruise) |>
  dplyr::summarise(
    date_start = min(date_start, na.rm = TRUE), 
    date_end = max(date_end, na.rm = TRUE)) |> 
  dplyr::mutate(survey_dates = paste0(
    format(x = as.Date(date_start), "%b %d"),
    " - ", 
    format(x = as.Date(date_end), "%b %d"))) |> 
  dplyr::ungroup() |>
  dplyr::right_join(dat_survey) %>% 
  dplyr::select(-stratum, -vessel_id) 

