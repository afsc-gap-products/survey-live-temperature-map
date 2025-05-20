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
  vessel_name = ifelse(is.na(vessel_name), NA, paste0("F/V ", vessel_name))) # %>% 
  # dplyr::left_join(gap_products_akfin_area0 %>%
  #                    # dplyr::filter(area_type == "STRATUM") %>%
  #                    dplyr::left_join(gap_products_akfin_area0 %>%
  #                                       # dplyr::filter(area_type == "STRATUM") %>%
  #                                       dplyr::group_by(survey_definition_id) %>%
  #                                       dplyr::summarise(design_year = max(design_year)) %>%
  #                                       dplyr::ungroup() %>%
  #                                       dplyr::mutate(design_year = ifelse(survey_definition_id == 47, 1984, design_year)) %>% # TOLEDO - 2025 edit
  #                                       dplyr::mutate(include = TRUE) ) %>%
  #                    dplyr::filter(include == TRUE) %>%
  #                    dplyr::select(stratum = area_id, area_name, survey_definition_id, design_year) %>%
  #                    # dplyr::distinct() %>%
  #                    dplyr::mutate(area_name = dplyr::case_when(
  #                      # area_id %in% C(211, 15, 14, 113, 511) ~ "Shumagin",
  #                      # area_id %in% C(123, 24, 521, 222, 23) ~ "Chirikof",
  #                      # area_id %in% C(36, 136, 105, 37, 38, 531) ~ "Kodiak",
  #                      # area_id %in% C(43, 541, 242, 42) ~ "Yakutat",
  #                      # area_id %in% C(51, 551, 152, 352, 253) ~ "Southeastern",
  #                      grepl(pattern = "Prince of", x = area_name) ~ "Southeastern",
  #                      # grepl(pattern = "Alaska Pen", x = area_name) ~ "Alaska Pen",
  #                      grepl(pattern = "Barren Is", x = area_name) ~ "Kodiak",
  #                      grepl(pattern = "Lower Cook", x = area_name) ~ "Kodiak",
  #                      grepl(pattern = "Fairweather", x = area_name) ~ "Yakutat",
  #                      grepl(pattern = "Fox Isl", x = area_name) ~ "Shumagin",
  #                      grepl(pattern = "Middleton", x = area_name) ~ "Kodiak",
  #                      grepl(pattern = "Kenai", x = area_name) ~ "Kodiak",
  #                      grepl(pattern = "Albatross", x = area_name) ~ "Kodiak",
  #                      grepl(pattern = "Yakutat", x = area_name) ~ "Yakutat",
  #                      grepl(pattern = "Yakataga", x = area_name) ~ "Yakutat",
  #                      grepl(pattern = "Kodiak", x = area_name) ~ "Kodiak",
  #                      grepl(pattern = "Southeast", x = area_name) ~ "Southeastern",
  #                      grepl(pattern = "Davidson Bank", x = area_name) ~ "Shumagin",
  #                      grepl(pattern = "Upper Alaska", x = area_name) ~ "Chirikof",
  #                      grepl(pattern = "Lower Alaska", x = area_name) ~ "Shumagin",
  #                      grepl(pattern = "Portlock Flats", x = area_name) ~ "Kodiak",
  #                      grepl(pattern = "Shumagin", x = area_name) ~ "Shumagin",
  #                      grepl(pattern = "Sanak Gully", x = area_name) ~ "Shumagin",
  #                      grepl(pattern = "Chichagof", x = area_name) ~ "Southeastern",
  #                      grepl(pattern = "Semidi Bank", x = area_name) ~ "Chirikof",
  #                      grepl(pattern = "Chirikof", x = area_name) ~ "Chirikof",
  #                      grepl(pattern = "Shelikof", x = area_name) ~ "Chirikof",
  #                      TRUE ~ area_name
  #                    )) %>%
  #                    dplyr::select(-design_year) )
  
# require(sf)
# shape <- read_sf(dsn = here::here("data", "aigrid_trawable_thru2018_Emily.shp"), layer = "SHAPEFILE")

# ggplot2::ggplot() + # %>%
# #                   # dplyr::mutate(
# #                   #   area_id = stratum, 
# #                   #   area_name = dplyr::case_when(
# #                   #   area_id %in% C(211, 15, 14, 113, 511) ~ "Shumagin",
# #                   #   area_id %in% C(123, 24, 521, 222, 23) ~ "Chirikof",
# #                   #   area_id %in% C(36, 136, 105, 37, 38, 531) ~ "Kodiak",
# #                   #   area_id %in% C(43, 541, 242, 42) ~ "Yakutat",
# #                   #   area_id %in% C(51, 551, 152, 352, 253) ~ "Southeastern"
# #                   #   # TRUE ~ area_name
# #                   # )), 
# #                  
# #                   dplyr::mutate(
# #                     area_name = dplyr::case_when( 
# #                       stratum %in% c(211, 15, 14, 113, 511) ~ "Shumagin",
# #                       stratum %in% c(123, 24, 521, 222, 23, 321) ~ "Chirikof", 
# #                       stratum %in% c(36, 136, 105, 37, 38, 531, 135) ~ "Kodiak", 
# #                       stratum %in% c(43, 541, 242, 42, 144, 145) ~ "Yakutat", 
# #                       stratum %in% c(51, 551, 152, 352, 253, 252) ~ "Southeastern"#, 
# #                       # TRUE ~ stratum
# #                     )) %>% 
#                 # mapping = aes()  + 
#   # geom_sf(data = shp$survey.strata[shp$survey.strata$srvy == "GOA",], 
#   #         mapping = aes(geometry = geometry)) +
#   geom_point(data = dat_survey %>%
#                   dplyr::filter(year == 2025 &
#                                   srvy == "GOA"),
#              mapping = aes(color = area_name,
#                            #fill = area_name,
#                            x = longitude_dd_start,
#                            y = latitude_dd_start)) #+
#   # geom_sf_label(mapping = aes(label = stratum))


# aaa <- shp$survey.strata %>% 
#   dplyr::filter(srvy == "GOA") %>% 
#   dplyr::select(stratum, geometry) %>% 
#   dplyr::mutate(
#     # area_id = stratum, 
#     area_name = dplyr::case_when( # 
#       stratum %in% c(211, 15, 14, 113, 511) ~ "Shumagin",
#       stratum %in% c(123, 24, 521, 222, 23, 321) ~ "Chirikof", 
#       stratum %in% c(36, 136, 105, 37, 38, 531, 135) ~ "Kodiak", 
#       stratum %in% c(43, 541, 242, 42, 144, 145) ~ "Yakutat", 
#       stratum %in% c(51, 551, 152, 352, 253, 252) ~ "Southeastern"#, 
#       # TRUE ~ stratum
#     ))
# 
# ggplot(data = aaa) + # %>% dplyr::filter(is.na(area_name))) + 
#   geom_sf(mapping = aes(fill = area_name), color = "black") + 
#   geom_sf_label(mapping = aes(label = stratum), position = position_jitter(height = 1000)) 

# if (data_source == "gd") {
#   if (max(dat_survey$year) < max(dat_googledrive$year, na.rm = TRUE)) {
#     
#     dat_survey <- dat_survey %>%
#       dplyr::filter(year < maxyr) %>%
#       dplyr::bind_rows(
#         dat_googledrive %>% # Combined previous and new haul data
#           dplyr::mutate(vessel_name1 = toupper(vessel_name)) %>%
#           dplyr::left_join( # join to a distinct list of vessels
#             dat_survey %>%
#               dplyr::mutate(vessel_name1 = toupper(vessel_name)) %>%
#               dplyr::select(vessel_name1, vessel_id) %>%
#               dplyr::filter(!is.na(vessel_id)) %>%
#               dplyr::distinct())) %>%
#       dplyr::select(-vessel_name1)
#     
#     # dat_survey <- dat_survey %>%
#     #   dplyr::filter(year < maxyr) %>%
#     #   dplyr::bind_rows(dat_googledrive) # Combined previous and new haul data
#   }
# }

dat_survey <- race_data_cruises_mod0 %>% 
  dplyr::select(#survey_id, 
                cruise, vessel_id, survey_definition_id, 
                date_start = start_date, date_end = end_date) %>% 
  # dplyr::left_join(dat_survey %>% 
  #                    dplyr::select(cruise, vessel_id, survey_definition_id)) %>% 
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

# dat_survey <- dat_survey %>% 
#   dplyr::arrange(-year) %>%
#   dplyr::mutate(
#     vessel_shape = ifelse(is.na(vessel_name), NA, as.character(substr(x = vessel_name, start = 1, stop = 1))), 
#     vessel_ital = ifelse(is.na(vessel_name), NA, paste0("F/V *", stringr::str_to_title(vessel_name), "*")), 
#     vessel_name = ifelse(is.na(vessel_name), NA, paste0("F/V ", stringr::str_to_title(vessel_name))),
#     srvy = dplyr::case_when(
#       survey_definition_id == 98 ~ "EBS", 
#       survey_definition_id == 143 ~ "NBS", 
#       survey_definition_id == 78 ~ "BSS",
#       survey_definition_id == 47 ~ "GOA", 
#       survey_definition_id == 52 ~ "AI"), 
#     survey = dplyr::case_when(
#       srvy == "EBS" ~ "Eastern Bering Sea", 
#       srvy == "NBS" ~ "Northern Bering Sea", 
#       srvy == "BSS" ~ "Bering Sea Slope", 
#       srvy == "GOA" ~ "Gulf of Alaska", 
#       srvy == "AI" ~ "Aleutian Islands"), 
#     date = as.Date(date_time_start))  %>%
#   dplyr::filter(!is.na(srvy)) #%>% 
# # dplyr::filter( # there shouldn't be bottom temps of 0 in the AI or GOA
# #   ((survey_definition_id %in% c(52, 47) & surface_temperature_c != 0) | 
# #      (survey_definition_id %in% c(78, 98, 143))) & 
# #     ((survey_definition_id %in% c(52, 47) & gear_temperature_c != 0) | 
# #        (survey_definition_id %in% c(78, 98, 143)))) %>% 
# # dplyr::rename(st = surface_temperature_c, 
# #               bt = gear_temperature_c) 
# # dplyr::select(year, srvy, survey_dates, vessel_id, vessel_shape, vessel_name, vessel_ital, survey) %>% 
# # dplyr::distinct() 

# Load shapefiles --------------------------------------------------------------

load(file = here::here("data", "shp_all.rdata"), verbose = TRUE)
shp <- shp_all


