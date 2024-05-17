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

# if (data_source == "gd") {
# Load new data from Google Sheet -------------------------------------------------

if (googledrive_dl) {
  # https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
  googledrive::drive_download(file = googledrive::as_id(dir_googledrive_log),  #"gap_survey_progression.csv",
                              type = "xlsx", 
                              overwrite = TRUE, 
                              path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"))
}
a <- readxl::excel_sheets(path = paste0(dir_wd, "data/gap_survey_progression.xlsx"))
a <- a[grepl(pattern = maxyr, x = a)]
dat_googledrive <- data.frame()
if (length(a)>0) { # if there are enteries for this year in the spreadsheet
  for (i in a) {
    b <- readxl::read_xlsx(path = paste0(dir_wd, "data/gap_survey_progression.xlsx"), sheet = i, skip = 1)
    # if (grepl(pattern = "AI", x = i) | grepl(pattern = "GOA", x = i)) {
    #   b <- b %>% 
    #     dplyr::filter(!is.na(gear_temperature_c))
    # }
    
    dat_googledrive <- dplyr::bind_rows( 
      dat_googledrive, 
      b %>% 
        dplyr::filter(!is.na(station))%>%
        dplyr::mutate(
                      # data_type = i, 
                      station = as.character(station), 
                      stratum = as.numeric(stratum)))
  }
dat_googledrive <- dat_googledrive %>% 
  dplyr::filter(!is.na(SRVY)) %>% 
  dplyr::mutate(
    data_type = "googledrive",
                year = maxyr, # as.numeric(format(x = date, "%Y")), 
                date = as.Date(date), 
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
      dplyr::select(cruisejoin, year, vessel_id, date_start, date_end, survey_definition_id, vessel_name) %>% 
      dplyr::distinct()) %>% 
  dplyr::mutate(data_type = "offical")

if (data_source == "gd" & 
    max(dat_survey$year) < max(dat_googledrive$year, na.rm = TRUE)) {
dat_survey <- dat_survey %>% 
  dplyr::filter(year < maxyr) %>% 
  dplyr::bind_rows(dat_googledrive) # Combined previous and new haul data
}

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
    date = as.Date(date_time_start),
    survey_dates = paste0(
      format(x = min(as.Date(date_start), na.rm = TRUE), "%b %d"),
      " - ", 
      format(x = max(as.Date(date_end), na.rm = TRUE), "%b %d"))) %>% 
    # dplyr::filter( # there shouldn't be bottom temps of 0 in the AI or GOA
    #   ((survey_definition_id %in% c(52, 47) & surface_temperature_c != 0) | 
    #      (survey_definition_id %in% c(78, 98, 143))) & 
    #     ((survey_definition_id %in% c(52, 47) & gear_temperature_c != 0) | 
    #        (survey_definition_id %in% c(78, 98, 143)))) %>% 
  dplyr::rename(st = surface_temperature_c, 
                bt = gear_temperature_c) 
# dplyr::select(year, SRVY, survey_dates, vessel_id, vessel_shape, vessel_name, vessel_ital, survey) %>% 
  # dplyr::distinct() 

# Load shape files -------------------------------------------------------------

library(ggplot2)
library(viridis)
# devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
library(akgfmaps)
library(sf)

# crs.out <- shp_bs$survey_area$crs$input

shp_bs <- akgfmaps::get_base_layers(select.region = "bs.all", set.crs = "auto")
shp_ebs <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
shp_nbs <- akgfmaps::get_base_layers(select.region = "bs.north", set.crs = "auto")
shp_ai <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto")
shp_ai$survey.strata$Stratum <- shp_ai$survey.strata$STRATUM
shp_goa <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
shp_goa$survey.strata$Stratum <- shp_goa$survey.strata$STRATUM
shp_bss <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "auto")

aa <- gap_products_akfin_area0 %>% 
  dplyr::filter(design_year <= maxyr) %>% 
  dplyr::group_by(survey_definition_id) %>% 
  dplyr::summarise(design_year = max(design_year, na.rm = TRUE)) 

areas <- gap_products_akfin_area0  %>% 
  # find the most up to date design_year's
  dplyr::filter(eval(parse(text=paste0("(survey_definition_id == ", aa$survey_definition_id, 
                                       " & ", "design_year == ", aa$design_year, ") ", collapse = " | ")))) %>%
  # dplyr::filter((survey_definition_id %in% c(52, 47) & area_type %in% c("INPFC", "STRATUM")) | 
  #                 (survey_definition_id %in% c(143, 98, 78) & area_type == "STRATUM")) %>% 
  dplyr::filter(area_type %in% c("STRATUM", "INPFC")) %>% 
  dplyr::mutate(#area_name = stringr::str_to_title(inpfc_area),
    area_name = dplyr::case_when(
      area_name %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians",
      TRUE ~ area_name)#, 
    # SRVY = dplyr::case_when(
    #   survey_definition_id == 98 ~ "EBS", 
    #   survey_definition_id == 143 ~ "NBS", 
    #   survey_definition_id == 78 ~ "BSS",
    #   survey_definition_id == 47 ~ "GOA", 
    #   survey_definition_id == 52 ~ "AI")
    ) %>% 
  dplyr::select(survey_definition_id, area_id, area_type, area_name) 

areas <- dplyr::bind_rows(
  areas %>% 
    dplyr::mutate(stratum = area_id) %>%
    dplyr::filter(!(survey_definition_id %in% c(47, 52)) & 
                    area_type == "STRATUM"), 
    gap_products_akfin_stratum_groups0 %>% 
                     dplyr::filter(eval(parse(text=paste0("(survey_definition_id == ", aa$survey_definition_id, 
                                                          " & ", "design_year == ", aa$design_year, ") ", collapse = " | ")))) %>%
                     dplyr::filter(survey_definition_id %in% c(52, 47)) %>% 
                     dplyr::filter(area_id %in% unique(areas$area_id[areas$area_type == "INPFC"])) %>%
    dplyr::select(-design_year) %>% 
      dplyr::left_join(areas %>% 
                         dplyr::filter(area_type == "INPFC") ))  

shp_all <- shp <- list(
  # Stratum
  survey.strata = dplyr::bind_rows(list(
  shp_bs$survey.strata %>%
    sf::st_transform(crs = "EPSG:3338") %>%
    dplyr::mutate(SRVY = "BS", 
                  survey_definition_id = ifelse(SURVEY == "EBS_SHELF", 98, 143),
                  stratum = as.numeric(Stratum)) %>% 
    dplyr::select(-Stratum),
  shp_ebs$survey.strata %>%
    sf::st_transform(crs = "EPSG:3338") %>%
    dplyr::mutate(SRVY = "EBS", 
                  survey_definition_id = 98,
                  stratum = as.numeric(Stratum)) %>% 
    dplyr::select(-Stratum),
  shp_nbs$survey.strata  %>%
    sf::st_transform(crs = "EPSG:3338") %>%
    dplyr::mutate(survey = "NBS", 
                  survey_definition_id = 143,
                  stratum = as.numeric(Stratum)) %>% 
    dplyr::select(-Stratum),
  shp_ai$survey.strata %>%
    sf::st_transform(crs = "EPSG:3338") %>%
    dplyr::mutate(SRVY = "AI", 
                  survey_definition_id = 52,
                  stratum = as.numeric(STRATUM)) %>% 
    dplyr::select(-STRATUM),
  shp_goa$survey.strata %>%
    sf::st_transform(crs = "EPSG:3338") %>%
    dplyr::mutate(SRVY = "GOA",
                  survey_definition_id = 47,
                  stratum = as.numeric(STRATUM)) %>% 
    dplyr::select(-STRATUM, -Stratum),
  shp_bss$survey.strata %>%
    sf::st_transform(crs = "EPSG:3338") %>%
    dplyr::mutate(SRVY = "BSS", 
                  survey_definition_id = 78,
                  stratum = as.numeric(STRATUM)) %>% 
    dplyr::select(-STRATUM))) %>%
  dplyr::select(SRVY, survey_definition_id, stratum, geometry) %>%
  # dplyr::left_join(y = dat_survey %>%
  #                    dplyr::select(stratum, SRVY, survey, survey, survey_definition_id) %>% 
  #                    dplyr::distinct()) %>% 
    dplyr::left_join(y = areas, relationship = "many-to-many"), 
  
  # Regions
  survey.area = dplyr::bind_rows(list(
    shp_bs$survey.area %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "BS", 
                    SRVY1 = ifelse(SURVEY == "EBS_SHELF", "EBS", "NBS")),
    shp_ebs$survey.area %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "EBS"),
    shp_nbs$survey.area  %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "NBS"),
    shp_ai$survey.area %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "AI"),
    shp_goa$survey.area %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "GOA"),
    shp_bss$survey.area %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "BSS"))) %>%
    dplyr::select(SRVY, SRVY1, geometry), #  %>%
    # dplyr::left_join(y = dat_survey %>%
    #                    dplyr::select(SRVY, survey, survey, survey_definition_id) %>% 
    #                    dplyr::distinct()), 
  
  # Stations
  survey.grid = dplyr::bind_rows(list(
    shp_bs$survey.grid %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "BS", 
                    station = STATIONID) %>% 
      dplyr::left_join(y = dat_survey %>% 
                         dplyr::filter(SRVY %in% c("EBS", "NBS")) %>% 
                         dplyr::select(station, stratum, survey_definition_id) %>% 
                         dplyr::distinct()),
    shp_ebs$survey.grid %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "EBS", 
                    station = STATIONID) %>% 
      dplyr::left_join(y = dat_survey %>% 
                         dplyr::filter(SRVY == "EBS") %>% 
                         dplyr::select(SRVY, station, stratum, survey_definition_id) %>% 
                         dplyr::distinct()),
    shp_nbs$survey.grid  %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "NBS", 
                    station = STATIONID) %>% 
      dplyr::left_join(y = dat_survey %>% 
                         dplyr::filter(SRVY == "NBS") %>% 
                         dplyr::select(SRVY, station, stratum, survey_definition_id) %>% 
                         dplyr::distinct()),
    shp_ai$survey.grid %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "AI", 
                    survey_definition_id = 52, 
                    station = ID, 
                    stratum = STRATUM) %>% 
      dplyr::left_join(y = dat_survey %>% 
                         dplyr::filter(SRVY == "AI") %>% 
                         dplyr::select(SRVY, station, stratum) %>% 
                         dplyr::distinct()),
    shp_goa$survey.grid %>%
      sf::st_transform(crs = "EPSG:3338") %>%
      dplyr::mutate(SRVY = "GOA", 
                    survey_definition_id = 47, 
                    station = ID, 
                    stratum = STRATUM) %>% 
      dplyr::left_join(y = dat_survey %>% 
                         dplyr::filter(SRVY == "GOA") %>% 
                         dplyr::select(SRVY, station, stratum) %>% 
                         dplyr::distinct())
      # sf::st_transform(crs = "EPSG:3338") %>%
      # dplyr::mutate(SRVY = "GOA",
      #               station = ID, 
      #               stratum = STRATUM) %>% 
      # dplyr::left_join(y = dat_survey %>% 
      #                    dplyr::filter(SRVY == "GOA") %>% 
      #                    dplyr::select(SRVY, station, stratum, survey_definition_id) %>% 
      #                    dplyr::distinct())
    )) %>%
    dplyr::select(SRVY, survey_definition_id, station, stratum, geometry) %>%
    dplyr::left_join(y = areas, relationship = "many-to-many"), 
  
  # # plot.boundary
  # plot.boundary = dplyr::bind_rows(list(
  #   shp_bs$plot.boundary %>% 
  #     dplyr::mutate(SRVY = "BS", 
  #                   z = rownames(.)),
  #   shp_ebs$plot.boundary %>% 
  #     dplyr::mutate(SRVY = "EBS", 
  #                   z = rownames(.)),
  #   shp_nbs$plot.boundary %>% 
  #     dplyr::mutate(SRVY = "NBS", 
  #                   z = rownames(.)),
  #   shp_bss$plot.boundary %>% 
  #     dplyr::mutate(SRVY = "BSS", 
  #                   z = rownames(.)),
  #   shp_goa$plot.boundary %>% 
  #     dplyr::mutate(SRVY = "GOA", 
  #                   z = rownames(.)),
  #   shp_ai$plot.boundary %>% 
  #     dplyr::mutate(SRVY = "AI", 
  #                   z = rownames(.))
  # )), 
    
    # lon.breaks
    lon.breaks = list(
      "BS" = shp_bs$lon.breaks,
      "EBS" = shp_ebs$lon.breaks,
      "NBS" = shp_nbs$lon.breaks,
      "BSS" = shp_bss$lon.breaks,
      "GOA" = shp_goa$lon.breaks,
      "AI" = shp_ai$lon.breaks
    ), 
    
    # lat.breaks
    lat.breaks = list(
      "BS" = shp_bs$lat.breaks,
      "EBS" = shp_ebs$lat.breaks,
      "NBS" = shp_nbs$lat.breaks,
      "BSS" = shp_bss$lat.breaks,
      "GOA" = shp_goa$lat.breaks,
      "AI" = shp_ai$lat.breaks
    ), 
  
  # bathymetry
  bathymetry = dplyr::bind_rows(list(
    shp_bs$bathymetry %>% 
      dplyr::mutate(SRVY = "BS") %>%
      sf::st_transform(crs = "EPSG:3338"),
    shp_ebs$bathymetry %>% 
      dplyr::mutate(SRVY = "EBS") %>%
      sf::st_transform(crs = "EPSG:3338"),
    shp_nbs$bathymetry %>% 
      dplyr::mutate(SRVY = "NBS") %>%
      sf::st_transform(crs = "EPSG:3338"),
    shp_bss$bathymetry %>% 
      dplyr::mutate(SRVY = "BSS") %>%
      sf::st_transform(crs = "EPSG:3338"),
    shp_goa$bathymetry %>% 
      dplyr::mutate(SRVY = "GOA") %>%
      sf::st_transform(crs = "EPSG:3338"),
    shp_ai$bathymetry %>% 
      dplyr::mutate(SRVY = "AI") %>%
      sf::st_transform(crs = "EPSG:3338")
  )) %>% 
    dplyr::select(geometry, SRVY, meters = METERS),
  
  # place labels
  place.labels = dplyr::bind_rows(list(
    data.frame(type = "bathymetry", 
               lab = c("50 m", "100 m", "200 m"), 
               x = c(-168.122, -172.736, -174.714527), 
               y = c(58.527, 58.2857, 58.504532), 
               SRVY = "BS") %>%
      sf::st_as_sf(coords = c("x", "y"), 
                   remove = FALSE,  
                   crs = "+proj=longlat") %>%
      sf::st_transform(crs = "EPSG:3338"), 
    data.frame(type = "bathymetry", 
               lab = c("50 m", "100 m", "200 m"), 
               x = c(-168, -172.5, -174.714527), 
               y = c(58.527, 58.2857, 58.504532), 
               SRVY = "EBS") %>%
      sf::st_as_sf(coords = c("x", "y"), 
                   remove = FALSE,  
                   crs = "+proj=longlat") %>%
      sf::st_transform(crs = "EPSG:3338")
  )) )
