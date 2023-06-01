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

a<-list.files(path = paste0(dir_wd, "data/"))
for (i in 1:length(a)){
  b <- read_csv(file = paste0(dir_wd, "data/", a[i]))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
}

# The surveys this script will be covering -------------------------------------

# need race_data_v_cruises because it has the cruise start and end dates

dat_survreg <- 
  dplyr::right_join( # get SRVY
    x = racebase_foss_join_foss_cpue_haul0 %>% 
      dplyr::select(SRVY = srvy, survey_definition_id, year, vessel_id) %>% 
      dplyr::distinct(),
    y = race_data_v_cruises0 %>% 
      dplyr::select(year, vessel_id, start_date, end_date, survey_definition_id, vessel_name) %>% 
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
  dplyr::select(year, SRVY, reg_dates, vessel_id, vessel_shape, vessel_name, vessel_ital, region_long) %>% 
  dplyr::distinct()


# > head(dat_survreg)
# # A tibble: 6 × 6
# year SRVY  reg_dates       vessel_id vessel_shape region_long       
# <dbl> <chr> <chr>               <dbl> <chr>        <chr>             
#   1  1982 EBS   May 29 - Aug 23        19 P            Eastern Bering Sea
# 2  1982 EBS   May 29 - Aug 23         1 C            Eastern Bering Sea
# 3  1982 EBS   May 29 - Aug 23        19 P            Eastern Bering Sea

# haul data --------------------------------------------------------------------

haul <- racebase_foss_join_foss_cpue_haul0 %>% 
  dplyr::rename(SRVY = srvy) %>%
  dplyr::filter(
    !(is.na(station)) &
      !is.na(surface_temperature_c) &
      !is.na(bottom_temperature_c) & 
      # there shouldn't be bottom temps of 0 in the AI or GOA
      ((SRVY %in% c("AI", "GOA") & surface_temperature_c != 0) | (SRVY %in% c("EBS", "NBS"))) & 
      ((SRVY %in% c("AI", "GOA") & bottom_temperature_c != 0) | (SRVY %in% c("EBS", "NBS")))) %>% 
  dplyr::mutate(date = as.Date(date_time)) %>% 
  dplyr::select(-vessel_name) %>%
  dplyr::left_join( # get vessel_shape
    x = ., 
    y = dat_survreg, 
    by = c("SRVY", "year", "vessel_id")) %>% 
  dplyr::select(
    SRVY, year, stratum, station, date, region_long, reg_dates, 
    vessel_shape, vessel_name, vessel_ital, 
    st = surface_temperature_c, 
    bt = bottom_temperature_c, 
  ) %>% 
  dplyr::arrange(-year)

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