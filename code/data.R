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

# The surveys this script will be covering -------------------------------------

dat_survreg <- readxl::read_excel(
  path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"), 
  sheet = "runs") %>% 
  dplyr::filter(year == maxyr) %>%
  dplyr::mutate(
    region_long = dplyr::case_when(
      SRVY == "EBS" ~ "Eastern Bering Sea", 
      SRVY == "NBS" ~ "Northern Bering Sea", 
      SRVY == "GOA" ~ "Gulf of Alaska", 
      SRVY == "AI" ~ "Aleutian Islands"), 
    region = dplyr::case_when(
      SRVY %in% c("EBS", "NBS") ~ "BS", 
      SRVY == "GOA" ~ "GOA", 
      SRVY == "AI" ~ "AI"), 
    )

# Load Design Based Estimates --------------------------------------------------

load(here::here("data", "publicdata", "all_data.Rdata"))
lastdl <- ageoffile(here::here("data", "publicdata", "all_data.Rdata"))

# Download data from oracle saved locally: -------------------------------------

a<-list.files(path = paste0(dir_wd, "data/oracle/"))
for (i in 1:length(a)){
  b <- read_csv(file = paste0(dir_wd, "data/oracle/", a[i]))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
}

# haul data --------------------------------------------------------------------
# haul <- dplyr::left_join(
#   x = racebase_haul0, 
#   y = race_data_v_cruises0 %>% 
#     dplyr::select(cruise_id,  year, survey_name, vessel_id, survey_definition_id, 
#                   vessel_name, start_date, end_date, cruisejoin), 
#   by = "cruisejoin") %>% 
#   dplyr::mutate(#year = substr(x = cruise, start = 1, stop = 4),  
#     bottom_type = NULL, 
#     SRVY = dplyr::case_when(
#       survey_definition_id == 143 ~"NBS", 
#       survey_definition_id == 98 ~ "EBS", 
#       survey_definition_id == 47 ~ "GOA", 
#       survey_definition_id == 52 ~ "AI", 
#       survey_definition_id == 78 ~"BSS")  ) %>%
#   dplyr::filter(#year <= maxyr &
#     year != 2020 & # no surveys happened this year because of COVID
#       ((year >= 1982 & SRVY == "EBS") | # 1982 BS inclusive - much more standardized after this year
#          (year >= 2010 & SRVY == "NBS") | # 1982 BS inclusive - much more standardized after this year
#          (year != 2018 & SRVY == "NBS") | # 2018 NBS was not a standard haul
#          SRVY == "BSS" | # keep all years of the BSS
#          (year >= 1991 & SRVY %in% c("AI", "GOA")))) %>% # 1991 AI and GOA (1993) inclusive - much more standardized after this year
#   # !is.na(SRVY) & 
#   dplyr::filter(#SRVY %in% unique(dat_survreg$SRVY) &
#     haul_type == 3 &
#       abundance_haul == "Y" &
#       performance >= 0 &
#       !(is.null(stationid)))
haul <- racebase_foss_join_foss_cpue_haul0 %>% 
  dplyr::rename(SRVY = srvy) %>%
  dplyr::mutate(date = as.Date(date_time))
  
# vessel -----------------------------------------------------------------------

vessel_info <- haul %>% 
  dplyr::select(vessel_name, vessel_id) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(vessel_ital = paste0("F/V *", stringr::str_to_title(vessel_name), "*")) %>%
  dplyr::mutate(vessel_name = paste0("F/V ", stringr::str_to_title(vessel_name)))

# Load shape files -------------------------------------------------------------

## EBS + NBS  ------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "bs.all", set.crs = "auto")
survey_area$survey.grid <- survey_area$survey.grid %>% 
  sf::st_transform(x = ., survey_area$crs$input) %>%
  dplyr::rename(station = STATIONID) %>%
  sp::merge(x = ., 
            y = haul %>%
              # dplyr::rename(station = stationid) %>% 
              dplyr::select(station, stratum) %>% 
              dplyr::distinct(), 
            all.x = TRUE) %>% 
  dplyr::mutate(region = "Bering Sea")
survey_area$place.labels$y[survey_area$place.labels$lab == "200 m"] <- -60032.7
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(SRVY = ifelse(SURVEY == "EBS_SHELF", "EBS", "NBS"))
shp_bs <- survey_area

## EBS  ------------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
survey_area$survey.area <- shp_bs$survey.area %>% 
  dplyr::filter(SRVY == "EBS")
shp_ebs <- survey_area

## NBS  ------------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "bs.north", set.crs = "auto")
survey_area$survey.area <- shp_bs$survey.area %>% 
  dplyr::filter(SRVY == "NBS")
shp_nbs <- survey_area

## AI  ------------------------------------------------------------------------
# J:\RACE_GF\GOA\GOA 2021\Files for boats\ArcGIS\GOAGRID_2021
# J:\RACE_GF\ALEUTIAN\AI 2022\ArcGIS\Chart Products\aigrid_trawlable_thru2018 - ERRORS,do not use!
# G:\RACE_GF\ALEUTIAN\AI 2022\ArcGIS\Chart Products\aigrid_trawable_thru2018_Emily.shp

# shp_ai <- get_base_layers(select.region = "ai", 
#                           set.crs = "+proj=longlat +datum=WGS84")$survey.strata
survey_area <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto")
survey_area$survey.grid <-  survey_area$survey.grid %>% 
  dplyr::rename(station = ID) %>%
  sp::merge(
    x = .,
    y = goa_goa_strata0 %>%
      dplyr::filter(survey == "AI") %>%
      dplyr::mutate(SRVY = "AI",
                    region = stringr::str_to_title(inpfc_area),
                    region = dplyr::case_when(
                      region %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians",
                      TRUE ~ region)) %>%
      dplyr::select(SRVY, stratum, region) %>%
      dplyr::distinct(),
    all.x = TRUE)  %>% 
  dplyr::arrange(region)
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(SURVEY = "AI", 
                SRVY = "AI")
# survey_area$survey.grid <- rgdal::readOGR(dsn = paste0(dir_wd, '/plot_gridiles/'),# Prepare map objects
#                                           layer = "aigrid_trawable_thru2018_Emily",
#                                           verbose=F) %>%
#   sp::spTransform(x = ., CRS(survey_area$crs$input)) %>%
#   st_as_sf(x = .) %>%
#   dplyr::rename(station = ID,
#                 stratum = STRATUM) %>%
#   dplyr::filter(stratum %in% unique(goa_goa_strata0$stratum) &
#                   stratum != 0) %>% # land
#   sp::merge(
#     x = .,
#     y = goa_goa_strata0 %>%
#       dplyr::filter(survey == "AI") %>%
#       dplyr::mutate(SRVY = "AI",
#                     region = stringr::str_to_title(inpfc_area),
#                     region = dplyr::case_when(
#                       region %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians",
#                       TRUE ~ region)) %>%
#       dplyr::select(SRVY, stratum, region) %>%
#       dplyr::distinct(),
#     all.x = TRUE)  %>% 
#   dplyr::arrange(region)
# survey_area$survey.grid1 <- survey_area$survey.grid
shp_ai <- survey_area

## GOA  ------------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
survey_area$survey.grid <-  survey_area$survey.grid %>% 
  sp::merge(
    x = .,
    y = goa_goa_strata0 %>%
      dplyr::filter(survey == "GOA") %>%
      dplyr::mutate(SRVY = "GOA",
                    region = stringr::str_to_title(inpfc_area)) %>%
      dplyr::select(SRVY, stratum, region) %>%
      dplyr::distinct(),
    all.x = TRUE)  %>% 
  dplyr::arrange(region)
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(SURVEY = "GOA", 
                SRVY = "GOA")
shp_goa <- survey_area

