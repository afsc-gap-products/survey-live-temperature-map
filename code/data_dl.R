#' ---------------------------------------------
#' title: Create public data 
#' author: EH Markowitz
#' start date: 2022-01-01
#' Notes: 
#' ---------------------------------------------

# Dowload oracle data ----------------------------------------------------------

# This has a specific username and password because I DONT want people to have access to this!
source("https://raw.githubusercontent.com/afsc-gap-products/metadata/main/code/functions_oracle.R")
locations <- c("Z:/Projects/ConnectToOracle.R", 
               "C:/Users/emily.markowitz/Documents/Projects/ConnectToOracle.R")
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

locations <- c(
  "RACEBASE_FOSS.JOIN_FOSS_CPUE_HAUL", 
  "RACE_DATA.V_CRUISES", 
  "GOA.GOA_STRATA"
)

oracle_dl(
  locations = locations, 
  channel = channel, 
  dir_out = paste0(here::here("data"), "/"))


# Shapefiles -------------------------------------------------------------------

surveys <- 
  data.frame(survey_definition_id = c(143, 98, 47, 52, 78), 
             SRVY = c("NBS", "EBS", "GOA", "AI", "BSS"), 
             SRVY_long = c("northern Bering Sea", 
                           "eastern Bering Sea", 
                           "Gulf of Alaska", 
                           "Aleutian Islands", 
                           "Bering Sea Slope") )

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
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(SRVY = "BSS")
shp_bss <- survey_area

## combine  ------------------------------------------------------------------------

### survey area  ------------------------------------------------------------------------

shp_surv <- dplyr::bind_rows(list(
  shp_ebs$survey.area %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    dplyr::mutate(SRVY = "EBS"), 
  # shp_bs$survey.area %>% 
  #   sf::st_transform(crs = "EPSG:3338") %>% 
  #   dplyr::mutate(SRVY = "BS"), 
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
  dplyr::select(SRVY, geometry) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  # st_transform(crs = "+proj=longlat +ellps=WGS84 +pm=-360 +datum=WGS84 +no_defs")
  dplyr::left_join(x = ., 
                   y = surveys %>% 
                     dplyr::mutate(survey = stringr::str_to_title(SRVY_long), 
                                   survey_long = paste0(survey, " Bottom Trawl Survey")), 
                   by = "SRVY")

### survey station points  ------------------------------------------------------------------------

shp_stn <- dplyr::bind_rows(list(
  shp_ebs$survey.grid %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    sf::st_centroid() %>%
    dplyr::mutate(SRVY = "EBS") %>% 
    dplyr::select(SRVY, station = STATIONID, geometry), 
  shp_nbs$survey.grid  %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    sf::st_centroid() %>%
    dplyr::mutate(SRVY = "NBS") %>% 
    dplyr::select(SRVY, station = STATIONID, geometry), 
  shp_ai$survey.grid %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    sf::st_centroid() %>%
    dplyr::mutate(SRVY = "AI") %>% 
    dplyr::select(SRVY, station, stratum, region, geometry), 
  shp_goa$survey.grid %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    sf::st_centroid() %>%
    dplyr::mutate(SRVY = "GOA") %>% 
    dplyr::select(SRVY, station, stratum, region, geometry)#, 
  # shp_bss$survey.grid %>% 
  #   sf::st_transform(crs = "EPSG:3338") %>% 
  #   sf::st_centroid() %>% 
  #   dplyr::mutate(SRVY = "BSS"))
)) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  # st_transform(crs = "+proj=longlat +ellps=WGS84 +pm=-360 +datum=WGS84 +no_defs")
  dplyr::left_join(x = ., 
                   y = surveys %>% 
                     dplyr::mutate(survey = stringr::str_to_title(SRVY_long), 
                                   survey_long = paste0(survey, " Bottom Trawl Survey")), 
                   by = "SRVY")

shp_stn <- shp_stn %>% 
  dplyr::bind_cols(shp_stn %>% 
                     st_coordinates() %>% 
                     data.frame()) %>% 
  dplyr::rename(lon = X, 
                lat = Y) 

### survey stratum areas ------------------------------------------------------------------------

shp_strat <- dplyr::bind_rows(list(
  shp_bs$survey.strata %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    dplyr::mutate(SRVY = "NEBS") %>% 
    dplyr::rename(STRATUM = Stratum) %>% 
    dplyr::mutate(STRATUM = as.character(STRATUM)), 
  shp_ebs$survey.strata %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    dplyr::mutate(SRVY = "EBS") %>% 
    dplyr::rename(STRATUM = Stratum) %>% 
    dplyr::mutate(STRATUM = as.character(STRATUM)), 
  shp_nbs$survey.strata  %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    dplyr::mutate(SRVY = "NBS") %>% 
    dplyr::rename(STRATUM = Stratum) %>% 
    dplyr::mutate(STRATUM = as.character(STRATUM)), 
  shp_ai$survey.strata %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    dplyr::mutate(SRVY = "AI") %>% 
    dplyr::mutate(STRATUM = as.character(STRATUM)), 
  shp_goa$survey.strata %>% 
    sf::st_transform(crs = "EPSG:3338") %>% 
    dplyr::mutate(SRVY = "GOA") %>% 
    dplyr::mutate(STRATUM = as.character(STRATUM)), 
  shp_bss$survey.strata %>%
    sf::st_transform(crs = "EPSG:3338") %>%
    dplyr::mutate(SRVY = "BSS") %>% 
    dplyr::mutate(STRATUM = as.character(STRATUM)))
)  %>% 
  dplyr::select(SRVY, stratum = STRATUM, geometry) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  # st_transform(crs = "+proj=longlat +ellps=WGS84 +pm=-360 +datum=WGS84 +no_defs")
  dplyr::left_join(x = ., 
                   y = surveys %>% 
                     dplyr::mutate(survey = stringr::str_to_title(SRVY_long), 
                                   survey_long = paste0(survey, " Bottom Trawl Survey")), 
                   by = "SRVY")

save(shp_surv, shp_stn, shp_strat, file = here::here("data", "shp.rdata"))
