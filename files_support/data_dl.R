#' ---------------------------------------------
#' title: Create public data 
#' author: EH Markowitz
#' start date: 2022-01-01
#' Notes: 
#' ---------------------------------------------


# Shapefiles -------------------------------------------------------------------

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


#### NEW AND IMPROVDED, BUT NOT YET INTEGRATED ---------------------------------

# Load shape files -------------------------------------------------------------



# Download oracle data ----------------------------------------------------------

# Connect to oracle ------------------------------------------------------------

PKG <- c("magrittr", "readr", "dplyr")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p, verbose = FALSE)
    require(p,character.only = TRUE)}
}

if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
  channel <- channel_products
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

# locations <- c(
#   "RACEBASE_FOSS.JOIN_FOSS_CPUE_HAUL", 
#   "RACE_DATA.V_CRUISES", 
#   "GOA.GOA_STRATA"
# )

locations<-c(
  # "GAP_PRODUCTS.AKFIN_CRUISE",
  # "GAP_PRODUCTS.AKFIN_HAUL",
  "GAP_PRODUCTS.AKFIN_AREA",
  "GAP_PRODUCTS.AKFIN_STRATUM_GROUPS"#, 
  # "RACE_DATA.CRUISES" # needed for survey start and end dates
  
  
  # "AI.AIGRID_GIS",
  # "GOA.GOA_STRATA",
  # # "RACE_DATA.VESSELS", 
  # "RACE_DATA.V_CRUISES",
  # # "RACEBASE.HAUL", 
  # # "RACE_DATA.V_CRUISES"
  # "RACEBASE_FOSS.JOIN_FOSS_CPUE_HAUL"
)

error_loading <- c()
for (i in 1:length(locations)){
  print(locations[i])
  
  a <- RODBC::sqlQuery(channel = channel, 
                       query = paste0("SELECT *
    FROM ", locations[i], "
    FETCH FIRST 1 ROWS ONLY;"))
  
  end0 <- c()
  
  start0 <- ifelse(!("START_TIME" %in% names(a)), 
                   "*", 
                   paste0(paste0(names(a)[names(a) != "START_TIME"], sep = ",", collapse = " "),
                          " TO_CHAR(START_TIME,'MM/DD/YYYY HH24:MI:SS') START_TIME "))
  
  a <- RODBC::sqlQuery(channel = channel, 
                       query = paste0("SELECT ", start0, " FROM ", locations[i], end0, "; "))
  
  if (is.null(nrow(a))) { # if (sum(grepl(pattern = "SQLExecDirect ", x = a))>1) {
    error_loading <- c(error_loading, locations[i])
  } else {
    write.csv(x = a, 
              here::here("data",
                         paste0(tolower(gsub(pattern = '.', 
                                             replacement = "_", 
                                             x = locations[i], 
                                             fixed = TRUE)),
                                ".csv")))
  }
  remove(a)
}
error_loading



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

save(areas, file = here::here("data", "shp1.rdata"))



