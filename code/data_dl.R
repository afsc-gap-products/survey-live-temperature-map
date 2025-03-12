

# Dowload FOSS data ------------------------------------------------------------

# adatapted from https://afsc-gap-products.github.io/gap_products/content/foss-api-r.html#haul-data
# September 26, 2024 by Emily Markowitz

library(httr)
library(jsonlite)
print("FOSS haul data")

dat <- data.frame()
for (i in seq(0, 500000, 10000)){
  print(i)
  ## query the API link
  res <- httr::GET(url = paste0('https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_haul/',
                                "?offset=",i,"&limit=10000"))
  ## convert from JSON format
  data <- jsonlite::fromJSON(base::rawToChar(res$content))
  
  ## if there are no data, stop the loop
  if (is.null(nrow(data$items))) {
    break
  }
  
  ## bind sub-pull to dat data.frame
  dat <- dplyr::bind_rows(dat,
                          data$items %>%
                            dplyr::select(-links)) # necessary for API accounting, but not part of the dataset)
}
foss_haul <- dat %>%
  dplyr::mutate(date_time = as.POSIXct(date_time,
                                       format = "%Y-%m-%dT%H:%M:%S",
                                       tz = Sys.timezone()), 
                data_type = "offical", 
                date = format(x = min(date_time, na.rm = TRUE), format = "%Y-%m-%d"), 
                vessel_shape = ifelse(is.na(vessel_name), NA, as.character(substr(x = vessel_name, start = 1, stop = 1))), 
                vessel_ital = ifelse(is.na(vessel_name), NA, paste0("F/V *", stringr::str_to_title(vessel_name), "*")), 
                vessel_name = ifelse(is.na(vessel_name), NA, paste0("F/V ", stringr::str_to_title(vessel_name)))) %>% 
  dplyr::select(year, SRVY = srvy, survey, survey_definition_id, cruise, cruisejoin, hauljoin, 
                # date_start, date_end, survey_dates, 
                station, stratum, date_time_start = date_time, 
                latitude_dd_start, longitude_dd_start,
                st = surface_temperature_c, bt = bottom_temperature_c, 
                vessel_id, vessel_name, 
  data_type, vessel_shape, vessel_ital, date)

foss_haul <- foss_haul %>% 
  dplyr::left_join(
    foss_haul %>% 
      dplyr::group_by(survey_definition_id, year) %>% 
      dplyr::summarise(date_start = format(x = min(date_time_start, na.rm = TRUE), format = "%B %d"), 
                       date_end = format(x = max(date_time_start, na.rm = TRUE), format = "%B %d"), 
                       survey_dates = paste0(date_start, " - ", date_end))    
  )

save(foss_haul, file = here::here("data", "foss_haul.rdata"))

# Download oracle data ----------------------------------------------------------

PKG <- c("magrittr", "readr", "dplyr")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p, verbose = FALSE)
    require(p,character.only = TRUE)}
}

### Connect to oracle
if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
  channel <- channel
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

locations<-c(
  # "GAP_PRODUCTS.AKFIN_CRUISE",
  # "GAP_PRODUCTS.AKFIN_HAUL",
  "GAP_PRODUCTS.AKFIN_AREA",
  "GAP_PRODUCTS.AKFIN_STRATUM_GROUPS", 
  "RACE_DATA.CRUISES" # needed for survey start and end dates
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
    b <- paste0(tolower(gsub(pattern = '.', 
                             replacement = "_", 
                             x = locations[i], 
                             fixed = TRUE)),
                ".csv")
    write.csv(x = a, 
              here::here("data", b))
    
    names(a) <- tolower(names(a))
    assign(x = gsub(x = paste0(b, "0"),
      pattern = ifelse(grepl(pattern = ".csv", x = b, fixed = TRUE), ".csv", ".xlsx"), 
      replacement = ""), 
      value = a)
  }
  remove(a)
}
error_loading

# Load shape files -------------------------------------------------------------

shp_bs <- akgfmaps::get_base_layers(select.region = "bs.all", set.crs = "auto")
shp_ebs <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
shp_nbs <- akgfmaps::get_base_layers(select.region = "bs.north", set.crs = "auto")
shp_ai <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto")
shp_goa <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
shp_bss <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "auto")

aa <- gap_products_akfin_area0 %>% 
  dplyr::filter(design_year <= maxyr) %>% 
  dplyr::group_by(survey_definition_id) %>% 
  dplyr::summarise(design_year = max(design_year, na.rm = TRUE)) 

areas <- gap_products_akfin_area0  %>% 
  # find the most up to date design_year's
  dplyr::filter(eval(parse(text=paste0("(survey_definition_id == ", aa$survey_definition_id, 
                                       " & ", "design_year == ", aa$design_year, ") ", collapse = " | ")))) %>%
  dplyr::filter(area_type %in% c("STRATUM", "INPFC")) %>% 
  dplyr::mutate(
    area_name = dplyr::case_when(
      area_name %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians",
      TRUE ~ area_name)
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
    shp_bs$survey.strata,
    shp_ebs$survey.strata,
    shp_nbs$survey.strata,
    shp_ai$survey.strata,
    shp_goa$survey.strata,
    shp_bss$survey.strata)) %>%
    dplyr::mutate(SRVY = dplyr::case_when(
      SURVEY_DEFINITION_ID == 98 ~ "EBS", 
      SURVEY_DEFINITION_ID == 143 ~ "NBS", 
      SURVEY_DEFINITION_ID == 78 ~ "BSS", 
      SURVEY_DEFINITION_ID == 52 ~ "AI", 
      SURVEY_DEFINITION_ID == 47 ~ "GOA" 
    )) %>% 
    janitor::clean_names() %>%
    dplyr::mutate(area_id = stratum) %>% 
    dplyr::left_join(y = areas, relationship = "many-to-many"), 
  
  # Regions
  survey.area = dplyr::bind_rows(list(
    shp_bs$survey.area,
    shp_ebs$survey.area,
    shp_nbs$survey.area,
    shp_ai$survey.area,
    shp_goa$survey.area,
    shp_bss$survey.area)) %>%
    janitor::clean_names()  %>%
    dplyr::mutate(SRVY = dplyr::case_when(
      survey_definition_id == 98 ~ "EBS", 
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 78 ~ "BSS", 
      survey_definition_id == 52 ~ "AI", 
      survey_definition_id == 47 ~ "GOA" 
    )),
  
  # Stations
  survey.grid = dplyr::bind_rows(list(
    shp_bs$survey.grid, 
    shp_ebs$survey.grid,
    shp_nbs$survey.grid,
    shp_ai$survey.grid,
    shp_goa$survey.grid)
  ) %>%
    janitor::clean_names()  %>%
    dplyr::mutate(SRVY = dplyr::case_when(
      survey_definition_id == 98 ~ "EBS", 
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 78 ~ "BSS", 
      survey_definition_id == 52 ~ "AI", 
      survey_definition_id == 47 ~ "GOA" 
    )), 
  
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
      dplyr::mutate(SRVY = "BS"),
    shp_ebs$bathymetry %>% 
      dplyr::mutate(SRVY = "EBS"),
    shp_nbs$bathymetry %>% 
      dplyr::mutate(SRVY = "NBS"),
    shp_bss$bathymetry %>% 
      dplyr::mutate(SRVY = "BSS"),
    shp_goa$bathymetry %>% 
      dplyr::mutate(SRVY = "GOA"),
    shp_ai$bathymetry %>% 
      dplyr::mutate(SRVY = "AI")
  ))  %>%
    janitor::clean_names() %>% 
    dplyr::select(geometry, SRVY = srvy, meters) %>%
    dplyr::mutate(survey_definition_id = dplyr::case_when(
      SRVY == "EBS" ~ 98, 
      SRVY == "NBS" ~ 143, 
      SRVY == "BSS" ~ 78, 
      SRVY == "AI" ~ 52, 
      SRVY == "GOA" ~ 47 
    )),
  
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


save(shp_all, file = here::here("data", "shp_all.rdata"))

