

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
                          data$items |>
                            dplyr::select(-links)) # necessary for API accounting, but not part of the dataset)
}
dat_foss <- dat |>
  dplyr::mutate(date_time = as.POSIXct(date_time,
                                       format = "%Y-%m-%dT%H:%M:%S",
                                       tz = Sys.timezone()), 
                source = "offical", 
                date = format(x = min(date_time, na.rm = TRUE), format = "%Y-%m-%d") # , 
                # vessel_shape = ifelse(is.na(vessel_name), NA, as.character(substr(x = vessel_name, start = 1, stop = 1))), 
                # vessel_ital = ifelse(is.na(vessel_name), NA, paste0("F/V *", stringr::str_to_title(vessel_name), "*")), 
                # vessel_name = ifelse(is.na(vessel_name), NA, paste0("F/V ", stringr::str_to_title(vessel_name)))
  ) |> 
  dplyr::select(year, srvy = srvy, survey, survey_definition_id, cruise, cruisejoin, hauljoin, 
                # date_start, date_end, survey_dates, 
                station, stratum, date_time_start = date_time, 
                latitude_dd_start, longitude_dd_start,
                st = surface_temperature_c, bt = bottom_temperature_c, 
                vessel_id, vessel_name, #vessel_shape, vessel_ital, 
                source, date)

dat_foss <- dat_foss |> 
  dplyr::left_join(
    dat_foss |> 
      dplyr::group_by(survey_definition_id, year) |> 
      dplyr::summarise(date_start = format(x = min(date_time_start, na.rm = TRUE), format = "%B %d"), 
                       date_end = format(x = max(date_time_start, na.rm = TRUE), format = "%B %d"), 
                       survey_dates = paste0(date_start, " - ", date_end))    
  )

# save(dat_foss, file = here::here("data", "dat_foss.rdata"))
write.csv(x = dat_foss, file = here::here("data", "dat_foss.csv"))

# Download oracle data ----------------------------------------------------------

PKG <- c("readr", "dplyr")

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
  "GAP_PRODUCTS.AKFIN_STRATUM_GROUPS"#, 
  # "RACE_DATA.CRUISES" # needed for survey start and end dates
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


race_data_cruises0 <- 
  RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.CRUISES;")  |> 
  dplyr::left_join(
    RODBC::sqlQuery(channel, "SELECT SURVEY_ID, SURVEY_DEFINITION_ID FROM RACE_DATA.SURVEYS;") )  |> 
  dplyr::left_join(
    RODBC::sqlQuery(channel, "SELECT VESSEL_ID, NAME AS VESSEL_NAME FROM RACE_DATA.VESSELS;") )  |> 
  janitor::clean_names()

write.csv(x = race_data_cruises0, file = here::here("data", "race_data_cruises_mod.csv"))

# Load shape files -------------------------------------------------------------

shp_bs <- akgfmaps::get_base_layers(select.region = "bs.all", set.crs = "auto")
shp_ebs <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
shp_nbs <- akgfmaps::get_base_layers(select.region = "bs.north", set.crs = "auto")
shp_ai <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto")
shp_goa <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
shp_bss <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "auto")

aa <- gap_products_akfin_area0 |> 
  dplyr::filter(design_year <= maxyr) |> 
  dplyr::group_by(survey_definition_id) |> 
  dplyr::summarise(design_year = max(design_year, na.rm = TRUE)) 

areas <- gap_products_akfin_area0  |> 
  # find the most up to date design_year's
  dplyr::filter(eval(parse(text=paste0("(survey_definition_id == ", aa$survey_definition_id, 
                                       " & ", "design_year == ", aa$design_year, ") ", collapse = " | ")))) |>
  dplyr::filter(area_type %in% c("STRATUM", "INPFC")) |> 
  dplyr::mutate(
    area_name = dplyr::case_when(
      area_name %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians",
      TRUE ~ area_name)
  ) |> 
  dplyr::select(survey_definition_id, area_id, area_type, area_name) 

# areas <- dplyr::bind_rows(
#   areas |> 
#     dplyr::mutate(stratum = area_id) |>
#     dplyr::filter(!(survey_definition_id %in% c(47, 52)) & 
#                     area_type == "STRATUM"), 
#   gap_products_akfin_stratum_groups0 |> 
#     dplyr::filter(eval(parse(text=paste0("(survey_definition_id == ", aa$survey_definition_id, 
#                                          " & ", "design_year == ", aa$design_year, ") ", collapse = " | ")))) |>
#     dplyr::filter(survey_definition_id %in% c(52, 47)) |> 
#     dplyr::filter(area_id %in% unique(areas$area_id[areas$area_type == "INPFC"])) |>
#     dplyr::select(-design_year) |> 
#     dplyr::left_join(areas |> 
#                        dplyr::filter(area_type == "INPFC") ))  

shp_all <- shp <- list(
  # Stratum
  survey.strata = dplyr::bind_rows(list(
    # shp_bs$survey.strata,
    shp_ebs$survey.strata |> 
      dplyr::left_join(areas |> 
                        dplyr::mutate(stratum = area_id) |>
                        dplyr::filter(survey_definition_id == 98 & area_type == "STRATUM") |> 
      dplyr::rename_all(toupper) ),
    shp_nbs$survey.strata |> 
      dplyr::left_join(areas |> 
                         dplyr::mutate(stratum = area_id) |>
                         dplyr::filter(survey_definition_id == 143 & area_type == "STRATUM") |> 
                         dplyr::rename_all(toupper) ),
    # ggplot(data = aaa, mapping = aes(fill = area_name, label = STRATUM)) + geom_sf(color = "black") + geom_sf_label()
    shp_ai$survey.strata |> # require(sf); shape <- read_sf(dsn = here::here("data", "aigrid_trawable_thru2018_Emily.shp"))
      dplyr::mutate(# 322
        AREA_NAME = dplyr::case_when(
          STRATUM %in% c(212, 214, 211, 213, 223, 223, 224, 221, 222) ~ "Western Aleutians",
          STRATUM %in% c(424, 422, 421, 414, 314, 313, 423, 324, 413, 312, 311, 323, 322, 321, 412, 411) ~ "Central Aleutians",
          STRATUM %in% c(513, 614, 621, 511, 612, 611, 522,
                         624, 512, 622, 613, 521, 623, 523, 594) ~ "Eastern Aleutians",
          STRATUM %in% c(712, 794, 721, 722, 793, 711) ~ "South Bering Sea"
        )),
    shp_goa$survey.strata |> 
      dplyr::mutate(
        AREA_NAME = dplyr::case_when(
          STRATUM %in% c(211, 15, 14, 113, 511) ~ "Shumagin",
          STRATUM %in% c(123, 24, 521, 222, 23, 321) ~ "Chirikof",
          STRATUM %in% c(36, 136, 105, 37, 38, 531, 135) ~ "Kodiak",
          STRATUM %in% c(43, 541, 242, 42, 144, 145) ~ "Yakutat",
          STRATUM %in% c(51, 551, 152, 352, 253, 252) ~ "Southeastern"
        )),
    shp_bss$survey.strata |> 
      dplyr::left_join(areas |> 
                         dplyr::mutate(stratum = area_id) |>
                         dplyr::filter(survey_definition_id == 78 & area_type == "STRATUM") |> 
                         dplyr::rename_all(toupper)) )) |>
    janitor::clean_names() |>
    # dplyr::left_join(areas) |> 
    dplyr::mutate(srvy = dplyr::case_when(
      survey_definition_id == 98 ~ "EBS", 
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 78 ~ "BSS", 
      survey_definition_id == 52 ~ "AI", 
      survey_definition_id == 47 ~ "GOA" 
    )) |>     
    dplyr::mutate(area_id = stratum), #|> 
    # dplyr::left_join(y = areas, relationship = "many-to-many"), 
  
  # Regions
  survey.area = dplyr::bind_rows(list(
    # shp_bs$survey.area,
    shp_ebs$survey.area,
    shp_nbs$survey.area,
    shp_ai$survey.area,
    shp_goa$survey.area,
    shp_bss$survey.area)) |>
    janitor::clean_names()  |>
    dplyr::mutate(srvy = dplyr::case_when(
      survey_definition_id == 98 ~ "EBS", 
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 78 ~ "BSS", 
      survey_definition_id == 52 ~ "AI", 
      survey_definition_id == 47 ~ "GOA" 
    )),
  
  # Stations
  survey.grid = dplyr::bind_rows(list(
    # shp_bs$survey.grid, 
    shp_ebs$survey.grid |> 
      dplyr::mutate(survey_definition_id = 98),
    shp_nbs$survey.grid |> 
      dplyr::mutate(survey_definition_id = 143),
    shp_ai$survey.grid |> 
      dplyr::mutate(survey_definition_id = 52),
    shp_goa$survey.grid |> 
      dplyr::mutate(survey_definition_id = 47)
  )) |>
    janitor::clean_names()  |>
    dplyr::mutate(srvy = dplyr::case_when(
      survey_definition_id == 98 ~ "EBS", 
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 78 ~ "BSS", 
      survey_definition_id == 52 ~ "AI", 
      survey_definition_id == 47 ~ "GOA" 
    )) |> 
    dplyr::select(-survey_definition_id_2), 
  
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
    shp_bs$bathymetry |> 
      dplyr::mutate(srvy = "BS"),
    shp_ebs$bathymetry |> 
      dplyr::mutate(srvy = "EBS"),
    shp_nbs$bathymetry |> 
      dplyr::mutate(srvy = "NBS"),
    shp_bss$bathymetry |> 
      dplyr::mutate(srvy = "BSS"),
    shp_goa$bathymetry |> 
      dplyr::mutate(srvy = "GOA"),
    shp_ai$bathymetry |> 
      dplyr::mutate(srvy = "AI")
  ))  |>
    janitor::clean_names() |> 
    dplyr::select(geometry, srvy = srvy, meters) |>
    dplyr::mutate(survey_definition_id = dplyr::case_when(
      srvy == "EBS" ~ 98, 
      srvy == "NBS" ~ 143, 
      srvy == "BSS" ~ 78, 
      srvy == "AI" ~ 52, 
      srvy == "GOA" ~ 47 
    )),
  
  # place labels
  place.labels = dplyr::bind_rows(list(
    data.frame(type = "bathymetry", 
               lab = c("50 m", "100 m", "200 m"), 
               x = c(-168.122, -172.736, -174.714527), 
               y = c(58.527, 58.2857, 58.504532), 
               srvy = "BS") |>
      sf::st_as_sf(coords = c("x", "y"), 
                   remove = FALSE,  
                   crs = "+proj=longlat") |>
      sf::st_transform(crs = "EPSG:3338"), 
    data.frame(type = "bathymetry", 
               lab = c("50 m", "100 m", "200 m"), 
               x = c(-168, -172.5, -174.714527), 
               y = c(58.527, 58.2857, 58.504532), 
               srvy = "EBS") |>
      sf::st_as_sf(coords = c("x", "y"), 
                   remove = FALSE,  
                   crs = "+proj=longlat") |>
      sf::st_transform(crs = "EPSG:3338")
  )) )

# GOA
aaa <- dplyr::left_join(
  shp_all$survey.grid[shp_all$survey.grid$srvy == "GOA",], 
  sf::st_join(shp_all$survey.grid[shp_all$survey.grid$srvy == "GOA",] |> 
                sf::st_centroid(), 
              shp_all$survey.strata[shp_all$survey.strata$srvy == "GOA",] |> 
                dplyr::select(area_name, stratum)) |> 
    dplyr::select(area_name, stratum, grid_id, station) |> 
    sf::st_drop_geometry())
shp_all$survey.grid <- shp_all$survey.grid[shp_all$survey.grid$srvy != "GOA",]
shp_all$survey.grid <- dplyr::bind_rows(aaa, shp_all$survey.grid)

# AI
aaa <- dplyr::left_join(
  shp_all$survey.grid[shp_all$survey.grid$srvy == "AI",] |> 
    dplyr::select(-stratum, -area_name), 
  sf::st_join(shp_all$survey.grid[shp_all$survey.grid$srvy == "AI",] |> 
                sf::st_centroid() |> 
                dplyr::select(-stratum, -area_name), 
              shp_all$survey.strata[shp_all$survey.strata$srvy == "AI",] |> 
                dplyr::select(area_name, stratum)) |> 
    dplyr::select(area_name, stratum, grid_id, station) |> 
    sf::st_drop_geometry())
shp_all$survey.grid <- shp_all$survey.grid[shp_all$survey.grid$srvy != "AI",]
shp_all$survey.grid <- dplyr::bind_rows(aaa, shp_all$survey.grid)

# EBS + NBS
aaa <- dplyr::left_join(
  shp_all$survey.grid[shp_all$survey.grid$srvy %in% c("NBS", "EBS", "BSS"),] |> 
    dplyr::select(-stratum, -area_name), 
  dplyr::full_join(
  areas |> 
    dplyr::mutate(stratum = area_id) |>
    dplyr::filter(survey_definition_id %in% c(78, 143, 98) & area_type == "STRATUM") , #|>
dat_foss |> 
    dplyr::select(station, stratum, srvy, survey_definition_id) |>
    dplyr::distinct() |>
    dplyr::filter(srvy %in% c("NBS", "EBS")) ) )
shp_all$survey.grid <- shp_all$survey.grid[!(shp_all$survey.grid$srvy %in% c("NBS", "EBS")),]
shp_all$survey.grid <- dplyr::bind_rows(aaa, shp_all$survey.grid)

# ggplot(data = aaa, # |> dplyr::filter(is.na(area_name)),
#        mapping = aes(fill = area_name, label = stratum)) +
#   geom_sf(color = "black") #+ geom_sf_label()

save(shp_all, file = here::here("data", "shp_all.rdata"))

