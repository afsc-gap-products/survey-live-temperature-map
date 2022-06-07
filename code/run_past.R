#' ---------------------------
#' title: Survey Daily and Anomaly Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2022)
#' purpose: run script
#' ---------------------------

# Knowns -----------------------------------------------------------------------
dir_googledrive_upload_test = "https://drive.google.com/drive/folders/1rsR0aFfFzrspTBFU48Bb26EJvdhIZSpl"

# SIGN INTO GOOGLE DRIVE--------------------------------------------------------
## This sign in needs to be here for the Task Scheduler to run, please do not comment out.
googledrive::drive_deauth()
googledrive::drive_auth()
1

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------
## Actually we cant use the here package - it actually causes issues with the tasks scheduler, 
## which has no concept of a project root folder. 
# dir_wd <-"C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
#dir_wd <- "G:/EBSother/GAPsurveyTemperatureMap/"
# dir_wd <-"C:/Users/emily.markowitz/Work/Projects/GAPSurveyTemperatureMap/"
dir_wd <- paste0(getwd(), "/")

source(file = paste0(dir_wd,"code/functions.R"))
# source(file = paste0(dir_wd, "code/data_dl.R")) # you don't unnecessarily run this each time
source(file = paste0(dir_wd, "code/data.R"))

# Map --------------------------------------------------------------------------

## NBS + EBS -------------------------------------------------------------------
maxyr <- 2022 #CHANGE
data_source <- "gd" # google drive
SRVY <- "BS"
plot_subtitle <- "NOAA Fisheries Bering Sea Bottom Trawl Survey"
dir_googledrive_upload <- googledrive::as_id(dir_googledrive_upload_bs)
# dir_googledrive_upload <- googledrive::as_id(dir_googledrive_upload_test)
region_akgfmaps = "bs.all"
survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")

# Daily
var = "bt"
dates0 <- "latest" # "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
show_planned_stations <- TRUE
plot_anom <- FALSE
survey_area$survey.grid <- survey_area$survey.grid %>% 
  sf::st_transform(x = ., survey_area$crs$input) %>%
  dplyr::rename(station = STATIONID) %>%
  sp::merge(x = ., 
            y = haul %>%
              dplyr::rename(station = stationid) %>% 
              dplyr::select(station, stratum) %>% 
              dplyr::distinct(), 
            all.x = TRUE) %>% 
  dplyr::mutate(region = "Bering Sea")
survey_area$place.labels$y[survey_area$place.labels$lab == "200 m"] <- -60032.7

data_source = "oracle"
plot_anom = FALSE
dates0 <- "all"

dat_ebs <- data.frame(reg_shapefile = "EBS_SHELF", 
                          region_long = "Eastern Bering Sea", 
                          SRVY = "EBS", 
                          region = "BS", 
                          vessel_id = c(94, 162), 
                          vessel_shape = c("V", "A"), 
                          reg_dates = "May 25 - Aug 03 2022") 
dat_nbs <- data.frame(reg_shapefile = "NBS_SHELF", 
                                           region_long = "Northern Bering Sea", 
                                           SRVY = "NBS", 
                                           region = "BS", 
                                           vessel_id = c(94, 162), 
                                           vessel_shape = c("V", "A"), 
                                           reg_dates = "Aug 03 - Aug 28 2022") 
dat_survdat <- dplyr::bind_rows(dat_ebs, dat_nbs)


yrs <- list(2021 = c("https://drive.google.com/drive/folders/1q4UN9INXFAyZcIwqy8W9UYfY3G1LuQgW"), 
                     # dat = dat_bs), 
            2019 = c("https://drive.google.com/drive/folders/1S5FyXwWyFUgFkvDlGTC6cymtISyZdd9R"), 
                     # dat = dat_bs), 
            2017 = c("https://drive.google.com/drive/folders/1nAeb9Jq9_FxUc70ZfvXaYZbXrZCQTD4u"))#, 
# dat = dat_bs))


for (i in 1:length(yrs)){
  
  maxyr <- as.numeric(names(yrs)[i])
  dir_googledrive_upload <- googledrive::as_id(yrs[i])
  make_grid_wrapper(maxyr = maxyr,                             # Blank grid plot
                    SRVY = SRVY,
                    haul = haul,
                    dat_survreg = dat_survreg,
                    dir_googledrive_upload = dir_googledrive_upload,
                    survey_area = survey_area,
                    data_source = data_source,
                    plot_subtitle = plot_subtitle,
                    dir_wd = dir_wd)
  make_varplot_wrapper(maxyr = maxyr,                               # Daily plot
                       SRVY = SRVY,
                       haul = haul,
                       dat_survreg = dat_survreg,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0,
                       survey_area = survey_area,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       plot_anom = plot_anom,
                       dir_wd = dir_wd)
  make_varplot_wrapper(maxyr = maxyr,                       # Anom and mean plot
                       SRVY = SRVY,
                       haul = haul,
                       dat_survreg = dat_survreg,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = "latest",
                       survey_area = survey_area,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       plot_daily = FALSE,
                       plot_anom = TRUE,
                       plot_mean = TRUE,
                       dir_wd = dir_wd)
}

### EBS ------------------------------------------------------------------------
SRVY <- "EBS"
plot_subtitle <- "NOAA Fisheries eastern Bering Sea Bottom Trawl Survey"
region_akgfmaps = "bs.south"
survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")
survey_area$survey.grid <- survey_area$survey.grid %>%
  sf::st_transform(x = ., survey_area$crs$input) %>%
  dplyr::rename(station = STATIONID) %>%
  sp::merge(x = .,
            y = haul %>%
              dplyr::rename(station = stationid) %>%
              dplyr::select(station, stratum) %>%
              dplyr::distinct(),
            all.x = TRUE) %>%
  dplyr::filter(station %in% akgfmaps::get_survey_stations(select.region = region_akgfmaps))  %>%
  dplyr::mutate(region = "Bering Sea")

yrs <- list(2018 = "https://drive.google.com/drive/folders/1jaJrvKE729I15YnC6LhRDVI5Xxg4xEPo")

for (i in 1:length(yrs)) {
  
  maxyr <- as.numeric(names(yrs)[i])
  dir_googledrive_upload <- googledrive::as_id(yrs[i])
  
  make_grid_wrapper(maxyr = maxyr,                             # Blank grid plot
                    SRVY = SRVY,
                    haul = haul,
                    dat_survreg = dat_survreg,
                    dir_googledrive_upload = dir_googledrive_upload,
                    survey_area = survey_area,
                    data_source = data_source,
                    plot_subtitle = plot_subtitle,
                    dir_wd = dir_wd)
  make_varplot_wrapper(maxyr = maxyr,                               # Daily plot
                       SRVY = SRVY,
                       haul = haul,
                       dat_survreg = dat_survreg,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0,
                       survey_area = survey_area,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       plot_anom = plot_anom,
                       dir_wd = dir_wd)
  make_varplot_wrapper(maxyr = maxyr,                       # Anom and mean plot
                       SRVY = SRVY,
                       haul = haul,
                       dat_survreg = dat_survreg,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = "latest",
                       survey_area = survey_area,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       plot_daily = FALSE,
                       plot_anom = TRUE,
                       plot_mean = TRUE,
                       dir_wd = dir_wd)
}

## AI --------------------------------------------------------------------------
SRVY <- "AI"
plot_anom <- FALSE
plot_subtitle = "NOAA Fisheries Aleutian Islands Bottom Trawl Survey"
region_akgfmaps = "ai"
var = "bt"
show_planned_stations <- FALSE
survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")

survey_area$survey.grid <- rgdal::readOGR(dsn = paste0(dir_wd, '/shapefiles/'),# Prepare map objects
                                          layer = "aigrid_trawable_thru2018_Emily",
                                          verbose=F) %>%
  sp::spTransform(x = ., CRS(survey_area$crs$input)) %>%
  st_as_sf(x = .) %>%
  dplyr::rename(station = ID,
                stratum = STRATUM) %>%
  dplyr::filter(stratum %in% unique(goa_strata0$stratum) &
                  stratum != 0) %>% # land
  sp::merge(
    x = .,
    y = goa_strata0 %>%
      dplyr::filter(survey == "AI") %>%
      dplyr::mutate(SRVY = "AI",
                    region = stringr::str_to_title(inpfc_area),
                    region = dplyr::case_when(
                      region %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians",
                      TRUE ~ region)) %>%
      dplyr::select(SRVY, stratum, region) %>%
      dplyr::distinct(),
    all.x = TRUE)  %>% # , duplicateGeoms = TRUE
  dplyr::arrange(region)
survey_area$survey.grid1 <- survey_area$survey.grid

data_source = "oracle"
plot_anom = FALSE
dates0 <- "all" # latest 

dat_survreg <- dplyr::bind_rows(dat_survreg,
                                data.frame(reg_shapefile = "AI",
                                           region_long = "Aleutian Islands",
                                           SRVY = "AI",
                                           region = "AI",
                                           vessel_id = c(148, 143), # CHANGE
                                           vessel_shape = c("OEX", "SS"), # CHANGE
                                           reg_dates = "\n(June 07-Aug 17 2022)")) # CHANGE

yrs <- list(2018 = "https://drive.google.com/drive/folders/1dzWwb3bXnPXlSy_JIaY4BKo6WDshru_d")

for (i in 1:length(yrs)) {
  
  maxyr <- as.numeric(names(yrs)[i])
  dir_googledrive_upload <- googledrive::as_id(yrs[i])
  
  make_grid_wrapper(maxyr = maxyr,                               # Blank grid plot
                    SRVY = SRVY,
                    haul = haul,
                    dat_survreg = dat_survreg,
                    dir_googledrive_upload = dir_googledrive_upload,
                    survey_area = survey_area,
                    data_source = data_source,
                    plot_subtitle = plot_subtitle,
                    dir_wd = dir_wd)
  make_varplot_wrapper(maxyr = maxyr,                                 # Daily plot
                       SRVY = SRVY,
                       haul = haul,
                       dat_survreg = dat_survreg,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0,
                       survey_area = survey_area,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       plot_anom = plot_anom,
                       dir_wd = dir_wd)
}

# ## GOA --------------------------------------------------------------------------
# maxyr <- 2023 #CHANGE
# data_source <- "gd" # google drive
# SRVY <- "GOA"
# plot_anom <- FALSE
# plot_subtitle = "NOAA Fisheries Gulf of Alaska Bottom Trawl Survey"
# region_akgfmaps = "goa"
# # dir_googledrive_upload <- googledrive::as_id(dir_googledrive_upload_goa)
# # #dir_googledrive_upload <- googledrive::as_id(dir_googledrive_upload_test)
# var = "bt"
# # dates0 <- "latest" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
# show_planned_stations <- FALSE
# survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")
# 
# survey_area$survey.grid <- rgdal::readOGR(dsn = paste0(dir_wd, '/shapefiles/'),# Prepare map objects
#                                 layer = "aigrid_trawable_thru2018_Emily",
#                                 verbose=F) %>%
#   sp::spTransform(x = ., CRS(survey_area$crs$input)) %>%
#   st_as_sf(x = .) %>%
#   dplyr::rename(station = ID,
#                 stratum = STRATUM) %>%
#   dplyr::filter(stratum %in% unique(goa_strata0$stratum) &
#                   stratum != 0) %>% # land
#       dplyr::select(SRVY, stratum, region) %>%
#       dplyr::distinct(),
#     all.x = TRUE)  %>% # , duplicateGeoms = TRUE
#   dplyr::arrange(region)
# 
# make_grid_wrapper(maxyr = maxyr,                             # Blank grid plot
#                   SRVY = SRVY,
#                   haul = haul,
#                   dat_survreg = dat_survreg,
#                   dir_googledrive_upload = dir_googledrive_upload,
#                   survey_area = survey_area,
#                   data_source = data_source,
#                   plot_subtitle = plot_subtitle,
#                   dir_wd = dir_wd)
# make_varplot_wrapper(maxyr = maxyr,                               # Daily plot
#                   SRVY = SRVY,
#                   haul = haul,
#                   dat_survreg = dat_survreg,
#                   var = var,
#                   dir_googledrive_upload = dir_googledrive_upload,
#                   dates0 = dates0,
#                   survey_area = survey_area,
#                   plot_subtitle = plot_subtitle,
#                   show_planned_stations = show_planned_stations,
#                   data_source = data_source,
#                   plot_anom = plot_anom,
#                   dir_wd = dir_wd)
# 
# ### past years -----------------------------------------------------------------
# data_source = "oracle"
# plot_anom = FALSE
# dates0 <- "all" 
# 
# dat_survreg <- dplyr::bind_rows(dat_survreg, 
#                                data.frame(reg_shapefile = "GOA", 
#                                           region_long = "Gulf of Alaska", 
#                                           SRVY = "GOA", 
#                                           region = "GOA", 
#                                           vessel_id = c(148, 176), # CHANGE
#                                           vessel_shape = c("OEX", "SS"), # CHANGE
#                                           reg_dates = "May 25 - Aug 04")) # CHANGE
# 
# maxyr <- 2021
# dir_googledrive_upload <- googledrive::as_id("https://drive.google.com/drive/folders/1VANFDlNVQYi5GeYsIKn1ISBtXzEadCUx")
# make_grid_wrapper(maxyr = maxyr,                             # Blank grid plot
#                   SRVY = SRVY,
#                   haul = haul,
#                   dat_survreg = dat_survreg,
#                   dir_googledrive_upload = dir_googledrive_upload,
#                   survey_area = survey_area,
#                   data_source = data_source,
#                   plot_subtitle = plot_subtitle,
#                   dir_wd = dir_wd)
# make_varplot_wrapper(maxyr = maxyr,                               # Daily plot
#                   SRVY = SRVY,
#                   haul = haul,
#                   dat_survreg = dat_survreg,
#                   var = var,
#                   dir_googledrive_upload = dir_googledrive_upload,
#                   dates0 = dates0,
#                   survey_area = survey_area,
#                   plot_subtitle = plot_subtitle,
#                   show_planned_stations = show_planned_stations,
#                   data_source = data_source,
#                   plot_anom = plot_anom,
#                   dir_wd = dir_wd)
# make_varplot_wrapper(maxyr = maxyr,                       # Anom and mean plot
#                   SRVY = SRVY,
#                   haul = haul,
#                   dat_survreg = dat_survreg,
#                   var = var,
#                   dir_googledrive_upload = dir_googledrive_upload,
#                   dates0 = "latest",
#                   survey_area = survey_area,
#                   plot_subtitle = plot_subtitle,
#                   show_planned_stations = show_planned_stations,
#                   data_source = data_source,
#                   plot_daily = FALSE,
#                   plot_anom = TRUE,
#                   plot_mean = TRUE,
#                   dir_wd = dir_wd)

