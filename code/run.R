#' ---------------------------
#' title: Survey Daily and Anomaly Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (Jan 2022)
#' purpose: run script
#' ---------------------------

# Knowns -----------------------------------------------------------------------

maxyr <- 2022 #CHANGE
googledrive_dl <- TRUE
dir_googledrive_log <- "https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit#gid=315914502"
dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/1vWza36Dog0SpZLcTN22wD-iCEn6ooGCM"
dir_googledrive_upload_ai = "https://drive.google.com/drive/folders/1SeNOAh5-muQ2BDgOHWZWwYIoLl68DHWX"
#dir_googledrive_upload_test = "https://drive.google.com/drive/folders/1HDKTMR8wPghIL6pUo2zv8OJF3Od8exev"

# The surveys this script will be covering 
dat_survreg <- data.frame(reg_shapefile = "EBS_SHELF", 
                          region_long = "Eastern Bering Sea", 
                          SRVY = "EBS", 
                          region = "BS", 
                          vessel_id = c(94, 162), # CHANGE
                          vessel_shape = c("V", "A"), # CHANGE
                          reg_dates = "\n(May 25-Aug 03)") # CHANGE
dat_survreg <- dplyr::bind_rows(dat_survreg, 
                                data.frame(reg_shapefile = "NBS_SHELF", 
                                           region_long = "Northern Bering Sea", 
                                           SRVY = "NBS", 
                                           region = "BS", 
                                           vessel_id = c(94, 162), # CHANGE
                                           vessel_shape = c("V", "A"), # CHANGE
                                           reg_dates = "\n(Aug 03-Aug 28)")) # CHANGE
dat_survreg <- dplyr::bind_rows(dat_survreg, 
                                data.frame(reg_shapefile = "AI",
                                           region_long = "Aleutian Islands",
                                           SRVY = "AI",
                                           region = "AI",
                                           vessel_id = c(148, 176), # CHANGE
                                           vessel_shape = c("O", "S"), # CHANGE
                                           reg_dates = "\n(May 25-Aug 04)")) # CHANGE
# dat_survreg <- dplyr::bind_rows(dat_survreg, 
#                                data.frame(reg_shapefile = "GOA", 
#                                           region_long = "Gulf of Alaska", 
#                                           SRVY = "GOA", 
#                                           region = "GOA", 
#                                           vessel_id = c(148, 176), # CHANGE
#                                           vessel_shape = c("OEX", "SS"), # CHANGE
#                                           reg_dates = "\n(May 25-Aug 04)")) # CHANGE

# SIGN INTO GOOGLE DRIVE--------------------------------------------------------
## This sign in needs to be here for the Task Scheduler to run, please do not comment out.
googledrive::drive_deauth()
googledrive::drive_auth()
1

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------
## Actually we cant use the here package - it actually causes issues with the tasks scheduler, 
## which has no concept of a project root folder. 
# dir_in<-"C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
# dir_in <- "G:/EBSother/GAPsurveyTemperatureMap/"
# dir_in<-"C:/Users/emily.markowitz/Work/Projects/GAPSurveyTemperatureMap/"
dir_in<- paste0(getwd(), "/") 

source(file = paste0(dir_in,"code/functions.R"))
# source(file = paste0(dir_in, "code/data_dl.R")) # you don't unnecessarily run this each time
source(file = paste0(dir_in, "code/data.R"))

# dates0 <- as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
# dates0 <-"all"

# Map --------------------------------------------------------------------------

## NBS + EBS Maps --------------------------------------------------------------
maxyr <- 2022 #CHANGE
SRVY <- "BS"
plot_subtitle <- "NOAA Fisheries Bering Sea Bottom Trawl Survey"
dir_googledrive_upload <- googledrive::as_id(dir_googledrive_upload_bs)
#dir_googledrive_upload <- googledrive::as_id(dir_googledrive_upload_test)
region_akgfmaps = "bs.all"
survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")

# Daily
var = "bt"
dates0 <- "all" # "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))git 
show_planned_stations <- TRUE
survey_area$survey.grid <- survey_area$survey.grid %>% 
  sf::st_transform(x = ., survey_area$crs$input) %>%
  # dplyr::filter(!(STATION %in% c("DD-09", "AA-10"))) %>% 
  dplyr::rename(station = STATIONID) %>%
  sp::merge(x = ., 
            y = haul %>%
              dplyr::rename(station = stationid) %>% 
              dplyr::select(station, stratum) %>% 
              dplyr::distinct(), 
            all.x = TRUE) %>% 
  dplyr::mutate(region = "Bering Sea")
survey_area$place.labels$y[survey_area$place.labels$lab == "200 m"] <- -60032.7

make_plot_wrapper(maxyr = maxyr, 
                  SRVY = SRVY, 
                  haul = haul, 
                  dat_survreg = dat_survreg, 
                  var = var,
                  dir_googledrive_upload = dir_googledrive_upload, 
                  dates0 = dates0, 
                  survey_area = survey_area, 
                  plot_subtitle = plot_subtitle, 
                  show_planned_stations = show_planned_stations)

# Blank Grid (no survey data) 
# Just the empty grid (comment this v out when running after beginning of survey)
# make_grid_wrapper(maxyr = maxyr, 
#                   SRVY = SRVY, 
#                   haul = haul, 
#                   dat_survreg = dat_survreg, 
#                   dir_googledrive_upload = dir_googledrive_upload, 
#                   survey_area = survey_area, 
#                   plot_subtitle = plot_subtitle)

# ### past years -----------------------------------------------------------------
# data_source = "haul"
# plot_anom = FALSE
# dates0 <- "all"
# 
# maxyr <- 2021
# dir_googledrive_upload <- googledrive::as_id("https://drive.google.com/drive/folders/1q4UN9INXFAyZcIwqy8W9UYfY3G1LuQgW")
# make_plot_wrapper(maxyr = maxyr, 
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
#                   plot_anom = TRUE)
# 
# maxyr <- 2019
# dir_googledrive_upload <- googledrive::as_id("https://drive.google.com/drive/folders/1S5FyXwWyFUgFkvDlGTC6cymtISyZdd9R")
# make_plot_wrapper(maxyr = maxyr, 
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
#                   plot_anom = plot_anom)
# 
# maxyr <- 2018
# SRVY <- "EBS"
# plot_subtitle <- "NOAA Fisheries eastern Bering Sea Bottom Trawl Survey"
# dir_googledrive_upload <- googledrive::as_id("https://drive.google.com/drive/folders/1jaJrvKE729I15YnC6LhRDVI5Xxg4xEPo")
# region_akgfmaps = "bs.south"
# survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")
# survey_area$survey.grid <- survey_area$survey.grid %>% 
#   sf::st_transform(x = ., survey_area$crs$input) %>%
#   dplyr::rename(station = STATIONID) %>%
#   sp::merge(x = ., 
#             y = haul %>%
#               dplyr::rename(station = stationid) %>% 
#               dplyr::select(station, stratum) %>% 
#               dplyr::distinct(), 
#             all.x = TRUE) %>% 
#   dplyr::filter(station %in% akgfmaps::get_survey_stations(select.region = region_akgfmaps))  %>% 
#   dplyr::mutate(region = "Bering Sea")
# 
# make_plot_wrapper(maxyr = maxyr, 
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
#                   plot_anom = plot_anom)
# 
# # Blank Grid (no survey data) 
# # Just the empty grid (comment this v out when running after beginning of survey)
# make_grid_wrapper(maxyr = maxyr, 
#                   SRVY = SRVY, 
#                   haul = haul, 
#                   dat_survreg = dat_survreg, 
#                   dir_googledrive_upload = dir_googledrive_upload, 
#                   survey_area = survey_area, 
#                   data_source = data_source, 
#                   plot_subtitle = plot_subtitle)

## AI --------------------------------------------------------------------------
maxyr <- 2018 #CHANGE
SRVY <- "AI"
plot_anom <- FALSE
plot_subtitle = "NOAA Fisheries Aleutian Islands Bottom Trawl Survey"
region_akgfmaps = "ai"
dir_googledrive_upload <- googledrive::as_id(dir_googledrive_upload_ai)
#dir_googledrive_upload <- googledrive::as_id(dir_googledrive_upload_test)
var = "bt"
dates0 <- "2018-08-08" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
show_planned_stations <- FALSE
survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")

survey_area$survey.grid <- rgdal::readOGR(dsn = paste0(dir_in, '/shapefiles/'),# Prepare map objects
                                layer = "aigrid_trawable_thru2018",
                                verbose=F) %>% 
  sp::spTransform(x = ., CRS(survey_area$crs$input)) %>%
  st_as_sf(x = .) %>%
  dplyr::rename(station = ID, 
                stratum = STRATUM) %>%
  dplyr::filter(stratum %in% unique(goa_strata0$stratum)) %>%
  # dplyr::mutate(region = dplyr::case_when())
  sp::merge(
    x = .,
    y = goa_strata0 %>%
      dplyr::mutate(SRVY == "AI",
                    region = stringr::str_to_title(inpfc_area),
                    region = dplyr::case_when(
                      region %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians", 
                      TRUE ~ region)) %>%
      dplyr::select(stratum, region) %>%
      dplyr::distinct(),
    all.x = TRUE, duplicateGeoms = TRUE)

# Daily
make_plot_wrapper(maxyr = maxyr, 
                  SRVY = SRVY, 
                  haul = haul, 
                  dat_survreg = dat_survreg, 
                  var = var,
                  dir_googledrive_upload = dir_googledrive_upload, 
                  dates0 = dates0, 
                  survey_area = survey_area, 
                  plot_subtitle = plot_subtitle, 
                  show_planned_stations = show_planned_stations)

# Blank Grid (no survey data) 
# Just the empty grid (comment this v out when running after beginning of survey)
make_grid_wrapper(maxyr = maxyr, 
                  SRVY = SRVY, 
                  haul = haul, 
                  dat_survreg = dat_survreg, 
                  dir_googledrive_upload = dir_googledrive_upload, 
                  survey_area = survey_area, 
                  plot_subtitle = plot_subtitle)

# ### past years -----------------------------------------------------------------
#data_source = "haul"
# plot_anom = FALSE
# dates0 <- "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
# 
# maxyr <- 2018
# dir_googledrive_upload <- googledrive::as_id("https://drive.google.com/drive/folders/1dzWwb3bXnPXlSy_JIaY4BKo6WDshru_d")
# make_plot_wrapper(maxyr = maxyr,
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
#                   plot_anom = FALSE)

