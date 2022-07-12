#' ---------------------------
#' title: Survey Daily and Anomaly Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2022)
#' purpose: run script
#' ---------------------------

# Knowns -----------------------------------------------------------------------

maxyr <- 2022 
data_source <- "gd" # google drive
dates0 <- "latest" # "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
shapef <- FALSE #set to TRUE to run make_grid_wrapper to run shapefiles for EBS and AI
var <- "bt"

googledrive_dl <- TRUE
dir_googledrive_log <- "https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit#gid=315914502"
#dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/1vWza36Dog0SpZLcTN22wD-iCEn6ooGCM"
dir_googledrive_upload_ai = "https://drive.google.com/drive/folders/1SeNOAh5-muQ2BDgOHWZWwYIoLl68DHWX"
#dir_googledrive_upload_test = "https://drive.google.com/drive/folders/1rsR0aFfFzrspTBFU48Bb26EJvdhIZSpl"

# The surveys this script will be covering 
dat_survreg <- data.frame(reg_shapefile = "EBS_SHELF", 
                          region_long = "Eastern Bering Sea", 
                          SRVY = "EBS", 
                          region = "BS", 
                          vessel_id = c(94, 162), # CHANGE
                          vessel_shape = c("V", "A"), # CHANGE
                          reg_dates = "May 25 - Aug 03 2022") # CHANGE
dat_survreg <- dplyr::bind_rows(dat_survreg, 
                                data.frame(reg_shapefile = "NBS_SHELF", 
                                           region_long = "Northern Bering Sea", 
                                           SRVY = "NBS", 
                                           region = "BS", 
                                           vessel_id = c(94, 162), # CHANGE
                                           vessel_shape = c("V", "A"), # CHANGE
                                           reg_dates = "Aug 03 - Aug 28 2022")) # CHANGE
dat_survreg <- dplyr::bind_rows(dat_survreg, 
                                data.frame(reg_shapefile = "AI",
                                           region_long = "Aleutian Islands",
                                           SRVY = "AI",
                                           region = "AI",
                                           vessel_id = c(148, 176), # CHANGE
                                           vessel_shape = c("OEX", "AP"), # CHANGE
                                           reg_dates = "Jun 07 - Aug 17 2022")) # CHANGE
# dat_survreg <- dplyr::bind_rows(dat_survreg, 
#                                data.frame(reg_shapefile = "GOA", 
#                                           region_long = "Gulf of Alaska", 
#                                           SRVY = "GOA", 
#                                           region = "GOA", 
#                                           vessel_id = c(148, 176), # CHANGE
#                                           vessel_shape = c("OEX", "SS"), # CHANGE
#                                           reg_dates = "May 25 - Aug 04")) # CHANGE

# SIGN INTO GOOGLE DRIVE--------------------------------------------------------
## This sign in needs to be here for the Task Scheduler to run, please do not comment out.
googledrive::drive_deauth()
googledrive::drive_auth()
1

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------
## Actually we cant use the here package, here - it actually causes issues with 
## the tasks scheduler, which has no concept of a project root folder. 
#dir_wd <-"C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
#dir_wd <- "G:/EBSother/GAPsurveyTemperatureMap/"
#dir_wd <- paste0(getwd(), "/")
#dir_wd <-"C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
dir_wd <- "C:/Users/caitlin.akselrud/Work/survey-live-temperature-map/"

source(file = paste0(dir_wd,"code/functions.R"))
# source(file = paste0(dir_wd, "code/data_dl.R")) # you don't unnecessarily run this each time
source(file = paste0(dir_wd, "code/data.R"))
if (googledrive_dl == TRUE) {
  source(file = paste0(dir_wd, "code/ftp.R")) # removed in gitignore - ask for premission
}

# Map --------------------------------------------------------------------------
# ## AI --------------------------------------------------------------------------
SRVY <- "AI"
region_akgfmaps = "ai"
plot_subtitle = "NOAA Fisheries Aleutian Islands Bottom Trawl Survey"
dir_googledrive_upload <- (dir_googledrive_upload_ai)
plot_anom <- FALSE
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
    all.x = TRUE)  %>% 
  dplyr::arrange(region)
survey_area$survey.grid1 <- survey_area$survey.grid

if(shapef == TRUE)
{
  make_grid_wrapper(maxyr = maxyr,                             # Blank grid plot
                    SRVY = SRVY,
                    haul = haul,
                    dat_survreg = dat_survreg,
                    dir_googledrive_upload = dir_googledrive_upload,
                    survey_area = survey_area,
                    data_source = data_source,
                    plot_subtitle = plot_subtitle,
                    dir_wd = dir_wd)
}

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

## send all current files to the FTP -------------------------------------------
# vars here defined in ftp.R
dir_out <- paste0(dir_wd,"/output/",maxyr,"_",SRVY,"/")
temp <- list.files(path = dir_out, pattern = "current_daily", full.names = FALSE)
dest <- dev_ai

for (iiii in 1:length(temp)) {
  print(temp[iiii])
  
  RCurl::ftpUpload(
    what = paste0(dir_out, "/", temp[iiii]),
    asText = FALSE,
    to = paste0(glue::glue("{protocol}://STOR@{server}/{dest}/", temp[iiii])),
    userpwd = paste0(user,":", pass),
    .opts=curlOptions(verbose=TRUE))
}

# NBS + EBS Maps --------------------------------------------------------------

maxyr <- 2022 
data_source <- "gd" # google drive
dates0 <- "latest" # "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
shapef <- FALSE #set to TRUE to run make_grid_wrapper to run shapefiles for EBS and AI
var <- "bt"

googledrive_dl <- TRUE
dir_googledrive_log <- "https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit#gid=315914502"
dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/1vWza36Dog0SpZLcTN22wD-iCEn6ooGCM"
#dir_googledrive_upload_ai = "https://drive.google.com/drive/folders/1SeNOAh5-muQ2BDgOHWZWwYIoLl68DHWX"
#dir_googledrive_upload_test = "https://drive.google.com/drive/folders/1rsR0aFfFzrspTBFU48Bb26EJvdhIZSpl"

# The surveys this script will be covering 
dat_survreg <- data.frame(reg_shapefile = "EBS_SHELF", 
                          region_long = "Eastern Bering Sea", 
                          SRVY = "EBS", 
                          region = "BS", 
                          vessel_id = c(94, 162), # CHANGE
                          vessel_shape = c("V", "A"), # CHANGE
                          reg_dates = "May 25 - Aug 03 2022") # CHANGE
dat_survreg <- dplyr::bind_rows(dat_survreg, 
                                data.frame(reg_shapefile = "NBS_SHELF", 
                                           region_long = "Northern Bering Sea", 
                                           SRVY = "NBS", 
                                           region = "BS", 
                                           vessel_id = c(94, 162), # CHANGE
                                           vessel_shape = c("V", "A"), # CHANGE
                                           reg_dates = "Aug 03 - Aug 28 2022")) # CHANGE
dat_survreg <- dplyr::bind_rows(dat_survreg, 
                                data.frame(reg_shapefile = "AI",
                                           region_long = "Aleutian Islands",
                                           SRVY = "AI",
                                           region = "AI",
                                           vessel_id = c(148, 176), # CHANGE
                                           vessel_shape = c("OEX", "AP"), # CHANGE
                                           reg_dates = "Jun 07 - Aug 17 2022")) # CHANGE
# dat_survreg <- dplyr::bind_rows(dat_survreg, 
#                                data.frame(reg_shapefile = "GOA", 
#                                           region_long = "Gulf of Alaska", 
#                                           SRVY = "GOA", 
#                                           region = "GOA", 
#                                           vessel_id = c(148, 176), # CHANGE
#                                           vessel_shape = c("OEX", "SS"), # CHANGE
#                                           reg_dates = "May 25 - Aug 04")) # CHANGE

# SIGN INTO GOOGLE DRIVE--------------------------------------------------------
## This sign in needs to be here for the Task Scheduler to run, please do not comment out.
googledrive::drive_deauth()
googledrive::drive_auth()
1

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------
## Actually we cant use the here package, here - it actually causes issues with 
## the tasks scheduler, which has no concept of a project root folder. 
#dir_wd <-"C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
#dir_wd <- "G:/EBSother/GAPsurveyTemperatureMap/"
#dir_wd <- paste0(getwd(), "/")
#dir_wd <-"C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
dir_wd <- "C:/Users/caitlin.akselrud/Work/survey-live-temperature-map/"

source(file = paste0(dir_wd,"code/functions.R"))
# source(file = paste0(dir_wd, "code/data_dl.R")) # you don't unnecessarily run this each time
source(file = paste0(dir_wd, "code/data.R"))
if (googledrive_dl == TRUE) {
  source(file = paste0(dir_wd, "code/ftp.R")) # removed in gitignore - ask for premission
}

SRVY <- "BS"
region_akgfmaps = "bs.all"
plot_subtitle <- "NOAA Fisheries Bering Sea Bottom Trawl Survey"
dir_googledrive_upload <- (dir_googledrive_upload_bs)
survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")
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

if(shapef == TRUE)
{
  make_grid_wrapper(maxyr = maxyr,                             # Blank grid plot
                    SRVY = SRVY,
                    haul = haul,
                    dat_survreg = dat_survreg,
                    dir_googledrive_upload = dir_googledrive_upload,
                    survey_area = survey_area,
                    data_source = data_source,
                    plot_subtitle = plot_subtitle,
                    dir_wd = dir_wd)
}

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

dir_out <- paste0(dir_wd,"/output/",maxyr,"_",SRVY,"/")
temp <- list.files(path = dir_out, pattern = "current_daily", full.names = FALSE)
dest <- dev_bs

for (iiii in 1:length(temp)) {
  print(temp[iiii])
  
  RCurl::ftpUpload(
    what = paste0(dir_out, "/", temp[iiii]),
    asText = FALSE,
    to = paste0(glue::glue("{protocol}://STOR@{server}/{dest}/", temp[iiii])),
    userpwd = paste0(user,":", pass),
    .opts=curlOptions(verbose=TRUE))
}
