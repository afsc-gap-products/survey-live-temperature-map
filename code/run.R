#' ---------------------------
#' title: Survey Daily and Anomaly Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2022)
#' purpose: run script
#' ---------------------------

# KNOWNS -----------------------------------------------------------------------

maxyr <- 2022
data_source <- "gd" # google drive
data_source <- "oracle" # testing
dates0 <- "latest" # "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
plot_grid <- FALSE #set to TRUE to run make_grid_wrapper to run plot_gridiles for EBS and AI
var <- "bt"

# SIGN INTO GOOGLE DRIVE--------------------------------------------------------
## This sign in needs to be here for the Task Scheduler to run, please do not comment out.
googledrive_dl <- TRUE
googledrive::drive_deauth()
googledrive::drive_auth()
1

dir_googledrive_log <- "https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit#gid=315914502"
dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/1V9GLy2DkOz8UbMTw6eC0GxjMfWa5FeHm"
dir_googledrive_upload_test = "https://drive.google.com/drive/folders/1MrAq9jtQL1YBlYQcbeGiHh751EpPybY2"
dir_googledrive_upload_ai = dir_googledrive_upload_test
dir_googledrive_upload_goa = "https://drive.google.com/drive/folders/1OAZa4TDO3OOCKsKzMX-UzTwKGVVGFsOW"

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------
## Actually we cant use the here package, here - it actually causes issues with 
## the tasks scheduler, which has no concept of a project root folder. 
locations <- c(
  "C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/",
  "C:/Users/christopher.anderson/Work/survey-live-temperature-map/",
  "Z:/Projects/survey-live-temperature-map/", 
  "C:/Users/emily.markowitz/Documents/Projects/survey-live-temperature-map/")

for (i in 1:length(locations)){
  if (file.exists(locations[i])) {
    dir_wd  <- locations[i]
  }
}

source(file = paste0(dir_wd,"code/functions.R"))
# source(file = paste0(dir_wd, "code/data_dl.R")) # you don't unnecessarily run this each time
source(file = paste0(dir_wd, "code/data.R"))

ftp_dl <- (googledrive_dl & file.exists(paste0(dir_wd, "code/ftp.R")))
if (ftp_dl) {
  source(file = paste0(dir_wd, "code/ftp.R")) # removed in gitignore - ask for premission
}

# Map --------------------------------------------------------------------------

## GOA --------------------------------------------------------------------------

if ("GOA" %in% dat_survreg$SRVY) {
  
  maxyr <- 2021 # testing
  data_source <- "oracle" # testing
  
  SRVY <- "GOA"
  plot_subtitle <- "NOAA Fisheries Gulf of Alaska Bottom Trawl Survey"
  dir_googledrive_upload <- (dir_googledrive_upload_bs)
  show_planned_stations <- FALSE
  plot_anom <- FALSE
  survey_area <- shp_goa
  
  if (plot_grid){
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
  
  ### send all current files to the FTP -------------------------------------------
if (ftp_dl){
  upload_ftp( # vars here defined in ftp.R
    dir_wd = dir_wd, 
    dir_out = dir_out, 
    maxyr = maxyr, 
    SRVY = SRVY, 
    dest = dev_goa, 
    user = user, 
    pass = pass)
}
}
## NBS + EBS Maps --------------------------------------------------------------

if ("NBS" %in% dat_survreg$SRVY & "EBS" %in% dat_survreg$SRVY) {
  
  SRVY <- "BS"
  plot_subtitle <- "NOAA Fisheries Bering Sea Bottom Trawl Survey"
  dir_googledrive_upload <- (dir_googledrive_upload_bs)
  show_planned_stations <- TRUE
  plot_anom <- TRUE
  survey_area <- shp_bs
  
  if (plot_grid){
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
  
  ### send all current files to the FTP -------------------------------------------
  if (ftp_dl){
    upload_ftp( # vars here defined in ftp.R
    dir_wd = dir_wd, 
    dir_out = dir_out, 
    maxyr = maxyr, 
    SRVY = SRVY, 
    dest = dev_bs, 
    user = user, 
    pass = pass)
  }
}

## AI --------------------------------------------------------------------------
if ("AI" %in% dat_survreg$SRVY) {
  
  SRVY <- "AI"
  plot_subtitle = "NOAA Fisheries Aleutian Islands Bottom Trawl Survey"
  dir_googledrive_upload <- (dir_googledrive_upload_ai)
  plot_anom <- FALSE
  show_planned_stations <- FALSE
  survey_area <- shp_ai
  
  if (plot_grid) {
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
  
  ### send all current files to the FTP -------------------------------------------
  if (ftp_dl){
    upload_ftp( # vars here defined in ftp.R
    dir_wd = dir_wd, 
    dir_out = dir_out, 
    maxyr = maxyr, 
    SRVY = SRVY, 
    dest = dev_ai, 
    user = user, 
    pass = pass)
  }
}