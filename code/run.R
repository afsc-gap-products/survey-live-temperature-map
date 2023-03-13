#' ---------------------------
#' title: Survey Daily and Anomaly Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2022)
#' purpose: run script
#' ---------------------------

# KNOWNS -----------------------------------------------------------------------

maxyr <- 2023
data_source <- "gd" # google drive
dates0 <- "latest" # "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
var <- "bt"

# SIGN INTO GOOGLE DRIVE--------------------------------------------------------

## This sign in needs to be here for the Task Scheduler to run, please do not comment out.
googledrive_dl <- TRUE
# if (googledrive_dl) {
googledrive::drive_deauth()
googledrive::drive_auth()
1
# }

dir_googledrive_log <- "https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE"
dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/1V9GLy2DkOz8UbMTw6eC0GxjMfWa5FeHm"
dir_googledrive_upload_goa = "https://drive.google.com/drive/folders/1OAZa4TDO3OOCKsKzMX-UzTwKGVVGFsOW"

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------

## Actually we cant use the here package, here - it actually causes issues with 
## the tasks scheduler, which has no concept of a project root folder. 
locations <- c(
  "C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/",
  "C:/Users/christopher.anderson/Work/survey-live-temperature-map/", 
  "C:/Users/emily.markowitz/Documents/Projects/survey-live-temperature-map/",
  "Z:/Projects/survey-live-temperature-map/")

for (i in 1:length(locations)){
  if (file.exists(locations[i])) {
    dir_wd  <- locations[i]
  }
}

source(file = paste0(dir_wd,"code/functions.R"))
# source(file = paste0(dir_wd, "code/data_dl.R")) # you don't unnecessarily run this each time
source(file = paste0(dir_wd, "code/data.R"))

# dat_survreg <- dat_survreg %>%
#   dplyr::filter(year == maxyr)

ftp_dl <- (googledrive_dl & file.exists(paste0(dir_wd, "code/ftp.R")))
ftp <- list(ftp_dl = ftp_dl)
if (ftp_dl) {
  source(file = paste0(dir_wd, "code/ftp.R")) # removed in gitignore - ask for permission
  ftp <- list(
    ftp_dl = ftp_dl, 
    user = user, 
    pass = pass)
}

## Update README (sometimes) ---------------------------------------------------

# rmarkdown::render(paste0("./README.Rmd"),
#                   output_dir = "./",
#                   output_file = paste0("README.md"))

## Testing ---------------------------------------------------------------------

dir_googledrive_upload_test = "https://drive.google.com/drive/folders/1MrAq9jtQL1YBlYQcbeGiHh751EpPybY2"
temp <- googledrive::drive_ls(googledrive::as_id(dir_googledrive_upload_test))
dir_googledrive_upload_bs = temp$id[temp$name=="2022_BS"]
dir_googledrive_upload_ai = temp$id[temp$name=="2022_AI"]
dir_googledrive_upload_goa = temp$id[temp$name=="2021_GOA"]
data_source <- "oracle" # testing
dates0 <- "all"

# Map --------------------------------------------------------------------------

## GOA --------------------------------------------------------------------------

# if ("GOA" %in% dat_survreg$SRVY) {
  
  maxyr <- 2021 # testing
  data_source <- "oracle" # testing
  
  SRVY <- "GOA"
  plot_subtitle <- "NOAA Fisheries Gulf of Alaska Bottom Trawl Survey"
  if (googledrive_dl) {
    dir_googledrive_upload <- dir_googledrive_upload_goa
  } else {
    dir_googledrive_upload <- NULL
  }
  show_planned_stations <- FALSE
  survey_area <- shp_goa
  if(ftp_dl){ftp$dest <- dev_goa}
  
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
                       file_end0 = c("daily", "grid"),# "mean", "anom"
                       dir_wd = dir_wd)
# }

## NBS + EBS Maps --------------------------------------------------------------

# if ("NBS" %in% dat_survreg$SRVY & "EBS" %in% dat_survreg$SRVY) {
  
  maxyr <- 2022 # testing
  
  SRVY <- "BS"
  plot_subtitle <- "NOAA Fisheries Bering Sea Bottom Trawl Survey"
  if (googledrive_dl) {
    dir_googledrive_upload <- dir_googledrive_upload_bs
  } else {
    dir_googledrive_upload <- NULL
  }
  show_planned_stations <- TRUE
  plot_anom <- TRUE
  survey_area <- shp_bs
  if(ftp_dl){ftp$dest <- dev_bs}
  
  make_varplot_wrapper(maxyr = maxyr,                               # Daily plot
                       SRVY = SRVY,
                       haul = haul,
                       dat_survreg = dat_survreg,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0, # "all", # "first", # "latest", # dates0,
                       survey_area = survey_area,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       file_end0 = c("daily", "anom", "grid", "mean"),
                       dir_wd = dir_wd, 
                       ftp = ftp)
# }

## AI --------------------------------------------------------------------------
# if ("AI" %in% dat_survreg$SRVY) {
  
  maxyr <- 2022 # testing
  data_source <- "oracle" # testing
  
  SRVY <- "AI"
  plot_subtitle = "NOAA Fisheries Aleutian Islands Bottom Trawl Survey"
  if (googledrive_dl) {
    dir_googledrive_upload <- dir_googledrive_upload_ai
  } else {
    dir_googledrive_upload <- NULL
  }
  plot_anom <- FALSE
  show_planned_stations <- FALSE
  survey_area <- shp_ai
  if(ftp_dl){ftp$dest <- dev_ai}
  
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
                       file_end0 = c("grid", "daily"), #"mean", "anom"
                       dir_wd = dir_wd, 
                       ftp = ftp)
# }
