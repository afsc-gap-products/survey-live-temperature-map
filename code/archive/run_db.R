#' ---------------------------
#' title: Survey Daily and Anomaly Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2022)
#' purpose: run script
#' ---------------------------

# KNOWNS -----------------------------------------------------------------------

maxyr <- 2024
data_source <- "gd" # "oracle" 
dates0 <- "latest" # "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
var <- "bt"
survey_definition_id0 <- c(52, 98) # Survey ID. The survey definition ID key code uniquely identifies a survey/survey design. Integer code that uniquely identifies survey. Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS. IDs used in GAP_PRODUCTS are: 47 (Gulf of Alaska); 52 (Aleutian Islands); 78 (Bering Sea Slope); 98 (Eastern Bering Sea Shelf); 143 (Northern Bering Sea Shelf). The column "survey_definition_id" is associated with the "srvy" and "survey" columns. For a complete list of surveys, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

dir_googledrive_log <- "https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE"
dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/1lszcLPkynLF28JI-GcOkEX94hAErFXJR" # TEST LINK
dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/1pYyox1MFBAUJUHqqvAupnfkEN1uSaFV3" # REAL LINK COLOR BLIND
dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/1sP34UMQiTQvci4U6PMOFcnlFI0vQ1BH9" # REAL LINK RAINBOW
dir_googledrive_upload_ai = "https://drive.google.com/drive/folders/1lLFgva1E5h7aEvRc9w4Q-gEQkBOE5_80" # TEST LINK
dir_googledrive_upload_ai = "https://drive.google.com/drive/folders/1N0L8yfTK_XbmsVgMD4OLbaeDJEeZtRG-" # REAL LINK COLOR BLIND
dir_googledrive_upload_ai = "https://drive.google.com/drive/folders/1PK2nnSprqOYV12Ae80YE_avp3Qj5IKEL" # REAL LINK RAINBOW

# SIGN INTO GOOGLE DRIVE--------------------------------------------------------

googledrive_dl <- TRUE
googledrive::drive_deauth()
googledrive::drive_auth()
2

# Set Working Directory --------------------------------------------------------
## Actually we cant use the here package, here - it actually causes issues with 
## the tasks scheduler, which has no concept of a project root folder. 
# locations <- c(
#   "C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/",
#   "C:/Users/christopher.anderson/Work/survey-live-temperature-map/",
#   "C:/Users/emily.markowitz/Work/projects/survey-live-temperature-map/",
#   "Z:/Projects/survey-live-temperature-map/")
# 
# for (i in 1:length(locations)){
#   if (file.exists(locations[i])) {
#     dir_wd  <- locations[i]
#   }
# }
# dir_wd <- "C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
dir_wd <- paste0(here::here(), "/")

# LOG --------------------------------------------------------------------------
# sink(file = paste0(dir_wd, "/output/", Sys.Date(), ".txt"), append=TRUE)

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------
source(file = here::here("code","functions.R"))
# source(file = paste0(dir_wd, "code/data_dl.R")) # you don't unnecessarily run this each time
source(file = here::here("code", "data_db.R"))

# What surveys should be run for this year and obtain necessary metadata
dat_survey <- dat_survey |>
  dplyr::filter(year == maxyr)

# SIGN INTO FTP ----------------------------------------------------------------
ftp_dl <- FALSE 
# ftp_dl <- (googledrive_dl & file.exists(paste0(dir_wd, "code/ftp.R")))
ftp <- list(ftp_dl = ftp_dl)
if (ftp_dl) {
  source(file = paste0(dir_wd, "code/ftp.R")) # removed in gitignore - ask for permission
  ftp <- list(
    ftp_dl = ftp_dl,
    user = user,
    pass = pass)
}

## UPDATE README (sometimes) ---------------------------------------------------

# rmarkdown::render(paste0("./README.Rmd"),
#                   output_dir = "./",
#                   output_file = paste0("README.md"))

# Map --------------------------------------------------------------------------

## GOA --------------------------------------------------------------------------

if ("GOA" %in% dat_survey$srvy) {

  srvy <- "GOA"
  plot_subtitle <- "NOAA Fisheries Gulf of Alaska Bottom Trawl Survey"
  if (googledrive_dl) {
    dir_googledrive_upload <- dir_googledrive_upload_goa
  } else {
    dir_googledrive_upload <- NULL
  }
  show_planned_stations <- FALSE
  survey_area <- shp_goa
  if(ftp_dl){ftp$dest <- dev_goa}
  
  make_varplot_wrapper(maxyr = maxyr, 
                       srvy = srvy,
                       haul = haul,
                       dat_survey = dat_survey,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0,
                       survey_area = survey_area,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       file_end0 = c("daily", "grid"),
                       dir_wd = dir_wd)
}

## NBS + EBS Maps --------------------------------------------------------------

if ("NBS" %in% dat_survey$srvy & "EBS" %in% dat_survey$srvy) {

  srvy <- "BS"
  plot_subtitle <- "NOAA Fisheries Bering Sea Bottom Trawl Survey"
  if (googledrive_dl) {
    dir_googledrive_upload <- dir_googledrive_upload_bs
  } else {
    dir_googledrive_upload <- NULL
  }
  show_planned_stations <- FALSE
  survey_area <- shp_bs
  if(ftp_dl){ftp$dest <- dev_bs}
  
  make_varplot_wrapper(maxyr = maxyr,
                       srvy = srvy,
                       haul = haul,
                       dat_survey = dat_survey,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0,
                       survey_area = survey_area,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       file_end0 = c("daily", "anom", 'grid', "mean"), 
                       dir_wd = dir_wd, 
                       ftp = ftp)
}

## AI --------------------------------------------------------------------------
if ("AI" %in% dat_survey$srvy) { # won't run in 2023 because is not in dat_survey
  
  srvy <- "AI"
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
  
  make_varplot_wrapper(maxyr = maxyr, 
                       srvy = srvy,
                       haul = haul,
                       dat_survey = dat_survey,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0,
                       survey_area = survey_area,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       file_end0 = c("daily", "grid"), 
                       dir_wd = dir_wd, 
                       ftp = ftp)
}


