#' ---------------------------
#' title: Survey Daily and Anomaly Temperature Plot
#' maintained: Emily Markowitz
#' purpose: run script
#' 
#' notes: 
#' https://github.com/afsc-gap-products/survey-live-temperature-map
#' AI/GOA station allocation docs can be found in (similar) G:\ALEUTIAN\AI 2024\Station Allocation
#' ---------------------------

# Set Working Directory --------------------------------------------------------
## Actually we cant use the here package, here - it actually causes issues with 
## the tasks scheduler, which has no concept of a project root folder. 
locations <- c("Z:/Projects/survey-live-temperature-map/")

for (i in 1:length(locations)){
  if (file.exists(locations[i])) {
    dir_wd  <- locations[i]
  }
}

# KNOWNS -----------------------------------------------------------------------

istest <- TRUE
maxyr <- 2024
data_source <- "gd" # "gd" = google dirve, "oracle" 
dates0 <-  "latest" # "latest" # "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
if (format(Sys.Date(), format = "%A") %in% c("Sunday", "Thursday") & 
    format(Sys.time(), format = "%H") %in% 2) { # maintenance cycle - make another task scheduler for this 
  dates0 <- "all"
}
var <- "bt"
survey_definition_id0 <- c(52, 98) # Survey ID. The survey definition ID key code uniquely identifies a survey/survey design. Integer code that uniquely identifies survey. Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS. IDs used in GAP_PRODUCTS are: 47 (Gulf of Alaska); 52 (Aleutian Islands); 78 (Bering Sea Slope); 98 (Eastern Bering Sea Shelf); 143 (Northern Bering Sea Shelf). The column "survey_definition_id" is associated with the "srvy" and "survey" columns. For a complete list of surveys, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).

dir_googledrive_log <- "https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE"
dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/1sP34UMQiTQvci4U6PMOFcnlFI0vQ1BH9" # REAL LINK
dir_googledrive_upload_ai = "https://drive.google.com/drive/folders/1PK2nnSprqOYV12Ae80YE_avp3Qj5IKEL" # REAL LINK

if(istest) {
  dir_googledrive_upload_ai = "https://drive.google.com/drive/folders/16Za6GFBGGg1YdwByF4gUqz_XU42IM4rA" # TEST LINK
  dir_googledrive_upload_bs = "https://drive.google.com/drive/folders/19Cy0gSn4TWcGymKeSNB8XIpwyEIzzw0m" # TEST LINK
  dates0 <- "all"
}
print(dates0)
# SIGN INTO GOOGLE DRIVE--------------------------------------------------------

googledrive_dl <- TRUE
googledrive::drive_deauth()
googledrive::drive_auth()
2

# LOG --------------------------------------------------------------------------
# if(!istest) {
#   sink(file = paste0(dir_wd, "/output/", Sys.Date(), ".txt"), append=TRUE, )
# }

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------
source(file = paste0(dir_wd,"code/functions.R"))
# source(file = paste0(dir_wd, "code/data_dl.R")) # you don't unnecessarily run this each time
source(file = paste0(dir_wd, "code/data.R"))

# SIGN INTO FTP ----------------------------------------------------------------
ftp_dl <- FALSE # test
# ftp_dl <- (googledrive_dl & file.exists(paste0(dir_wd, "code/ftp.R")))
ftp <- list(ftp_dl = ftp_dl)
if (ftp_dl) {
  source(file = paste0(dir_wd, "code/ftp.R")) # removed in gitignore - ask for permission
  ftp <- list(
    ftp_dl = ftp_dl,
    user = user,
    pass = pass)
}

# UPDATE README (sometimes) ----------------------------------------------------

# rmarkdown::render(here::here("code/README.Rmd"),
#                   output_dir = "./",
#                   output_file = here::here("README.md"))

# Map --------------------------------------------------------------------------

## AI --------------------------------------------------------------------------
if (52 %in% survey_definition_id0) { 
  
  SRVY <- "AI"; print(paste0("------------", SRVY, " Plots ------------"))
  plot_subtitle = "NOAA Fisheries Aleutian Islands Bottom Trawl Survey"
  dir_googledrive_upload <- ifelse(exists(x = "dir_googledrive_upload_ai") & googledrive_dl, dir_googledrive_upload_ai, NULL)
  plot_anom <- FALSE
  show_planned_stations <- FALSE
  dir_out <- paste0(dir_wd, "/output/", ifelse(istest, "TEST", maxyr), "_", SRVY, "/")  
  if(ftp_dl){ftp$dest <- dev_ai}
  
  file_end0 = c("daily") # , "grid"
  
  make_varplot_wrapper(maxyr = maxyr, 
                       SRVY = SRVY,
                       dat_survey = dat_survey,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0,
                       shp = shp,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       file_end0 = file_end0, 
                       dir_wd = dir_wd, 
                       dir_out = dir_out, 
                       ftp = ftp)
}

## EBS Maps --------------------------------------------------------------------
if (98 %in% survey_definition_id0) { 

  SRVY <- "EBS"; print(paste0("------------", SRVY, " Plots ------------"))
  plot_subtitle <- "NOAA Fisheries Eastern Bering Sea Bottom Trawl Survey"
  dir_googledrive_upload <- ifelse(exists("dir_googledrive_upload_bs") & googledrive_dl, dir_googledrive_upload_bs, NULL)
  show_planned_stations <- TRUE
  plot_anom <- TRUE
  dir_out <- paste0(dir_wd, "/output/", ifelse(istest, "TEST", maxyr), "_", SRVY, "/")  
  if(ftp_dl){ftp$dest <- dev_bs}
  
  file_end0 = c("daily", "anom") # , "grid", "mean", "anom"
  
  make_varplot_wrapper(maxyr = maxyr,
                       SRVY = SRVY,
                       dat_survey = dat_survey,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0,
                       shp = shp,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       file_end0 = file_end0, 
                       dir_wd = dir_wd, 
                       dir_out = dir_out, 
                       ftp = ftp)
}

### GOA --------------------------------------------------------------------------
# 
# if ("GOA" %in% dat_survey$SRVY) {
#   
#   SRVY <- "GOA"
#   plot_subtitle <- "NOAA Fisheries Gulf of Alaska Bottom Trawl Survey"
#   dir_googledrive_upload <- ifelse(exists("dir_googledrive_upload_ai") & googledrive_dl, dir_googledrive_upload_goa, NULL)
#   show_planned_stations <- FALSE
#   shp <- shp_goa
#   if(ftp_dl){ftp$dest <- dev_goa}
#   
#   make_varplot_wrapper(maxyr = maxyr, 
#                        SRVY = SRVY,
#                        dat_survey = dat_survey,
#                        var = var,
#                        dir_googledrive_upload = dir_googledrive_upload,
#                        dates0 = dates0,
#                        shp = shp,
#                        plot_subtitle = plot_subtitle,
#                        show_planned_stations = show_planned_stations,
#                        data_source = data_source,
#                        file_end0 = c("daily"),
#                        dir_wd = dir_wd)
# }
# 
### NBS + EBS Maps --------------------------------------------------------------
# 
# if ("NBS" %in% dat_survey$SRVY & "EBS" %in% dat_survey$SRVY) {
#   
#   SRVY <- "BS"
#   plot_subtitle <- "NOAA Fisheries Bering Sea Bottom Trawl Survey"
#   dir_googledrive_upload <- ifelse(exists("dir_googledrive_upload_bs") & googledrive_dl, dir_googledrive_upload_bs, NULL)
#   show_planned_stations <- TRUE
#   plot_anom <- TRUE
#   shp <- shp_bs
#   if(ftp_dl){ftp$dest <- dev_bs}
#   
#   make_varplot_wrapper(maxyr = maxyr,
#                        SRVY = SRVY,
#                        dat_survey = dat_survey,
#                        var = var,
#                        dir_googledrive_upload = dir_googledrive_upload,
#                        dates0 = dates0,
#                        shp = shp,
#                        plot_subtitle = plot_subtitle,
#                        show_planned_stations = show_planned_stations,
#                        data_source = data_source,
#                        file_end0 = c("daily", "anom"), 
#                        dir_wd = dir_wd, 
#                        ftp = ftp)
# }

# Log --------------------------------------------------------------------------
# if(!istest) {
#   sink()
# }