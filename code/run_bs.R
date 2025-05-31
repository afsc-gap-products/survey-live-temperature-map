#' ---------------------------
#' title: Survey Daily and Anomaly Temperature Plot
#' maintained: Emily Markowitz
#' 
#' notes: 
#' https://github.com/afsc-gap-products/survey-live-temperature-map
#' AI/GOA station allocation docs can be found in (similar) G:/ALEUTIAN/AI 2024/Station Allocation
#' ---------------------------

# Set Working Directory --------------------------------------------------------
## Actually we cant directly use the here package, here - it actually causes 
# issues with the tasks scheduler, which has no concept of a project root folder.

library(here)
here::here("Z:/Projects/survey-live-temperature-map_general/survey-live-temperature-map/") 
dir_wd <- paste0(here::here(), "/")

# KNOWNS -----------------------------------------------------------------------

istest <- FALSE
maxyr <- 2025
data_source <- c("gd", "race_data") # "gd" = google dirve, "oracle" 
dates0 <-  "latest" # "latest" # "all" # latest # "all", #"2021-06-05",# Sys.Date(), # as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))
if (format(Sys.Date(), format = "%A") %in% c("Sunday", "Tuesday", "Thursday") & 
    format(Sys.time(), format = "%H") %in% 2) { # maintenance cycle - make another task scheduler for this 
  dates0 <- "all"
}
var <- "bt"

survey_definition_id0 <- c(47, 98, 143) 

dir_googledrive_log <- "https://docs.google.com/spreadsheets/d/1ymH0K3d9SgfrhmP9PQI7y4PqInKFal2phKjot92xr1U"
dir_googledrive_upload_bs = 
  ifelse(istest, 
         "https://drive.google.com/drive/folders/1TjNRlpRUF-Nzx2LsZdWWFluMb8iUCRth",
         "https://drive.google.com/drive/folders/1H4kfU4xd3_vWggxrLwnImE6ikg4ub0a7")
dir_googledrive_upload_goa = 
  ifelse(istest, 
         "https://drive.google.com/drive/folders/1WXmQXmUjNjzQm915GSyrKyrZVrRfsBfT", 
         "https://drive.google.com/drive/folders/10m709GkyPTofj37a3afQtZQGrzacXwb9")

# SIGN INTO GOOGLE DRIVE--------------------------------------------------------

googledrive_dl <- TRUE
googledrive::drive_deauth()
googledrive::drive_auth()
2

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------
source(file = paste0(dir_wd,"code/functions.R"))
# source(file = paste0(dir_wd, "code/data_dl.R")) # you don't unnecessarily run this each time
source(file = paste0(dir_wd, "code/data.R"))

# UPDATE README (sometimes) ----------------------------------------------------

# rmarkdown::render(here::here("code/README.Rmd"),
#                   output_dir = "./",
#                   output_file = here::here("README.md"))

# Map --------------------------------------------------------------------------

## NBS + EBS Maps --------------------------------------------------------------

if ("NBS" %in% dat_survey$srvy & "EBS" %in% dat_survey$srvy) {
  
  srvy <- "BS"
  plot_subtitle <- "NOAA Fisheries Bering Sea Bottom Trawl Survey"
  dir_googledrive_upload <- ifelse(exists("dir_googledrive_upload_bs") & googledrive_dl, dir_googledrive_upload_bs, NULL)
  show_planned_stations <- TRUE
  plot_anom <- TRUE
  
  make_varplot_wrapper(maxyr = maxyr,
                       srvy = srvy,
                       dat_survey = dat_survey,
                       var = var,
                       dir_googledrive_upload = dir_googledrive_upload,
                       dates0 = dates0,
                       shp = shp,
                       plot_subtitle = plot_subtitle,
                       show_planned_stations = show_planned_stations,
                       data_source = data_source,
                       file_end0 = c("daily", "grid", "mean"), # c("daily", "anom"),, "anom"
                       dir_wd = dir_wd
  )
}
