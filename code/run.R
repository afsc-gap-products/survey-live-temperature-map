#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: run script
#' ---------------------------


# dir_in<-"C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
dir_in <- "G:/EBSother/GAPsurveyTemperatureMap/"
# dir_in<-"C:/Users/emily.markowitz/Work/Projects/GAPSurveyTemperatureMap/"


source(file = paste0(dir_in,"code/functions.R"))

# Load data --------------------------------------------------------------------

# *** Static Data --------------------------------------------------------------

dat_vess <- data.frame(var = c("v", "V", "a", "A"), 
                       vess = c("F/V Vesteraalen", "F/V Vesteraalen", 
                                   "F/V Alaska Knight", "F/V Alaska Knight"))

dat_survreg0 <- data.frame(reg_shapefile = c("EBS_SHELF", "NBS_SHELF"), 
                           region_long = c("Eastern Bering Sea", "Northern Bering Sea"), 
                          region = c("EBS", "NBS"))


# *** Oracle -------------------------------------------------------------------

if (FALSE) { # so you don't unnecessarially run this each time

  source(file = paste0(dir_in, "code/data_dl.R"))

}

# *** Google Drive ------------------------------------------------------------

# if (FALSE) { # so you don't unnecessarially run this each time
  
  drive_deauth()
  drive_auth()
  1
  
  # Load data from Google Sheet
  # https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
  drive_download(as_id("16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE"), #"heatLog.csv",
                 type = "csv",
                 overwrite = T, 
                 path = paste0(dir_in, "data/heatLog.csv"))
# }

# Map Prep ---------------------------------------------------------------------

  
# *** Log into Google Drive ----------------------------------------------------
  
drive_deauth()
drive_auth()
1

# How to set up the task scheduler: 
# https://docs.google.com/document/d/1pwBmR6AqgnvUx_AiWYQxtYxIRjWMfdd5EPWwFvpI3Ug/edit

# Where the files will be saved to: 
# https://docs.google.com/document/d/1pwBmR6AqgnvUx_AiWYQxtYxIRjWMfdd5EPWwFvpI3Ug/edit

# *** General Variables to Change Annually -------------------------------------

yr <- 2021 #CHANGE

anom_firstyr_nbs<-2010
anom_firstyr_ebs<-1987
anom_lastyr <- 2019

dir_out <-  paste0(dir_in, "results/", yr)

plot_subtitle <- "NOAA Fisheries Eastern Bering Sea Bottom Trawl Survey"

# Replace "bs.south" (EBS) with "bs.all" (EBS+NBS). See the "select.region" argument under '?get_base_layers'
region_akgfmaps <- "bs.all" #CHANGE

# Repalced shapefile "EBSgrid" with "NEBSgrid_df". Latter includes NBS+EBS grids
region_grid <- "NEBSgrid" #CHANGE

# What are your survey dates and for what regions?
dat_survreg <- dat_survreg0 %>%
  dplyr::left_join(x = ., 
                   y = data.frame(reg_dates = c("\n(May 25-Aug 04)", # CHANGE
                                                "\n(Aug 02-Aug 28)"), # CHANGE
                                  region = c("EBS", "NBS")), 
                   by = "region")

# *** Create Analysis-Specific Data --------------------------------------------

heatLog0 <- read_csv(paste0(dir_in, "data/heatLog.csv"))
heatLog0 <- heatLog0[!(is.na(heatLog0$region)),]

heatLog <- heatLog0 %>%
  # Select data for this year
  dplyr::select(region, station, paste0(yr, "_bt"), paste0(yr, "_date")) %>%
  dplyr::rename("var" = paste0(yr, "_bt"),
                "date" = paste0(yr, "_date")) %>%
  # add survey region data and planned survey dates
  dplyr::left_join(x = ., y = dat_survreg, by = "region") %>%
  dplyr::mutate(reg_lab = paste0(region_long, " ", reg_dates)) %>%
  dplyr::arrange((paste0(yr, "_bt")))

# add survey vessel data
if (length(grep(x = heatLog$var, pattern = "[A-Za-z]"))>0) {
  heatLog <- heatLog %>%
    dplyr::left_join(x = ., y = dat_vess, by = "var")
}

dat_nbs <- read_csv(file =
                      paste0(dir_in, "data/",
                             paste0("dat_nbs_",
                                    anom_firstyr_nbs,"-",
                                    anom_lastyr, ".csv")))

dat_ebs <- read_csv(file =
                      paste0(dir_in, "data/",
                             paste0("dat_ebs_",
                                    anom_firstyr_ebs,"-",
                                    anom_lastyr, ".csv")))

anom <- anom_create(
  dat_nbs = dat_nbs,
  dat_ebs = dat_ebs,
  yr_first = anom_firstyr_ebs,
  yr_last = yr-1,
  var = "GEAR_TEMPERATURE", # so you can also do SST if you need...
  save = TRUE)

# Run maps ---------------------------------------------------------------------

# *** Blank Grid (no survey data) ----------------------------------------------

# Just the empty grid (comment this v out when running after beginning of survey)
create_vargridplots(yr = yr, 
                    anom = NULL, 
                    heatLog = heatLog, 
                    plot_title = "Survey Grid",
                    plot_subtitle = plot_subtitle,
                    dates = "none", # latest # "all", #"2021-06-05", 
                    region_akgfmaps = region_akgfmaps, 
                    region_grid = region_grid, 
                    file_end = "grid",
                    gif = FALSE, 
                    dir_in = dir_in, 
                    dir_out = dir_out)


# *** During the Survey --------------------------------------------------------

# The bottom temperatures for this "yr"
create_vargridplots(yr = yr,
               anom = NULL,
               heatLog = heatLog,
               plot_title = paste0(yr, ' Bottom Temperature (\u00B0C)'),
               plot_subtitle = plot_subtitle,
               legend_temp = 'Bottom\nTemperature (\u00B0C)',
               dates = "latest", # "all", #"2021-06-05",
               region_akgfmaps = region_akgfmaps,
               region_grid = region_grid,
               file_end = "daily",
               dir_in = dir_in, 
               dir_out = dir_out)


# The bottom temperature anomaly between this year and past years
create_vargridplots(yr = yr,
               anom = anom,
               heatLog = heatLog,
               plot_title = paste0(yr, ' Bottom Temperature Anomaly\n(EBS: ',
                                   ifelse(length(unique(dat_ebs$year))>3,
                                          paste(range(as.numeric(unique(dat_ebs$year))),
                                                collapse = "-"),
                                          text_list(x = unique(dat_ebs$year))),
                                   '; NBS: ',
                                   ifelse(length(unique(dat_nbs$year))>3,
                                          paste(range(as.numeric(unique(dat_nbs$year))),
                                                collapse = "-"),
                                          text_list(x = unique(dat_nbs$year))),
                                   ')'),
               plot_subtitle = plot_subtitle,
               legend_temp = 'Bottom Temperature\nAnomaly (\u00B0C)',
               dates = "all", # "all", #"2021-06-05",
               region_akgfmaps = region_akgfmaps,
               region_grid = region_grid,
               file_end = "anom",
               dir_in = dir_in, 
               dir_out = dir_out, 
               planned_stations = FALSE)

# *** Previous years --------------------------------------------------
#
# yr<-2019
#
# heatLog <- heatLog0 %>%
#   # Select data for this year
#   dplyr::select(region, station, paste0(yr, "_bt"), paste0(yr, "_date")) %>%
#   dplyr::rename("var" = paste0(yr, "_bt"),
#                 "date" = paste0(yr, "_date")) %>%
#   # add survey region data and planned survey dates
#   dplyr::left_join(x = ., y = dat_survreg, by = "region") %>%
#   dplyr::mutate(reg_lab = paste0(region_long, " ", reg_dates))
#
# # add survey vessel data
# if (length(grep(x = heatLog$var, pattern = "[A-Za-z]"))>0) {
#   heatLog <- heatLog %>%
#     dplyr::left_join(x = ., y = dat_vess, by = "var")
# }
#
# anom <- anom_create(
#   dat_nbs = dat_nbs,
#   dat_ebs = dat_ebs,
#   yr_first = anom_firstyr_ebs,
#   yr_last = yr-1,
#   var = "GEAR_TEMPERATURE",
#   save = TRUE)
#
# # The bottom temperatures for this "yr"
# create_vargridplots(yr = yr,
#                anom = NULL,
#                heatLog = heatLog,
#                plot_title = paste0(yr, ' Survey Bottom Temperature (\u00B0C)'),
#                plot_subtitle = plot_subtitle,
#                legend_temp = 'Bottom\nTemperature (\u00B0C)',
#                dates = "latest", # "all", #"2021-06-05",
#                region_akgfmaps = region_akgfmaps,
#                region_grid = region_grid,
#                file_end = "daily",
# dir_in = dir_in, 
# dir_out = paste0(dir_in, "results/", yr))
#
# # The bottom temperature anomaly between this year and past years
# create_vargridplots(yr = yr,
#                anom = anom,
#                heatLog = heatLog,
#                plot_title = paste0(yr, ' Bottom Temperature Anomaly\n(EBS: ',
#                                    ifelse(length(unique(dat_ebs$year))>3,
#                                           paste(range(as.numeric(unique(dat_ebs$year))),
#                                                 collapse = "-"),
#                                           text_list(x = unique(dat_ebs$year))),
#                                    '; NBS: ',
#                                    ifelse(length(unique(dat_nbs$year))>3,
#                                           paste(range(as.numeric(unique(dat_nbs$year))),
#                                                 collapse = "-"),
#                                           text_list(x = unique(dat_nbs$year))),
#                                    ')'),
#                plot_subtitle = plot_subtitle,
#                legend_temp = 'Bottom Temperature\nAnomaly (\u00B0C)',
#                dates = "latest", # "all", #"2021-06-05",
#                region_akgfmaps = region_akgfmaps,
#                region_grid = region_grid,
#                file_end = "anom",
# dir_in = dir_in, 
# dir_out = paste0(dir_in, "results/", yr))
#

