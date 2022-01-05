#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (Jan 2022)
#' purpose: run script
#' ---------------------------

# dir_in<-"C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
#dir_in <- "G:/EBSother/GAPsurveyTemperatureMap/"
# dir_in<-"C:/Users/emily.markowitz/Work/Projects/GAPSurveyTemperatureMap/"
dir_in<-paste0(getwd(), "/")

source(file = paste0(dir_in,"code/functions.R"))

# Load data --------------------------------------------------------------------

# *** Oracle -------------------------------------------------------------------

if (FALSE) { # so you don't unnecessarially run this each time
  source(file = paste0(dir_in, "code/data_dl.R"))
}

source(file = paste0(dir_in, "code/data.R"))

# *** Google Drive ------------------------------------------------------------
  
drive_deauth()
drive_auth()
1
  
# if (FALSE) { # so you don't unnecessarially run this each time

  
  # Load data from Google Sheet
  # https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
googledrive::drive_download(file = googledrive::as_id("16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE"),  #"gap_survey_progression.csv",
                              type = "xlsx", 
                              overwrite = TRUE, 
                              path = paste0(dir_in, "data/gap_survey_progression.xlsx"))
  
                              
# }

# Make Map ---------------------------------------------------------------------

# *** General Variables to Change Annually -------------------------------------

  yr <- 2022 #CHANGE

# NBS + EBS --------------------------------------------------------------------------
SRVY <- "NEBS"
plot_subtitle <- "NOAA Fisheries Bering Sea Bottom Trawl Survey"
region_akgfmaps <- "bs.all" # Replace "bs.south" (EBS) with "bs.all" (EBS+NBS). See the "select.region" argument under '?get_base_layers'
region_grid <- "NEBSgrid" # Repalced shapefile "EBSgrid" with "NEBSgrid_df". Latter includes NBS+EBS grids

dir_out <-  paste0(dir_in, "results/", yr, "_", SRVY)

# What are your survey dates and for what regions?
# dat_survreg <- dat_survreg0 %>%
#   dplyr::left_join(x = ., 
#                    y = data.frame(reg_dates = c("\n(May 25-Aug 04)", # CHANGE
#                                                 "\n(Aug 02-Aug 28)"), # CHANGE
#                                   region = c("EBS", "NBS")), 
#                    by = "region")

# *** Create Analysis-Specific Data --------------------------------------------

yr <- 2022
reg <- "BS"
case <- paste0(yr, "_", reg)

gap_survey_progression <- 
  readxl::read_xlsx(path = paste0(dir_in, "data/gap_survey_progression.xlsx"), 
                    sheet = case, skip = 1) %>%
  dplyr::filter(!is.na(region)) %>% # remove rows of empty data
  # Select data for this year
  dplyr::select(region, station, bt, date, vessel) %>%
  dplyr::rename("var" = "bt") %>%
  # add survey region data and planned survey dates
  dplyr::left_join(x = ., 
                   y = dat_survreg, 
                   by = "region") %>%
  dplyr::mutate(reg_lab = paste0(region_long, " ", reg_dates), 
                vessel_shape = vessel) %>%
  dplyr::arrange("var") %>%
  dplyr::left_join(x = ., 
                   y = vessel_info %>% 
                     dplyr::filter(year == yr), 
                   by = c("region", "vessel_shape")) # add survey vessel data

dat_nbs <- read_csv(file =
                      paste0(dir_in, "data/",
                             paste0("dat_nbs_2010-2021.csv")))

dat_ebs <- read_csv(file =
                      paste0(dir_in, "data/",
                             paste0("dat_ebs_1984-2021.csv")))

anom <- anom_create(
  dat_nbs = list(dat_nbs, dat_ebs),
  yr_first = anom_firstyr_ebs,
  yr_last = yr-1,
  var = "GEAR_TEMPERATURE", # so you can also do SST if you need...
  save = TRUE)

# Run maps ---------------------------------------------------------------------

# *** Blank Grid (no survey data) ----------------------------------------------

# Just the empty grid (comment this v out when running after beginning of survey)
# create_vargridplots(yr = yr, 
#                     anom = NULL, 
#                     gap_survey_progression = gap_survey_progression, 
#                     plot_title = "Survey Grid",
#                     plot_subtitle = plot_subtitle,
#                     dates = "none", # latest # "all", #"2021-06-05", 
#                     region_akgfmaps = region_akgfmaps, 
#                     region_grid = region_grid, 
#                     file_end = "grid",
#                     gif = FALSE, 
#                     dir_in = dir_in, 
#                     dir_out = dir_out)


# *** During the Survey --------------------------------------------------------

# The bottom temperatures for this "yr"
create_vargridplots(yr = yr,
               anom = NULL,
               gap_survey_progression = gap_survey_progression,
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
               gap_survey_progression = gap_survey_progression,
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
               dates = "latest", # "all", #"2021-06-05", #change to "latest" to only run current day's anom plot
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
# gap_survey_progression <- gap_survey_progression0 %>%
#   # Select data for this year
#   dplyr::select(region, station, paste0(yr, "_bt"), paste0(yr, "_date")) %>%
#   dplyr::rename("var" = paste0(yr, "_bt"),
#                 "date" = paste0(yr, "_date")) %>%
#   # add survey region data and planned survey dates
#   dplyr::left_join(x = ., y = dat_survreg, by = "region") %>%
#   dplyr::mutate(reg_lab = paste0(region_long, " ", reg_dates))
#
# # add survey vessel data
# if (length(grep(x = gap_survey_progression$var, pattern = "[A-Za-z]"))>0) {
#   gap_survey_progression <- gap_survey_progression %>%
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
#                gap_survey_progression = gap_survey_progression,
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
#                gap_survey_progression = gap_survey_progression,
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

