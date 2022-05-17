#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (Jan 2022)
#' purpose: run script
#' ---------------------------

# Knowns -----------------------------------------------------------------------

maxyr <- yr <- 2022 #CHANGE
googledrive_dl <- TRUE
dir_googledrive_log <- "16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE"
dir_googledrive_upload_nebs = "1vWza36Dog0SpZLcTN22wD-iCEn6ooGCM" # https://drive.google.com/drive/folders/1vWza36Dog0SpZLcTN22wD-iCEn6ooGCM?usp=sharing
dir_googledrive_upload_ai = "1SeNOAh5-muQ2BDgOHWZWwYIoLl68DHWX" # https://drive.google.com/drive/folders/1SeNOAh5-muQ2BDgOHWZWwYIoLl68DHWX?usp=sharing

# The surveys this script will be covering 
dat_survreg <- data.frame(reg_shapefile = "EBS_SHELF", 
                          region_long = "Eastern Bering Sea", 
                          SRVY = "EBS", 
                          region = "BS", 
                          vessel_id = c(94, 162), # CHANGE
                          vessel_shape = c("V", "A"), # CHANGE
                          reg_dates = "\n(May 25-Aug 04)") # CHANGE
dat_survreg <- dplyr::bind_rows(dat_survreg, 
                               data.frame(reg_shapefile = "NBS_SHELF", 
                                          region_long = "Northern Bering Sea", 
                                          SRVY = "NBS", 
                                          region = "BS", 
                                          vessel_id = c(94, 162), # CHANGE
                                          vessel_shape = c("V", "A"), # CHANGE
                                          reg_dates = "\n(Aug 02-Aug 28)")) # CHANGE
dat_survreg <- dplyr::bind_rows(dat_survreg, 
                                data.frame(reg_shapefile = "AI",
                                          region_long = "Aleutian Islands",
                                          SRVY = "AI",
                                          region = "AI",
                                          vessel_id = c(148, 176), # CHANGE
                                          vessel_shape = c("OEX", "SS"), # CHANGE
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

# googledrive::drive_deauth()
# googledrive::drive_auth()
# 1

# SOURCE SUPPORT SCRIPTS -------------------------------------------------------

# dir_in<-"C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/"
# dir_in <- "G:/EBSother/GAPsurveyTemperatureMap/"
# dir_in<-"C:/Users/emily.markowitz/Work/Projects/GAPSurveyTemperatureMap/"
library(here)
dir_in<- here::here() #paste0(getwd(), "/")

# source(file = paste0(dir_in,"code/functions.R"))
# source(file = paste0(dir_in, "code/data_dl.R")) # you don't unnecessarially run this each time
# source(file = paste0(dir_in, "code/data.R"))

source(here::here("code", "functions.R"))
# source(here::here("code", "data_dl.R"))
source(here::here("code", "data.R"))

# Make Map ---------------------------------------------------------------------

## NBS + EBS -------------------------------------------------------------------
var = "bt"
maxyr <- yr <- 2022 #CHANGE
SRVY <- "BS"
dir_googledrive_upload <- dir_googledrive_upload_nebs
dates0 <- "2022-06-01"
planned_stations <- TRUE
grid_stations <- rgdal::readOGR(dsn = paste0(dir_in, '/shapefiles'),# Prepare map objects
                                 layer = "NEBSgrid", 
                                 verbose=F) 
grid_stations <- spTransform(grid_stations,
                              "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") # crs(survey_area))
grid_stations<-sf::st_as_sf(grid_stations) %>%
    dplyr::filter(!(STATION %in% c("DD-09", "AA-10"))) %>% 
    dplyr::rename(station = STATION_ID)
grid_stations <- sp::merge(x = grid_stations, 
                           y = haul %>%
                             dplyr::rename(station = stationid) %>% 
                             dplyr::select(station, stratum) %>% 
                             dplyr::distinct(), 
                           all.x = TRUE) 
region_akgfmaps = "bs.all"
plot_subtitle = "NOAA Fisheries Bering Sea Bottom Trawl Survey"

## AI --------------------------------------------------------------------------
# var = "bt"
# maxyr <- yr <- 2018 #CHANGE
# SRVY <- "AI"
# dir_googledrive_upload <- dir_googledrive_upload_ai
# dates0 <- "2018-06-10"
# planned_stations <- FALSE
# extrap.box <- c(xmn = -179.5, xmx = -130, ymn = 54, ymx = 60)
# grid_stations <- rgdal::readOGR(dsn = paste0(dir_in, '/shapefiles'),# Prepare map objects
#                                 layer = "aigrid_trawable_thru2018", 
#                                 verbose=F) 
# grid_stations <- spTransform(grid_stations,
#                              "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") # crs(survey_area))
#grid_stations<-st_as_sf(grid_stations) %>% 
#  dplyr::rename(station = ID, 
#                stratum = STRATUM)
#grid_stations <- sp::merge(x = grid_stations, 
#                           y = goa_strata0 %>%
#                             dplyr::mutate(regulatory_area_name = stringr::str_to_title(regulatory_area_name), 
#                                           regulatory_area_name = gsub(pattern = "Goa", replacement = "GOA", x = regulatory_area_name)) %>%
#                             dplyr::select(stratum, regulatory_area_name) %>% 
#                             dplyr::distinct(), 
#                           all.x = TRUE)
# grid_stations$station <- grid_stations$ID
# grid_stations$STATION_ID <- grid_stations$station
# grid_stations$stratum <- grid_stations$STRATUM
#region_akgfmaps = "ai"
#plot_subtitle = "NOAA Fisheries Aluetian Islands Bottom Trawl Survey"

# Funct ------------------------------------------------------------------------

make_plot_warpper <- function(maxyr, 
                              SRVY, 
                              report_types, 
                              haul, 
                              var = "bt",
                              dir_googledrive_upload, 
                              dates0) {
  
  # a <- report_types[names(report_types) == SRVY][[1]]
  # for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }
  
  # dir_out <-  paste0(here::here("results", yr, "_", SRVY), yr, "_", SRVY)
  dir_out <- here::here("results", paste0(yr, "_", SRVY))
  
  case <- paste0(maxyr, "_", SRVY)
  
  heatLog <- 
    readxl::read_xlsx(path = here::here("data/gap_survey_progression.xlsx"), #paste0(dir_in, ), 
                      sheet = case, skip = 1) %>%
    janitor::clean_names() %>% 
    dplyr::rename("var" = all_of(var), 
                  "SRVY" = "srvy", 
                  "vessel_shape" = "vessel") %>%
    dplyr::filter(!is.na(SRVY))  
    
  
  if (sum(names(heatLog) %in% "stratum")==0) { # for BS
    heatLog <- heatLog %>% 
      dplyr::left_join(x = ., 
                     y = haul %>% 
                       dplyr::filter(region == SRVY) %>% 
                       dplyr::select(stationid, stratum) %>% 
                       dplyr::distinct(), 
                     by = c("station" = "stationid"))
  }
  
  heatLog <- heatLog %>% # remove rows of empty data
    # Select data for this year
    dplyr::select(SRVY, stratum, station, var, date, vessel_shape) %>% 
    # add survey region data and planned survey dates
    dplyr::left_join(x = ., 
                     y = dat_survreg, 
                     by = c("SRVY", "vessel_shape")) %>%
    dplyr::mutate(reg_lab = paste0(region_long, " ", reg_dates)) %>%
    dplyr::arrange("var") %>%
    dplyr::left_join(x = ., 
                     y = vessel_info, 
                     by = c("vessel_id")) # add survey vessel data
  
  anom <- anom_create(
    dat = haul[haul$SRVY %in% ifelse(SRVY == "BS", c("EBS", "NBS"), SRVY), ],
    # yr_first = anom_firstyr_ebs,
    # yr_last = yr-1,
    var = "gear_temperature", #"GEAR_TEMPERATURE", # so you can also do SST if you need...
    save = FALSE)
  
  # Daily plot
  plot_title = paste0(yr, ' Bottom Temperature (\u00B0C)')
  legend_temp = 'Bottom\nTemperature (\u00B0C)'
  anom = NULL
  file_end = "daily"
  
  create_vargridplots(yr = yr,
                      anom = anom,
                      heatLog = heatLog,
                      plot_title = plot_title,
                      plot_subtitle = plot_subtitle,
                      legend_temp = legend_temp,
                      dates0 = dates0, #"latest", # "all", #"2021-06-05",
                      region_akgfmaps = region_akgfmaps,
                      grid_stations = grid_stations,
                      file_end = file_end,
                      dir_in = dir_in,
                      dir_out = dir_out, 
                      dir_googledrive_upload = dir_googledrive_upload, 
                      gif = TRUE, 
                      planned_stations = planned_stations)
  
  # create_vargridplots(yr = yr,
  #                     anom = anom,
  #                     heatLog = heatLog,
  #                     plot_title = paste0(yr, ' Bottom Temperature (\u00B0C)'),
  #                     plot_subtitle = plot_subtitle,
  #                     legend_temp = 'Bottom\nTemperature (\u00B0C)',
  #                     dates0 = dates0, #"latest", # "all", #"2021-06-05",
  #                     region_akgfmaps = region_akgfmaps,
  #                     shapefile = shapefile,
  #                     file_end = "anom",
  #                     dir_in = dir_in,
  #                     dir_out = dir_out, 
  #                     dir_googledrive_upload = dir_googledrive_upload)  
  
}
























 # Replace "bs.south" (EBS) with "bs.all" (EBS+NBS). See the "select.region" argument under '?get_base_layers'
# region_grid <- "NEBSgrid" # Repalced shapefile "EBSgrid" with "NEBSgrid_df". Latter includes NBS+EBS grids


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
  readxl::read_xlsx(path = here::here("data/gap_survey_progression.xlsx"), #paste0(dir_in, "data/gap_survey_progression.xlsx"), 
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
                             paste0("dat_ebs_1982-2021.csv")))

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

