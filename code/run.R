#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: run script
#' ---------------------------

source(file = here::here("code", "functions.R"))

# Load data --------------------------------------------------------------------

# *** Static Data --------------------------------------------------------------

dat_vess <- data.frame(var = c("v", "V", "a", "A"), 
                       vess = c("F/V Vesteraalen", "F/V Vesteraalen", 
                                   "F/V Alaska Knight", "F/V Alaska Knight"))

# *** Oracle -------------------------------------------------------------------

if (FALSE) { # so you don't unnecessarially run this each time

  source(file = here::here("code", "data_dl.R"))

}

# *** google drive ------------------------------------------------------------

if (FALSE) { # so you don't unnecessarially run this each time
  
  drive_deauth()
  drive_auth()
  1
  
  # Load data from Google Sheet
  # https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
  drive_download(as_id("16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE"), #"heatLog.csv",
                 type = "csv",
                 overwrite = T)
}

# Run maps ---------------------------------------------------------------------

drive_deauth()
drive_auth()
1


# *** During the Survey --------------------------------------------------------

# What year are these temperatures from?
yr <- 2021 #CHANGE
EBS_proposed_dates<-"(May 25-Aug 04)"
NBS_proposed_dates<-"(Aug 02-Aug 28)"

# Replace "bs.south" (EBS) with "bs.all" (EBS+NBS). See the "select.region" argument under '?get_base_layers'
region_akgfmaps <- "bs.all" #CHANGE

# Repalced shapefile "EBSgrid" with "NEBSgrid_df". Latter includes NBS+EBS grids
region_grid <- "NEBSgrid" #CHANGE

heatLog0 <- read_csv(here::here("data", "heatLog.csv"))
heatLog0 <- heatLog0[!(is.na(heatLog0$region)),]

heatLog <- heatLog0 %>%
  dplyr::select(region, station, paste0(yr, "_bt"), paste0(yr, "_date")) %>%
                  dplyr::rename("var" = paste0(yr, "_bt"), 
                                "date" = paste0(yr, "_date")) 
if (length(grep(x = heatLog$var, pattern = "[A-Za-z]"))>0) {
  heatLog <- heatLog %>%
    dplyr::left_join(x = ., y = dat_vess, by = "var") 
} 

anom_firstyr_nbs<-2010
anom_firstyr_ebs<-1987
anom_lastyr <- 2019

dat_nbs <- read_csv(file = 
                      here::here("data", 
                                 paste0("dat_nbs_",
                                        anom_firstyr_nbs,"-", 
                                        anom_lastyr, ".csv")))

dat_ebs <- read_csv(file = 
                      here::here("data", 
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


# The bottom temperatures for this "yr"
create_vargridplots(yr = yr, 
               anom = NULL, 
               heatLog = heatLog, 
               plot_title = paste0(yr, ' Bottom Temperature (°C)'),
               legend_temp = 'Bottom\nTemperature (°C)',
               dates = "latest", # "all", #"2021-06-05", 
               EBS_proposed_dates = EBS_proposed_dates,
               NBS_proposed_dates = NBS_proposed_dates,
               region_akgfmaps = region_akgfmaps, 
               region_grid = region_grid, 
               file_end = "daily",
               dir_out = here::here("results", yr, "daily") )


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
               legend_temp = 'Bottom Temperature\nAnomaly (°C)',
               dates = "latest", # "all", #"2021-06-05", 
               EBS_proposed_dates = EBS_proposed_dates,
               NBS_proposed_dates = NBS_proposed_dates,
               region_akgfmaps = region_akgfmaps, 
               region_grid = region_grid, 
               file_end = "anom",
               dir_out = here::here("results", yr, "anomalies"))

# *** Previous years --------------------------------------------------

yr<-2019

heatLog <- heatLog0 %>%
  dplyr::select(region, station, paste0(yr, "_bt"), paste0(yr, "_date")) %>%
  dplyr::rename("var" = paste0(yr, "_bt"), 
                "date" = paste0(yr, "_date")) 
if (length(grep(x = heatLog$var, pattern = "[A-Za-z]"))>0) {
  heatLog <- heatLog %>%
    dplyr::left_join(x = ., y = dat_vess, by = "var") 
} 
         
anom <- anom_create(
  dat_nbs = dat_nbs, 
  dat_ebs = dat_ebs, 
  yr_first = anom_firstyr_ebs, 
  yr_last = yr-1, 
  var = "GEAR_TEMPERATURE",
  save = TRUE)

# The bottom temperatures for this "yr"
create_vargridplots(yr = yr, 
               anom = NULL, 
               heatLog = heatLog, 
               plot_title = paste0(yr, ' Survey Bottom Temperature (°C)'),
               legend_temp = 'Bottom\nTemperature (°C)',
               dates = "latest", # "all", #"2021-06-05", 
               EBS_proposed_dates = EBS_proposed_dates,
               NBS_proposed_dates = NBS_proposed_dates,
               region_akgfmaps = region_akgfmaps, 
               region_grid = region_grid, 
               file_end = "daily",
               dir_out = here::here("results", yr, "daily") )

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
               legend_temp = 'Bottom Temperature\nAnomaly (°C)',
               dates = "latest", # "all", #"2021-06-05",
               EBS_proposed_dates = EBS_proposed_dates,
               NBS_proposed_dates = NBS_proposed_dates,
               region_akgfmaps = region_akgfmaps, 
               region_grid = region_grid, 
               file_end = "anom",
               dir_out = here::here("results", yr, "anomalies"))


