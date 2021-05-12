#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: run script
#' ---------------------------

# Configuration ----------------------------------------------------------------

# sessionInfo()

# R version 4.0.2 (2020-06-22)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] mapproj_1.2.7      readr_1.4.0        broom_0.7.6        janitor_2.1.0      bindrcpp_0.2.2     googledrive_1.0.1 
# [7] viridis_0.6.0      viridisLite_0.4.0  rgeos_0.5-5        maps_3.3.0         akgfmaps_1.5.1     stars_0.5-2       
# [13] abind_1.4-5        shadowtext_0.0.7   sf_0.9-8           reshape_0.8.8      rgdal_1.5-23       RColorBrewer_1.1-2
# [19] raster_3.4-5       sp_1.4-5           plyr_1.8.6         magrittr_2.0.1     gstat_2.0-7        ggspatial_1.1.5   
# [25] ggplot2_3.3.3      dplyr_1.0.5        classInt_0.4-3    
# 
# loaded via a namespace (and not attached):
#   [1] httr_1.4.2         tidyr_1.1.3        jsonlite_1.7.2     assertthat_0.2.1   askpass_1.1        yaml_2.2.1        
# [7] pillar_1.6.0       backports_1.2.1    lattice_0.20-41    glue_1.4.2         digest_0.6.27      snakecase_0.11.0  
# [13] colorspace_2.0-0   pkgconfig_2.0.3    purrr_0.3.4        scales_1.1.1       intervals_0.15.2   openssl_1.4.3     
# [19] tibble_3.1.0       proxy_0.4-25       farver_2.1.0       generics_0.1.0     ellipsis_0.3.1     withr_2.4.2       
# [25] cli_2.4.0          crayon_1.4.1       fs_1.5.0           fansi_0.4.2        xts_0.12.1         lwgeom_0.2-6      
# [31] class_7.3-17       FNN_1.1.3          tools_4.0.2        hms_1.0.0          gargle_1.1.0       lifecycle_1.0.0   
# [37] stringr_1.4.0      munsell_0.5.0      compiler_4.0.2     e1071_1.7-6        spacetime_1.2-4    rlang_0.4.10      
# [43] units_0.7-1        grid_4.0.2         rstudioapi_0.13    rappdirs_0.3.3     gtable_0.3.0       codetools_0.2-16  
# [49] DBI_1.1.1          curl_4.3           R6_2.5.0           gridExtra_2.3      zoo_1.8-9          lubridate_1.7.10  
# [55] utf8_1.2.1         bindr_0.1.1        KernSmooth_2.23-17 stringi_1.5.3      parallel_4.0.2     Rcpp_1.0.6        
# [61] vctrs_0.3.7        tidyselect_1.1.0
# setwd('G:/EBSother/EBS2021/HeatMapR/') # Not strictly necessary with the R Project architecture

# Install libraries ------------------------------------------------------------

### Requirements
# You need these packages:  
PKG <- c(
  # Mapping
  "akgfmaps", # devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
  "rgdal", 
  "maps", 
  "sp", 
  "raster", 
  "rgeos",
  
  # color managment (color blind friendly!)
  # "viridis", 
  "colorRamps",
  
  # google drive
  "googledrive", # library(googlesheets)
  
  # data management
  "bindrcpp", 
  "janitor", 
  
  # tidyverse, 
  "broom", 
  "readr", 
  "dplyr"
)

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}


# Load data --------------------------------------------------------------------

dat_vess <- data.frame(var = c("v", "V", "a", "A"), 
                       vess = c("F/V Vesteraalen", "F/V Vesteraalen", 
                                   "F/V Alaska Knight", "F/V Alaska Knight"))

# *** Oracle -------------------------------------------------------------------

if (FALSE) { # so you don't unnecessarially run this each time

  source(file = here::here("code", "data_dl.R"))
# }

# *** google drive ------------------------------------------------------------

drive_deauth()
drive_auth()
1

# Load data from Google Sheet
# https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
drive_download("16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE", #"heatLog.csv",
               type = "csv",
               overwrite = T)

}

# Run maps ---------------------------------------------------------------------

# *** During the Survey --------------------------------------------------------

# What year are these temperatures from?
yr <- 2021 #CHANGE

# Replace "bs.south" (EBS) with "bs.all" (EBS+NBS). See the "select.region" argument under '?get_base_layers'
region_akgfmaps <- "bs.all" #CHANGE

# Repalced shapefile "EBSgrid" with "NEBSgrid_df". Latter includes NBS+EBS grids
region_grid <- "NEBSgrid" #CHANGE

heatLog0 <- read_csv(here::here("data", "heatLog.csv"))
heatLog0 <- heatLog0[!(is.na(heatLog0$region)),]

heatLog <- heatLog0 %>%
  dplyr::select(region, station, paste0(yr, "_bt"), paste0(yr, "_date")) %>%
                  dplyr::rename("var" = paste0(yr, "_bt"), 
                                "date" = paste0(yr, "_date")) %>%
  dplyr::left_join(x = ., y = dat_vess, by = "var")

anom_firstyr_nbs<-2010
anom_firstyr<-1987
anom_lastyr<-2019

dat_nbs <- read_csv(file = 
                      here::here("data", 
                                 paste0("dat_nbs_",
                                        anom_firstyr_nbs,"-", 
                                        anom_lastyr, ".csv")))

dat_ebs <- read_csv(file = 
                      here::here("data", 
                                 paste0("dat_ebs_",
                                        anom_firstyr,"-", 
                                        anom_lastyr, ".csv")))

anom <- anom_create(
  dat_nbs = dat_nbs, 
  dat_ebs = dat_ebs, 
  yr_first = anom_firstyr, 
  yr_last = anom_lastyr, 
  var = "GEAR_TEMPERATURE", # so you can also do SST if you need...
  save = TRUE)


# The bottom temperatures for this "yr"
create_BTplots(yr = yr, 
               anom = NULL, 
               heatLog = heatLog, 
               plot_title = paste0(yr, ' Bottom Temperature (°C)'),
               legend_temp = 'Bottom\nTemperature (°C)',
               dates = "latest", # "all", #"2021-06-05", 
               region_akgfmaps = region_akgfmaps, 
               region_grid = region_grid, 
               file_end = "daily",
               dir_out = here::here("results", yr, "daily") )


# The bottom temperature anomaly between this year and past years
create_BTplots(yr = yr, 
               anom = anom, 
               heatLog = heatLog, 
               plot_title = paste0(yr, ' Bottom Temperature Anomaly\n(EBS: ',
                                   anom_firstyr,'-',anom_lastyr,'; NBS: ',
                                   ifelse(length(unique(substr(x = dat_nbs$CRUISE, 1,4)))>3,
                                          paste(range(as.numeric(unique(substr(
                                            x = dat_nbs$CRUISE, 1,4)))), collapse = "-"),
                                          text_list(x = unique(substr(
                                            x = dat_nbs$CRUISE, 1,4)))),
                                   ')'),
               legend_temp = 'Bottom Temperature\nAnomaly (°C)',
               dates = "latest", # "all", #"2021-06-05", 
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

# anom_firstyr_nbs <- 2010
# anom_firstyr <- 1987
anom_lastyr <- 2018

anom <- anom_create(
  dat_nbs = dat_nbs, 
  dat_ebs = dat_ebs, 
  yr_first = anom_firstyr, 
  yr_last = anom_lastyr, 
  var = "GEAR_TEMPERATURE",
  save = TRUE)

# The bottom temperatures for this "yr"
create_BTplots(yr = yr, 
               anom = NULL, 
               heatLog = heatLog, 
               plot_title = paste0(yr, ' Survey Bottom Temperature (°C)'),
               legend_temp = 'Bottom\nTemperature (°C)',
               dates = "latest", # "all", #"2021-06-05", 
               region_akgfmaps = region_akgfmaps, 
               region_grid = region_grid, 
               file_end = "daily",
               dir_out = here::here("results", yr, "daily") )

# The bottom temperature anomaly between this year and past years
create_BTplots(yr = yr, 
               anom = anom, 
               heatLog = heatLog, 
               plot_title = paste0(yr, ' Bottom Temperature Anomaly\n(EBS: ',
                                   anom_firstyr,'-',anom_lastyr,'; NBS: ',
                                   ifelse(length(unique(substr(x = dat_nbs$CRUISE, 1,4)))>3,
                                          paste(range(as.numeric(unique(substr(
                                            x = dat_nbs$CRUISE, 1,4)))), collapse = "-"),
                                          text_list(x = unique(substr(
                                            x = dat_nbs$CRUISE, 1,4)))),
                                   ')'),
               legend_temp = 'Bottom Temperature\nAnomaly (°C)',
               dates = "latest", # "all", #"2021-06-05", 
               region_akgfmaps = region_akgfmaps, 
               region_grid = region_grid, 
               file_end = "anom",
               dir_out = here::here("results", yr, "anomalies"))