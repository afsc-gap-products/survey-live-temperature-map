#' ############
#' title: "Temperature Plot"
#' OG author: Jason Conner
#' Maintained: Emily Markowitz and Liz Dawson (April 2021)
#' ############

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

# Functions --------------------------------------------------------------------

#' Combine regional survey data to create the anomaly dataset
#'
#' @param dat_nbs data.frame of quieried data from RACEBASE.HAUL for the NBS. 
#' @param dat_ebs data.frame of quieried data from RACEBASE.HAUL for the EBS. 
#' @param var String of the name of the column that needs to be summarized. 
#' @param yr_first The earliest year (YYYY) of data that needs to be included in the anom dataset.
#' @param yr_last The latest year (YYYY) of data that needs to be included in the anom dataset. 
#' @param save Logical. TRUE saves a copy of this dataset to the "data" folder, FALSE does not save the data set anywhere. 
#' @export
anom_create<-function(dat_nbs, 
                      dat_ebs, 
                      yr_first, 
                      yr_last,
                      var = "GEAR_TEMPERATURE",
                      save = FALSE){
  
  dat_anom <- rbind.data.frame(dat_nbs, dat_ebs) %>%
    dplyr::mutate(year = substr(x = CRUISE, start = 1, stop = 4)) %>%
    dplyr::filter(year <= yr_first | year >= yr_last) %>%
    dplyr::rename("var" = var) %>%
    dplyr::select(STATIONID, var) %>%
    janitor::clean_names() %>% 
    dplyr::group_by(stationid) %>%
    dplyr::summarise(mean = mean(var, na.rm = TRUE)) %>%
    dplyr::rename(station = stationid)
  
  if (isTRUE(save)){
    write_csv(x = dat_anom, 
              file = here::here("data", paste0("dat_anom_nebs_", yr_last, ".csv")))
  }
  
  return(dat_anom)
}

#' Create temperature and anomaly plots
#'
#' @param yr The year being plotted or compared to in the anomaly
#' @param anom A data.frame with two columns: 'station' and 'mean'. Stations in 'station' should match with those in the heatLog.csv file. values in 'mean' should be the means of the values for several years of the survey
#' @param heatLog The csv file from googledrive with data for the 'yr' being plotted or compared to in the anomally.  
#' @param plot_title A string of what the plot title should be. 
#' @param legend_temp A string of the temperature legend header. 
#' @param dates A string of either the date to be plotted to ("YYYY-MM-DD"), "all" which will plot all of the days where data was collected for in that year, or "latest" where only the most recent date's data will be plotted. Default = "latest". 
#' @param region_akgfmaps Inherited from the akgfmaps package. "bs.south" plots the EBS region, "bs.all" plots EBS and NBS regions. See the "select.region" argument under '?get_base_layers'. Default = "bs.all"
#' @param region_grid Select the shapefile you want to use for the grids. "EBSgrid" plots the EBS region and "NEBSgrid_df" plots EBS and NBS regions. Default = "NEBSgrid".
#' @param height Numeric height of the figure in inches. default = 8.
#' @param width Numeric width of the figure in inches. default = 10.5.
#' @param dir_out A path for the files to be saved to when the funciton is complete.
#' @export
create_BTplots <- function(yr, 
                           anom = NULL, 
                           heatLog, 
                           plot_title = "",
                           plot_subtitle = "Recorded during NOAA Fisheries Southeastern Bering Sea Bottom Trawl Survey",
                           legend_temp = 'Bottom\nTemperature (°C)',
                           dates = "latest",
                           region_akgfmaps = "bs.all", 
                           region_grid, 
                           height = 8, 
                           width = 10.5,
                           file_end = "",
                           dir_out = "./") {
  
  
  plot_title <- ifelse(plot_title == "", 
                       paste0(yr, " Temperature °C"), 
                       plot_title)
  
  # Create Directories
  dir.create(path = here::here("results"), showWarnings = FALSE)
  dir.create(path = here::here("results", yr), showWarnings = FALSE)
  dir.create(path = dir_out, showWarnings = FALSE)
  
  # set Base Layers
  
  # Replace "bs.south" (EBS) with "bs.all" (EBS+NBS). See the "select.region" argument under '?get_base_layers'
  survey_area <- get_base_layers(select.region = "bs.all", 
                                 set.crs = "auto")
  # survey_area$graticule <- spTransform(survey_area$graticule, 
  #                                      crs(survey_area$akland))
  
  # Prepare map objects
  # Repalced shapefile "EBSgrid" with "NEBSgrid_df". Latter includes NBS+EBS grids
  BSgrid <- rgdal::readOGR('./shapefiles',
                           "NEBSgrid", 
                           verbose=F)
  
  
  BSgrid <- spTransform(BSgrid,
                        "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") # crs(survey_area))
  
  # Break temps into factors
  if (is.null(anom)) { # if anom DOES NOT exist (straight temps!)
    varBreaks <- c(-10,
                    seq(from = -2, to = 8, by = 0.5), 
                    50)
  } else { # for the anomaly data...
    varBreaks <- c(-10,
                    seq(from = -2, to = 3, by = 0.5), 
                    50)
  }
  
  varLabels <- c()
  for(i in 2:c(length(varBreaks))) {
    varLabels <- c(varLabels, 
                    dplyr::case_when(
                      i==2 ~ paste0("<= ",varBreaks[i],"°C"), 
                      i==(length(varBreaks)) ~ paste0("> ", varBreaks[i-1], "°C"),
                      TRUE ~ paste0("> ",
                        varBreaks[i-1],"° to ",varBreaks[i],"°C")
                    ))
  }
  
  # varColour <- rainbow(n = length(varLabels), rev = FALSE, start = .8)
  
  varColour <- colorRamps::matlab.like(length(varLabels))
  #viridis::viridis(n = length(varLabels)) # color-blind friendly!
  
  #MACARONI ^ I added a "region" column to the heatLog.csv file to differentiate which station is EBS vs. NBS.  Maybe we could have those two shaded in grey or stripes? and designate colors/patterns those columns in the legend as "EBS station 5/23 to 8/1", 
  #                 "NBS station 7/30 to 8/25"
  
  ### Input New Data
  # Next, use Excel to update heatLog.csv in the folder *G:\\EBSother\\EBS2019\\HeatMapR\\* with the new data, filling Station (same format as RACEBASE) and bottom temperature. Surface temperatures are optional. You may want to re-sort these by Station to make sure there are no duplicates, although this shouldn't crash the script. **Do not** change column names or file names.

  dat <- heatLog %>%
    janitor::clean_names() %>% 
    dplyr::filter(!is.na(var)) %>%
    dplyr::select("region", "station", "var", "date")#, "year", "binTemp") 

  
  if (!(is.null(anom))) { # if anom is extant
    dat <- dat %>%
      dplyr::left_join(., anom, c("station")) %>%
      dplyr::rename(var_orig = var) %>%
      dplyr::mutate(var = var_orig - mean)
  }
  
  dat <- dat %>%
    dplyr::mutate(binTemp = base::cut(x = var,
                                breaks = varBreaks,
                                labels = FALSE, #varLabels,
                                include.lowest = TRUE,
                                right = FALSE) ) %>%
    dplyr::mutate(binTemp = base::factor(x = varLabels[binTemp], 
                                         levels = varLabels, 
                                         labels = varLabels) ) %>%
    dplyr::mutate(year = yr) %>%
    dplyr::mutate(date = strptime(x = paste0("0", date, "/", yr), 
                                          format = "%m/%d/%Y")) 

  
  BSgrid <- sp::merge(x = BSgrid, 
                      y = dat, 
                      by.x = "STATION_ID", 
                      by.y = "station") 
  
  
  BSgrid<-st_as_sf(BSgrid)
  
  # Create new temperature maps ---------------------------------------------------
  
  # This will create a preview in this Rmarkdown document, and save a PNG image file in the *G:\\EBSother\\EBS2019\\HeatMapR\\* directory named "*HeatPlotMM-DD-YYYY.png*", where MM-DD-YYYY is the date of trawl. 
  
  date_entered<-sort(unique(BSgrid$date))
  date_entered<-date_entered[!is.na(date_entered)]
  
  fig_list<-list()
  
  if(dates == "all"){
    iterate <- 1:length(date_entered) # if you want to run all of plots for each date_entered: 
  } else if (dates == "latest") {
    iterate <- length(date_entered) # if you want to just run todays/a specific date:
  } else { # if you want to run a specific date
    iterate <- which(as.character(date_entered) == dates)
  }
  
  for (i in iterate) { 

    max_date <- date_entered[i]
    BSgrid0<-BSgrid
    BSgrid0$binTemp[BSgrid0$date_entered>max_date]<-NA  # only use dates including this date and before this date
    #MACARONI I think we only need one plot to be produced for the max_date, not multiple plot for each date of temp entries.

    gg <- ggplot() +
      # AK Map (thanks {akgfmaps}!)
      geom_sf(data = survey_area$akland, fill = "white") +
      # geom_sf(data = survey_area$bathymetry) +
      geom_sf(data = survey_area$graticule, color = "grey70", alpha = 0.5) +
      coord_sf(xlim = survey_area$plot.boundary$x, 
               ylim = survey_area$plot.boundary$y) +
      scale_x_continuous(name = "Longitude", 
                         breaks = survey_area$lon.breaks) + 
      scale_y_continuous(name = "Latitude", 
                         breaks = survey_area$lat.breaks) + 
      ggtitle(label = plot_title, 
              subtitle = plot_subtitle) +
      
      geom_sf(data = BSgrid0,       # Add temperature squares
              aes(group = STATION_ID, 
                  fill = binTemp), 
              colour = "grey80",
              show.legend = legend_temp) +
      
      geom_sf(data = survey_area$survey.area, 
              aes(color = survey_area$survey.area$SURVEY_REG), 
              fill = NA, 
              size = 2,
              show.legend = "") + #Survey Region") +

      scale_fill_manual(name = legend_temp,
                        values = varColour, 
                        # TOLEDO - not color-blind friendly!
                        # values = c("#8000ff","#3700ff","#0012ff","#005bff","#00a4ff","#00C8ff",
                        #            "#00FFED","#00ff92","#00ff00","#80ff00","#C8ff00","#FFff00",
                        #            "#ffB600","#ff8000","#ff4900","#FF0000","#dbfbdc","#cbfcfb"),
                        labels = varLabels,
                        drop = F,
                        na.translate = F
      ) +
      scale_color_manual(name = "",#Survey Region", 
                         values = c(alpha("grey30",0.7), 
                                    alpha("grey60",0.7)), 
                         breaks = c("NEBS", "SEBS"), 
                         labels = c("NBS", "EBS")) +
      coord_sf(xlim = c(extent(BSgrid)[1], 
                        extent(BSgrid)[2]),
               ylim = c(extent(BSgrid)[3] , 
                        extent(BSgrid)[4])) +
      # Style it up!
      
      guides(
        # tempartures # in case you want to have 2+ columns for the legend!
        fill = guide_legend(ncol=1, 
                                 override.aes = list(colour = c("white"),
                                                     size = 0),
                                 order = 1),
       # survey regions
       colour = guide_legend(order = 2, 
                                   override.aes = list(fill = c("grey30", "grey60"), 
                                                       size = 2))) + 
      theme( 
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.position="right",
        legend.direction="vertical",
        legend.justification="left",
        legend.title=element_text(size=18),
        legend.key.size=(unit(.3,"cm"))
      ) +
      # Annotations
      annotate("text", 
               x = quantile(extent(BSgrid)[1]:extent(BSgrid)[2], .9), 
               y = quantile(extent(BSgrid)[3]:extent(BSgrid)[4], .7), 
               label = "Alaska", 
               color = "black", size = 10) +
      annotate("text", 
               x = quantile(extent(BSgrid)[1]:extent(BSgrid)[2], .1), 
               y = quantile(extent(BSgrid)[3]:extent(BSgrid)[4], .1), 
               label = paste0("Date created:\n", format(Sys.Date(), "%B %d, %Y"), 
                              "\nRecorded as of:\n", format(max_date, "%B %d, %Y")), 
               color = "black", size = 4) 
    
    # Save plots
    
    # TOLEDO - I saved the name of the file with the last date_entered instead sys.date() - I think that is more useful, is a better thing to do?
    filename0<-paste0(dir_out, '/HeatPlot',paste(max_date), 
                      ifelse(file_end=="", "", paste0("_", file_end)))
    ggsave(paste0(filename0,'.png'), 
           height = 8, width = 10.5,
           plot=gg, device="png") #pngs are great for viewing quickly and are small
    
    ggsave(paste0(filename0,'.pdf'), 
           height = 8, width = 10.5,
           plot=gg, device="pdf") # pdfs are great for editing later
    
    fig_list<-c(fig_list, list(gg))
    names(fig_list)[length(fig_list)] <- paste(max_date)
    
  }
  
  # Save all of the figures in case you need to print them again in a different way but dont want to rerun everything above :)
  # this is a MASSIVE file, so maybe run this sparingly?
  
  # HeatPlot_all<-fig_list
  # save(HeatPlot_all, file = paste0(dir_out, '/HeatPlot_all.RData'))
  
}


# Load data --------------------------------------------------------------------

if (FALSE) { # so you don't unnecessarially run this each time

# *** Oracle -------------------------------------------------------------------

PKG <- c("dplyr", "tidyverse", "knitr", "kableExtra", "reshape")
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

# This has a specific link because I DONT want people to have access to this!
source("C:/Users/emily.markowitz/Documents/Projects/ConnectToOracle.R")


# NBS Data
dat_nbs <- sqlQuery(channel, "
                        SELECT * 

                        FROM RACEBASE.HAUL 

                        WHERE

                        REGION = 'BS' 

                        AND STRATUM IN(70,71,81)

                        AND CRUISE IN(201002, 201702, 201902);
                        ")

yr_first <- min(substr(x = dat_nbs$CRUISE, start = 1, stop = 4))
yr_last <- max(substr(x = dat_nbs$CRUISE, start = 1, stop = 4))
dat_nbs$REGION<-"NBS"
dat_nbs$BOTTOM_TYPE<-NULL
write_csv(x = dat_nbs, 
          file = here::here("data", paste0("dat_nbs_",yr_first,"-", yr_last, ".csv")))

# EBS Data
dat_ebs <- sqlQuery(channel, "
                        SELECT * 

                        FROM RACEBASE.HAUL 

                        WHERE

                        REGION = 'BS' 

                        AND STRATUM IN(10,20,31,32,41,42,43,50,61,62,82,90)

                        AND CRUISE >= 198701;
                        ")

yr_first <- min(substr(x = dat_ebs$CRUISE, start = 1, stop = 4))
yr_last <- max(substr(x = dat_ebs$CRUISE, start = 1, stop = 4))
dat_ebs$REGION<-"EBS"
dat_ebs$BOTTOM_TYPE<-NULL
write_csv(x = dat_ebs, 
          file = here::here("data", paste0("dat_ebs_",yr_first,"-", yr_last, ".csv")))

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
                                "date" = paste0(yr, "_date"))

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


