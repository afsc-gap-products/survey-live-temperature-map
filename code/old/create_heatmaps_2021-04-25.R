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
  "viridis", 
  
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

# TOLEDO - we actually don't need this... 

# Function to prepare shapefile objects for maps
#' We create a function to load and format ArcGIS shapefiles for use with ggplot2.
#'
#' @param dir Sting. The directory of the shapefile. 
#' @param shp String. The name of the shapefile we are looking for in the the "dir" directory. 
#'
#' @return A tibble of the "long","lat", "order", "hole","piece", "group", "id" of each feature of the "shp" shapefile. 
#' @export
# importEBSgrid <- function(dir, shp) {
#   shapefile <- rgdal::readOGR(dir,shp, verbose=F)
#   # Mess around to get StationID on the tidy dataframe
#   stationIds <- data.frame(STATION_ID=shapefile@data$STATION_ID)
#   stationIds$id <- as.character(seq(0,nrow(stationIds)-1))
#   shape.df <- broom::tidy(shapefile)
#   latLon <- cbind.data.frame(lon=shape.df$long, lat=shape.df$lat)
#   coordinates(latLon) <- ~ lon+lat
#   projection(latLon) <- sp::proj4string(shapefile)
#   latLon <- spTransform(latLon, CRS("+proj=longlat"))  %>%
#     data.frame()
#   ShapeUTM <- cbind(latLon,
#                     shape.df[,3:7])
#   names(ShapeUTM) <- c("long","lat", "order", "hole","piece", "group", "id")
#   shapeStations <- dplyr::inner_join(ShapeUTM,stationIds, by='id')
#   
#   return(list("shapeStations" = shapeStations, 
#               "shapefile" = shapefile))
# }

# Load data --------------------------------------------------------------------

yr <- 2018 # TOLEDO - change me!


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
tempBreaks <- round(x = c(-2,-1,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,18,21,30.4), 
                    digits = 2)
# TOLEDO - is this better than what we had below?
tempLabels <- c()
for(i in 2:length(tempBreaks)) {
  tempLabels <- c(tempLabels, 
                  dplyr::case_when(
                    i==2 ~ paste0("<= ",tempBreaks[i],"°"), 
                    i==length(tempBreaks) ~ paste0("> ", tempBreaks[i], "°"),
                    TRUE ~ paste0("> ",tempBreaks[i-1],"° to ",tempBreaks[i],"°")))
}

tempCol <- viridis::viridis(n = length(tempLabels)) # color-blind friendly!


# TOLEDO - is this improved by the code above?
# tempLabels <- c("<= -1.0°",
#                 "> -1.0° to -0.5°",
#                 "> -0.5° to 0°",
#                 "> 0.0° to 0.5°",
#                 "> 0.5° to 1.0°",
#                 "> 1.0° to 1.5°",
#                 "> 1.5° to 2.0°",
#                 "> 2.0° to 2.5°",
#                 "> 2.5° to 3.0°",
#                 "> 3.0° to 3.5°",
#                 "> 3.5° to 4.0°",
#                 "> 4.0° to 4.5°",
#                 "> 4.5° to 5.0°",
#                 "> 5.0° to 5.5°",
#                 "> 5.5° to 6.0°",
#                 "> 6.0°", 
#                 "EBS station 5/29 to 8/3", 
#                 "NBS station 8/4 to 8/27")

### Input New Data
# Next, use Excel to update heatLog.csv in the folder *G:\\EBSother\\EBS2019\\HeatMapR\\* with the new data, filling Station (same format as RACEBASE) and bottom temperature. Surface temperatures are optional. You may want to re-sort these by Station to make sure there are no duplicates, although this shouldn't crash the script. **Do not** change column names or file names.

drive_deauth()
drive_auth()
1

# Load data from Google Sheet
drive_download("heatLog.csv",
               type = "csv",
               overwrite = T)

heatLog <- read_csv("heatLog.csv") %>%
  janitor::clean_names() %>% 
  dplyr::filter(!is.na(bottom_temp)) %>%
  dplyr::mutate(binTemp = cut(bottom_temp,
                              breaks=tempBreaks,
                              labels=tempLabels,
                              right = F) ) %>%
  dplyr::mutate(year = yr) %>%
  dplyr::mutate(date_entered = strptime(x = paste0("0", date_entered, "/", yr), 
                                        format = "%m/%d/%Y")) %>% # TOLEDO! Are these dates cleared and entered as the survey is happening? How can I use this column? 
  dplyr::select("station", "bottom_temp", "surface_temp", "date_entered", "year", "binTemp")


BSgrid <- sp::merge(x = BSgrid, 
                    y = heatLog, 
                    by.x = "STATION_ID", 
                    by.y = "station") 


BSgrid<-st_as_sf(BSgrid)

# Create new temperature maps ---------------------------------------------------

# This will create a preview in this Rmarkdown document, and save a PNG image file in the *G:\\EBSother\\EBS2019\\HeatMapR\\* directory named "*HeatPlotMM-DD-YYYY.png*", where MM-DD-YYYY is the date of trawl. 

date_entered<-sort(unique(BSgrid$date_entered))
date_entered<-date_entered[!is.na(date_entered)]

fig_list<-list()

for (i in 1:length(date_entered)) { # if you want to run all of plots for each date_entered: 
  # for (i in length(date_entered)) { # if you want to just run todays/a specific date:
  
  max_date <- date_entered[i]
  BSgrid0<-BSgrid
  BSgrid0$binTemp[BSgrid0$date_entered>max_date]<-NA  # only use dates including this date and before this date
  
  
  gg <- ggplot() +
    # AK Map (thanks {akgfmaps}!)
    geom_sf(data = survey_area$akland, fill = "white") +
    # geom_sf(data = survey_area$bathymetry) +
    # geom_sf(data = survey_area$survey.area, fill = NA) +
    geom_sf(data = survey_area$graticule, color = "grey70", alpha = 0.5) +
    coord_sf(xlim = survey_area$plot.boundary$x, 
             ylim = survey_area$plot.boundary$y) +
    scale_x_continuous(name = "Longitude", 
                       breaks = survey_area$lon.breaks) + 
    scale_y_continuous(name = "Latitude", 
                       breaks = survey_area$lat.breaks) + 
    # Add temperature squares
    geom_sf(data = BSgrid0, 
            aes(group = STATION_ID, fill = binTemp)) +
    scale_fill_manual(name = 'Bottom\nTemperature (°C)',
                      values = tempCol,
                      # TOLEDO - not color-blind friendly!
                      # values = c("#8000ff","#3700ff","#0012ff","#005bff","#00a4ff","#00C8ff",
                      #            "#00FFED","#00ff92","#00ff00","#80ff00","#C8ff00","#FFff00",
                      #            "#ffB600","#ff8000","#ff4900","#FF0000","#dbfbdc","#cbfcfb"),
                      labels = tempLabels,
                      drop = F,
                      na.translate = F
    ) +
    coord_sf(xlim = c(extent(BSgrid)[1], 
                      extent(BSgrid)[2]),
             ylim = c(extent(BSgrid)[3] , 
                      extent(BSgrid)[4])) +
    # Style it up!
    guides(fill=guide_legend(ncol=1)) + # in case you want to have 2+ columns for the legend!
    theme( 
      legend.text=element_text(size=16), 
      legend.position="right",
      legend.direction="vertical",
      legend.justification="left",
      legend.title=element_text(size=20),
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
  
  ggsave(paste0('./results/HeatPlot',paste(max_date),'.png'), 
         height = 8, width = 10.5,
         plot=gg, device="png") #pngs are great for viewing quickly and are small
  
  ggsave(paste0('./results/HeatPlot',paste(max_date),'.pdf'), 
         height = 8, width = 10.5,
         plot=gg, device="pdf") # pdfs are great for editing later
  
  fig_list<-c(fig_list, list(gg))
  names(fig_list)[length(fig_list)] <- paste(max_date)
  
}

# Save all of the figures in case you need to print them again in a different way but dont want to rerun everything above :)
# this is a MASSIVE file, so maybe run this sparingly?

# HeatPlot_all<-fig_list
# save(HeatPlot_all, file = './results/HeatPlot_all.RData')

