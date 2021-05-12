#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: functions
#' ---------------------------


# Install libraries ------------------------------------------------------------

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
  
  "taskscheduleR",
  
  # tidyverse, 
  "broom", 
  "readr", 
  "dplyr", 
  "glue"
)

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}


# Configuration ----------------------------------------------------------------

con <- file("notes.log")
sink(con, append=TRUE)

sessionInfo()

# Restore output to console
sink() 

# And look at the log...
# cat(readLines("notes.log"), sep="\n")

# Funcitons --------------------------------------------------------------------


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
    dplyr::rename("var" = all_of(var)) %>%
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


#' Takes a string of words and combines them into a sentance that lists them.
#'
#' This function alows you to take a string of words and combine them into a sentance list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
#' @param x Character strings you want in your string.
#' @param oxford T/F: would you like to use an oxford comma? Default = TRUE
#' @keywords strings
#' @export
#' @examples text_list(c(1,2,"hello",4,"world",6))
text_list<-function(x, oxford = TRUE) {
  x<-x[which(x!="")]
  # x<-x[which(!is.null(x))]
  x<-x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x)==2) {
    str1<-paste(x, collapse = " and ")
  } else if (length(x)>2) {
    str1<-paste(x[1:(length(x)-1)], collapse = ", ")
    str1<-paste0(str1,
                 ifelse(oxford == TRUE, ",", ""),
                 " and ", x[length(x)])
  } else {
    str1<-x
  }
  return(str1)
}



#' Create temperature and anomaly plots
#'
#' @param yr The year being plotted or compared to in the anomaly
#' @param anom A data.frame with two columns: 'station' and 'mean'. Stations in 'station' should match with those in the heatLog.csv file. values in 'mean' should be the means of the values for several years of the survey
#' @param heatLog The csv file from googledrive with data for the 'yr' being plotted or compared to in the anomally. 
#' @param plot_title A string of what the plot title should be. 
#' @param plot_subtitle A string of what the plot subtitle should be. 
#' @param legend_temp A string of the temperature legend header. 
#' @param dates A string of either the date to be plotted to ("YYYY-MM-DD"), "all" which will plot all of the days where data was collected for in that year, or "latest" where only the most recent date's data will be plotted. Default = "latest". 
#' @param region_akgfmaps Inherited from the akgfmaps package. "bs.south" plots the EBS region, "bs.all" plots EBS and NBS regions. See the "select.region" argument under '?get_base_layers'. Default = "bs.all"
#' @param region_grid Select the shapefile you want to use for the grids. "EBSgrid" plots the EBS region and "NEBSgrid_df" plots EBS and NBS regions. Default = "NEBSgrid".
#' @param height Numeric height of the figure in inches. default = 8.
#' @param width Numeric width of the figure in inches. default = 10.5.
#' @param file_end String. Will appear after "_" in the file name. 
#' @param dir_out String. A path for the files to be saved to when the funciton is complete.
#' @param upload_googledrive Upload documents to google drive. Here, enter the path on google drive where these files should be saved or "NULL" if you do not want it saved to google drive. Default = as_id("1iZKhka07guho68WVUn2TJ7iI521mdWEb"). https://drive.google.com/drive/folders/1iZKhka07guho68WVUn2TJ7iI521mdWEb?usp=sharing
#'
#' @export
create_vargridplots <- function(
  yr, 
  anom = NULL, 
  heatLog, 
  plot_title = "",
  plot_subtitle = "Recorded during the NOAA Fisheries Southeastern Bering Sea\nBottom Trawl Survey",
  legend_temp = 'Bottom\nTemperature (°C)',
  dates = "latest",
  region_akgfmaps = "bs.all", 
  region_grid, 
  height = 8, 
  width = 10.5,
  file_end = "",
  dir_out = "./", 
  upload_googledrive = as_id("1iZKhka07guho68WVUn2TJ7iI521mdWEb")) {

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
  BSgrid0 <- rgdal::readOGR('./shapefiles',
                            "NEBSgrid", 
                            verbose=F)
  
  
  BSgrid0 <- spTransform(BSgrid0,
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
                     i==2 ~ paste0("<= ",varBreaks[i],"\u00B0C"), 
                     i==(length(varBreaks)) ~ paste0("> ", varBreaks[i-1], "\u00B0C"),
                     TRUE ~ paste0("> ",
                                   varBreaks[i-1],"\u00B0 to ",varBreaks[i],"\u00B0C")
                   ))
  }
  
  # varColour <- rainbow(n = length(varLabels), rev = FALSE, start = .8)
  
  varColour <- colorRamps::matlab.like(length(varLabels))
  #viridis::viridis(n = length(varLabels)) # color-blind friendly!
  
  #MACARONI ^ I added a "region" column to the heatLog.csv file to differentiate which station is EBS vs. NBS.  Maybe we could have those two shaded in grey or stripes? and designate colors/patterns those columns in the legend as "EBS station 5/23 to 8/1", 
  #                 "NBS station 7/30 to 8/25"
  
  ### Input New Data
  # Next, use Excel to update heatLog.csv in the folder *G:\\EBSother\\EBS2019\\HeatMapR\\* with the new data, filling Station (same format as RACEBASE) and bottom temperature. Surface temperatures are optional. You may want to re-sort these by Station to make sure there are no duplicates, although this shouldn't crash the script. **Do not** change column names or file names.
  
  # select the data you care about
  dat <- heatLog %>%
    janitor::clean_names() %>% 
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(year = yr) %>%
    dplyr::mutate(date = strptime(x = paste0("0", date, "/", yr), 
                                  format = "%m/%d/%Y")) 
  
  # sepeate out the data for the temperature and planned stations if there are planned stations listed
  if (length(grep(x = heatLog$var, pattern = "[A-Za-z]"))>0) {
    
    # planned stations
    BSgrid_center <- cbind.data.frame(BSgrid0$STATION_ID, coordinates(BSgrid0))
    names(BSgrid_center) <- c("station", "lon", "lat")
    
    dat_planned<-dat[grep(x = dat$var, pattern = "[A-Za-z]"),] %>%
      dplyr::left_join(x = ., y = BSgrid_center, "station") %>%
      dplyr::mutate(var = toupper(var)) %>%
      dplyr::mutate(lab = "") #%>% 
      # dplyr::select("region", "station", "var", "date", "vess", "lab") 
      
    
    for (i in 1:length(unique(dat_planned$vess))) {
      vess <- unique(dat_planned$vess)[i]
      vess_date <- range(dat_planned$date[dat_planned$vess %in% vess])
      vess_date <- ifelse(vess_date[1] == vess_date[2], 
                          format(vess_date[1], # "%a %b %d"
                                 "%b %d"), 
                          paste(format(vess_date, # "%a %b %d"
                                       "%b %d"), collapse = "-"))
      dat_planned$lab[dat_planned$vess %in% vess]<-
        paste0(vess, " (", vess_date, ")")
      
    }
    
    # surveyed stations
    dat$date[grep(x = dat$var, pattern = "[A-Za-z]")]<-NA
    dat$var[grep(x = dat$var, pattern = "[A-Za-z]")]<-''
    dat <- dat %>%
      dplyr::mutate(var = as.numeric(var)) %>% 
      dplyr::select("region", "station", "var", "date") 
  }
  
  # combine temperture data with anomaly data?
  if (!(is.null(anom))) { # if anom is extant
    dat <- dat %>%
      dplyr::left_join(x = ., y = anom, by = "station") %>%
      dplyr::rename(var_orig = var) %>%
      dplyr::mutate(var = var_orig - mean)
  }
  
  # prepare data for final prodcut
  dat <- dat %>%
    dplyr::mutate(binTemp = base::cut(x = var,
                                      breaks = varBreaks,
                                      labels = FALSE, #varLabels,
                                      include.lowest = TRUE,
                                      right = FALSE) ) %>%
    dplyr::mutate(binTemp = base::factor(x = varLabels[binTemp], 
                                         levels = varLabels, 
                                         labels = varLabels) ) #%>% 
    # dplyr::select("region", "station", "var", "date", "bintemp", "year") 
  
  
  BSgrid <- sp::merge(x = BSgrid0, 
                      y = dat, 
                      by.x = "STATION_ID", 
                      by.y = "station") 
  
  
  BSgrid<-st_as_sf(BSgrid)
  
  # Create new temperature maps
  
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
    BSgrid0$binTemp[BSgrid0$date>max_date]<-NA  # only use dates including this date and before this date
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
              show.legend = "") +
      coord_sf(xlim = c(extent(BSgrid)[1], 
                        extent(BSgrid)[2]),
               ylim = c(extent(BSgrid)[3] , 
                        extent(BSgrid)[4])) +
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
      scale_color_manual(name = "Survey Region", 
                         values = c(alpha("grey30",0.7), 
                                    alpha("grey60",0.7)), 
                         breaks = c("NEBS", "SEBS"), 
                         labels = c("NBS", "EBS")) 
    
    
    # if there are planned dates
    if (length(grep(x = heatLog$var, pattern = "[A-Za-z]"))>0) {
      gg <- gg +
        geom_point(data = dat_planned, 
                   mapping = aes(x = lon, y = lat, shape = factor(var)), 
                   size = 4#, show.legend = TRUE
        ) +
        scale_shape_manual(
          name = "Planned Stations",
          values = unique(dat_planned$var),
          breaks = unique(dat_planned$var), 
          labels = unique(dat_planned$lab)) +
        
        guides(
          fill = guide_legend(ncol=1, # tempartures # in case you want to have 2+ columns for the legend!
                              override.aes = list(colour = c("white"),
                                                  size = 0),
                              order = 1),
          colour = guide_legend(order = 2, # survey regions
                                override.aes = list(fill = c("grey30", "grey60"), 
                                                    size = 2)) , 
          shape = guide_legend(order = 3, # planned stations
                               override.aes = list(fill = "grey95", 
                                                   linetype = c("blank")))
        )
      
    } else {
      gg <- gg +
        guides(
          # tempartures # in case you want to have 2+ columns for the legend!
          fill = guide_legend(ncol=1, 
                              override.aes = list(colour = c("white"),
                                                  size = 0),
                              order = 1),
          # survey regions
          colour = guide_legend(order = 2, 
                                override.aes = list(fill = c("grey30", "grey60"), 
                                                    size = 2))) 
    }
    
    gg <- gg +
      theme( 
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size=14), 
        legend.text=element_text(size=16), 
        legend.position="right",
        legend.direction="vertical",
        legend.justification="left",
        legend.title=element_text(size=16),
        # legend.key = element_rect(colour = NA, fill = NA), 
        legend.box.background = element_blank(),
        legend.key = element_blank(), 
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
  
    filename0 <- paste0(dir_out, '/TempPlot',paste(max_date), 
                        ifelse(file_end=="", "", paste0("_", file_end)))
    
    ggsave(paste0(filename0,'.png'),
           height = height, 
           width = width,
           plot=gg, 
           device="png") # pdfs are great for editing later
    
    ggsave(paste0(filename0,'.pdf'),
           height = height, 
           width = width,
           plot=gg, 
           device="pdf") # pdfs are great for editing later
    
    
    if (!(is.null(upload_googledrive))) {
      
    drive_upload(
      media = paste0(filename0,'.png'), 
      path = upload_googledrive, 
      overwrite = TRUE)
    
    drive_upload(
      media = paste0(filename0,'.pdf'), 
      path = upload_googledrive, 
      overwrite = TRUE)
}
    

    fig_list<-c(fig_list, list(gg))
    names(fig_list)[length(fig_list)] <- paste(max_date)
    
  }
  
  # Save all of the figures in case you need to print them again in a different way but dont want to rerun everything above :)
  # this is a MASSIVE file, so maybe run this sparingly?
  
  # HeatPlot_all<-fig_list
  # save(HeatPlot_all, file = paste0(dir_out, '/HeatPlot_all.RData'))
  
}
