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
  
  #logo
  "png",
  "grid",
  "cowplot",
  "magick", 
  
  # color managment (color blind friendly!)
  # "viridis", 
  "colorRamps",
  
  # google drive
  "googledrive", # library(googlesheets)
  
  # data management
  "bindrcpp", 
  "janitor", 
  
  # "taskscheduleR",
  
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

# con <- file("notes.log")
# sink(con, append=TRUE)
# 
# sessionInfo()
# 
# # Restore output to console
# sink() 

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
              file = paste0(dir_in, "data/", paste0("dat_anom_nebs_", yr_last, ".csv")))
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
#' @param dates A string of either the date to be plotted to ("YYYY-MM-DD"), "all" which will plot all of the days where data was collected for in that year, or "latest" where only the most recent date's data will be plotted, or "none" where only the grid (no data) will bbe printed. Default = "latest". 
#' @param region_akgfmaps Inherited from the akgfmaps package. "bs.south" plots the EBS region, "bs.all" plots EBS and NBS regions. See the "select.region" argument under '?get_base_layers'. Default = "bs.all"
#' @param region_grid Select the shapefile you want to use for the grids. "Egrid_stations" plots the EBS region and "NEgrid_stations_df" plots EBS and NBS regions. Default = "NEgrid_stations".
#' @param height Numeric height of the figure in inches. default = 8.
#' @param width Numeric width of the figure in inches. default = 10.5.
#' @param file_end String. Will appear after "_" in the file name. 
#' @param gif Logical. Default = TRUE means that a gif of lastest and past files will be created.  
#' @param dir_out String. A path for the files to be saved to when the funciton is complete.
#' @param upload_googledrive Upload documents to google drive. Here, enter the path on google drive where these files should be saved or "NULL" if you do not want it saved to google drive. Default = as_id("1iZKhka07guho68WVUn2TJ7iI521mdWEb"). https://drive.google.com/drive/folders/1iZKhka07guho68WVUn2TJ7iI521mdWEb?usp=sharing
#'
#' @export
create_vargridplots <- function(
  yr = NULL, 
  anom = NULL, 
  heatLog = NULL, 
  plot_title = "",
  plot_subtitle = "",
  legend_temp = "",
  dates = "latest",
  region_akgfmaps = "bs.all", 
  region_grid = "NEgrid_stations", 
  height = 8.5, 
  width = 10.5,
  file_end = "",
  gif = TRUE,
  dir_out = "./", 
  dir_in = "./",
  upload_googledrive = as_id("1iZKhka07guho68WVUn2TJ7iI521mdWEb")) {

  
  plot_title <- ifelse(plot_title == "", 
                       paste0(yr, " Temperature °C"), 
                       plot_title)
  
  # Create Directories
  dir.create(path = paste0(dir_in, "results/"), showWarnings = FALSE)
  dir.create(path = dir_out, showWarnings = FALSE)
  # dir.create(path = paste(dir_out, "/", file_end), showWarnings = FALSE)
  
  # set Base Layers
  
  # Replace "bs.south" (EBS) with "bs.all" (EBS+NBS). See the "select.region" argument under '?get_base_layers'
  survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, 
                                 set.crs = "auto")
  # survey_area$graticule <- spTransform(survey_area$graticule, 
  #                                      crs(survey_area$akland))
  
  # Prepare map objects
  # Replaced shapefile "Egrid_stations" with "NEgrid_stations_df". Latter includes NBS+EBS grids
  grid_stations0 <- rgdal::readOGR(paste0(dir_in, '/shapefiles'),
                            region_grid, 
                            verbose=F) 
  
  
  grid_stations0 <- spTransform(grid_stations0,
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
  dat <- dat00 <- heatLog %>%
    janitor::clean_names() %>% 
    dplyr::filter(!is.na(region)) %>%
    dplyr::mutate(year = yr) %>%
    dplyr::mutate(date = strptime(x = paste0("0", date, "/", yr), 
                                  format = "%m/%d/%Y")) 
  
    
  # sepeate out the data for the temperature and planned stations if there are planned stations listed
  if (length(grep(x = heatLog$var, 
                  pattern = "[A-Za-z]"))>0) {
    
    # planned stations
    grid_stations_center <- cbind.data.frame(grid_stations0$STATION_ID, sp::coordinates(grid_stations0))
    names(grid_stations_center) <- c("station", "lon", "lat")
    
    dat_planned<-dat[grep(x = dat$var, pattern = "[A-Za-z]"),] %>%
      dplyr::left_join(x = ., y = grid_stations_center, "station") %>%
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
        paste0(vess, "\n(", vess_date, ")")
      
    }
    
    # surveyed stations
    dat$date[grep(x = dat$var, pattern = "[A-Za-z]")]<-NA
    dat$var[grep(x = dat$var, pattern = "[A-Za-z]")]<-''
    dat <- dat %>%
      dplyr::mutate(var = as.numeric(var))# %>% 
      # dplyr::select("region", "station", "var", "date") 
  }
  
  
  if (dates != "none") { # If you are not using any data from heatLog.csv
    
  # combine temperture data with anomaly data?
  if (!(is.null(anom))) { # if anom is extant
    dat <- dat %>%
      dplyr::left_join(x = ., y = anom, by = "station") %>%
      dplyr::rename(var_orig = var) %>%
      dplyr::mutate(var = var_orig - mean)
  }
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

  
  grid_stations <- sp::merge(x = grid_stations0, 
                      y = dat, 
                      by.x = "STATION_ID", 
                      by.y = "station") 
  
  grid_stations<-st_as_sf(grid_stations) %>%
    dplyr::filter(!(STATION_ID %in% c("DD-09", "AA-10")))
  
  # Create new temperature maps
  
  # This will create a preview in this Rmarkdown document, and save a PNG image file in the *G:\\EBSother\\EBS2019\\HeatMapR\\* directory named "*HeatPlotMM-DD-YYYY.png*", where MM-DD-YYYY is the date of trawl. 
  
  
  if (dates != "none") { # If you are not using any data from heatLog.csv
    date_entered<-sort(unique(grid_stations$date))
    date_entered<-date_entered[!is.na(date_entered)]
  }
  
  fig_list<-list()
if (dates == "none") { # If you are not using any data from heatLog.csv
  iterate <- 1 
  } else  if(dates == "all"){
    iterate <- 1:length(date_entered) # if you want to run all of plots for each date_entered: 
  } else if (dates == "latest") {
    iterate <- length(date_entered) # if you want to just run todays/a specific date:
  } else { # if you want to run a specific date
    iterate <- which(as.character(date_entered) == dates)
  }
  
  for (i in iterate) { 
    # print(i)
    # print(iterate[i])
    
    survey_reg_col <- gray.colors(length(unique(dat$reg_shapefile))+2)
    survey_reg_col <- survey_reg_col[-((length(survey_reg_col)-1):length(survey_reg_col))]
    
    
      grid_stations0<-grid_stations
      
    if (dates != "none") { # If you are not using any data from heatLog.csv
      max_date <- date_entered[i]
      if (length(max_date)==0) {
        max_date <- (min(dat00$date, na.rm = TRUE)-1) # The earliest planned date
        # max_date <- format(max_date, "%Y-%m-%d")
      }
      grid_stations0$binTemp[grid_stations0$date>max_date]<-NA  # only use dates including this date and before this date
    } else {
      max_date <- NA
      grid_stations0$binTemp<-NA  # only use dates including this date and before this date
      
    }
      
    gg <- ggplot() +
      # AK Map (thanks {akgfmaps}!)
      geom_sf(data = survey_area$akland, fill = "white") +
      # geom_sf(data = survey_area$bathymetry) +
      geom_sf(data = survey_area$graticule, 
              color = "grey70", 
              alpha = 0.5) +
      coord_sf(xlim = survey_area$plot.boundary$x, 
               ylim = survey_area$plot.boundary$y) +
      scale_x_continuous(name = "Longitude", 
                         breaks = survey_area$lon.breaks) + 
      scale_y_continuous(name = "Latitude", 
                         breaks = survey_area$lat.breaks) + 
      ggtitle(label = plot_title, 
              subtitle = plot_subtitle) +
      geom_sf(data = survey_area$survey.area, 
              aes(color = survey_area$survey.area$SURVEY), 
              fill = NA, 
              size = 2,
              show.legend = "") +
      scale_color_manual(name = "Survey Region", 
                         values = c(alpha(survey_reg_col, 0.7)), 
                         breaks = rev(unique(dat$reg_shapefile)), 
                         labels = rev(unique(dat$reg_lab)))  +
      theme( 
        panel.background = element_rect(fill = "grey90"#, colour = "lightblue"
        ),
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size=14), 
        legend.text=element_text(size=13), 
        legend.position="right",
        legend.direction="vertical",
        legend.justification="left",
        legend.background = element_blank(),
        legend.title=element_text(size=16),
        legend.box.background = element_blank(),
        legend.key = element_blank(), 
        legend.key.size=(unit(.3,"cm")), 
        axis.text = element_text(size=14), 
        axis.title=element_text(size=14)
        # line = element_line(colour = "green"),
        # rect = element_rect(fill = NA, 
        #                             colour = "red")
        # rect = element_rect(colour = "c8c8c8", fill = "NA", size = 5)
        # panel.border = element_rect(colour = "#c8c8c8", fill=NA, size=5)
      )  +
      # Annotations
      annotate("text", 
               x = quantile(extent(grid_stations)[1]:extent(grid_stations)[2], .9), 
               y = quantile(extent(grid_stations)[3]:extent(grid_stations)[4], .7), 
               label = "Alaska", 
               color = "black", size = 10) +
      annotate("text", 
               x = quantile(extent(grid_stations)[1]:extent(grid_stations)[2], .1), 
               y = quantile(extent(grid_stations)[3]:extent(grid_stations)[4], .1), 
               label = ifelse(is.na(max_date), 
                          "", 
                          paste0("Date created:\n", 
                                 format(Sys.Date(), "%B %d, %Y"), 
                              "\nRecorded as of:\n", 
                              format(x = max_date, format = "%B %d, %Y"))), 
               color = "black", size = 4) 
    # Add temperature squares
 
      
    if (dates != "none") { # If you are using any data from heatLog.csv
    gg <- gg +
      geom_sf(data = grid_stations0, 
              aes(group = STATION_ID, 
                  fill = binTemp), 
              colour = "grey50",
              show.legend = legend_temp) +
      
      
      scale_fill_manual(name = legend_temp,
                        values = varColour, 
                        labels = varLabels,
                        drop = F,
                        na.translate = F)
      
    # if there are planned dates
    if (length(grep(x = heatLog$var, pattern = "[A-Za-z]"))>0 & 
        i == iterate) {
      gg <- gg +
        geom_point(data = dat_planned, 
                   mapping = aes(x = lon, y = lat, shape = factor(var)), 
                   size = 4) +
        scale_shape_manual(
          name = "Planned Stations",
          values = sort(unique(dat_planned$var)),
          breaks = sort(unique(dat_planned$var)), 
          labels = sort(unique(dat_planned$lab))) +
        
        guides(
          fill = guide_legend(ncol=1, # tempertures # in case you want to have 2+ columns for the legend!
                              override.aes = list(colour = c("white"),
                                                  size = 0),
                              order = 1),
          colour = guide_legend(order = 2, # survey regions
                                override.aes = list(fill = survey_reg_col,
                                                    size = 2#, 
                                                    # color = "white"
                                                    )) , 
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
                                override.aes = list(fill = survey_reg_col, 
                                                    size = 2#, 
                                                    # color = "white"
                                                    ))) 
    }
    } else {
    
    gg <- gg +
        geom_sf(data = grid_stations, 
                aes(group = STATION_ID), 
                fill = NA,
                colour = "grey50") + 
      guides(
        # survey regions
        colour = guide_legend(order = 2, 
                              override.aes = list(fill = survey_reg_col, 
                                                  size = 2#, 
                                                  # color = "white"
                                                  ))) 
    }
  gg <- gg +
    coord_sf(xlim = c(extent(grid_stations)[1], 
                      extent(grid_stations)[2]),
             ylim = c(extent(grid_stations)[3] , 
                      extent(grid_stations)[4]))
 
    gg <- ggdraw(gg) +
      draw_image(image = paste0(dir_in, "img/noaa-50th-logo.png"), 
                 x = 0, y = 0,
                 hjust = -4.12, vjust = -.45,
                 width = .19)
    
    # Save plots
  
    filename0 <- paste0(dir_out, '/',
                        ifelse(dates == "none", "", 
                               paste(format(max_date, "%Y-%m-%d"))), 
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
    
    if (gif | dates != "none") {
    create_vargridplots_gif(yr = yr, 
                            file_end = file_end, 
                            max_date = max_date,
                            dir_out = dir_out)
    }
    
    if (!(is.null(upload_googledrive))) {
      
    drive_upload(
      media = paste0(filename0,'.png'), 
      path = upload_googledrive, 
      overwrite = TRUE)
    
    drive_upload(
      media = paste0(filename0,'.pdf'), 
      path = upload_googledrive, 
      overwrite = TRUE)
    
    # change each day of the survey
    if (gif | dates != "none") {
      drive_upload(
      media = paste0(filename0,'.gif'), 
      path = upload_googledrive, 
      overwrite = TRUE)
    }
}
    

    # fig_list<-c(fig_list, list(gg))
    # names(fig_list)[length(fig_list)] <- paste(max_date)
    
  }
  
  # Save all of the figures in case you need to print them again in a different way but dont want to rerun everything above :)
  # this is a MASSIVE file, so maybe run this sparingly?
  
  # HeatPlot_all<-fig_list
  # save(HeatPlot_all, file = paste0(dir_out, '/HeatPlot_all.RData'))
  
}


create_vargridplots_gif<-function(yr, file_end, max_date, dir_out) {
  
  imgs <- list.files(path = dir_out, 
                         pattern = paste0(file_end, ".png"), 
                     full.names = TRUE)
  
  imgs<-sort(imgs)

  as.numeric(strsplit(imgs, '\\/')[[1]][-1])
  
  dates_ploted <- gsub(pattern = paste0("_", file_end, ".png"), 
       replacement = "", 
       x = unlist(lapply(
    lapply(imgs, function(x) 
    (strsplit(x, '\\/')[[1]][-1])), tail, n = 1L)))
  dates_ploted <- strptime(x = dates_ploted, format = "%Y-%m-%d")
  max_date1 <- strptime(x = max_date, 
                        format = "%Y-%m-%d")
  
  dates_ploted_idx <- which(dates_ploted <= max_date1)
  
  img_list <- lapply(imgs[dates_ploted_idx], 
                     image_read)
  
  img_list <- lapply(img_list, 
         image_resize, 
         geometry = "1280")
  
  ## join the images together
  img_joined <- magick::image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- magick::image_animate(img_joined, 
                                        fps = 2)
  
  ## view animated image
  # img_animated
  
  ## save to disk
  magick::image_write(
    image = img_animated, 
    path = paste0(dir_out, "/", max_date1, "_", 
                            file_end, ".gif") )
  
}


