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
  "sf",
  
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


#' #' Combine regional survey data to create the anomaly dataset
#' #'
#' #' @param dat_nbs data.frame of quieried data from RACEBASE.HAUL for the NBS. 
#' #' @param dat_ebs data.frame of quieried data from RACEBASE.HAUL for the EBS. 
#' #' @param var String of the name of the column that needs to be summarized. 
#' #' @param yr_first The earliest year (YYYY) of data that needs to be included in the anom dataset.
#' #' @param yr_last The latest year (YYYY) of data that needs to be included in the anom dataset. 
#' #' @param save Logical. TRUE saves a copy of this dataset to the "data" folder, FALSE does not save the data set anywhere. 
#' #' @export
#' anom_create<-function(dat, 
#'                       var = "GEAR_TEMPERATURE"){
#'   
#'   dat_anom <- dat %>% 
#'     dplyr::rename("var" = all_of(var)) %>%
#'     dplyr::rename(station = stationid) %>%
#'     dplyr::filter(!(is.na(station))) %>%
#'     dplyr::group_by(SRVY, stratum, station) %>% 
#'     dplyr::summarise(mean = mean(var, na.rm = TRUE))
#'   
#'   return(dat_anom)
#' }


#' Takes a string of words and combines them into a sentance that lists them.
#'
#' This function alows you to take a string of words and combine them into a sentance list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
#' @param x Character strings you want in your string.
#' @param oxford T/F: would you like to use an oxford comma? Default = TRUE
#' @param sep string. default = ", " but "; " or " " might be what you need!
#' @param sep_last string. default = " and " but " & " or " , " might be what you need!
#' @keywords strings
#' @export
#' @examples
#' text_list(c(1,2,"hello",4,"world",6))
#' text_list(c(1,"world"))
#' text_list(c(1,2,"hello",4,"world",6), oxford = FALSE)
#' paste0("here is a list of things: ",
#'   text_list(paste0("list", 1:5), sep = " ", sep_last = ""))
text_list<-function(x = "",
                    oxford = TRUE,
                    sep = ", ",
                    sep_last = "and ") {
  
  x<-x[which(x!="")]
  # x<-x[which(!is.null(x))]
  x<-x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x)==2) {
    str1<-paste(x, collapse = paste0(" ", sep_last))
  } else if (length(x)>2) {
    str1<-paste(x[1:(length(x)-1)], collapse = paste0(sep))
    str1<-paste0(str1,
                 ifelse(oxford == TRUE, sep, " "),
                 sep_last, x[length(x)])
  } else {
    str1<-x
  }
  return(str1)
}


make_plot_warpper <- function(maxyr, 
                              SRVY, 
                              haul, 
                              dat_survreg, 
                              var = "bt", 
                              dir_googledrive_upload, 
                              dates0, 
                              grid_stations, 
                              plot_subtitle = "", 
                              region_akgfmaps, 
                              extrap.box, 
                              show_planned_stations) {
  
  case <- paste0(maxyr, "_", SRVY)
  dir_out <- here::here("output", case)
  
  SRVY1 <- SRVY
  if (SRVY == "BS") {
    SRVY1 <- c("EBS", "NBS")
  }
  
  # Define var
  if (var == "bt") {
    var00 = 'Bottom Temperature'
    unit0 <- '(\u00B0C)'
    var0 <- "gear_temperature"
    var_breaks <- c(-10, seq(from = -2, to = 8, by = 0.5), 50) # if anom DOES NOT exist (straight temps!)
  } else if (var == "st") {
    var00 = 'Surface Temperature'
    unit0 <- '(\u00B0C)'
    var0 <- "surface_temperature"
    var_breaks <- c(-10, seq(from = -2, to = 8, by = 0.5), 50) # if anom DOES NOT exist (straight temps!)
  }
  
  # Prepare title
  plot_title <- paste0(maxyr, " ", var00, " ", unit0)
  legend_title <- paste0(gsub(pattern = " ", replacement = "\n", x = var00), " ", unit0)
  anom_years <- temp <- haul[haul$SRVY %in% SRVY1, ] %>% 
    dplyr::select(SRVY, year) %>% 
    dplyr::distinct() %>%
    dplyr::group_by(SRVY) %>% 
    dplyr::summarize(min = min(year, na.rm = TRUE), 
                     max = max(year, na.rm = TRUE), 
                     nn = n()) %>% 
    dplyr::mutate(range = paste0(min, "-", max, " (", nn, " years)"))
  plot_title_anom <- paste0(maxyr,  " ", var00, ' Anomaly\n',
                           text_list(paste0(anom_years$SRVY, " ", anom_years$range), sep = ";"))
  legend_title_anom = paste0(var00, '\nAnomaly ', unit0)
  
  # Calculate var averages from pervious data
  dat_anom <- haul %>% 
    dplyr::rename(station = stationid) %>%
    dplyr::filter(!(is.na(station)) &
                    SRVY == ifelse(SRVY == "BS", c("EBS", "NBS"), SRVY)) %>%
    dplyr::rename("var" = dplyr::all_of(var0)) %>%
    dplyr::group_by(SRVY, stratum, station) %>% 
    dplyr::summarise(mean = mean(var, na.rm = TRUE))
  
  # Temperature data from google drive
  dat <- 
    readxl::read_xlsx(path = "./data/gap_survey_progression.xlsx", 
                      sheet = case, skip = 1) %>%
    janitor::clean_names() %>% 
    dplyr::rename("var" = all_of(var), 
                  "SRVY" = "srvy", 
                  "vessel_shape" = "vessel") %>%
    dplyr::filter(!is.na(SRVY)) %>% 
    dplyr::select(SRVY, stratum, station, var, date, vessel_shape) %>% 
    dplyr::left_join(x = ., 
                     y = dat_survreg, 
                     by = c("SRVY", "vessel_shape")) %>%
    dplyr::left_join(x = ., 
                     y = vessel_info, 
                     by = c("vessel_id")) %>% # add survey vessel data
    dplyr::left_join(
      x = ., 
      y = dat_anom, 
      by = c("SRVY", "stratum", "station")) %>% 
    dplyr::filter(!is.na(SRVY)) %>%
    dplyr::mutate(reg_lab = paste0(region_long, " ", reg_dates), 
                  var = as.numeric(var), 
                  anom = var-mean, # calculate anomalies
                  date = as.character(date)) %>%
    dplyr::arrange(date)
  
  # Daily plot
  file_end = "daily"
  create_vargridplots(dat = dat,
                      var_breaks = var_breaks, 
                      plot_title = plot_title,
                      plot_subtitle = plot_subtitle,
                      legend_title = legend_title,
                      dates0 = dates0, #"latest", # "all", #"2021-06-05",
                      region_akgfmaps = region_akgfmaps,
                      grid_stations = grid_stations,
                      file_end = file_end,
                      dir_in = dir_in,
                      dir_out = dir_out, 
                      dir_googledrive_upload = dir_googledrive_upload, 
                      make_gifs = TRUE, 
                      show_planned_stations = show_planned_stations)
  
  # Anomaly plot
  dat <- dat %>% 
    dplyr::rename(bt = var, 
            var = anom)
  var_breaks <- c(-10, seq(from = -2, to = 3, by = 0.5), 50) # for the anomaly data
  file_end = "anom"
  plot_title <- plot_title_anom
  legend_title = legend_title_anom
  create_vargridplots(dat = dat,
                      var_breaks = var_breaks, 
                      plot_title = plot_title,
                      plot_subtitle = plot_subtitle,
                      legend_title = legend_title,
                      dates0 = dates0, #"latest", # "all", #"2021-06-05",
                      region_akgfmaps = region_akgfmaps,
                      grid_stations = grid_stations,
                      file_end = file_end,
                      dir_in = dir_in,
                      dir_out = dir_out, 
                      dir_googledrive_upload = dir_googledrive_upload, 
                      make_gifs = TRUE, 
                      show_planned_stations = show_planned_stations)
  
}


#' Create temperature and anomaly plots
#'
#' @param dat The csv file from googledrive with data for the 'maxyr' being plotted or compared to in the anomally. 
#' @param plot_title A string of what the plot title should be. 
#' @param plot_subtitle A string of what the plot subtitle should be. 
#' @param legend_title A string of the temperature legend header. 
#' @param dates0 A string of either the date to be plotted to ("YYYY-MM-DD"), "all" which will plot all of the days where data was collected for in that year, or "latest" where only the most recent date's data will be plotted, or "none" where only the grid (no data) will bbe printed. Default = "latest". 
#' @param region_akgfmaps Inherited from the akgfmaps package. "bs.south" plots the EBS region, "bs.all" plots EBS and NBS regions. See the "select.region" argument under '?get_base_layers'. Default = "bs.all"
#' @param shapefile Select the shapefile you want to use for the grids. "Egrid_stations" plots the EBS region and "NEgrid_stations_df" plots EBS and NBS regions. Default = "NEgrid_stations".
#' @param height Numeric height of the figure in inches. default = 8.
#' @param width Numeric width of the figure in inches. default = 10.5.
#' @param file_end String. Will appear after "_" in the file name. 
#' @param make_gifs Logical. Default = TRUE means that a make_gifs of lastest and past files will be created.  
#' @param dir_out String. A path for the files to be saved to when the funciton is complete.
#' @param show_planned_stations Logical. Default = TURE. If FALSE, will not show planned stations. 
#' @param dir_googledrive_upload Upload documents to google drive. Here, enter the path on google drive where these files should be saved or "NULL" if you do not want it saved to google drive. Default = as_id("1iZKhka07guho68WVUn2TJ7iI521mdWEb"). https://drive.google.com/drive/folders/1iZKhka07guho68WVUn2TJ7iI521mdWEb?usp=sharing
#'
#' @export
create_vargridplots <- function(
    dat = NULL, 
    var_breaks, 
    plot_title = "",
    plot_subtitle = "",
    legend_title = "",
    dates0 = "latest",
    region_akgfmaps = "bs.all", 
    grid_stations, #  = "NEgrid_stations", 
    height = 8.5, 
    width = 10.5,
    file_end = "",
    make_gifs = TRUE,
    dir_out = "./", 
    dir_in = "./",
    show_planned_stations = TRUE,
    dir_googledrive_upload) {
  
  # dat0 <- dat
  
  # Create Directories
  dir.create(path = dir_out, showWarnings = FALSE)
  
  # set Base Layers
  survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")
  
  # define colors for var
  var_labels <- c()
  for(i in 2:c(length(var_breaks))) {
    var_labels <- c(var_labels, 
                   dplyr::case_when(
                     i==2 ~ paste0("\u2264 ",var_breaks[i]), # ,"\u00B0C" <=
                     i==(length(var_breaks)) ~ paste0("> ", var_breaks[i-1]), # , "\u00B0C"
                     TRUE ~ paste0("> ",
                                   var_breaks[i-1]," \u2013 ",var_breaks[i]) # ,"\u00B0C" "\u00B0"
                   ))
  }
  var_color <- colorRamps::matlab.like(length(var_labels))
  
  # bin var
  dat <- dat %>%
    dplyr::mutate(var_bin = base::cut(x = as.numeric(dat$var),
                                      breaks = var_breaks,
                                      labels = FALSE, 
                                      include.lowest = TRUE,
                                      right = FALSE) ) %>%
    dplyr::mutate(var_bin = base::factor(x = var_labels[var_bin], 
                                         levels = var_labels, 
                                         labels = var_labels) ) 

  # bind dat to grid_stations for plotting
  grid_stations <- sp::merge(x = grid_stations, 
                             y = dat %>%
                               dplyr::select(station, stratum, var_bin, reg_shapefile, 
                                             region_long, reg_dates, reg_lab, vessel_shape, date) %>% 
                               dplyr::distinct(), 
                             all.x = TRUE) 
  
  
  # Create new temperature maps
  if (as.character(dates0[1]) != "none") { # If you are not using any data from temp data
    if (length(dates0)) {
    date_entered<-sort(unique(grid_stations$date))
    date_entered<-date_entered[!is.na(date_entered)]
    } else {
      date_entered <- dates0
    }
  }
  
  fig_list<-list()
  if (length(dates0)>1) { # if there is a specific range of dates
    iterate <- which(as.character(date_entered) %in% dates0)
  } else if (dates0 == "none") { # If you are not using any data from temp data
    iterate <- 1 
  } else  if(dates0 == "all"){
    iterate <- 1:length(date_entered) # if you want to run all of plots for each date_entered: 
  } else if (dates0 == "latest") {
    iterate <- length(date_entered) # if you want to just run todays/a specific date:
  } else { # if you want to run a specific date
    iterate <- which(as.character(date_entered) == dates0)
  }
  
  for (i in iterate) {
    
    start_time <- Sys.time()
    
    survey_reg_col <- gray.colors(length(unique(dat$SRVY))+2)
    survey_reg_col <- survey_reg_col[-((length(survey_reg_col)-1):length(survey_reg_col))]
    
    grid_stations_plot<-grid_stations
    dat_plot <- dat
    if (as.character(dates0[1]) == "none") { # If you ARE NOT using any data from temp data
      max_date <- NA
      grid_stations_plot$var_bin<-NA  # only use dates including this date and before this date
    } else { # If you ARE using any data from temp data
      max_date <- date_entered[i]

      # only use dates including this date and before this date
      dat_plot$var[as.Date(dat_plot$date)>as.Date(max_date)]<-NA 
      grid_stations_plot$var_bin[as.Date(grid_stations_plot$date)>as.Date(max_date)]<-NA 
      # only use dates including 2 days before this date and before this date, so we can see the planned progression
      if (i != iterate[length(iterate)]) {
        dat_plot$vessel_shape[as.Date(dat_plot$date)>as.Date(max_date)+2]<-NA
        dat_plot$date[as.Date(dat_plot$date)>as.Date(max_date)+2]<-NA
        grid_stations_plot$vessel_shape[as.Date(grid_stations_plot$date) > as.Date(max_date)+2]<-NA
        grid_stations_plot$date[as.Date(grid_stations_plot$date) > as.Date(max_date)+2]<-NA
      }
    }
    
    # if (dates0 != "none") { # If you ARE using any data from temp data
    #   max_date <- date_entered[i]
    #   if (length(max_date)==0) {
    #     max_date <- (min(dat00$date, na.rm = TRUE)-1) # The earliest planned date
    #     # max_date <- format(max_date, "%Y-%m-%d")
    #   }
    #   grid_stations_plot$var_bin[grid_stations_plot$date>max_date]<-NA  # only use dates including this date and before this date
    # } else { # If you ARE NOT using any data from temp data
    #   max_date <- NA
    #   grid_stations_plot$var_bin<-NA  # only use dates including this date and before this date
    # }
    
    
    # separate out the data for the temperature and planned stations if there are planned stations listed
    if (show_planned_stations & 
        sum(is.na(dat_plot$var) & !is.na(dat_plot$vessel_shape))>0) {
      
      # planned stations
      loc <- dat_plot %>% 
        dplyr::filter(is.na(var) & !is.na(vessel_shape)) %>% 
        dplyr::mutate(planned = "Y")
      
      dat_planned <- as(grid_stations_plot, 'Spatial')
      
      dat_planned <- 
        cbind.data.frame(station = dat_planned$station, 
                         stratum = dat_planned$stratum, 
                         sp::coordinates(dat_planned)) %>% 
        dplyr::rename(lon = "1", 
                      lat = "2") %>%
        dplyr::left_join(x = ., 
                         y = loc %>% 
                           dplyr::select(stratum, station, planned, vessel_shape, vessel_name, date), 
                         by = c("stratum", "station")) %>%
        dplyr::filter(!is.na(planned)) %>% 
        dplyr::mutate(lab = "")
      
      for (i in 1:length(unique(dat_planned$vessel_name))) {
        vess <- unique(dat_planned$vessel_name)[i]
        vess_date <- as.Date(range(dat_planned$date[dat_planned$vessel_name %in% vess]))
        vess_date <- ifelse(vess_date[1] == vess_date[2], 
                            format(vess_date[1], 
                                   "%b %d"), 
                            paste(format(vess_date, 
                                         "%b %d"), collapse = "-"))
        dat_planned$lab[dat_planned$vessel_name %in% vess] <-
          paste0(vess, "\n(", vess_date, ")")
      }
      
      # if there are other vessels in the data than there are planned stations for, add a row back for that vessel so we can plot it
      all_vess <- unique(dat_plot$vessel_shape)
      all_vess <- all_vess[!is.na(all_vess)]
      plan_vess <- unique(dat_planned$vessel_shape)
      plan_vess <- plan_vess[!is.na(plan_vess)]
      if (length(all_vess) != length(plan_vess)) {
        # find which vessels are not represented in dat_planned
        temp <- c(setdiff(all_vess, plan_vess), 
                  setdiff(plan_vess, all_vess))
        # temp <- all_vess[!is.na(temp)]
        temp1 <- rep_len(x = NA, length.out = ncol(dat_planned))
        names(temp1) <- names(dat_planned)
        dat_planned <- dplyr::bind_rows(dat_planned, 
                                        temp1 %>% 
                                          t() %>%
                                          data.frame() %>%
                                          dplyr::mutate(vessel_shape = temp, 
                                                        vessel_name = unique(dat_plot$vessel_name[dat_plot$vessel_shape %in% temp]),
                                                        planned = "N", 
                                                        lab = unique(dat_plot$vessel_name[dat_plot$vessel_shape %in% temp], "\n")))
      }
      
    } else if (show_planned_stations) { # if we are sharing planned stations but there aren't any to share
      dat_planned <- data.frame(station = NA, 
                                stratum = NA, 
                                lon = 0, 
                                lat = 0, 
                                planned = "N", 
                                vessel_shape = unique(dat_plot$vessel_shape)[!is.na(unique(dat_plot$vessel_shape))], 
                                vessel_name = unique(dat_plot$vessel_name)[!is.na(unique(dat_plot$vessel_name))], 
                                date = NA, 
                                lab = paste0(unique(dat_plot$vessel_name)[!is.na(unique(dat_plot$vessel_name))], "\n"))
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
              subtitle = plot_subtitle)  +
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
        axis.title=element_text(size=14) )  +
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
                                     format(x = as.Date(max_date), format = "%B %d, %Y"))), 
               color = "black", size = 4) 
    
    # if there is more than one survey region on the plot, add survey region to the plot
    if (length(dat_plot$reg_shapefile)>1) {
      gg <- gg  +
        geom_sf(data = survey_area$survey.area, 
                aes(color = survey_area$survey.area$SURVEY), 
                fill = NA, 
                size = 2,
                show.legend = TRUE) +
        scale_color_manual(name = "Survey Region", 
                           values = c(alpha(survey_reg_col, 0.7)), 
                           breaks = rev(unique(dat_plot$reg_shapefile)), 
                           labels = rev(unique(dat_plot$reg_lab))) 
    }
    
    # Add temperature squares
    if (as.character(dates0[1]) != "none") { # If you are using any data from temp data
      gg <- gg +
        ggplot2::geom_sf(data = grid_stations_plot, # %>% 
                           # dplyr::filter(stratum == "212"), 
                         aes(group = station, 
                             fill = var_bin), 
                         colour = "grey50",
                         show.legend = legend_title) +
        ggplot2::scale_fill_manual(name = legend_title,
                                   values = var_color, 
                                   labels = var_labels,
                                   drop = F,
                                   na.translate = F)
      
      # if we are showing planned stations
      if (show_planned_stations #& 
          # length(is.na(dat_planned$station)) != nrow(dat_planned)
          ) {
        
        gg <- gg +
          geom_point(data = dat_planned, 
                     mapping = aes(x = lon, y = lat, shape = factor(vessel_shape)), 
                     size = 4, na.rm = TRUE) +
          scale_shape_manual(
            name = "Planned Stations",
            values = sort(unique(dat_planned$vessel_shape)),
            breaks = sort(unique(dat_planned$vessel_shape)), 
            labels = sort(unique(dat_planned$lab))) +
          
          guides(
            fill = guide_legend(ncol=ifelse(length(var_breaks)>15, 2, 1), # tempertures # in case you want to have 2+ columns for the legend!
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
                                                     linetype = c("blank")))  )
      } else { # we are not showing planned stations
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
    # } else {
    #   
    #   if (show_planned_stations) {
    #     gg <- gg +
    #       geom_sf(data = grid_stations_plot, 
    #               aes(group = STATION_ID), 
    #               fill = NA,
    #               colour = "grey50") + 
    #       guides(
    #         # survey regions
    #         colour = guide_legend(order = 2, 
    #                               override.aes = list(fill = survey_reg_col, 
    #                                                   size = 2#, 
    #                                                   # color = "white"
    #                               ))) 
    #   }
    }
    # gg <- gg +
    #   coord_sf(xlim = extrap.box[1:2], 
    #            ylim = extrap.box[3:4])
    gg <- gg +
      coord_sf(xlim = c(extent(grid_stations)[1],
                        extent(grid_stations)[2]),
               ylim = c(extent(grid_stations)[3] ,
                        extent(grid_stations)[4]))
    
    gg <- ggdraw(gg) +
      draw_image(image = paste0(dir_in, "img/noaa-fish-wide.png"), 
                 x = .35, y = .37, 
                 scale = .15
                 )
    
    # gg <- ggdraw(gg) +
    #   draw_image(image = paste0(dir_in, "img/noaa-50th-logo.png"), 
    #              x = 0, y = 0,
    #              hjust = -4.12, vjust = -.45,
    #              width = .19)
    
    # Save plots
    filename0 <- paste0(dir_out, '/',
                        ifelse(as.character(dates0[1]) == "none", "", 
                               max_date), 
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
    
    if (make_gifs | as.character(dates0[1]) != "none") {
      create_vargridplots_gif(file_end = file_end, 
                              max_date = max_date,
                              dir_out = dir_out)
    }
    
    if (!(is.null(dir_googledrive_upload))) {
      
      drive_upload(
        media = paste0(filename0,'.png'), 
        path = dir_googledrive_upload, 
        overwrite = TRUE)
      
      drive_upload(
        media = paste0(filename0,'.pdf'), 
        path = dir_googledrive_upload, 
        overwrite = TRUE)
      
      # change each day of the survey
      if (make_gifs | as.character(dates0[1]) != "none") {
        drive_upload(
          media = paste0(filename0,'.gif'), 
          path = dir_googledrive_upload, 
          overwrite = TRUE)
      }
    }
    
    end_time <- Sys.time()
    print((end_time - start_time))
    
    # fig_list<-c(fig_list, list(gg))
    # names(fig_list)[length(fig_list)] <- paste(max_date)
    
  }
  
  # Save all of the figures in case you need to print them again in a different way but dont want to rerun everything above :)
  # this is a MASSIVE file, so maybe run this sparingly?
  
  # HeatPlot_all<-fig_list
  # save(HeatPlot_all, file = paste0(dir_out, '/HeatPlot_all.RData'))
  
}


create_vargridplots_gif<-function(file_end, max_date, dir_out) {
  
  imgs <- list.files(path = dir_out, 
                       pattern = paste0(as.Date(max_date), "_", file_end, ".png"), 
                       full.names = TRUE)
  
  temp <- strsplit(x = list.files(path = dir_out, pattern = paste0("_", file_end, ".gif")), split = "_")
  
  if (length(temp) != 0) {
    temp <- as.Date(sort(sapply(temp,"[[",1)))
    temp <- max(temp[as.Date(temp) < as.Date(max_date)])
    imgs <- c(list.files(path = dir_out, 
                        pattern = paste0(as.character(temp), "_", file_end, ".gif"), 
                        full.names = TRUE), 
             imgs)
  }
  
  # imgs <- sort(list.files(path = dir_out,
  #                    pattern = paste0(file_end, ".png"),
  #                    full.names = TRUE))
  #
  # gifs <- sort(list.files(path = dir_out,
  #                         pattern = paste0(file_end, ".gif"),
  #                         full.names = TRUE))
  #
  # imgs<-c(gifs[length(gifs)], imgs[length(imgs)])

  # as.numeric(strsplit(imgs, '\\/')[[1]][-1])

  # dates_ploted <- gsub(pattern = paste0("_", file_end, ".png"),
  #                      replacement = "",
  #                      x = unlist(lapply(
  #                        lapply(imgs, function(x)
  #                          (strsplit(x, '\\/')[[1]][-1])), tail, n = 1L)))
  # dates_ploted <- strptime(x = dates_ploted, format = "%Y-%m-%d")
  # max_date1 <- strptime(x = max_date,
  #                       format = "%Y-%m-%d")
  # 
  # dates_ploted_idx <- which(dates_ploted <= max_date1)
  
  img_list <- lapply(imgs, #imgs[dates_ploted_idx], 
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
    path = paste0(dir_out, "/", max_date, "_", 
                  file_end, ".gif") )
  
}


