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
  "ggsn",
  
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
  
  # "tinytex",
  
  # tidyverse, 
  "broom", 
  "readr", 
  "dplyr", 
  "glue",
  "ggplot2",
  "rmarkdown"
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

# Functions --------------------------------------------------------------------

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


make_plot_wrapper <- function(maxyr, 
                              SRVY, 
                              haul, 
                              dat = NULL,
                              dat_survreg, 
                              var = "bt", 
                              dir_googledrive_upload, 
                              dates0, 
                              survey_area, 
                              plot_subtitle = "", 
                              show_planned_stations, 
                              data_source = "gd", 
                              plot_anom = TRUE) {
  
  case <- paste0(maxyr, "_", SRVY)
  dir_out <- paste0("./output/", case, "/")
  
  SRVY1 <- SRVY
  create_vargridplots <- create_vargridplots_ai
  if (SRVY == "BS") {
    SRVY1 <- c("EBS", "NBS")
  }
  if (SRVY %in% c("BS", "EBS", "NBS")) {
    create_vargridplots <- create_vargridplots_bs
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
                    year <= maxyr &
                    SRVY == ifelse(SRVY == "BS", c("EBS", "NBS"), SRVY)) %>%
    dplyr::rename("var" = dplyr::all_of(var0)) %>%
    dplyr::group_by(SRVY, stratum, station) %>% 
    dplyr::summarise(mean = mean(var, na.rm = TRUE))
  
  if (data_source == "gd") { # Temperature data from google drive
    dat <- 
      readxl::read_xlsx(path = "./data/gap_survey_progression.xlsx", 
                        sheet = case, skip = 1) %>%
      janitor::clean_names() %>% 
      dplyr::rename("var" = all_of(var), 
                    "SRVY" = "srvy", 
                    "vessel_shape" = "vessel") %>%
      dplyr::filter(#!is.na(SRVY) &
                      SRVY %in% SRVY1) %>%
      dplyr::select(SRVY, stratum, station, var, date, vessel_shape) 
    
  } else if (data_source == "haul") {
    dat <- haul %>% 
      dplyr::filter(year == maxyr &
                      SRVY %in% SRVY1 &
                      !is.na(dplyr::all_of(var))) %>% 
      dplyr::rename(var = all_of(var0), 
                    station = stationid, 
                    date = start_time) #%>% 
      # dplyr::select(-vessel_id) 
    dat$date <- as.character(as.Date(sapply(strsplit(x = dat$date, split = " ", fixed = TRUE),`[[`, 1), "%m/%d/%Y"))
    for (ii in 1:length(unique(dat$SRVY))){
      temp <- unique(dat_survreg$SRVY)[ii]
      dat_survreg$reg_dates[dat_survreg$SRVY == temp] <- paste0("\n(", 
                                    format(x = min(as.Date(dat$date[dat$SRVY == temp]), na.rm = TRUE), "%B %d"),
                                    " - ", 
                                    format(x = max(as.Date(dat$date[dat$SRVY == temp]), na.rm = TRUE), "%B %d"), ")")
    }
    dat <- dat %>% 
      dplyr::left_join(
        x = ., 
        y = dat_survreg %>% 
          dplyr::select(vessel_id, vessel_shape) %>%
          dplyr::distinct(), 
        by = "vessel_id")  %>% 
      dplyr::select(SRVY, stratum, station, var, date, vessel_shape)
    
  }
  
  dat <- dat %>% 
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
    dplyr::mutate(reg_lab = paste0(region_long, " ", reg_dates), 
                  var = as.numeric(var), 
                  anom = var-mean, # calculate anomalies
                  date = as.character(date)) %>%
    dplyr::arrange(date)
  
  survey_area$survey.grid <- survey_area$survey.grid %>% 
    dplyr::mutate(id = paste0(stratum, "_", station)) %>% 
    dplyr::left_join(x = ., 
                     y = dat  %>%
                       dplyr::mutate(in_survey = TRUE, 
                                     id = paste0(stratum, "_", station)) %>% 
                      dplyr::select(in_survey, id),
                      by = c("id")) %>% 
    dplyr::filter(!is.na(in_survey)) %>%
    dplyr::select(-in_survey)
  
  # Daily plot
  file_end = "daily"
  create_vargridplots(dat = dat,
                      var_breaks = var_breaks, 
                      plot_title = plot_title,
                      plot_subtitle = plot_subtitle,
                      legend_title = legend_title,
                      dates0 = dates0, #"latest", # "all", #"2021-06-05",
                      survey_area = survey_area,
                      file_end = file_end,
                      dir_in = dir_in,
                      dir_out = dir_out, 
                      dir_googledrive_upload = dir_googledrive_upload, 
                      make_gifs = TRUE, 
                      data_source = data_source,
                      show_planned_stations = show_planned_stations)
  
  if (plot_anom) {
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
                      survey_area = survey_area,
                      file_end = file_end,
                      dir_in = dir_in,
                      dir_out = dir_out, 
                      dir_googledrive_upload = dir_googledrive_upload, 
                      make_gifs = TRUE, 
                      data_source = data_source,
                      show_planned_stations = show_planned_stations)
  }
}



make_grid_wrapper<-function(maxyr, 
                            SRVY, 
                            haul, 
                            dat_survreg, 
                            dir_googledrive_upload,  
                            survey_area, 
                            plot_subtitle = "", 
                            data_source = "gd") {
  
  case <- paste0(maxyr, "_", SRVY)
  dir_out <- paste0("./output/", case, "/")
  
  file_end = "grid"
  dates0 <- "none"
  show_planned_stations = FALSE
  make_gifs = FALSE
  show_planned_stations = FALSE

  SRVY1 <- SRVY
  create_vargridplots <- create_vargridplots_ai
  if (SRVY == "BS") {
    SRVY1 <- c("EBS", "NBS")
  }
  if (SRVY %in% c("BS", "EBS", "NBS")) {
    create_vargridplots <- create_vargridplots_bs
  }
  
  plot_title <- "Survey Grid"
  legend_title <- ""
  
  if (data_source == "gd") { # Temperature data from google drive
    dat <- 
      readxl::read_xlsx(path = "./data/gap_survey_progression.xlsx", 
                        sheet = case, skip = 1) %>%
      janitor::clean_names() %>% 
      dplyr::rename(#"var" = all_of(var), 
                    "SRVY" = "srvy", 
                    "vessel_shape" = "vessel")  %>%
      dplyr::mutate(var = NA) %>%
      dplyr::filter(!is.na(SRVY) & 
                      SRVY %in% SRVY1) %>% 
      dplyr::select(SRVY, stratum, station, date, vessel_shape) 
  } else if (data_source == "haul") {
    dat <- haul %>% 
      dplyr::filter(year == maxyr &
                      SRVY %in% SRVY1) %>% 
      dplyr::rename(station = stationid, 
                    date = start_time) %>%
      dplyr::mutate(var = NA)
    # dplyr::select(-vessel_id) 
    dat$date <- as.character(as.Date(sapply(strsplit(x = dat$date, split = " ", fixed = TRUE),`[[`, 1), "%m/%d/%Y"))
    for (ii in 1:length(unique(dat$SRVY))){
      temp <- unique(dat_survreg$SRVY)[ii]
      dat_survreg$reg_dates[dat_survreg$SRVY == temp] <- paste0("\n(", 
                                                                format(x = min(as.Date(dat$date[dat$SRVY == temp]), na.rm = TRUE), "%B %d"),
                                                                " - ", 
                                                                format(x = max(as.Date(dat$date[dat$SRVY == temp]), na.rm = TRUE), "%B %d"), ")")
    }
    dat <- dat %>% 
      dplyr::left_join(
        x = ., 
        y = dat_survreg %>% 
          dplyr::select(vessel_id, vessel_shape) %>%
          dplyr::distinct(), 
        by = "vessel_id")  %>% 
      dplyr::select(SRVY, stratum, station, var, date, vessel_shape)
  }

  dat <- dat %>%
    dplyr::filter(!is.na(SRVY))  %>% 
    dplyr::left_join(x = ., 
                     y = dat_survreg, 
                     by = c("SRVY", "vessel_shape")) %>%
    dplyr::left_join(x = ., 
                     y = vessel_info, 
                     by = c("vessel_id")) %>% # add survey vessel data
    dplyr::mutate(reg_lab = paste0(region_long, " ", reg_dates), 
                  var_bin = NA) %>%
    dplyr::arrange(date)
  
  # region_akgfmaps = "bs.all"
  # survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")
  # survey_area$survey.grid <- survey_area$survey.grid %>% 
  #   sf::st_transform(x = ., survey_area$crs$input) %>%
  #   # dplyr::filter(!(STATION %in% c("DD-09", "AA-10"))) %>% 
  #   dplyr::rename(station = STATIONID) %>%
  #   sp::merge(x = ., 
  #             y = haul %>%
  #               dplyr::rename(station = stationid) %>% 
  #               dplyr::select(station, stratum) %>% 
  #               dplyr::distinct(), 
  #             all.x = TRUE) 
  
  create_vargridplots(dat = dat,
                      var_breaks = var_breaks, 
                      plot_title = plot_title,
                      plot_subtitle = plot_subtitle,
                      legend_title = ,
                      dates0 = dates0,
                      survey_area = survey_area,
                      file_end = file_end,
                      dir_in = dir_in,
                      dir_out = dir_out, 
                      dir_googledrive_upload = dir_googledrive_upload, 
                      make_gifs = make_gifs, 
                      show_planned_stations = show_planned_stations)
  
}


#' Create temperature and anomaly plots
#'
#' @param dat The csv file from googledrive with data for the 'maxyr' being plotted or compared to in the anomally. 
#' @param plot_title A string of what the plot title should be. 
#' @param plot_subtitle A string of what the plot subtitle should be. 
#' @param legend_title A string of the temperature legend header. 
#' @param dates0 A string of either the date to be plotted to ("YYYY-MM-DD"), "all" which will plot all of the days where data was collected for in that year, or "latest" where only the most recent date's data will be plotted, or "none" where only the grid (no data) will bbe printed. Default = "latest". 
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
create_vargridplots_bs <- function(
    dat = NULL, 
    var_breaks, 
    plot_title = "",
    plot_subtitle = "",
    legend_title = "",
    dates0 = "latest",
    survey_area, 
    height = 8.5, 
    width = 10.5,
    file_end = "",
    make_gifs = TRUE,
    dir_out = "./", 
    dir_in = "./",
    show_planned_stations = TRUE,
    data_source = "gd",
    dir_googledrive_upload) {
  
  # Create Directories
  dir.create(path = dir_out, showWarnings = FALSE)
  
  # set Base Layers
  # survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")
  survey_reg_col <- gray.colors(length(unique(dat$SRVY))+2) # region colors
  survey_reg_col <- survey_reg_col[-((length(survey_reg_col)-1):length(survey_reg_col))]
  survey_area$survey.area <- sp::merge(
    x = survey_area$survey.area, 
    y = dat %>% 
      dplyr::select(reg_shapefile, region_long, reg_lab) %>% 
      dplyr::filter(!is.na(reg_shapefile)) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(survey_reg_col = c(alpha(survey_reg_col, 0.7))) %>%
      dplyr::rename(SURVEY = reg_shapefile), 
    all.x = TRUE) %>% 
    dplyr::arrange(desc(SURVEY))
  
  if (as.character(dates0[1]) != "none") {
    
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
    
    # Create new temperature maps
    # if (length(dates0)>1) {
    date_entered<-sort(unique(dat$date))
    date_entered<-date_entered[!is.na(date_entered)]
    # } else {
    #   date_entered <- dates0
    # }
  }
  
  # bind dat to survey_area$survey.grid for plotting
  survey_area$survey.grid <- sp::merge(x = survey_area$survey.grid, 
                                       y = dat %>%
                                         dplyr::select(station, stratum, var_bin, reg_shapefile, 
                                                       region_long, reg_dates, reg_lab, vessel_shape, date) %>% 
                                         dplyr::distinct(), 
                                       all.x = TRUE) 
  
  # fig_list<-list()
  if (length(dates0)>1) { # if there is a specific range of dates
    iterate <- which(as.character(date_entered) %in% dates0)
  } else if (dates0 == "none") { # If you are not using any data from temp data
    iterate <- 1 
  } else if (dates0 == "all") {
    iterate <- 1:length(date_entered) # if you want to run all of plots for each date_entered: 
  } else if (dates0 == "latest") {
    iterate <- length(date_entered) # if you want to just run todays/a specific date:
  } else { # if you want to run a specific date
    iterate <- which(as.character(date_entered) %in% as.character(dates0))
  }
  
  for (i in iterate) {
    
    start_time <- Sys.time()
    
    grid_stations_plot<-survey_area$survey.grid
    dat_plot <- dat
    
    if (as.character(dates0[1]) == "none") { # If you ARE NOT using any data from temp data
      
      max_date <- NA
      grid_stations_plot$var_bin<-NA  # only use dates including this date and before this date
      
    } else {
      
      max_date <- date_entered[i]
      
      # only use dates including this date and before this date
      dat_plot$var[as.Date(dat_plot$date)>as.Date(max_date)]<-NA 
      grid_stations_plot$var_bin[as.Date(grid_stations_plot$date)>as.Date(max_date)]<-NA 
      # only use dates including 1 days before this date and before this date, so we can see the planned progression
      # if (date_entered[i] != date_entered[length(date_entered)]) {
      dat_plot$vessel_shape[as.Date(dat_plot$date)>as.Date(max_date)+1]<-NA
      dat_plot$date[as.Date(dat_plot$date)>as.Date(max_date)+1]<-NA
      grid_stations_plot$vessel_shape[as.Date(grid_stations_plot$date) > as.Date(max_date)+1]<-NA
      grid_stations_plot$date[as.Date(grid_stations_plot$date) > as.Date(max_date)+1]<-NA
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
        
        all_vess <- unique(dat_plot$vessel_shape)
        all_vess <- all_vess[!is.na(all_vess)]
        for (ii in 1:length(all_vess)) {
          vess <- all_vess[ii]
          if (sum(unique(dat_planned$vessel_shape) %in% vess)==0) {
            temp1 <- rep_len(x = NA, length.out = ncol(dat_planned))
            names(temp1) <- names(dat_planned)
            dat_planned <- dplyr::bind_rows(dat_planned, 
                                            temp1 %>% 
                                              t() %>%
                                              data.frame() %>%
                                              dplyr::mutate(vessel_shape = vess, 
                                                            vessel_name = unique(dat_plot$vessel_name[dat_plot$vessel_shape %in% vess]), 
                                                            lon = 5000000, 
                                                            lat = 5000000,
                                                            planned = "N")) 
            vess_date <- paste0("\n ")
          } else {
            vess_date <- as.Date(range(as.Date(dat_planned$date[dat_planned$vessel_shape %in% vess])))
            vess_date <- ifelse(vess_date[1] == vess_date[2],
                                format(vess_date[1],
                                       "%b %d"),
                                paste(format(vess_date,
                                             "%b %d"), collapse = "-"))
            vess_date <- paste0("\n(", vess_date, ")")
          }
          
          dat_planned$lab[dat_planned$vessel_shape %in% vess] <-
            paste0(unique(dat_plot$vessel_name[dat_plot$vessel_shape %in% vess]), vess_date)
        }
      } else if (show_planned_stations) { # if we are sharing planned stations but there aren't any to share
        dat_planned <- data.frame(station = NA, 
                                  stratum = NA, 
                                  lon = 5000000, 
                                  lat = 5000000, 
                                  planned = "N", 
                                  vessel_shape = unique(dat_plot$vessel_shape)[!is.na(unique(dat_plot$vessel_shape))], 
                                  vessel_name = unique(dat_plot$vessel_name)[!is.na(unique(dat_plot$vessel_name))], 
                                  date = NA, 
                                  lab = paste0(unique(dat_plot$vessel_name)[!is.na(unique(dat_plot$vessel_name))], "\n "))
      }
    }
    
    gg <- ggplot() +
      geom_sf(data = survey_area$akland, fill = "white") + # AK Map (thanks {akgfmaps}!)
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
        panel.background = element_rect(fill = "grey90"),
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
        axis.title=element_text(size=14) )
    
    if (as.character(dates0[1]) == "none") {
      temp <-survey_area$place.labels[survey_area$place.labels$type == "bathymetry",]
      gg <- gg +
        ggplot2::geom_sf(data = survey_area$survey.grid, 
                         # aes(group = station), 
                         colour = "grey50",
                         show.legend = legend_title) +
        geom_sf(data = survey_area$bathymetry) +
        guides(colour = guide_legend(override.aes = list(fill = survey_area$survey.area$survey_reg_col)))  # survey regions
      if (nrow(temp)>0) {
        gg <- gg +
          
          # geom_text(data = temp, mapping = aes(x = x, y = y, label = lab), 
          #           size = 4, hjust=0, vjust=1, fontface=2, angle = 90)
          annotate(geom = "text", x = temp$x, y = temp$y, label = temp$lab,
                   color = "darkblue", fontface="bold", angle = 0)
      }
      
    }
    
    # if (length(dat_plot$reg_shapefile)>1) {
    gg <- gg  +
      geom_sf(data = survey_area$survey.area, 
              aes(color = SURVEY), 
              fill = NA,
              size = 2,
              show.legend = TRUE) +
      scale_color_manual(name = "Survey Region", 
                         values = survey_area$survey.area$survey_reg_col,  
                         breaks = survey_area$survey.area$SURVEY, 
                         labels = survey_area$survey.area$reg_lab) 
    # }
    
    # Add temperature squares
    # } else 
    if (as.character(dates0[1]) != "none") { # If you are using any data from temp data
      
      gg <- gg +
        ggplot2::geom_sf(data = grid_stations_plot, 
                         aes(#group = region, 
                           fill = var_bin), 
                         colour = "grey50",
                         show.legend = legend_title) +
        ggplot2::scale_fill_manual(name = legend_title,
                                   values = var_color, 
                                   labels = var_labels,
                                   drop = F,
                                   na.translate = F) +
        ggspatial::coord_sf(
          xlim = c(extent(grid_stations_plot)), # %in% c(213, 511),])[1:2]),
          ylim = c(extent(grid_stations_plot))) #+ # %in% c(213, 511),])[3:4])) +
      #   ggplot2::facet_wrap(~ region, ncol = 2)
      # 
      # library(tmap)
      # 
      # grid_stations_plot %>% 
      #   group_by(region) %>% 
      #   tm_shape() +
      #   tm_polygons('var_bin',
      #               title = legend_title,
      #               style = 'cont', col = 2) +
      #   tm_facets('region')
      #   # tm_layout(legend.outside.position =  "bottom", # legend outside below
      #   #           legend.position = c(.8, 1.1))        # manually position legend
      # 
      # 
      # maps_shared <- map(.x = unique(grid_stations_plot$region), 
      #                    .f = function(x) grid_stations_plot %>% 
      #                      dplyr::filter(region == x) %>% 
      #                      # st_join(acled) %>% 
      #                      group_by(region) %>% 
      #                      ggplot(aes(fill = var_bin)) +
      #                      geom_sf(lwd = NA) #+
      #                      # scale_fill_continuous(limits = levels(grid_stations_plot$var_bin)[c(1, length(levels(grid_stations_plot$var_bin)))],
      #                      #                       name = 'PKO targeting\nevents (logged)') 
      #                    ) #+
      #                      # theme_rw() +
      #                      # theme(axis.text = element_blank(),
      #                      #       axis.ticks = element_blank()))
      # 
      # plot_grid(plotlist = c(map(.x = maps_shared,
      #                            .f = function(x) x + theme(legend.position = 'none')),
      #                        list(get_legend(maps_shared[[1]]))),
      #           labels = LETTERS[1:5], label_size = 10, nrow = 2)
      
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
            fill = guide_legend(ncol=ifelse(length(var_breaks)>15, 2, 1), # temperatures # in case you want to have 2+ columns for the legend!
                                override.aes = list(colour = c("white"),
                                                    size = 0),
                                order = 1),
            colour = guide_legend(order = 2, # survey regions
                                  override.aes = list(#color = "white", 
                                    fill = survey_area$survey.area$survey_reg_col#,
                                    # size = 2#,
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
                                  override.aes = list(fill = survey_area$survey.area$survey_reg_col#, 
                                                      # size = 2#, 
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
    
    
    # if (sum(names(grid_stations_plot) == "region")>0) {
    #   for (i in 1:)
    #   gg1 <- gg + grid_stations_plot
    #     
    # } else {
    gg <- gg +
      ggspatial::coord_sf(
        xlim = c(extent(survey_area$survey.grid)[1:2]),
        ylim = c(extent(survey_area$survey.grid)[3:4])) 
    # }
    
    gg <- gg +
      ggsn::scalebar(data = survey_area$survey.grid,
                     location = "bottomleft",
                     dist = 100,
                     dist_unit = "nm",
                     transform = FALSE,
                     st.dist = 0.03,
                     # height = 0.02,
                     st.bottom = FALSE,
                     # st.size = 3, # 2.5
                     model = survey_area$crs)  +
      annotate("text", 
               x = quantile(extent(survey_area$survey.grid)[1]:extent(survey_area$survey.grid)[2], .9), 
               y = quantile(extent(survey_area$survey.grid)[3]:extent(survey_area$survey.grid)[4], .7), 
               label = "Alaska", 
               color = "black", size = 10) +
      annotate("text", 
               x = quantile(extent(survey_area$survey.grid)[1]:extent(survey_area$survey.grid)[2], .12), 
               y = quantile(extent(survey_area$survey.grid)[3]:extent(survey_area$survey.grid)[4], .15), 
               label = ifelse(is.na(max_date), 
                              "", 
                              ifelse(min(as.Date(dat$date), na.rm = TRUE) == max_date, 
                                     paste0(format(x = min(as.Date(dat_plot$date), na.rm = TRUE), "%B %d, %Y")), 
                                     paste0(format(x = min(as.Date(dat_plot$date), na.rm = TRUE), "%B %d"), 
                                            " \u2013\n", 
                                            format(x = as.Date(max_date), format = "%B %d, %Y")))), 
               color = "black", size = 5, fontface=2) 
    
    gg <- ggdraw(gg) +
      draw_image(image = paste0(dir_in, "img/noaa-fish-wide.png"), # "img/noaa-50th-logo.png"
                 x = .37, y = .43, # x = 0, y = 0, hjust = -4.12, vjust = -.45, width = .19
                 scale = .15 )
    
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
    
    if (file_end %in% c("grid", "daily")){
      rmarkdown::render(paste0("./code/template.Rmd"),
                        output_dir = dir_out,
                        output_file = paste0(".", dir_out, filename0, ".pdf"))
      file.remove(list.files(path = "./code/", pattern = ".log", full.names = TRUE))

    } else if (file_end == "anom"){
      ggsave(paste0(filename0,'.pdf'),
             height = height,
             width = width,
             plot=gg,
             device="pdf") # pdfs are great for editing later
    }
    
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


#' Create temperature and anomaly plots
#'
#' @param dat The csv file from googledrive with data for the 'maxyr' being plotted or compared to in the anomally. 
#' @param plot_title A string of what the plot title should be. 
#' @param plot_subtitle A string of what the plot subtitle should be. 
#' @param legend_title A string of the temperature legend header. 
#' @param dates0 A string of either the date to be plotted to ("YYYY-MM-DD"), "all" which will plot all of the days where data was collected for in that year, or "latest" where only the most recent date's data will be plotted, or "none" where only the grid (no data) will bbe printed. Default = "latest". 
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
create_vargridplots_ai <- function(
    dat = NULL, 
    var_breaks, 
    plot_title = "",
    plot_subtitle = "",
    legend_title = "",
    dates0 = "latest",
    survey_area, 
    height = 6, 
    width = 10.5,
    file_end = "",
    make_gifs = TRUE,
    dir_out = "./", 
    dir_in = "./",
    show_planned_stations = TRUE,
    data_source = "gd",
    dir_googledrive_upload) {
  
  # Create Directories
  dir.create(path = dir_out, showWarnings = FALSE)
  
  # set Base Layers
  # survey_area <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")
  survey_reg_col <- gray.colors(length(unique(dat$SRVY))+2) # region colors
  survey_reg_col <- survey_reg_col[-((length(survey_reg_col)-1):length(survey_reg_col))]
  survey_area$survey.area <- sp::merge(
    x = survey_area$survey.area, 
    y = dat %>% 
      dplyr::select(reg_shapefile, region_long, reg_lab) %>% 
      dplyr::filter(!is.na(reg_shapefile)) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(survey_reg_col = c(alpha(survey_reg_col, 0.7))) %>%
      dplyr::rename(SURVEY = reg_shapefile), 
    all.x = TRUE) %>% 
    dplyr::arrange(desc(SURVEY))
  
  if (as.character(dates0[1]) != "none") {

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
  
  # Create new temperature maps
    # if (length(dates0)>1) {
    date_entered<-sort(unique(dat$date))
    date_entered<-date_entered[!is.na(date_entered)]
    # } else {
    #   date_entered <- dates0
    # }
  }
  
  # bind dat to survey_area$survey.grid for plotting
  survey_area$survey.grid <- sp::merge(x = survey_area$survey.grid, 
                                       y = dat %>%
                                         dplyr::select(station, stratum, var_bin, reg_shapefile, 
                                                       region_long, reg_dates, reg_lab, vessel_shape, date) %>% 
                                         dplyr::distinct(), 
                                       all.x = TRUE) 

  # fig_list<-list()
  if (length(dates0)>1) { # if there is a specific range of dates
    iterate <- which(as.character(date_entered) %in% dates0)
  } else if (dates0 == "none") { # If you are not using any data from temp data
    iterate <- 1 
  } else if (dates0 == "all") {
    iterate <- 1:length(date_entered) # if you want to run all of plots for each date_entered: 
  } else if (dates0 == "latest") {
    iterate <- length(date_entered) # if you want to just run todays/a specific date:
  } else { # if you want to run a specific date
    iterate <- which(as.character(date_entered) %in% as.character(dates0))
  }
  
  for (i in iterate) {
    
    start_time <- Sys.time()
    
    grid_stations_plot<-survey_area$survey.grid
    dat_plot <- dat
    
    if (as.character(dates0[1]) == "none") { # If you ARE NOT using any data from temp data
      
      max_date <- NA
      grid_stations_plot$var_bin<-NA  # only use dates including this date and before this date
      
    } else {

      max_date <- date_entered[i]

      # only use dates including this date and before this date
      dat_plot$var[as.Date(dat_plot$date)>as.Date(max_date)]<-NA 
      grid_stations_plot$var_bin[as.Date(grid_stations_plot$date)>as.Date(max_date)]<-NA 
      # only use dates including 1 days before this date and before this date, so we can see the planned progression
      # if (date_entered[i] != date_entered[length(date_entered)]) {
        dat_plot$vessel_shape[as.Date(dat_plot$date)>as.Date(max_date)+1]<-NA
        dat_plot$date[as.Date(dat_plot$date)>as.Date(max_date)+1]<-NA
        grid_stations_plot$vessel_shape[as.Date(grid_stations_plot$date) > as.Date(max_date)+1]<-NA
        grid_stations_plot$date[as.Date(grid_stations_plot$date) > as.Date(max_date)+1]<-NA
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
      
      all_vess <- unique(dat_plot$vessel_shape)
      all_vess <- all_vess[!is.na(all_vess)]
      for (ii in 1:length(all_vess)) {
        vess <- all_vess[ii]
        if (sum(unique(dat_planned$vessel_shape) %in% vess)==0) {
          temp1 <- rep_len(x = NA, length.out = ncol(dat_planned))
          names(temp1) <- names(dat_planned)
          dat_planned <- dplyr::bind_rows(dat_planned, 
                                          temp1 %>% 
                                            t() %>%
                                            data.frame() %>%
                                            dplyr::mutate(vessel_shape = vess, 
                                                          vessel_name = unique(dat_plot$vessel_name[dat_plot$vessel_shape %in% vess]), 
                                                          lon = 5000000, 
                                                          lat = 5000000,
                                                          planned = "N")) 
          vess_date <- paste0("\n ")
        } else {
          vess_date <- as.Date(range(as.Date(dat_planned$date[dat_planned$vessel_shape %in% vess])))
          vess_date <- ifelse(vess_date[1] == vess_date[2],
                              format(vess_date[1],
                                     "%b %d"),
                              paste(format(vess_date,
                                           "%b %d"), collapse = "-"))
          vess_date <- paste0("\n(", vess_date, ")")
        }

        dat_planned$lab[dat_planned$vessel_shape %in% vess] <-
          paste0(unique(dat_plot$vessel_name[dat_plot$vessel_shape %in% vess]), vess_date)
      }
    } else if (show_planned_stations) { # if we are sharing planned stations but there aren't any to share
      dat_planned <- data.frame(station = NA, 
                                stratum = NA, 
                                lon = 5000000, 
                                lat = 5000000, 
                                planned = "N", 
                                vessel_shape = unique(dat_plot$vessel_shape)[!is.na(unique(dat_plot$vessel_shape))], 
                                vessel_name = unique(dat_plot$vessel_name)[!is.na(unique(dat_plot$vessel_name))], 
                                date = NA, 
                                lab = paste0(unique(dat_plot$vessel_name)[!is.na(unique(dat_plot$vessel_name))], "\n "))
    }
  }
    
    gg <- ggplot() +
      geom_sf(data = survey_area$akland, fill = "white") + # AK Map (thanks {akgfmaps}!)
      geom_sf(data = survey_area$graticule, 
              color = "grey70", 
              alpha = 0.5) +
      coord_sf(xlim = survey_area$plot.boundary$x, 
               ylim = survey_area$plot.boundary$y) +
      scale_x_continuous(name = "Longitude", 
                         breaks = survey_area$lon.breaks) + 
      scale_y_continuous(name = "Latitude", 
                         breaks = survey_area$lat.breaks)  +
      theme( 
        panel.background = element_rect(fill = "grey90"),
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size=14), 
        legend.text=element_text(size=10), 
        legend.position="right",
        legend.direction="vertical",
        legend.justification="left",
        legend.background = element_blank(),
        legend.title=element_text(size=12),
        legend.box.background = element_blank(),
        legend.key = element_blank(), 
        legend.key.size=(unit(.3,"cm")), 
        axis.text = element_text(size=9), 
        axis.title=element_text(size=14) )

    if (as.character(dates0[1]) == "none") {
      temp <-survey_area$place.labels[survey_area$place.labels$type == "bathymetry",]
      gg <- gg +
        ggplot2::geom_sf(data = survey_area$survey.grid, 
                         # aes(group = station), 
                         colour = "grey50",
                         show.legend = legend_title) +
        geom_sf(data = survey_area$bathymetry) +
        guides(colour = guide_legend(override.aes = list(fill = survey_area$survey.area$survey_reg_col)))  # survey regions
      if (nrow(temp)>0) {
        gg <- gg +
       
        # geom_text(data = temp, mapping = aes(x = x, y = y, label = lab), 
        #           size = 4, hjust=0, vjust=1, fontface=2, angle = 90)
        annotate(geom = "text", x = temp$x, y = temp$y, label = temp$lab,
                 color = "darkblue", fontface="bold", angle = 0)
      }
        
    } else if (as.character(dates0[1]) != "none") { # If you are using any data from temp data # Add temperature squares
        
      # now we build a plot list
      lapply(unique(grid_stations_plot$region), function(x) {
        # x <- unique(grid_stations_plot$region)
        grid_stations_plot1 <- grid_stations_plot %>% 
          dplyr::arrange(region)
        grid_stations_plot1$region <- factor(grid_stations_plot1$region)        
        grid_stations_plot1<-grid_stations_plot1[grid_stations_plot1$region == x,]
          
        gg  + 
          ggplot2::geom_sf(data = grid_stations_plot1, 
                           aes(#group = region, 
                             fill = var_bin), 
                           colour = "black", 
                           size = .05,
                           show.legend = FALSE) +
          ggplot2::scale_fill_manual(name = legend_title,
                                     values = var_color, 
                                     labels = var_labels,
                                     drop = F,
                                     na.translate = F) +
          ggspatial::coord_sf(
            xlim = c(extent(grid_stations_plot1)[1:2]), 
            ylim = c(extent(grid_stations_plot1)[3:4])) +
        ggtitle(x)  +
          ggsn::scalebar(data = grid_stations_plot1,
                         location = ifelse(x == "Western Aleutians", "topright", "topleft"),
                         dist = 25,
                         dist_unit = "nm",
                         transform = FALSE,
                         st.dist = 0.06,
                         border.size = .25,
                         height = 0.03,
                         st.bottom = TRUE,
                         st.size = 3, 
                         model = survey_area$crs) +
        theme(
          plot.title = element_text(size = 10, face = "bold"), 
          # axis.text = element_text(size=9),
          axis.title = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"))

      }) -> region_list
      
        # grid_stations_plot1<-grid_stations_plot
        # grid_stations_plot1$region <- factor(grid_stations_plot1$region)        
        # grid_stations_plot1<-grid_stations_plot1[grid_stations_plot1$region == x,]
        
        # legend
        legend_temp <- gg  + 
          ggplot2::geom_sf(data = grid_stations_plot[grid_stations_plot$region == "Central Aleutians",], 
                           aes(#group = region, 
                             fill = var_bin), 
                           colour = "grey50",
                           show.legend = legend_title) +
          ggplot2::scale_fill_manual(name = legend_title,
                                     values = var_color, 
                                     labels = var_labels,
                                     drop = F,
                                     na.translate = F) +
          theme(
            axis.text = element_text(size=5))
        
        
        # now mimic facet_wrap layout with grid.arrange
        # gg1 <- do.call(gridExtra::grid.arrange, c(region_list, ncol=2))
        # gg1 <- gridExtra::grid.arrange(region_list, ncol=2, common.legend = TRUE, legend = "right")
        # cowplot::plot_grid(plotlist = region_list, ncol = 2, greedy = TRUE) 
        # gg1 <- cowplot::plot_grid(
        #   cowplot::plot_grid(plotlist = region_list, ncol = 2, greedy = TRUE),
        #   cowplot::get_legend(plot = temp + 
        #                         # scale_shape(guide = FALSE) + 
        #                         theme(legend.position = "right")))
        
        
      # inset
      gg_insert <- ggplot() +
        geom_sf(data = survey_area$akland, fill = "black", color = NA) + # AK Map (thanks {akgfmaps}!)
        geom_sf(data = survey_area$graticule, 
                color = "grey70", 
                alpha = 0.5) +
        coord_sf(xlim = survey_area$plot.boundary$x, 
                 ylim = survey_area$plot.boundary$y) +
        scale_x_continuous(name = "Longitude",
                           breaks = survey_area$lon.breaks) +
        scale_y_continuous(name = "Latitude",
                           breaks = survey_area$lat.breaks)
      bb <- data.frame()
      for (iiii in 1:length(unique(grid_stations_plot$region))) {
        x <- unique(grid_stations_plot$region)[iiii]
        grid_stations_plot1<-grid_stations_plot
        grid_stations_plot1$region <- factor(grid_stations_plot1$region)        
        grid_stations_plot1<-grid_stations_plot1[grid_stations_plot1$region == x,]
        
        a <- extent(grid_stations_plot1)
        b <- data.frame(t(matrix(a)))
        names(b) <- c("xmin", "xmax", "ymin", "ymax")
        b$lab <- gsub(pattern = "and\n", replacement = "and ", 
                      x = gsub(pattern = " ", replacement = "\n", x = x, fixed = TRUE), 
                      fixed = TRUE)
        b$x <- mean(a[1:2])
        b$y <- a[4]
        bb <- rbind.data.frame(bb, b)
          
        poly <- data.frame(a[3:4], a[1:2])
        names(poly) <- c("lat", "lon") 
        poly <- poly %>%
          st_as_sf(coords = c("lon", "lat"), 
                   crs = survey_area$crs) %>% 
          st_bbox() %>% 
          st_as_sfc()
        
        gg_insert <- gg_insert + 
          geom_sf(data = poly, fill = NA, color = "black", size = .5)#+ 
          # geom_text(mapping = aes(x = mean(a[1:2]), y = a[4]+10000, label = x))
          
      }
      
      gg_insert <- gg_insert + 
        geom_text(data = bb, 
                  mapping = aes(x = x, y = y+200000, label = lab), 
                  size = 3) +
        ggspatial::coord_sf(
          xlim = c(extent(grid_stations_plot)[1:2]),
          ylim = c(extent(grid_stations_plot)[3], extent(grid_stations_plot)[4]+400000)) + 
        ggtitle(gsub(pattern = "\n", replacement = "", x = unique(survey_area$survey.area$reg_lab), fixed = TRUE)) +
        theme_minimal() + 
        theme(
          panel.border = element_rect(colour = "grey50", fill=NA, size=1), 
          plot.title = element_text(size = 8, face = "bold"), 
          # axis.text.x = element_text(size=9),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm") ) # + 
        # annotate(geom = "text",
        #          x = quantile(extent(grid_stations_plot)[1]:extent(grid_stations_plot)[2], .5), 
        #          y = quantile(extent(grid_stations_plot)[3]:extent(grid_stations_plot)[4], 1), 
        #          fontface = "bold",
        #          size = 5, 
        #          label = unique(survey_area$survey.area$reg_lab))
      
      # region_list[[length(region_list)+1]] <- NULL
      # region_list[[length(region_list)+1]] <- gg_insert
      # gg1 <- do.call(gridExtra::grid.arrange, c(region_list, ncol=2))
      
      
      # gg1 <- cowplot::plot_grid(
      #   cowplot::plot_grid(plotlist = region_list, ncol = 3, greedy = TRUE, byrow = FALSE),
      #   cowplot::get_legend(plot = legend_temp + 
      #                         # scale_shape(guide = FALSE) + 
      #                         theme(legend.position = "right")))
      
      
      # put together the bottom row and then everything
      
      title_row <- cowplot::ggdraw() + 
        draw_label(
          plot_title,
          fontface = 'bold',
          x = 0,
          hjust = 0, 
          size = 20) +
        theme(# add margin on the left of the drawing canvas,
          plot.margin = margin(0, 0, 0, 7)) # so title is aligned with left edge of first plot
      
      subtitle_row <- cowplot::ggdraw() + 
        draw_label(
          paste0(plot_subtitle, "\n", 
                   ifelse(min(as.Date(dat$date), na.rm = TRUE) == max_date, 
                               paste0(format(x = min(as.Date(dat_plot$date), na.rm = TRUE), "%B %d, %Y")), 
                               paste0(format(x = min(as.Date(dat_plot$date), na.rm = TRUE), "%B %d"), 
                                      " \u2013 ", 
                                      format(x = as.Date(max_date), format = "%B %d, %Y")))),
          fontface = 'bold',
          x = 0,
          hjust = 0, 
          size = 14) +
        theme(# add margin on the left of the drawing canvas,
          plot.margin = margin(0, 0, 0, 7)) # so title is aligned with left edge of first plot
      
      side_bar <- cowplot::plot_grid(
        cowplot::get_legend(legend_temp), gg_insert, 
        rel_heights = c(1.25, .75),
        nrow = 2, align = "v", axis = "r"
      )
      
      gg <- cowplot::plot_grid(
        region_list[[1]], region_list[[2]], region_list[[3]], region_list[[4]],
                      ncol = 2, nrow = 2, greedy = TRUE, rel_widths = c(1.5, 1.5)) +
        draw_label("Longitude", x = 0.25, y = 0, vjust = -0.5, angle = 0) +
        draw_label("Latitude", x = 0, y = 0.5, vjust = 1.5, angle = 90)
      
      
      gg <- cowplot::plot_grid(
        gg, NULL, side_bar, 
        ncol = 3, greedy = TRUE, rel_widths = c(1, .05, .5))
      
      gg <- cowplot::plot_grid(
        title_row, 
        subtitle_row, 
        gg, 
        nrow = 3, greedy = TRUE, rel_heights = c(0.1, 0.2, 2))
      
      }
    
    gg <- ggdraw(gg) +
      draw_image(image = paste0(dir_in, "img/noaa-fish-wide.png"), # "img/noaa-50th-logo.png"
                 x = .37, y = .43, # x = 0, y = 0, hjust = -4.12, vjust = -.45, width = .19
                 scale = .15 )
    
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

    if (file_end %in% c("grid", "daily")){
    rmarkdown::render(paste0("./code/template.Rmd"),
                      output_dir = dir_out,
                      output_file = paste0(".", dir_out, filename0, ".pdf"))
    file.remove(list.files(path = "./code/", pattern = ".log", full.names = TRUE))

    } else if (file_end == "anom"){
    ggsave(paste0(filename0,'.pdf'),
           height = height,
           width = width,
           plot=gg,
           device="pdf") # pdfs are great for editing later
    }
    
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


