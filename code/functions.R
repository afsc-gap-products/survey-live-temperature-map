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
  
  #images
  "png",
  "grid",
  "cowplot",
  "magick", 
  "qpdf",
  
  # color managment (color blind friendly!)
  # "viridis", 
  "colorRamps",
  
  # google drive
  "googledrive", # library(googlesheets)
  
  # data management
  "bindrcpp", 
  "janitor", 
  
  # "tinytex",
  "RCurl",
  
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

# Functions --------------------------------------------------------------------

#' Takes a string of words and combines them into a sentence that lists them.
#'
#' This function allows you to take a string of words and combine them into a sentence list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
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
  x<-x[which(!is.na(x))]
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


#' Wrapper function to produce daily or anomaly plots
#' 
#' This function prepares data to be used in the make_figures() function
#'
#' @param maxyr Numeric. The 4 digit year (YYYY) of the year that the data is from. 
#' @param SRVY String. Accepted values include "BS" (EBS and NBS combined), "EBS", "NBS", "AI". "GOA" will be added as an option in 2023. 
#' @param haul data.frame. Here, derived from the RACEBASE.HAUL and RACE_DATA.V_CRUISES oracle tables. See example in data.R script. 
#' @param dat_survreg data.frame. Must include reg_shapefile (the name of the shapefile polygon), region_long (the formal name of the survey), SRVY ("BS" (EBS and NBS combined), "EBS", "NBS", "AI". "GOA"), region (to match the region of the survey in the haul data), vessel_id (from RACE_DATA.VESSELS), vessel_shape (as listed in the google spreadsheet/how you want the vessel to appear on the plot), reg_dates (the planned dates of the survey).
#' @param var String. Default = "bt". "bt" will select the bottom temperature column and "st" will select the surface temperature column in haul data. 
#' @param dir_googledrive_upload googledrive::as_id("..."). Default = NULL. The location where outputs will be saved to in google drive. If NULL outputs will NOT be saved to googledrive. 
#' @param dates0 Default = "latest". A string of either the date to be plotted to ("YYYY-MM-DD"), a string of dates to be plotted (as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))), "all" which will plot all of the days where data was collected for in that year, or "latest" where only the most recent date's data will be plotted, or "none" where only the grid (no data) will be printed. 
#' @param survey_area From akgfmaps::get_base_layers(select.region = ..., set.crs = "auto"). 
#' @param plot_subtitle String. Default = "". The desired subtitle for the figure. 
#' @param show_planned_stations Logical. Default = TRUE. TRUE will show planned stations on the plot and FALSE will not. 
#' @param data_source String. Default = "gd". "gd" will pull data from the google drive file and "oracle" will use the haul data (noted above) that comes from oracle. 
#' @param plot_anom Logical. Default = TRUE. TRUE will plot anomaly plots and FALSE will not. 
#' @param dir_wd String. Default = "./", or the root directory. Necessary because the task scheduler does not understand the concept of the R Project root directory (therefore, beware of the {here} R package.. This string should be the path to the R Project root directory. 
#'
#' @return
#' @export
#'
#' @examples
make_varplot_wrapper <- function(maxyr, 
                                 SRVY, 
                                 haul, 
                                 dat_survreg, 
                                 var = "bt", 
                                 dir_googledrive_upload = NULL, 
                                 dates0 = "latest", 
                                 survey_area, 
                                 plot_subtitle = "", 
                                 show_planned_stations = TRUE, 
                                 data_source = "gd", 
                                 plot_daily = TRUE,
                                 plot_anom = TRUE, 
                                 plot_mean = FALSE,
                                 dir_wd = "./") {
  
  case <- paste0(maxyr, "_", SRVY)
  dir_out <- paste0(dir_wd, "/output/", case, "/")
  
  SRVY1 <- SRVY
  if (SRVY == "BS") {
    SRVY1 <- c("EBS", "NBS")
  }
  
  # Define var
  if (var == "bt") {
    var00 = 'Bottom Temperature'
    unit0 <- '(\u00B0C)'
    var0 <- "gear_temperature"
    if (SRVY == "BS") {
      var_breaks <- c(-10, seq(from = -2, to = 8, by = 0.5), 50)
    } else {
      var_breaks <- c(-10, seq(from = 2, to = 8, by = 0.5), 50)
    }
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
      readxl::read_xlsx(path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"), 
                        sheet = case, skip = 1) %>%
      janitor::clean_names() %>% 
      # dplyr::mutate(var = all_of(var0)) %>%
      dplyr::rename(#"var" = all_of(var), 
        "SRVY" = "srvy", 
        "vessel_shape" = "vessel") %>%
      dplyr::filter(
        SRVY %in% SRVY1) 
    
    dat$var <- as.numeric(unlist(dat[,var]))
    
    dat <- dat %>%
      dplyr::select(SRVY, stratum, station, var, date, vessel_shape) 
    
  } else if (data_source == "oracle") {
    dat <- haul %>% 
      dplyr::filter(year == maxyr &
                      SRVY %in% SRVY1 &
                      !is.na(dplyr::all_of(var))) %>% 
      # dplyr::mutate(var = haul[,var0]) %>%
      dplyr::rename(station = stationid, 
                    date = start_time) 
    dat$var <- unlist(dat[,var0])
    dat$var0 <- unlist(dat[,var0])
    names(dat)[names(dat) == "var0"] <- var
    
    if (SRVY %in% c("AI", "GOA")){
      dat <- dat %>%
        dplyr::filter(var != 0) # there shouldn't be bottom temps of 0 in the AI or GOA
    }
    dat$date <- as.character(as.Date(sapply(strsplit(x = dat$date, split = " ", fixed = TRUE),`[[`, 1), "%m/%d/%Y"))
    for (ii in 1:length(unique(dat$SRVY))){
      temp <- unique(dat$SRVY)[ii]
      dat_survreg$reg_dates[dat_survreg$SRVY == temp] <- paste0(#"\n(", 
        format(x = min(as.Date(dat$date[dat$SRVY == temp]), na.rm = TRUE), "%b %d"),
        " - ", 
        format(x = max(as.Date(dat$date[dat$SRVY == temp]), na.rm = TRUE), "%b %d")#, ")"
      )
    }
    dat <- dat %>% 
      dplyr::left_join(
        x = ., 
        y = dat_survreg %>% 
          dplyr::select(vessel_id, vessel_shape) %>%
          dplyr::distinct(), 
        by = "vessel_id")  %>% 
      dplyr::select(SRVY, stratum, station, var, dplyr::all_of(var), date, vessel_shape)
    
  }
  
  if (show_planned_stations) {
    vess <- unique(dat_survreg$vessel_shape[dat_survreg$SRVY == SRVY1[1]])
    # if (sum(vess %in% dat$vessel_shape) != length(vess)) {
    temp <- data.frame(matrix(data = NA, 
                              ncol = ncol(dat), 
                              nrow = length(unique(dat_survreg$vessel_shape[dat_survreg$SRVY == SRVY1[1]])))) 
    names(temp) <- names(dat)
    dat <- dplyr::bind_rows(
      temp %>% dplyr::mutate(SRVY = SRVY1[1], 
                             stratum = 0,
                             station = "0", 
                             var = 0,
                             date = as.character(min(as.Date(dat$date), na.rm = TRUE)-1), 
                             vessel_shape = unique(dat_survreg$vessel_shape[dat_survreg$SRVY == SRVY1[1]])), 
      dat %>% 
        dplyr::mutate(SRVY = as.character(SRVY), 
                      stratum = as.numeric(stratum), 
                      station = as.character(station), 
                      var = as.numeric(var),
                      date = as.character(date),
                      vessel_shape = as.character(vessel_shape)) )
    # }
  }
  
  dat <- dat %>% 
    dplyr::left_join(
      x = ., 
      y = dat_survreg %>%
        dplyr::select(-dplyr::starts_with("vessel_"))%>%
        dplyr::distinct() ) %>%
    dplyr::left_join(
      x = ., 
      y = dat_survreg %>%
        dplyr::select(dplyr::starts_with("vessel_"))%>%
        dplyr::distinct() ) %>%
    dplyr::left_join(
      x = ., 
      y = dat_survreg) %>%
    dplyr::left_join(
      x = ., 
      y = vessel_info, 
      by = c("vessel_id")) %>% # add survey vessel data
    dplyr::left_join(
      x = ., 
      y = dat_anom, 
      by = c("SRVY", "stratum", "station")) %>% 
    dplyr::mutate(reg_lab = paste0(region_long, "\n(", reg_dates, ")"), 
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
  
  height = ifelse(SRVY %in% "AI", 6, 8)
  
  ### Daily --------------------------------------------------------------------
  if (plot_daily) {  # Daily plot
    file_end = "daily"
    make_figure(SRVY = SRVY, 
                dat = dat,
                var_breaks = var_breaks, 
                plot_title = plot_title,
                plot_subtitle = plot_subtitle,
                legend_title = legend_title,
                dates0 = dates0, 
                survey_area = survey_area,
                file_end = file_end,
                dir_wd = dir_wd,
                dir_out = dir_out, 
                dir_googledrive_upload = dir_googledrive_upload, 
                make_gifs = TRUE, 
                data_source = data_source,
                show_planned_stations = show_planned_stations, 
                height = height)
  }
  
  ### Mean ---------------------------------------------------------------------
  dat <- dat %>%
    dplyr::mutate(reg_lab = paste0(region_long, "\n "))
  
  # googledrive::drive_mkdir(name = "anom", 
  #                          path = dir_googledrive_upload,
  #                          overwrite = FALSE)
  
    dir_googledrive_upload0 <- googledrive::drive_ls(path = googledrive:as_id(dir_googledrive_upload)) %>% 
    dplyr::filter(name == "anom") %>% 
    dplyr::select("id") %>% 
    unlist() %>% 
    googledrive::as_id()
  
  if (plot_mean) {
    
    dat <- dat %>% # Mean plot
      dplyr::mutate(var = mean)
    file_end <- "mean"
    plot_title <- gsub(pattern = "Anomaly", replacement = "Mean", x = plot_title_anom)
    legend_title = gsub(pattern = "Anomaly", replacement = "Mean", x = legend_title_anom)
    make_figure(SRVY = SRVY, 
                dat = dat,
                var_breaks = var_breaks, 
                plot_title = plot_title,
                plot_subtitle = plot_subtitle,
                legend_title = legend_title,
                dates0 = dates0, 
                survey_area = survey_area,
                file_end = file_end,
                dir_wd = dir_wd,
                dir_out = dir_out, 
                dir_googledrive_upload = dir_googledrive_upload0, 
                make_gifs = FALSE, 
                data_source = data_source,
                show_planned_stations = FALSE, 
                height = height)
  }
  
  ### Anomaly ------------------------------------------------------------------
  if (plot_anom) {
    dat <- dat %>% # Anomaly plot
      dplyr::mutate(var = anom)
    var_breaks <- c(-10, seq(from = -2, to = 3, by = 0.5), 50) # for the anomaly data
    file_end <- "anom"
    plot_title <- plot_title_anom
    legend_title = legend_title_anom
    make_figure(SRVY = SRVY, 
                dat = dat,
                var_breaks = var_breaks, 
                plot_title = plot_title,
                plot_subtitle = plot_subtitle,
                legend_title = legend_title,
                dates0 = dates0, 
                survey_area = survey_area,
                file_end = file_end,
                dir_wd = dir_wd,
                dir_out = dir_out, 
                dir_googledrive_upload = dir_googledrive_upload0, 
                make_gifs = FALSE, 
                data_source = data_source,
                show_planned_stations = FALSE, 
                height = height)
  }
}



#' Wrapper function to produce empty grid plots
#' 
#' This function prepares data to be used in the make_figures() function
#'
#' @param maxyr Numeric. The 4 digit year (YYYY) of the year that the data is from. 
#' @param SRVY String. Accepted values include "BS" (EBS and NBS combined), "EBS", "NBS", "AI". "GOA" will be added as an option in 2023. 
#' @param haul data.frame. Here, derived from the RACEBASE.HAUL and RACE_DATA.V_CRUISES oracle tables. See example in data.R script. 
#' @param dat_survreg data.frame. Must include reg_shapefile (the name of the shapefile polygon), region_long (the formal name of the survey), SRVY ("BS" (EBS and NBS combined), "EBS", "NBS", "AI". "GOA"), region (to match the region of the survey in the haul data), vessel_id (from RACE_DATA.VESSELS), vessel_shape (as listed in the google spreadsheet/how you want the vessel to appear on the plot), reg_dates (the planned dates of the survey).
#' @param dir_googledrive_upload googledrive::as_id("..."). Default = NULL. The location where outputs will be saved to in google drive. If NULL outputs will NOT be saved to googledrive. 
#' @param survey_area From akgfmaps::get_base_layers(select.region = ..., set.crs = "auto"). 
#' @param plot_subtitle String. Default = "". The desired subtitle for the figure. 
#' @param data_source String. Default = "gd". "gd" will pull data from the google drive file and "oracle" will use the haul data (noted above) that comes from oracle. 
#' @param dir_wd String. Default = "./", or the root directory. Necessary because the task scheduler does not understand the concept of the R Project root directory (therefore, beware of the {here} R package.. This string should be the path to the R Project root directory. 
#'
#' @return
#' @export
#'
#' @examples
make_grid_wrapper<-function(maxyr, 
                            SRVY, 
                            haul, 
                            dat_survreg, 
                            dir_googledrive_upload = NULL,  
                            survey_area, 
                            plot_subtitle = "", 
                            data_source = "gd", 
                            dir_wd = "./") {
  
  case <- paste0(maxyr, "_", SRVY)
  dir_out <- paste0(dir_wd, "/output/", case, "/")
  
  file_end = "grid"
  dates0 <- "none"
  show_planned_stations = FALSE
  make_gifs = FALSE

  SRVY1 <- SRVY
  if (SRVY == "BS") {
    SRVY1 <- c("EBS", "NBS")
  }
  
  plot_title <- ifelse(SRVY == "AI", "Survey Region", "Survey Grid")
  legend_title <- ""
  
  if (data_source == "gd") { # Temperature data from google drive
    dat <- 
      readxl::read_xlsx(path = paste0(dir_wd, "/data/gap_survey_progression.xlsx"), 
                        sheet = case, skip = 1) %>%
      janitor::clean_names() %>%
      dplyr::rename(
        "SRVY" = "srvy", 
        "vessel_shape" = "vessel")  %>%
      dplyr::filter(!is.na(SRVY) & 
                      SRVY %in% SRVY1) %>% 
      dplyr::select(SRVY, stratum, station, date, vessel_shape) 
  } else if (data_source == "oracle") {
    dat <- haul %>% 
      dplyr::filter(year == maxyr &
                      SRVY %in% SRVY1) %>% 
      dplyr::rename(station = stationid, 
                    date = start_time) %>%
      dplyr::mutate(var = NA)
    dat$date <- as.character(as.Date(sapply(strsplit(x = dat$date, split = " ", fixed = TRUE),`[[`, 1), "%m/%d/%Y"))
    for (ii in 1:length(unique(dat$SRVY))){
      temp <- unique(dat_survreg$SRVY)[ii]
      dat_survreg$reg_dates[dat_survreg$SRVY == temp] <- paste0("\n(", 
                                                                format(x = min(as.Date(dat$date[dat$SRVY == temp]), na.rm = TRUE), "%b %d"),
                                                                " - ", 
                                                                format(x = max(as.Date(dat$date[dat$SRVY == temp]), na.rm = TRUE), "%b %d"), ")")
    }
    dat <- dat  %>% 
      dplyr::mutate(var = NA, 
                    vessel_id = NA, 
                    vessel_name = NA,
                    vessel_shape = NA) %>%
      dplyr::select(SRVY, stratum, station, var, date, vessel_shape)
  }
  
  dat <- dat %>%
    dplyr::filter(!is.na(SRVY))  %>% 
    dplyr::left_join(x = ., 
                     y = dat_survreg %>% 
                       dplyr::select(-dplyr::starts_with("vessel_")) %>% 
                       dplyr::distinct(), 
                     by = c("SRVY")) %>%
    dplyr::mutate(reg_lab = paste0(region_long), 
                  var_bin = NA) %>%
    dplyr::arrange(date)
  
  make_gifs = FALSE
  height = ifelse(SRVY == "AI", 6, 8.5)
  
  # Daily plot
  make_figure(SRVY = SRVY, 
              dat = dat,
              var_breaks = var_breaks, 
              plot_title = plot_title,
              plot_subtitle = plot_subtitle,
              legend_title = legend_title,
              dates0 = dates0, #"latest", # "all", #"2021-06-05",
              survey_area = survey_area,
              file_end = file_end,
              dir_wd = dir_wd,
              dir_out = dir_out, 
              dir_googledrive_upload = dir_googledrive_upload, 
              make_gifs = make_gifs, 
              data_source = data_source,
              show_planned_stations = show_planned_stations, 
              height = height)
}



#' Create temperature and anomaly plots
#'
#' @param SRVY String. Accepted values include "BS" (EBS and NBS combined), "EBS", "NBS", "AI". "GOA" will be added as an option in 2023. 
#' @param dat The csv file from googledrive or oracle with data for the 'maxyr' being plotted or compared to in the anomally. 
#' @param var_breaks String. Desired breaks for var to be binned into. 
#' @param plot_title String. Default = "". The desired title for the figure. 
#' @param plot_subtitle String. Default = "". The desired subtitle for the figure. 
#' @param legend_title String. Default = "". The desired legend title for the variable in mapped in the figure. 
#' @param dates0 A string of either the date to be plotted to ("YYYY-MM-DD"), "all" which will plot all of the days where data was collected for in that year, or "latest" where only the most recent date's data will be plotted, or "none" where only the grid (no data) will bbe printed. Default = "latest". 
#' @param survey_area From akgfmaps::get_base_layers(select.region = ..., set.crs = "auto"). 
#' @param height Numeric. Default = 6. Numeric height of the figure in inches. 
#' @param width Numeric. Default = 10.5. Numeric width of the figure in inches. 
#' @param file_end String. Will appear after "_" in the file name. Options include "grid" (will produce an empty grid), "daily" (will produce the daily updated plot), "anom" (will produce anomally differences in the updated plot).
#' @param make_gifs Logical. Default = TRUE means that a gifs will be compiled from the latest gif.  
#' @param dir_out String. A path for the files to be saved to on the local machine when the funciton is complete. 
#' @param dir_wd String. Default = "./", or the root directory. Necessary because the task scheduler does not understand the concept of the R Project root directory (therefore, beware of the {here} R package.. This string should be the path to the R Project root directory. 
#' @param show_planned_stations Logical. Default = TRUE. TRUE will show planned stations on the plot and FALSE will not. 
#' @param data_source String. Default = "gd". "gd" will pull data from the google drive file and "oracle" will use the haul data (noted above) that comes from oracle. 
#' @param dir_googledrive_upload googledrive::as_id("..."). Default = NULL. The location where outputs will be saved to in google drive. If NULL outputs will NOT be saved to googledrive. 
make_figure <- function(
    SRVY, 
    dat = NULL, 
    var_breaks, 
    plot_title = "",
    plot_subtitle = "",
    legend_title = "",
    dates0 = "latest",
    survey_area, 
    height = 8, 
    width = 10.5,
    file_end = "",
    make_gifs = TRUE,
    dir_out = "./", 
    dir_wd = "./",
    show_planned_stations = TRUE,
    data_source = "gd",
    dir_googledrive_upload = NULL) {
  
  # Create Directories
  dir.create(path = dir_out, showWarnings = FALSE)
  
  ## Set Base Layers ------------------------------------------------------------
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
  
  ## Define colors and bins for var ----------------------------------------------
  if (as.character(dates0[1]) != "none") {
    
    var_labels <- c()
    for(i in 2:c(length(var_breaks))) {
      var_labels <- c(var_labels, 
                      dplyr::case_when(
                        i==2 ~ paste0("\u2264 ",# "≤ ", #
                                      var_breaks[i]), # ,"\u00B0C" <=
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
    date_entered <- sort(unique(dat$date))
    date_entered <- date_entered[!is.na(date_entered)]
  }
  
  # bind dat to survey_area$survey.grid for plotting
  survey_area$survey.grid <- sp::merge(x = survey_area$survey.grid, 
                                       y = dat %>%
                                         dplyr::select(station, stratum, var_bin, reg_shapefile, 
                                                       region_long, reg_dates, reg_lab, vessel_shape, date) %>% 
                                         dplyr::distinct(), 
                                       all.x = TRUE) 
  
  ## Define date range of figures to create ------------------------------------
  if (length(dates0)>1) { # if there is a specific range of dates
    iterate <- which(as.character(date_entered) %in% dates0)
  } else if (dates0 == "none") { # If you are not using any data from temp data
    iterate <- 1 
  } else if (dates0 == "all") {
    iterate <- 1:length(date_entered)# if you want to run all of plots for each date_entered: 
    if (sum(is.na(dat$var))!=0 & # if the survey is not yet complete
        show_planned_stations & # if we plan to show planned stations
        sum(is.na(dat$var) & !is.na(dat$vessel_shape))>0) { # and if there are planned stations to show
      iterate <- iterate[-length(iterate)]
    }
  } else if (dates0 == "latest") {
    iterate <- length(date_entered) # if you want to just run todays/a specific date:
    if (sum(is.na(dat$var))!=0 & # if the survey is not yet complete
        show_planned_stations & # if we plan to show planned stations
        sum(is.na(dat$var) & !is.na(dat$vessel_shape))>0) { # and if there are planned stations to show
      iterate <- iterate-1
    }
  } else { # if you want to run a specific date
    iterate <- which(as.character(date_entered) %in% as.character(dates0))
  }
  
  ## Make plots for each day of dates0 -----------------------------------------
  for (i in iterate) {
    
    start_time <- Sys.time()
    
    grid_stations_plot<-survey_area$survey.grid
    dat_plot <- dat    
    dat_planned <- NULL
    
    if (as.character(dates0[1]) == "none") { # If you ARE NOT using any data from temp data
      
      max_date <- NA
      grid_stations_plot$var_bin<-NA  # only use dates including this date and before this date
      
    } else {
      
      max_date <- date_entered[i]
      print(max_date)
      next_date <- ifelse(date_entered[i]==date_entered[length(date_entered)], 
                          as.character(as.Date(date_entered[i])+1), 
                          date_entered[i+1])
      
      # only use dates including this date and before this date
      dat_plot$var[as.Date(dat_plot$date)>as.Date(max_date)]<-NA 
      grid_stations_plot$var_bin[as.Date(grid_stations_plot$date)>as.Date(max_date)]<-NA 
      # only use dates including the next day before this date and before this date, so we can see the planned progression
      # if (date_entered[i] != date_entered[length(date_entered)]) {
      dat_plot$vessel_shape[as.Date(dat_plot$date)>as.Date(next_date)]<-NA
      dat_plot$date[as.Date(dat_plot$date)>as.Date(next_date)]<-NA
      grid_stations_plot$vessel_shape[as.Date(grid_stations_plot$date) > as.Date(next_date)]<-NA
      grid_stations_plot$date[as.Date(grid_stations_plot$date) > as.Date(next_date)]<-NA
      
      # separate out the data for the temperature and planned stations if there are planned stations listed
      # if (show_planned_stations & 
      #     sum(is.na(dat_plot$var) & !is.na(dat_plot$vessel_shape))>0) {
        if (#sum(is.na(dat$var))!=0 & # if the survey is not yet complete
            show_planned_stations & # if we plan to show planned stations
            sum(is.na(dat_plot$var) & !is.na(dat_plot$vessel_shape))>0) { # and if there are any planned stations to show

        # planned stations
        loc <- dat_plot %>% 
          dplyr::filter(is.na(var) & 
                          !is.na(vessel_shape) & 
                          as.Date(date) == as.Date(next_date)) %>% 
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
      geom_sf(data = survey_area$akland, fill = "white") + 
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
    
    if (SRVY %in% c("BS", "EBS", "NBS")) {
      ### Bering Sea -----------------------------------------------------------
      
      gg <- gg +
        ggtitle(label = plot_title, 
                subtitle = plot_subtitle) +
        theme(
          legend.text=element_text(size=13), 
          legend.title=element_text(size=16),
          axis.text = element_text(size=14))
      
      if (as.character(dates0[1]) == "none") {
        temp <-survey_area$place.labels[survey_area$place.labels$type == "bathymetry",]
        gg <- gg +
          ggplot2::geom_sf(data = survey_area$survey.grid, 
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
      if (as.character(dates0[1]) != "none") { # If you are using any data from temp data
        
        # if we are showing planned stations
        if (show_planned_stations) {
          
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
              fill = guide_legend(ncol = 2, #ifelse(length(var_breaks)>15, 2, 1), # temperatures # in case you want to have 2+ columns for the legend!
                                  override.aes = list(colour = c("white"),
                                                      size = 0),
                                  order = 1),
              colour = guide_legend(order = 2, # survey regions
                                    override.aes = list(#color = "white", 
                                      fill = survey_area$survey.area$survey_reg_col)) ,
              shape = guide_legend(order = 3, # planned stations
                                   override.aes = list(fill = "grey95", 
                                                       linetype = c("blank")))  )
        } else { # we are not showing planned stations
          gg <- gg +
            guides(
              # tempartures # in case you want to have 2+ columns for the legend!
              fill = guide_legend(ncol=2, #1, 
                                  override.aes = list(colour = c("white"),
                                                      size = 0),
                                  order = 1),
              # survey regions
              colour = guide_legend(order = 2, 
                                    override.aes = list(fill = survey_area$survey.area$survey_reg_col))) 
        }
      }
      
      gg <- gg +
        ggspatial::coord_sf(
          xlim = c(extent(survey_area$survey.grid)[1:2]),
          ylim = c(extent(survey_area$survey.grid)[3:4])) 
      
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
                                       paste0(format(x = min(as.Date(dat_plot$date), na.rm = TRUE), "%b %d, %Y")), 
                                       paste0(format(x = min(as.Date(dat_plot$date), na.rm = TRUE), "%b %d"), 
                                              " \u2013\n", 
                                              format(x = as.Date(max_date), format = "%b %d, %Y")))), 
                 color = "black", size = 5, fontface=2) 
      
      gg <- ggdraw(gg) +
        draw_image(image = paste0(dir_wd, "img/noaa-fish-wide.png"), # "img/noaa-50th-logo.png"
                   x = .37, y = .43, # x = 0, y = 0, hjust = -4.12, vjust = -.45, width = .19
                   scale = .15 )
      
    } else if (SRVY == "AI") {
      ### Aleutian Islands -----------------------------------------------------------
      
      # if (as.character(dates0[1]) == "none") {
      # temp <-survey_area$place.labels[survey_area$place.labels$type == "bathymetry",]
      gg <- gg +
        ggplot2::geom_sf(data = survey_area$survey.grid1, 
                         # mapping = aes(colour = survey_area$survey.grid$region,
                         #               fill = survey_area$survey.grid$region),
                         colour = ifelse((as.character(dates0[1]) == "none"), "grey20", "grey50"),
                         size = ifelse((as.character(dates0[1]) == "none"), .05, .02),
                         show.legend = FALSE) #+
      # geom_sf(data = survey_area$bathymetry) +
      # guides(colour = guide_legend(override.aes = list(fill = survey_area$survey.area$survey_reg_col)))  # survey regions
      # if (nrow(temp)>0) {
      #   gg <- gg +
      # 
      #   # geom_text(data = temp, mapping = aes(x = x, y = y, label = lab),
      #   #           size = 4, hjust=0, vjust=1, fontface=2, angle = 90)
      #   annotate(geom = "text", x = temp$x, y = temp$y, label = temp$lab,
      #            color = "darkblue", fontface="bold", angle = 0)
      # }
      # } 
      
      # now we build a plot list
      lapply(unique(grid_stations_plot$region), function(x) {
        # x <- unique(grid_stations_plot$region)
        grid_stations_plot1 <- grid_stations_plot
        grid_stations_plot1$region <- factor(grid_stations_plot1$region)        
        grid_stations_plot1<-grid_stations_plot1[grid_stations_plot1$region == x,]
        
        gg1 <- gg
        
        if (as.character(dates0[1]) != "none") { # If you are using any data from temp data # Add temperature squares
          
          gg1 <- gg1  + 
            ggplot2::geom_sf(data = grid_stations_plot1, 
                             aes(fill = var_bin), 
                             colour = "black", # "grey20" 
                             size = .05, 
                             show.legend = FALSE) +
            ggplot2::scale_fill_manual(name = legend_title,
                                       values = var_color, 
                                       labels = var_labels,
                                       drop = F,
                                       na.translate = F)
        } 
        gg1 <- gg2 <- gg1 +
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
            legend.position = "none", 
            plot.title = element_text(size = 10, face = "bold"), 
            # axis.text = element_text(size=9),
            axis.title = element_blank(),
            plot.margin=unit(c(0,0,0,0), "cm"))
        
        gg1
        
      }) -> region_list
      
      # var legend
      if (as.character(dates0[1]) != "none") { # If you are using any data from temp data # Add temperature squares
        legend_temp <- gg  + 
          ggplot2::geom_sf(data = grid_stations_plot,#[grid_stations_plot$region == "Southern Bering Sea",], 
                           aes(fill = var_bin), 
                           colour = "black", # "grey20" 
                           size = .05,
                           show.legend = legend_title) +
          ggplot2::scale_fill_manual(name = legend_title,
                                     values = var_color, 
                                     labels = var_labels,
                                     drop = F,
                                     na.translate = F) +
          ggspatial::coord_sf(
            xlim = c(extent(grid_stations_plot)[1:2]), 
            ylim = c(extent(grid_stations_plot)[3:4])) +
          theme(
            plot.title = element_text(size = 10, face = "bold"), 
            axis.title = element_blank(),
            plot.margin=unit(c(0,0,0,0), "cm"), 
            axis.text = element_text(size=5))
      }
      # }
      # inset map
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
                  mapping = aes(x = x, y = y+ifelse(file_end == "grid", 100000, 200000), label = lab), 
                  size = ifelse(file_end != "grid", 3, 5)) +
        ggspatial::coord_sf(
          xlim = c(extent(grid_stations_plot)[1:2]),
          ylim = c(extent(grid_stations_plot)[3], extent(grid_stations_plot)[4]+350000)) + 
        ggtitle(gsub(pattern = "\n", replacement = " ", x = unique(survey_area$survey.area$reg_lab), fixed = TRUE)) +
        theme_minimal() # + 
      # theme(
      #   panel.border = element_rect(colour = "grey50", fill=NA, size=1), 
      #   plot.title = element_text(size = 8, face = "bold"), 
      #   axis.text.y = element_blank(),
      #   axis.title = element_blank(),
      #   plot.margin=unit(c(0,0,0,0), "cm") ) 
      
      if (file_end == "grid") { 
        gg_insert <- gg_insert  + 
          theme(
            panel.border = element_rect(colour = "grey50", fill=NA, size=1), 
            plot.title = element_text(size = 20, face = "bold"), 
            axis.text = element_text(size = 9), 
            axis.title = element_text(size = 14)#, 
            # plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm") 
          )
      } else {
        gg_insert <- gg_insert  + 
          theme(
            panel.border = element_rect(colour = "grey50", fill=NA, size=1), 
            plot.title = element_text(size = 8, face = "bold"), 
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            plot.margin=unit(c(0,0,0,0), "cm") ) 
      }
      
      # put all plot peices together
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
          paste0(plot_subtitle, 
                 ifelse(dates0 == "none", "", paste0("\n", 
                                                     ifelse(min(as.Date(dat$date), na.rm = TRUE) == max_date, 
                                                            paste0(format(x = min(as.Date(dat_plot$date), na.rm = TRUE), "%b %d, %Y")), 
                                                            paste0(format(x = min(as.Date(dat_plot$date), na.rm = TRUE), "%b %d"), 
                                                                   " \u2013 ", 
                                                                   format(x = as.Date(max_date), format = "%b %d, %Y")))))),
          fontface = 'bold',
          x = 0,
          hjust = 0, 
          size = 14) +
        theme(# add margin on the left of the drawing canvas,
          plot.margin = margin(0, 0, 0, 7)) # so title is aligned with left edge of first plot
      
      noaa_logo <- ggdraw() + cowplot::draw_image(image = paste0(dir_wd, "img/noaa-fish-wide.png"))
      
      header_row <- cowplot::plot_grid(
        title_row, 
        subtitle_row, 
        nrow = 2, greedy = TRUE, rel_heights = c(0.2, 0.3))
      
      header_row <- cowplot::plot_grid(
        header_row, 
        # gg_insert, 
        noaa_logo, 
        ncol = 2, greedy = TRUE, rel_widths = c(2.6, 0.4))
      
      if (as.character(dates0[1]) != "none") { # If you are using any data from temp data # Add temperature squares
        gg1 <- cowplot::plot_grid(
          region_list[[1]], region_list[[2]], cowplot::get_legend(legend_temp), 
          ncol = 3, nrow = 1, greedy = TRUE, rel_widths = c(1.1, 1.5, 0.4))    
        # } else {
        #   gg1 <- cowplot::plot_grid(
        #     region_list[[1]], region_list[[2]], NULL, 
        #     ncol = 3, nrow = 1, greedy = TRUE, rel_widths = c(1.1, 1.5, 0.4))    
        # }
        
        gg2 <- cowplot::plot_grid(
          # region_list[[1]], region_list[[2]], 
          region_list[[3]], region_list[[4]], gg_insert, 
          ncol = 3, nrow = 1, greedy = TRUE, rel_widths = c(1.25, 0.75, 0.6)) +
          draw_label("Longitude", x = 0.4, y = 0, vjust = -0.5, angle = 0) +
          draw_label("Latitude", x = 0, y = 0.75, vjust = 1.5, angle = 90)   
        
        gg <- cowplot::plot_grid(header_row, gg1, gg2, 
                                 ncol = 1, nrow = 3, greedy = TRUE, rel_heights = c(0.4, 1, 1))
      } else {
        gg <- cowplot::plot_grid(header_row, gg_insert, 
                                 ncol = 1, nrow = 2, greedy = TRUE, rel_heights = c(0.4, 2))
      }
      
    }
    
    filename0 <- paste0(ifelse((as.character(dates0[1]) == "none" | file_end == "mean"), 
                               "", 
                               max_date), 
                        ifelse(file_end=="", "", paste0("_", file_end)))
    
    ### PNG -------------------------------------------------------------------------
    ggsave(filename = paste0(filename0,'.png'), 
           path = dir_out,
           height = height, 
           width = width,
           plot = gg, 
           dpi = 320,
           bg = "white", 
           device = "png") 
    
    ### PDF -------------------------------------------------------------------------
    # Create main PDF
    if (file_end %in% c("grid", "daily", "mean")){
      rmarkdown::render(paste0(dir_wd, "/code/template.Rmd"),
                        output_dir = dir_out,
                        output_file = paste0(filename0, ".pdf"))
      file.remove(list.files(path = paste0(dir_wd, "/code/"), 
                             pattern = ".log", full.names = TRUE))
      
    } else if (file_end %in% c("anom")){
      ggsave(filename = paste0(filename0,'.pdf'),
             path = dir_out,
             height = height,
             width = width,
             plot = gg,
             dpi = 320, 
             bg = "white", 
             device ="pdf") # pdfs are great for editing later
    }
    
    # Create Binded PDF
    if (file_end %in% c("daily")) { # "anom", 
      
      # remove file if already exists - qpdf::pdf_combine() will not overwrite
      if (length(list.files(path = dir_out, pattern = paste0(filename0, "_bind.pdf"))) != 0) {
        file.remove(paste0(dir_out, filename0, "_bind.pdf"))
      }
      
      if (as.character(date_entered[1]) == as.character(date_entered[i])) { # 
        qpdf::pdf_combine(input = c(paste0(dir_out, filename0,'.pdf'), 
                                    ifelse(file.exists(paste0(dir_out,'_grid.pdf')), paste0(dir_out,'_grid.pdf'), "")), 
                          output = c(paste0(dir_out, filename0, "_bind.pdf")))      
      } else {
        temp <- list.files(path = dir_out, pattern = paste0("_", file_end, "_bind.pdf"))
        temp <- temp[!grepl(pattern = "current", x = temp)]
        temp <- strsplit(x = temp, split = "_")
        temp <- as.Date(sort(sapply(temp,"[[",1)))
        temp <- max(temp[as.Date(temp) < as.Date(max_date)])
        
        filename00 <- paste0(ifelse(as.character(dates0[1]) == "none", "", 
                                    as.character(temp)), 
                             ifelse(file_end=="", "", paste0("_", file_end)), "_bind")
        
        qpdf::pdf_combine(input = c(paste0(dir_out, filename0, ".pdf"), 
                                    paste0(dir_out, filename00,'.pdf')), 
                          output = c(paste0(dir_out, filename0, "_bind.pdf")))      
      }
      
    }
    
    ### GIF -------------------------------------------------------------------------
    if (make_gifs) {
      # if (as.character(dates0[1]) != "none") {
      make_figure_gif(file_end = file_end, 
                      max_date = max_date,
                      dir_out = dir_out, 
                      filename0 = filename0)
      # }
    }
    
    ### rename "current" plots for easy finding ------------------------------------
    if (i == iterate[length(iterate)] & file_end %in% c("anom", "daily")) {
      temp <- list.files(path = dir_out, pattern = filename0, full.names = TRUE)
      for (iiii in 1:length(temp)){
        file.copy(from = temp[iiii], to = gsub(pattern = max_date, replacement = "current", x = temp[iiii]))
      }
    }
    
    ### Upload to google drive ------------------------------------------------------
    
    # if (file_end != "anom") {
    #   temp <- googledrive::drive_ls(path = dir_googledrive_upload, recursive = FALSE) 
    #   
    #   temp0 <- temp %>% 
    #     dplyr::filter(grepl(pattern = "archive", x = name, fixed = TRUE))
    #   
    #   temp <- temp %>% 
    #     dplyr::filter(grepl(pattern = ".", x = name, fixed = TRUE))
    #   
    #   googledrive::drive_mv(file = temp, path = )
    # }
    
    if (!(is.null(dir_googledrive_upload))) {
      temp <- list.files(path = dir_out, pattern = filename0, full.names = TRUE)
      if (i == iterate[length(iterate)] & file_end %in% c("anom", "daily")) {
        temp <- c(temp, 
                  list.files(path = dir_out, pattern = sub(pattern = max_date, replacement = "current", x = filename0), full.names = TRUE))
      }
      for (iii in 1:length(temp)) {
        drive_upload(
          media = temp[iii], 
          path = googledrive::as_id(dir_googledrive_upload),
          #path = ifelse(class(dir_googledrive_upload)[1] == "drive_id", dir_googledrive_upload, googledrive::as_id(dir_googledrive_upload)), 
          overwrite = TRUE)
      }
    }
    
    end_time <- Sys.time()
    print((end_time - start_time))
  }
}



#' gif compiler
#'
#' Compile gif from previous gifs in folder
#'
#' @param file_end String. Will appear after "_" in the file name. Options include "grid" (will produce an empty grid), "daily" (will produce the daily updated plot), "anom" (will produce anomally differences in the updated plot).
#' @param max_date String. The maximum date the function will inclusively create the gif for. 
#' @param dir_out String. A path for the files to be saved to on the local machine when the funciton is complete. 
#' @param filename0 String. The desired name of the gif to be created. Must include ".gif" at the end of the name. 
make_figure_gif<-function(file_end, 
                          max_date, 
                          dir_out, 
                          filename0) {
  
  imgs <- list.files(path = dir_out, 
                     pattern = paste0(as.Date(max_date), "_", file_end, ".png"), 
                     full.names = TRUE)
  
  temp <- strsplit(x = list.files(path = dir_out, pattern = paste0("_", file_end, ".gif")), split = "_")
  temp <- temp[!grepl(pattern = "current", x = temp)]
  
  if (length(temp) != 0) {
    temp <- as.Date(sort(sapply(temp,"[[",1)))
    temp <- max(temp[as.Date(temp) < as.Date(max_date)])
    imgs <- c(list.files(path = dir_out, 
                         pattern = paste0(as.character(temp), "_", file_end, ".gif"), 
                         full.names = TRUE), 
              imgs)
  }
  
  img_list <- lapply(imgs, 
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
    path = paste0(dir_out, filename0, ".gif") )
  
}




# save_figure <- function(gg,
#                         dat, 
#                         dat_plot, 
#                         dat_planned = NULL, 
#                         survey_area, 
#                         grid_stations_plot, 
#                         data_source, 
#                         dir_out, 
#                         dates0, 
#                         max_date, 
#                         file_end, 
#                         height, 
#                         width, 
#                         dir_googledrive_upload = NULL, 
#                         make_gifs = TRUE, 
#                         date_entered) {
#   
#   filename0 <- paste0(ifelse(as.character(dates0[1]) == "none", "", 
#                              max_date), 
#                       ifelse(file_end=="", "", paste0("_", file_end)))
#   
#   ggsave(filename = paste0(filename0,'.png'),
#          path = dir_out,
#          height = height, 
#          width = width,
#          plot=gg, 
#          dpi = 320, 
#          device="png") # pdfs are great for editing later
#   
#   if (file_end %in% c("grid", "daily")){
#     rmarkdown::render(paste0(dir_wd, "/code/template.Rmd"),
#                       output_dir = dir_out,
#                       output_file = paste0(filename0, ".pdf"))
#     file.remove(list.files(path = paste0(dir_wd, "/code/"), 
#                            pattern = ".log", full.names = TRUE))
#     
#   } else if (file_end == "anom"){
#     ggsave(filename = paste0(filename0,'.pdf'),
#            path = dir_out,
#            height = height,
#            width = width,
#            plot = gg,
#            dpi = 320, 
#            device ="pdf") # pdfs are great for editing later
#   }
#   
#   if (make_gifs | as.character(dates0[1]) != "none") {
#     make_figure_gif(file_end = file_end, 
#                     max_date = max_date,
#                     dir_out = dir_out, 
#                     filename0 = filename0)
#   }
#   
#   if (!(is.null(dir_googledrive_upload))) {
#     
#     drive_upload(
#       media = paste0(dir_out, filename0,'.png'), 
#       path = dir_googledrive_upload, 
#       overwrite = TRUE)
#     
#     drive_upload(
#       media = paste0(dir_out, filename0,'.pdf'), 
#       path = dir_googledrive_upload, 
#       overwrite = TRUE)
#     
#     # change each day of the survey
#     if (make_gifs | as.character(dates0[1]) != "none") {
#       drive_upload(
#         media = paste0(dir_out, filename0,'.gif'), 
#         path = dir_googledrive_upload, 
#         overwrite = TRUE)
#     }
#   }
#   
# }
