#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: functions
#' ---------------------------


# Install libraries ------------------------------------------------------------

PKG <- c(
  "devtools",
  # Mapping
  "akgfmaps", # devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
  "sf",
  "ggspatial", 
  
  #images
  "cowplot",
  "magick", 
  "qpdf",
  "janitor",
  "ggplot2", # Create Elegant Data Visualizations Using the Grammar of Graphics
  "viridis", 
  
  # tidy
  "readr", 
  "rmarkdown", 
  "tidyr", 
  "dplyr",
  "googledrive",
  # "magrittr",
  "stringr", 
  "here",
  "readxl",
  # "knitr", # A general-purpose tool for dynamic report generation in R
  # "officer"
  "quarto",
  "tinytex", # tinytex::install_tinytex(); https://yihui.org/tinytex/
  
  "RODBC", 
  "RCurl" # for ftp connection
)

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    if (p %in% c("akgfmaps", "coldpool", "gapindex")) {
      devtools::install_github(paste0("afsc-gap-products/", p))
    } else {
      install.packages(p, verbose = FALSE)
    }
    require(p,character.only = TRUE)}
}

# Functions --------------------------------------------------------------------

#' Wrapper function to produce daily or anomaly plots
#' 
#' This function prepares data to be used in the make_figures() function
#'
#' @param maxyr Numeric. The 4 digit year (YYYY) of the year that the data is from. 
#' @param srvy String. Accepted values include "BS" (EBS and NBS combined), "EBS", "NBS", "AI". "GOA" will be added as an option in 2023. 
#' @param haul data.frame. Here, derived from the RACEBASE.HAUL and RACE_DATA.V_CRUISES oracle tables. See example in data.R script. 
#' @param dat_survey data.frame. Must include reg_shapefile (the name of the shapefile polygon), survey (the formal name of the survey), srvy ("BS" (EBS and NBS combined), "EBS", "NBS", "AI". "GOA"), region (to match the region of the survey in the haul data), vessel_id (from RACE_DATA.VESSELS), vessel_shape (as listed in the google spreadsheet/how you want the vessel to appear on the plot), survey_dates (the planned dates of the survey).
#' @param var String. Default = "bt". "bt" will select the bottom temperature column and "st" will select the surface temperature column in haul data. 
#' @param dir_googledrive_upload googledrive::as_id("..."). Default = NULL. The location where outputs will be saved to in google drive. If NULL outputs will NOT be saved to googledrive. 
#' @param dates0 Default = "latest". A string of either the date to be plotted to ("YYYY-MM-DD"), a string of dates to be plotted (as.character(seq(as.Date("2022-07-30"), as.Date("2022-08-14"), by="days"))), "all" which will plot all of the days where data was collected for in that year, "first" that uses the first date of the time series, or "latest" where only the most recent date's data will be plotted, or "none" where only the grid (no data) will be printed. 
#' @param shp From akgfmaps::get_base_layers(select.region = ..., set.crs = "auto"). 
#' @param plot_subtitle String. Default = "". The desired subtitle for the figure. 
#' @param show_planned_stations Logical. Default = TRUE. TRUE will show planned stations on the plot and FALSE will not. 
#' @param data_source String. Default = "gd". "gd" will pull data from the google drive file and "oracle" will use the haul data (noted above) that comes from oracle. 
#' @param plot_anom Logical. Default = TRUE. TRUE will plot anomaly plots and FALSE will not. 
#' @param dir_wd String. Default = "./", or the root directory. Necessary because the task scheduler does not understand the concept of the R Project root directory (therefore, beware of the {here} R package.. This string should be the path to the R Project root directory. 
#' @param dir_out String. Default = "./", or the root directory. This is where files will be saved to. Necessary because the task scheduler does not understand the concept of the R Project root directory (therefore, beware of the {here} R package.. This string should be the path to the R Project root directory.  
#'
#' @return
#' @export
#'
#' @examples
make_varplot_wrapper <- function(
    maxyr, 
    srvy, 
    dat_survey, 
    var = "bt", 
    dir_googledrive_upload = NULL, 
    dates0 = "latest", 
    shp, 
    plot_subtitle = "", 
    show_planned_stations = TRUE, 
    data_source = "gd", 
    file_end0 = c("grid", "daily", "mean", "anom"), 
    dir_wd = "./"#, 
    # dir_out = "./"#, 
    # ftp
    ) {
  
  # Establish output directory -------------------------------------------------
  dir_out <- paste0(dir_wd, "output/", maxyr, "_", srvy, "/")
  dir.create(path = dir_out, showWarnings = FALSE)
  
  # Establish knowns and variables ---------------------------------------------
  srvy0 <- srvy
  srvy1 <- srvy
  if (srvy == "BS") {
    srvy1 <- c("EBS", "NBS")
  }
  
  shp$survey.strata <- shp$survey.strata[shp$survey.strata$srvy %in% srvy1,]
  shp$survey.area <- shp$survey.area[shp$survey.area$srvy %in% srvy1,]
  shp$survey.grid <- shp$survey.grid[shp$survey.grid$srvy %in% srvy1,]
  shp$lon.breaks <- shp$lon.breaks[[srvy]]
  shp$lat.breaks <- shp$lat.breaks[[srvy]]
  shp$bathymetry <- shp$bathymetry[shp$bathymetry$srvy %in% srvy,]
  shp$place.labels <- shp$place.labels[shp$place.labels$srvy %in% srvy,]

  height0 <- ifelse(srvy %in% c("AI", "GOA"), 6, 8)
  
  # Define var
  if (!is.null(var)){
    if (var == "bt") {
      var00 = 'Bottom Temperature'
      unit0 <- '(\u00B0C)'
    } else if (var == "st") {
      var00 = 'Surface Temperature'
      unit0 <- '(\u00B0C)'
    }
  }
  
  ## Wrangle Anomaly data -----------------------------------------------------
  anom_years <- dat_survey |>
    dplyr::select(
      srvy, year, stratum, station, date, survey, survey, survey_dates,
      vessel_shape, vessel_name, vessel_ital, st, bt) |>
    dplyr::distinct() |> 
    dplyr::filter(!(is.na(station)) &
                    year < maxyr &
                    srvy %in% srvy1) |>
    dplyr::select(srvy, survey, year) |> 
    dplyr::distinct() |>
    dplyr::group_by(srvy, survey) |> 
    dplyr::summarize(min = min(year, na.rm = TRUE), 
                     max = max(year, na.rm = TRUE), 
                     nn = n()) |> 
    dplyr::mutate(range = paste0(min, "-", max, " (", nn, " years)")) 
  
  # Calculate var averages from previous data
  dat_anom <- dat_survey |>
    dplyr::select(
      srvy, year, stratum, station, date, survey, survey, survey_dates,
      vessel_shape, vessel_name, vessel_ital, st, bt) |>
    dplyr::distinct() |> 
    dplyr::filter(year < maxyr &
                    srvy %in% srvy1) |>
    dplyr::rename("var" = dplyr::all_of(var)) |>
    dplyr::group_by(srvy, stratum, station) |> 
    dplyr::summarise(mean = mean(var, na.rm = TRUE), 
                     sd = sd(var, na.rm = TRUE))
  
  ## Wrangle observed (daily) data ---------------------------------------------
  
  dat <- dat_survey |>
    dplyr::filter(srvy %in% srvy1 & 
                    year == maxyr) |> 
    dplyr::select(srvy, stratum, station, date, year, 
                  vessel_shape, var = dplyr::all_of(var), 
                  survey_dates, survey, vessel_name, source)
  
  dat <- dplyr::left_join(
    x = dat,
    y = dat_anom,
    by = c("srvy", "stratum", "station"))
  
  dat <- dat |>
    dplyr::mutate(reg_lab = paste0(survey, "\n(", survey_dates, ")"), 
                  var = as.numeric(var), 
                  anom = var-mean) |>
    # anom = as.numeric(ifelse(sum(var0 == 0) == nrow(.), var0-mean, NA))) |>
    dplyr::arrange(date) |> 
    dplyr::ungroup()
  
  ## Wrangle shapefiles ---------------------------------------------
  
  # shp$lon.breaks <- shp$lon.breaks[srvy0][[1]] 
  # shp$lat.breaks <- shp$lat.breaks[srvy0][[1]] 
  # shp$plot.boundary <- shp$plot.boundary |> 
  #   dplyr::filter(srvy %in% srvy0)
  
  shp$survey.strata <- shp$survey.strata |> 
    # dplyr::filter(srvy %in% srvy0) |>
    dplyr::left_join(y = dat_survey |>
                       dplyr::select(stratum, srvy, survey, survey, survey_definition_id) |>
                       dplyr::distinct())
  
  shp$survey.area <- shp$survey.area |> 
    # dplyr::filter(srvy %in% srvy0) |>
    dplyr::left_join(y = dat_survey |>
                       dplyr::select(srvy, survey, survey_definition_id) |>
                       dplyr::distinct())
  
  shp$place.labels <- shp$place.labels |> 
    # dplyr::filter(srvy %in% srvy0) |>
    dplyr::left_join(y = dat_survey |>
                       dplyr::select(srvy, survey, survey_definition_id) |>
                       dplyr::distinct())
  
  shp$survey.grid <- shp$survey.grid |> 
    # dplyr::filter(srvy %in% srvy0) |> 
    dplyr::left_join(y = dat_survey |>
                       dplyr::select(station, stratum, srvy, survey, survey, survey_definition_id) |>
                       dplyr::distinct())
  
  shp$bathymetry <- shp$bathymetry |> 
    # dplyr::filter(srvy %in% srvy0) |>
    dplyr::left_join(y = dat_survey |>
                       dplyr::select(srvy, survey, survey_definition_id) |>
                       dplyr::distinct())
  
  is_there_data_to_plot <- TRUE
  if (nrow(dat[!is.na(dat$date),])==0) {
    is_there_data_to_plot <- FALSE
    message("NOTE: There are no daily data to plot. No daily or anomaly plots will be created. ") 
  }
  
  ### Grid ----------------------------------------------------------------------
  if ("grid" %in% file_end0) {
    file_end <- "grid"; print(paste0("------------", file_end, "------------"))
    
    dat00 = dat |>
      dplyr::filter(stratum != 0) |> 
      dplyr::mutate(reg_lab = ifelse(srvy == "BS", 
                                     paste0(survey, "\n"), 
                                     survey))
    var_breaks = NA
    plot_title = ifelse(srvy %in% c("GOA", "AI"), "Survey Districts", "Survey Grid")
    legend_title = ""
    dates0 = "none"
    make_gifs = FALSE
    show_planned_stations = FALSE
    
    make_figure(
      srvy = srvy, 
      dat00 = dat00,
      var_breaks = var_breaks,
      plot_title = plot_title,
      plot_subtitle = plot_subtitle,
      legend_title = legend_title,
      dates0 = dates0,
      shp = shp,
      file_end = file_end,
      dir_wd = dir_wd,
      dir_out = dir_out, 
      dir_googledrive_upload = dir_googledrive_upload, 
      make_gifs = make_gifs, 
      data_source = data_source,
      show_planned_stations = show_planned_stations, 
      height0 = height0)
  }
  
  ### Mean ---------------------------------------------------------------------
  if ("mean" %in% file_end0) {
    file_end <- "mean"; print(paste0("------------", file_end, "------------"))
    
    # Define temperature bins
    if (!is.null(var)){
      if (var == "bt") {
        if (srvy %in% c("BS", "EBS", "NBS")) {
          var_breaks <- c(-10, seq(from = -2, to = 8, by = 1), 50)
        } else if (srvy %in% c("GOA", "AI")) {
          var_breaks <- c(-10, seq(from = 2, to = 10, by = 1), 50)
        }
      } else if (var == "st") {
        var_breaks <- c(-10, seq(from = -2, to = 8, by = 1), 50) # if anom DOES NOT exist (straight temps!)
      }
    }
    
    for (virids_option in c("H"#, # Rainbow color scheme
                            # "B"
                            )){ # Inferno color scheme
      
      dat00 = dat |> # Mean plot
        dplyr::mutate(var = mean, 
                      reg_lab = paste0(survey, "\n "))
      plot_title = paste0("Time Series Mean ", var00)
      plot_subtitle = gsub(pattern = "and ", replacement = "and\n", 
                           x = paste0("NOAA Fisheries ", 
                                      text_list(paste0(anom_years$survey, " Bottom Trawl Survey ", anom_years$range)), 
                                      ifelse(nrow(anom_years)>1, "s", "")))
      legend_title = paste0(var00, '\nMean ', unit0)
      dates0 = "none"
      file_end = ifelse(virids_option == "B", paste0(file_end, "_cb"), file_end)
      make_gifs = FALSE
      show_planned_stations = FALSE
      
      make_figure(
        srvy = srvy, 
        dat00 = dat00,
        var_breaks = var_breaks, 
        plot_title = plot_title,
        plot_subtitle = plot_subtitle,
        legend_title = legend_title,
        dates0 = dates0, 
        shp = shp,
        file_end = file_end,
        dir_wd = dir_wd,
        dir_out = dir_out, 
        dir_googledrive_upload = dir_googledrive_upload, 
        make_gifs = make_gifs, 
        data_source = data_source,
        show_planned_stations = show_planned_stations, 
        height0 = height0, 
        var00 = var00, 
        virids_option = virids_option)
    }
  }
  
  ### Daily --------------------------------------------------------------------
  if ("daily" %in% file_end0 & is_there_data_to_plot) {  
    file_end <- "daily"; print(paste0("------------", file_end, "------------"))
    
    # Define temperature bins
    if (!is.null(var)){
      if (var == "bt") {
        if (srvy %in% c("BS", "EBS", "NBS")) {
          var_breaks <- c(-10, seq(from = -2, to = 8, by = 1), 50)
        } else if (srvy %in% c("GOA")) {
          var_breaks <- c(-10, seq(from = 3, to = 10, by = 1), 50)
        } else if (srvy %in% c("AI")) {
          var_breaks <- c(-10, seq(from = 3, to = 6, by = 0.5), 50)
        }
      } else if (var == "st") {
        var_breaks <- c(-10, seq(from = -2, to = 8, by = 1), 50) # if anom DOES NOT exist (straight temps!)
      }
    }
    
    for (virids_option in c("H" # , # Rainbow color scheme
                            # "B"
                            )){ # Inferno color scheme
      
      dat00 <- dat
      plot_title = paste0(maxyr, " ", var00, " ", unit0)
      legend_title = paste0(gsub(pattern = " ", replacement = "\n", x = var00), " ", unit0)
      file_end = ifelse(virids_option == "B", paste0(file_end, "_cb"), file_end) 
      make_gifs = TRUE
      
      make_figure(
        srvy = srvy, 
        dat00 = dat00,
        var_breaks = var_breaks, 
        plot_title = plot_title,
        plot_subtitle = plot_subtitle,
        legend_title = legend_title,
        dates0 = dates0, 
        shp = shp,
        file_end = file_end,
        dir_wd = dir_wd,
        dir_out = dir_out, 
        dir_googledrive_upload = dir_googledrive_upload, 
        make_gifs = make_gifs, 
        data_source = data_source,
        show_planned_stations = show_planned_stations, 
        height0 = height0, 
        var00 = var00, 
        virids_option = virids_option)
    }
  }
  
  ### Anomaly ------------------------------------------------------------------
  if ("anom" %in% file_end0 & is_there_data_to_plot) {  
    file_end <- "anom"; print(paste0("------------", file_end, "------------"))
    
    
    for (virids_option in c("H"#, # Rainbow color scheme
                            # "B"
                            )) { # Inferno color scheme
      
      dat00 = dat |> # Anomaly plot
        dplyr::filter(station != 0) |> # because show_planned_stations = FALSE
        dplyr::mutate(var = anom, 
                      reg_lab = ifelse(srvy == "BS", 
                                       paste0(survey, "\n"), 
                                       survey))
      var_breaks = c(-10, seq(from = -2, to = 3, by = 1), 50)
      plot_title = paste0(maxyr,  " ", var00, ' Anomaly' )
      plot_subtitle = gsub(pattern = "and ", replacement = "and\n", 
                           x = paste0("NOAA Fisheries ", 
                                      text_list(paste0(anom_years$survey, " Bottom Trawl Survey ", anom_years$range)), 
                                      ifelse(nrow(anom_years)>1, "s", "")))
      legend_title = paste0(var00, '\nAnomaly ', unit0)
      file_end = ifelse(virids_option == "B", paste0(file_end, "_cb"), file_end)
      make_gifs = FALSE
      show_planned_stations = FALSE
      
      make_figure(
        srvy = srvy, 
        dat00 = dat00,
        var_breaks = var_breaks, 
        plot_title = plot_title,
        plot_subtitle = plot_subtitle,
        legend_title = legend_title,
        dates0 = dates0, 
        shp = shp,
        file_end = file_end,
        dir_wd = dir_wd,
        dir_out = dir_out, 
        dir_googledrive_upload = dir_googledrive_upload, 
        make_gifs = make_gifs, 
        data_source = data_source,
        show_planned_stations = show_planned_stations, 
        height0 = height0, 
        var00 = var00, 
        virids_option = virids_option)
    }
  }
  
  message("RUN COMPLETE")
}

#' Create temperature and anomaly plots
#'
#' @param srvy String. Accepted values include "BS" (EBS and NBS combined), "EBS", "NBS", "AI". "GOA" will be added as an option in 2023. 
#' @param dat The csv file from googledrive or oracle with data for the 'maxyr' being plotted or compared to in the anomally. 
#' @param var_breaks String. Desired breaks for var to be binned into. 
#' @param plot_title String. Default = "". The desired title for the figure. 
#' @param plot_subtitle String. Default = "". The desired subtitle for the figure. 
#' @param legend_title String. Default = "". The desired legend title for the variable in mapped in the figure. 
#' @param dates0 A string of either the date to be plotted to ("YYYY-MM-DD"), "all" which will plot all of the days where data was collected for in that year, or "latest" where only the most recent date's data will be plotted, or "none" where only the grid (no data) will be printed. Default = "latest". 
#' @param shp From akgfmaps::get_base_layers(select.region = ..., set.crs = "auto"). 
#' @param height0 Numeric. Default = 6. Numeric height of the figure in inches. 
#' @param width0 Numeric. Default = 10.5. Numeric width of the figure in inches. 
#' @param file_end String. Will appear after "_" in the file name. Options include "grid" (will produce an empty grid), "daily" (will produce the daily updated plot), "anom" (will produce anomally differences in the updated plot).
#' @param make_gifs Logical. Default = TRUE means that a gifs will be compiled from the latest gif.  
#' @param dir_out String. A path for the files to be saved to on the local machine when the funciton is complete. 
#' @param dir_wd String. Default = "./", or the root directory. Necessary because the task scheduler does not understand the concept of the R Project root directory (therefore, beware of the {here} R package.. This string should be the path to the R Project root directory. 
#' @param show_planned_stations Logical. Default = TRUE. TRUE will show planned stations on the plot and FALSE will not. 
#' @param data_source String. Default = "gd". "gd" will pull data from the google drive file and "oracle" will use the haul data (noted above) that comes from oracle. 
#' @param dir_googledrive_upload googledrive::as_id("..."). Default = NULL. The location where outputs will be saved to in google drive. If NULL outputs will NOT be saved to googledrive. 
make_figure <- function(
    srvy, 
    dat00, 
    var_breaks, 
    plot_title = "",
    plot_subtitle = "",
    legend_title = "",
    dates0 = "latest",
    shp, 
    height0 = 8, 
    width0 = 10.5,
    file_end = "",
    make_gifs = TRUE,
    dir_out = "./", 
    dir_wd = "./",
    show_planned_stations = TRUE,
    data_source = "gd",
    dir_googledrive_upload = NULL, 
    var00 = "", 
    virids_option = "H") {
  
  # Set Base Layers ------------------------------------------------------------
  dat <- dat00
  if (show_planned_stations) {
    dat <- dat |> 
      dplyr::bind_rows(dat |>
                         dplyr::filter(date == min(date, na.rm = TRUE)) |> 
                         dplyr::mutate(date = (date-1), 
                                       stratum = 9999, 
                                       station = "9999") |> 
                         head(1))
  }
  
  # shp <- akgfmaps::get_base_layers(select.region = region_akgfmaps, set.crs = "auto")
  survey_reg_col <- gray.colors(length(unique(dat$srvy))+2) # region colors
  survey_reg_col <- survey_reg_col[-((length(survey_reg_col)-1):length(survey_reg_col))]
  shp$survey.area <- dplyr::left_join(
    x = shp$survey.area, 
    y = dat |> 
      dplyr::select(survey, reg_lab, srvy) |> 
      dplyr::distinct() |> 
      dplyr::mutate(survey_reg_col = c(alpha(survey_reg_col, 0.7))), 
    by = "srvy") |> 
    dplyr::arrange(desc(srvy))
  
  # Colors and bins for var ----------------------------------------------
  if (file_end %in% c("anom", "anom_cb", "daily_cb", "daily", "mean_cb", "mean")) {
    
    var_labels <- c()
    for(i in 2:c(length(var_breaks))) {
      var_labels <- c(var_labels, 
                      dplyr::case_when(
                        i==2 ~ paste0("\u2264 ",# "≤ ", #
                                      var_breaks[i]), # ,"\u00B0C" <=
                        i==(length(var_breaks)) ~ paste0("> ", var_breaks[i-1]), # , "\u00B0C"
                        TRUE ~ paste0("> ",
                                      var_breaks[i-1],"\u2013",var_breaks[i]) # ,"\u00B0C" "\u00B0"
                      ))
    }
    
    if (virids_option == "H") {
    var_color <- viridis::viridis_pal(option = virids_option)(length(var_labels))
    } else {
    var_color <- viridis::viridis_pal(begin = 0.2, end = 0.9, option = virids_option)(length(var_labels))
    # var_color <- viridis::viridis_pal(begin = 0.2, end = 0.9, option = "B")(length(var_labels))
    }
    
    # bin var
    dat <- dat |>
      dplyr::mutate(var_bin = base::cut(x = as.numeric(dat$var),
                                        breaks = var_breaks,
                                        labels = FALSE, 
                                        include.lowest = TRUE,
                                        right = FALSE) ) |>
      dplyr::mutate(var_bin = base::factor(x = var_labels[var_bin], 
                                           levels = var_labels, 
                                           labels = var_labels) ) 
    
    # Create new temperature maps
    date_entered <- sort(unique(dat$date))
    date_entered <- date_entered[!is.na(date_entered)]
    date_entered <- c(#min(date_entered),#-1, 
      date_entered)
    if (show_planned_stations & # TOLEDO
        (data_source %in% c("oracle", "race_data") | 
         sum(is.na(dat$var)) == 0)) { #survey is finished
      date_entered <- c(date_entered, max(date_entered)+1)
    }
  } else {
    dat <- dat |>
      dplyr::mutate(var_bin = NA)
  }
  
  # bind dat to shp$survey.grid for plotting
  # shp$survey.grid <- 
  #   dplyr::left_join(
  #     x = shp$survey.grid, 
  #     y = dat |>
  #       dplyr::select(station, stratum, var_bin, # reg_shapefile, 
  #                     survey, survey_dates, reg_lab, vessel_shape, date) |> 
  #       dplyr::distinct(), 
  #     by = c("stratum", "station")) 
  
  ## Date range of figures to create ------------------------------------
  
  if (dates0 == "none") { # If you are not using any data from temp data
    iterate <- 1 
  } else if (dates0 == "all") {
    iterate <- 1:length(date_entered)# if you want to run all of plots for each date_entered: 
    if (sum(is.na(dat$var))!=0 & # if the survey is not yet complete
        show_planned_stations & # if we plan to show planned stations
        sum(is.na(dat$var) & !is.na(dat$vessel_shape))>0) { # and if there are planned stations to show
      iterate <- iterate[-length(iterate)]
    }
  } else if (dates0 == "first") {
    iterate <- 1 
  } else if (dates0 == "latest") {
    iterate <- length(date_entered) # if you want to just run todays/a specific date:
    if (sum(is.na(dat$var))!=0 & # if the survey is not yet complete
        show_planned_stations & # if we plan to show planned stations
        sum(is.na(dat$var) &  # and if there are planned stations to show
            dat$station != 0 & # not including dummy stations
            !is.na(dat$vessel_shape))>0 &
        sum(is.na(dat$var)) != nrow(dat)) { # day before first day: if stations are planned, but have not been completed yet
      iterate <- iterate-1
    } 
  } else { # if you want to run a specific date
    #   iterate <- which((date_entered) %in% (dates0))
    # } if else(!(dates0[1] %in% c("none", "all", "first", "last","latest"))) { # if there is a specific range of dates
    iterate <- which(as.character(date_entered) %in% dates0)
  }  
  
  ## Loop through dates -----------------------------------------
  for (i in iterate) {
    
    start_time <- Sys.time()
    
    grid_stations_plot <- shp$survey.grid
    dat_plot <- dat    
    dat_planned <- NULL
    
    if (as.character(dates0[1]) == "none" & file_end == "grid") { # If you ARE NOT using any data from temp data
      
      max_date <- NA
      grid_stations_plot$var_bin <- NA  # only use dates including this date and before this date
      
    } else {
      max_date <- date_entered[i]
      print(max_date)
      message(paste0("plotting ", file_end, " for ", max_date))
      if (date_entered[i]==date_entered[length(date_entered)]) {
        next_date <- date_entered[i]
      } else {
        next_date <- date_entered[i+1]
      }
      
      if (file_end != "mean") {
      # only use dates including this date and before this date
        dat_plot$var[as.Date(dat_plot$date)>as.Date(max_date)]<-NA 
      }  
      grid_stations_plot <- grid_stations_plot |> 
        dplyr::left_join(dat_plot)

      if (file_end != "mean") {
        grid_stations_plot$var_bin[as.Date(grid_stations_plot$date)>as.Date(max_date)]<-NA 
        # only use dates including the next day before this date and before this date, so we can see the planned progression
        dat_plot$vessel_shape[dat_plot$date>next_date]<-NA
        dat_plot$date[dat_plot$date>next_date]<-NA
        grid_stations_plot$vessel_shape[grid_stations_plot$date > next_date]<-NA
        grid_stations_plot$date[grid_stations_plot$date > next_date]<-NA
      }      
      
      # separate out the data for the temperature and planned stations if there are planned stations listed
      # if (show_planned_stations & 
      #     sum(is.na(dat_plot$var) & !is.na(dat_plot$vessel_shape))>0) {
      if (#sum(is.na(dat$var))!=0 & # if the survey is not yet complete
        show_planned_stations & # if we plan to show planned stations
        sum(is.na(dat_plot$var) & !is.na(dat_plot$vessel_shape))>0) { # and if there are any planned stations to show
        
        # planned stations
        loc <- dat_plot |> 
          dplyr::filter(
            is.na(var) &
              !is.na(vessel_shape) & 
              date == next_date) |> 
          dplyr::mutate(planned = "Y")
        
        dat_planned <- grid_stations_plot |> 
          st_simplify(TRUE, dTolerance = 5000) |> 
          st_cast("MULTIPOLYGON") |> 
          st_centroid() |>
          st_coordinates() |>
          data.frame() |> 
          dplyr::rename(lon = X, 
                        lat = Y) |> 
          dplyr::mutate(
            station = grid_stations_plot$station, 
            stratum = grid_stations_plot$stratum) |>
          dplyr::left_join(x = ., 
                           y = loc |> 
                             dplyr::select(stratum, station, planned, vessel_shape, vessel_name, date), 
                           by = c("stratum", "station")) |>
          dplyr::filter(!is.na(planned)) |> 
          dplyr::mutate(lab = "")
        
        all_vess <- unique(dat_plot$vessel_shape)
        all_vess <- all_vess[!is.na(all_vess)]
        for (ii in 1:length(all_vess)) {
          vess <- all_vess[ii]
          if (sum(unique(dat_planned$vessel_shape) %in% vess)==0) {
            temp1 <- rep_len(x = NA, length.out = ncol(dat_planned))
            names(temp1) <- names(dat_planned)
            dat_planned <- dplyr::bind_rows(dat_planned, 
                                            temp1 |> 
                                              t() |>
                                              data.frame() |>
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
        dat_planned <- data.frame(
          station = NA, 
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
    
    # world_coordinates <- maps::map("world", plot = FALSE, fill = TRUE) |> 
    #   sf::st_as_sf() |>
    #   # sf::st_union() |> 
    #   sf::st_transform(crs = "EPSG:3338") |> 
    #   dplyr::filter(ID %in% c("USA", "Russia", "Canada")) |> 
    #   dplyr::mutate(ID = ifelse(ID == "USA", "Alaska", ID))
    
    world_coordinates <- akgfmaps::get_base_layers(select.region = "bs.all")$akland |>
      sf::st_transform(crs = "EPSG:3338")
    
    gg <- ggplot() +
      ggplot2::geom_sf(data = world_coordinates, 
                       fill = "black") + #ifelse(srvy %in% c("GOA", "AI"), "black", "white")) + 
      # ggplot2::geom_sf(data = shp$graticule, 
      #                  color = "grey90", 
      #                  alpha = 0.5) +
      ggplot2::scale_x_continuous(name = "Longitude",
                                  breaks = shp$lon.breaks,
                                  limits = c(sf::st_bbox(shp$survey.grid)$xmin, 
                                             sf::st_bbox(shp$survey.grid)$xmax)) +
      ggplot2::scale_y_continuous(name = "Latitude",
                                  breaks = shp$lat.breaks,
                                  limits = c(sf::st_bbox(shp$survey.grid)$ymin, 
                                             sf::st_bbox(shp$survey.grid)$ymax)) +
      ggplot2::ggtitle(label = plot_title, 
                       subtitle = plot_subtitle) +
      ggplot2::theme_minimal() + 
      ggplot2::theme(
        panel.border = element_rect(colour = "grey50", fill=NA, linewidth=.5), 
        plot.margin=unit(c(0,0,0,0), "cm") , 
        panel.background = element_rect(fill = "white"), #grey95
        plot.title = element_text(size = 20, face = "bold"), 
        plot.subtitle = element_text(size=14), 
        legend.text=element_text(size=12), 
        legend.position="right",
        legend.direction="vertical",
        legend.justification="left",
        legend.background = element_blank(),
        legend.title=element_text(size=14),
        axis.text = element_text(size=14), 
        legend.box.background = element_blank(),
        legend.key = element_blank(), 
        legend.key.size=(unit(.3,"cm")), 
        axis.title=element_text(size=14) ) # +
    # ggplot2::coord_sf(xlim = shp$plot.boundary$x, 
    #                   ylim = shp$plot.boundary$y)  +
    # ggplot2::coord_sf(xlim = c(sf::st_bbox(shp$survey.grid)$xmin, 
    #                            sf::st_bbox(shp$survey.grid)$xmax), # shp$plot.boundary$x,
    #                   ylim = c(sf::st_bbox(shp$survey.grid)$ymin, 
    #                            sf::st_bbox(shp$survey.grid)$ymax)) # shp$plot.boundary$y)
    
    if (srvy %in% c("BS", "EBS", "NBS")) {
      ## Bering Sea -----------------------------------------------------------
      
      gg <- gg +
        ggplot2::geom_sf(data = shp$survey.grid, 
                         colour = "grey50",
                         show.legend = legend_title)
      
      ### grid map ---------------------------------------------------
      if (file_end == "grid") {
        temp <- shp$place.labels[shp$place.labels$type == "bathymetry",]
        gg <- gg +
          geom_sf(data = shp$bathymetry) +
          guides(colour = 
                   guide_legend(override.aes = 
                                  list(fill = shp$survey.area$survey_reg_col)))  # survey regions
        
        if (nrow(temp)>0) {
          
          gg <- gg +
            ggplot2::geom_sf_text(
              data = temp, 
              mapping = aes(label = lab), 
              color = "darkblue", 
              fontface = "bold", 
              angle = 0)
        }
      }
      
      ### temperature plots ---------------------------------------------------------
      
      gg <- gg +
        # ggsn::scalebar(data = shp$survey.grid,
        #                location = "bottomleft",
        #                dist = 100,
        #                dist_unit = "nm",
        #                transform = FALSE,
        #                st.dist = 0.03,
        #                st.bottom = FALSE,
        #                model = shp$crs)  +
        ggplot2::annotate("text", 
                          x = quantile(sf::st_bbox(shp$survey.grid)[1]:sf::st_bbox(shp$survey.grid)[3], .9), 
                          y = quantile(sf::st_bbox(shp$survey.grid)[2]:sf::st_bbox(shp$survey.grid)[4], ifelse(srvy == "BS", .68, .8)), 
                          label = "Alaska", 
                          color = "white", 
                          size = 10)  +
        geom_sf(data = shp$survey.area, 
                aes(color = reg_lab), 
                linewidth = 1.5, 
                fill = "NA",
                show.legend = TRUE) +
        scale_color_manual(name = "Survey Region", 
                           values = shp$survey.area$survey_reg_col,  
                           breaks = shp$survey.area$reg_lab, 
                           labels = shp$survey.area$reg_lab) 
      
      
      # Add temperature squares
      if (file_end != "grid") { # If you are using any data from temp data
        
        gg <- gg +
          ggplot2::geom_sf(data = grid_stations_plot, 
                           aes(fill = var_bin), 
                           colour = "grey50",
                           # size = .5, 
                           show.legend = legend_title) +
          ggplot2::scale_fill_manual(
            name = legend_title,
            values = var_color, 
            labels = var_labels, 
            drop = FALSE,
            na.translate = FALSE) +
          ggspatial::coord_sf(
            xlim = c(sf::st_bbox(grid_stations_plot)[c(1,3)]),
            ylim = c(sf::st_bbox(grid_stations_plot)[c(2,4)])) 
        
        # if we are showing planned stations
        if (show_planned_stations) {
          
          gg <- gg +
            geom_point(data = dat_planned, 
                       mapping = aes(x = lon, y = lat, shape = factor(vessel_shape)), 
                       size = 4, na.rm = TRUE) +
            scale_shape_manual(
              name = "Planned Stations",
              values = sort(as.character(unique(dat_planned$vessel_shape))),
              breaks = sort(as.character(unique(dat_planned$vessel_shape))), 
              labels = sort(unique(dat_planned$lab))) +
            
            ggplot2::guides(
              fill = guide_legend(ncol = 2, 
                                  override.aes = list(colour = c("white"),
                                                      size = 0),
                                  order = 1),
              colour = guide_legend(order = 2, # survey regions
                                    override.aes = list(#color = "white", 
                                      fill = shp$survey.area$survey_reg_col)) ,
              shape = guide_legend(order = 3, # planned stations
                                   override.aes = list(fill = "grey95", 
                                                       linetype = c("blank")))  )
        } else { # we are not showing planned stations
          gg <- gg +
            ggplot2::guides(
              fill = guide_legend(ncol=2,  # tempartures # in case you want to have 2+ columns for the legend! 
                                  override.aes = list(colour = c("white"),
                                                      size = 0),
                                  order = 1),
              colour = guide_legend( # survey regions
                order = 2, 
                override.aes = list(fill = shp$survey.area$survey_reg_col))) 
        }
      }      
      
      if (file_end %in% c("daily" , "anom", "daily_cb", "anom_cb")) { 
        
        min_date <- min(as.Date(dat_plot$date), na.rm = TRUE)
        # if (show_planned_stations) { min_date <- min_date-1 } 
        
        gg <- gg +
          annotate("text", 
                   x = quantile(sf::st_bbox(shp$survey.grid)[1]:sf::st_bbox(shp$survey.grid)[3], .12), 
                   y = quantile(sf::st_bbox(shp$survey.grid)[2]:sf::st_bbox(shp$survey.grid)[4], .15), 
                   label = ifelse(is.na(max_date), 
                                  "", 
                                  ifelse(min_date == max_date, 
                                         paste0(format(x = min_date, "%b %d, %Y")), 
                                         paste0(format(x = min_date, "%b %d"), 
                                                " \u2013\n", 
                                                format(x = as.Date(max_date), format = "%b %d, %Y")))), 
                   color = "black", 
                   size = 5, 
                   fontface=2) 
      }
      
    } else if (srvy %in% c("AI", "GOA")) {
      ## Aleutian Islands and Gulf of Alaska ----------------------------------
      
      ### grid map ---------------------------------------------------
      
      gg <- gg +
        # Survey area
        ggplot2::geom_sf(data = shp$survey.area, 
                         color = "grey50", 
                         linewidth = .5, 
                         fill = "NA",
                         show.legend = TRUE) 
      
      # Draw bounding boxes
      bb <- data.frame()
      grid_stations_plot0 <- grid_stations_plot |> 
        dplyr::filter(!is.na(area_name))
      for (iiii in 1:length(unique(grid_stations_plot0$area_name))) {
        x <- unique(grid_stations_plot0$area_name)[iiii]
        grid_stations_plot1 <- grid_stations_plot
        grid_stations_plot1$area_name <- factor(grid_stations_plot1$area_name)        
        grid_stations_plot1 <- grid_stations_plot1[grid_stations_plot1$area_name == x,]
        
        a <- sf::st_bbox(grid_stations_plot1)
        b <- data.frame(t(as.numeric(unlist(a))))
        names(b) <- c("xmin", "ymin", "xmax", "ymax")
        b$lab <- x
        
        b$x <- mean(a[c(1,3)])
        b$y <- a[4]
        bb <- rbind.data.frame(bb, b)
        
        poly <- data.frame(a[c(2,4)], a[c(1,3)])
        names(poly) <- c("lat", "lon") 
        poly <- poly |>
          st_as_sf(coords = c("lon", "lat"), 
                   crs = "EPSG:3338") |> 
          sf::st_bbox() |> 
          sf::st_as_sfc()
        
        gg <- gg + 
          geom_sf(data = poly, fill = NA, color = "black", size = .5)
      }
      
      if (file_end == 'grid') {
        # label regions
        gg <- gg + 
          # full station grid
          ggplot2::geom_sf(
            data = shp$survey.grid,
            colour = "grey95", #ifelse((as.character(dates0[1]) == "none"), "grey50", "grey95"),
            size = ifelse(file_end == "grid", .05, .02),
            show.legend = FALSE)  + 
          ggplot2::geom_sf(data = shp$survey.area, 
                           colour = "grey50",
                           fill = "transparent",
                           show.legend = legend_title) +
          # fix extent
          ggspatial::coord_sf(
            xlim = c(sf::st_bbox(grid_stations_plot)[c(1,3)]),
            ylim = c(sf::st_bbox(grid_stations_plot)[c(2)], 
                     sf::st_bbox(grid_stations_plot)[c(4)]+50000)) + 
          ggplot2::geom_label(data = bb, 
                              color = ifelse(srvy == "AI", "black", "white"), 
                              fill = ifelse(srvy == "AI", NA, "black"),
                              fontface = "bold",
                              label.size = NA,
                              label.r = unit(0, "pt"),
                              mapping = 
                                aes(x = x, 
                                    y = ifelse(lab %in% c("Western Aleutians"), (y-25000), (y+25000)), 
                                    label = lab), 
                              size = 4)
      } 
      
      if (file_end != 'grid') {
        ### Create temperature plots ---------------------------------------------------------
        grid_stations_plot_visited <- grid_stations_plot |> 
          dplyr::filter(!is.na(var_bin)) |> 
          sf::st_centroid() |> 
          st_geometry() |> 
          data.frame() |>
          dplyr::mutate(
            var_bin = grid_stations_plot$var_bin[!is.na(grid_stations_plot$var_bin)])
        
        if (nrow(grid_stations_plot_visited)==0) {
          
          # still need the temperature legend, so adding in fake data off the map
          grid_stations_plot_visited <- grid_stations_plot |> 
            sf::st_centroid() |> 
            st_geometry() |> 
            data.frame()  |>
            dplyr::mutate(var_bin = 1:nrow(.)) |> 
            dplyr::filter(var_bin == 1) |>
            dplyr::mutate(
              var_bin = factor(levels(grid_stations_plot$var_bin)[1], 
                               levels = levels(grid_stations_plot$var_bin), 
                               labels = levels(grid_stations_plot$var_bin)))
          
          grid_stations_plot_visited$geometry[[1]][1] <- 1e12 # somewhere off the map
          grid_stations_plot_visited$geometry[[1]][2] <- 1e12 # somewhere off the map
        }
        
        gg <- gg +
          # full station grid
          # ggplot2::geom_sf(
          #   data = shp$survey.grid,
          #   colour = "grey95", #ifelse((as.character(dates0[1]) == "none"), "grey50", "grey95"),
          #   size = ifelse(file_end == "grid", .05, .02),
          #   show.legend = FALSE)  +
          # allocated station grid
          ggplot2::geom_sf(
            data = grid_stations_plot |> dplyr::filter(!is.na(year)), 
            colour = ifelse((as.character(dates0[1]) == "none"), "grey50", "grey70"),
            size = ifelse((as.character(dates0[1]) == "none"), .05, .02),
            show.legend = FALSE) +
          # temperate points
          ggplot2::geom_sf(
            data = grid_stations_plot_visited,
            mapping = aes(geometry = geometry,
                          fill = var_bin,
                          color = var_bin), na.rm = TRUE, 
            size = 3, 
            show.legend = legend_title)  + 
          ggplot2::scale_fill_manual(
            name = gsub(pattern = "\n", replacement = " ", x = legend_title),
            values = var_color,
            labels = var_labels,
            drop = FALSE,
            na.translate = FALSE,
            guide = guide_legend(
              # direction = "horizontal",
              label.position = "bottom",
              legend.justification = "center",
              nrow = 1,
              # label.theme = element_text(angle = 90)
              label.hjust = 0.5,
              label.vjust = 0.5,
            ) ) +
          ggplot2::theme(
            legend.direction = "horizontal", 
            legend.box = "horizontal", 
            legend.position = "bottom", 
            legend.spacing.x = unit(.5, 'cm')) +
          # legend managment
          ggplot2::scale_color_manual(
            name = "",
            values = var_color,
            labels = var_labels,
            drop = FALSE,
            na.translate = FALSE) +
          ggplot2::guides(color = "none") +
          # scale bar
          # ggsn::scalebar(data = grid_stations_plot,
          #                location = "bottomright",
          #                dist = 100,
          #                dist_unit = "nm",
          #                transform = FALSE,
          #                st.dist = 0.06,
          #                border.size = .25,
          #                height0 = 0.03,
          #                st.bottom = FALSE,
          #                st.size = 3, 
          #                model = shp$crs) +
          # fix extent
          ggspatial::coord_sf(
            xlim = c(sf::st_bbox(grid_stations_plot)[c(1,3)]),
            ylim = c(sf::st_bbox(grid_stations_plot)[c(2)], sf::st_bbox(grid_stations_plot)[c(4)]+40000)) 
        
        # if (file_end %in% c("daily", "anom")) {
        gg <- gg +
          ggplot2::annotate("text", 
                            # x = quantile(sf::st_bbox(shp$survey.grid)[1]:sf::st_bbox(shp$survey.grid)[3], .15), 
                            x = quantile(sf::st_bbox(shp$survey.grid)[1]:sf::st_bbox(shp$survey.grid)[3], ifelse(srvy == "AI", .1, .15)),  # ifelse(srvy == "AI", .60, .15)
                            y = quantile(sf::st_bbox(shp$survey.grid)[2]:sf::st_bbox(shp$survey.grid)[4], ifelse(srvy == "AI", .2, .80)), 
                            label = ifelse(is.na(max_date), 
                                           "", 
                                           ifelse(min(as.Date(dat$date), na.rm = TRUE) == max_date,
                                                  paste0(format(x = min(as.Date(dat_plot$date)+ifelse(show_planned_stations, 1, 0), na.rm = TRUE), "%b %d, %Y")), 
                                                  paste0(format(x = min(as.Date(dat_plot$date)+ifelse(show_planned_stations, 1, 0), na.rm = TRUE), "%b %d"), 
                                                         " \u2013\n", 
                                                         format(x = as.Date(max_date)+ifelse(show_planned_stations, 1, 0), format = "%b %d, %Y")))), 
                            color = "black", size = 5, fontface=2) 
      }
    }
    
    ## Save files --------------------------------------------------------------
    
    gg <- ggdraw(gg) +
      draw_image(image = paste0(dir_wd, "www/noaa-fish-wide.png"), # "www/noaa-50th-logo.png"
                 x = .37, y = ifelse(srvy %in% c("AI", "GOA") & file_end == "grid", .25, .32), # .38 # .43 # x = 0, y = 0, hjust = -4.12, vjust = -.45, width0 = .19
                 scale = .15 )
    
    filename0 <- paste0(
      ifelse((file_end %in% c("grid", "mean", "mean_cb")), "", as.character(max_date)), 
      "_", file_end)
    
    filename1 <- c()
    
    # only make current if it is the last plot of the run
    if (file_end %in% c("grid", "mean", "mean_cb")) {
      lastplotofrun <- TRUE
    } else {
      lastplotofrun <- (i == iterate[length(iterate)])
    }
    # is this the first or last day of the survey?
    lastdayofsurvey <- ((sum(is.na(dat_plot$date))) == 0)
    if (show_planned_stations) {
      lastdayofsurvey <- (lastdayofsurvey & 
                            nrow(dat_planned[!is.na(dat_planned$date),])==0)
    }
    firstplotofrun <- (i == 1)
    
    ### PNG -------------------------------------------------------------------------
    message("Create PNG")
    filename1 <- c(filename1, 
                   paste0(dir_out, filename0,'.png'))
    ggsave(filename = paste0(filename0,'.png'), 
           path = dir_out,
           height = height0, 
           width = width0,
           plot = gg, 
           dpi = 320,
           bg = "white", 
           device = "png") 
    
    ### PDF -------------------------------------------------------------------------
    message("Create PDF")
    
    filename1 <- c(filename1, 
                   paste0(dir_out, filename0,'.pdf'), 
                   paste0(dir_out, filename0,'.txt'))
    
    # RMarkdown ---
    rmarkdown::render(paste0(dir_wd, "/code/template.Rmd"),
                      output_dir = dir_out,
                      output_file = paste0(filename0, ".pdf"))

    file.remove(list.files(path = paste0(dir_wd, "/code/"),
                           pattern = ".log", full.names = TRUE))

    # # Quarto ---
    # quarto::quarto_render(  # render the quarto document
    #   input = paste0(dir_wd, "/code/template.qmd"),
    #   output_format = "pdf",
    #   output_file = paste0(filename0, ".pdf"),
    #   execute_params = list(
    #     "dat_plot" = dat_plot,
    #     "dat" = dat,
    #     "shp" = shp,
    #     "srvy" = srvy,
    #     "data_source" = data_source,
    #     "file_end" = file_end,
    #     "var00" = var00,
    #     "maxyr" = maxyr,
    #     "max_date" = max_date,
    #     "dir_out" = dir_out,
    #     "filename0" = filename0,
    #     "gg" = gg))
    # 
    # file.copy(from = here::here(paste0("report_",report_cruise, file_type)),
    #           to = here::here("output", report_yr, paste0("report_",report_cruise, file_type)),
    #           overwrite = TRUE)
    # file.remove(from = here::here(paste0("report_",report_cruise, file_type)))
    
    ### Combined PDF -----------------------------------------------------------------
    
    if (file_end %in% c("daily", "daily_cb", "anom", "anom_cb")) {  
      message("Create combined PDF of all daily/anom pdfs")
      
      filename1 <- c(filename1, 
                     paste0(dir_out, filename0,'_bind.pdf'))
      # remove file if already exists - qpdf::pdf_combine() will not overwrite
      if (length(list.files(path = dir_out, pattern = paste0(filename0, "_bind.pdf"))) != 0) {
        file.remove(paste0(dir_out, filename0, "_bind.pdf"))
      }
      
      # if the first day of the survey
      if (date_entered[1] == date_entered[i]) {
        input0 <- paste0(dir_out, filename0,'.pdf')
        if (file.exists(paste0(dir_out,'_grid.pdf'))) { 
          input0 <- c(input0, paste0(dir_out,'_grid.pdf'))
        } else {
          warning("WARNING: There is no _grid.pdf file to append this *_daily.pdf or *_anom.pdf file to in the *_bind.pdf to. ") 
        }
        qpdf::pdf_combine(
          input = input0,
          output = c(paste0(dir_out, filename0, "_bind.pdf")))
      } else {
        temp <- list.files(path = dir_out, pattern = paste0("_", file_end, "_bind.pdf"))
        if (length(temp)==0) {
          stop("ERROR: There are no other *_bind.pdf files in your output directory. Check and make sure you have run previous days of the surey before running this plot. ") 
        }
        temp <- temp[!grepl(pattern = "current", x = temp)]
        temp <- strsplit(x = temp, split = "_")
        temp <- as.Date(sort(sapply(temp,"[[",1)))
        temp <- max(temp[temp < max_date])
        
        qpdf::pdf_combine(input = c(paste0(dir_out, filename0, ".pdf"),
                                    paste0(dir_out, as.character(temp),"_", file_end, "_bind.pdf")),
                          output = c(paste0(dir_out, filename0, "_bind.pdf")))
        
      }
    }
    
    ### GIF -------------------------------------------------------------------------
    if (make_gifs) {
      message("Make GIF")
      filename1 <- c(filename1, 
                     paste0(dir_out, filename0,'.gif'))
      make_figure_gif(file_end = file_end, 
                      max_date = max_date,
                      dir_out = dir_out, 
                      filename0 = paste0(filename0))
    }
    
    ### CURRENT plots for easy finding -------------------------------------------
    if (lastplotofrun) {
      message("Make current_* files")
      temp <- list.files(path = dir_out, pattern = filename0, full.names = TRUE)
      temp <- temp[!grepl(pattern = "current_", x = temp)]
      if (grepl(pattern = "_cb", x = file_end)) {
        # if there is _cb in the name, only pull _cb named things
        temp <- temp[grepl(pattern = "_cb", x = temp)]
      } else {
        # if there is no _cb in the name, only pull non_cb named things
        temp <- temp[!grepl(pattern = "_cb", x = temp)]
      }
      for (iiii in 1:length(temp)) {
        if (file_end  %in% c("anom", "daily", "anom_cb", "daily_cb")) {
          filename00 <- gsub(pattern = max_date, replacement = "current", x = temp[iiii])
          filename00 <- gsub(pattern = paste0("current_", file_end, "."), 
                             replacement = paste0("current_", file_end, "_",tolower(srvy),"."), 
                             x = filename00, fixed = TRUE)
          filename00 <- gsub(pattern = paste0("current_", file_end, "_bind."), 
                             replacement = paste0("current_", file_end, "_bind_",tolower(srvy),"."), 
                             x = filename00, fixed = TRUE)
        } else if (file_end %in% c("grid", "mean", "mean_cb")) {
          filename00 <- gsub(pattern = paste0("_", file_end,"."), 
                             replacement = paste0("current_", file_end, "_",tolower(srvy),"."), 
                             x = temp[iiii], 
                             fixed = TRUE)
        }
        filename1 <- c(filename1, filename00) # add current files to list of files to upload
        file.copy(from = temp[iiii], 
                  to = filename00, 
                  overwrite = TRUE)
      }
      
      # ### FTP -------------------------------------------
      # # only make current if it is the last plot of the run
      # if (ftp$ftp_dl){
      #   message("Uploading files to FTP")
      #   upload_ftp(
      #     dir_in = filename1, # [grepl(pattern = "current_", x = filename1)],
      #     dest = ftp$dest, 
      #     user = ftp$user, 
      #     pass = ftp$pass)
      # }
      
    }
    
    ### GOOGLE DRIVE ------------------------------------------------------
    if (!(is.null(dir_googledrive_upload))) {
      message("Uploading files to googledrive")
      filename1 <- unique(filename1)
      dir_googledrive_upload_archive <- googledrive::drive_ls(path = googledrive::as_id(dir_googledrive_upload))
      dir_googledrive_upload_archive <- dir_googledrive_upload_archive$id[grep(pattern = "archive", x = dir_googledrive_upload_archive$name)]
      for (iii in 1:length(filename1)) {
          drive_upload(
            media = filename1[iii],
            path = googledrive::as_id(ifelse(grepl(pattern = "current", x = filename1[iii]), 
                                             dir_googledrive_upload, # if a current file, put in the main folder
                                             dir_googledrive_upload_archive)), # if the day-stamp file, put directly in the archive folder for safe keeping
            overwrite = TRUE)          
      }
    }
    
    end_time <- Sys.time()
    print((end_time - start_time))
  }
  
  # return(gg)
}

## figure helper functions -----------------------------------------------------

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

upload_ftp <- function(dir_in, 
                       dest, 
                       user, 
                       pass){
  
  for (iiii in 1:length(dir_in)) {
    print(dir_in[iiii])
    
    filename0 <- strsplit(x = dir_in[iiii], split = "/", fixed = TRUE)[[1]]
    filename0 <- filename0[length(filename0)]
    
    RCurl::ftpUpload(
      what = paste0(dir_in[iiii]), 
      asText = FALSE,
      to = paste0(glue::glue("{protocol}://STOR@{server}/{dest}/", filename0)),
      userpwd = paste0(user,":", pass),
      .opts=curlOptions(verbose=TRUE))
  }
}

## other helper functions ------------------------------------------------------

format_date_txt <- function(x = "1998-09-02", format0 = "%B %d") {
  if (!(isClass(Class = "Date", where = x))) {x <- as.Date(x)}
  x <- gsub("(\\D)0", "\\1", format(x, format0))
  return(x)
}

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
