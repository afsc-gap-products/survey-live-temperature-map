
###########PACKAGES###################


#############INSTALL PACKAGES##############
# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  # For creating R Markdown Docs
  "knitr", # A general-purpose tool for dynamic report generation in R
  "rmarkdown", # R Markdown Document Conversion, #https://stackoverflow.com/questions/33499651/rmarkdown-in-shiny-application
  
  # File Management
  "here", # For finding the root directory of your scripts and thus, find your files
  "officer",
  
  # Keeping Organized
  "devtools", # Package development tools for R; used here for downloading packages from GitHub
  # "renv", # saves the packages in the R environment
  
  
  # Graphics
  "ggplot2", # Create Elegant Data Visualisations Using the Grammar of Graphics
  "nmfspalette",  # devtools::install_github("nmfs-general-modeling-tools/nmfspalette"
  "cowplot",
  "png",
  "extrafont",
  
  # Text
  # "NMFSReports", # devtools::install_github("emilymarkowitz-noaa/NMFSReports") # Package of my favorite grammar and file managment functions for writing reproducible reports
  
  # Citations
  "knitcitations", # devtools::install_github("cboettig/knitcitations")
  
  # other tidyverse
  "dplyr",
  # "googledrive",
  "magrittr",
  "readr",
  "magrittr", 
  "stringr", 
  "data.table",
  #Piping/operators which promote semantics
  "tidyr", 
  # library(tidyverse)
  "glue", 

  # Text Management
  "stringr",
  "htmltools", 
  "htmlwidgets",
  
  # Spatial
  "akgfmaps", # devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
  "sf",
  "rlist", 
  "jsonlite", 
  "prettymapr",
  "rgdal", 
  "rosm", 
  "shadowtext", 
  "ggspatial", 
  "digest", 
  "ggsn",
  "rgdal", 
  "ps", 
  "backports", 
  "callr", 
  "labeling", 
  "gstat", 
  "raster", 
  "reshape", 
  "stars",
  "mapview",
  "maptools", 
  "spatialEco", 
  
  # Time
  # "lubridate",
  
  # Species
  "taxize", 
  
  # For outputting JS files
  "jsonlite", 
  
  # For editing XML files
  "XML", 
  
  # Oracle
  "RODBC", 
  
  #shiny
  "shiny", # Need for running Shiny apps
  "shinydashboard", 
  "shinythemes", 
  "shinyauthr", 
  
  # Use Java Script
  "shinyjs", 
  "shinyBS", 
  "V8", 
  
  # For table formatting
  "DT", 
  "kableExtra", 
  "formattable", 
  

  
  "extrafont", # install.packages("extrafont")
  # loadfonts()
  # extrafont::font_import()
  #windowsFonts()
  
  # colors
  # https://drive.google.com/file/d/1EwZOVAzzqwniXczu611yk6uXs4_TuRwH/view?usp=sharing
  "nmfspalette", # oceans, waves, seagrass, urchin, crusteacean, coral
  "viridis", 
  
  # leaflet
  "leaflet", 
  "leafem", 
  "leafpop", 
  
  # spatial
  "rgeos", 
  "sp", 
  "rgdal", 
  "gstat", 
  "sp",     # Used for the spsample function
  "KernSmooth")


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}


licence0 <- "Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States."


########COLORS#########
NOAAFisheries.Colors<-list(
  
  Oceans = list(
  "Process Blue" = "#0093D0", 
  "Reflex Blue" = "#0055A4", #Nav Bar Hover
  "PMS 541" = "#00467F", # Nav Bar
  "White" = "#FFFFFF"
  ), 
  
  Waves = list(
  "PMS 319" = "#1ECAD3", 
  "PMS 321" = "#008998", 
  "PMS 322" = "#00708", 
  "Gray 10%" = "#E8E8E8"
  ),

  Seagrass = list(
  "PMS 375" = "#93D500", 
  "PMS 362" = "#4C9C2E", 
  "PMS 322" = "#007078", 
  "Gray 20%" = "#D0D0D0"
  ), 
  
  Urchin = list(
  "Custom" = "#7F7FFF", 
  "PMS 2725" = "#625BC4", 
  "PMS 7670" = "#575195",
  "Gray 40%" = "#9A9A9A"
  ), 
  
  Crustacean = list(
    "PMS 151" = "#FF8300", 
    "PMS 717" = "#D65F00", 
    "PMS 7670" = "#575195", 
    "Gray 50%" = "#7B7B7B"
  ), 
  
  Coral = list(
    "Warm Red" = "#FF4438", 
    "PMS 711" = "D02C2F", 
    "PMS 1805" = "#B2292E", 
    "Gray 70%" = "#646464"
  ),
  
  "NOAA Colors" = list(
  
  #Primary Colors
  "REFLEX BLUE" = "#0A4595", 
  "PROCESS BLUE" = "#0099D8", 
  "DARK SLATE GREY" = "#333333", 
  "WHITE" = "#FFFFFF", 
  
  #Secondary Colors
  "DARK GREY" = "#575757", 
  "MEDIUM GREY" = "#666666",
  "LIGHT GREY" = "#ACACAC",
  "FADED BLUE" = "#6B84B4",
  "RICH BLUE GREY" = "#28282A"
  )

)

NOAA.Fonts<-"Proxima Nova"


######FUNCTIONS#######
# https://stackoverflow.com/questions/37446283/creating-legend-with-circles-leaflet-r
addLegendCustom <- function(map, position = "bottomright", title = "", colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, position = position, title = title, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}

ageoffile<-function(path) {
  # system("touch temp")
  info <- file.info(path)
  x<-format(info$mtime,"%B %d, %Y")
  return(x)
}

SameColNames<-function(df.ls) {
  #All column names
  colnames0<-c()
  for (i in 1:length(df.ls)){
    df0<-df.ls[[i]]
    # colnames(df0)<-toupper(colnames(df0))
    df0<-janitor::clean_names(df0)
    df.ls[[i]]<-df0
    colnames0<-c(colnames0, (colnames(df0)))
  }
  colnames0<-sort(unique(colnames0), decreasing = T)
  
  #New df's
  df.ls0<-list()
  df.rbind0<-c()
  for (i in 1:length(df.ls)){
    df0<-df.ls[[i]]
    colnames.out<-colnames0[!(colnames0 %in% colnames(df0))]
    if (length(colnames.out) != 0) {
      for (ii in 1:length(colnames.out)){
        df0[,(ncol(df0)+1)]<-NA
        names(df0)[ncol(df0)]<-colnames.out[ii]
      }
    }
    df0<-df0[,match(table =  colnames(df0), x = colnames0)]
    df.ls0[[i]]<-df0
    names(df.ls0)[i]<-names(df.ls)[i]
    df.rbind0<-rbind.data.frame(df.rbind0, df0)
  }
  return(df.rbind0)
}

scale_values<-function(X) {
  # X <- X[X != 0]
  val <- ((X - min(X, na.rm = TRUE))/diff(range(X, na.rm = TRUE))) 
  return(val)
}

make_idw_map0 <- 
  function (x = NA, COMMON_NAME = NA, LATITUDE = NA, LONGITUDE = NA, 
            CPUE_KGHA = NA, region = "bs.south", extrap.box = NA, 
            set.breaks = "jenks", grid.cell = c(0.05, 0.05), in.crs = "+proj=longlat", 
            out.crs = "auto", key.title = "auto", log.transform = FALSE, 
            idw.nmax = 4, use.survey.bathymetry = TRUE, return.continuous.grid = TRUE) {
    if (is.na(x)) {
      x <- data.frame(COMMON_NAME = COMMON_NAME, 
                      LATITUDE = LATITUDE, 
                      LONGITUDE = LONGITUDE, 
                      CPUE_KGHA = CPUE_KGHA)
    }
    if (key.title == "auto") {
      key.title <- x$COMMON_NAME[1]
    }
    if (is.na(extrap.box)) {
      if (region %in% c("bs.south", "sebs")) {
        extrap.box = c(xmn = -179.5, xmx = -157, 
                       ymn = 54, ymx = 63)
      }
      if (region %in% c("bs.all", "ebs")) {
        extrap.box = c(xmn = -179.5, xmx = -157, 
                       ymn = 54, ymx = 68)
      }
      if (region %in% c("bs.north", "nbs")) {
        extrap.box = c(xmn = -176.5, xmx = -160, 
                       ymn = 58, ymx = 66)
      } 
    }
    
    map_layers <- get_base_layers0(select.region = region, 
                                   set.crs = out.crs)
    out.crs <- map_layers$crs
    if (use.survey.bathymetry) {
      map_layers$bathymetry <- get_survey_bathymetry0(select.region = region, 
                                                      set.crs = out.crs)
    }
    x <- sf::st_as_sf(x, coords = c(x = "LONGITUDE", y = "LATITUDE"), 
                      crs = sf::st_crs(in.crs)) %>% 
      sf::st_transform(crs = map_layers$crs)
    idw_fit <- gstat::gstat(formula = CPUE_KGHA ~ 1, 
                            locations = x, 
                            nmax = idw.nmax)
    stn.predict <- predict(idw_fit, x)
    sp_extrap.raster <- raster::raster(xmn = extrap.box["xmn"], 
                                       xmx = extrap.box["xmx"], 
                                       ymn = extrap.box["ymn"], 
                                       ymx = extrap.box["ymx"], 
                                       ncol = (extrap.box["xmx"] - 
                                                 extrap.box["xmn"])/grid.cell, 
                                       nrow = (extrap.box["ymx"] - 
                                                 extrap.box["ymn"])/grid.cell, 
                                       crs = crs(in.crs)) 
    if (as.character(crs(x)) != as.character(crs(sp_extrap.raster))) {
      sp_extrap.raster <- sp_extrap.raster %>% 
        projectRaster(crs = crs(x))
    }
    extrap.grid <- predict(idw_fit, 
                           as(sp_extrap.raster, "SpatialPoints")) %>% 
      sf::st_as_sf() %>% 
      sf::st_transform(crs = crs(x)) %>% 
      stars::st_rasterize() %>% 
      sf::st_join(map_layers$survey.area, 
                  join = st_intersects)
    
    if (return.continuous.grid) {
      continuous.grid <- extrap.grid
    } else {
      continuous.grid <- NA
    }
    alt.round <- 0
    if (is.character(set.breaks[1])) {
      set.breaks <- tolower(set.breaks)
      break.vals <- classInt::classIntervals(x$CPUE_KGHA, n = 5, 
                                             style = set.breaks)$brks
      alt.round <- floor(-1 * (min((log10(break.vals) - 2)[abs(break.vals) > 
                                                             0])))
      set.breaks <- c(-1, round(break.vals, alt.round))
    }
    if (min(set.breaks) > 0) {
      set.breaks <- c(0, set.breaks)
    }
    if (min(set.breaks) == 0) {
      set.breaks <- c(-1, set.breaks)
    }
    if (max(set.breaks) < max(stn.predict$var1.pred)) {
      set.breaks[length(set.breaks)] <- max(stn.predict$var1.pred) + 1
    }
    
    dig.lab <- 7
    set.levels <- cut(x = stn.predict$var1.pred, set.breaks, right = TRUE, 
                      dig.lab = dig.lab)
    
    # if (alt.round > 0) {
    #   while (dig.lab > alt.round) {
    #     dig.lab <- dig.lab - 1
    #     set.levels <- cut(x = stn.predict$var1.pred,
    #                       breaks = set.breaks,
    #                       right = TRUE,
    #                       dig.lab = dig.lab)
    #   }
    # } else {
    #   while (length(grep("\\.", set.levels)) > 0) {
    #     dig.lab <- dig.lab - 1
    #     print(dig.lab)
    #     set.levels <- base::cut(x = stn.predict$var1.pred,
    #                       breaks = set.breaks,
    #                       right = TRUE,
    #                       dig.lab = dig.lab)
    #   }
    # }
    
    # if (alt.round > 0) {
    #   while (dig.lab > alt.round) {
    #     dig.lab <- dig.lab - 1
    #     set.levels <- cut(stn.predict$var1.pred, set.breaks, 
    #                       right = TRUE, dig.lab = dig.lab)
    #   }
    # }
    # else {
    #   while (length(grep("\\.", set.levels)) > 0) {
    #     dig.lab <- dig.lab - 1
    #     # print(dig.lab)
    #     set.levels <- cut(stn.predict$var1.pred, set.breaks, 
    #                       right = TRUE, dig.lab = dig.lab)
    #   }
    # }
    
    extrap.grid$var1.pred <- cut(extrap.grid$var1.pred, 
                                 set.breaks, 
                                 right = TRUE, 
                                 dig.lab = dig.lab)
    
    sig.dig <- round(set.breaks[which(nchar(round(set.breaks)) >= 4)])
    
    make_level_labels <- function(vec, sig.dig) {
      vec <- as.character(vec)
      vec[grep("-1", vec)] <- "No catch"
      vec <- sub("\\(", "\\>", vec)
      vec <- sub("\\,", "–", vec)
      vec <- sub("\\]", "", vec)
      if (length(sig.dig) > 3) {
        for (j in 1:length(sig.dig)) {
          vec <- sub(sig.dig[j], format(sig.dig[j], nsmall = 0, 
                                        big.mark = ","), vec)
        }
      }
      return(vec)
    }
    
    extrap.grid$var1.pred <- factor(make_level_labels(vec = extrap.grid$var1.pred, sig.dig), 
                                    levels = make_level_labels(levels(set.levels), sig.dig))
    
    
    n.breaks <- length(levels(set.levels))
    
    p1 <- ggplot() + 
      geom_sf(data = map_layers$survey.area, fill = NA) + 
      geom_stars(data = extrap.grid) + 
      geom_sf(data = map_layers$survey.area, 
              fill = NA) + 
      # geom_sf(data = map_layers$akland$geometry[2],
      #         fill = "grey80") +
      geom_sf(data = map_layers$akland,
              fill = "grey80") +
      # geom_sf(data = sf::st_shift_longitude(sf::st_shift_longitude(map_layers$akland$geometry[1])),
      #         fill = "grey80") +
      # geom_sf(data = sf::st_wrap_dateline(map_layers$akland),
      #         fill = "grey80") +
      geom_sf(data = map_layers$bathymetry) + 
      geom_sf(data = map_layers$graticule, 
              color = alpha("grey70", 0.3)) + 
      scale_fill_manual(name = paste0(key.title, "\nCPUE (kg/ha)"), 
                        values = c("white", RColorBrewer::brewer.pal(9, name = "Blues")[c(2, 4, 6, 8, 9)]),
                        na.translate = FALSE, 
                        drop = FALSE) + 
      scale_x_continuous(breaks = map_layers$lon.breaks) + 
      scale_y_continuous(breaks = map_layers$lat.breaks) + 
      coord_sf(xlim = map_layers$plot.boundary$x, 
               ylim = map_layers$plot.boundary$y) + 
      theme(panel.border = element_rect(color = "black", fill = NA), 
            panel.background = element_rect(fill = NA, color = "black"), 
            legend.key = element_rect(fill = NA, color = "grey70"), 
            legend.position = c(0.12, 0.18), 
            axis.title = element_blank(), 
            axis.text = element_text(size = 10), 
            legend.text = element_text(size = 10), 
            legend.title = element_text(size = 10), 
            plot.background = element_rect(fill = NA, color = NA))
    
    return(list(plot = p1, map_layers = map_layers, extrapolation.grid = extrap.grid, 
                continuous.grid = continuous.grid, region = region, n.breaks = n.breaks, 
                key.title = key.title, crs = out.crs))
  }



get_base_layers0 <- 
  function (select.region, 
            set.crs = "+proj=longlat +datum=NAD83", 
            use.survey.bathymetry = FALSE) {
    if (set.crs == "auto") {
      region.crs <- c("+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=68.1 +lat_2=70.7 +lat_0=69.4 +lon_0=-162.6 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=60.8 +lat_2=67 +lat_0=63.9 +lon_0=-167 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=60 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=60 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
      set.crs <- region.crs[match(select.region, c("bs.south", 
                                                   "sebs", "bs.all", "ebs", "ecs", 
                                                   "ebs.ecs", "bs.north", "nbs"))]
    }
    akland <- sf::st_read(system.file("data", "ak_russia.shp", 
                                      package = "akgfmaps"), quiet = TRUE)

    bathymetry <- sf::st_read(system.file("data", "npac_0-200_meters.shp", 
                                          package = "akgfmaps"), quiet = TRUE)
    if (select.region %in% c("bs.south", "sebs")) {
      survey.area <- sf::st_read(system.file("data", 
                                             "sebs_survey_boundary.shp", package = "akgfmaps"), 
                                 quiet = TRUE)
      survey.strata <- sf::st_read(system.file("data", 
                                               "sebs_strata.shp", package = "akgfmaps"), 
                                   quiet = TRUE)
      plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(
        x = c(-177.3, -154.3), 
        y = c(54.5, 63.15)), 
        out.crs = set.crs)
      graticule <- st_graticule(lat = seq(54, 64, 2), 
                                lon = seq(-180, -140, 5), 
                                margin = 1e-05)
      lon.breaks <- seq(-180, -154, 5)
      lat.breaks <- seq(54, 64, 2)
    }
    if (select.region %in% c("bs.all", "ebs")) {
      survey.area <- sf::st_read(system.file("data", 
                                             "ebs_survey_boundary.shp", package = "akgfmaps"), 
                                 quiet = TRUE)
      survey.strata <- sf::st_read(system.file("data", 
                                               "ebs_strata.shp", package = "akgfmaps"), 
                                   quiet = TRUE)

      plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(
        x = c(-177.8, -154.7), 
        y = c(54, 65.1)), 
        out.crs = set.crs)
      graticule <- st_graticule(lat = seq(54, 68, 2), 
                                lon = seq(-180, -140, 5), 
                                margin = 1e-05)
      lon.breaks <- seq(-180, -154, 5)
      lat.breaks <- seq(54, 66, 2)
    }
    if (select.region %in% c("bs.north", "nbs")) {
      survey.area <- sf::st_read(system.file("data", 
                                             "ebs_survey_boundary.shp", package = "akgfmaps"), 
                                 quiet = TRUE)$geometry[2]
      
      survey.strata <- sf::st_read(system.file("data", 
                                               "ebs_strata.shp", package = "akgfmaps"), 
                                   quiet = TRUE) %>% 
        dplyr::filter(STRATUM %in% c(70, 71, 81))
      
      plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(
        # x = c(-177.8, -154.7), 
        # y = c(63.15, 65.1)), 
        x = c(-176.5, -160), 
        y = c(60, 66)), 
        out.crs = set.crs)
      
      graticule <- st_graticule(lat = seq(64, 68, 2), 
                                lon = seq(-180, -140, 5), 
                                margin = 1e-05)
      lon.breaks <- seq(-180, -154, 5)
      lat.breaks <- seq(64, 66, 2)
    }
    if (select.region == "ecs") {
      survey.area <- sf::st_read(system.file("data", 
                                             "chukchi_survey_boundary.shp", package = "akgfmaps"), 
                                 quiet = TRUE)
      survey.strata <- sf::st_read(system.file("data", 
                                               "chukchi_strata.shp", package = "akgfmaps"), 
                                   quiet = TRUE)
      plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-170, -156), 
                                                                     y = c(65, 73)), 
                                                          out.crs = set.crs)
      graticule <- st_graticule(lat = seq(60, 76, 2), lon = seq(-180, -140, 5), margin = 1e-05)
      lon.breaks <- seq(-180, -154, 5)
      lat.breaks <- seq(66, 76, 2)
    }
    if (select.region == "ebs.ecs") {
      survey.area <- sf::st_read(system.file("data", 
                                             "ebs_chukchi_survey_boundary.shp", package = "akgfmaps"), 
                                 quiet = TRUE)
      survey.strata <- sf::st_read(system.file("data", 
                                               "ebs_chukchi_strata.shp", package = "akgfmaps"), 
                                   quiet = TRUE)
      bathymetry <- sf::st_read(system.file("data", "npac_0-200_meters.shp", 
                                            package = "akgfmaps"), quiet = TRUE)
      plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177, -151), 
                                                                     y = c(54.5, 72.5)), 
                                                          out.crs = set.crs)
      graticule <- st_graticule(lat = seq(54, 78, 4), 
                                lon = seq(-180, -140, 5), 
                                margin = 1e-05)
      lon.breaks <- seq(-180, -150, 5)
      lat.breaks <- seq(54, 78, 4)
    }
    if (tolower(class(set.crs)) != "crs") {
      set.crs <- sf::st_crs(set.crs)
    }
    akland <- akland %>% sf::st_transform(crs = set.crs)
    survey.area <- survey.area %>% sf::st_transform(crs = set.crs)
    survey.strata <- survey.strata %>% sf::st_transform(crs = set.crs)
    bathymetry <- bathymetry %>% sf::st_transform(crs = set.crs)
    place.labels <- read.csv(file = system.file("data",
                                                "placenames.csv", package = "akgfmaps")) %>%
      dplyr::filter(region == select.region) %>% akgfmaps::transform_data_frame_crs(out.crs = set.crs)
    return(list(akland = akland, survey.area = survey.area, survey.strata = survey.strata, 
                bathymetry = bathymetry, place.labels = place.labels, 
                graticule = graticule, crs = set.crs, plot.boundary = plot.boundary, 
                lon.breaks = lon.breaks, lat.breaks = lat.breaks))
  }


get_survey_bathymetry0 <- 
  function (select.region, set.crs) {
    
    if (set.crs == "auto") {
      region.crs <- c("+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=68.1 +lat_2=70.7 +lat_0=69.4 +lon_0=-162.6 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=60.8 +lat_2=67 +lat_0=63.9 +lon_0=-167 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=60 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                      "+proj=aea +lat_1=60 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
      set.crs <- region.crs[match(select.region, c("bs.south", 
                                                   "sebs", "bs.all", "ebs", "ecs", 
                                                   "ebs.ecs", "bs.north", "nbs"))]
    }
    
    if (select.region %in% c("bs.all", "ebs")) {
      bathymetry <- sf::st_read(system.file("data", "ebs_survey_bathymetry.shp", 
                                            package = "akgfmaps"), quiet = TRUE) %>% st_transform(crs = set.crs)
    } else if (select.region %in% c("bs.south", "sebs")) {
      bathymetry <- sf::st_read(system.file("data", "ebs_survey_bathymetry.shp", 
                                            package = "akgfmaps"), quiet = TRUE) %>% st_transform(crs = set.crs) %>% 
        dplyr::filter(FNODE_ != 5)
    } else if (select.region %in% c("bs.north", "nbs")) {
      bathymetry <- sf::st_read(system.file("data", "ebs_survey_bathymetry.shp", 
                                            package = "akgfmaps"), quiet = TRUE) %>% st_transform(crs = set.crs) %>% 
        dplyr::filter(FNODE_ == 5)
    } else {
      stop(paste0("No survey-specific bathymetry available for ", 
                  select.region, ". If using make_idw_map, set use.survey.bathymetry = FALSE."))
    }
    return(bathymetry)
  }

