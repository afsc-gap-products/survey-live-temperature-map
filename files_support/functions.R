
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

  
  # Graphics
  "ggplot2", # Create Elegant Data Visualisations Using the Grammar of Graphics
  "nmfspalette",  # devtools::install_github("nmfs-general-modeling-tools/nmfspalette"
  "cowplot",
  "png",
  "extrafont",
  "akgfmaps", # devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
  "coldpool", # devtools::install_github("sean-rohan-NOAA/coldpool", build_vignettes = TRUE)
  
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
  
  "viridis",
  "coldpool",
  
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
  
  "rnaturalearth", 
  "rnaturalearthdata", 
  
  # colors
  # https://drive.google.com/file/d/1EwZOVAzzqwniXczu611yk6uXs4_TuRwH/view?usp=sharing
  "nmfspalette", # oceans, waves, seagrass, urchin, crusteacean, coral
  "viridis", 
  
  # leaflet
  "leaflet", 
  "leafem", 
  "leafpop", 
  "leaflet.extras"#, 
  
  # spatial
  # "sf", 
  # "rgeos", 
  # "sp", 
  # "rgdal", 
  # "gstat", 
  # "sp",     # Used for the spsample function
  # "KernSmooth"
  )

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


