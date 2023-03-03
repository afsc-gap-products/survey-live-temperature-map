#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: download oracle data
#' ---------------------------

# *** Oracle -------------------------------------------------------------------

# This has a specific username and password because I DONT want people to have access to this!
source("https://raw.githubusercontent.com/afsc-gap-products/metadata/main/code/functions_oracle.R")

locations <- c("C:/Users/liz.dawson/Work/Projects/ConnectToOracle.R", 
               "Z:/Projects/ConnectToOracle.R", 
               "C:/Users/emily.markowitz/Work/Projects/ConnectToOracle.R")
for (i in 1:length(locations)){
  if (file.exists(locations[i])){
    source(locations[i])
  }
}
# I set up a ConnectToOracle.R that looks like this: 
#   
#   PKG <- c("RODBC")
# for (p in PKG) {
#   if(!require(p,character.only = TRUE)) {  
#     install.packages(p)
#     require(p,character.only = TRUE)}
# }
# 
# channel<-odbcConnect(dsn = "AFSC",
#                      uid = "USERNAME", # change
#                      pwd = "PASSWORD", #change
#                      believeNRows = FALSE)
# 
# odbcGetInfo(channel)

locations<-c(
  # "RACE_DATA.VESSELS", 
  "GOA.GOA_STRATA", 
  "RACEDATA.V_CRUISES",
  # "AI.AIGRID_GIS",
  # "RACEBASE.HAUL", 
  # "RACE_DATA.V_CRUISES"
  "RACEBASE_FOSS.JOIN_FOSS_CPUE_HAUL"
)

oracle_dl(
  locations = locations, 
  channel = channel, 
  dir_out = paste0(dir_wd, "/data/oracle/"))
