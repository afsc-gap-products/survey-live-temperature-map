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
#' # I set up a ConnectToOracle.R that looks like this: 
#' #   
#' #' Define RODBC connection to ORACLE
#' #'
#' #' @param schema default = 'AFSC'. 
#' #'
#' #' @return oracle channel connection
#' #' @export
#' #'
#' #' @examples
#' #' # Not run
#' #' # channel <- oracle_connect()
#' oracle_connect <- function(
#'     schema='AFSC', 
#'     username = NULL, 
#'     passowrd = NULL){(echo=FALSE)
#'   
#'   library("RODBC")
#'   library("getPass")
#'   if (is.null(username)) {
#'     username <- getPass(msg = "Enter your ORACLE Username: ")
#'   }
#'   if (is.null(password)) {
#'     password <- getPass(msg = "Enter your ORACLE Password: ")
#'   }
#'   channel  <- RODBC::odbcConnect(
#'     paste(schema),
#'     paste(username),
#'     paste(password), 
#'     believeNRows=FALSE)
#'   return(channel)
#' }

locations<-c(
  # "RACE_DATA.VESSELS", 
  "GOA.GOA_STRATA", 
  "RACE_DATA.V_CRUISES",
  # "AI.AIGRID_GIS",
  # "RACEBASE.HAUL", 
  # "RACE_DATA.V_CRUISES"
  "RACEBASE_FOSS.JOIN_FOSS_CPUE_HAUL"
)

oracle_dl(
  locations = locations, 
  channel = channel, 
  dir_out = paste0(dir_wd, "/data/oracle/"))
