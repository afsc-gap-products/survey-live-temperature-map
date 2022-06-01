#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: download oracle data
#' ---------------------------

# *** Oracle -------------------------------------------------------------------

# This has a specific username and password because I DONT want people to have access to this!
# source("C:/Users/emily.markowitz/Work/Projects/ConnectToOracle.R")
#source("C:/Users/emily.markowitz/Documents/Projects/ConnectToOracle.R")
source("C:/Users/liz.dawson/Work/Projects/ConnectToOracle.R")
# source("Z:/Projects/ConnectToOracle.R")

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
  "RACE_DATA.VESSELS", 
  "AI.AIGRID_GIS", 
  "GOA.GOAGRID_GIS", 
  "GOA.GOA_STRATA", 
  "RACEBASE.HAUL", 
  "RACE_DATA.V_CRUISES"
)


#sinks the data into connection as text file
sink("./data/oracle/metadata.txt")

print(Sys.Date())

for (i in 1:length(locations)){
  print(locations[i])
  if (locations[i] == "RACEBASE.HAUL") { # that way I can also extract TIME
    
    a<-RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
    
    a<-RODBC::sqlQuery(channel, paste0("SELECT ",
                                       paste0(names(a)[names(a) != "START_TIME"], sep = ",", collapse = " "),
                                       " TO_CHAR(START_TIME,'MM/DD/YYYY HH24:MI:SS') START_TIME  FROM ", 
                                       locations[i]))
  } else {
    a<-RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
  }
  write.csv(x=a, 
            paste0("./data/oracle/",
                   tolower(strsplit(x = locations[i], 
                                    split = ".", 
                                    fixed = TRUE)[[1]][2]),
                   ".csv"))
  remove(a)
}

sink()


