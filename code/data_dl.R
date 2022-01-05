#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: download oracle data
#' ---------------------------

# *** Oracle -------------------------------------------------------------------

# This has a specific username and password because I DONT want people to have access to this!
# source("C:/Users/emily.markowitz/Work/Projects/ConnectToOracle.R")
source("C:/Users/emily.markowitz/Documents/Projects/ConnectToOracle.R")

locations<-c(
  "RACEBASE.HAUL", 
  "RACE_DATA.VESSELS", 
  "AI.AIGRID_GIS", 
  "GOA.GOAGRID_GIS", 
  "RACE_DATA.V_CRUISES"
)


#sinks the data into connection as text file
sink("./data/metadata.txt")

print(Sys.Date())

for (i in 1:length(locations)){
  print(locations[i])
  a<-RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
  write.csv(x=a, 
            paste0("./data/oracle/",
                   tolower(strsplit(x = locations[i], 
                                    split = ".", 
                                    fixed = TRUE)[[1]][2]),
                   ".csv"))
  remove(a)
}

sink()


