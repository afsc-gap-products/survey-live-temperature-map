#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: download oracle data
#' ---------------------------

# *** Oracle -------------------------------------------------------------------

PKG <- c("dplyr", "tidyverse", "knitr", "kableExtra", "reshape")
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

# This has a specific link because I DONT want people to have access to this!
source("C:/Users/emily.markowitz/Documents/Projects/ConnectToOracle.R")


# NBS Data
dat_nbs <- sqlQuery(channel, "
                    SELECT * 
                    
                    FROM RACEBASE.HAUL 
                    
                    WHERE
                    
                    REGION = 'BS' 
                    
                    AND STRATUM IN(70,71,81)
                    
                    AND CRUISE IN(201002, 201702, 201902);
                    ")

dat_nbs$year <- substr(x = dat_nbs$CRUISE, start = 1, stop = 4)
yr_first <- min(dat_nbs$year)
yr_last <- max(dat_nbs$year)
dat_nbs$REGION<-"NBS"
dat_nbs$BOTTOM_TYPE<-NULL
write_csv(x = dat_nbs, 
          file = here::here("data", paste0("dat_nbs_",yr_first,"-", yr_last, ".csv")))

# EBS Data
dat_ebs <- sqlQuery(channel, "
                        SELECT * 

                        FROM RACEBASE.HAUL 

                        WHERE

                        REGION = 'BS' 

                        AND STRATUM IN(10,20,31,32,41,42,43,50,61,62,82,90)

                        AND CRUISE >= 198701;
                        ")

dat_ebs$year <- substr(x = dat_ebs$CRUISE, start = 1, stop = 4)
yr_first <- min(dat_ebs$year)
yr_last <- max(dat_ebs$year)
dat_ebs$REGION<-"EBS"
dat_ebs$BOTTOM_TYPE<-NULL
write_csv(x = dat_ebs, 
          file = here::here("data", paste0("dat_ebs_",yr_first,"-", yr_last, ".csv")))
