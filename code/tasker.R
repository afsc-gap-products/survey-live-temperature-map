#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: run script every 6 hours
#' ---------------------------

# Task Scheduler ---------------------------------------------------------------

# https://cran.r-project.org/web/packages/taskscheduleR/vignettes/taskscheduleR.html


library(taskscheduleR)


taskscheduler_create(taskname = "test_run",
                     rscript = "./code/run.R",
                     schedule = "MINUTE",
                     starttime = format(Sys.time(), "%H:%M"))

# taskscheduler_create(taskname = "test_run", 
#                      rscript = "./run.R", 
#                      schedule = "HOURLY", 
#                      starttime = format(Sys.time() + 50, "%H:%M"),
#                      modifier = 2)

taskscheduler_ls()
taskscheduler_stop("test_run")