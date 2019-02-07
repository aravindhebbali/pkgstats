install.packages('taskscheduleR')
library(taskscheduleR)
library(here)
myscript <- "J:/R/Others/pkgstats/country_wise.R"
taskscheduler_create(taskname = "cran_downloads_new", rscript = myscript, 
                     schedule = "DAILY", starttime = "22:27")


