rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(lubridate)
library(gdata)
#library(prettyR)

filename <- "mwec1_usgsraw_2.csv"
## assumes:
## 1 no commas in cfs values - preprocess first or add comma removal line below
## in excel filter blanks and replace as `-901.00`
## - check output for NAs - data card can't have any
com5id <- "MWEC1"

#{
nwsid <- read_csv(filename)
head(nwsid) 
nwsid <- nwsid %>% mutate(date_time = mdy_hm(date_time))
tail(nwsid)

nwsid <- nwsid %>% transmute(nwsid = com5id, month = month(date_time),
                                  year = year(date_time), day = day(date_time), cfs)
head(nwsid)
tail(nwsid)

nwsid <- nwsid %>% mutate(month = ifelse(month <10, paste0(0, month), month))
tail(nwsid)

nwsid <- nwsid %>% mutate(year = str_sub(year, -2))
tail(nwsid)

nwsid <- nwsid %>% transmute(nwsid, monyear = paste0(month,year), day, cfs)
tail(nwsid)

nwsid <- nwsid %>% mutate(cfs = format(round(cfs, 2), nsmall = 2))
tail(nwsid)

nwsid <- nwsid %>% mutate(cfs = trimws(cfs), 
                                  monyear = trimws(monyear),
                                  nwsid = trimws(nwsid))
head(nwsid)
tail(nwsid)

 # assumes missings in excel are -901s - check
nwsid <- nwsid %>% mutate(cfs = ifelse(cfs == "-901.00", "-999", cfs))
#nwsid <- nwsid %>% mutate(cfs = ifelse(is.na(cfs), "-999", cfs))
head(nwsid)
tail(nwsid)



nwsid <- nwsid %>% mutate(cfs = as.numeric(cfs))
tail(nwsid)

nwsid$day <- sprintf(paste0("%0", max(nchar(nwsid$day)), "s"), nwsid$day)


nwsid$cfs <- sprintf("%0.02f", nwsid$cfs)
nwsid$cfs <- sprintf(paste0("%0", max(nchar(nwsid$cfs)), "s"), nwsid$cfs)
tail(nwsid)

nwsid <- nwsid %>% mutate(nwsid = paste0(nwsid, "      "),
                                  day = paste0(" ", day, " ")) # data card seems to want 6 spaces between day and cfs

head(nwsid)
tail(nwsid)



write.table(nwsid, paste0(com5id,"_hrly.dat"), quote = FALSE, row.names = FALSE)
#}