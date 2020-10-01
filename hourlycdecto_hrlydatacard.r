rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(lubridate)
library(gdata)
#library(prettyR)

filename <- "DLMC1_qme_hrly.csv"
## assumes:
## 1 no commas in cfs values - preprocess first or add comma removal line below
## in excel filter blanks and replace as `-901.00`
## - check output for NAs - data card can't have any
com5id <- "DLMC1"


nwsid <- read_csv(filename)
head(nwsid) 
nwsid <- nwsid %>% mutate(date_time = ymd_hms(OBSDATE))
tail(nwsid)

nwsid <- nwsid %>% transmute(nwsid = com5id, month = month(date_time),
                                  year = year(date_time), day = day(date_time), cfs = VALUE)
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
#nwsid <- nwsid %>% mutate(cfs = ifelse(cfs == "-901.00", "-999", cfs))
##nwsid <- nwsid %>% mutate(cfs = ifelse(is.na(cfs), "-999", cfs))
#head(nwsid)
#tail(nwsid)



nwsid <- nwsid %>% mutate(cfs = as.numeric(cfs))
head(nwsid)
tail(nwsid)



nwsid$day <- sprintf(paste0("%0", max(nchar(nwsid$day)), "s"), nwsid$day)
head(nwsid)
tail(nwsid)

nwsid$cfs <- sprintf("%0.02f", nwsid$cfs)
head(nwsid)
tail(nwsid)

######################## good?


## example formatting
col1 <- rep("a", 4)
col2 <- c(0.60, 1234.55, 678.90, -999.00)
df <- data.frame(col1,col2)
head(df)

out <- paste(capture.output(print(df, row.names=F)), collapse = "\n")
writeLines(out, "df.dat")

df$col2 <- sprintf("%0.02f", df$col2)
df$col2 <- sprintf(paste0("%0", max(nchar(df$col2)), "s"), df$col2)

df
## example formatting (end)


#nwsid$cfs <- sprintf(paste0("%0", max(nchar(nwsid$cfs)), "s"), nwsid$cfs)
#head(nwsid)
#tail(nwsid)
#
#nwsid <- nwsid %>% mutate(nwsid = paste0(nwsid, "      "),
#                                  day = paste0(" ", day, " ")) # data card seems to want 6 spaces between day and cfs
#

result <- apply(nwsid,1,function(x){
  paste0(x["nwsid"],
         sprintf("%7s",""),
         sprintf("%04s",x["monyear"]),
         sprintf("%4s",x["day"]),
         sprintf("%10.2f",as.numeric(x["cfs"]))
  )})
head(result)
write.table(result,
            file = paste0(com5id, "hrl_qme.dat"),
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE)



head(nwsid)
tail(nwsid)

lapply(nwsid$day, sub, pattern='^0+([1-9])', replacement='\\1')
head(nwsid)
tail(nwsid)


write.table(nwsid, paste0(com5id,"_hrly.dat"), quote = FALSE, row.names = FALSE)
#}
