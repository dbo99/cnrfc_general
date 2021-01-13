


{
  rm(list = ls())
  rstudioapi::getActiveDocumentContext
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  #library(plyr)
  library(tidyverse)
  library(lubridate)
  library(cowplot)
  library(ggridges)
  library(viridis)
  source("fun_defs.r")
  
}




###################################
####
#### Folsom
####
###################################  


####################
## cdec reader
####################

{  
  cdecfile <- "fol.csv"
  cdec3id <- "fol"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                                     #date = lubridate::date(date),#,
                                     cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
  res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
  as_tibble(res_cdec)
  as_tibble(tail(res_cdec)) 
  
  
  
  ####################
  ## nwsrfc reader
  ####################
  
  
  nwsfile <- "folc1f_2019.qme.txt"
  nws5id <- "folc1"
  
  res_nws <- read_table(nwsfile)
  head(res_nws) 
  tail(res_nws) 
  
  #res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
 # as_tibble(res_nws)
  
  ## beware some nwsrfs columns have (Jan 27th) 0127 or just 127 - see differences below
  res_nws <- res_nws %>% mutate(yr = str_sub(moyr, -2,-1))
  as_tibble(res_nws)
  
  res_nws$monthno <- substr(res_nws$moyr,1,nchar(res_nws$moyr)-2)
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = as.integer(yr))
  res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
  as_tibble(res_nws)    
  
  res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
  as_tibble(res_nws)
  
  #write_csv(res_nws, "debug_folsom.csv")
  res_nws <- res_nws %>% mutate(date = mdy(date))
  as_tibble(res_nws)
  
  
  
  ## bind cdec and nws timeseries attributes ### 
  ## if duplicate values for any date, take max (flood forecasting persp)
  
  
  df <- rbind(res_cdec, res_nws) 
  as_tibble(df)
  df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
  as_tibble(df)
  
  minyr   <- year(min(df$date))
  lastdate <- max(df$date)
  
  write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))
}



###################################
####
#### Isabella
####
###################################  


####################
## cdec reader
####################

{  
  cdecfile <- "isa.csv"
  cdec3id <- "isa"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                                     #date = lubridate::date(date),#,
                                     cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
  res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
  as_tibble(res_cdec)
  as_tibble(tail(res_cdec)) 
  
  
  
  ####################
  ## nwsrfc reader
  ####################
  
  
  nwsfile <- "isac1_fnf_best.qme.txt"
  nws5id <- "isac1"
  
  res_nws <- read_table(nwsfile)
  head(res_nws) 
  tail(res_nws) 
  
  res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = as.integer(yr))
  res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
  as_tibble(res_nws)    
  
  res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
  as_tibble(res_nws)
  
  
  res_nws <- res_nws %>% mutate(date = mdy(date))
  as_tibble(res_nws)
  
  
  
  ## bind cdec and nws timeseries attributes ### 
  ## if duplicate values for any date, take max (flood forecasting persp)
  
  
  df <- rbind(res_cdec, res_nws) 
  as_tibble(df)
  df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
  as_tibble(df)
  
  minyr   <- year(min(df$date))
  lastdate <- max(df$date)
  
  write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))
}


###################################
####
#### Oroville
####
###################################  


####################
## cdec reader
####################

{  
  cdecfile <- "oro.csv"
  cdec3id <- "oro"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                                     #date = lubridate::date(date),#,
                                     cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
  res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
  as_tibble(res_cdec)
  as_tibble(tail(res_cdec)) 
  
  
  
  ####################
  ## nwsrfc reader
  ####################
  
  
  nwsfile <- "ordc1f_2019.qme.txt"
  nws5id <- "ordc1"
  
  res_nws <- read_table(nwsfile)
  head(res_nws) 
  tail(res_nws) 
  
  res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = as.integer(yr))
  res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
  as_tibble(res_nws)    
  
  res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
  as_tibble(res_nws)
  
  
  res_nws <- res_nws %>% mutate(date = mdy(date))
  as_tibble(res_nws)
  
  
  
  ## bind cdec and nws timeseries attributes ### 
  ## if duplicate values for any date, take max (flood forecasting persp)
  
  
  df <- rbind(res_cdec, res_nws) 
  as_tibble(df)
  df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
  as_tibble(df)
  
  minyr   <- year(min(df$date))
  lastdate <- max(df$date)
  
  write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))
}


###################################
####
#### Millerton
####
###################################  


####################
## cdec reader
####################

{  
  cdecfile <- "mil.csv"
  cdec3id <- "mil"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                                     #date = lubridate::date(date),#,
                                     cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
  res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
  as_tibble(res_cdec)
  as_tibble(tail(res_cdec)) 
  
  
  
  ####################
  ## nwsrfc reader
  ####################
  
  
  nwsfile <- "frac1_fnf_best.qme.txt"
  nws5id <- "frac1"
  
  res_nws <- read_table(nwsfile)
  head(res_nws) 
  tail(res_nws) 
  
  res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = as.integer(yr))
  res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
  as_tibble(res_nws)    
  
  res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(date = mdy(date))
  as_tibble(res_nws)
  
  
  
  ## bind cdec and nws timeseries attributes ### 
  ## if duplicate values for any date, take max (flood forecasting persp)
  
  
  df <- rbind(res_cdec, res_nws) 
  as_tibble(df)
  df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
  as_tibble(df)
  
  minyr   <- year(min(df$date))
  lastdate <- max(df$date)
  
  write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))
}


###################################
####
#### Pine Flat
####
###################################  


####################
## cdec reader
####################

{  
  cdecfile <- "pnf.csv"
  cdec3id <- "pnf"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                                     #date = lubridate::date(date),#,
                                     cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
  res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
  as_tibble(res_cdec)
  as_tibble(tail(res_cdec)) 
  
  
  
  ####################
  ## nwsrfc reader
  ####################
  
  
  nwsfile <- "pftc1.full.qme.ed.txt"
  nws5id <- "pftc1"
  
  res_nws <- read_table(nwsfile)
  head(res_nws) 
  tail(res_nws) 
  
  res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = as.integer(yr))
  res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
  as_tibble(res_nws)    
  
  res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(date = mdy(date))
  as_tibble(res_nws)
  
  
  
  ## bind cdec and nws timeseries attributes ### 
  ## if duplicate values for any date, take max (flood forecasting persp)
  
  
  df <- rbind(res_cdec, res_nws) 
  as_tibble(df)
  df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
  as_tibble(df)
  
  minyr   <- year(min(df$date))
  lastdate <- max(df$date)
  
  write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))
}



###################################
####
#### New Melones
####
###################################  


####################
## cdec reader
####################

{  
  cdecfile <- "nml.csv"
  cdec3id <- "nml"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                                     #date = lubridate::date(date),#,
                                     cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
  res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
  as_tibble(res_cdec)
  # as_tibble(tail(res_cdec)) 
  
  
  
  ####################
  ## nwsrfc reader
  ####################
  
  
  nwsfile <- "nmsc1_fnf_best.qme.txt"
  nws5id <- "nmsc1"
  
  res_nws <- read_table(nwsfile)
  head(res_nws) 
  tail(res_nws) 
  
  res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = as.integer(yr))
  res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
  as_tibble(res_nws)    
  
  res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(date = mdy(date))
  as_tibble(res_nws)
  
  
  
  ## bind cdec and nws timeseries attributes ### 
  ## if duplicate values for any date, take max (flood forecasting persp)
  
  
  df <- rbind(res_cdec, res_nws) 
  as_tibble(df)
  df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
  as_tibble(df)
  
  minyr   <- year(min(df$date))
  lastdate <- max(df$date)
  
  write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))
}




###################################
####
#### Exchequer
####
###################################  


####################
## cdec reader
####################

{  
  cdecfile <- "exc.csv"
  cdec3id <- "exc"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                                     #date = lubridate::date(date),#,
                                     cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
  res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
  as_tibble(res_cdec)
  # as_tibble(tail(res_cdec)) 
  
  
  
  ####################
  ## nwsrfc reader
  ####################
  
  
  nwsfile <- "exqc1_fnf_best.qme3.txt"
  nws5id <- "exqc1"
  
  res_nws <- read_table(nwsfile)
  head(res_nws) 
  tail(res_nws) 
  
  res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = as.integer(yr))
  res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
  as_tibble(res_nws)    
  
  res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(date = mdy(date))
  as_tibble(res_nws)
  
  
  
  ## bind cdec and nws timeseries attributes ### 
  ## if duplicate values for any date, take max (flood forecasting persp)
  
  
  df <- rbind(res_cdec, res_nws) 
  as_tibble(df)
  df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
  as_tibble(df)
  
  minyr   <- year(min(df$date))
  lastdate <- max(df$date)
  
  write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))
}

###################################
####
#### Trinity
####
###################################  


####################
## cdec reader
####################

{  
  cdecfile <- "cle.csv"
  cdec3id <- "cle"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                                     #date = lubridate::date(date),#,
                                     cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
  res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
  as_tibble(res_cdec)
  # as_tibble(tail(res_cdec)) 
  
  
  
  ####################
  ## nwsrfc reader
  ####################
  
  
  nwsfile <- "cegc1_in.qme2.txt"
  nws5id <- "cegc1"
  
  res_nws <- read_table(nwsfile)
  head(res_nws) 
  tail(res_nws) 
  
  res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = as.integer(yr))
  res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
  as_tibble(res_nws)    
  
  res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(date = mdy(date))
  as_tibble(res_nws)
  
  
  
  ## bind cdec and nws timeseries attributes ### 
  ## if duplicate values for any date, take max (flood forecasting persp)
  
  
  df <- rbind(res_cdec, res_nws) 
  as_tibble(df)
  df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
  as_tibble(df)
  
  minyr   <- year(min(df$date))
  lastdate <- max(df$date)
  
  write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))
}

  

  ###################################
  ####
  #### Shasta
  ####
  ###################################  
  
 
  ####################
  ## cdec reader
  ####################
  
{  
  cdecfile <- "sha.csv"
  cdec3id <- "sha"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                                     #date = lubridate::date(date),#,
                                     cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
  res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
  as_tibble(res_cdec)
  # as_tibble(tail(res_cdec)) 
  
  
  
  ####################
  ## nwsrfc reader
  ####################
  
  
  nwsfile <- "shdc1_fnf_best.qme.txt"
  nws5id <- "shdc1"
  
  res_nws <- read_table(nwsfile)
  head(res_nws) 
  tail(res_nws) 
  
  res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(yr = as.integer(yr))
  res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
  as_tibble(res_nws)    
  
  res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
  as_tibble(res_nws)
  
  res_nws <- res_nws %>% mutate(date = mdy(date))
  as_tibble(res_nws)



## bind cdec and nws timeseries attributes ### 
## if duplicate values for any date, take max (flood forecasting persp)


df <- rbind(res_cdec, res_nws) 
as_tibble(df)
df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
as_tibble(df)

minyr   <- year(min(df$date))
lastdate <- max(df$date)

write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))
}
  
###################################
####
#### New Don Pedro
####
###################################  
{
  
####################
## cdec reader
####################
  

cdecfile <- "ndp.csv"
cdec3id <- "ndp"
  
  res_cdec <- read_csv(cdecfile)
  head(res_cdec) 
  res_cdec <- res_cdec %>% transmute(date = mdy(`DATE / TIME (PST)`),
                           #date = lubridate::date(date),#,
                        cfs = `FNF CFS`)
  as_tibble(res_cdec)
  ## remove commas
 res_cdec$cfs <- as.numeric(gsub(",","",res_cdec$cfs))
 as_tibble(res_cdec)
# as_tibble(tail(res_cdec)) 

  
  
####################
## nwsrfc reader
####################
  
  
nwsfile <- "ndpc1_cdec.qme.ed.txt"
nws5id <- "ndpc1"
  
res_nws <- read_table(nwsfile)
head(res_nws) 

res_nws <- res_nws %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
as_tibble(res_nws)

res_nws <- res_nws %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
as_tibble(res_nws)

res_nws <- res_nws %>% mutate(yr = as.integer(yr))
res_nws <- res_nws %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
as_tibble(res_nws)    

res_nws <- res_nws %>% transmute(date = paste0(monthno, "/", dy, "/", yr), cfs)
as_tibble(res_nws)


res_nws <- res_nws %>% mutate(date = mdy(date))
as_tibble(res_nws)



## bind cdec and nws timeseries attributes ### if duplicate values for any date, take max (flood forecasting persp)


df <- rbind(res_cdec, res_nws) 
as_tibble(df)
df <- df %>% group_by(date) %>% slice(which.max(cfs)) %>% mutate(res = nws5id)
as_tibble(df)

minyr   <- year(min(df$date))
lastdate <- max(df$date)

write_csv(df, paste0(nws5id,"_",minyr, "_", lastdate, ".csv"))

}
