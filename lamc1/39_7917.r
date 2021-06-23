


 
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
source("libs.r")
source("fun_defs.r")


  
df  <-  list.files(pattern = "*.csv", full.names = F) %>% 
    map_df(~read_plus(.)) %>% 
    #mutate(fcastday = ymd(substr(filename, start = 1, stop = 8))) %>%
    mutate(GMT = ymd_hms(GMT))

cols2to40 <-   seq(from = 1979, to = 2017, by = 1)

colnames(df) <- c("GMT", cols2to40, "filename")  
#head(df)

df <- df %>% pivot_longer(cols = 2:40, names_to = c("member"),
                        values_to = "kcfs")
head(df)


df_l <- df %>% transmute(fcastday12z = ymd(substr(filename, start = 1, stop = 8)), member = as.integer(member), fcasthr_gmt = GMT, kcfs = as.numeric(kcfs),
                       cfs = 1000*kcfs, af =  cfs*0.0826443, kaf = af/1000, membergroup = "39_79to17" )
head(df_l) 

write_csv(df_l, "39_79to17.csv")  
