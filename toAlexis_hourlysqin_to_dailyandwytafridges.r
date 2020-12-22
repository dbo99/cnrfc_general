






#{
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(lubridate)
source("fun_defs.r")

chvc1 <- read_table("chvc1.sqin.1hr.txt")  %>% transmute(cms_chvc1 = cms)
lnrc1 <- read_table("lnrc1.sqin.1hr.txt")  %>% transmute(cms_lnrc1 = cms)
hetc1 <- read_table("hetc1.sqin.1hr.txt")  %>% transmute(cms_hetc1 = cms)
tib <- hetc1[23377,]
as_tibble(tib) #should be 143.8 not 43.8


#hetc2 <- read.table("hetc1.sqin.1hr.txt", sep = "" ,
#                    header = T )#,
#                   #na.strings ="",
                  # stringsAsFactors= F)
#Sys.timezone(location = TRUE)
hours <- seq(from=as.POSIXct("1979-10-01 01:00"), to=as.POSIXct("2019-10-01 00:00"), 
             by="hour", tz = "America/Los_Angeles",
             format = "%d-%m-%Y %H:%M:%S")  

#my_datetime <- as.POSIXlt("15-10-2017 16:41:00",format = "%d-%m-%Y %H:%M:%S")
df <- cbind(hours, chvc1, hetc1, lnrc1)
as_tibble(df)
#df<- df %>% mutate(hr = ymd_hms(hours, format = "%d-%m-%Y %H:%M:%S")  )
as_tibble(df)
as_tibble(df)
tib <- df[23377,]
as_tibble(tib)
df <- df %>% transmute(tstep = hours,  cfs_chvc1 = 35.3147*cms_chvc1,
                       cfs_lnrc1 = 35.3147*cms_lnrc1,
                       cfs_hetc1 = 35.3147*cms_hetc1 )

as_tibble(df)
maxhet <- max(df$cfs_hetc1)
maxlnr <- max(df$cfs_lnrc1)
maxchv <- max(df$cfs_chvc1)

df <- df %>% mutate(cfs_hetc1 = round(cfs_hetc1, 4),
                    cfs_lnrc1 = round(cfs_lnrc1, 4),
                    cfs_chvc1 = round(cfs_chvc1, 4))
#}

write.csv(df, "hetc1_chvc1_lnrc1_1hrsqin_Oct1979_Aug2019.csv")

#ggplot(df, aes(x = hours, y = cfs_hetc1)) + geom_line()
#ggplot(df, aes(x = hours, y = cfs_lnrc1)) + geom_line()
#ggplot(df, aes(x = hours, y = cfs_chvc1)) + geom_line()

#################################################################################
####------ ridges
################################################################################

chvc1 <- read_table("chvc1.sqin.1hr.txt")  %>% transmute(cms, reservoir = "chvc1")
lnrc1 <- read_table("lnrc1.sqin.1hr.txt")  %>% transmute(cms, reservoir = "lnrc1")
hetc1 <- read_table("hetc1.sqin.1hr.txt")  %>% transmute(cms, reservoir = "hetc1")
tib <- hetc1[23377,]
as_tibble(tib) #should be 143.8 not 43.8


#hetc2 <- read.table("hetc1.sqin.1hr.txt", sep = "" ,
#                    header = T )#,
#                   #na.strings ="",
# stringsAsFactors= F)

hours <- seq(from=as.POSIXct("1979-10-01 01:00"), to=as.POSIXct("2019-10-01 00:00"), 
             by="hour", tz = "America/Los_Angeles",
             format = "%d-%m-%Y %H:%M:%S")

chvc1 <- cbind(hours, chvc1) %>% mutate(year = year(hours), month = month(hours), 
                                        day = day(hours), yday = yday(hours),
                                        wy = water_year(hours))
as_tibble(chvc1)

chvc1dly <- chvc1 %>% group_by(wy, yday) %>% summarize(cfs = 35.3147*mean(cms), reservoir = "chvc1")
as_tibble(chvc1dly)

dowy <- read_csv("daily_dowy.csv") %>% mutate(wy = water_year(date))
as_tibble(dowy)

chvc1dly <- right_join(chvc1dly, dowy, join_by = c("wy", "yday"))


###----------------------------

#lnrc1 <- cbind(hours, lnrc1) %>% mutate(year = year(hours), month = month(hours), day = day(hours), yday = yday(hours))
#lnrc1dly <- lnrc1 %>% group_by(year, month, day) %>% summarize(cfs = 35.3147*mean(cms), reservoir = "lnrc1")
#as_tibble(lnrc1dly)

hours <- seq(from=as.POSIXct("1979-10-01 01:00"), to=as.POSIXct("2019-10-01 00:00"), 
             by="hour", tz = "America/Los_Angeles",
             format = "%d-%m-%Y %H:%M:%S")

lnrc1 <- cbind(hours, lnrc1) %>% mutate(year = year(hours), month = month(hours), 
                                        day = day(hours), yday = yday(hours),
                                        wy = water_year(hours))
as_tibble(lnrc1)

lnrc1dly <- lnrc1 %>% group_by(wy, yday) %>% summarize(cfs = 35.3147*mean(cms), reservoir = "lnrc1")
as_tibble(lnrc1dly)

dowy <- read_csv("daily_dowy.csv") %>% mutate(wy = water_year(date))
as_tibble(dowy)

lnrc1dly <- right_join(lnrc1dly, dowy, join_by = c("wy", "yday"))


####-----


#hetc1 <- cbind(hours, hetc1) %>% mutate(year = year(hours), month = month(hours), day = day(hours), yday = yday(hours))
#hetc1dly <- hetc1 %>% group_by(year, month, day) %>% summarize(cfs = 35.3147*mean(cms), reservoir = "hetc1")
#as_tibble(hetc1dly)

hours <- seq(from=as.POSIXct("1979-10-01 01:00"), to=as.POSIXct("2019-10-01 00:00"), 
             by="hour", tz = "America/Los_Angeles",
             format = "%d-%m-%Y %H:%M:%S")

hetc1 <- cbind(hours, hetc1) %>% mutate(year = year(hours), month = month(hours), 
                                        day = day(hours), yday = yday(hours),
                                        wy = water_year(hours))
as_tibble(hetc1)

hetc1dly <- hetc1 %>% group_by(wy, yday) %>% summarize(cfs = 35.3147*mean(cms), reservoir = "hetc1")
as_tibble(hetc1dly)

dowy <- read_csv("daily_dowy.csv") %>% mutate(wy = water_year(date))
as_tibble(dowy)

hetc1dly <- right_join(hetc1dly, dowy, join_by = c("wy", "yday"))
as_tibble(hetc1dly)



#################
#df_hrly <- rbind(chvc1, hetc1, lnrc1)
df_dly <- rbind(chvc1dly, lnrc1dly, hetc1dly)
as_tibble(df_dly)

df_dly <- df_dly %>% mutate(taf = 1.98347*cfs/1000)
as_tibble(df_dly)

df_anntaf <- df_dly %>% group_by(reservoir, wy) %>% summarize(anntaf = sum(taf))
as_tibble(df_anntaf)

df_dly <- inner_join(df_dly, df_anntaf)
as_tibble(df_dly)


oct <- 1
nov <- 32   
dec <- 62  
jan <- 93  
feb <- 124   
mar <- 153   
apr <- 184   
may <- 214   
jun <- 245    
jul <- 275   
aug <- 306   
sep <- 337 

breaks = c( oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep)
breaks
labels = c( "oct", "nov", "dec", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep")

ggplot(df_dly, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = anntaf))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                 scale = 0.0004, 
                  alpha = 0.8) + #, 
                 #min_height = -minh) + 
  facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "taf/wy") + theme_gray() + scale_x_continuous(breaks = breaks, labels = labels,
                                        sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year (wy)") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(1, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL))
#scale_fill_distiller(palette = "Spectral", name = "taf/yr", direction = -1 ) 
ggsave("hetc1_chvc1_lnrc1_dlymeansqin_anntaf.png", width = 21, height = 11, units = "in", dpi = 300)



