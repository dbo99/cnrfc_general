


#{

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(lubridate)
library(cowplot)
library(ggridges)
source("fun_defs.r")


frac1_qme <- read_table("frac1_fnf_best.qme_87-90lower.txt")  
as_tibble(frac1_qme)
as_tibble(tail(frac1_qme))

frac1_qme <- frac1_qme %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
as_tibble(frac1_qme)

frac1_qme <- frac1_qme %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
as_tibble(frac1_qme)

frac1_qme <- frac1_qme %>% mutate(yr = as.integer(yr))
frac1_qme <- frac1_qme %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
as_tibble(frac1_qme)    

frac1_qme <- frac1_qme %>% mutate(date = paste0(monthno, "/", dy, "/", yr))
as_tibble(frac1_qme)
                                  
                                  
frac1_qme <- frac1_qme %>% mutate(date = mdy(date))
frac1_qme_dates <- frac1_qme %>% transmute(date, qme_cfs, res = "frac1")
as_tibble(frac1_qme_dates)

#}

#dupdates <- frac1_qme_dates[duplicated(frac1_qme_dates$date) | duplicated(frac1_qme_dates$date, fromLast = TRUE), ]
#version <- c(rep(c("a"), 1096), rep(c("b"), 1096))
#dupdates <- cbind(dupdates, version)

#ggplot(dupdates, aes(x = date, y = qme_cfs, color = version, linetype = version)) + geom_line()

#frac1_qme_dates <- distinct(frac1_qme_dates)

#ggplot(frac1_qme, aes(x = date, y = qme_cfs)) + geom_line()


#frac1_days <- seq(from=as.POSIXct("1960-10-01"), to=as.POSIXct("2019-09-30"), 
 #                 by="day", tz = "America/Los_Angeles",
 #                 format = "%d-%m-%Y %H:%M:%S")  

#################
## Kings 
#################


pftc1_qme <- read_table("pftc1.full.qme.ed.txt")  
as_tibble(pftc1_qme)
as_tibble(tail(pftc1_qme))

pftc1_qme <- pftc1_qme %>% mutate(monthno = stringr::str_extract(moyr, "^.{2}"))
as_tibble(pftc1_qme)

pftc1_qme <- pftc1_qme %>% mutate(yr = stringr::str_sub(moyr,-2,-1))
as_tibble(pftc1_qme)

pftc1_qme <- pftc1_qme %>% mutate(yr = as.integer(yr))
pftc1_qme <- pftc1_qme %>% mutate(yr = ifelse(yr >25, yr + 1900, yr + 2000))
as_tibble(pftc1_qme)    

pftc1_qme <- pftc1_qme %>% mutate(date = paste0(monthno, "/", dy, "/", yr))
as_tibble(pftc1_qme)


pftc1_qme <- pftc1_qme %>% mutate(date = mdy(date))
pftc1_qme_dates <- pftc1_qme %>% transmute(date, qme_cfs, res = "pftc1")
as_tibble(pftc1_qme_dates)

#pftc1_days <- seq(from=as.POSIXct("1955-10-01"), to=as.POSIXct("2019-09-30"), 
#                 by="day", tz = "America/Los_Angeles",
#                 format = "%d-%m-%Y %H:%M:%S")  


df <- rbind(frac1_qme_dates, pftc1_qme_dates)
rm(frac1_qme, frac1_qme_dates, pftc1_qme, pftc1_qme_dates)

df <- df %>% mutate(wy = water_year(date), month = month(date), wm = 
                      water_month(date))
as_tibble(df)

sjwyt <- read_csv("sjwyt.csv")
as_tibble(tail(sjwyt))



### pick dry second years (picked swyt 08, 09, 12,13,14,15)

df_dry <- df %>% filter(wy == 2001 |
                        wy == 2002 |                          
                        wy == 2003 |                         
                        wy == 2004 |                         
                        wy == 2007 | 
                        wy == 2008 |
                        wy == 2009 |
                        wy == 2013 |                      
                        wy == 2014 |   
                        wy == 2015 |   
                        wy == 2016 )  

df_dry <- df_dry %>% filter(wm <= 4) #, wm >1)    
as_tibble(df)

df_dry_sum <- df_dry %>% group_by(wy,wm, res) %>% summarize(FNF_cfs_meanmonthly = mean(qme_cfs))   
as_tibble(df_dry_sum)

df_dry_sum <- inner_join(df_dry_sum, sjwyt)

wm <- c(1,2,3,4)
wmname <- c("oct", "nov", "dec", "jan")
wmlookup <- data.frame(wm, wmname)

df_dry_sum <- inner_join(df_dry_sum, wmlookup)
as_tibble(df_dry_sum)


df_dry_sum <- df_dry_sum %>% mutate(wmname = factor(wmname, levels = c("oct", "nov", "dec", "jan")))

sjwyt = c("D", "BN", "C")
ytypelab = c("dry", "below norm", "critical")
ytypedf <- data.frame(sjwyt, ytypelab)


as_tibble(df_dry_sum)
as_tibble(ytypedf)
df_dry_sum <- inner_join(df_dry_sum, ytypedf)
df_dry_sum <- df_dry_sum %>% mutate(wylab = paste0(wy, " - ", ytypelab))
as_tibble(df_dry_sum)



p1 <- ggplot(df_dry_sum, aes(x = res, y = FNF_cfs_meanmonthly, fill = res)) + 
  geom_bar(
   # position = "dodge"
    stat = "identity",
    width = 1) + 
  facet_grid(wylab~wmname, scales = "free_y") +   theme(strip.text.y = element_text(angle = 0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + scale_y_continuous(sec.axis = dup_axis(name = NULL))
  #theme(aspect.ratio = 2/1)
p1

#ggsave

#################################
#### difference plot
#################################

df_dry_sum_sj <- df_dry_sum %>% filter(res == "frac1") %>% 
                 ungroup %>%
                 transmute(cfs_qme_frac1 = FNF_cfs_meanmonthly, wm1 = wm, wy1 = wy) 
df_dry_sum_kg <- df_dry_sum %>% filter(res == "pftc1") %>% mutate(cfs_qme_pftc1 = FNF_cfs_meanmonthly)

df_diff <- cbind(df_dry_sum_sj, df_dry_sum_kg) %>% transmute(FNF_cfs_meanmonthly_FRAC1minusPFTC1 = 
          round(cfs_qme_frac1 - cfs_qme_pftc1,0), wy, month = wmname)

p2 <- ggplot(df_diff, aes(x = month, y = FNF_cfs_meanmonthly_FRAC1minusPFTC1, fill = month, 
                          label = FNF_cfs_meanmonthly_FRAC1minusPFTC1 )) + 
  geom_bar(position = "dodge",stat = "identity") + geom_text(hjust = 1, angle = 90) +
 # geom_tile()
  facet_grid(~wy) + #, scales = "free_y") #+  
  theme(strip.text.y = element_text(angle = 0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p2
p <- plot_grid(p1, p2, ncol = 1)
p
ggsave("explore.png", dpi = 300, width = 21, height = 13, units = "in")
