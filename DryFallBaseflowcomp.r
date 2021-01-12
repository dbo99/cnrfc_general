

## shortcuts taken - don't run piecemeal - run whole script

{
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(plyr)
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

}

{
df <- rbind(frac1_qme_dates, pftc1_qme_dates)
rm(frac1_qme, frac1_qme_dates, pftc1_qme, pftc1_qme_dates)

df <- df %>% mutate(wy = water_year(date), month = month(date), wm = 
                      water_month(date))
as_tibble(df)

sjwyt <- read_csv("sjwyt.csv")
as_tibble(tail(sjwyt))

df <- df %>% filter(wm <= 5) #, wm >1)    
as_tibble(df)
as_tibble(df)

}
df_sum <- df %>% 
  group_by(wy, wm, res) %>%
summarize(cfs_meanmonthly = mean(qme_cfs))   
as_tibble(df_sum)

df_sum <- inner_join(df_sum, sjwyt)

wm <- c(1,2,3,4,5)
wmname <- c("O", "N", "D", "J", "F")
wmlookup <- data.frame(wm, wmname)

df_sum <- inner_join(df_sum, wmlookup)
as_tibble(df_sum)


df_sum <- df_sum %>% mutate(wmname = factor(wmname, levels = c("O", "N", "D", "J", "F")))

sjwyt = c("D", "BN", "C", "AN", "W")
ytypelab = c("dry", "below norm", "critical", "above normal", "wet")
ytypedf <- data.frame(sjwyt, ytypelab)


as_tibble(df_sum)
as_tibble(ytypedf)
df_sum <- inner_join(df_sum, ytypedf)
df_sum <- df_sum %>% mutate(wylab = paste0(wy, " - ", ytypelab))
as_tibble(df_sum)

df_sum <- df_sum %>% mutate(fnf_kcfs_meanmonthly = round(cfs_meanmonthly/1000,1),
                                    wyres = paste0(wy, " - ", res))

##if want al, define df_dry above as df (ca nselect either above, yes dumb)

p1_all <- ggplot(df_sum, aes(x = wmname, y = cfs_meanmonthly, 
                                 group = wyres, 
                                # color = as.factor(wy), 
                                 color = res, label = round(cfs_meanmonthly,0) )) + 
geom_line() +
 facet_grid(~wylab, scales = "free_y") + #, labeller = label_wrap_gen(width=20)) + 
  theme(strip.text.x = element_text(angle = 90, hjust = 0)) +
 theme(axis.title.x=element_blank()) + 
  scale_y_continuous(breaks = c(0,250, 500, 1000,2000,3000,4000,5000,7500,10000,12500),
                     sec.axis = dup_axis(name = NULL))
      # axis.text.x=element_blank(),
      # axis.ticks.x=element_blank()) + scale_y_continuous(sec.axis = dup_axis(name = NULL)) 
# geom_text()#hjust = 1, angle = 90) 
p1_all

#ggsave

#################################
#### difference plot
#################################

df_sum_sj <- df_sum %>% filter(res == "frac1") %>% 
                 ungroup %>%
                 transmute(cfs_qme_frac1 = cfs_meanmonthly, wm1 = wm, wy1 = wy) 
df_sum_kg <- df_sum %>% filter(res == "pftc1") %>% mutate(cfs_qme_pftc1 = cfs_meanmonthly)

df_diff <- cbind(df_sum_sj, df_sum_kg) %>% transmute(cfs_meanmonthly_FRAC1minusPFTC1 = 
          round(cfs_qme_frac1 - cfs_qme_pftc1,0), wy, month = wmname)

p2 <- ggplot(df_diff, aes(x = month, y = cfs_meanmonthly_FRAC1minusPFTC1, fill = month, 
                          label = cfs_meanmonthly_FRAC1minusPFTC1 )) + 
  geom_bar(position = "dodge",stat = "identity") + geom_text() + #hjust = 1) + #, angle = 90) +
 # geom_tile()
  facet_grid(~wy) + #, scales = "free_y") #+  
  theme(strip.text.y = element_text(angle = 0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + scale_y_continuous(sec.axis = dup_axis(name = NULL))
#p2
p <- plot_grid(p1_all, p2, ncol = 1)
#p
ggsave("p1_all.png", dpi = 300, width = 21, height = 13, units = "in")

#######################################
##### dry #######
########################################
df_sum <- df_sum %>% filter(          wy == 2001 |
                                      wy == 2002 |  
                                      wy == 2003 |  
                                      wy == 2004 |  
                                      wy == 2007 |  
                                      wy == 2008 |  
                                      wy == 2009 |  
                                      wy == 2012 |  
                                      wy == 2013 |  
                                      wy == 2014 |  
                                      wy == 2015 |  
                                      wy == 2016 )

p1_dry <- ggplot(df_sum, aes(x = wmname, y = cfs_meanmonthly, 
                             group = wyres, 
                             # color = as.factor(wy), 
                             color = res, label = round(cfs_meanmonthly,0) )) + 
  geom_line() +
  facet_grid(~wylab, scales = "free_y") + #, labeller = label_wrap_gen(width=20)) + 
  theme(strip.text.x = element_text(angle = 90, hjust = 0)) +
  theme(axis.title.x=element_blank()) + 
  scale_y_continuous(breaks = c(0, 50, 100, 200,300,400,500,600,700,800,900,1000,1250, 1500),
                     sec.axis = dup_axis(name = NULL))
# axis.text.x=element_blank(),
# axis.ticks.x=element_blank()) + scale_y_continuous(sec.axis = dup_axis(name = NULL)) 
# geom_text()#hjust = 1, angle = 90) 
p1_dry

#ggsave

#################################
#### difference plot
#################################

df_sum_sj <- df_sum %>% filter(res == "frac1") %>% 
  ungroup %>%
  transmute(cfs_qme_frac1 = cfs_meanmonthly, wm1 = wm, wy1 = wy) 
df_sum_kg <- df_sum %>% filter(res == "pftc1") %>% mutate(cfs_qme_pftc1 = cfs_meanmonthly)

df_diff <- cbind(df_sum_sj, df_sum_kg) %>% transmute(cfs_meanmonthly_FRAC1minusPFTC1 = 
                                                       round(cfs_qme_frac1 - cfs_qme_pftc1,0), wy, month = wmname)

p2 <- ggplot(df_diff, aes(x = month, y = cfs_meanmonthly_FRAC1minusPFTC1, fill = month, 
                          label = cfs_meanmonthly_FRAC1minusPFTC1 )) + 
  geom_bar(position = "dodge",stat = "identity") + geom_text() + #hjust = 1) + #, angle = 90) +
  # geom_tile()
  facet_grid(~wy) + #, scales = "free_y") #+  
  theme(strip.text.y = element_text(angle = 0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + scale_y_continuous(sec.axis = dup_axis(name = NULL),
        breaks = c(-250, -100, -50, 0, 50, 100, 250, 500))
#p2
p <- plot_grid(p1_dry, p2, ncol = 1)
#p
ggsave("p1_dry.png", dpi = 300, width = 21, height = 13, units = "in")


