


{
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(lubridate)
library(cowplot)
library(ggridges)
source("fun_defs.r")

chvc1 <- read_table("chvc1.sqin.1hr.txt")  %>% transmute(cms_chvc1 = cms)
lnrc1 <- read_table("lnrc1.sqin.1hr.txt")  %>% transmute(cms_lnrc1 = cms)
hetc1 <- read_table("hetc1.sqin.1hr.txt")  %>% transmute(cms_hetc1 = cms)
ndpc1 <- read_table("ndpc1f.sqin.1hr.txt")  %>% transmute(cms_ndpc1 = cms)

tib <- hetc1[23377,]
as_tibble(tib) #should be 143.8 not 43.8


hours <- seq(from=as.POSIXct("1979-10-01 01:00"), to=as.POSIXct("2019-10-01 00:00"), 
             by="hour", tz = "America/Los_Angeles",
             format = "%d-%m-%Y %H:%M:%S")  

df <- cbind(hours, chvc1, hetc1, lnrc1, ndpc1)
as_tibble(df)
#df<- df %>% mutate(hr = ymd_hms(hours, format = "%d-%m-%Y %H:%M:%S")  )
as_tibble(df)
as_tibble(df)
tib <- df[23377,]
as_tibble(tib)
df <- df %>% transmute(tstep = hours,  cfs_chvc1 = 35.3147*cms_chvc1,
                       cfs_lnrc1 = 35.3147*cms_lnrc1,
                       cfs_hetc1 = 35.3147*cms_hetc1,
                       cfs_ndpc1 = 35.3147*cms_ndpc1)

as_tibble(df)
maxhet <- max(df$cfs_hetc1)
maxlnr <- max(df$cfs_lnrc1)
maxchv <- max(df$cfs_chvc1)
maxndp <- max(df$cfs_ndpc1)

df <- df %>% mutate(cfs_hetc1 = round(cfs_hetc1, 4),
                    cfs_lnrc1 = round(cfs_lnrc1, 4),
                    cfs_chvc1 = round(cfs_chvc1, 4),
                    cfs_ndpc1 = round(cfs_ndpc1, 4))
#}

write.csv(df, "hetc1_chvc1_lnrc1_ndpc1_1hrsqin_Oct1979_Sep2019_v2.csv")
}
#ggplot(df, aes(x = hours, y = cfs_hetc1)) + geom_line()
#ggplot(df, aes(x = hours, y = cfs_lnrc1)) + geom_line()
#ggplot(df, aes(x = hours, y = cfs_chvc1)) + geom_line()

#################################################################################
####------ ridges
################################################################################

chvc1 <- read_table("chvc1.sqin.1hr.txt")  %>% transmute(cms, reservoir = "chvc1")
lnrc1 <- read_table("lnrc1.sqin.1hr.txt")  %>% transmute(cms, reservoir = "lnrc1")
hetc1 <- read_table("hetc1.sqin.1hr.txt")  %>% transmute(cms, reservoir = "hetc1")
ndpc1 <- read_table("ndpc1f.sqin.1hr.txt")  %>% transmute(cms, reservoir = "ndpc1")
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

####---------------
hours <- seq(from=as.POSIXct("1979-10-01 01:00"), to=as.POSIXct("2019-10-01 00:00"), 
             by="hour", tz = "America/Los_Angeles",
             format = "%d-%m-%Y %H:%M:%S")

ndpc1 <- cbind(hours, ndpc1) %>% mutate(year = year(hours), month = month(hours), 
                                        day = day(hours), yday = yday(hours),
                                        wy = water_year(hours))
as_tibble(chvc1)

ndpc1dly <- ndpc1 %>% group_by(wy, yday) %>% summarize(cfs = 35.3147*mean(cms), reservoir = "ndpc1")
as_tibble(chvc1dly)

dowy <- read_csv("daily_dowy.csv") %>% mutate(wy = water_year(date))
as_tibble(dowy)

ndpc1dly <- right_join(ndpc1dly, dowy, join_by = c("wy", "yday"))

#################
#df_hrly <- rbind(chvc1, hetc1, lnrc1)
df_dly <- rbind(chvc1dly, lnrc1dly, hetc1dly, ndpc1dly)
as_tibble(df_dly)

df_dly <- df_dly %>% mutate(maf = 1.98347*cfs/1000000)
as_tibble(df_dly)

df_annmaf <- df_dly %>% group_by(reservoir, wy) %>% summarize(annmaf = sum(maf))
as_tibble(df_annmaf)

df_dly <- inner_join(df_dly, df_annmaf)
as_tibble(df_dly)

#crime %>%
#  distinct(offense) %>%
#  mutate(offense = fct_relevel(offense, c("theft", "auto theft", "robbery", "burglary", "aggravated assault", "rape", "murder"))) %>%
#  arrange(offense)

df_dly$reservoir <- factor(df_dly$reservoir,      # Reordering group factor levels
                         levels = c("chvc1", "lnrc1", "hetc1", "ndpc1"))


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


scale <- 0.0004
alpha = 0.7
breaks = c( oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep)
breaks
labels = c( "oct", "nov", "dec", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep")
colors = c("black","black","black","black","black","black","red","black","black","black","red","black")
df_dly <- df_dly %>% filter(wy <=2019)
ybreaks <- seq(1980, 2019, by = 1)

##############################
## one scale by facets - works but cant get the subplot yaxis labels, which
##                       messes with the justfication comparing with other
##                       dup-yaxis labeled plots
##############################



#p_samescale_faceted <- ggplot(df_dly, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = annmaf))+
#  geom_ridgeline( stat = "identity", show.legend = T, 
#                 scale = 0.00023, 
#                  alpha = alpha) + #, 
#                 #min_height = -minh) + 
#  facet_wrap(~reservoir, nrow = 1 ) + #, scales = "free_y") +
#  scale_fill_viridis(name = "maf/wy") + theme_gray() + scale_x_continuous(breaks = breaks, labels = labels,
#                                        sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
#  labs(x = NULL) + labs(y="water year (wy)") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(1, "in")) +
#  scale_y_continuous(breaks = ybreaks, sec.axis = dup_axis(name = NULL)) +
#  theme(legend.position = "bottom") + 
#  geom_vline(xintercept= 184, color = "red", linetype = "dashed", size = 1.5 )+ 
#  geom_vline(xintercept= 306, color = "red", linetype = "dashed" , size = 1.5)
#p_samescale_faceted 

################3
#### one scale by viridis limits (puts y axis on each y of each subplot)
########################

{
min_dfdly <- min(df_dly$annmaf)
max_dfdly <- max(df_dly$annmaf)
viridislimits <- c(min_dfdly, max_dfdly)




df_chvc1 <- df_dly %>% filter(reservoir == "chvc1")
p_chvc1_onescale <- ggplot(df_chvc1, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = annmaf))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.00065, 
                  alpha = alpha) + #, 
  #min_height = -minh) + 
  facet_wrap(~reservoir, nrow = 1 ) + #, scales = "free_y") +
  scale_fill_viridis(limits = viridislimits, name = "maf/wy") + theme_gray() + 
  scale_x_continuous(breaks = breaks, labels = labels, #color = colors,
                   sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year (wy)") +  
  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(0.25, "in")) +
  scale_y_continuous(
    # sec.axis = dup_axis(name = NULL),
    breaks = ybreaks) + 
  theme(legend.position = "bottom") + 
  geom_vline(xintercept= 184, color = "red", linetype = "dashed", size = 1 )+ 
  geom_vline(xintercept= 306, color = "red", linetype = "dashed" , size = 1) +
  theme(strip.text.x = element_text(size = 30, colour = "black", angle = 0)) +
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text( size = 15))+
  theme(axis.text = element_text(size = 13)) 
#p_chvc1_onescale


df_lnrc1 <- df_dly %>% filter(reservoir == "lnrc1")
p_lnrc1_onescale <- ggplot(df_lnrc1, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = annmaf))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.0006, 
                  alpha = alpha) + #, 
  #min_height = -minh) + 
  facet_wrap(~reservoir, nrow = 1 ) + #, scales = "free_y") +
  scale_fill_viridis(limits = viridislimits, name = "maf/wy") + theme_gray() + scale_x_continuous(breaks = breaks, labels = labels,
                                                                                                  sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year (wy)") +
  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(0.25, "in")) +
  scale_y_continuous(
    # sec.axis = dup_axis(name = NULL),
    breaks = ybreaks) + 
  theme(legend.position = "bottom") + 
  geom_vline(xintercept= 184, color = "red", linetype = "dashed", size = 1 )+ 
  geom_vline(xintercept= 306, color = "red", linetype = "dashed" , size = 1) +
  theme(strip.text.x = element_text(size = 30, colour = "black", angle = 0))+
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text( size = 15))+
  theme(axis.text = element_text(size = 13)) 
#p_chvc1_onescale
#p_chvc1_onescale
#p_lnrc1_onescale

df_hetc1 <- df_dly %>% filter(reservoir == "hetc1")
p_hetc1_onescale <- ggplot(df_hetc1, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = annmaf))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.0003, 
                  alpha = alpha) + #, 
  #min_height = -minh) + 
  facet_wrap(~reservoir, nrow = 1 ) + #, scales = "free_y") +
  scale_fill_viridis(limits = viridislimits, name = "maf/wy") + theme_gray() + scale_x_continuous(breaks = breaks, labels = labels,
                                                                                                  sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year (wy)") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(0.25, "in")) +
  scale_y_continuous(
    # sec.axis = dup_axis(name = NULL),
    breaks = ybreaks) + 
  theme(legend.position = "bottom") + 
  geom_vline(xintercept= 184, color = "red", linetype = "dashed", size = 1 )+ 
  geom_vline(xintercept= 306, color = "red", linetype = "dashed" , size = 1) +
  theme(strip.text.x = element_text(size = 30, colour = "black", angle = 0))+
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text( size = 15))+
  theme(axis.text = element_text(size = 13)) 
#p_chvc1_onescale
#p_chvc1_onescale
#p_hetc1_onescale


df_ndpc1 <- df_dly %>% filter(reservoir == "ndpc1")
p_ndpc1_onescale <- ggplot(df_ndpc1, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = annmaf))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.000095, 
                  alpha = alpha) + #, 
  #min_height = -minh) + 
  facet_wrap(~reservoir, nrow = 1 ) + #, scales = "free_y") +
  scale_fill_viridis(limits = viridislimits, name = "maf/wy") + theme_gray() + scale_x_continuous(breaks = breaks, labels = labels,
                                                                                                  sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year (wy)") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(0.25, "in")) +
  scale_y_continuous(
    # sec.axis = dup_axis(name = NULL),
    breaks = ybreaks) + 
  theme(legend.position = "bottom") + 
  geom_vline(xintercept= 184, color = "red", linetype = "dashed", size = 1 )+ 
  geom_vline(xintercept= 306, color = "red", linetype = "dashed" , size = 1) +
  theme(strip.text.x = element_text(size = 30, colour = "black", angle = 0))+
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text( size = 15))+
  theme(axis.text = element_text(size = 13)) 
#p_chvc1_onescale
#p_chvc1_onescale
#p_ndpc1_onescale

p_samescale_viridlims <- plot_grid(p_hetc1_onescale,p_chvc1_onescale, 
                         p_lnrc1_onescale, p_ndpc1_onescale, nrow = 1)
p_samescale_viridlims
#ggsave("cp_samescale_viridlims.png", width = 21, height = 11, units = "in", dpi = 300)

##############################
## individually scaled
##############################

###---just chvc1

pchvc1 <- ggplot(df_dly %>% filter(reservoir == "chvc1"), aes(dowy, wy, height = cfs, group=as.factor(wy), fill = annmaf))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.00065,# 0.0006, 
                  alpha = alpha) + #, 
  #min_height = -minh) + 
  facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "maf/wy") + theme_gray() + 
  scale_x_continuous(breaks = breaks, labels = labels,
                      sec.axis = dup_axis(name = NULL), 
                     expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year (wy)") +  
  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(0.25, "in")) +
  scale_y_continuous(
    # sec.axis = dup_axis(name = NULL),
    breaks = ybreaks) + 
  geom_vline(xintercept= 184, color = "red", linetype = "dashed", size = 1 ) + 
  geom_vline(xintercept= 306, color = "red", linetype = "dashed" , size = 1) +
  theme(strip.text.x = element_text(size = 30, colour = "black", angle = 0))+
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text( size = 15))+
  theme(axis.text = element_text(size = 13)) 
#p_chvc1_onescale
#p_chvc1_onescale
#pchvc1
#scale_fill_distiller(palette = "Spectral", name = "taf/yr", direction = -1 ) 
#ggsave("chvc1_dlymeansqin_annmaf.png", width = 21, height = 11, units = "in", dpi = 300)



###---just lnrc1

plnrc1 <- ggplot(df_dly %>% filter(reservoir == "lnrc1"), aes(dowy, wy, height = cfs, group=as.factor(wy), fill = annmaf))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.0006, 
                  alpha = alpha) + #, 
  #min_height = -minh) + 
  facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "maf/wy") + 
  theme_gray() + scale_x_continuous(breaks = breaks, labels = labels,#,
  sec.axis = dup_axis(name = NULL), 
  expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year (wy)") +  
  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(0.25, "in")) +
  scale_y_continuous(
    # sec.axis = dup_axis(name = NULL),
    breaks = ybreaks) + 
  geom_vline(xintercept= 184, color = "red", linetype = "dashed", size = 1 ) + 
  geom_vline(xintercept= 306, color = "red", linetype = "dashed" , size = 1) +
  theme(strip.text.x = element_text(size = 30, colour = "black", angle = 0))+
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text( size = 15)) +
  theme(legend.title = element_text( size = 15))+
  theme(axis.text = element_text(size = 13)) 
#p_chvc1_onescale
#p_chvc1_onescale
#scale_fill_distiller(palette = "Spectral", name = "taf/yr", direction = -1 ) 
#plnrc1
#ggsave("lnrc1_dlymeansqin_annmaf.png", width = 21, height = 11, units = "in", dpi = 300)



###---just hetc1

phetc1 <- ggplot(df_dly %>% filter(reservoir == "hetc1"), aes(dowy, wy, height = cfs, group=as.factor(wy), fill = annmaf))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.0003, 
                  alpha = alpha) + #, 
  #min_height = -minh) + 
  facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "maf/wy") + theme_gray() + 
  scale_x_continuous(breaks = breaks, labels = labels,
                             sec.axis = dup_axis(name = NULL), 
                             expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year (wy)") +  
  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(0.25, "in")) +
  scale_y_continuous(
    # sec.axis = dup_axis(name = NULL),
    breaks = ybreaks) + 
  geom_vline(xintercept= 184, color = "red", linetype = "dashed", size = 1 )+ 
  geom_vline(xintercept= 306, color = "red", linetype = "dashed" , size = 1) +
  theme(strip.text.x = element_text(size = 30, colour = "black", angle = 0))+
  theme(legend.text=element_text(size=15)) +
  theme(legend.title = element_text( size = 15))+
  theme(axis.text = element_text(size = 13)) #+ theme_dark()
#p_chvc1_onescale
#p_chvc1_onescale
#scale_fill_distiller(palette = "Spectral", name = "taf/yr", direction = -1 ) 
#phetc1
#ggsave("hetc1_dlymeansqin_annmaf.png", width = 21, height = 11, units = "in", dpi = 300)



###---just ndpc1

#pndpc1 <- ggplot(df_dly %>% filter(reservoir == "ndpc1"), aes(dowy, wy, height = cfs, group=as.factor(wy), fill = annmaf))+
#  geom_ridgeline( stat = "identity", show.legend = T, 
#                  scale = 0.000095, 
#                  alpha = 0.7) + #, 
#  #min_height = -minh) + 
#  facet_wrap(~reservoir, nrow = 3 ) +
#  scale_fill_viridis(name = "maf/wy") + theme_gray() + 
#  scale_x_continuous(breaks = breaks, labels = labels,
#          sec.axis = dup_axis(name = NULL), 
#          expand = c(0,0)) +
#  labs(x = NULL) + labs(y="water year (wy)") +  
#  theme(legend.key.width = unit(0.55, "in"),
#        legend.key.height = unit(0.25, "in")) +
#  scale_y_continuous(
#                   # sec.axis = dup_axis(name = NULL),
#                    breaks = ybreaks) + 
#  geom_vline(xintercept= 184, color = "red", linetype = "dashed", size = 1 )+ 
#  geom_vline(xintercept= 306, color = "red", linetype = "dashed" , size = 1) +
#  theme(strip.text.x = element_text(size = 30, colour = "black", angle = 0))+
#  theme(legend.text=element_text(size=15)) +
#  theme(legend.title = element_text( size = 15)) +
#  theme(axis.text = element_text(size = 13)) 
#p_chvc1_onescale
#p_chvc1_onescale


pndpc1<- p_ndpc1_onescale
#scale_fill_distiller(palette = "Spectral", name = "taf/yr", direction = -1 ) 
#ggsave("ndpc1_dlymeansqin_annmaf.png", width = 21, height = 11, units = "in", dpi = 300)




## combine indivs / remove dup x-axis

pchvc1 <- pchvc1 + theme(legend.position = "bottom") 
plnrc1 <- plnrc1 + theme(legend.position = "bottom")
phetc1 <- phetc1 + theme(legend.position = "bottom")
pndpc1 <- pndpc1 + theme(legend.position = "bottom")


p_indivscaled <- plot_grid( phetc1, pchvc1, plnrc1, pndpc1, nrow = 1)
                         #  0#,
               #rel_heights = c(6,6,6,6))
#p_indivscaled
#ggsave("hetc1_chvc1_lnrc1_ndpc1_dlymeansqin_annmaf.png", width = 25, height = 17, units = "in", dpi = 300)

p <- plot_grid(p_samescale_viridlims, p_indivscaled, nrow = 2) 
p <- p_indivscaled
p
ggsave("p1_p2_v2.png", width = 26, height = 16, units = "in", dpi = 300)
}

