rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#library(plyr)
library(tidyverse)
library(lubridate)
library(plotly)
library(cowplot)

df_csv <- read_csv("df.csv") #%>% transmute(station, dwr3id, pname, date = cdecday)
#head(df_Csv)

df20 <- df_csv %>% filter(wateryear == 2020)
df21 <- df_csv %>% filter(wateryear == 2021)
unique(df20$basin)
unique(df21$basin)

active20 <- unique(df20$pillow) 
active21 <- unique(df21$pillow)

setdiff(active20, active21) #find pillows that don't have match in other year

## pineplat KUP and KUB are out
{
df <- df_csv %>% filter(date > "2020-01-25", dwr3id != "KUP" , dwr3id != "KUB", 
                        dwr3id != "LLP" ,  dwr3id != "RBP",
                        dwr3id != "LOS" ,  dwr3id != "SCN",  dwr3id != "AGP") %>% 
     mutate(elev = elev_cdec) %>% filter(dowy > 116) %>%  #day or two before the big storm
                                 filter(basin != "Eel")   #no Eeel in 21

#df <- df %>% mutate(elevation = "tbd")
#df <- df %>% mutate(elevation = ifelse(elev > 5000  & elev < 8000, paste0("5-8k'"), elevation))
#df <- df %>% mutate(elevation = ifelse(elev >= 8000  & elev < 10000, paste0("8-10k'"), elevation))  
#
#df <- df %>% mutate(elevation = ifelse(elev >= 10000, paste0("10k'+"), elevation))    
  
df <- df %>% mutate(elevation = "tbd")
#df <- df %>% mutate(elevation = ifelse(elev > 5000  & elev < 6000, paste0("5-6k'"), elevation))
df <- df %>% mutate(elevation = ifelse(elev >= 5000  & elev < 7000, paste0("5-7k'"), elevation))  
df <- df %>% mutate(elevation = ifelse(elev >= 7000  & elev < 8000, paste0("7-8k'"), elevation))  
df <- df %>% mutate(elevation = ifelse(elev >= 8000  & elev < 9000, paste0("8-9k'"), elevation))  
df <- df %>% mutate(elevation = ifelse(elev >= 9000  & elev < 10000, paste0("9-10k'"), elevation))  
df <- df %>% mutate(elevation = ifelse(elev >= 10000  & elev < 11000, paste0("10-11k'"), elevation))  
df <- df %>% mutate(elevation = ifelse(elev >= 11000  & elev < 12000, paste0("11-12k'"), elevation))  
    
                                                       
basin <- unique(df$basin_f)   
Basin <- c("Central", "East Side", "Northern & Trinity", "Southern", "Southern", "Southern", "Central",
            "Central", "East Side", "Northern & Trinity", "Southern", "Central", "Northern & Trinity", "East Side", 
            "Northern & Trinity", "East Side",
            "Central","East Side", "Central")
basins <- data.frame(basin, Basin)

df <- inner_join(df, basins)                        
                            

#df$elevation <- factor(df$elevation, levels = c("5-8k'", "8-10k'", "10k'+"))

df$elevation <- factor(df$elevation, levels = c("5-7k'", "7-8k'","8-9k'","9-10k'","10-11k'", "11-12k'"))
  
df$Basin <- factor(df$Basin, levels = c("Northern & Trinity", "Central", "Southern", "East Side"))
  
df_inch <- df %>% filter(pname == "inch")
df_perc <- df %>% filter(pname == "perc_apr1mean")




df_inch_stats1 <- df_inch %>% 
  
  filter(dowy == 176) %>%  
  group_by(dowy, wateryear, Basin, elevation) %>% summarize(mean = mean(value))



df_inch_stats2 <- df_inch %>% 
  
                 filter(dowy == 176) %>%  mutate(Mar24mean = paste0("3/25/", wateryear, ", Sierra pillow mean")) %>%
                 group_by(dowy, Mar24mean, Basin, elevation) %>% summarize(mean = mean(value))



p1 <- ggplot() + geom_line(#df %>% filter(pname == "perc_apr1mean"), 
            #df %>%filter(pname == "inch"), 
            data = df_inch,
            aes(dowy, value, color = elevation, group = dwr3id)) + geom_line() + 
     #facet_grid(wateryear~Basin) +

  scale_x_continuous(breaks = c(117, #1/25
                                                                175, # , #3/24 +
                                                                245), # 6/1
                                                               # 188  ), #4/4 +
                                                      labels = c("1/25", "3/25", "6/1")) + # "4/4")) + 
    geom_vline(xintercept = 117, linetype = "dashed") + 
    geom_vline(xintercept = 176, linetype = "dashed") + labs(x = "", y = "Pillow SWE (in)") + 
    geom_vline(xintercept = 245, linetype = "dashed") + 
    geom_point(data = df_inch_stats1, aes(dowy, mean, 
                                         #color = elevation, 
                                         fill = elevation), shape = 21, size = 2) +
      theme(strip.text.y.right = element_text(angle = 0))+
  facet_grid(Basin~wateryear) 
p1
}
#ggplotly(p1)


p2 <- ggplot(df_inch_stats2,aes(x = dowy, y = mean, fill = elevation, label = round(mean, 1))) + 
  geom_bar(position = "dodge",stat = "identity") + 
 # theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + 
  #theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
  ylab("Pillow SWE mean (in)") + 
  xlab("3/25 SWE") +
  geom_text(color = "dark blue", angle = 0, hjust = 0.5,vjust = 1, position = position_dodge(0.9)) + 
 theme(#axis.title.x=element_blank(),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank()) +
  facet_grid(Basin~Mar24mean) +
  theme(strip.text.y.right = element_text(angle = 0))

p2

title <- ggdraw() + 
  draw_label(
    "WY2020 & WY2021 Pillow Data (swe inches) from 1/25 on, w/ March 25th means",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p3 <- plot_grid(p1,p2, ncol = 2, rel_widths = c(2,1.2)) 
p4 <- plot_grid(title, p3, ncol = 1, rel_heights = c(0.05, 1))
p4

ggsave("Mar24pillows.pdf",  dpi = 300, width = 17, height = 11, units = "in") 



#df_forspatialjoin <- df_csv %>%
#           filter(wateryear == 2021, dowy == 176, pname == "perc_apr1mean") %>% transmute(value, dwr3id)
#
#write_csv(df_forspatialjoin, "March25sweinches.csv")


#######################
#############
####### For subbasins
##################
##########################
df_inch$basin <- factor(df_inch$basin, levels = 
                                 c(  'Trinity',
                                     'Sacramento',
                                     'Surprise',
                                     'Feather',
                                     'Yuba',
                                     'American',
                                     'Truckee',
                                     'Tahoe',
                                     'Carson',
                                     'Mokelumne',
                                     'Stanislaus',
                                     'Tuolumne',
                                     'Walker',
                                     'Merced',
                                     'San Joaquin',
                                     'Owens',
                                     'Kings',
                                     'Kaweah',
                                     'Kern'
                                 ))

p1 <- ggplot() + geom_line(#df %>% filter(pname == "perc_apr1mean"), 
  #df %>%filter(pname == "inch"), 
  data = df_inch,
  aes(dowy, value, color = elevation, group = dwr3id)) + geom_line() + 
  #facet_grid(wateryear~Basin) +
  
  scale_x_continuous(breaks = c(117, #1/25
                                175, # , #3/24 +
                                245), # 6/1
                     # 188  ), #4/4 +
                     labels = c("1/25", "3/25", "6/1")) + # "4/4")) + 
  geom_vline(xintercept = 117, linetype = "dashed") + 
  geom_vline(xintercept = 176, linetype = "dashed") + labs(x = "", y = "Pillow SWE (in)") + 
  geom_vline(xintercept = 245, linetype = "dashed") + 
 # geom_point(data = df_inch_stats1, aes(dowy, mean, 
                                        #color = elevation, 
           #                             fill = elevation), shape = 21, size = 2) +
  theme(strip.text.y.right = element_text(angle = 0))+
  facet_grid(basin~wateryear,scales="free") 
p1

#ggplotly(p1)
df_inch_stats3 <- df_inch %>% 
  
  filter(dowy == 176) %>%  mutate(Mar24mean = paste0("3/25/", wateryear, ", Sierra pillow mean")) %>%
  group_by(dowy, Mar24mean, basin, elevation) %>% summarize(mean = mean(value))

df_inch_stats3$basin <- factor(df_inch_stats3$basin, levels = 
                               c(  'Trinity',
                               'Sacramento',
                               'Surprise',
                               'Feather',
                               'Yuba',
                               'American',
                               'Truckee',
                               'Tahoe',
                               'Carson',
                               'Mokelumne',
                               'Stanislaus',
                               'Tuolumne',
                               'Walker',
                               'Merced',
                               'San Joaquin',
                               'Owens',
                               'Kings',
                               'Kaweah',
                               'Kern'
                            ))



#df_inch_stats3_20 <- df_inch_stats3 %>% filter(wateryear == 2020)
#df_inch_stats3_21 <- df_inch_stats3 %>% filter(wateryear == 2021) 

p2 <- ggplot(df_inch_stats3,aes(x = dowy, y = mean, fill = elevation, label = round(mean, 1))) + 
  geom_bar(position = "dodge",stat = "identity") + 
  # theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + 
  #theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
  ylab("Pillow SWE mean (in)") + 
  xlab("3/25 SWE") +
  geom_text(color = "dark blue", angle = 0, hjust = 0,vjust = 1, 
            position = position_dodge(0.9), size = 2.25) + 
  theme(#axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()) +
  facet_grid(basin~Mar24mean, scales="free") +
  theme(strip.text.y.right = element_text(angle = 0))

p2
ggsave("Mar24pillows_3.pdf",  dpi = 300, width = 17, height = 11, units = "in") 
title <- ggdraw() + 
  draw_label(
    "WY2020 & WY2021 Pillow Data (swe inches) from 1/25 on, w/ March 25th means",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
p3 <- plot_grid(p1,p2, ncol = 2, rel_widths = c(2,1.2)) 
p4 <- plot_grid(title, p3, ncol = 1, rel_heights = c(0.05, 1))
p4

ggsave("Mar24pillows_2.pdf",  dpi = 300, width = 17, height = 11, units = "in") 



#py <- ggplot(#df %>% filter(pname == "perc_apr1mean"), 
#  #df %>%filter(pname == "inch"), 
#  df_inch,
#  aes(dowy, value, color = elevation, group = dwr3id)) + geom_line() + 
#  facet_grid(wateryear~Basin) +
#
#  scale_x_continuous(breaks = c(117, #1/25
#                                                              175, # , #3/24 +
#                                                              245), # 6/1
#                                                   # 188  ), #4/4 +
#                                                   labels = c("1/25", "3/24", "6/1")) + # "4/4")) + 
#  geom_vline(xintercept = 117, linetype = "dashed") + 
#  geom_vline(xintercept = 176, linetype = "dashed") + labs(x = "", y = "inch") + 
#  geom_vline(xintercept = 245, linetype = "dashed") #+ 
#
#py
#
#
#ggplotly(py)
