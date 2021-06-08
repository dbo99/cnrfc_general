



{
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(lubridate)
library(viridis)
library(plotly)
library(cowplot)
library(gghighlight)
library(sf)
source("fun_defs.r")

loc <- read_csv("location.csv")
loc = loc[order(loc[,'res'],-loc[,'lat']),]
loc = loc[!duplicated(loc$res),]



wyts <- read_csv("WYTsthru2020.csv")
stor <- read_csv("historicalReservoirData_nominus9s.csv")
resnames <- subset(stor, select=-c(tstep))
resnames <- colnames(resnames)
stor <- stor %>% pivot_longer(!tstep, names_to = "res", values_to = "af") %>% 
        mutate(tstep = mdy(tstep), wy = water_year(tstep), wm = water_month(tstep),
               kaf = af/1000) 
}
stor <- right_join(stor, wyts)
stor <- stor[complete.cases(stor), ]
stor <- right_join(stor, loc)

stor <- stor %>% filter(res %in% resnames)
df <- stor
length(resnames)
df3 <- df %>% filter(res == "BMNC1")
#plot_both(df3)

for (cat in unique(df$res)) {
d <- df %>% filter(res == cat )
plot_both(d)
}

for (cat in unique(df$res)) {
  d <- df %>% filter(res == cat )
  p1ly(d)
}

for (cat in unique(df$res)) {
  d <- df %>% filter(res == cat )
  p2ly(d)
}

for (cat in unique(df$res)) {
  d <- df %>% filter(res == cat )
  pspag(d)
}

for (cat in unique(df$res)) {
  d <- df %>% filter(res == cat )
  plot_three(d) }
  
for (cat in unique(df$res)) {
    d <- df %>% filter(res == cat )
    pspag2ly(d)}

plot_three(df3)
pspag2ly(df3)
dfmn <- df %>% filter(wm >=5, wm <=8) %>% group_by(res, wm) %>% summarize(mean = mean(kaf) )
dfmn21 <- df %>% filter(wy==2021, wm >=5, wm <=8) %>% group_by(res, wm) %>% summarize(stor21 = mean(kaf))
df_stats <- inner_join(dfmn, dfmn21) %>% mutate(percnorm = stor21/mean*100) %>%
  mutate(diff21 = stor21 - mean, percdiff21 = diff21/stor21*100)

df_map <- right_join(df_stats, loc)
df_map <- df_map[complete.cases(df_map), ]
#df_map %>% filter(wm == 5) %>% write_csv("df_map_feb.csv")
#df_map %>% filter(wm == 6) %>% write_csv("df_map_mar.csv")
#df_map %>% filter(wm == 7) %>% write_csv("df_map_apr.csv")
#df_map %>% filter(wm == 8) %>% write_csv("df_map_may.csv")
#

df_map <- df_map %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% filter(mean < 400)

ggplot(df_map) +
  geom_sf(aes(color = percdiff21)) +
  #geom_sf_label(aes(label = res)) +
  facet_wrap(~wm)
