
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
###################################
## read in clean csvs created above
###################################
###################################

{
cegc1 <- read_csv("cegc1_1963_2021-01-11.csv")
shdc1 <- read_csv("shdc1_1950_2021-01-11.csv")
ordc1 <- read_csv("ordc1_1960_2021-01-10.csv")
folc1 <- read_csv("folc1_1948_2021-01-10.csv")
nmsc1 <- read_csv("nmsc1_1992_2021-01-11.csv")
ndpc1 <- read_csv("ndpc1_1982_2021-01-10.csv")
exqc1 <- read_csv("exqc1_1960_2021-01-10.csv")
frac1 <- read_csv("frac1_1960_2021-01-11.csv")
pftc1 <- read_csv("pftc1_1955_2021-01-11.csv")
isac1 <- read_csv("isac1_1960_2021-01-11.csv")

df <- rbind(cegc1, shdc1, ordc1, folc1, nmsc1, ndpc1, exqc1, frac1, pftc1, isac1)
  
rm(cegc1, shdc1, ordc1, folc1, nmsc1, ndpc1, exqc1, frac1, pftc1, isac1) 
as_tibble(df)

}
  ## add attributes
  
df <- df %>% transmute(reservoir = res, date, cfs = round(as.numeric(cfs),1)  , 
                       month = month(date), wy = water_year(date),
                         wm = water_month(date), yday = yday(date))              # year = year(date_time), day = day(date_time))
as_tibble(df)
as_tibble(tail(df)) 

dowy <- read_csv("daily_dowy.csv") %>% mutate(wy = water_year(date))
as_tibble(dowy)

df <- left_join(df, dowy, join_by = c("wy", "yday"))
as_tibble(df)
as_tibble(tail(df)) 

df <- df %>% mutate(kaf = 1.98347*cfs/1000)
as_tibble(df)
as_tibble(tail(df)) 



## add water year total column
df_wysum <- df %>% group_by(wy, reservoir) %>% summarize(Jan16thruSep30 = sum(kaf))
as_tibble(df_wysum)
as_tibble(tail(df_wysum)) 
df <- left_join(df, df_wysum)
as_tibble(df)
as_tibble(tail(df)) 

## filter through day of submittal (1/15 = 107th day of water year) and add column for thru 1/5 volume
df_107 <- df %>% filter (dowy <= 107)
df_107_wysum <- df_107 %>% group_by(wy, reservoir) %>% summarize(Oct1thruJan15 = sum(kaf))
as_tibble(df_107_wysum)
as_tibble(tail(df_107_wysum)) 
df <- left_join(df, df_107_wysum)

df_107 <- df %>% filter (dowy <= 107)

df_sum <- inner_join(df_wysum, df_107_wysum)
as_tibble(df_sum)
as_tibble(tail(df_sum)) 
df_sum <- df_sum <- pivot_longer(df_sum, names_to = "period", values_to = "kaf", cols = 3:4) 
as_tibble(df_sum)
df_sum$period <- factor(df_sum$period, levels = c("Oct1thruJan15", "Jan16thruSep30")) 

## start with 1960 for consistency ##

df_sum <- df_sum %>% filter(wy >= 1960)
df_107 <- df_107 %>% filter(wy >= 1960)
#############################################
#####################
### reservoir plots
#####################
############################################
oct <- 1
nov <- 32   
dec <- 62  
jan <- 93 
jan15th <- 107

breaks = c( oct, nov, dec, jan, jan15th)
breaks
labels = c( "oct", "nov", "dec", "jan", "jan15th")

##########################
## SHASTA ##
##########################

shdc1 <- df_sum %>% filter(reservoir == "shdc1")
shdc1_thrujan15 <- shdc1 %>% filter (wy == 2021, period == "Oct1thruJan15")
shdc1_thrujan15 <- shdc1_thrujan15$kaf



p1 <- ggplot(shdc1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
              geom_bar(stat = "identity", position = "dodge") + 
              geom_hline(yintercept = shdc1_thrujan15, linetype = "dashed") +
              scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,12000, by = 1000)) +
  labs(x = NULL) + ggtitle("shasta")
#ggsave("shdc1_bars.png", width = 21, height = 11, units = "in", dpi = 300)



shdc1_r <- df_107 %>% filter(reservoir == "shdc1")
p2 <- ggplot(shdc1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.00008, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
 # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
              sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p3 <- plot_grid(p1,p2, ncol=1)
ggsave("Shasta.png", width = 8.5, height = 11, units = "in", dpi = 300)


##########################
## Folsom ##
##########################

folc1 <- df_sum %>% filter(reservoir == "folc1")
folc1_thrujan15 <- folc1 %>% filter (wy == 2021, period == "Oct1thruJan15")
folc1_thrujan15 <- folc1_thrujan15$kaf



p1 <- ggplot(folc1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = folc1_thrujan15, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,8000, by = 500)) +
  labs(x = NULL) + ggtitle("folsom")
#ggsave("folc1_bars.png", width = 21, height = 11, units = "in", dpi = 300)


p1
folc1_r <- df_107 %>% filter(reservoir == "folc1")

p2 <- ggplot(folc1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.00015, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
  # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
    sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p2
p3 <- plot_grid(p1,p2, ncol=1)
ggsave("Folsom.png", width = 8.5, height = 11, units = "in", dpi = 300)


##########################
## TRINITY ##
##########################

cegc1 <- df_sum %>% filter(reservoir == "cegc1")
cegc1_thrujan15 <- cegc1 %>% filter (wy == 2021, period == "Oct1thruJan15")
cegc1_thrujan15 <- cegc1_thrujan15$kaf



p1 <- ggplot(cegc1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = cegc1_thrujan15, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,3000, by = 200)) +
  labs(x = NULL) + ggtitle("trinity")
#ggsave("cegc1_bars.png", width = 21, height = 11, units = "in", dpi = 300)


p1
cegc1_r <- df_107 %>% filter(reservoir == "cegc1")

p2 <- ggplot(cegc1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.0003, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
  # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
    sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p2
p3 <- plot_grid(p1,p2, ncol=1)
ggsave("Trinity.png", width = 8.5, height = 11, units = "in", dpi = 300)

##########################
## OROVILLE ##
##########################

ordc1 <- df_sum %>% filter(reservoir == "ordc1")
ordc1_thrujan15 <- ordc1 %>% filter (wy == 2021, period == "Oct1thruJan15")
ordc1_thrujan15 <- ordc1_thrujan15$kaf



p1 <- ggplot(ordc1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = ordc1_thrujan15, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,9000, by = 500)) +
  labs(x = NULL) + ggtitle("oroville")
#ggsave("ordc1_bars.png", width = 21, height = 11, units = "in", dpi = 300)


p1
ordc1_r <- df_107 %>% filter(reservoir == "ordc1")

p2 <- ggplot(ordc1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.0001, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
  # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
    sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p2
p3 <- plot_grid(p1,p2, ncol=1)
ggsave("Oroville.png", width = 8.5, height = 11, units = "in", dpi = 300)


##########################
## New Melones ##
##########################

nmsc1 <- df_sum %>% filter(reservoir == "nmsc1")
nmsc1_thrujan15 <- nmsc1 %>% filter (wy == 2021, period == "Oct1thruJan15")
nmsc1_thrujan15 <- nmsc1_thrujan15$kaf



p1 <- ggplot(nmsc1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = nmsc1_thrujan15, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,9000, by = 200)) +
  labs(x = NULL) + ggtitle("new melones")
#ggsave("nmsc1_bars.png", width = 21, height = 11, units = "in", dpi = 300)


p1
nmsc1_r <- df_107 %>% filter(reservoir == "nmsc1")

p2 <- ggplot(nmsc1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.00024, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
  # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
    sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p2
p3 <- plot_grid(p1,p2, ncol=1)
ggsave("NewMelones.png", width = 8.5, height = 11, units = "in", dpi = 300)



##########################
## Millerton ##
##########################

frac1 <- df_sum %>% filter(reservoir == "frac1")
frac1_thrujan15 <- frac1 %>% filter (wy == 2021, period == "Oct1thruJan15")
frac1_thrujan15 <- frac1_thrujan15$kaf



p1 <- ggplot(frac1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = frac1_thrujan15, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,9000, by = 250)) +
  labs(x = NULL) + ggtitle("millerton")
#ggsave("frac1_bars.png", width = 21, height = 11, units = "in", dpi = 300)


p1
frac1_r <- df_107 %>% filter(reservoir == "frac1")

p2 <- ggplot(frac1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.00034, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
  # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
    sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p2
p3 <- plot_grid(p1,p2, ncol=1)
ggsave("Millerton.png", width = 8.5, height = 11, units = "in", dpi = 300)

##########################
## Pine Flat ##
##########################

pftc1 <- df_sum %>% filter(reservoir == "pftc1")
pftc1_thrujan15 <- pftc1 %>% filter (wy == 2021, period == "Oct1thruJan15")
pftc1_thrujan15 <- pftc1_thrujan15$kaf



p1 <- ggplot(pftc1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = pftc1_thrujan15, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,9000, by = 250)) +
  labs(x = NULL) + ggtitle("pine flat")
#ggsave("pftc1_bars.png", width = 21, height = 11, units = "in", dpi = 300)


p1
pftc1_r <- df_107 %>% filter(reservoir == "pftc1")

p2 <- ggplot(pftc1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.00044, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
  # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
    sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p2
p3 <- plot_grid(p1,p2, ncol=1)
ggsave("PineFlat.png", width = 8.5, height = 11, units = "in", dpi = 300)


##########################
## Exchequer ##
##########################

exqc1 <- df_sum %>% filter(reservoir == "exqc1")
exqc1_thrujan15 <- exqc1 %>% filter (wy == 2021, period == "Oct1thruJan15")
exqc1_thrujan15 <- exqc1_thrujan15$kaf



p1 <- ggplot(exqc1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = exqc1_thrujan15, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,9000, by = 250)) +
  labs(x = NULL) + ggtitle("mcclure")
#ggsave("exqc1_bars.png", width = 21, height = 11, units = "in", dpi = 300)


p1
exqc1_r <- df_107 %>% filter(reservoir == "exqc1")

p2 <- ggplot(exqc1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.00044, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
  # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
    sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p2
p3 <- plot_grid(p1,p2, ncol=1)
ggsave("Exchequer.png", width = 8.5, height = 11, units = "in", dpi = 300)


##########################
## Don Pedro ##
##########################

ndpc1 <- df_sum %>% filter(reservoir == "ndpc1")
ndpc1_thrujan15 <- ndpc1 %>% filter (wy == 2021, period == "Oct1thruJan15")
ndpc1_thrujan15 <- ndpc1_thrujan15$kaf



p1 <- ggplot(ndpc1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = ndpc1_thrujan15, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,9000, by = 250)) +
  labs(x = NULL) + ggtitle("don pedro")
#ggsave("ndpc1_bars.png", width = 21, height = 11, units = "in", dpi = 300)


p1
ndpc1_r <- df_107 %>% filter(reservoir == "ndpc1")

p2 <- ggplot(ndpc1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.00029, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
  # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
    sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p2
p3 <- plot_grid(p1,p2, ncol=1)
ggsave("DonPedro.png", width = 8.5, height = 11, units = "in", dpi = 300)




##########################
## Isabella ##
##########################

isac1 <- df_sum %>% filter(reservoir == "isac1")
isac1_thrujan15 <- isac1 %>% filter (wy == 2021, period == "Oct1thruJan15")
isac1_thrujan15 <- isac1_thrujan15$kaf



p1 <- ggplot(isac1, aes(x = wy, y = kaf, fill = period, group = wy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = isac1_thrujan15, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(0,9000, by = 250)) +
  labs(x = NULL) + ggtitle("isabella")
#ggsave("isac1_bars.png", width = 21, height = 11, units = "in", dpi = 300)


p1
isac1_r <- df_107 %>% filter(reservoir == "isac1")

p2 <- ggplot(isac1_r, aes(dowy, wy, height = cfs, group=as.factor(wy), fill = Oct1thruJan15))+
  geom_ridgeline( stat = "identity", show.legend = T, 
                  scale = 0.0007, 
                  alpha = 0.8) + #, 
  #min_height = -minh) + 
  # facet_wrap(~reservoir, nrow = 3 ) +
  scale_fill_viridis(name = "kaf") + theme_gray() + scale_x_continuous(
    breaks = breaks, labels = labels,
    sec.axis = dup_axis(name = NULL), expand = c(0,0)) +
  labs(x = NULL) + labs(y="water year") +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(.75, "in")) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), breaks = seq(1950, 2021, by = 5))

p2
p3 <- plot_grid(p1,p2, ncol=1)
ggsave("Isabella.png", width = 8.5, height = 11, units = "in", dpi = 300)

