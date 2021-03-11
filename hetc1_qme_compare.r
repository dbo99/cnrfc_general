rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(lubridate)
library(plotly)
library(cowplot)
source("fun_defs.r")

#######################
##### annual web #####
######################

hetc1_web_ann <- read_csv("water-year-historical-flow-for-hetc1.csv") %>% 
  transmute(wy = year(`Water Year`), annual_inflow_taf = `Annual Flow`, srce = "cnrfc.com", version = "webdwnld_8Mar2021")
head(hetc1_web_ann)
tail(hetc1_web_ann)

#####################################
##### most recent 'best' (2004 - 2019) icp3, non "f" suffix ####
#####################################
hetc1_icp_0419 <- read_table("hetc1.qme_all_r.txt")
head(hetc1_icp_0419) #10/1/2004
tail(hetc1_icp_0419) #9/30/2019

## define daily timeseries rather than parsing out dates from text file format

hetc1_icp_0419_days <- seq(ymd('2004-10-01'),ymd('2019-09-30'), by = '1 day')


head(hetc1_icp_0419)

hetc1_icp_0419 <- cbind(hetc1_icp_0419, hetc1_icp_0419_days)
rm(hetc1_icp_0419_days)
head(hetc1_icp_0419)

hetc1_icp_0419 <- hetc1_icp_0419 %>% transmute(date = hetc1_icp_0419_days, wy = water_year(hetc1_icp_0419_days),
                                          daily_inflow_taf = cfsd*1.9834592/1000,
                                          srce = "calibration", version = "hetc1_qme_all(04_19)")
head(hetc1_icp_0419)
## get water year sum
hetc1_icp_0419_ann <- hetc1_icp_0419 %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(daily_inflow_taf)) %>% 
  mutate( srce = "calibration", version =  "hetc1_qme_all(04_19)")
head(hetc1_icp_0419_ann)
#rm(hetc1_icp_0419)


#####################################
#####  'hetc1.fnf.7015' icp3
#####################################
hetc1_icp_7015 <- read_table("hetc1.fnf.7015.ed_r.txt")
head(hetc1_icp_7015) #01/01/1970
tail(hetc1_icp_7015) #9/30/2015

## define daily timeseries rather than parsing out dates from text file format

hetc1_icp_7015_days <- seq(ymd('1970-01-01'),ymd('2015-09-30'), by = '1 day')


head(hetc1_icp_7015)

hetc1_icp_7015 <- cbind(hetc1_icp_7015, hetc1_icp_7015_days)
rm(hetc1_icp_7015_days)
head(hetc1_icp_7015)

## get water year sum
hetc1_icp_7015 <- hetc1_icp_7015 %>% transmute(date = hetc1_icp_7015_days, wy = water_year(hetc1_icp_7015_days),
                                            daily_inflow_taf = cfsd*1.9834592/1000, srce = "calibration",
                                            version = "hetc1.fnf.7015.ed")
head(hetc1_icp_7015)

hetc1_icp_7015_ann <- hetc1_icp_7015 %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(daily_inflow_taf)) %>% 
  mutate( srce = "calibration", version =  "hetc1.fnf.7015.ed")
head(hetc1_icp_7015_ann)
#rm(hetc1_icp_7015)


#####################################
#####  'hetc1.fnf.6106' icp3
#####################################
hetc1_icp_6106 <- read_table("hetc1.fnf.6106_r.txt") %>% mutate(res = "hetc1")
head(hetc1_icp_6106) #10/01/1960
tail(hetc1_icp_6106) #04/30/2006

## define daily timeseries rather than parsing out dates from text file format

hetc1_icp_6106_days <- seq(ymd('1960-10-01'),ymd('2006-04-30'), by = '1 day')


head(hetc1_icp_6106)

hetc1_icp_6106 <- cbind(hetc1_icp_6106, hetc1_icp_6106_days)
rm(hetc1_icp_6106_days)
head(hetc1_icp_6106)
## get water year sum
hetc1_icp_6106 <- hetc1_icp_6106 %>% transmute(date = hetc1_icp_6106_days, wy = water_year(hetc1_icp_6106_days),
                                            daily_inflow_taf = cfsd*1.9834592/1000,
                                            srce = "calibration", version = "hetc1.fnf.6106") %>% 
                                    filter(wy < 2006)
head(hetc1_icp_6106)

hetc1_icp_6106_ann <- hetc1_icp_6106 %>% group_by(wy) %>% 
  summarize(annual_inflow_taf = sum(daily_inflow_taf, na.rm=TRUE)) %>% 
  mutate( srce = "calibration", version =  "hetc1.fnf.6106")
head(hetc1_icp_6106_ann)
#rm(hetc1_icp_6106)




###############################
##### SFPUC  email 1  ####
###############################

hetc1_icp_email_1 <- read_csv("HHWP_Inflows_WY2005_2019_4CNRFC.csv") %>% transmute(date = Date, HETC1 = `Hetch Hetchy`)
head(hetc1_icp_email_1) #10/1/2004
tail(hetc1_icp_email_1) #9/30/2019

hetc1_icp_email_1_days <- seq(ymd('2004-10-01'),ymd('2019-09-30'), by = '1 day')
hetc1_icp_email_1 <- cbind(hetc1_icp_email_1, hetc1_icp_email_1_days) %>% 
                     transmute(date = hetc1_icp_email_1_days, wy = water_year(date), daily_inflow_taf = 1.9834592/1000*HETC1, 
                               srce = "email", version = "HHWP_Inflows..csv")

head(hetc1_icp_email_1)

hetc1_email_1_ann <- read_csv("HHWP_Inflows_WY2005_2019_4CNRFC.csv") %>% transmute(date = Date, HETC1 = `Hetch Hetchy`)
head(hetc1_email_1_ann) #10/1/2004
tail(hetc1_email_1_ann) #9/30/2019

hetc1_email_1_ann <- hetc1_email_1_ann %>% mutate(wy = water_year(dmy(date)), taf = 1.9834592/1000*HETC1)
head(hetc1_email_1_ann)
hetc1_email_1_ann <- hetc1_email_1_ann %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(taf)) %>%
  mutate(srce = "email", version = "HHWP_Inflows..csv")
head(hetc1_email_1_ann)
#rm(hetc1_email_1_ann) 


#########################################
##### SFPUC  email 2  - 3 traces ####
#########################################

hetc1_email_2_ann <- read_csv("Mar6_hetc1.csv") 
head(hetc1_email_2_ann)
colnames(hetc1_email_2_ann)
hetc1_email_2_ann <- hetc1_email_2_ann %>% 
                     pivot_longer(!wy, names_to = "version", values_to = "annual_inflow_taf") %>%
                     mutate( srce = "email") %>% mutate(annual_inflow_taf = annual_inflow_taf/1000)

head(hetc1_email_2_ann)

########## make df daily and df annual ####

## daily ##
df_hetc1_dly <- rbind(hetc1_icp_email_1, hetc1_icp_0419, hetc1_icp_7015, hetc1_icp_6106) %>% mutate(tstep = "dly")

## dummy data for plotly colors
date <- c("1901-10-02","1901-10-03", "1901-10-04", "1901-10-05" )
wy <- c(1902,1902, 1902, 1902)
daily_inflow_taf <- c(NA, NA, NA, NA)
version <- c("water-year-historical.xlsx_hhwp_smth7019", 
             "water-year-historical.xlsx_cnrfc",
             "water-year-historical.xlsx_hhwp",
             "webdwnld_8Mar2021" )
srce <- c("email", "email", "email", "cnrfc.com")
tstep <-  rep("daily", 4)
dummydata <- data.frame(date, wy, daily_inflow_taf, srce, version, tstep)
head(dummydata)
df_hetc1_dly <- rbind(df_hetc1_dly,dummydata )

## ann #
df_hetc1_dly

df_hetc1_ann <- rbind(hetc1_web_ann, hetc1_icp_0419_ann, hetc1_icp_6106_ann, 
                   hetc1_icp_7015_ann, hetc1_email_1_ann, hetc1_email_2_ann) %>%
  filter(wy > 1900, wy < 2021) %>% mutate(tstep = "annual")




#df_hetc1_ann$version <- factor(df_hetc1_ann$version)

####### daily plot ###########

p_het_dly <- ggplot(df_hetc1_dly, aes(date, daily_inflow_taf, color = version, linetype = srce)) + geom_line()
p_het_dly
p_het_dly_plotly <- ggplotly(p_het_dly)

### annual plot ####

p_het_ann <- ggplot(df_hetc1_ann, aes(wy, annual_inflow_taf, color = version, linetype = srce)) + geom_line() +
             scale_colour_discrete(drop=TRUE,limits = levels(df_hetc1_ann$version))
p_het_ann
p_het_ann_plotly <- ggplotly(p_het_ann)


subplot(p_het_ann_plotly, p_het_dly_plotly, nrows = 2 )

versions <- c(unique(df_hetc1_ann$version))
versions
unique(df_hetc1_dly$version)

