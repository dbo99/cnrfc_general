rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



library(tidyverse)
library(lubridate)
library(plotly)
library(cowplot)
source("fun_defs.r")

#################################
##########################
###### HETC1 #######
##########################
################################
#{
#######################
##### annual web #####
######################

hetc1_web_ann <- read_csv("water-year-historical-flow-for-hetc1.csv") %>% 
                transmute(wy = year(`Water Year`), annual_inflow_taf = `Annual Flow`, srce = "hetc1_rfc_web_08March21")
head(hetc1_web_ann)
tail(hetc1_web_ann)

#####################################
##### daily icp3, non "f" suffix ####
#####################################
hetc1_icp_dly <- read_table("hetc1.qme_all_r.txt")
head(hetc1_icp_dly) #10/1/2004
tail(hetc1_icp_dly) #9/30/2019

## define daily timeseries rather than parsing out dates from text file format

hetc1_icp_dly_days <- seq(ymd('2004-10-01'),ymd('2019-09-30'), by = '1 day')


head(hetc1_icp_dly)

hetc1_icp_dly <- cbind(hetc1_icp_dly, hetc1_icp_dly_days)
rm(hetc1_icp_dly_days)
head(hetc1_icp_dly)

## get water year sum
hetc1_icp_dly <- hetc1_icp_dly %>% mutate(wy = water_year(hetc1_icp_dly_days),
                                          taf = cfsd*1.9834592/1000)
head(hetc1_icp_dly)

hetc1_icp_ann <- hetc1_icp_dly %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(taf)) %>% 
                 mutate( srce = "calibration", version =  "hetc1_qme_all")
head(hetc1_icp_ann)
rm(hetc1_icp_dly)

###############################
##### daily icp3, f suffix ####
###############################

hetc1f_icp_dly <- read_table("hetc1.fnf.7015.ed_r.txt")
head(hetc1f_icp_dly) #1/1/1970
tail(hetc1f_icp_dly) #9/30/2015

## define daily timeseries rather than parsing out dates from text file format
hetc1f_icp_dly_days <- seq(ymd('1970-01-01'),ymd('2015-09-30'), by = '1 day')
tail(hetc1f_icp_dly_days)
hetc1f_icp_dly <- cbind(hetc1f_icp_dly, hetc1f_icp_dly_days)
rm(hetc1f_icp_dly_days)

## get water year sum
hetc1f_icp_dly <- hetc1f_icp_dly %>% mutate(wy = water_year(hetc1f_icp_dly_days),
                                            taf = cfsd*1.9834592/1000)
head(hetc1f_icp_dly)

hetc1f_icp_ann <- hetc1f_icp_dly %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(taf)) %>%
                  mutate(srce = "calibration", version = "hetc1.fnf.7015.ed")
head(hetc1f_icp_ann)
rm(hetc1f_icp_dly)

###############################
##### SFPUC  emailed ####
###############################

hetc1_emailed <- read_csv("HHWP_Inflows_WY2005_2019_4CNRFC.csv") %>% transmute(date = Date, HETC1 = `Hetch Hetchy`)
head(hetc1_emailed) #10/1/2004
tail(hetc1_emailed) #9/30/2019

hetc1_emailed <- hetc1_emailed %>% mutate(wy = water_year(dmy(date)), taf = 1.9834592/1000*HETC1)
head(hetc1_emailed)
hetc1_emailed_ann <- hetc1_emailed %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(taf)) %>%
                     mutate(srce = "email", version = "HHWP_Inflows..csv")
head(hetc1_emailed_ann)
rm(hetc1_emailed) 

## combine all hetc1 into one timeseries

hetc1 <- rbind(hetc1_emailed_ann, 
               hetc1_icp_ann, 
               #hetc1_web_ann, 
               hetc1f_icp_ann) %>% 
         mutate( res = "hetc1")

rm(hetc1_emailed_ann, hetc1_icp_ann, hetc1_web_ann, hetc1f_icp_ann)

#hetc1 <- hetc1 %>% filter(wy > 1969)

p_het <- ggplot(hetc1, aes(wy, annual_inflow_taf, group = srce, color = srce, linetype = srce))  + geom_line()  #+ facet_wrap(~srce, ncol = 1)
#p1

#pl_het <- ggplotly(p_het)


#################################
##########################
###### CHVC1 #######
##########################
################################

## annual web ##

chvc1_web_ann <- read_csv("water-year-historical-flow-for-chvc1.csv") %>% 
               transmute(wy = year(`Water Year`), annual_inflow_taf = `Annual Flow`, srce = "chvc1_rfc_web_08March21")
head(chvc1_web_ann) #1981
tail(chvc1_web_ann) #2021

## icp daily to wy 

chvc1_icp_dly <- read_table("chvc1.qme.ed_r.txt")
head(chvc1_icp_dly) #1/1/1959
tail(chvc1_icp_dly) #9/30/2019

chvc1_icp_dly_days <- seq(ymd('1959-01-01'),ymd('2019-09-30'), by = '1 day')

chvc1_icp_dly <- cbind(chvc1_icp_dly, chvc1_icp_dly_days)
rm(chvc1_icp_dly_days)
head(chvc1_icp_dly)
chvc1_icp_dly <- chvc1_icp_dly %>% transmute(chvc1_icp_dly_days, taf = 1.9834592/1000*cfsd)
head(chvc1_icp_dly)

## get water year sum
chvc1_icp_dly <- chvc1_icp_dly %>% mutate(wy = water_year(chvc1_icp_dly_days))
head(chvc1_icp_dly)

chvc1_icp_ann <- chvc1_icp_dly %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(taf)) %>%
  mutate(srce = "calibration", version =  "chvc1.qme.ed")
head(chvc1_icp_ann)
rm(chvc1_icp_dly)

## emailed ##


chvc1_emailed <- read_csv("HHWP_Inflows_WY2005_2019_4CNRFC.csv") %>% transmute(date = Date, CHVC1 = `Cherry`)
head(chvc1_emailed) #10/1/2004
tail(chvc1_emailed) #9/30/2019

chvc1_emailed <- chvc1_emailed %>% mutate(wy = water_year(dmy(date)), taf = 1.9834592/1000*CHVC1)
head(chvc1_emailed)
chvc1_emailed_ann <- chvc1_emailed %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(taf)) %>%
  mutate(srce = "email", version = "HHWP_Inflows..csv")
head(chvc1_emailed_ann)




chvc1 <- rbind(#chvc1_web_ann,
               chvc1_icp_ann, 
               chvc1_emailed_ann) %>% 
         mutate( res = "chvc1")
rm(chvc1_emailed, chvc1_web_ann,chvc1_icp_ann, chvc1_emailed_ann) 
#chvc1 <- hetc1 %>% filter(wy > 1969)

p_chv <- ggplot(chvc1, aes(wy, annual_inflow_taf, group = srce, color = srce, linetype = srce))  + geom_line()  #+ facet_wrap(~srce, ncol = 1)
#p1

pl_chv <- ggplotly(p_chv)
#pl_chv



#################################
##########################
###### LNRC1 #######
##########################
################################


## annual web ##

lnrc1_web_ann <- read_csv("water-year-historical-flow-for-lnrc1.csv") %>% 
  transmute(wy = year(`Water Year`), annual_inflow_taf = `Annual Flow`, srce = "lnrc1_rfc_web_08March21")
head(lnrc1_web_ann) #1981
tail(lnrc1_web_ann) #2021

## icp daily to wy 

lnrc1_icp_dly <- read_table("lnrc1.qme.9620_r.txt")
head(lnrc1_icp_dly) #10/1/1996
tail(lnrc1_icp_dly) #9/30/2019

lnrc1_icp_dly_days <- seq(ymd('1996-10-01'),ymd('2019-09-30'), by = '1 day')

lnrc1_icp_dly <- cbind(lnrc1_icp_dly, lnrc1_icp_dly_days)
rm(lnrc1_icp_dly_days)
head(lnrc1_icp_dly)
lnrc1_icp_dly <- lnrc1_icp_dly %>% transmute(lnrc1_icp_dly_days, taf = 1.9834592/1000*cfsd)
head(lnrc1_icp_dly)

## get water year sum
lnrc1_icp_dly <- lnrc1_icp_dly %>% mutate(wy = water_year(lnrc1_icp_dly_days))
head(lnrc1_icp_dly)

lnrc1_icp_ann <- lnrc1_icp_dly %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(taf)) %>%
  mutate(srce = "calibration", version = "lnrc1.qme.9620")
head(lnrc1_icp_ann)
rm(lnrc1_icp_dly)

## emailed ##


lnrc1_emailed <- read_csv("HHWP_Inflows_WY2005_2019_4CNRFC.csv") %>% transmute(date = Date, LNRC1 = `Eleanor`)
head(lnrc1_emailed) #10/1/2004
tail(lnrc1_emailed) #9/30/2019

lnrc1_emailed <- lnrc1_emailed %>% mutate(wy = water_year(dmy(date)), taf = 1.9834592/1000*LNRC1)
head(lnrc1_emailed)
lnrc1_emailed_ann <- lnrc1_emailed %>% group_by(wy) %>% summarize(annual_inflow_taf = sum(taf)) %>%
  mutate(srce = "email", version = "HHWP_Inflows..csv")
head(lnrc1_emailed_ann)




lnrc1 <- rbind(#lnrc1_web_ann,
               lnrc1_icp_ann, 
               lnrc1_emailed_ann) %>% 
  mutate( res = "lnrc1")
rm(lnrc1_emailed, lnrc1_web_ann,lnrc1_icp_ann, lnrc1_emailed_ann) 
#lnrc1 <- hetc1 %>% filter(wy > 1969)

#p_lnr <- ggplot(lnrc1, aes(wy, annual_inflow_taf, group = srce, color = srce, linetype = srce))  + geom_line()  #+ facet_wrap(~srce, ncol = 1)
##p1
#
#pl_lnr <- ggplotly(p_lnr)
##pl_lnr
#
#
#p <- plot_grid(p_het,p_chv, p_lnr, ncol = 1 )
#p
#
#pl <- ggplotly(p)

df <- rbind(hetc1, chvc1, lnrc1) %>% mutate(annual = paste0(res, "_", version),
                                            trace = srce) %>%
                                     filter(wy >= 2005)


p_all <- ggplot(df, aes(wy, annual_inflow_taf,  
                        color = annual,  
                        linetype = trace )) + 
                        geom_line()
p_all
p_annual <- ggplotly(p_all)
p_annual
#}

################################
#########################
###############
###### Daily
###############
#########################
################################
#rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lubridate)
library(plotly)
library(cowplot)
source("fun_defs.r")

####################
## HETC1 daily #####
####################
{
#####################################
##### daily icp3, non "f" suffix ####
#####################################
hetc1_icp_dly <- read_table("hetc1.qme_all_r.txt")
head(hetc1_icp_dly) #10/1/2004
tail(hetc1_icp_dly) #9/30/2019

## define daily timeseries rather than parsing out dates from text file format

hetc1_icp_dly_days <- seq(ymd('2004-10-01'),ymd('2019-09-30'), by = '1 day')
head(hetc1_icp_dly)

hetc1_icp_dly <- cbind(hetc1_icp_dly, hetc1_icp_dly_days)
rm(hetc1_icp_dly_days)
head(hetc1_icp_dly)

## get water year sum
hetc1_icp_dly <- hetc1_icp_dly %>% transmute(date = date(hetc1_icp_dly_days),
                                             wy = water_year(date),
                                             taf = cfsd*1.9834592/1000,
                                             res = "hetc1",
                                             srce = "calibration",
                                             version = "qme_all")
head(hetc1_icp_dly)



###############################
##### daily icp3, f suffix ####
###############################

hetc1f_icp_dly <- read_table("hetc1.fnf.7015.ed_r.txt")
head(hetc1f_icp_dly) #1/1/1970
tail(hetc1f_icp_dly) #9/30/2015

## define daily timeseries rather than parsing out dates from text file format
hetc1f_icp_dly_days <- seq(ymd('1970-01-01'),ymd('2015-09-30'), by = '1 day')

hetc1f_icp_dly <- cbind(hetc1f_icp_dly, hetc1f_icp_dly_days)
rm(hetc1f_icp_dly_days)

## get water year sum
hetc1f_icp_dly <- hetc1f_icp_dly %>% transmute(date = date(hetc1f_icp_dly_days),
                                            wy = water_year(date),
                                            taf = cfsd*1.9834592/1000,
                                            res = "hetc1",
                                            srce = "calibration",
                                            version = "wy70_wy15")
head(hetc1f_icp_dly)




###############################
##### SFPUC  emailed ####
###############################

hetc1_emailed <- read_csv("HHWP_Inflows_WY2005_2019_4CNRFC.csv") %>% transmute(date = dmy(Date), HETC1 = `Hetch Hetchy`)
head(hetc1_emailed) #10/1/2004
tail(hetc1_emailed) #9/30/2019

hetc1_emailed <- hetc1_emailed %>% transmute(date,
                                             wy = water_year(date), 
                                             taf = 1.9834592/1000*HETC1,
                                             res = "hetc1",
                                             srce = "emailed",
                                             version = "HHWP_Inflows..csv")
head(hetc1_emailed)

hetc1 <- rbind(hetc1_emailed,hetc1_icp_dly, hetc1f_icp_dly )
head(hetc1)
}


#####################
### CHVC1 daily 
#####################
{
## icp daily to wy 

chvc1_icp_dly <- read_table("chvc1.qme.ed_r.txt")
head(chvc1_icp_dly) #1/1/1959
tail(chvc1_icp_dly) #9/30/2019

chvc1_icp_dly_days <- seq(ymd('1959-01-01'),ymd('2019-09-30'), by = '1 day')

chvc1_icp_dly <- cbind(chvc1_icp_dly, chvc1_icp_dly_days)
rm(chvc1_icp_dly_days)
head(chvc1_icp_dly)
chvc1_icp_dly <- chvc1_icp_dly %>% transmute(date = date(chvc1_icp_dly_days),
                                             wy = water_year(date),
                                             taf = 1.9834592/1000*cfsd,
                                             srce = "calibration",
                                             version = "chvc1.qme.ed")
head(chvc1_icp_dly)

chvc1_emailed <- read_csv("HHWP_Inflows_WY2005_2019_4CNRFC.csv") %>% 
                 transmute(date = dmy(Date), CHVC1 = `Cherry`)
head(chvc1_emailed) #10/1/2004
tail(chvc1_emailed) #9/30/2019

chvc1_emailed <- chvc1_emailed %>% transmute(date = ymd(date), 
                                             wy = water_year(ymd(date)), 
                                             taf = 1.9834592/1000*CHVC1,
                                             srce = "emailed",
                                             version = "HHWP_Inflows..csv")
head(chvc1_emailed)

chvc1 <- rbind(chvc1_icp_dly, chvc1_emailed) %>% mutate(res = "chvc1")
head(chvc1)
}
#####################
### LNRC1 daily 
#####################

{
## icp daily to wy 

lnrc1_icp_dly <- read_table("lnrc1.qme.9620_r.txt")
head(lnrc1_icp_dly) #10/1/1996
tail(lnrc1_icp_dly) #9/30/2019


lnrc1_icp_dly_days <- seq(ymd('1996-10-01'),ymd('2019-09-30'), by = '1 day')

lnrc1_icp_dly <- cbind(lnrc1_icp_dly, lnrc1_icp_dly_days)
rm(lnrc1_icp_dly_days)
head(lnrc1_icp_dly)
lnrc1_icp_dly <- lnrc1_icp_dly %>% transmute(date = date(lnrc1_icp_dly_days), taf = 1.9834592/1000*cfsd)
head(lnrc1_icp_dly)

## 
lnrc1_icp_dly <- lnrc1_icp_dly %>% mutate(date = date(date), 
                                          wy = water_year(date),
                                          srce = "calibration",
                                          version = "lnrc1.qme.9620")
head(lnrc1_icp_dly)



## emailed ##


lnrc1_emailed <- read_csv("HHWP_Inflows_WY2005_2019_4CNRFC.csv") %>% transmute(date = dmy(Date), LNRC1 = `Eleanor`)
head(lnrc1_emailed) #10/1/2004
tail(lnrc1_emailed) #9/30/2019

lnrc1_emailed <- lnrc1_emailed %>% transmute(date, 
                                             wy = water_year(date), 
                                             taf = 1.9834592/1000*LNRC1,
                                             srce = "emailed",
                                             version = "HHWP_Inflows..csv")
head(lnrc1_emailed)

lnrc1 <- rbind(lnrc1_icp_dly, lnrc1_emailed) %>% mutate(res = "lnrc1")
head(lnrc1)




}


df_dly <- rbind(chvc1, lnrc1, hetc1) %>% transmute(date= date(date), 
                                            wy, srce, res, daily = srce,
                                            daily_inflow_taf = round(taf,2),
                                            trace = paste0(res, "_", version),version)
head(df_dly) 

df_dly_3Nov2015 <- df_dly %>% filter(date == "2015-11-03")

df_dly_0519 <- df_dly %>% filter(wy >= 2005, wy <= 2019)

p1 <- ggplot(df_dly_0519, aes(date, daily_inflow_taf, linetype = daily, color = trace)) + geom_line()
p1
p1_ly <- ggplotly(p1)
p1_ly

unique(df_dly$trace)
################
######
## difference
######
################

## chvc1 ##

chvc1_diff_email <- df_dly %>% filter(wy >= 2005, wy <= 2019, res == "chvc1", srce  == "emailed")
chvc1_diff_calib <- df_dly %>% filter(wy >= 2005, wy <= 2019, res == "chvc1", srce  == "calibration")

chvc1_diff <- chvc1_diff_email %>% mutate(
                                         chvc1_emailed_minus_calib_inflow_taf = 
                chvc1_diff_email$daily_inflow_taf - chvc1_diff_calib$daily_inflow_taf)

chvc1_diff <- chvc1_diff %>% transmute(date, wy,  taf_diff = chvc1_emailed_minus_calib_inflow_taf,
                                       res = "chvc1: email minus calibration")

## lnrc1 ##

lnrc1_diff_email <- df_dly %>% filter(wy >= 2005, wy <= 2019, res == "lnrc1", srce  == "emailed")
lnrc1_diff_calib <- df_dly %>% filter(wy >= 2005, wy <= 2019, res == "lnrc1", srce  == "calibration")

lnrc1_diff <- lnrc1_diff_email %>% mutate(
                                         lnrc1_emailed_minus_calib_inflow_taf = 
    lnrc1_diff_email$daily_inflow_taf - lnrc1_diff_calib$daily_inflow_taf)

lnrc1_diff <- lnrc1_diff %>% transmute(date, wy, taf_diff=  round(lnrc1_emailed_minus_calib_inflow_taf,2),
                                         res = "lnrc1: email minus calibration")

head(chvc1_diff)
head(lnrc1_diff)


## hetc1 ##
head(df_dly)
unique(df_dly$version)
hetc1_diff_email <- df_dly %>% filter(wy >= 2005, wy <= 2019, res == "hetc1", version  == "HHWP_Inflows..csv")# %>%
                              #filter(date == "2015-11-03")
hetc1_diff_email

hetc1_diff_calib_qmeall <- df_dly %>% filter(wy >= 2005, wy <= 2019, res == "hetc1", srce  == "calibration", 
                                            version == "qme_all") %>% mutate( qme1inf = daily_inflow_taf) %>%
                                            transmute(date1 = date, qme1inf) # %>% filter(date1 == "2015-11-03")
hetc1_diff_calib_qmeall

#hetc1_diff_calib_qmeall_3Nov15
hetc1_diff_calib_wy7015 <- df_dly %>% filter(wy >= 2005, wy <= 2019, res == "hetc1", srce  == "calibration", 
                                             version == "wy70_wy15") %>% mutate( qme2inf = daily_inflow_taf) %>%
                                             transmute(date2 = date, qme2inf) #%>% filter(date2 == "2012-12-03")
hetc1_diff_calib_wy7015

#hetc1_diff_1 <- cbind(hetc1_diff_email,hetc1_diff_calib_qmeall )
#hetc1_diff_3Nov15 <- hetc1_diff_1 %>% filter(date == "2015-11-03")


hetc1_diff_1 <- hetc1_diff_email %>% mutate(
  hetc1_emailed_minus_qmeall = 
    hetc1_diff_email$daily_inflow_taf - hetc1_diff_calib_qmeall$qme1inf)

hetc1_diff_1 <- hetc1_diff_1 %>% transmute(date, wy,  taf_diff =  hetc1_emailed_minus_qmeall,
                                     res = "hetc1: email minus calibv1 (qme_all)")
head(hetc1_diff_1)

hetc1_diff_email2 <- hetc1_diff_email %>% filter(wy <= 2015, wy >= 2005)
hetc1_diff_2 <- hetc1_diff_email2 %>% mutate(
  hetc1_emailed_minus_wy7015 = 
    hetc1_diff_email2$daily_inflow_taf - hetc1_diff_calib_wy7015$qme2inf)

hetc1_diff_2 <- hetc1_diff_2 %>% transmute(date, wy,  taf_diff =  hetc1_emailed_minus_wy7015,
                                           res = "hetc1: email minus calibv2 (wy70wy15)")
head(hetc1_diff_2)

head(hetc1_diff_1)
head(hetc1_diff_2)

hetc1_diff <- rbind(hetc1_diff_1, hetc1_diff_2)

hetc1_lnrc1_chvc1_dlydiff <- rbind(hetc1_diff, chvc1_diff, lnrc1_diff) %>% mutate(daily_diff = res)

colnames(hetc1_lnrc1_chvc1_dlydiff)




p_dlydiff <- ggplot(hetc1_lnrc1_chvc1_dlydiff, aes(date, taf_diff, color = daily_diff )) + geom_line()
p_dlydiff
p_dlydiff_ly <- ggplotly(p_dlydiff)
p_dlydiff_ly
pboth <- plot_grid(p1,p_dlydiff, ncol = 1)
pboth
subplot(p_annual, p1_ly, p_dlydiff_ly, nrows = 3 )


