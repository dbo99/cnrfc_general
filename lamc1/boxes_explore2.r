

{
  rm(list = ls())
  rstudioapi::getActiveDocumentContext
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  source("libs.r")
  source("fun_defs.r")
  
  thir9 <- read_csv("39_79to17.csv")
  sixt1 <- read_csv("61_50to10.csv")
  qme_dly <- read_table("lamc1_fnf3.qme.ed.txt") # to 10/1/1975 9/30/19
  date <- seq(ymd('1975-10-01'), ymd('2019-09-30'), by = '1 day')
  qme_dly <- cbind(qme_dly, date) %>% transmute(date, cfsd = cmsd * 35.3147, af = cfsd * 1.98347 )
  rm(date)##
  
}


df_hrly <- rbind(thir9, sixt1) %>% mutate(fcastday_memgroup_mem = 
          paste0(fcastday12z, "_", membergroup, "_", member)) %>%
  #      filter(fcasthr_gmt <= "1986-02-20 04:00:00", fcastday12z >= "1986-02-11", fcastday12z <= "1986-02-17") %>% 
  mutate(am_fcast = as.factor(fcastday12z), member_n = member, member = as.factor(member), fcast_issue_date = fcastday12z,
         fcastdate = date(fcasthr_gmt)) %>% select(-fcastday12z)
head(df_hrly)
date_range <- c("1986-02-10")
                #"1986-02-11",
                #"1986-02-12", 
                #"1986-02-13",
                #"1986-02-14", 
                #"1986-02-15",
                #"1986-02-16")
df_temp <- df_hrly %>% filter( am_fcast  %in% date_range,
                               membergroup == "61_50to10")
qme_hrly_date <- seq(
                 from=as.POSIXct("1986-02-10 12:00", tz="America/Los_Angeles"),
                 to=as.POSIXct("1986-03-03 12:00", tz="America/Los_Angeles"),
                 by="hour")  
qme_hrly_date <- data.frame(qme_hrly_date) %>% mutate(date = date(qme_hrly_date))

qme_hrlydf <- right_join( qme_dly, qme_hrly_date,) %>%
            transmute(qme_hrly = qme_hrly_date, cfs = cfsd, date, af)



p_hrly <- ggplot() + geom_line(data = df_temp %>% #filter(fcasthr_gmt <= "1986-02-20 04:00:00", 
                              #      fcastdate >= "1986-02-11", 
                              #      fcastdate <= "1986-02-17"),
                # filter(fcasthr_gmt <= "1986-02-20 04:00:00", 
                    #    filter(
                        #fcasthr_gmt <= "1986-02-24 12:00:00", 
                    #    am_fcast  %in% date_range,
                    #    membergroup == "61_50to10"),
         # aes(fcasthr_gmt, cfs, group = fcastday_memgroup_mem),
             # color = member),
         #     color = "gray60",
         #     alpha = 0.5) +   
             geom_line(data = qme_hrlydf %>% filter(date >= "1986-02-10", 
                                                   date <= "1986-03-03") , 
              aes(x = qme_hrly, y = cfs), color = "blue", size = 1) #+
                #facet_grid(fcast_issue_date~membergroup) 
     
  #facet_grid(am_fcast~membergroup)
# facet_grid(membergroup~fcastday12z)
p_hrly
#ggsave("explor10.pdf", width = 21, height = 11, units = "in", dpi = 300)
#################
###############3
as_tibble(qme_hrlydf)
qme_hrly_test <- qme_hrlydf %>% filter(qme_hrly == ymd_hms("1986-02-10 12:00:00"))#, 
                             #filter(qme_hrly >= ymd_hms("1986-02-10 00:00:00")) %>%
                            # filter(qme_hrly <= ymd_hms("1986-03-03 00:00:00"))
################
#################
as_tibble(qme_hrly)
ggplot(qme_hrly %>% filter(
                           qme_hrly >= ymd_hms("1986-02-10 12:00:00"), 
                           qme_hrly <= ymd_hms("1986-03-03 00:00:00")), 
       aes(qme_hrly, cfs)) + geom_point()


df_dly <-  #df_hrly %>%
           df_temp %>%
           # filter(fcasthr_gmt <= "1986-02-20 04:00:00", fcast_issue_date >= "1986-02-11", fcast_issue_date <= "1986-02-17") %>%  
           filter(fcast_issue_date >= "1986-02-10", fcast_issue_date <= "1986-02-17") %>%  
           mutate(membergroup = as.factor(membergroup)) %>%
           group_by (membergroup, member, fcast_issue_date, fcastdate) %>% 
           summarize(cfsd = mean(cfs), af = 1.98347*cfsd) 

head(df_dly)
df_dly <- df_dly %>% mutate(fcastissdate_memgroup_mem = paste0(fcast_issue_date, "_", membergroup, "_", member)) 
head(df_dly)
p_dly <- ggplot() + geom_line(data = df_dly, aes(x = fcastdate, y = cfsd, 
                   group = fcastissdate_memgroup_mem), color = "gray60", size = 0.1) + 
                   geom_line(data = qme_dly %>% filter(date >= "1986-02-10", date <= "1986-03-03") , 
                              aes(x = date, y = cfsd), color = "blue", size = 0.5) +
  geom_point(data = qme_dly %>% filter(date >= "1986-02-10", date <= "1986-03-03") , 
            aes(x = date, y = cfsd), color = "blue", size = 0.53) +
          facet_grid(fcast_issue_date~membergroup) 
# facet_grid(membergroup~fcastday12z)
p_dly
plot_grid(p_hrly, p_dly, ncol = 1)
ggsave("daily.pdf", width = 21, height = 11, units = "in", dpi = 300)
