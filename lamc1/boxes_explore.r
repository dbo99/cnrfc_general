

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
df_hrly <- rbind(thir9, sixt1) %>% mutate(fcastday_memgroup_mem = paste0(fcastday12z, "_", membergroup, "_", member)) %>%
  #      filter(fcasthr_gmt <= "1986-02-20 04:00:00", fcastday12z >= "1986-02-11", fcastday12z <= "1986-02-17") %>% 
      mutate(am_fcast = as.factor(fcastday12z), member_n = member, member = as.factor(member), fcast_issue_date = fcastday12z,
             fcastdate = date(fcasthr_gmt)) %>% select(-fcastday12z)
head(df_hrly)

p_hrly <- ggplot(df, aes(fcasthr_gmt, cfs, group = fcastday_memgroup_mem), color = "gray60") + geom_line(alpha = 0.3) + 
  facet_grid(fcastday12z~membergroup)
# facet_grid(membergroup~fcastday12z)
ggsave("explor10.pdf", width = 21, height = 11, units = "in", dpi = 300)

#      filter(fcasthr_gmt <= "1986-02-20 04:00:00", fcastday12z >= "1986-02-11", fcastday12z <= "1986-02-17") %>% 



df_dly <- df_hrly %>% 
         # filter(fcasthr_gmt <= "1986-02-20 04:00:00", fcast_issue_date >= "1986-02-11", fcast_issue_date <= "1986-02-17") %>%  
          filter(fcast_issue_date >= "1986-02-11", fcast_issue_date <= "1986-02-17") %>%  mutate(membergroup = as.factor(membergroup)) %>%
          group_by (membergroup, member, fcast_issue_date, fcastdate) %>% summarize(cfsd = mean(cfs), af = 1.98347*cfsd) 
head(df_dly)
head(qme_dly)
qme_dly1 <- qme_dly %>% mutate(membergroup = "39_79to17", fcast_issue_date = date, fcastdate = date, member = "obs") %>% select(-date)
qme_dly2 <- qme_dly %>% mutate(membergroup = "61_50to10", fcast_issue_date = date, fcastdate = date, member = "obs") %>% select(-date)
qme_dly <- rbind(qme_dly1, qme_dly2) %>% mutate(member = as.factor(member), membergroup = as.factor(membergroup))
qme_dly <- as.data.frame(qme_dly)
head(qme_dly)
head(df_dly)
df_dly <- df_dly %>% ungroup()
head(df_dly)
df_dly <- rbind(df_dly, qme_dly) #%>% ungroup()
head(df_dly)
df_dly <- df_dly %>% mutate(fcastiss_memgroup_mem = paste0(fcast_issue_date, "_", membergroup, "_", member ))

head(df_dly)
df_dly <- df_dly %>%  filter(fcast_issue_date >= "1986-02-11", fcast_issue_date <= "1986-02-17")
min_dfdly <- min(df_dly$fcastdate)
max_dfdly <- max(df_dly$fcastdate)

#qme_dly <- qme_dly %>% filter(date >= min_dfdly, date <= max_dfdly) #%>% 
                 #mutate(fcastiss_memgroup_mem = "obs", member = "obs",fcastdate = date, membergroup = "obs")
head(qme_dly)


                                  
                                  
                                  
                                  
                                  
p_dly <- ggplot(df_dly, aes(fcastdate, cfsd, group = fcastiss_memgroup_mem), color = "gray60") + geom_line(alpha = 0.3) + 
  facet_grid(fcast_issue_date~membergroup) + geom_line(data = df_dly %>% filter(member== "obs"), aes(fcastdate, cfsd), color = "red" )
# facet_grid(membergroup~fcastday12z)
p_dly
ggsave("daily.pdf", width = 21, height = 11, units = "in", dpi = 300)