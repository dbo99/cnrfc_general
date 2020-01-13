

start_date <- ymd("1990-10-01")
end_date <- ymd("2025-09-30")

date <- seq.Date(start_date, end_date, by = "day")
year <-  year(date)
df <- data.frame(date, year)        
df_cumdoy <- read_csv("leap_yrs.csv")                        

df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 
df <- df %>% mutate(yday = yday(date))




df <- df %>% mutate(dowy =   ifelse(cumdoy > 365, 
                                    ifelse(yday>274, yday-274, yday+92), 
                                    ifelse(yday>273, yday-273, yday+92)))
                    

write_csv(df, "daily_dowy.csv")