



{
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(plotly)
#library(ggplot2)
#library(readxl) 
#library(tidyr)

#read_excel_allsheets <- function(filename, tibble = FALSE) {
#  # I prefer straight data.frames
#  # but if you like tidyverse tibbles (the default with read_excel)
#  # then just pass tibble = TRUE
#  sheets <- readxl::excel_sheets(filename)
#  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
#  if(!tibble) x <- lapply(x, as.data.frame)
#  names(x) <- sheets
#  x
#}
#
#mysheets <- read_excel_allsheets("2021newbasinelevmetercounts.xlsx")
path <- "2021newbasinelevmetercounts.xlsx"
mysheets <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

}

#mysheets2 <- lapply(names(mysheets), function(x) {
mysheets <- lapply( names(mysheets), function(x) {
 mysheets[[x]] %>% 
    mutate(zone = x)
})

df <- bind_rows(mysheets)





#list2env(mysheets2 ,.GlobalEnv)
#list2env(mysheets ,.GlobalEnv)

#names(mysheets) <- paste("membro", seq_along(mysheets), sep = ".")
#list2env(mysheets,envir=.GlobalEnv)

#lapply(mysheets, transform, ratio = y / y[1])
#rm(mysheets)
df <- df %>% transmute(elev_ft = VALUE*3.28084, COUNT, zone)

un_df <- df %>% group_by(zone) %>% uncount(COUNT)

 p <- ggplot(un_df, aes(elev_ft, color = zone)) + 
   stat_ecdf() +
  # stat_ecdf(geom= "step", n = NULL) +
   coord_flip() +
     #geom_hline(yintercept = 0.10)
     scale_y_continuous(breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) + 
     labs(y = "proportion of total")
 
ggplotly(p)


undf_table <- un_df %>% 
  group_by(zone) %>% 
  summarise(elev_ft = quantile(elev_ft, c(0,.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95, 1)), 
                                    q = c(0,.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95, 1)) %>%
          mutate(elev_ft = round(elev_ft, 1))
undf_table_w <- undf_table %>% pivot_wider(names_from = zone, values_from = elev_ft) 
write_csv(undf_table_w, "area_elev.csv")
