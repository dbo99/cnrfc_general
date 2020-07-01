

{
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)

pmeans_dbo <- read_csv("PrismVersions_dbo_fall19zones.csv") 
head(pmeans_dbo)

#pmeans_dbo_l <- pmeans_dbo %>% pivot_longer(-Name, names_to = "version", values_to = "inch")
#head(pmeans_dbo_l)

pmeans_pf <- read_csv("basins2019_pf.csv") 
head(pmeans_pf)

#pmeans_pf_l <- pmeans_pf %>% pivot_longer(-Name, names_to = "version", values_to = "inch")
#head(pmeans_pf_l)

pmeans_471to500 <- right_join( pmeans_pf, pmeans_dbo)
head(pmeans_471to500)

pmeans_500to471 <- right_join( pmeans_dbo, pmeans_pf)
head(pmeans_471to500)





write_csv(pmeans_471to500, "pmeans_471to503.csv")
write_csv(pmeans_500to471, "pmeans_503to471.csv")
}


