


  rm(list = ls())
  rstudioapi::getActiveDocumentContext
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  library(tidyverse)
  library(rgdal)
  library(mapview)
  library(sf)
  library(ggthemes)
  #library(ggspatial)
  
  
  prismmeans_w <- read_csv("Prism_1971-2000PrcpNormalswithone81.csv") 
  head(prismmeans_w)
  
  prismmeans_l <- prismmeans_w %>% pivot_longer(-Name, names_to = "version", values_to = "inch")
  head(prismmeans_l)
  
  prismfacs <- read_csv("paf.csv") %>% select(-prism_in) %>% select(Name, wy81_10_calb_in) %>% transmute(Name, inch = wy81_10_calb_in, version = "calb_81_10" )
  head(prismfacs)
  
  prismmeans_l <- rbind(prismmeans_l, prismfacs)
  
  zones <- readOGR(".", "cnrfc_zones_03222020_wgs84_enfor._0.05_ret") %>% st_as_sf() #%>% 
  # mutate(zone = Name, OBJECTID = OBJECTID_1) %>% select(-Name, -OBJECTID_1, -Shape_Leng, -Shape_Area)
  head(zones)
 
  #prismmeans <- read_csv("Prism_1971-2000PrcpNormalswithone81.csv")
  #head(prismmeans)
  #zones <- right_join(prismfacs, zones) %>% st_as_sf
  #head(zones)
  zones <- right_join(prismmeans_l, zones) %>% st_as_sf
  #zones <- rbind(zones, prismfacs) %>% st_as_sf
  head(zones)
   # for some reason a few basins likey coyc1hof don't have version? raster extraction with wrong shapefile?
  #zones <- as.data.frame(zones) %>% drop_na(version) %>% st_as_sf
  
  unique(zones$version)
  
#ggplot(zones, aes(fill = inch)) + geom_sf()

#plot(zones, max.plot = 14)

#library(tmap)
#
#tm_shape(zones) +
#  tm_borders() +
#  tm_facets(by = "version")
#
  

  zones$version = factor(zones$version, levels=c( "p71_01" ,    "p71_02",  "p71_03" ,"p71_04" , 
                                                  "p71_05"  , "p71_06"  , "p71_07", "p71_08",
                                                  "p71_09", "p71_10","p71_11" , "p71_12",    
                                                    "p71_14" , "p81_5Jun20", "calb_81_10"))
  
  
p <- ggplot() +
  geom_sf(data = zones, aes(fill = inch), size = 0.001, color = NA) +
  facet_wrap(~version, ncol = 8) +
  scale_fill_viridis()  + theme(axis.title.x=element_blank(),
                               axis.text.x=element_blank(),
                               axis.ticks.x=element_blank()) +
  
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(2, "in"))

p
ggsave("test.pdf", dpi = 300, width = 22.5, height = 13, units = "in") 
z <- ggplotly(p)

htmlwidgets::saveWidget(as_widget(z), "test2.html")
