rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse) 
library(sf) 
library(rmapshaper)
library(rgdal)

#nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
#  transmute(NAME, geometry) %>% #keeps just the county column for simplicity
#  ms_simplify(keep = 0.01) #reduces the number of vertices for simplicity
#head(nc)
#plot(nc)


basins <- readOGR(".", "cnrfc_basins_10112019_wgs84_thin_0.8_ret") %>% 
          st_as_sf(check_ring_dir = TRUE) %>%
          transmute(NAME = Basin, geometry) %>% 
          st_cast("MULTIPOLYGON") # instead of Polygon
#head(basins)
#plot(basins)
#enforce again check_ring_dir argument of sf::st_read()
basins <- st_read(basins, check_ring_dir = T)

{
#extract coordinates from sf
coord    <- st_coordinates(basins) %>% 
            as.data.frame() %>% mutate(X = sprintf('%.6f',X)) %>% 
            mutate(Y = sprintf('%.6f',Y) )%>%
            group_by(L3) %>% 
            mutate(L4 = row_number() )
#head(coord)
#extract data from sf
polygons <- st_drop_geometry(basins) %>% 
            mutate(NAME = as.character(NAME) ) %>%
            rownames_to_column(var = "id") %>% 
            mutate(id = as.numeric(id) ) %>%
#join coordinates
           left_join( coord, by = c("id" = "L3") )
#head(polygons)

#split polygons-dataframe to list

l <- split( polygons, f = polygons$id )
#head(l)

#extract text needed from each polygon
result <- lapply( l, function(x) {
  paste0 ( paste0( unique( x$NAME ), ", AUTO\n" ),
           paste0( "  ", x$X, "       ", x$Y, collapse = "\n" ),
           "\nEND" )
                                  }
                 )
}

writeLines( unlist(result), "basins_10112019_80percentretained.dat")


