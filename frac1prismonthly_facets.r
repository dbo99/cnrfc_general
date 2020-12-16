rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

{
library(raster)
library(sp)
library(rgdal)
library(tidyverse)
library(viridis)
library(ggthemes)
library(reshape)
library(naniar)
  library(sf)

ckfire <- readOGR(".", "creekfire") %>% st_as_sf() #%>% 
frac1hmf <- readOGR(".", "frac1hmf") %>% st_as_sf() #%>% 

ann <- raster("prism19812010ann.tif")
a <- raster("October.tif") 
b <- raster("November.tif")
c <- raster("December.tif")
d <- raster("January.tif")
e <- raster("February.tif")
f <- raster("March.tif")
g <- raster("April.tif")
h <- raster("May.tif")
i <- raster("June.tif")
j <- raster("July.tif")
k <- raster("August.tif")
l <- raster("September.tif")

}
s12 <- stack(a,b,c,d,e,f,g,h,i,j,k,l)
s6 <- stack(b,c,d,e,f,g)
sann <- stack(ann)

# https://datacarpentry.org/r-raster-vector-geospatial/12-time-series-raster/
df12 <- as.data.frame(s12, xy = TRUE) %>%
  melt(id.vars = c('x','y')) %>% drop_na()

df12 <- df12 %>% replace_with_na(replace = list(value <= 0))

df6 <- as.data.frame(s6, xy = TRUE) %>%
  melt(id.vars = c('x','y')) %>% drop_na()

df6 <- df6 %>% replace_with_na(replace = list(value <= 0))

dfann <- as.data.frame(sann, xy = TRUE) %>%
  melt(id.vars = c('x','y')) %>% drop_na()

dfann <- df6 %>% replace_with_na(replace = list(value <= 0))



{
p <- ggplot() +
  geom_raster(data = df12 , aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable) + scale_x_continuous(expand = c(0,0)) +
  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_fill_viridis(name = "mean in/mo") + 
 # geom_sf(data = ckfire, fill = NA, color = "red", size = .4) + 
   # geom_sf(data = frac1hmf, fill = NA, color = "black", size = .4) + 
     theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(1, "in")) 
p

}
ggsave("prism12months.jpeg", width = 13.333, height = 7.5, units = "in", dpi = 300)


p <- ggplot() +
  geom_raster(data = df6 , aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable) + scale_x_continuous(expand = c(0,0)) +
  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_fill_viridis(name = "mean in/mo") + 
  geom_sf(data = ckfire, fill = NA, color = "red", size = .4) +
  geom_sf(data = frac1hmf, fill = NA, color = "black", size = .4) + 
   theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(1, "in")) 
p


ggsave("prism6months.jpeg", width = 13.333, height = 7.5, units = "in", dpi = 300)

p <- ggplot() +
  geom_raster(data = dfann , aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable) + scale_x_continuous(expand = c(0,0)) +
  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_fill_viridis(name = "mean in/yr") + 
  geom_sf(data = ckfire, fill = NA, color = "red", size = .75) +
 # geom_sf(data = frac1hmf, fill = NA, color = "black", size = .4) + 
  theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(1, "in")) 
p


ggsave("prismann.jpeg", width = 13.333, height = 7.5, units = "in", dpi = 300)














t <- as(s, "SpatialPixelsDataFrame")
t <- as.data.frame(t)
colnames(t) <- c("value", "x", "y")






ggplot() +  
  geom_tile(data=s, aes(x=x, y=y, fill=value), alpha=0.8) + 
  #geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
   #            fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) + facet_wrap(~band)
