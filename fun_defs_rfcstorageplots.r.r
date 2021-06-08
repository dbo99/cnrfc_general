#Created on Aug 16 10:43:42 2018

### Function Definitions ###

#####################################
### year and month classifications ##
#####################################

water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}

prast_mon_ts_kaf <- function(df) {
  minyr <- min(df$wy)
  maxyr <- max(df$wy)
  res <- df$name
  
  ggplot(df, aes(x=wy, y = -wm,  fill = kaf))+#, color = ScenWYTSJR_txt))# +
    geom_raster()+ 
    #theme_black3() + 
    scale_y_continuous(expand = c(0.04, 0.04),
                       breaks = c(-1, -2,-3,-4,-5,-6,-7,-8,-9,-10, -11, -12), sec.axis = dup_axis(name = NULL) , 
                       #labels = c( "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug", "Sep"))+
                       labels = c( "O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"))+
    scale_x_continuous(breaks = c(1920, 1930, 1940,1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
                       labels = c("'20", "'30", "'40","'50", "'60", "'70", "'80", "'90", "'00", "'10", "'20"),
                       expand = c(0.01, 0.01))+
    scale_fill_viridis( name = "kaf") + 
    theme(strip.text.y = element_text(angle = 0))+
    #facet_grid(scen~dv) +
    theme(legend.key.width = unit(0.5, "in"), legend.key.height = unit(0.35, "in")) +
    labs(x ="water year", y = NULL) +
    ggtitle(paste0(res, ", End of Month Storage (", minyr, " - ", maxyr,")" )) 
}
#p1 <- prast_mon_ts_kaf(df3)
#p1
#ggplotly(p1)

p_mon_excd_kaf <- function(df) {
  minyr <- min(df$wy)
  maxyr <- max(df$wy)
  res <- df$name
  df  %>% filter(wm == 8) %>% arrange( (kaf)) %>% mutate(kaf_dv_rank = row_number(),
                                                         excdxaxis = kaf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = kaf)) +
    # + theme_black3() +
    geom_point() + labs(x = "percentile", y = "kaf")+theme_gray() +
    guides(colour = guide_legend(override.aes = list(size=1))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    # scale_color_manual(values = df_cols) +
    gghighlight(wy == 2021, ) + scale_x_continuous(expand = c(0.01, 0.01), breaks = c(0, 0.025, 0.10, 0.25, 0.5, 0.75, 1.00),
                              labels = c("0", "0.025",  "0.10", "0.25", "0.5", "0.75", "1.00")) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
    ggtitle(paste0( res, ", End of May Storage (", minyr, " - ", maxyr,")" ))    }

#p2 <- p_mon_excd_kaf(df3)
#p2
plot_both <- function(df) {
  res <- head(df,1) 
  name <- res$name
  res <- res$res
  p1 <- prast_mon_ts_kaf(df)
  p2 <- p_mon_excd_kaf(df)
  plot_grid(p1, p2, ncol = 1)
  
  ggsave(paste0(res,"_",name,".jpeg"), dpi = 300, width = 17, height = 11, units = "in") }


p1ly <- function(df) {
  res <- head(df,1) 
  name <- res$name
  res <- res$res
  p1 <- prast_mon_ts_kaf(df)
  p1 <- ggplotly(p1)
  htmlwidgets::saveWidget(as_widget(p1), paste0(res,"_",name,".html")) }

p_mon_excd_kaf_ly <- function(df) {
  
  minyr <- min(df$wy)
  maxyr <- max(df$wy)
  res <- df$name
  df2021 <- df %>% filter(wy == 2021,wm == 8) %>% arrange( (kaf)) %>% mutate(kaf_dv_rank = row_number(),
                                                                             excdxaxis = kaf_dv_rank/(n()+1))
  df  %>% filter(wm == 8) %>% arrange( (kaf)) %>% mutate(kaf_dv_rank = row_number(),
        excdxaxis = kaf_dv_rank/(n()+1)) %>% 
    ggplot(aes(x = excdxaxis, y = kaf, color = wy)) + scale_color_viridis() +
    # + theme_black3() +
    geom_point() + labs(x = "percentile", y = "kaf")+theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=1))) +
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    # scale_color_manual(values = df_cols) +
    #gghighlight(wy == 2021 ) +

    ggtitle(paste0("End of May, ", res, ", Storage (", minyr, " - ", maxyr,")" ))    }
#p_mon_excd_kaf_ly(df3)
p2ly <- function(df) {
  res <- head(df,1) 
  name <- res$name
  res <- res$res
  p2 <- p_mon_excd_kaf_ly(df)
  p2 <- ggplotly(p2)
  htmlwidgets::saveWidget(as_widget(p2), paste0(res,"_",name,"_EndofMay.html")) }

pspag <- function(df) {
  res <- head(df,1) 
  name <- res$name
  res <- res$res
  minyr <- min(df$wy)
  maxyr <- max(df$wy)
  
ggplot(df, aes(x = wm, y = kaf, group = wy)) + 
  geom_line() + gghighlight(wy == 2021) + 
  scale_x_continuous(expand = c(0.01, 0.01), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("O", "N" , "D" , "J", "F", "M", "A", "M", "J","J", "A", "S")) + labs(x = "month")+
  ggtitle(paste0(name, ", Storage (", minyr, " - ", maxyr,")" )) + scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
ggsave(paste0(res,"_",name,"_spaghetti.jpeg"), dpi = 300, width = 17, height = 11, units = "in") }
#pspag(df3)


pspag2 <- function(df) {
  res <- head(df,1) 
  name <- res$name
  res <- res$res
  minyr <- min(df$wy)
  maxyr <- max(df$wy)
  
  ggplot(df, aes(x = wm, y = kaf, group = wy)) + 
    geom_line() + gghighlight(wy == 2021) + 
    scale_x_continuous(expand = c(0.01, 0.01),breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("O", "N" , "D" , "J", "F", "M", "A", "M", "J","J", "A", "S")) + 
    labs(x = "month")+ scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
    ggtitle(paste0(name, ", End of Month Storage (", minyr, " - ", maxyr,")" ))}
  #ggsave(paste0(res,"_",name,"_spaghetti.jpeg"), dpi = 300, width = 17, height = 11, units = "in") }

pspag2ly <- function(df) {
  res <- head(df,1) 
  name <- res$name
  res <- res$res
  minyr <- min(df$wy)
  maxyr <- max(df$wy)
  
  p <- ggplot(df, aes(x = wm, y = kaf, group = wy, color = wy)) + 
    geom_line() + 
    #gghighlight(wy == 2021) + 
    scale_x_continuous(expand = c(0.01, 0.01),breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("O", "N" , "D" , "J", "F", "M", "A", "M", "J","J", "A", "S")) + 
    labs(x = "month")+ scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
    ggtitle(paste0(name, ", End of Month Storage (", minyr, " - ", maxyr,")")) +
                     scale_color_viridis()
  p1 <- ggplotly(p)
  htmlwidgets::saveWidget(as_widget(p1), paste0(res,"_",name,"_EndofMonStors.html")) }
#ggsave(paste0(res,"_",name,"_spaghetti.jpeg"), dpi = 300, width = 17, height = 11, units = "in") }

plot_three <- function(df) {
  res <- head(df,1) 
  name <- res$name
  res <- res$res
  p1 <- prast_mon_ts_kaf(df)
  p2 <- pspag2(df) 
  p3 <- p_mon_excd_kaf(df) 
  plot_grid(p1, p2,p3, rel_heights = c(1.5,1,1.5),   ncol = 1)
  
  ggsave(paste0(name,"_",res,".jpeg"), dpi = 300, width = 13.333, height = 7.5, units = "in") }
