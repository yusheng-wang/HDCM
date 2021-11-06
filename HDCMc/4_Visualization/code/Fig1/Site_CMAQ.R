###################################################################
###################################################################
source("./R/PSTVB_Packages.R")
data("CMAQ_PM25", package = "stBase")
data("GeoMap", package = "stBase")
data("SiteData", package = "stBase")
file = "./figure/"
###################################################################
###################################################################
Site <- setDF(Site)
Site$CITY <- as.character(Site$CITY)
Site_label <- plyr::ddply(Site[, c(2, 4:5)],
                          .(CITY),
                          plyr::summarize,
                          LON = mean(LON),
                          LAT  =  mean(LAT),
                          .progress = "text")
Site_label$LAT[5] <- Site_label$LAT[5] - 0.3
Site_label$LAT[12] <- Site_label$LAT[12] + 0.1
Site_label$LAT[3] <- Site_label$LAT[3] - 0.3
Site_label$LAT[7] <- Site_label$LAT[7] - 0.4
Site_label$LON[7] <- Site_label$LON[7] - 0.3
Site_label$LAT[11] <- Site_label$LAT[11] - 0.4

size = c(10, 12, 3)
###################################################################
#                       Figure 2 (a)
###################################################################
{
  p1 <- ggmap::ggmap(GoogleMap_BTH) +
    geom_polygon(data = Map_BTH,
                 aes(x = long,y = lat, group = group),
                 colour = '#808080',
                 fill = NA) +
    geom_point(data = CMAQ_Site , aes(x = LON, y = LAT)
               , shape = 20, col ="#A9A9A9", size = 0.3) +
    geom_point(data = Site, aes(x = LON, y = LAT)
               , shape = 20
               , col = "red", size = 1) +
    # geom_text(aes(x = 112.2, y = 43
    #               , label =  "(a)"),
    #           # family = c("sans"),
    #           fontface = 'bold',
    #           color = "black", size = size[3] + 2,
    # ) +
    geom_text(data = Site_label
              , aes(x = LON, y = LAT + 0.2
                    , label = CITY ), 
              color = "black", size = size[3]) +
    scale_x_continuous(limits=c(112, 121.2),
                       breaks = seq(113, 121, 2), 
                       labels = paste0(seq(113, 121, 2), "° E")) +
    scale_y_continuous(limits=c(35, 43.5),
                       breaks = seq(35, 43.5, 2),
                       labels = paste0(seq(35, 43.5, 2), "° N"))+
    xlab("longitude") + ylab("latitude") +
    theme_bw() +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      # axis.ticks = element_line(size = 0.5, colour = "black")
      # , axis.text = element_text(size = size[1], colour = "black")
      # , axis.title= element_text(size = size[2], colour = "black")
      , legend.title = element_blank()
      , legend.position = "none"
      , panel.grid.major = element_blank() 
      , panel.grid.minor = element_blank()
    )
}
ggsave(plot = p1, height = 6, width = 5, dpi = 300, file = paste0(file, '/Fig1_Site_CMAQ.png'))

