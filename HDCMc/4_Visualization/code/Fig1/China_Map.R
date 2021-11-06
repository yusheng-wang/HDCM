data("GeoMap", package = "stBase")
# library(rgdal)
source("./R/PSTVB_Packages.R")
china_map <- ggplot() + geom_polygon(
             data = CPM
             , aes(x = long,
                 y = lat, 
                 group = group,
                 fill = as.character(Flag)
             )
             , colour  = "black"
             , size = .5) +
  geom_polygon(
    data = CPM.HKG
    , aes(x = long,
          y = lat, 
          group = group,
          fill = as.character(Flag)
    )
    , colour  = "black"
    , size = .5) +
  geom_polygon(
    data = CPM.MAC
    , aes(x = long,
          y = lat, 
          group = group,
          fill = as.character(Flag)
    )
    , colour  = "black"
    , size = .5) +
  geom_polygon(
    data = CPM.TWN
    , aes(x = long,
          y = lat, 
          group = group,
          fill = as.character(Flag)
    )
    , colour  = "black"
    , size = .5) +
  geom_polygon(
    data = CPM.Other[CPM.Other$lat <= 21,]
    , aes(x = long,y = lat, group = group)
    ,  fill = "#F5F5F5"
    , colour  = "black"
    , size = .5) + 
  coord_map("polyconic") + 
  scale_fill_manual(values = c("red", "#F5F5F5")) +
  scale_x_continuous(limits=c(73, 136)) + 
  theme_bw() + 
  theme( 
        ,axis.title=element_blank()
        ,axis.text=element_blank()
        ,axis.ticks=element_blank()
        , panel.border = element_blank()
        , legend.title = element_blank()
        , legend.position = "none"
        , panel.grid.major = element_blank() 
        , panel.grid.minor = element_blank()
      )
ggsave(plot = china_map, paste0("./figure/", 'Fig1_China.jpg'), dpi = 300)
