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
  # scale_y_continuous(limits=c(15, 54))+
  theme_bw() + #coord_fixed() +
  labs(x =  "longitude", y = "latitude") +
  theme( axis.ticks = element_line(size = 1, colour = "black")
         , axis.text = element_text(size = 14, colour = "black", face = "bold")
         , axis.title= element_text(size = 16, colour = "black", face = "bold")
         # , axis.title.x = element_text(vjust = -5.5)
         , legend.title = element_blank()
         , legend.position = "none"
         , panel.grid.major = element_blank() 
         , panel.grid.minor = element_blank()
  )
ggsave(plot = china_map, paste0("./figure/", 'Fig1_China.jpg'), dpi = 800)
##############################################################################
##############################################################################
City <- CPM.Other[CPM.Other$NAME %in% c(
                                # "北京市", 
                                "唐山市",
                                # "邢台市", 
                                "张家口市",
                                "保定市"), c(9, 11:12)] %>% 
  distinct() %>% setnames(c("经度", "纬度"),
                          c("LON", "LAT"))
City$Label <- c(
                # "Xingtai",
                "Baoding", 
                "Zhangjiakou",
                "Tangshan"
                # ,"Beijing"
                )
##############################################################################
# ggmap(map) +
BTH_City <- ggplot() + 
  geom_polygon(data = CCM[CCM$NAME_1 %in% c("Beijing", "Hebei", "Tianjin"),],
               aes(x = long, y = lat, group = group
               ), fill = "transparent"
               , colour = "white", size = .1) +
  coord_map("polyconic") +
  geom_polygon(data = Map_BTH,
               aes(x = long,y = lat, group = group),
               colour = 'black',
               fill = "#F5F5F5") +
  geom_point(data = City, aes(x = LON, y = LAT)
             , shape = 17
             , col = "red", size = 2) +
  geom_text(data = City
            , aes(x =  ifelse(
              Label %in% c("Zhangjiakou"), LON -0,
              ifelse(Label %in% c("Baoding"), LON - 0,
                     LON - 0.10)), 
              y = ifelse(
                Label %in% c("Zhangjiakou"), LAT - 0.42,
                ifelse(Label %in% c("Baoding"), LAT - 0.2,
                       LAT - 0.18))
              , label = Label
              , angle = ifelse(
                Label %in% c("Zhangjiakou"), 30,
                ifelse(Label %in% c("Baoding"), 0, 335))
            ), 
            # family = c("sans"),
            color = "black", size = 4) +
  scale_x_continuous(limits=c(113.2, 120),
                     breaks = seq(113, 121, 2), 
                     labels = paste0(seq(113, 121, 2), "° E")) +
  scale_y_continuous(limits = c(36, 42.9),
                     breaks = seq(35, 42, 2),
                     labels = paste0(seq(35, 42, 2), "° N"))+
  xlab("longitude") + ylab("latitude") +
  theme_bw() +
  theme(
    axis.ticks = element_line(size = 1, colour = "black")
    , axis.text = element_text(size = 14, colour = "black", face = "bold")
    , axis.title= element_text(size = 16, colour = "black", face = "bold")
    , legend.title = element_blank()
    , legend.position = "none"
    , panel.grid.major = element_blank() 
    , panel.grid.minor = element_blank()
  )
##############################################################################
ggsave(plot = BTH_City, paste0("./figure/", 'Fig1_BTH_City.jpg'), dpi = 800)
##############################################################################












# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(data = CPM,
#        aes(x = long, y = lat, 
#            group = group)) + geom_polygon(
#              aes(fill = as.character(Flag)
#              )
#              # , fill = "gray"
#              , colour  = "black"
#              , size = .5) +
#   coord_map("polyconic") + 
#   scale_fill_manual(values = c("red", "white")) +
#   scale_x_continuous(limits=c(73, 136)) + 
#   scale_y_continuous(limits=c(15, 54))+
#   theme_bw() + #coord_fixed() +
#   labs(x =  "longitude", y = "latitude") +
#   theme( axis.ticks = element_line(size = 1, colour = "black")
#          , axis.text = element_text(size = 14, colour = "black", face = "bold")
#          , axis.title= element_text(size = 16, colour = "black", face = "bold")
#          # , axis.title.x = element_text(vjust = -5.5)
#          , legend.title = element_blank()
#          , legend.position = "none"
#          , panel.grid.major = element_blank() 
#          , panel.grid.minor = element_blank()
#   )
