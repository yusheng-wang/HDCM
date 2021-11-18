source("./R/PSTVB_Packages.R")
data("SiteData", package = "stBase")
# data("GeoMap", package = "stBase")
######################################################################
#      1.  Calibration from Dec, 17, 2015 to Dec, 22, 2015
######################################################################
File.source = "./3_Calibration/"
t1 <- proc.time()
#source(paste0(File.source, "Winter/2_0_Fit_HDCMw.R"))
t2 <- proc.time()
print(t2 - t1)
cat("\n ........................ \n")
File.source = "./3_Calibration/"
source(paste0(File.source, "Winter/2_1_Cali_CMAQ_W_build_data.R"))
source(paste0(File.source, "Winter/2_2_Cali_CMAQ_W_build_H.R"))
# source(paste0(File.source, "Winter/2_3_Cali_CMAQ_W_prediction.R"))


######################################################################
######################################################################
#                    run time for calibration
t1 <- proc.time()
source(paste0(File.source, "Winter/2_3_Cali_CMAQ_W_prediction_Ensemble.R"))
t2 <- proc.time()
print(t2 - t1)
######################################################################
Label <- as_labeller(c(`1` = "After calibration", 
                       `2` = "Before calibration"))

# library(STRbook)
y.max <- max(Da$CMAQ_PM25, PM2.5$REAL_PM25)
int <- 1e2
######################################################################
######################################################################
{
  p1 <- ggplot() +
    geom_tile(data = Da, 
              aes(x = LON, y = LAT,
                  color =  CMAQ_PM25, 
                  fill =  CMAQ_PM25)) +
    geom_point(data = Da#[day(Da$DATE_TIME)==18,]
               , aes(x = LON, y = LAT
                     , color = CMAQ_PM25
                     , fill =  CMAQ_PM25
               )
               , size = 0.6, pch = 20) +
    geom_point(data = PM2.5#[day(DATE_TIME)==18]
               , aes(x = LON, y = LAT
                     , color = REAL_PM25
                     , fill =  REAL_PM25
               )
               , size = 1.5, pch = 18) +
    facet_grid(Method ~ Date
               , labeller = labeller(Method = Label), switch="both") +
    scale_fill_distiller(palette = "Spectral", 
                         name = expression(atop(atop(textstyle("PM"[2.5]),
                                                     textstyle(paste("(", mu, g, "/", m^{3},")"))), NA)), 
                         limits=c(0, y.max),
                         breaks = c(seq(0, y.max, int)),
                         labels = c(seq(0, y.max, int))
    ) +
    # scale_colour_distiller(palette = "Spectral", 
    #                        name = "PM25", 
    #                        limits=c(0, 700),
    #                        guide = "colourbar", 
    #                        breaks =  seq(0, y.max, 1.5e2),
    #                        labels =  seq(0, y.max, 1.5e2)) + 
    col_scale(name = c("PM25"), limits = c(0, y.max)) +
    scale_x_continuous(limits = c(113.5, 120),
                       breaks = seq(113.5, 120, 2), 
                       labels = paste0(seq(113.5, 120, 2), "° E")) +
    scale_y_continuous(limits = c(36, 42.6),
                       breaks = seq(36, 42.6, 2),
                       labels = paste0(seq(36, 42.6, 2), "° N"))+
    theme_bw() +
    theme(
      axis.text = element_blank()
      , axis.title = element_blank()
      , axis.ticks = element_blank()
      # rect = element_blank()
      # axis.text = element_text(size = 12, colour = "black")
      #     , axis.text.x = element_text(angle=90, hjust=1)
      #     , axis.title = element_text(size = 14, colour = "black")
      , legend.title = element_text(size = 10, colour = "black")
      , legend.text = element_text(size = 10, colour = "black")
      , legend.key.width = unit(8.0,"line")
      , legend.key.height = unit(1,"line")
      , panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , legend.position = "bottom"
      , legend.background = element_rect("transparent")
      , legend.margin= margin(t = 10, 0, 0, 0)
      # , legend.box.margin= margin(t=10,0,0,0)
      , legend.box.spacing = margin(t = -5,0,0,0)
      , strip.text = element_text(size = 10, colour = "black")
    ) + guides(col = FALSE)
  
}
ggsave(plot = p1, file = "./figure/Fig7.pdf", width = 9, height = 4)