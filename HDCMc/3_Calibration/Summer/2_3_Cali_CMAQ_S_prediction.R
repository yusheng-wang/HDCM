# rm(list=ls())
###################################################################
###################################################################
source("./R/PSTVB_Packages.R")
data("SiteData", package = "stBase")
data("GeoMap", package = "stBase")
data("CMAQ", package = "stBase")
Model_Base_Table_2021 <- Model_Base_Table_Update
###################################################################
###################################################################
Year <- as.numeric(substr(YearMonth, 1, 4))
Month <- as.numeric(substr(YearMonth, 5, 6))
DATE <- unique(Model_Base_Table_2021[YEAR_MONTH %in% 
                   c(201506, 201507, 201508), .(DATE_TIME)]) %>% as.data.frame()
date.index <- data.frame(T_index = 1:length(DATE$DATE_TIME), 
                         DATE_TIME = DATE$DATE_TIME,
                         YEAR = year(DATE$DATE_TIME),
                         MONTH = month(DATE$DATE_TIME),
                         DAY = day(DATE$DATE_TIME)) %>% 
  dplyr::filter(YEAR %in% Year, MONTH %in% Month, DAY %in% Day)
###################################################################
###################################################################
###################################################################
file = "./2_Full_Data_Model/data/"
load(paste0(file, "./Cali_HDCM_S.RData"))
Tab <- list.files(file)
HDCM <- Tab[grepl(paste0("Fit"), Tab) & grepl(paste0("_S_"), Tab)]
load(paste0(file, HDCM[1]))
###################################################################
###################################################################
XX_ts <- Cali_Data_Inf$X_ts
XX_ts[2,,] <- matrix(sqrt(as.vector(Cali_Data_Inf$X_ts[2,,])),
                                  nrow = nrow(Cali_Data_Inf$X_ts[2,,]), 
                                  ncol = Cali_Data_Inf$Nt)
###################################################################
# Cali.fit <- matrix(NA, nrow =  GSD_Other$Nt, ncol = GSD_Other$CMAQ_N)
CMAQ_Cali <- as.data.frame(Cali_Data_Inf$CMAQ_Cali)
CMAQ_Cali$Cali.fit <- 0
CMAQ_Cali$W_ts <- 0
para_est <- Re$PIU
###################################################################
###################################################################
t1 <- proc.time()
W_ts <- matrix(0, nrow =  Cali_Data_Inf$Nt, ncol = Cali_Data_Inf$CMAQ_N)
# by expectation of ensemble
Xt_ts <- X_ts_Transf(Cali_Data_Inf$Nt, XX_ts, 
                    para_est$beta$E_betaX)
for(t in 1:length(date.index$T_index))#1:Cali_Data_Inf$Nt
{
  W_ts[t, ] <- Cali_Data_Inf$H %*% Re$Ks$Xs[date.index$T_index[t] + 1, ]
 
  Cali.fit <-  Xt_ts[t, ] +  W_ts[t, ]  + rnorm(nrow(Cali_Data_Inf$H), 
                            0, sqrt(Re$PIU$Obs.tau2$E_tau2))
  CMAQ_Cali[CMAQ_Cali$T_index == t, ]$Cali.fit = Cali.fit^2
  CMAQ_Cali[CMAQ_Cali$T_index == t, ]$W_ts = W_ts[t, ]
  cat("time: ", as.character(DATE_TIME[t]), "\n")
}
t2 <- proc.time()
cat("Runing time from HDCM model: \n")
print(t2 - t1)
###################################################################
###################################################################
###################################################################
setDT(CMAQ_Cali)
CMAQ_Cali_PM25 <-  CMAQ_Cali %>% filter(YEAR_MONTH %in% YearMonth,
                                        DAY %in% c(Day))%>%
            ddply(.(CMAQ_ID, LON, LAT, DATE_TIME, 
                       YEAR, MONTH, DAY, YEAR_MONTH, T_index )
                   , .fun = plyr::summarize
                   , CMAQ_PM25.1 = mean(CMAQ_PM25_30, na.rm = TRUE)
                   , CMAQ_PM25 = mean(Cali.fit, na.rm = TRUE)
                   , W_ts = mean(W_ts, na.rm = TRUE)
                   , Mean.W_ts = mean(W_ts, na.rm = TRUE)
                   , .progress = "text"
            )
CMAQ_Cali_PM25$Method = 1
range(CMAQ_Cali_PM25$CMAQ_PM25)
# range(CMAQ_PM25_Choose$CMAQ_PM25)

# library(PBSmapping)
# polyGrid = makeGrid(coor$LON, coor$LAT,
#                     byrow=TRUE, addSID=TRUE, 
#                     projection=NULL, 
#                     zone = NULL, 
#                     type="rectangle")
# 
# ##--- plot the grid
# plotPolys(polyGrid, density=0, projection=1)
CMAQ_PM25_Choose <- CMAQ_PM25 %>%
                    filter(YEAR_MONTH %in% YearMonth,
                           DAY %in% c(Day)) %>%
                    setorder(CMAQ_ID, DATE_TIME)
CMAQ_PM25_Choose$YEAR <- year(CMAQ_PM25_Choose$DATE_TIME)
CMAQ_PM25_Choose$MONTH <- month(CMAQ_PM25_Choose$DATE_TIME)
CMAQ_PM25_Choose$DAY <- day(CMAQ_PM25_Choose$DATE_TIME)
CMAQ_PM25_Choose$Method <- 2
setDT(CMAQ_PM25_Choose)
###################################################################
###################################################################
# index <- which(colnames(CMAQ_Cali_PM25) %in% 
#                  colnames(CMAQ_PM25_Choose))
Da <- rbind(CMAQ_Cali_PM25[, c("LON", "LAT", "DATE_TIME", "CMAQ_PM25", "Method")], 
            CMAQ_PM25_Choose[, c("LON", "LAT", "DATE_TIME", "CMAQ_PM25", "Method")])
###################################################################
#              the real spatialtemporal data
###################################################################
# load("./data/Model_Base_Table_Update.RData")
###################################################################
{
  PM25_2015_2017_CMAQ <- Model_Base_Table_2021 %>%
    filter(YEAR_MONTH %in% YearMonth,
           DAY %in% c(Day)) %>%
    setorder(SITEID, DATE_TIME) %>%
    ddply(.(CITY, YEAR_MONTH, DATE_TIME)
          , plyr::summarize
          , REAL_PM25 = mean(REAL_PM25, na.rm = T)
          , CMAQ_PM25 = median(CMAQ_PM25, na.rm = T)
          , LON = mean(LON, na.rm = T)
          , LAT = mean(LAT, na.rm = T)
    )
  
  # PM25_2015_2017_CMAQ <- Model_2015_2017_Tab %>%
  #   filter(YEAR %in% c(2015),
  #          MONTH %in% c(Month),
  #          DAY %in% c(Day)) %>%
  #   setorder(SITEID, DATE_TIME) 
  # PM25_2015_2017_CMAQ <- PM25_2015_2017_CMAQ %>%
  #   ddply(.(CITY, YEAR_MONTH, DATE_TIME)
  #         , summarize
  #         , CITY_NAME = sample(CITY_NAME, 1)
  #   ) %>% left_join(dplyr::select(PM25_2015_2017_CMAQ, 
  #                                 CITY_NAME, DATE_TIME, 
  #                                 LON, LAT, REAL_PM25)  %>%
  #                     distinct(),
  #                   by = c("CITY_NAME", "DATE_TIME"))
  
  setDT(PM25_2015_2017_CMAQ)
  #####################################################################
  R1 <- PM25_2015_2017_CMAQ
  R2 <- PM25_2015_2017_CMAQ
  # R3 <- PM25_2015_2017_CMAQ
  R1$Method <- 1
  R2$Method <- 2
  # R3$Method <- 3
  PM2.5 <- rbind(R1, R2)
}
###################################################################
###################################################################
colnames(Da)
# setnames(PM2.5, 'REAL_PM25', "PM25")
# setnames(Da, "CMAQ_PM25", "PM25")
# PM25.Cali <- rbind(Da[, c("LON", "LAT", "PM25")], 
#                    PM2.5[, c("LON", "LAT", "PM25")])

Da$Date <- paste0("Jun", " ",day(Da$DATE_TIME), ", ", "2015")
PM2.5$Date <- paste0("Jun", " ", day(PM2.5$DATE_TIME), ", ", "2015")

Da$Method <- ordered(Da$Method, levels = c(2, 1))
PM2.5$Method <- ordered(PM2.5$Method, levels  = c(2, 1))

# Label <- as_labeller(c(`1` = "HDCM", `2` = "CMAQ"))
Label <- as_labeller(c(`1` = "After calibration", 
                       `2` = "Before calibration"))
# library(ggthemes)
# library(ggplot2)
library(STRbook)
# library(data.table)
# library(dplyr)
# library(lubridate);
# library(Hmisc)
y.max <- 200#max(Da$CMAQ_PM25, PM2.5$REAL_PM25)
int <- 5e1
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
               , size = 0.2, pch = 20) +
    geom_point(data = PM2.5#[day(DATE_TIME)==18]
               , aes(x = LON, y = LAT
                     , color = REAL_PM25
                     , fill =  REAL_PM25
               )
               , size = 0.6, pch = 18) +
    facet_grid(Method ~ Date
               , labeller = labeller(Method = Label), switch = "both") +
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
    col_scale(name = c("PM25"), limits=c(0, y.max)) +
    scale_x_continuous(limits=c(113.5, 120),
                       breaks = seq(113.5, 120, 2), 
                       labels = paste0(seq(113.5, 120, 2), "° E")) +
    scale_y_continuous(limits = c(36, 42.6),
                       breaks = seq(36, 42.6, 2),
                       labels = paste0(seq(36, 42.6, 2), "° N"))+
    theme_bw() +
    theme(
      axis.text =element_blank()
      , axis.title =element_blank()
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
ggsave(plot = p1, paste0(paste0("./figure/"), 'FigS3',
                         ".pdf"), dpi = 500, width  = 9, height = 4)
############################################################################
#                       addative  bias  distribution
############################################################################
y.min <- floor(min(CMAQ_Cali_PM25$W_ts))
y.max <- ceil(max(CMAQ_Cali_PM25$W_ts))
int <- 1e0
{
  Label <- as_labeller(c(`1` = "The local additive bias", `2` = "CMAQ"))
  CMAQ_Cali_PM25$Date <- paste0("Jun", " ", 
                                day(CMAQ_Cali_PM25$DATE_TIME), ", ",
                                "2015")
  p1 <- ggplot() +
    geom_tile(data = CMAQ_Cali_PM25, 
              aes(x = LON, y = LAT,
                  color =  W_ts, 
                  fill =  W_ts)) +
    geom_point(data = CMAQ_Cali_PM25#[day(Da$DATE_TIME)==18,]
               , aes(x = LON, y = LAT
                     , color = W_ts
                     , fill =  W_ts
               )
               , size = 0.2, pch = 20) +
    # geom_point(data = PM2.5#[day(DATE_TIME)==18]
    #            , aes(x = LON, y = LAT
    #                  , color = REAL_PM25
    #                  , fill =  REAL_PM25
    #            )
    #            , size = 0.6, pch = 18) +
    facet_grid(Method ~ Date
               , labeller = labeller(Method = Label), switch="both") +
    scale_fill_distiller(palette = "Spectral", 
                         name = expression(atop(atop(textstyle("PM"[2.5]),
                                                     textstyle(paste("(", mu, g, "/", m^{3},")"))), NA)), 
                         limits = c(y.min,  y.max),
                         breaks = round(c(seq(y.min, y.max, int)), 0),
                         labels = round(c(seq(y.min, y.max, int)), 0)
    ) +
    # scale_colour_distiller(palette = "Spectral", 
    #                        name = "PM25", 
    #                        limits=c(0, 700),
    #                        guide = "colourbar", 
    #                        breaks =  seq(0, y.max, 1.5e2),
    #                        labels =  seq(0, y.max, 1.5e2)) + 
    col_scale(name = c("PM25"), limits=c(y.min, y.max)) +
    scale_x_continuous(limits=c(113.5, 120),
                       breaks = seq(113.5, 120, 2), 
                       labels = paste0(seq(113.5, 120, 2), "° E")) +
    scale_y_continuous(limits = c(36, 42.6),
                       breaks = seq(36, 42.6, 2),
                       labels = paste0(seq(36, 42.6, 2), "° N"))+
    theme_bw() +
    theme(
      axis.text =element_blank()
      , axis.title =element_blank()
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
      , strip.text = element_text(size = 9, colour = "black")
    ) + guides(col = FALSE)
}
ggsave(plot = p1, paste0(paste0("./figure/"), 'FigS4_S_Mean_Wts',
                         ".pdf"), dpi= 500, width  = 9, height = 2.5)