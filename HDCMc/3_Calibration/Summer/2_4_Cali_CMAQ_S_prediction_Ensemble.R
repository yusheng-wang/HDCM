# rm(list=ls())
###################################################################
source("./R/PSTVB_Packages.R")
data("SiteData", package = "stBase")
data("GeoMap", package = "stBase")
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
CMAQ_Cali <- Cali_Data_Inf$CMAQ_Cali
CMAQ_Cali$Cali.pred <- 0
CMAQ_Cali$W_ts <- 0
CMAQ_Cali$sd1 <- 0
CMAQ_Cali$sd2 <- 0
setDT(CMAQ_Cali)
para_est <- Re$PIU
###################################################################
###################################################################

n <- nrow(Cali_Data_Inf$H)
n.Enseble <- dim(Re$Ks$EnXs)[3]
############################################################
############################################################
CMAQ_ID <- CMAQ_Cali[CMAQ_Cali$T_index == 1, ]$CMAQ_ID
N <- length(unique(CMAQ_ID))
############################################################
beta <- array(0, dim = c(length(para_est$beta$E_betaX), n, n.Enseble),
              dimnames = list(c(paste0("X", 1:length(para_est$beta$E_betaX))),
                              1:n,
                              paste0("En.", 1:n.Enseble)
              ))
# y.pred.sample <- W_ts.sample <- array(0, dim = c(Cali_Data_Inf$Nt, n, n.Enseble),
#               dimnames = list(unique(Cali_Data_Inf$CMAQ_Cali$DATE_TIME),
#                               1:n,
#                               paste0("En.", 1:n.Enseble)
#               ))
W_ts.sample <- matrix(0, nrow = n, ncol = n.Enseble)
y.pred.var <- array(0, dim = c(Cali_Data_Inf$Nt, N, n.Enseble),
                    dimnames = list(unique(Cali_Data_Inf$CMAQ_Cali$DATE_TIME),
                                    unique(CMAQ_ID),
                                    paste0("En.", 1:n.Enseble)
                    ))
Y.pred.sd <- distinct(CMAQ_Cali[, c(4, 5, 8, 17)])
Y.pred.sd$sd1 <- Y.pred.sd$sd2 <- -1
s.e <- seq_len(n.Enseble)
s.n <- seq_len(n)
H <- as( Cali_Data_Inf$H, "sparseMatrix")
############################################################
#                        by each ensemble 
############################################################ 
t1 <- proc.time()
for(t in 1:length(date.index$T_index)) #Cali_Data_Inf$Nt
{
  # system.time({
  # W <- apply(Cali_Data_Inf$H, 1, FUN = '%*%', Re$Ks$EnXs[t + 1, ,]) %>% t()
  # })
  
  # system.time({
  W <- sapply(s.e, function(s.e) H %*% Re$Ks$EnXs[date.index$T_index[t] + 1, , s.e],
              simplify = "array")
  # })
  for(i in s.e){
    W_ts.sample[, i] <- as.vector(W[[i]])
  }
  
  beta.sample <- mvnfast::rmvn(n * n.Enseble,
                               mu = para_est$beta$E_betaX, 
                               sigma = para_est$beta$betaX.Sigma2)
  # beta[1,,] <- matrix(beta.sample[, 1], nrow = n, ncol = n.Enseble)
  # beta[2,,] <- matrix(beta.sample[, 2], nrow = n, ncol = n.Enseble)
  
  for(i in 1:length(para_est$beta$E_betaX)){
    if(nrow(beta.sample) == n.Enseble){
      beta[i,,] <- t(matrix(beta.sample[, i], nrow = n.Enseble, ncol = n))  
    }else{
      beta[i,,] <- matrix(beta.sample[, i], nrow = n, ncol = n.Enseble)
    }
  }
  
  betaX.sample <- sapply(s.n, function(s.n) 
    t(XX_ts[, s.n, t]) %*% beta[, s.n, ], 
    simplify = "array")
  betaX.sample <- Matrix::t(betaX.sample[1, , ])
  # a = betaX[1, , 2]
  # b = t(Cali_Data_Inf$X_ts[, 2, t]) %*% beta[, 2, 1:100]
  # all.equal(as.vector(a),as.vector(b))
  y.pred.sample  <-  (betaX.sample + W_ts.sample + 
                        rnorm(n*n.Enseble, 0, 
                              sqrt(para_est$Obs.tau2$E_tau2)))^2
  
  ############################################################
  ############################################################
  # w <- rowMeans(W_ts.sample)
  CMAQ_Cali[T_index == t, ("W_ts"):= rowMeans(W_ts.sample)]
  # CMAQ_Cali[T_index == t, .(W_ts)]
  
  # y <- rowMeans(y.pred.sample[t, ,])
  CMAQ_Cali[T_index == t, ("Cali.pred"):= rowMeans(y.pred.sample)]
  # CMAQ_Cali[T_index == t, .(Cali.pred)]
  ############################################################
  ############################################################
  # y <- y.pred.sample[t, ,]
  y <- data.table(CMAQ_ID = CMAQ_ID, y.pred.sample) %>% gather(
    key = "Ensemble",       
    value = "PM25" ,   
    - CMAQ_ID 
  ) %>% setDT()
  Var.semble <- y[, lapply(.SD, mean), by = c("CMAQ_ID", "Ensemble")]
  Var.semble <- Var.semble[, -2]
  Var.semble <- Var.semble[, lapply(.SD, sd), by = CMAQ_ID][, 2] %>% as.data.frame()
  Y.pred.sd[T_index == t, ("sd2"):= Var.semble$PM25]
  
  y <- y[, -2]
  y <- y[, lapply(.SD, sd), by = CMAQ_ID][, 2] %>% as.data.frame()
  Y.pred.sd[T_index == t, ("sd1"):= y$PM25]
  
  
  
  
  
  # Y.pred.sd[T_index == t, .(sd)]
  # W <- data.table(CMAQ_ID = CMAQ_ID, w) %>% gather(
  #             key = "Ensemble",       
  #             value = "Wts" ,   
  #             - CMAQ_ID 
  #           ) %>% setDT()
  # W <- W[, -2]
  # W <- W[, lapply(.SD, mean), by = CMAQ_ID]
  
  
  # CMAQ_Cali[CMAQ_Cali$T_index == t, ]$W_ts = 
  ############################################################
  
  # CMAQ_Cali[CMAQ_Cali$T_index == t, ]$Cali.pred = Y[, lapply(.SD, mean), by = CMAQ_ID]
  ############################################################
  cat("time: ", as.character(DATE_TIME[t]), "\n")
  ############################################################
}
t2 <- proc.time()
cat("Runing time from HDCM model: \n")
print(t2 - t1)
cat("\n ........................ \n") 
############################################################
############################################################  
setDT(CMAQ_Cali)
CMAQ_Cali_PM25 <-  CMAQ_Cali %>% filter(YEAR_MONTH %in% YearMonth,
                                        DAY %in% c(Day))%>%
  ddply(.(CMAQ_ID, LON, LAT, DATE_TIME, 
          YEAR, MONTH, DAY, YEAR_MONTH, T_index)
        , .fun = plyr::summarize
        , CMAQ_PM25.1 = mean(CMAQ_PM25_30, na.rm = TRUE)
        , CMAQ_PM25 = mean(Cali.pred, na.rm = TRUE)
        , W_ts = mean(W_ts, na.rm = TRUE)
        , .progress = "text") %>% left_join(Y.pred.sd, 
                                            by = c("LON", "LAT",
                                                   "CMAQ_ID", 
                                                   "T_index"))
CMAQ_Cali_PM25$Method <- 1
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
CMAQ_PM25_Choose$Method = 2
setDT(CMAQ_PM25_Choose)
range(CMAQ_Cali_PM25$CMAQ_PM25)
range(CMAQ_PM25_Choose$CMAQ_PM25)

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

Da$Date = paste0("Jun", " ",
                 day(Da$DATE_TIME), ", ",
                 "2015")
PM2.5$Date = paste0("Jun", " ",
                    day(PM2.5$DATE_TIME), ", ",
                    "2015")

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
y.max <- 2e2#max(Da$CMAQ_PM25, PM2.5$REAL_PM25)
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
ggsave(plot = p1, paste0(paste0("./figure/"), 'FigS3_Cali',
                         ".pdf"), dpi = 500, width = 9, height = 4)

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
ggsave(plot = p1, paste0(paste0("./figure/"), 'FigS4_S_wts.ens',
                         ".pdf"), dpi= 500, width  = 9, height = 2.5)
############################################################################
#                       sd  distribution
############################################################################
y.min <- floor(min(CMAQ_Cali_PM25$sd2))
y.max <- ceil(max(CMAQ_Cali_PM25$sd2))
int <- 2e0
{
  Label <- as_labeller(c(`1` = "Standard deviation", `2` = "CMAQ"))
  CMAQ_Cali_PM25$Date <- paste0("Jun", " ", 
                                day(CMAQ_Cali_PM25$DATE_TIME), ", ",
                                "2015")
  p1 <- ggplot() +
    geom_tile(data = CMAQ_Cali_PM25, 
              aes(x = LON, y = LAT,
                  color =  sd2, 
                  fill =  sd2)) +
    geom_point(data = CMAQ_Cali_PM25#[day(Da$DATE_TIME)==18,]
               , aes(x = LON, y = LAT
                     , color = sd2
                     , fill =  sd2
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
ggsave(plot = p1, paste0(paste0("./figure/"), 'FigS5_S_sd.ens',
                         ".pdf"), dpi= 500, width  = 9, height = 2.5)
