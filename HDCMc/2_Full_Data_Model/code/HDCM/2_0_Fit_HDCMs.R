rm(list=ls())
source("./R/PSTVB_Packages.R")
data("SiteData", package = "stBase")
data("GeoMap", package = "stBase")
Model_Base_Table_2021 <- Model_Base_Table_Update
######################################################################
#                   1. Create grid
######################################################################
grid <- CreateGrid(Site,
                   max.edge = c(.35, .65), #0.3,0.7
                   offset = c(1e-1, 0.5), #0.4, 0.6
                   cutoff = .05, #0.1
                   col = "black", size = 1)
grid$plot.grid
######################################################################
#                    Mapping matrix: H
######################################################################
Data_Str <- CreateHmatrix(grid, method = c("indictor"), #"Gaussian, indictor, INLA
                          site = Site, factor = 1, 
                          cs = .15,  distance = F,
                          distance.method = "geodetic:km" #geodetic:1000km
)
######################################################################
#                             Data truncation
######################################################################
# 201506,201507,201508 # 201511, 201512, 201601
YearMonth <- c(201506, 201507, 201508)
Model_Base_Table_2021[,
                      c("CMAQ_PM25_30", "REAL_RAIN", "ALTITUDE", "REAL_PRES")]=
  sqrt(Model_Base_Table_2021[,
                             c("CMAQ_PM25_30", "REAL_RAIN", "ALTITUDE", "REAL_PRES")])
Yts_Xts <- ParYtsXts(Model_Base_Table_2021, include = list(
  YearMonth = YearMonth),
  X = c("CMAQ_PM25_30"
        , "REAL_LON_WIND", "REAL_TEMP"
        , "REAL_PRES", "REAL_DEWP", "REAL_LAT_WIND"
  )
  # ,Hs = Data_Str$Hs,
  # Z_ts = sqrt(Z_ts[4,,])
)
train <- list(H = Data_Str$Hs,
              Y_ts = Yts_Xts$Y_ts,
              X_ts = Yts_Xts$X_ts)

save(train, file = paste0("./2_Full_Data_Model/data/", "train_HDCM2_S.RData"))
######################################################################
#                         Model set
######################################################################

######################################################################
{
  # Yts_Xts$X_ts[2,,] <- sqrt(Yts_Xts$X_ts[2,,])
  p1 = dim(Yts_Xts$X_ts)[1]
  p2 = 3
  ######################################################################
  #                           Prior
  ######################################################################
  prior <- list(
    beta = list(betaX.mu = rep(0, p1),
                betaX.Sigma2 = 1e5*diag(1, p1, p1),
                betaZ.mu = rep(0, p2),
                betaZ.Sigma2 = 1e5*diag(1, p2, p2))
    , alpha = list(mu = c(0), Sigma2 = 1e5)
    , Obs.tau2 = list(a = 2, b = 1)
    , Proc.tau2 = list(a = 2, b = 1)
    , Proc0.tau2 = list(a = 2, b = 1)
    , theta1 = list(mu = 1e-2, Sigma2 =  1e5)
  )
  ######################################################################
  #                        initialize  parameters
  ######################################################################
  para <- list(
    beta = list(E_betaX = c(2.5, 0.5, rep(0, p1-2)), 
                E_betaZ = rep(0, p2))
    , alpha = list(E_alpha = c(1))
    , theta1 = list(E_theta1 = 1e-2, Sigma2 = 1)
    , k = list(E_k = 1, a = 1e-1, b = 2e1)
    , k0 = list(E_k0 = 1, a = 1e-1, b = 2e1)
    , theta2 = list(E_theta2 = 0.1, a = 1e-2, b = 0.1) #.5*max(Data_Str$BAUs.Dist)
    , Obs.tau2 = list(E_tau2 = 1, a = 2, b = 1)
    , Proc.tau2 = list(E_tau2 = 1, a = 2, b = 1)
    , Proc0.tau2 = list(E_tau2 = 1, a = 2, b = 1)
  )
}

######################################################################
#                           fit model and prediction
######################################################################
ds <- min(Data_Str$BAUs.Dist[row(Data_Str$BAUs.Dist)!= col(Data_Str$BAUs.Dist)])
# assign("tau2",  NULL, envir = .GlobalEnv)
library(profvis)
CV_T_Dist_s <- spMixCall(Tab = "Fit_HDCM2_S",
                         Site = Site, Yts_Xts = Yts_Xts, 
                         Data_Str = Data_Str,
                         prior = prior, para = para, 
                         Total = T, Database = TRUE,
                         parallel = TRUE, verbose.VB = TRUE,
                         verbose = TRUE, Object = "CITY",
                         cs = 0.4, ct = 1, tol.vb = 1e-5,
                         tol.real = 1e-3, itMax = 5e1, 
                         Obj.Seq = c(1:13))

