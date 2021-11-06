# HDCM
Codes for the paper: “Efficient and Effective Calibration of Numerical Model Outputs Using Hierarchical Dynamic Models” by Y. Chen, X. Chang, B. Zhang, and H. Huang. 

There are two parts to our codes: 
1. Our two algorithms, the VB and the EnKs, were written into the stBase package in the R statistical environment;
2. A project entitled ``HDCMc'' in the Rstudio environment was built to reproduce all the results (e.g., figures and tables) in this work. 

```
# Require core package
1. R >= 4.1.1
2. Rcpp >= 1.0.7
2. gpuR >= 2.0.3
```

The stBase package depends on the gpuR package that allows our codes to run on the GPU platform and hence further speeds up the calibration procedure. The source codes and its installation instructions for gpuR can now be found in a GitHub repository https://github.com/cdeterman/gpuR. One can also download the gpuR package here: https://cran.r-project.org/src/contrib/Archive/gpuR/.


```
# An example for fitting HDCM 
rm(list=ls())
# remove.packages("stBase")
# install.packages("./package/stBase_1.0.zip", repos = NULL, type = "win.binary")
source("./R/PSTVB_Packages.R")
data("SiteData", package = "stBase")
data("GeoMap", package = "stBase")
##########################################################################################
#              1. Create triangulated mesh of the BTH region
##########################################################################################
grid <- CreateGrid(Site, max.edge = c(.35, .65), offset = c(1e-1, 0.5),
                   cutoff = .05, col = "black", size = 1)
grid$plot.grid
```
![](./HDCMc/figure/Fig4.png)
```
##########################################################################################
#                       2. Mapping matrix: H
##########################################################################################
Data_Str <- CreateHmatrix(grid, method = c("indictor"), #"Gaussian kernel, indictor, INLA
                           site = Site, factor = 1, 
                           cs = .15,  distance = F,
                           distance.method = "geodetic:km" #geodetic:1000km
                          )
##########################################################################################
#                          3. Data for modeling 
##########################################################################################
YearMonth <- c(201511, 201512, 201601)

# do transformation for some variables 
Model_Base_Table_Update[, c("CMAQ_PM25_30", "REAL_RAIN", "ALTITUDE", "REAL_PRES")]=
  sqrt(Model_Base_Table_Update[, c("CMAQ_PM25_30", "REAL_RAIN", "ALTITUDE", "REAL_PRES")])
# construct datasets  
Yts_Xts <- ParYtsXts(Model_Base_Table_Update, 
                     include = list(YearMonth = YearMonth),
		     X = c("CMAQ_PM25_30", "REAL_LON_WIND", "REAL_TEMP",
			   "REAL_PRES", "REAL_DEWP", "REAL_LAT_WIND"))
##########################################################################################
#-----------------------------------------------------------------------------------------
#                         4. Model setting
##########################################################################################
{
  # Yts_Xts$X_ts[2,,] <- sqrt(Yts_Xts$X_ts[2,,])
  p1 = dim(Yts_Xts$X_ts)[1]
  p2 = 0
  #---------------------------------------------------------------------
  #                           4.1 Prior
  #---------------------------------------------------------------------
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
  #---------------------------------------------------------------------
  #                        4.2 initialize  parameters
  #---------------------------------------------------------------------
  para <- list(
    beta = list(E_betaX = c(2.5, 0.5, rep(0, p1-2)), 
                E_betaZ = rep(0, p2))
    , alpha = list(E_alpha = c(1))
    , theta1 = list(E_theta1 = 1e-2, Sigma2 = 1)
    , k = list(E_k = 1, a = 1e-1, b = 2e1)
    , k0 = list(E_k0 = 1, a = 1e-1, b = 2e1)
    , theta2 = list(E_theta2 = 0.1, a = 1e-2, b = 0.1) 
    , Obs.tau2 = list(E_tau2 = 1, a = 2, b = 1)
    , Proc.tau2 = list(E_tau2 = 1, a = 2, b = 1)
    , Proc0.tau2 = list(E_tau2 = 1, a = 2, b = 1)
  )
}
##########################################################################################
#                           Model fitting and prediction
##########################################################################################
ds <- min(Data_Str$BAUs.Dist[row(Data_Str$BAUs.Dist)!= col(Data_Str$BAUs.Dist)])
library(profvis)
CV_T_Dist_W <- spMixCall(Tab = "HDCM_W", Site = Site, Yts_Xts = Yts_Xts, 
            		Data_Str = Data_Str, prior = prior, para = para, 
                         Total = F, Database = FALSE, parallel = TRUE, 
			 verbose.VB = TRUE, verbose = TRUE, Object = "CITY",
            		 cs = 0.4, ct = 1, tol.vb = 1e-5, tol.real = 1e-3, 
			 itMax = 5e1, Obj.Seq = c(1:13))

```
