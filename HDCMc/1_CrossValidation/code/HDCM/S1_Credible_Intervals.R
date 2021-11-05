# rm(list=ls())
library(stBase)
data("SiteData", package = "stBase")
file = "./1_CrossValidation/"
######################################################################
######################################################################
# source(paste0(File, "S0_PI_HDCM1.R"))
# source(paste0(File, "S0_PI_HDCM1_Ens.R"))
# source(paste0(File, "S0_PI_HDCM2.R"))
source(paste0(file, "/code/S0_PI_HDCM2_Ens.R"))
source(paste0(file, "/code/S0_PI_SVC.R"))
######################################################################
######################################################################
# load(paste0(file, "Temp/HDCM1_", CITY.Name, ifelse(min(month) %in% 
#                                                      c(201511, 201512, 201601), "_W", "_S"),".RData"))
# load(paste0(file, "Temp/HDCM1.Ens_", CITY.Name, ifelse(min(month) %in% 
#                                                      c(201511, 201512, 201601), "_W", "_S"), ".RData"))
# load(paste0(file, "Temp/HDCM2_", CITY.Name, ifelse(min(month) %in% 
                                                     # c(201511, 201512, 201601), "_W", "_S"),".RData"))
load(paste0(file, "data/HDCM2.Ens_", CITY.Name, ifelse(min(month) %in% 
                                                     c(201511, 201512, 201601), "_W", "_S"), ".RData"))


