######################################################################
#   1.  Calibration from Jun, 19, 2015 to Jun, 24, 2015
######################################################################
File.source = "./3_Calibration/"
t1 <- proc.time()
source(paste0(File.source, "Summer/2_0_Fit_HDCMs.R"))
t2 <- proc.time()
print(t2 - t1)
cat("\n ........................ \n")
File.source = "./3_Calibration/"
source(paste0(File.source, "Summer/2_1_Cali_CMAQ_S_build_data.R"))
source(paste0(File.source, "Summer/2_2_Cali_CMAQ_S_build_H.R"))
# source(paste0(File.source, "Summer/2_3_Cali_CMAQ_S_prediction.R"))
source(paste0(File.source, "Summer/2_4_Cali_CMAQ_S_prediction_Ensemble.R"))

######################################################################
#      2.  Calibration from Dec, 17, 2015 to Dec, 22, 2015
######################################################################
File.source = "./3_Calibration/"
t1 <- proc.time()
source(paste0(File.source, "Winter/2_0_Fit_HDCMw.R"))
t2 <- proc.time()
print(t2 - t1)
cat("\n ........................ \n")
File.source = "./3_Calibration/"
source(paste0(File.source, "Winter/2_1_Cali_CMAQ_W_build_data.R"))
source(paste0(File.source, "Winter/2_2_Cali_CMAQ_W_build_H.R"))
# source(paste0(File.source, "Winter/2_3_Cali_CMAQ_W_prediction.R"))
source(paste0(File.source, "Winter/2_4_Cali_CMAQ_W_prediction_Ensemble.R"))
