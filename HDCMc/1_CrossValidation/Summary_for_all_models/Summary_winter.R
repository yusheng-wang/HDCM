library(stBase)
source("./R/PSTVB_Packages.R")
data("SiteData", package = "stBase")
file <- "./1_CrossValidation/data/"
######################################################################

######################################################################
Season <- "W"
City.Name <-  sort(as.character(unique(Site$CITY)))
Tab <- list.files(file)
if(length(Tab) == 0){
  cat("\n.............................\n")
  cat("Please first run the file \n\n 'Step1_run_all_model.R', ", "\n\n")
  cat("and then try again...", "\n\n")
  cat("\n.............................\n")
}else{
HDCMw <- NULL
######################################################################
for(City in 1:13)
{
  HDCM <- Tab[grepl(paste0("HDCM"), Tab) &
                grepl(paste0("_", Season ,"_"), Tab) &
                grepl((City.Name[City]), Tab)]
  
  
  load(paste0(file, HDCM))
  # CITY.Name <- "Beijing"
  #------------------------------------------------------------------
  Y.test <- test.HDCM(test = test, Ks = Ks, PIU = PIU, seed = 1234)
  HDCM.test <- (apply(Y.test, c(1, 2), quant))
  
  Pred.L25 <- HDCM.test[1,,] %>% as.matrix()
  Pred.Median <- HDCM.test[2,,] %>% as.matrix()
  Pred.U95 <- HDCM.test[3,,] %>% as.matrix()
  Pred.Mean <- (apply(Y.test, c(1, 2), mean)) %>% as.matrix()
  spT.validation()
  HDCMw <- rbind(HDCMw, data.frame(CITY = City.Name[City],
                                   RMSE = RMSE(test$Y_ts_true, Pred.Median), 
                                   CRPS = CRPS(test$Y_ts_true, Pred.Median),
                                   Corr = Coef(test$Y_ts_true, Pred.Median),
                                   FAC2 = FAC2(test$Y_ts_true, Pred.Median)))
  cat("city = ", City.Name[City], "\n\n")
}
######################################################################
setDT(HDCMw)
setorderv(HDCMw, c("CITY"), 1)
HDCMw <- rbind(HDCMw, data.frame(CITY = "AVG"
                                 , RMSE = round(mean(HDCMw$RMSE), 3)
                                 , CRPS = round(mean(HDCMw$CRPS), 3)
                                 , Corr = round(mean(HDCMw$Corr), 3)
                                 , FAC2 = round(mean(HDCMw$FAC2), 3))) 

# save(HDCMw, file = paste0(file, "/HDCMw.RData"))
######################################################################
#  CMAQ
######################################################################
CMAQ <- Model_Base_Table_Update %>% 
  filter(YEAR_MONTH %in% c(201511, 201512, 201601))%>%
  dplyr::select(CITY, REAL_PM25, CMAQ_PM25, YEAR_MONTH)
CMAQ <- Validation.Group.City(CMAQ, col = c("REAL_PM25", "CMAQ_PM25"), 
                              by = "CITY")
######################################################################
#  UK
######################################################################
load(paste0(file, "/UK_W.RData"))
UK <- Validation.Group.City(UK, col = c("True_REAL_PM25", "PM25.Pred"), 
                            by = "CITY")
######################################################################
#  RF
######################################################################
load(paste0(file, "/RF_W.RData"))
RF <- Validation.Group.City(RF, col = c("True_REAL_PM25", "PM25.Pred"), 
                            by = "CITY")
######################################################################
#  SVC
######################################################################
load(paste0(file, "/SVC_W.RData"))
SVC <- Validation.Group.City(SVC, col = c("True_REAL_PM25", "PM25.Pred"), 
                             by = "CITY")
######################################################################
######################################################################
#                 table 1
######################################################################
Resultw <- CMAQ[, 1:3] %>% left_join(UK[, 1:3], by = "CITY") %>%
  left_join(RF[, 1:3], by = "CITY") %>%
  left_join(SVC[, 1:3], by = "CITY") %>%
  left_join(HDCMw[, 1:3], by = "CITY") %>%
  data.table::setcolorder(c(1, 2, 4, 6, 8, 10,
                            3, 5, 7, 9, 11))
colnames(Resultw) <- c("City", "RMSE.CMAQ", "RMSE.UK",
                       "RMSE.RF", "RMSE.SVC", "RMSE.HDCM",
                       "CRPS.CMAQ", "CRPS.UK",
                       "CRPS.RF", "CRPS.SVC", "CRPS.HDCM")
######################################################################
#                            table S1
######################################################################
Resultw.SI.table <- CMAQ[, c(1, 4, 5)] %>% left_join(UK[, c(1, 4, 5)], by = "CITY") %>%
  left_join(RF[, c(1, 4, 5)], by = "CITY") %>%
  left_join(SVC[, c(1, 4, 5)], by = "CITY") %>%
  left_join(HDCMw[, c(1, 4, 5)], by = "CITY") %>%
  data.table::setcolorder(c(1, 3, 5, 7, 9, 11,
                            2, 4, 6, 8, 10 ))
colnames(Resultw.SI.table) <- c("City",
                                "FAC2.CMAQ", "FAC2.UK",
                                "FAC2.RF", "FAC2.SVC", "FAC2.HDCM",
                                "Corr.CMAQ", "Corr.UK",
                                "Corr.RF", "Corr.SVC", "Corr.HDCM")
}