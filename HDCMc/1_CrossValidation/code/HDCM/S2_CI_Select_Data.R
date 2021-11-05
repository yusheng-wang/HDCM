######################################################################
######################################################################
load_HDCM_SVC_data <- function(file, hdcm_tab, svc_tab){
  data("SiteData", package = "stBase")
  ######################################################################
  ######################################################################
  CMAQ_PM25 <- Model_Base_Table_2021[, c("DATE_TIME", "SITEID",
                                         "CMAQ_PM25", "REAL_PM25","CITY", 
                                         "YEAR_MONTH",
                                         "DAY")] %>%
    filter(YEAR_MONTH %in% month, DAY %in% day,
           CITY %in% tab[4]
    ) %>% dplyr::select(DATE_TIME, SITEID, CMAQ_PM25, REAL_PM25, CITY) %>%
    setnames("CMAQ_PM25", "Pred.Median")
  CMAQ_PM25$Pred.Mean <- CMAQ_PM25$Pred.Median
  CMAQ_PM25$Pred.L25 = CMAQ_PM25$Pred.L25= CMAQ_PM25$Pred.U95 = NA
  CMAQ_PM25$Model = "CMAQ"   
  
  HDCM.Ens <- load_HDCM_fun(file = file, tab = hdcm_tab)
  CMAQ_PM25 <- CMAQ_PM25 %>% setcolorder(base::colnames(HDCM.Ens))
  # colnames(CMAQ_PM2)
  ######################################################################
  ######################################################################
  HDCM.Ens <- HDCM.Ens %>%
    dplyr::filter(
      as.numeric(paste0(year(DATE_TIME), ifelse(month(DATE_TIME)<10, 
                                                paste0("0", month(DATE_TIME)),
                                                month(DATE_TIME)))) %in% month,
      day(DATE_TIME) %in% day)
  ######################################################################
  ######################################################################
  SVC <- load_SVC_fun(file, svc_tab) %>% dplyr::filter(
    as.numeric(paste0(year(DATE_TIME), ifelse(month(DATE_TIME)<10, 
                                              paste0("0", month(DATE_TIME)),
                                              month(DATE_TIME)))) %in% month,
    day(DATE_TIME) %in% day)
  
  #-----------------------------------------------------
  CMAQ_PM25.Err1 <- spT.validation(CMAQ_PM25$REAL_PM25,  
                                   CMAQ_PM25$Pred.Median, 
                                   NULL, F)
  cat("CMAQ_PM25.Err1 = \n")
  print(CMAQ_PM25.Err1)  
  
  HDCM.Ens.Err1 <- spT.validation(HDCM.Ens$REAL_PM25,  
                                   HDCM.Ens$Pred.Media, 
                                   NULL, F)
  cat("HDCM.Ens.Err1 = \n")
  print(HDCM.Ens.Err1)
  HDCM.Ens.Err2 <- spT.validation(HDCM.Ens$REAL_PM25,  
                                   HDCM.Ens$Pred.Mean, 
                                   NULL, F)
  cat("HDCM.Ens.Err2 = \n")
  print(HDCM.Ens.Err2)
  #-----------------------------------------------------
  
  # SVC1.Err1 <- spT.validation(SVC1[, "REAL_PM25"],  
  #                             SVC1[, "Pred.Median"], 
  #                             NULL, F)
  # cat("SVC1.Err1 = \n")
  # print(SVC1.Err1)
  SVC.Err1 <- spT.validation(SVC[, "REAL_PM25"],  
                              SVC[, "Pred.Median"], 
                              NULL, F)
  cat("SVC.Err1 = \n")
  print(SVC.Err1)
  
  #------------------------------------------------------------
  CMAQ_PM25 <- ddply(CMAQ_PM25
                     , .(CITY, DATE_TIME, Model)
                     , .fun = plyr::summarize
                     , REAL_PM25 = mean(REAL_PM25, na.rm = TRUE)
                     , Pred.L25 = mean(Pred.L25, na.rm = TRUE)
                     , Pred.U95 = mean(Pred.U95, na.rm = TRUE)
                     , Pred.Median = mean(Pred.Median, na.rm = TRUE)
                     , Pred.Mean = mean(Pred.Mean, na.rm = TRUE)
                     , .progress = "text"
  )
  CMAQ_PM25$RMSE <- as.vector(CMAQ_PM25.Err1[2]) %>% round(3)
  CMAQ_PM25$MAPE <- as.vector(CMAQ_PM25.Err1[4]) %>% round(3)
  CMAQ_PM25$Corr <- as.vector(CMAQ_PM25.Err1[10]) %>% round(3)
  CMAQ_PM25$FAC2 <- as.vector(CMAQ_PM25.Err1[12]) %>% round(3)
  CMAQ_PM25$CRPS <- as.vector(CMAQ_PM25.Err1[15]) %>% round(3)
  
  SVC <- ddply(SVC
                , .(CITY, DATE_TIME, Model)
                , .fun = plyr::summarize
                , REAL_PM25 = mean(REAL_PM25, na.rm = TRUE)
                , Pred.L25 = mean(Pred.L25, na.rm = TRUE)
                , Pred.U95 = mean(Pred.U95, na.rm = TRUE)
                , Pred.Median = mean(Pred.Median, na.rm = TRUE)
                , Pred.Mean = mean(Pred.Mean, na.rm = TRUE)
                , .progress = "text"
  )
  SVC$RMSE <- as.vector(SVC.Err1[2]) %>% round(3)
  SVC$MAPE <- as.vector(SVC.Err1[4]) %>% round(3)
  SVC$Corr <- as.vector(SVC.Err1[10]) %>% round(3)
  SVC$FAC2 <- as.vector(SVC.Err1[12]) %>% round(3)
  SVC$CRPS <- as.vector(SVC.Err1[15]) %>% round(3)
  
  
  HDCM.Ens <- ddply(HDCM.Ens
                     , .(CITY, DATE_TIME, Model)
                     , .fun = plyr::summarize
                     # , LON = mean(LON, na.rm = TRUE)
                     # , LAT = mean(LAT, na.rm = TRUE)
                     # , FAC2 = mean(Pred.Median/REAL_PM25, na.rm = TRUE)
                     , REAL_PM25 = mean(REAL_PM25, na.rm = TRUE)
                     , Pred.L25 = mean(Pred.L25, na.rm = TRUE)
                     , Pred.U95 = mean(Pred.U95, na.rm = TRUE)
                     , Pred.Median = mean(Pred.Median, na.rm = TRUE)
                     , Pred.Mean = mean(Pred.Mean, na.rm = TRUE)
                     , .progress = "text"
  )
  HDCM.Ens$RMSE <- as.vector(HDCM.Ens.Err1[2]) %>% round(3)
  HDCM.Ens$MAPE <- as.vector(HDCM.Ens.Err1[4]) %>% round(3)
  HDCM.Ens$Corr <- as.vector(HDCM.Ens.Err1[10]) %>% round(3)
  HDCM.Ens$FAC2 <- as.vector(HDCM.Ens.Err1[12]) %>% round(3)
  HDCM.Ens$CRPS <- as.vector(HDCM.Ens.Err1[15]) %>% round(3)
  return(list(HDCM.Ens = HDCM.Ens, SVC = SVC))
}

