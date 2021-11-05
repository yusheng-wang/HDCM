######################################################################
######################################################################
load_HDCM_fun <- function(file, tab)
{
  if(length(tab) == 4){
    load(paste0(file, "/data/", tab[1], "_", 
                tab[2], "_", tab[3], "_",
                tab[4], "_Summary.RData"))
  }else{
    load(paste0(file, "/data/", tab[1], 
                tab[2], "_", tab[3], "_Summary.RData"))
  }
  
  ######################################################################
  ######################################################################
  method <- "ensemble"
  
  Y.test <- test.HDCM(test = test, Ks = Ks)
  HDCM.test <- (apply(Y.test, c(1, 2), quant))
  ######################################################################
  ######################################################################
  Pred.L25 <- HDCM.test[1,,] %>% as.data.frame()
  Pred.Median <- HDCM.test[2,,] %>% as.data.frame()
  Pred.U95 <- HDCM.test[3,,] %>% as.data.frame()
  Pred.Mean <- (apply(Y.test, c(1, 2), mean)) %>% as.data.frame()
  
  Err <- spT.validation(test$Y_ts_true, as.matrix(Pred.Median), NULL, T)
  print(Err)
  
  Pred.L25$DATE_TIME <- rownames(Pred.L25) %>% as.Date()
  Pred.Median$DATE_TIME <- rownames(Pred.Median) %>% as.Date()
  Pred.U95$DATE_TIME <- rownames(Pred.U95) %>% as.Date()
  Pred.Mean$DATE_TIME <- rownames(Pred.Mean) %>% as.Date()
  
  ######################################################################
  ######################################################################
  REAL_PM25 <- test$Y_ts_true %>% as.data.frame()
  REAL_PM25$DATE_TIME <- rownames(REAL_PM25) %>% as.Date()
  
  REAL_PM25 <- gather(
    data = REAL_PM25,      #待转换的数据集名称
    key = "SITEID",       #转换后的分类字段名称（维度）
    value = "REAL_PM25" ,    #转换后的度量值名称
    -DATE_TIME 
  ) 
  
  Pred.L25 <- gather(
    data = Pred.L25,      #待转换的数据集名称
    key = "SITEID",       #转换后的分类字段名称（维度）
    value = "Pred.L25" ,    #转换后的度量值名称
    -DATE_TIME 
  ) 
  
  Pred.Median <- gather(
    data = Pred.Median,      #待转换的数据集名称
    key = "SITEID",       #转换后的分类字段名称（维度）
    value = "Pred.Median" ,    #转换后的度量值名称
    -DATE_TIME 
  ) 
  
  Pred.U95 <- gather(
    data = Pred.U95,      #待转换的数据集名称
    key = "SITEID",       #转换后的分类字段名称（维度）
    value = "Pred.U95" ,    #转换后的度量值名称
    -DATE_TIME 
  ) 
  
  Pred.Mean <- gather(
    data = Pred.Mean,      #待转换的数据集名称
    key = "SITEID",       #转换后的分类字段名称（维度）
    value = "Pred.Mean" ,    #转换后的度量值名称
    -DATE_TIME 
  ) 
  ######################################################################
  ######################################################################
  HDCM2.Ens <- Pred.L25 %>% left_join(Pred.Median, by = c("SITEID", "DATE_TIME")) %>% 
                            left_join(Pred.U95, by = c("SITEID", "DATE_TIME")) %>% 
                            left_join(REAL_PM25, by = c("SITEID", "DATE_TIME"))%>% 
                            left_join(Pred.Mean, by = c("SITEID", "DATE_TIME"))
  HDCM2.Ens$CITY <- CITY.Name
  HDCM2.Ens$Model <- "HDCM2.Ens"
  # save(HDCM2.Ens, file = paste0(file, "/data/HDCM2.Ens_", CITY.Name, 
  #                              ifelse(min(month) %in% c(201511, 201512, 201601),
  #                                     "_W", "_S"), ".RData"))
 return(HDCM2.Ens)
}


