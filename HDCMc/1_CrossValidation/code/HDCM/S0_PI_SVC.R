
# load(paste0(file, "./data/", tab[1], tab[2], ".RData"))
# setDT(SVC)
# SVC1 <- SVC[CITY%in% CITY.Name, ]
# load(paste0("./ModelProcess/1_Cross_Validation/CV/SVC/Predictor/SVC2_",
#             tab, ".RData"))
load_SVC_fun <- function(file, tab){
  load(paste0(file, "/data/", tab[1], "_", tab[2], ".RData"))
  setDT(SVC)
  SVC <- SVC[CITY%in% tab[3], ]
  
  # Err1 <- spT.validation(setDF(SVC1[, "True_REAL_PM25"]), 
  #                        setDF(SVC1[, "PM25.Pred"]),
  #                        NULL, F)
  # print(Err1)
  
  Err <- spT.validation(setDF(SVC[, "True_REAL_PM25"]), 
                         setDF(SVC[, "PM25.Pred"]),
                         NULL, F)
  print(Err)
  
  # SVC1$Model <- "SVC1";
  # SVC1$Pred.Mean <- SVC1$PM25.Pred
  
  SVC$Model <- "SVC"
  SVC$Pred.Mean <- SVC$PM25.Pred
  
  
  # SVC1 <- SVC1[, c("DATE_TIME" , "SITEID", "PM25.L25", "PM25.Pred",
  #                  "PM25.U95", "PM25", "Pred.Mean", "CITY", "Model")]
  SVC <- SVC[, c("DATE_TIME" , "SITEID", "PM25.L25", "PM25.Pred",
                   "PM25.U95", "PM25", "Pred.Mean", "CITY", "Model")]
  
  colnames(SVC) <- c("DATE_TIME", "SITEID", "Pred.L25",
                                        "Pred.Median", "Pred.U95", "REAL_PM25",
                                        "Pred.Mean", "CITY","Model")
  # save(SVC, file = paste0(file, "./data/", to, ".RData"))
  return(SVC)
}
