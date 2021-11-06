
# in winter
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
HDCMs <- NULL
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
  HDCMs <- rbind(HDCMs, data.frame(CITY = City.Name[City],
                                   Errors = as.vector(Pred.Median - test$Y_ts_true)))
  cat("city = ", City.Name[City], "\n\n")
}
HDCMs$Model <- "HDCM"
range(HDCMs$Errors, na.rm = T)
######################################################################
#  CMAQ
######################################################################
Model_Base_Table_2021 <- Model_Base_Table_Update
CMAQ <- Model_Base_Table_2021 %>% 
  filter(YEAR_MONTH %in% c(201511, 201512, 201601))%>%
  dplyr::select(CITY, REAL_PM25, CMAQ_PM25)

CMAQ$Errors <- CMAQ$CMAQ_PM25 - CMAQ$REAL_PM25
CMAQ$Model <- "CMAQ"
CMAQ <- CMAQ[, -c(2, 3)]
range(CMAQ$Errors, na.rm = T)
######################################################################
#  UK
######################################################################
load(paste0(file, "/UK_W.RData"))
UK$Errors <- UK$PM25.Pred - UK$True_REAL_PM25
UK$Model <- "UK"
UK <- UK[, c(1, 18, 19)]
mean(UK$Errors, na.rm = T)
range(UK$Errors, na.rm = T)
######################################################################
#  RF
######################################################################
load(paste0(file, "/RF_W.RData"))
RF$Errors <- RF$PM25.Pred - RF$True_REAL_PM25
RF$Model <- "RF"
RF <- RF[, c(1, 16, 17)]
mean(RF$Errors, na.rm = T)
range(RF$Errors, na.rm = T)
######################################################################
#  SVC
######################################################################
load(paste0(file, "/SVC_W.RData"))
SVC$Errors <- SVC$PM25.Pred - SVC$True_REAL_PM25
SVC$Model <- "SVC"
SVC <- SVC[, c(1, 18, 19)]
mean(SVC$Errors, na.rm = T)
range(SVC$Errors, na.rm = T)
da <- rbind(CMAQ, UK, RF, SVC, HDCMs)

da <- da[!is.na(da$Errors), ]
da <- da[abs(da$Errors) %>% between(0, thres), ]
######################################################################
#  plot
######################################################################
da$Model <- ordered(da$Model, levels = c("CMAQ", "UK",
                                         "RF", "SVC", "HDCM"))
p <- ggplot(data = da[da$Model %in%
                        c("CMAQ", "UK",
                          "RF", "SVC", "HDCM"),], aes(colour = Model, 
                                                      group = Model, 
                                                      fill = Model)) +
  geom_density(aes(Errors), alpha = 0.2, adjust = 3, size = 1)
# facet_wrap(~ LAT_label, ncol = 4
#            , labeller = labeller(LAT_label = Label)
# )    #facet_grid
p0 <- ggplot_build(p)  
winter.Residual <- p0$data[[1]]
lab <-  unique(da$Model)
winter.Residual$Model = if_else(winter.Residual$group==1, lab[1],
                                if_else(winter.Residual$group==2, lab[2],
                                        if_else(winter.Residual$group==3, lab[3],   
                                                if_else(winter.Residual$group==4, lab[4],
                                                        lab[5]))))
winter.Residual$Model <- ordered(winter.Residual$Model, levels = c("CMAQ", "UK",
                                                                   "RF", "SVC", "HDCM"))
}