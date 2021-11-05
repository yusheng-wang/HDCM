source("./R/PSTVB_Packages.R")
file = "./1_CrossValidation/data/"
##################################################################
###################################################################
#                           1. Data loading
###################################################################
{
  data("SiteData", package = "stBase")
  setDF(Model_Base_Table_Update)
  Model_Base_Table_Update$True_REAL_PM25 <- Model_Base_Table_Update$REAL_PM25
  Model_Base_Table_Update$REAL_PM25 = if_else(is.na(Model_Base_Table_Update$REAL_PM25)
                                            , Model_Base_Table_Update$NA.Kriging
                                            , Model_Base_Table_Update$REAL_PM25)
  MODE_BASE_TABLE <- Model_Base_Table_Update %>% 
    filter(YEAR_MONTH %in% c(201506, 201507, 201508)
           # , MONTH %in% c(12)
    ) %>% setorder(DATE_TIME, SITEID) 
  setorderv(MODE_BASE_TABLE, c("CITY", 'SITEID', 'DATE_TIME'))
  setnames(MODE_BASE_TABLE, "REAL_PM25", "PM25")
  City.Name <- as.character(unique(MODE_BASE_TABLE$CITY))
}
Covariate = c("CMAQ_PM25_30" 
              , "REAL_LON_WIND"
              , "REAL_TEMP"
              , "REAL_PRES"
              , "REAL_DEWP"#
              , "REAL_LAT_WIND"
);
colNames <- c("CITY", "CITY_NAME", "STATION_NAME",
              "LON", "LAT", "DATE_TIME", "YEAR_MONTH",
              "YEAR","MONTH","DAY","PM25","SITEID",
              "True_REAL_PM25","Miss_Flag")
setDT(MODE_BASE_TABLE)
for(City in 1:13)
{
  set.seed(1234)
  # data
  Da.mod <- MODE_BASE_TABLE[CITY %nin% City.Name[City], ]
  Da.pre <- MODE_BASE_TABLE[CITY %in% City.Name[City], ]
  setDF(Da.mod);setDF(Da.pre)
  # Da.mod$CMAQ_PM25_30 = sqrt(Da.mod$CMAQ_PM25_30)
  # Da.pre$CMAQ_PM25_30 = sqrt(Da.pre$CMAQ_PM25_30)
  Da.mod$PM25 <- (Da.mod$PM25)
  Da.pre$PM25 <- (Da.pre$PM25)
  
  Da.pre0 <- Da.pre
  Da.mod <- Da.mod[, c(Covariate, "PM25")]
  Da.pre <- Da.pre[, c(Covariate, "PM25")]
  # model
  # Y = Da.mod[, "PM25"]
  # X = as.matrix(cbind( sqrt(Da.mod[, c(Covariate)])))
  # mtry <- tuneRF(Da.mod[, -7], Da.mod$PM25, ntreeTry=500,
  #                stepFactor= 1.5,improve=0.01, 
  #                trace=TRUE, plot=TRUE)
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  rf <- randomForest((PM25)~., data = Da.mod, importance=TRUE, ntree = 500) 
  # summary(rf)
  # test
  
  PM25.Pred <- predict(rf, newdata = Da.pre[, -which(base::colnames(Da.pre) == "PM25")]) %>% as.vector()
  PM25.Pred <- PM25.Pred
  PM25.Pred <- ifelse(PM25.Pred > 800, 800, PM25.Pred)
  
  if(City == 1)
  {
    RF <- data.frame(Da.pre0[, colNames], PM25.Pred = PM25.Pred)
  }else{
    RF <- rbind(RF, data.frame(Da.pre0[, colNames], PM25.Pred = PM25.Pred)
    )
  }
  cat("....................RF..................\n\n")
  
  cat("       the ", City, "th City：", City.Name[City], "\n\n")
  cat("....................RF..................\n\n")
  spT <- spT.validation(Da.pre0[, "True_REAL_PM25"],
                        PM25.Pred, NULL, F)
  print(spT)
  # cat("........................................\n\n")
} 
save(RF, file = paste0(file, "RF_S", ".RData"))
