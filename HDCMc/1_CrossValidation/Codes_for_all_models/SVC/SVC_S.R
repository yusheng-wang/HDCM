rm(list=ls())
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
  # c(201506, 201507, 201508) 
  # c(201511, 201512, 201601) 
  MODE_BASE_TABLE <- Model_Base_Table_Update %>% 
    filter(YEAR_MONTH %in% c(201506, 201507, 201508) 
           # , MONTH %in% c(12)
    ) %>% setorder(DATE_TIME, SITEID) 
  setorderv(MODE_BASE_TABLE, c("CITY", 'SITEID', 'DATE_TIME'))
  setnames(MODE_BASE_TABLE, "REAL_PM25", "PM25")
  City.Name <- as.character(unique(MODE_BASE_TABLE$CITY))
}
Covariate <- c("CMAQ_PM25_30" 
              # , "REAL_LON_WIND"
              # , "REAL_TEMP"
              # , "REAL_PRES"
              # , "REAL_DEWP"#
              # , "REAL_LAT_WIND"
             );
# ###################################################################
# #                           2. Model
# ###################################################################
colnames(MODE_BASE_TABLE)
setDT(MODE_BASE_TABLE)
Tab_data <- MODE_BASE_TABLE
setDF(MODE_BASE_TABLE)
Cova <- which(colnames(MODE_BASE_TABLE) %in% Covariate)
MODE_BASE_TABLE[, c("CMAQ_PM25_30", "REAL_IRAIN", "ALTITUDE", "REAL_PRES")]=
  sqrt(MODE_BASE_TABLE[,  c("CMAQ_PM25_30", "REAL_IRAIN", "ALTITUDE", "REAL_PRES")])

if(length(Covariate)>1){
for(k in 1:length(Covariate))
{
  MODE_BASE_TABLE[, Cova[k]] = scale(as.vector(
    MODE_BASE_TABLE[, Cova[k]]))[, 1]
}}

###################################################################
#                  SVC
###################################################################
{
  p <- length(Covariate) + 1
  # set parameters 
  {
    n.samples <- 1e4
    
    starting <- list("phi"=1e-2, "sigma.sq" = 1
                     , "tau.sq"=1, 
                     'beta.starting'= c(2, 0.5, rep(0, p - 2)))
    tun = 0.1
    tuning <- list("phi"=tun, "sigma.sq"=tun
                   , "tau.sq" = tun,'beta'=c(tun,tun))
    
    priors.1 <- list("beta.Norm"=list(rep(0, p), diag(1e5,p)),
                     "phi.Unif" = c(1/2e2, 1/1e1),
                     "sigma.sq.IG"=c(2, 1),
                     "tau.sq.IG" = c(2, 1))
    
    cov.model <- "exponential"
    
    n.report <- 1000
    verbose <- F
  }
 
  city_num <- 1:13
  setDT(MODE_BASE_TABLE)
  Nt <- 92
  Ens <- ceil(0.5*n.samples)
  t <- 0
  colNames <- c("CITY", "CITY_NAME", "STATION_NAME",
                "LON", "LAT", "DATE_TIME", "YEAR_MONTH",
                "YEAR","MONTH","DAY","PM25","SITEID",
                "True_REAL_PM25","Miss_Flag")
  for(City in city_num)
  {
   
    year_range <- unique(MODE_BASE_TABLE$YEAR)
    tem <- MODE_BASE_TABLE[CITY %in% City.Name[City], "SITEID"]
    for(Year in year_range)
    {
      Base_Table <- MODE_BASE_TABLE[YEAR == Year,]
      month_range <- unique(Base_Table$MONTH)
      for(Month in month_range)
      {
        Base_Tab <- Base_Table[MONTH == Month,]
        day_range <- unique(Base_Tab$DAY)
        for(Day in day_range)
        {
          set.seed(1234)
          t = t + 1
          cat("\n\n   the ", City, "th City: ", City.Name[City], "!!!\n\n")
          cat("   year: ", Year, "; month: ", Month, "; day: ", Day, " \n\n")
          cat("...................SVC.................\n\n")
          # Database
          Da.mod <- Base_Tab[CITY %nin% City.Name[City] & DAY == Day, ]
          Da.pre <- Base_Tab[CITY %in% City.Name[City] & DAY == Day, ]
          
          setDF(Da.mod);setDF(Da.pre);
          Y <- Da.mod[, "PM25"]
          X <- as.matrix((Da.mod[, c(Covariate)]))
          
          coords <- as.matrix(Da.mod[, c("LON_X", "LAT_Y")])
          
          # model fitting
          m.1 <- spLM(sqrt(Y) ~ (X), coords = coords
                      , starting = starting, tuning = tuning
                      , priors = priors.1, cov.model = cov.model
                      , n.samples = n.samples, verbose = verbose
                      , n.report = n.report) 
          
          # View parameter estimates
          m.2 <- spRecover(m.1, start = Ens + 1, verbose = FALSE)
          beta <- round(summary(m.2$p.beta.recover.samples
          )$quantiles,2) %>% as.data.frame()
          
          beta <- as.data.frame(t(beta[, 3]))
          colnames(beta) <- c("Intercept", Covariate)

          X <- as.matrix((Da.pre[, c(Covariate)]))
         
          X <- cbind(1, X)
         
          coords <- as.matrix(Da.pre[, c("LON_X", "LAT_Y")])
          
          y.pred <- spPredict(m.2, pred.covars = X, 
                              pred.coords = coords,
                              start = Ens + 1, 
                              verbose = F)
         
          y.pred <- apply(y.pred$p.y.predictive.samples, 1, 
                          quant)
          beta$City <- City.Name[City]
          beta$Year <- Year
          beta$Month <- Month
          beta$Day <- Day
          
          PM25.U95 <-  ifelse(y.pred[3, ]< 0, 0, y.pred[3, ])
          PM25.Pred <- ifelse(y.pred[2, ]< 0, 0, y.pred[2, ])
          PM25.L25 <-  ifelse(y.pred[1, ]< 0, 0, y.pred[1, ])
          
          
          PM25.U95 <- PM25.U95^2
          PM25.Pred <- (PM25.Pred)^2
          PM25.L25 <- PM25.L25^2
          PM25.U95 <- ifelse(PM25.U95 > 800, 800, PM25.U95)
          PM25.Pred <- ifelse(PM25.Pred > 800, 800, PM25.Pred)
          PM25.L25 <- ifelse(PM25.L25 > 800, 800, PM25.L25)
          
          
          spT <- spT.validation(Da.pre$True_REAL_PM25, 
                                PM25.Pred, NULL, F)
          print(spT)
          
          # summary
          if(City == city_num[1] & Year == year_range[1]  & 
             Month == month_range[1] & Day == day_range[1])
          {
            
            SVC <- data.frame(Da.pre[, colNames]
                                    , PM25.L25 = PM25.L25
                                    , PM25.Pred = PM25.Pred
                                    , PM25.U95 = PM25.U95
            )
            Beta <- beta
            
          }else{
            SVC <- rbind(SVC, data.frame(Da.pre[, colNames]
                                            , PM25.L25 = PM25.L25
                                            , PM25.Pred = PM25.Pred
                                            , PM25.U95 = PM25.U95
                               )
            )
            Beta <- rbind(Beta, beta)
          }
          Beta <- as.data.frame(Beta)
          
          temp1 <- Validation.Group.City(SVC, 
                                         col = c("True_REAL_PM25", 
                                                 "PM25.Pred"), 
                                         by = "CITY")
          cat("\n.............................\n")
          print(temp1)
        }
         cat("\n.............................\n")
      }
    }
    temp0 <- Validation.Group.City(SVC, 
                                   col = c("True_REAL_PM25", 
                                           "PM25.Pred"), 
                                   by = "CITY")
    cat("\n.............................\n")
    print(temp0)
  }
}
save(SVC, file = paste0(file, "/SVC_S.RData"))