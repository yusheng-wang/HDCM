# rm(list=ls())
library(stBase)
source("./R/PSTVB_Packages.R")
file = "./2_Full_Data_Model/data/"
##################################################################
###################################################################
#                           2. Data loading
###################################################################
{
  data("SiteData", package = "stBase")
  Model_Base_Table_2021 <- Model_Base_Table_Update
  # load("./5_1_Generate_Data/BaseTable/Model_2015_2017_Spline_Tab.Rda")
  setDF(Model_Base_Table_2021)
  Model_Base_Table_2021$True_REAL_PM25 <- Model_Base_Table_2021$REAL_PM25
  Model_Base_Table_2021$REAL_PM25 = if_else(is.na(Model_Base_Table_2021$REAL_PM25)
                                            , Model_Base_Table_2021$NA.Kriging
                                            , Model_Base_Table_2021$REAL_PM25)
  # c(201506, 201507, 201508) 
  # c(201511, 201512, 201601) 
  MODE_BASE_TABLE <- Model_Base_Table_2021 %>% 
    filter(YEAR_MONTH %in% c(201511, 201512, 201601) 
           # , MONTH %in% c(12)
    ) %>% setorder(DATE_TIME, SITEID) 
  setorderv(MODE_BASE_TABLE, c("CITY", 'SITEID', 'DATE_TIME'))
  setnames(MODE_BASE_TABLE, "REAL_PM25", "PM25")
  City.Name <- as.character(unique(MODE_BASE_TABLE$CITY))
}
Covariate <- c("CMAQ_PM25_30" 
               , "REAL_LON_WIND"
               , "REAL_TEMP"
               , "REAL_PRES"
               , "REAL_DEWP"#
               , "REAL_LAT_WIND"
);
# ###################################################################
# #                           3. Model
# ###################################################################
# # M1
# {
#   Da2 <- Validation.Group.City(MODE_BASE_TABLE, col = c(41, 19), by = 1)
#   Da1 <- Validation.Group.City(MODE_BASE_TABLE, col = c(41, 40), by = 1)
#   # write_xlsx(Da, file = paste0(Save_file, M1_Result,".xlsx"))
# }
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
# da = MODE_BASE_TABLE[, Covariate]
# par(mfrow = c(3, 2))
# for(i in 1:length(Covariate)){
#   plot(density(da[,i], adjust = 5), main = Covariate[i])
# }
# da = MODE_BASE_TABLE[, Covariate]
# par(mfrow = c(3, 3))
# for(i in 1:7){
#   plot(density(da[,i], adjust = 5), main = Covariate[i])
# }
t1 <- proc.time()
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
  # load("G:/Mirror/5 Data analysis/5.2 Summary/SVCM/Code/Case4/CMAQ_PM25_Test.Rda")
  # Model
  city_num <- 1:13
  setDT(MODE_BASE_TABLE)
  Nt <- 92
  Ens <- ceil(0.5*n.samples)
  t <- 0
  colNames <- c("CITY", "CITY_NAME", "STATION_NAME",
                "LON", "LAT", "DATE_TIME", "YEAR_MONTH",
                "YEAR","MONTH","DAY","PM25","SITEID",
                "True_REAL_PM25","Miss_Flag")
  
  
  year_range <- unique(MODE_BASE_TABLE$YEAR)
  
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
        # cat("\n\n   the ", City, "th City: ", City.Name[City], "!!!\n\n")
        cat("   year:", Year, "; month: ", Month, "; day: ", Day, " \n\n")
        cat("...................SVC.................\n\n")
        # Database
        Da.mod <- Base_Tab[ DAY == Day, ]
        
        setDF(Da.mod);
        Y <- Da.mod[, "PM25"]
        X <- as.matrix((Da.mod[, c(Covariate)]))
        
        # for(k in 1:(p - 1))
        # {
        #   X[, k] = scale(as.vector(X[, k]))[, 1]
        # }
        # X = as.matrix(cbind(1, (Da.mod[, c(Covariate)]))) %>% log()
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
        )$quantiles, 2)[, 3] 
        
        w <- apply(m.2$p.w.recover.samples, 1, 
                   quantile, prob = 0.5)
        
        fitted <- cbind(1, X) %*% beta + w
        
        PM25.Pred <- ifelse(fitted < 0, 0, fitted)
        PM25.Pred <- (PM25.Pred)^2
        PM25.Pred <- ifelse(PM25.Pred > 800, 800, PM25.Pred)
        
        
        # 统计结果
        if(Year == year_range[1]  & 
           Month == month_range[1] & Day == day_range[1])
        {
          
          SVC <- data.frame(Da.mod[, colNames]
                            , PM25.Pred = PM25.Pred
          )
          
        }else{
          SVC <- rbind(SVC, data.frame(Da.mod[, colNames]
                                       , PM25.Pred = PM25.Pred
          )
          )
        }
      }
      cat("\n.............................\n")
    }
  }
}
t2 <- proc.time()
print(t2 - t1)
save(SVC, file = paste0(file, "/SVC_full_W.RData"))
