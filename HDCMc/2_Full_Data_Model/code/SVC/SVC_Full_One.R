rm(list=ls())
source("./R/PSTVB_Packages.R")
source("./R/BranchFun.R")
library(sp)
library(rgdal)
library(spBayes)
##################################################################
###################################################################
#                           2. Data loading
###################################################################
Year_time  = 2015
Month_time = 06
Day_time <- 19:24
###################################################################
{
  load("./data/Model_Base_Table_Update.RData")
  # load("./5_1_Generate_Data/BaseTable/Model_2015_2017_Spline_Tab.Rda")
  setDF(Model_Base_Table_Update)
  load("./data/Site.RData")
  Model_Base_Table_Update$True_REAL_PM25 <- Model_Base_Table_Update$REAL_PM25
  Model_Base_Table_Update$REAL_PM25 = if_else(is.na(Model_Base_Table_Update$REAL_PM25)
                                              , Model_Base_Table_Update$NA.Kriging
                                              , Model_Base_Table_Update$REAL_PM25)
  # c(201506, 201507, 201508) 
  # c(201511, 201512, 201601) 
  MODE_BASE_TABLE <- Model_Base_Table_Update %>% 
    filter(YEAR %in% c(Year_time) 
           , MONTH %in% c(Month_time)
           , DAY %in% c(Day_time)
    ) %>%
    setorder(SITEID) 
  setorderv(MODE_BASE_TABLE, c("CITY", 'SITEID', 'DATE_TIME'))
  setnames(MODE_BASE_TABLE, "REAL_PM25", "PM25")
  City.Name <- as.character(unique(MODE_BASE_TABLE$CITY))
  setDT(MODE_BASE_TABLE)
}
Covariate = c("CMAQ_PM25_30");

Tab_data <- MODE_BASE_TABLE
setDF(MODE_BASE_TABLE)
Cova <- which(base::colnames(MODE_BASE_TABLE) %in% Covariate)
MODE_BASE_TABLE[, c("CMAQ_PM25_30", "REAL_IRAIN", "ALTITUDE", "REAL_PRES")]=
  sqrt(MODE_BASE_TABLE[,  c("CMAQ_PM25_30", "REAL_IRAIN", "ALTITUDE", "REAL_PRES")])

if(length(Covariate)>1){
  for(k in 1:length(Covariate))
  {
    MODE_BASE_TABLE[, Cova[k]] = scale(as.vector(
      MODE_BASE_TABLE[, Cova[k]]))[, 1]
  }}


setDT(MODE_BASE_TABLE)
###################################################################
###################################################################
# load("./CMAQ_PM25_Test.Rda")
load("./ModelProcess/3_Calibration/Data/3_0_2_Cali_Data.RData")

Cali.Data <- Cali_Data %>% 
  filter(YEAR %in% c(Year_time) 
         , MONTH %in% c(Month_time)
         , DAY %in% c(Day_time)
  )
setDT(Cali.Data)
###################################################################
###################################################################
p = length(Covariate) + 1
# set parameters 
{
  
  n.samples <- 1e4
  
  starting <- list("phi"=1e-2, "sigma.sq" = 1
                   , "tau.sq"=1, 
                   'beta.starting'= c(2, 0.5, rep(0, p - 2)))
  tun = 0.1
  tuning <- list("phi"= 1e-3, "sigma.sq"=tun
                 , "tau.sq" = tun,'beta'=c(tun,tun))
  
  priors.1 <- list("beta.Norm"=list(rep(0, p), base::diag(1e5, p)),
                   # "phi.Unif" = c(1e-3, 5e-1),
                   "phi.Unif" = c(1/2e5, 1/1e2),#c(1/5e3, 1/1e0),  #c(1/2e5, 1/1e3) c(1km, 200km)
                   "sigma.sq.IG"=c(2, 1),
                   "tau.sq.IG" = c(2, 1))
  
  cov.model <- "exponential"
  n.thin <- 2
  n.report <- 5000
  verbose <- F
}
###################################################################
###################################################################

Base_Tab <- MODE_BASE_TABLE[MONTH == Month_time,]
day_range <- unique(Base_Tab$DAY)
t01 <- proc.time()
for(Day in day_range)
{
  set.seed(1234)
  cat("   年份:", Year_time, ";月份: ", Month_time, "; 第", Day,"天. \n\n")
  cat("...................M4.................\n\n")
  # Database
  Da.mod <- Base_Tab[DAY == Day, ]
  Da.pre <- Cali.Data[DAY == Day, ]
  
  t1 <- proc.time()
  setDF(Da.mod);setDF(Da.pre);
  
  
  Y = Da.mod[, "PM25"]
  X = as.matrix((Da.mod[, c(Covariate)]))
  
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
  burn.in <- 0.5 * n.samples
  m.2 <- spRecover(m.1, start = burn.in, verbose = FALSE)
  t2 <- proc.time()
  cat("SVC runing time in fitting step: \n")
  print(t2 - t1)
  cat("\n ........................ \n")
  # par(mfrow = c(2, 1))
  # plot(as.numeric(m.2[["p.theta.recover.samples"]][,1]))
  # plot(as.numeric(m.2[["p.theta.recover.samples"]][,2]))
  # round(summary(m.1$p.theta.recover.samples)$quantiles,2)
  t3 <- proc.time()
  X = as.matrix(sqrt(Da.pre[, c(Covariate)]))
  X = cbind(1, X)

  coords <- as.matrix(Da.pre[, c("LON_X", "LAT_Y")])

  y.pred <- spPredict(m.2, pred.covars = X, 
                      pred.coords = coords,
                      start = burn.in + 1, 
                      verbose = F, thin = n.thin)
  t4 <- proc.time()
  cat("SVC runing time in predictive step: \n")
  print(t4 - t3)
  cat("\n ........................ \n")
  y.pred <- apply(y.pred$p.y.predictive.samples, 1, 
                  quant)
  
  PM25.Pred <- ifelse(y.pred[2, ]< 0, 0, y.pred[2, ])
  PM25.Pred = (PM25.Pred)^2
  PM25.Pred <- ifelse(PM25.Pred > 800, 800, PM25.Pred)

  # 统计结果
  if(Day == day_range[1])
  {
    Predictor <- data.frame(Da.pre, PM25.Pred = PM25.Pred)
  }else{
    Predictor <- rbind(Predictor, data.frame(Da.pre, PM25.Pred = PM25.Pred)
    )
  }
}
t10 <- proc.time()
cat("SVC runing time in total: \n")
print(t10 - t01)
setDT(Predictor)
save(Predictor, file = "./ModelProcess/3_Calibration/Data/3_11_Cali_spBayes_S.RData")


