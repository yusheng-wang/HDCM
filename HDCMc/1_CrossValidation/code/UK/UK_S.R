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
  MODE_BASE_TABLE <- Model_Base_Table_Update %>% 
    filter(YEAR_MONTH %in% c(201506, 201507, 201508) 
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
# fmla <- as.formula(paste0("sqrt(PM25)~", paste(Covariate, collapse = "+")))
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
###################################################################
#                           2. CMAQ
###################################################################
# CMAQ
setDT(MODE_BASE_TABLE)

colNames <- c("CITY", "CITY_NAME", "STATION_NAME",
              "LON", "LAT", "DATE_TIME", "YEAR_MONTH",
              "YEAR","MONTH","DAY","PM25","SITEID",
              "True_REAL_PM25","Miss_Flag")
MODE_BASE_TABLE$PM25 = sqrt(MODE_BASE_TABLE$PM25)
###################################################################
#                           1. Universal Kriging
###################################################################
# Universal Kriging
setDT(MODE_BASE_TABLE)
{
  for( City in 1:13)
  {
    set.seed(1234)
    year_range <- unique(MODE_BASE_TABLE$YEAR)
    # Matrix with the estimated daily covariance parameters
    cov.pars.daily <- NULL
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
          cat("   the ", City, "th City: ", City.Name[City], "!!!\n\n")
          cat("   year:", Year, "; month: ", Month, "; day: ", Day,". \n\n")
          cat("...................UK.................\n\n")
          # data preparation 
          Da.mod <- Base_Tab[CITY %nin% City.Name[City] & DAY == Day, ]
          Da.pre <- Base_Tab[CITY %in% City.Name[City] & DAY == Day, ]
          setDF(Da.mod);setDF(Da.pre)
          # This first loop is to estimate the covariance parameters of the exponential covariance
          
          # geodata object with observed daily PM concentration and daily CMAQ output
          pm.ukrig.df <- data.frame(cbind(Da.mod[, "LON_X"],
                                          Da.mod[, "LAT_Y"],
                                          (Da.mod[, "PM25"]),
                                          Da.mod[, Covariate]))
          pm.ukrig.geo <- as.geodata(pm.ukrig.df,
                                     coords.col=c(1,2), 
                                     data.col=c(3:ncol(pm.ukrig.df)))
          
          # estimation of the covariance parameters via REML
          pm.ukrig.reml <- likfit(geodata=pm.ukrig.geo,
                                  coord=pm.ukrig.geo$coords,
                                  data=pm.ukrig.geo$data[,1],
                                  trend = ~pm.ukrig.geo$data[, -1],
                                  cov.model="exponential",
                                  ini=c(1, 100.0), 
                                  nugget= 1, 
                                  fix.nug = FALSE,
                                  lik.met="REML",
                                  lambda=1)
          
          cov.pars.daily <- rbind(cov.pars.daily, 
                                  data.frame(sigmasq = pm.ukrig.reml$sigmasq,
                                             phi = pm.ukrig.reml$phi,
                                             tausq = pm.ukrig.reml$tausq))
          
          
        }
      }
    }
    cov.pars.uk <- as.numeric(apply(cov.pars.daily,2,mean))
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
          cat("   the ", City, "th City: ", City.Name[City], "!!!\n\n")
          cat("   year:", Year, "; month: ", Month, "; day: ", Day,". \n\n")
          cat("...................UK.................\n\n")
          # data preparation 
          Da.mod <- Base_Tab[CITY %nin% City.Name[City] & DAY == Day, ]
          Da.pre <- Base_Tab[CITY %in% City.Name[City] & DAY == Day, ]
          # Da.mod$PM25 = (Da.mod$PM25)
          # Da.pre$PM25 = log(Da.pre$PM25)
          setDF(Da.mod);setDF(Da.pre)
          # Da.mod$CMAQ_PM25_30 = (Da.mod$CMAQ_PM25_30)
          # Da.pre$CMAQ_PM25_30 = (Da.pre$CMAQ_PM25_30)
          # Da.mod <- Da.mod[, c(Covariate, "PM25", "LON_X", "LAT_Y")]
          # Da.pre <- Da.pre[, c(Covariate, "PM25", "LON_X", "LAT_Y")]
          
          # geodata object with observed daily PM concentration and daily CMAQ output
          pm.ukrig.df <- data.frame(cbind(Da.mod[, "LON_X"],
                                          Da.mod[, "LAT_Y"],
                                          (Da.mod[, "PM25"]),
                                          Da.mod[, Covariate]))
          pm.ukrig.train.geo <- as.geodata(pm.ukrig.df,
                                     coords.col=c(1,2), 
                                     data.col=c(3:ncol(pm.ukrig.df)))
          pm.ukrig.test.df <- data.frame(cbind(Da.pre[, "LON_X"],
                                               Da.pre[, "LAT_Y"],
                                          (Da.pre[, "PM25"]),
                                          Da.pre[, Covariate]))
          pm.ukrig.test.geo <- as.geodata(pm.ukrig.test.df,
                                     coords.col=c(1,2), 
                                     data.col=c(3:ncol(pm.ukrig.test.df)))
          
          
          # Specifying all the options for Universal Kriging
          kc.uk.control <- krige.control(type.krige="ok",
                                         trend.d=~pm.ukrig.train.geo$data[, -1],
                                         trend.l=~pm.ukrig.test.geo$data[, -1],
                                         cov.model="exponential",
                                         cov.pars=c(cov.pars.uk[1],cov.pars.uk[2]),
                                         nugget=cov.pars.uk[3],
                                         lambda=1)
          
          # Predicting at test sites via Universal Kriging
          pred.uk.day <- krige.conv(pm.ukrig.train.geo, 
                                    coords=pm.ukrig.train.geo$coords,
                                    data=pm.ukrig.train.geo$data[,1],
                                    locations=pm.ukrig.test.geo$coords
                                    ,krige=kc.uk.control)
          
          new.pred.uk.day <- ifelse(pred.uk.day$predict<0, 0, pred.uk.day$predict)
          new.pred.uk.day = (new.pred.uk.day)^2
          new.pred.uk.day <- ifelse(new.pred.uk.day > 800, 800, new.pred.uk.day)
          
          
          # new.pred.uk.day <- (as.numeric(pred.uk.day$predict))^2
          new.low.bd.pred.uk.day <- new.pred.uk.day+qnorm(0.025)*sqrt(as.numeric(pred.uk.day$krige.var))*new.pred.uk.day
          new.upp.bd.pred.uk.day <- new.pred.uk.day+qnorm(0.975)*sqrt(as.numeric(pred.uk.day$krige.var))*new.pred.uk.day
          
          # m <- gstat::vgm(cov.pars.uk[1], model = "Exp", 
          #                 range = cov.pars.uk[2], 
          #                 nugget = cov.pars.uk[3])
          
          
          # m <- gstat::vgm(50, model = "Exp", range = 1e-6, nugget = 0.11)
          
          # Kriging.Fit <- gstat::krige(#formula = fmla
          #                             PM25~ CMAQ_PM25_30 +
          #                             REAL_LON_WIND +
          #                             REAL_PRES + REAL_TEMP +
          #                             REAL_DEWP + REAL_LAT_WIND
          #                             , data = Da.mod 
          #                             , locations =~ LON_X + LAT_Y
          #                             , model = m 
          #                             , newdata = Da.pre
          #                             , nmax = 40)
          
          # PM25.Pred <- ifelse(Kriging.Fit$var1.pred<0, 0, Kriging.Fit$var1.pred)
          PM25.Pred <- new.pred.uk.day
          
          
          
          # Kriging.Fit
          if(City == 1 & Year == year_range[1]  & Month == month_range[1] & Day == day_range[1])
          {
            
            UK <- data.frame(Da.pre[, colNames], PM25.Pred = PM25.Pred,
                                    low.Pred = new.low.bd.pred.uk.day,
                                    upp.Pred = new.upp.bd.pred.uk.day
            )
          }else{
            UK <- rbind(UK
                               , data.frame(Da.pre[, colNames], PM25.Pred = PM25.Pred,
                                            low.Pred = new.low.bd.pred.uk.day,
                                            upp.Pred = new.upp.bd.pred.uk.day  
                               )
            )
          }
          cat(".....................UK....................\n\n")
        }
      }
    }
  }
}
setDT(UK)
save(UK, file = paste0(file, "/UK_S.RData"))
