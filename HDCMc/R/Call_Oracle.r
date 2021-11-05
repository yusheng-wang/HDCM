######################################################################
Data_Result <- function(Table = "TEST_03", Tail = 10,
                        Site = Site, Err = 1e-3,
                        City.Num = 13, Er= F, Iter = 0)
{
  # rm(list=ls())
  City <- sort(as.character(unique(Site$CITY)))
  library(RODBC)
  library(data.table)
  DSN_01 <- odbcConnect("DSN_01", uid = "myname", pwd = "mypwd"
                        , believeNRows = FALSE, case = "toupper")
  Da <- data.frame()
  for(city in c(City.Num))
  {
    # City_Name <- paste0(City[city], "12_150km")
    Tab <- sqlTables(DSN_01, schema = "MYNAME")
    Tab <- Tab[grepl(Table, Tab$TABLE_NAME) &
                 grepl(toupper(City[city]), Tab$TABLE_NAME), 3]
    # Tab
    Result <- sqlQuery(DSN_01, paste0("select * from ", Tab), errors = F)
    if(length(Result) == 1){
      break;
    }
    Result0 <- Result
    if(Er){
      if(Iter!=0){
        Result <- Result[Result$ITER <= Iter, ]
      }else{Result <- Result[Result$ITER_LOGLIK_ERROR <= Err,]}
    }
    
    if(length(Result) > 1){
      cat("------------------------------------\n\n")
     index <- which(base::colnames(Result) %in% 
                      c("FITTING_RMSE", "TESTING_RMSE",
                        # "TESTING_CRPS",
                        # "ELAPSED", 
                        "ITER"))
     # index <- which(colnames(Result) %in% c("FITTING_RMSE",
     #                                        "HDCM_RMSE", 
     #                                        "HDCM_CRPS",
     #                                        "HDCM_CORR_AFT",
     #                                        "ELAPSED",
     #                                        "ITER"))
     library(magrittr)
     tem <- cbind(CITY = Result[,"OBJECT"], 
                  Result[, c(1:7, 9:11, 13, 15,
                             index, 16)])%>%
     setorderv("ITER")
      print(tail(tem, Tail))
      cat("------------------------------------\n\n")
      if(Er){
        if(Iter!=0){
          Result <- Result[which.max(Result$ITER), ]
        }else{Result <- Result[which.min(Result$ITER), ]}
      }else{
        Result <- Result[which.max(Result$ITER), ]
      }
   
      Da <- rbind(Da, data.frame(CITY = City[city]
                                 , FITTING_RMSE =
                                   Result[nrow(Result), c("FITTING_RMSE")]
                                 , HDCM_RMSE =
                                   # D[which.min(D$ITER)
                                   #           ,"TESTING_RMSE"]
                                   Result[nrow(Result)
                                          , c("TESTING_RMSE")]
                                 # , HDCM_MB =
                                 #   # D[which.min(D$ITER)
                                 #   #           ,"TESTING_RMSE"]
                                 #   Result[nrow(Result)
                                 #          , c("TESTING_MB")]
                                 , HDCM_NMB =
                                 #   # D[which.min(D$ITER)
                                 #   #           ,"TESTING_RMSE"]
                                   Result[nrow(Result)
                                          , c("TESTING_NMB")]
                                 , HDCM_NME =
                                 #   # D[which.min(D$ITER)
                                 #   #           ,"TESTING_RMSE"]
                                   Result[nrow(Result)
                                          , c("TESTING_NME")]
                                 , HDCM_CRPS =
                                   #   # D[which.min(D$ITER)
                                   #   #           ,"TESTING_RMSE"]
                                   Result[nrow(Result)
                                          , c("TESTING_CRPS")]
                                 , HDCM_ES =
                                   #   # D[which.min(D$ITER)
                                   #   #           ,"TESTING_RMSE"]
                                   Result[nrow(Result)
                                          , c("TESTING_ES")]
                                 , HDCM_CORR_AFT =
                                   # D[which.min(D$ITER)
                                   #           ,"TESTING_RMSE"]
                                   Result[nrow(Result)
                                          , c("CORR_AFT")]
                                 , ELAPSED = 
                                   Result[nrow(Result)
                                          , c("ELAPSED")]
                                 , ITER = 
                                   Result[nrow(Result)
                                          , c("ITER")]
      ))
    }
  }
  setorder(Da,  CITY)
  
  Da <- rbind(Da, data.frame(CITY = "AVG"
                             , FITTING_RMSE = round(mean(Da$FITTING_RMSE),3)
                             , HDCM_RMSE = round(mean(Da$HDCM_RMSE),3)
                             # , HDCM_MB = round(mean(Da$HDCM_MB),3)
                             , HDCM_NMB = round(mean(Da$HDCM_NMB),3)
                             , HDCM_NME = round(mean(Da$HDCM_NME),3)
                             , HDCM_CRPS = round(mean(Da$HDCM_CRPS),3)
                             , HDCM_ES = round(mean(Da$HDCM_ES),3)
                             , HDCM_CORR_AFT = round(mean(Da$HDCM_CORR_AFT), 3)
                             , ELAPSED = round(sum(Da$ELAPSED), 3)
                             , ITER = round(max(Da$ITER))
  ))
  odbcCloseAll()
  if(length(Result0) > 1){
    return(list(Result = Result0[, c(1:7, 9:18, 31:33)], Da = Da)) 
  }else{
    return(list(Result = NA, Da = Da))
  }
  
}
library(RODBC)
# ######################################################################
data("SiteData", package = "stBase")

Data_Result(Table = "HDCM_S", #H_S_X;NA_W_X;FNG0_WMV
            Site = Site, Err = 1e-1,
            City.Num = 1:13, Er = FS,
            Iter = 50, Tail = 5
)$Da[, c(1, 2:3, 6, 8:10)]

Data_Result(Table = "HDCM_W", #H_S_X;NA_W_X;FNG0_WMV
            Site = Site, Err = 1e-3,
            City.Num = 1:13, Er = F,
            Iter = 52, Tail = 15
)$Da[, c(1, 2:3, 6, 8:10)]
