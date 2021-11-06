# rm(list=ls())
library(stBase)
###########################################################################
YearMonth <- c(201512)
Day <- 17:22#27:31#
###########################################################################
file <- "./2_Full_Data_Model/data/"
###########################################################################
source("./R/PSTVB_Packages.R")
# 1. randomly generate U spatial points around s by binary uniform distribution
data("SiteData", package = "stBase")
data("CMAQ_PM25", package = "stBase")
data("GeoMap", package = "stBase")
cmaq_site <- spCoords.transform(as.data.frame(CMAQ_Site[, -c(4, 5)]), 
                                col = c("LON", "LAT"), 
                                method = 2) %>% setorderv("CMAQ_ID", 1)

# save(cmaq_site, file = "./data/cmaq_site.RData")
# coor <- cmaq_site[, 2:3]
r1 <- range(abs(diff(cmaq_site$LON)))[1]  #0.0026855
r2 <- range(abs(diff(cmaq_site$LAT)))[1]  #0.08362579
###########################################################################
###########################################################################

U <- 50
Predict.Location <- NULL
for(s in 1:nrow(cmaq_site))
{
  Predict.Location <- rbind(Predict.Location, data.frame(sample.id  = (U*(s - 1) + 1):(U*(s)),
                                 sample.lon = runif(U, cmaq_site$LON[s] - r1, cmaq_site$LON[s] + r1), 
                    sample.lat = runif(U, cmaq_site$LAT[s] - r2, cmaq_site$LAT[s] + r2),
                    LON = cmaq_site$LON[s], LAT = cmaq_site$LAT[s],
                    LON_X = cmaq_site$LON_X[s], LAT_Y = cmaq_site$LAT_Y[s],
                    CMAQ_ID = cmaq_site$CMAQ_ID[s]))
  if(s %% 500 == 0){
    cat("s = ", s, "\n")
  }
}

# plot(Predict.Location[Predict.Location$Grid.Bs == 1, 1:2])
# points(cmaq_site[1, 2:3], col = "red")

# 2. transform the coordinate
Predict.Location.trans <- spCoords.transform(Predict.Location, 
                                             col = c("sample.lon", "sample.lat"),
                                             colname = c("sLON_X", "sLAT_Y"), 
                                             method = 2)

# 3. solve covariate Xt(s) by invser distance weight average from 30km

# load("./data/Model_Base_Table_Update.RData")
# PM25_2015_2017_CMAQ <- Model_Base_Table_Update %>%
#   filter(YEAR %in% c(2015),
#          MONTH %in% c(Month),
#          DAY %in% c(Day)) %>%
#   setorder(SITEID, DATE_TIME)

# Predict.Location.trans <- Site[, 6:7]#PM25_2015_2017_CMAQ[1:5, c(5, 6, 17:18, 20)]
D <- rdist(Predict.Location.trans[, c("sLON_X", "sLAT_Y")], 
      cmaq_site[, c("LON_X", "LAT_Y")])
# diag(D) <- 1E7
# range(D[5,])

colnames(D) <- cmaq_site$CMAQ_ID
n.row <- nrow(Predict.Location.trans)
n.col <- nrow(cmaq_site)
Predict_D_30 <-  matrix(0, nrow = n.row, ncol = n.col)

for(pre in 1:n.row)
{
  index <- which(D[pre, ] <= 30)
  Predict_D_30[pre, index] <- 1/D[pre, index]
}
# save(Predict_D_30, file = paste0(file, "Predict_D_30_W.RData"))
###################################################################
###################################################################
###################################################################
###################################################################
CMAQ_PM25_Test <- CMAQ_PM25 %>% filter(YEAR_MONTH %in% YearMonth,
                                       DAY %in% Day) %>% 
                  setorder(CMAQ_ID, DATE_TIME) %>% setDF()
DATE_TIME <- unique(CMAQ_PM25_Test$DATE_TIME)
Nt <- length(unique(CMAQ_PM25_Test$DATE_TIME))
CMAQ_ID <- unique(CMAQ_PM25_Test$CMAQ_ID)
CMAQ_PM25_Test$T_index <- 1:Nt
N.BAUs <- length(CMAQ_ID)
CMAQ_N <- length(CMAQ_ID)
CMAQ_X_ts <- matrix(NA, nrow = Nt, ncol = N.BAUs)
setDT(CMAQ_PM25_Test)
for(t in 1:Nt)
{
  CMAQ_X_ts[t, ] <- dcast(CMAQ_PM25_Test[T_index == t, ]
                          , . ~ CMAQ_ID, 
                          value.var = "CMAQ_PM25"
  )[1, 2:(CMAQ_N + 1)]  %>% as.numeric()
  
  cat("time: ", as.character(DATE_TIME[t]), "\n")
}
row.sum_30 <- rowSums(Predict_D_30)
n.row <- nrow(Predict.Location.trans)
for(pre in 1:n.row)
{
  Predict_D_30[pre, ] <- Predict_D_30[pre, ]/row.sum_30[pre]
  if(pre %%1000 == 0)
    cat("s = ", pre, "\n")
}
temp <- Predict.Location.trans
setDF(Predict.Location.trans)
for(t in 1:Nt)
{
  if(t == 1)
  {
    CMAQ_PM25_30 <- Predict_D_30 %*% CMAQ_X_ts[t, ]
    Predict.Location.trans$CMAQ_PM25_30 = CMAQ_PM25_30
    Predict.Location.trans$DATE_TIME = as.Date(DATE_TIME[t])
    # Predict.Location.trans$YEAR = year(CMAQ_PM25_Test$DATE_TIME[t])
    # Predict.Location.trans$MONTH = month(CMAQ_PM25_Test$DATE_TIME[t])
    # Predict.Location.trans$DAY = day(CMAQ_PM25_Test$DATE_TIME[t])
  }else{
    Predict.Location.trans <- rbind(Predict.Location.trans, 
            cbind(temp, data.frame(CMAQ_PM25_30 = Predict_D_30 %*% CMAQ_X_ts[t, ],
            DATE_TIME = as.Date(DATE_TIME[t])#,
            # YEAR = year(CMAQ_PM25_Test$DATE_TIME[t]),
            # MONTH = month(CMAQ_PM25_Test$DATE_TIME[t]),
            # DAY = day(CMAQ_PM25_Test$DATE_TIME[t])
            # ))
      ))) 
    
  }
  # if(t %%10 ==0)
    cat("time: ", as.character(DATE_TIME[t]), "\n")
    # print(DATE_TIME[t])
}
# setDT(Predict.Location.trans)
# range(Predict.Location.trans$CMAQ_PM25_30)
# range(CMAQ_X_ts)

Cali.Data <- Predict.Location.trans
rm(Predict.Location.trans)
# Cali.Data <- Predict_Location
# Cali.Data <- rbind(Predict_Location, Cali.Data1)
Cali.Data$YEAR = year(Cali.Data$DATE_TIME)
Cali.Data$MONTH = month(Cali.Data$DATE_TIME)
Cali.Data$DAY = day(Cali.Data$DATE_TIME)
Cali.Data$YEAR_MONTH = paste0(Cali.Data$YEAR, ifelse(Cali.Data$MONTH < 10,
                                                     paste0("0",
                                                            as.character(Cali.Data$MONTH)
                                                     ),
                                                     as.character(Cali.Data$MONTH)
)) %>% as.numeric()
Cali.Data <- as.data.table(Cali.Data)
save(Cali.Data, file = paste0(file, "/Cali.Data.winter.RData"))
# range(Cali.Data$CMAQ_PM25_30)

# temp <- Species_Link_CMAQ( Site         # 站点
#                            , CMAQ_Table = CMAQ_PM25  # CMAQ 数据表
#                            , CMAQ_Site = CMAQ_Site
#                            , Distance_Threshold = 30  #选择加权的距离阈值
# )
