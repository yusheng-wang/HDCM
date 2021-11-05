#####################################################################
#####################################################################
# load("./data/CMAQ_Site.Rdata")
# load("./data/Site.RData")
data("SiteData", package = "stBase")
data("GeoMap", package = "stBase")
load(paste0(file, "/Cali.Data.summer.RData"))
#####################################################################
# Year  = c(2015, 2016)
# Month = c(11, 12, 1)
# Day <- 17:22
# Day <- 1:31
#####################################################################
Cali.Data <- Cali.Data %>%
  filter(YEAR_MONTH %in% YearMonth,
         DAY %in% c(Day)
  ) %>% setorder(sample.id, DATE_TIME)

setDF(Cali.Data)
Nt <- length(unique(Cali.Data$DATE_TIME))
Cali.Data$T_index <- 1:Nt
#####################################################################
# the distance from points to grid
#####################################################################
Temp1 <- Cali.Data[, c("sLON_X", "sLAT_Y")] %>% distinct()
grid <- CreateGrid(Site,
                   max.edge = c(.35, .65), #0.3,0.7
                   offset = c(1e-1, 0.5), #0.4, 0.6
                   cutoff = .05, #0.1
                   col = "black", size = 1)
Temp2 <- grid$grid.coords[, c("LON_X", "LAT_Y")] %>% distinct()
#####################################################################
#####################################################################
normal.constant <- 1e3
cs <- 0.15
#####################################################################
dist <- fields::rdist(Temp1[, c("sLON_X", "sLAT_Y")], 
                      Temp2[, c("LON_X", "LAT_Y")])/normal.constant
# threshold <- max(iDist(Site[, c("LON_X", "LAT_Y")],
#                        grid$grid.coords[, c("LON_X", "LAT_Y")]))*0.15/normal.constant
threshold <- max(fields::rdist(Site[, c("LON_X", "LAT_Y")],
                               grid$grid.coords[, c("LON_X", "LAT_Y")]))*cs/normal.constant
n.row <- nrow(Temp1)
n.col <- nrow(Temp2)
#####################################################################
#####################################################################
H <- matrix(1, nrow = n.row, ncol = n.col)
for(s in 1:n.row)
{
  if(s %% 1000 == 0){cat("s = ", s, "\n")}
  j.index1 <- which(dist[s, ] > threshold)
  # j.index2 <- which(dist[s, ] <= threshold)
  H[s, j.index1] = 0
}
#####################################################################
Cali.Data <- as.data.table(Cali.Data)
Cali_X_ts <- array(NA, dim = c(2, n.row, Nt))
Cali_X_ts[1, , ] = 1
for(t in 1:Nt)
{
  Cali_X_ts[2, , t] <- dcast(Cali.Data[T_index == t, ] 
                             , . ~ sample.id 
                             , value.var = "CMAQ_PM25_30"
  )[1, 2:(n.row + 1)] %>% as.numeric()
  # Cali_X_ts[3, , t] <- dcast(Cali.Data[T_index == t, ]
  #                            , . ~ Cali_Id, value.var = "CMAQ_PM25_10"
  # )[1, 2:(n.row + 1)]  %>% as.numeric()
  cat("time: ", as.character(DATE_TIME[t]), "\n")
}
#####################################################################
Cali_Data_Inf <- list(H = H
                      , Nt = Nt
                      , X_ts = Cali_X_ts
                      , CMAQ_N = n.row
                      , CMAQ_Cali = Cali.Data
)
save(Cali_Data_Inf, file = paste0(file, "./Cali_HDCM_S.RData"))
#####################################################################