source("./R/PSTVB_Packages.R")
data("SiteData", package = "stBase")
# library(gstat)
##########################################################################
#                             in summer of 2015
##########################################################################
Da.mod <- Model_Base_Table_Update %>%
  filter(YEAR_MONTH %in% c(201506, 201507, 201508)  #c(201511, 201512, 201601)
         # , MONTH %in% c(12)
  ) %>%
  setorder(SITEID)
Da.mod$REAL_PM25 = if_else(is.na(Da.mod$REAL_PM25)
                           , Da.mod$NA.Kriging
                           , Da.mod$REAL_PM25)


setorderv(Da.mod, c("DATE_TIME","CITY_NAME"))
temp_part <- as.Date(unique(Da.mod$DATE_TIME))
loc = unique(Da.mod[, c("LON_X", "LAT_Y")])
mod_gridded <- SpatialPoints(coords = loc)
da_long =   dplyr::select(Da.mod, CITY, CITY_NAME
                          , REAL_PM25, CMAQ_PM25)


Da.mod <- spacetime::STFDF(sp = mod_gridded
                           , time = temp_part
                           , data = da_long)


vv1 <- variogram(object = REAL_PM25 ~ 1 , # fixed effect component
                 data = Da.mod, # July data
                 cressie = T,
                 covariogram = T,
                 # pseudo = 1,
                 width = 30, # spatial bin (80 km)
                 cutoff = 300, # consider pts < 1000 km apart
                 tlags = seq(0, 5, , 6)) # 0 days to 6 days
##########################################################################
#                             in winter of 2015
##########################################################################
Da.mod <- Model_Base_Table_Update %>%
  filter(YEAR_MONTH %in% c(201511, 201512, 201601)  #c(201511, 201512, 201601)
         # , MONTH %in% c(12)
  ) %>%
  setorder(SITEID)

Da.mod$REAL_PM25 = if_else(is.na(Da.mod$REAL_PM25)
                           , Da.mod$NA.Kriging
                           , Da.mod$REAL_PM25)

# PM25 <- Da.mod %>%
#   dplyr::select(SITEID, DATE_TIME, REAL_PM25) %>%
#   data.table::dcast(DATE_TIME ~ SITEID, #fun =mean,
#                     value.var = "REAL_PM25")
setorderv(Da.mod, c("DATE_TIME","CITY_NAME"))
temp_part <- as.Date(unique(Da.mod$DATE_TIME))
loc = unique(Da.mod[, c("LON_X", "LAT_Y")])
mod_gridded <- SpatialPoints(coords = loc)
da_long =   dplyr::select(Da.mod, CITY, CITY_NAME
                          , REAL_PM25, CMAQ_PM25)


Da.mod <- spacetime::STFDF(sp = mod_gridded
                           , time = temp_part
                           , data = da_long)
vv2 <- variogram(object = REAL_PM25 ~ 1 , # fixed effect component
                 data = Da.mod, # July data
                 cressie = T,
                 covariogram = T,
                 # pseudo = 1,
                 width = 30, # spatial bin (80 km)
                 cutoff = 300, # consider pts < 1000 km apart
                 tlags = seq(0, 5, , 6)) # 0 days to 6 days
vv1$Group = paste0("Summer of 2015")
vv2$Group = paste0("Winter of 2015")

##########################################################################
#                      plot
##########################################################################
pdf(paste0(paste0("./figure/"), 'Fig2',".pdf"), width = 12, height =  6)
par(mfrow = c(1, 2))
op <- par(mar = c(4, 4, 2, 1) + 0.1) 
par(cex= 1.25)
par(mgp=c(2, 1, 0))
vv <- vv1
vv = vv[!is.na(vv$gamma), c(5, 6, 3)]
# vv$spacelag = vv$spacelag
vv$timelag <- as.numeric(vv$timelag)

surf <- mba.surf(vv, no.X = nrow(vv), no.Y = nrow(vv),
                 extend = TRUE)$xyz.est
graphics::contour(x = surf$x, y = surf$y, z = surf$z
                  , ylab = "Spatial lags (km)"
                  , xlab = "Temporal lags (days)"
                  , lwd  = 2.0
                  , labcex = 1.1
                  , )

title(main = paste0("Summer of 2015"))
vv <- vv2
vv = vv[!is.na(vv$gamma), c(5, 6, 3)]
# vv$spacelag = vv$spacelag
vv$timelag <- as.numeric(vv$timelag)

surf <- mba.surf(vv, no.X = nrow(vv), no.Y = nrow(vv),
                 extend = TRUE)$xyz.est
graphics::contour(x = surf$x, y = surf$y, z = surf$z
                  , ylab = "Spatial lags (km)"
                  , xlab = "Temporal lags (days)"
                  , lwd  = 2.0
                  , labcex = 1.1)
title(main = paste0("Winter of 2015"))
dev.off()










