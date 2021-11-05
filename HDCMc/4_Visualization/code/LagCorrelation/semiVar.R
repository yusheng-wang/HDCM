library(stBase)
data("SiteData", package = "stBase")
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

library(gstat)
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

Da.mod <- Model_Base_Table_Update %>% 
  filter(YEAR_MONTH %in% c(201511, 201512, 201601)  #c(201511, 201512, 201601)
         # , MONTH %in% c(12)
  ) %>%
  setorder(SITEID) 

Da.mod$REAL_PM25 = if_else(is.na(Da.mod$REAL_PM25)
                           , Da.mod$NA.Kriging
                           , Da.mod$REAL_PM25)

PM25 <- Da.mod %>% 
  dplyr::select(SITEID, DATE_TIME, REAL_PM25) %>%
  data.table::dcast(DATE_TIME ~ SITEID, #fun =mean, 
                    value.var = "REAL_PM25")






setorderv(Da.mod, c("DATE_TIME","CITY_NAME"))
temp_part <- as.Date(unique(Da.mod$DATE_TIME))
loc = unique(Da.mod[, c("LON_X", "LAT_Y")])
mod_gridded <- SpatialPoints(coords = loc)
da_long =   dplyr::select(Da.mod, CITY, CITY_NAME
                          , REAL_PM25, CMAQ_PM25)

library(gstat)
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
vv1$Group = paste0("summer of 2015")
vv2$Group = paste0("winter of 2015")



pdf(paste0(paste0("./4_Visualization/code/LagCorrelation/"),
           'Contours',".pdf"), width = 12, height =  6)
par(mfrow = c(1, 2))
op <- par(mar = c(4, 4, 2, 1) + 0.1) 
par(cex= 1.5)
par(mgp=c(2,1,0))
vv <- vv1
vv = vv[!is.na(vv$gamma), c(5, 6, 3)]
# vv$spacelag = vv$spacelag
vv$timelag <- as.numeric(vv$timelag)

surf <- mba.surf(vv, no.X = nrow(vv), no.Y = nrow(vv),
                 extend = TRUE)$xyz.est
graphics::contour(x = surf$x, y = surf$y, z = surf$z
                  , ylab = "Spatial lags (km)"
                  , xlab = "Temporal lags (days)"
                  , lwd  = 1.5
                  , labcex = 0.7
                  , )

title(main = paste0("summer of 2015"))
vv <- vv2
vv = vv[!is.na(vv$gamma), c(5, 6, 3)]
# vv$spacelag = vv$spacelag
vv$timelag <- as.numeric(vv$timelag)

surf <- mba.surf(vv, no.X = nrow(vv), no.Y = nrow(vv),
                 extend = TRUE)$xyz.est
graphics::contour(x = surf$x, y = surf$y, z = surf$z
                  , ylab = "Spatial lags (km)"
                  , xlab = "Temporal lags (days)"
                  , lwd  = 1.5
                  , labcex = 0.7)
title(main = paste0("winter of 2015"))
dev.off()















# vv <- rbind(vv1, vv2)


# vv$Group = paste0("the warm season")

var_data <- vv2
# var_data <- rbind(var_data, vv)
var_data$avgDist = var_data$avgDist
var_data$gamma = var_data$gamma/1000
Size = 20
setDF(var_data)
var_data$timelag = as.numeric(var_data$timelag)
pdf(paste0(paste0("./4_Visualization/code/LagCorrelation/"),
           'variogram_w',".pdf"))
pl <- wireframe(gamma ~ timelag * avgDist|Group,
                data = var_data#[var_data$Group %in% "summer of 2015",]
                , groups = Group
                , ylab = list(label = "Distance (km)", fontsize = Size
                              , rot = 340)
                # , xlab='blah1'
                , xlab = list(label = "Time lag (days)", fontsize = Size
                              , rot = 45)
                , zlab = list(label = "Variogram (x 1000)", fontsize = Size
                              , rot = 95)
                , scales = list(arrows = F, cex = 1.3)
                , shade = T
                , screen = list(z = 60, x = -60)
                , colorkey = F,  aspect = c(1, 0.8)
                , drape = F
                , col.regions = c("red", "blue")
                , layout = c(1, 1)
                , par.strip.text = list(cex = 2, lines = 1.5)
)
pl
dev.off()

plot(vv2,map=FALSE)
vv <- vv1

metricVgm <- vgmST(stModel = "metric",
                   joint = vgm(100, "Exp", 400, nugget = 0.1),
                   sill = 10,
                   stAni = 100)
metricVgm <- fit.StVariogram(vv, metricVgm)

sepVgm <- vgmST(stModel = "separable",
                space = vgm(10, "Exp", 400, nugget = 0.1),
                time = vgm(10, "Exp", 1, nugget = 0.1),
                sill = 20)
sepVgm <- fit.StVariogram(vv, sepVgm)

metricMSE <- attr(metricVgm, "optim")$value
sepMSE <- attr(sepVgm, "optim")$value

plot(vv, list(sepVgm, metricVgm), main = "Semi-variance")


library(CompRandFld)
da <- setDF(PM25)
da <- as.matrix(da[, -1])
mean.est <- mean(da)
var.est <- var(da)
vgm.emp <- EVariogram(data= da,
                      coordx= Da.mod@sp@coords,
                      coordt= Da.mod@time,
                      cloud=F,
                      maxdist=500,
                      maxtime=10)

cormod="gneiting"
fixed=list(sill=var.est, mean=mean.est,nugget=0,power_s=1,power_t=1)
start=list(scale_s=200,scale_t=2,sep=.5)


fit <- FitComposite(da, coordx=Da.mod@sp@coords,
                    coordt= Da.mod@time,
                    corrmodel="gneiting ", maxtime=5,
                    likelihood="Marginal",type="Pairwise", fixed=list(
                      nugget= 1, mean=mean.est), start=list(scale_s=0.2,
                                                           scale_t=1, sill=10))
Covariogram(fit,show.cov=TRUE)

# Plot of the fitted space-time variogram
Covariogram(fit,vario=vgm.emp,show.vario=TRUE)

# Plot of covariance, variogram and spatio and temporal profiles:
Covariogram(fit,vario=vgm.emp,fix.lagt=1,fix.lags=1,show.vario=TRUE,pch=20)

fit <- FitComposite(data= da,
                    coordx=Da.mod@sp@coords,
                    coordt= Da.mod@time,
                    maxdist=500, maxtime= 10,
                    corrmodel= cormod,
                    likelihood="Marginal",
                    type="Pairwise",
                    fixed=fixed,start=start)


