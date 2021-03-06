# # 1 loading packages ---------------------------------------
# packages <- c("RandomFields","data.table", "spdep", "ggplot2"
#               ,"plyr","parallel","sqldf","gpuR", "randomForest"
#               ,"reticulate","spBayes", "latex2exp", "gstat"
#               , "lubridate", "dplyr","INLA", "stBase"
#               , "Hmisc", "MASS", "inlabru", "tidyr"
#               , "progress", "RODBC", "fields", "rgdal"
#               , "cowplot", "invgamma","DEoptim", "geoR"
#               , "MBA", "scoringRules", "Rcpp", "writexl"
#               , "SpecsVerification", "STRbook", "ggmap"
#               , "verification", "mapproj", "sp", "mvnfast"
#               , "rgdal") 
packages <- c("RandomFields","data.table", "ggplot2"
              ,"plyr","parallel","sqldf","gpuR", "randomForest"
              , "spBayes", "latex2exp", "gstat", "ggtext"
              , "lubridate", "dplyr","INLA", "stBase"
              , "Hmisc", "MASS", "inlabru", "tidyr"
              , "progress", "RODBC", "fields", "rgdal"
              , "cowplot", "invgamma", "geoR"
              , "MBA", "scoringRules", "Rcpp", "writexl"
              , "SpecsVerification", "STRbook", "ggmap"
              , "verification", "mapproj", "sp", "mvnfast"
              , "rgdal") 
# ,'MASS'
# 2  library
for(i in 1:length(packages))
{
  if(!lapply(packages[i], require,
             character.only = TRUE)[[1]])
  {
    install.packages(packages[i])
    # library(packages[i])
    lapply(packages[i], require,
           character.only = TRUE)
  }else{lapply(packages[i], require,
               character.only = TRUE)}
}
# x=lapply(packages, require, character.only = TRUE)
# rm(list=ls())
rm(i, packages)


