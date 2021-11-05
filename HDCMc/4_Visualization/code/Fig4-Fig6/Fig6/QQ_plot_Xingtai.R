source("./R/PSTVB_Packages.R")
library(stBase)
######################################################################
# 1
######################################################################
month = c(201506, 201507, 201508)
day = 1:31
######################################################################
CITY.Name <- "Xingtai"
tab <- c("A_HDCM2", "S", "06_23", CITY.Name)
file <- "./1_CrossValidation/"
######################################################################
HDCM_SVC <- load_HDCM_SVC_data(file, hdcm_tab = tab, day = day,
                               month = month, seed = 1234,
                               svc_tab = c("SVC", tab[2], CITY.Name))

# save(HDCM_SVC, file = paste0(file, "/data/HDCM_SVC_", CITY.Name,
#                                   ifelse(min(month) %in% c(201511, 201512, 201601),
#                                          "_W", "_S"), ".RData"))
CMAQ_PM25 <- HDCM_SVC$CMAQ_PM25
HDCM.Ens <- HDCM_SVC$HDCM.Ens
SVC <- HDCM_SVC$SVC
######################################################################
######################################################################
# 2
######################################################################
######################################################################
Da <- rbind(CMAQ_PM25, SVC, HDCM.Ens)
Da$Model <- ifelse(Da$Model %in% "HDCM.Ens", "HDCM",
               ifelse(Da$Model %in% "SVC", "SVC",
                   ifelse(Da$Model %in% "CMAQ", "CMAQ", NA)))

Da$method <- ifelse(Da$Model%in% "SVC", 1, ifelse(Da$Model%in% "HDCM", 2, 0))

Da$RMSE = paste0("RMSE = ", Round((Da$RMSE)))
Da$MAPE = paste0("MAPE = ", Round((Da$MAPE)))
Da$Corr = paste0("Corr = ", Round((Da$Corr)))
Da$FAC2 = paste0("FAC2 = ", Round((Da$FAC2)))
Da$CRPS = paste0("CRPS = ", Round((Da$CRPS)))

Da$crps_x <- ifelse(Da$Model %in% "HDCM", 0.85,
                ifelse(Da$Model %in% "SVC", 0.70,
                   ifelse(Da$Model %in% "CMAQ", 0.75, NA)))

Da$corr_x <- ifelse(Da$Model %in% "HDCM", 9,
                ifelse(Da$Model %in% "SVC", 9,
                    ifelse(Da$Model %in% "CMAQ", 9, NA)))


Da$fac2_x <- ifelse(Da$Model %in% "HDCM", 6,
                 ifelse(Da$Model %in% "SVC", 6,
                     ifelse(Da$Model %in% "CMAQ", 6, NA)))
Up <- floor(max(Da$REAL_PM25, Da$Pred.Median, na.rm = T)) + 25
Low <- 2#ceil(min(Da$REAL_PM25, Da$Pred.Median))
x0 <- -12
size = c(16, 14, 5)
Label <- as_labeller(c(`0` = "CMAQ" ,`1` = "SVC", `2` = "HDCM"))
library(viridis)
Da$x = 45
######################################################################
######################################################################
# 3   plot
######################################################################
######################################################################
# library(grid)
# grid.newpage();grid.draw(roundrectGrob(gp = gpar(lwd = NA)))
{
  ggplot(Da, aes(x = REAL_PM25, y = Pred.Median)) + 
    geom_point(size = 0.8) +
    # geom_point(aes(col = as.factor(month(DATE_TIME))), size = 0.8) +
    # geom_bin2d(binwidth = c(20, 20), shape = 10) +
    # stat_density_2d(alpha = 0.5, geom = "polygon", 
    #                 aes(fill = (after_stat(level)))) +
    # scale_fill_distiller(palette =  "Spectral", direction = -1)+
    
    # stat_density_2d(geom = "raster",
    #                 aes(fill = stat(density)),
    #                 contour = FALSE) +
    
    geom_abline(slope = 1, color = "black", size = 1) +
    geom_abline(slope = 2, color = "gray", size = 0.5) +
    geom_abline(slope = 0.5, color = "gray", size = .5) +
    # geom_text(y = Up*0.95,
    #           aes(x = x0 + x, label = RMSE,  group = Model),
    #           size = size[3]) +
    # geom_text(y = Up*0.87,
    #           aes(x = x0 + x + crps_x, label = CRPS, group = Model),
    #           size = size[3]) +
    # geom_text(y = Up*0.83,
    #           aes(x = x0 +  x + crps_x, label = MAPE, group = Model),
    #           size = size[3]) +
    
    geom_label(y = Up*0.9,
               aes(x = x0 + x + fac2_x, label = FAC2, group = Model),
               size = size[3], label.size = 0) +
    geom_label(y = Up*0.8,
               aes(x = x0 + x + corr_x, label = Corr, group = Model),
               size = size[3], label.size = 0) +
    
    annotate(geom="text",x = (Up - Low)*0.97/2,
             y = Up*0.90,
             angle = 60,
             label = "k = 2",
             color = "gray",
             size = 5) +
    annotate(geom="text", x = (Up)*0.93,
             y = Up*0.88,
             angle = 40,
             label = "k = 1",
             size = 5,
             fontface = 1) +
    annotate(geom="text",x = (Up)*0.90,
             y = Up*0.49,
             angle = 25,
             label = "k = 0.5",
             color = "gray",
             size = 5) +
    # facet_wrap(~ Model, ncol = 4) +
    facet_grid(~ method
               , labeller = labeller(method = Label)
    ) +
    scale_x_continuous(limits = c(0, Up)
                       , expand = c(0, 0)
    ) +
    scale_y_continuous(limits = c(0, Up)
                       , expand = c(0, 0)
    ) +
    labs(color = "", x = TeX("Observed PM$_{2.5}$ ($μg/m^3$)")
         , y = TeX("Predicted PM$_{2.5}$ ($μg/m^3$)")) + theme_bw() + 
    theme( axis.title= element_text(size = size[1], colour = "black")
           , axis.text = element_text(size = size[2], colour = "black")
           # , legend.title = element_text(size = size[1], colour = "black")
           , legend.text= element_text(size = size[2], colour = "black")
           , legend.title = element_blank()
           # , legend.position="top"
           , legend.margin=margin(t = -0.1, unit='cm')
           , legend.background = element_rect(fill="transparent")
           , panel.grid.major = element_blank()
           , panel.grid.minor = element_blank()
           , legend.position = "top"
           # , legend.key.width = unit(1,"line")
           # , legend.key.height = unit(2,"line")
           , strip.text =  element_text(size = size[2], colour = "black")
    ) #+  guides(col = F)
}
######################################################################
######################################################################
# 4  save
######################################################################
######################################################################
ggsave(paste0("./figure/", 'Fig6_', CITY.Name, "_S", ".pdf"),
       dpi= 500, width  = 11, height = 4)
