library(stBase)
source("./R/PSTVB_Packages.R")
data("SiteData", package = "stBase")
file <- "./1_CrossValidation/data/"
######################################################################

######################################################################
Season <- "W"
City.Name <-  sort(as.character(unique(Site$CITY)))
Tab <- list.files(file)
HDCMs <- NULL
######################################################################
for(City in 1:13)
{
  HDCM <- Tab[grepl(paste0("HDCM"), Tab) &
                grepl(paste0("_", Season ,"_"), Tab) &
                grepl((City.Name[City]), Tab)]
  
  
  load(paste0(file, HDCM))
  # CITY.Name <- "Beijing"
  #------------------------------------------------------------------
  Y.test <- test.HDCM(test = test, Ks = Ks, PIU = PIU, seed = 1234)
  HDCM.test <- (apply(Y.test, c(1, 2), quant))
  
  Pred.L25 <- HDCM.test[1,,] %>% as.matrix()
  Pred.Median <- HDCM.test[2,,] %>% as.matrix()
  Pred.U95 <- HDCM.test[3,,] %>% as.matrix()
  Pred.Mean <- (apply(Y.test, c(1, 2), mean)) %>% as.matrix()
  spT.validation()
  HDCMs <- rbind(HDCMs, data.frame(CITY = City.Name[City],
                                   Errors = as.vector(Pred.Median - test$Y_ts_true)))
  cat("city = ", City.Name[City], "\n\n")
}
HDCMs$Model <- "HDCM"
range(HDCMs$Errors, na.rm = T)
######################################################################
#  CMAQ
######################################################################
Model_Base_Table_2021 <- Model_Base_Table_Update
CMAQ <- Model_Base_Table_2021 %>% 
  filter(YEAR_MONTH %in% c(201511, 201512, 201601))%>%
  dplyr::select(CITY, REAL_PM25, CMAQ_PM25)

CMAQ$Errors <- CMAQ$CMAQ_PM25 - CMAQ$REAL_PM25
CMAQ$Model <- "CMAQ"
CMAQ <- CMAQ[, -c(2, 3)]
range(CMAQ$Errors, na.rm = T)
######################################################################
#  UK
######################################################################
load(paste0(file, "/UK_W.RData"))
UK$Errors <- UK$PM25.Pred - UK$True_REAL_PM25
UK$Model <- "UK"
UK <- UK[, c(1, 18, 19)]
mean(UK$Errors, na.rm = T)
range(UK$Errors, na.rm = T)
######################################################################
#  RF
######################################################################
load(paste0(file, "/RF_W.RData"))
RF$Errors <- RF$PM25.Pred - RF$True_REAL_PM25
RF$Model <- "RF"
RF <- RF[, c(1, 16, 17)]
mean(RF$Errors, na.rm = T)
range(RF$Errors, na.rm = T)
######################################################################
#  SVC
######################################################################
load(paste0(file, "/SVC_W.RData"))
SVC$Errors <- SVC$PM25.Pred - SVC$True_REAL_PM25
SVC$Model <- "SVC"
SVC <- SVC[, c(1, 18, 19)]
mean(SVC$Errors, na.rm = T)
range(SVC$Errors, na.rm = T)
da <- rbind(CMAQ, UK, RF, SVC, HDCMs)

da <- da[!is.na(da$Errors), ]
da <- da[abs(da$Errors) %>% between(0, 400), ]
######################################################################
#  plot
######################################################################
da$Model <- ordered(da$Model, levels = c("CMAQ", "UK",
                                         "RF", "SVC", "HDCM"))
p <- ggplot(data = da[da$Model %in%
                        c("CMAQ", "UK",
                          "RF", "SVC", "HDCM"),], aes(colour = Model, 
                                                      group = Model, 
                                                      fill = Model)) +
  geom_density(aes(Errors), alpha = 0.2, adjust = 3, size = 1)
# facet_wrap(~ LAT_label, ncol = 4
#            , labeller = labeller(LAT_label = Label)
# )    #facet_grid
p0 <- ggplot_build(p)  
da0 <- p0$data[[1]]
lab <-  unique(da$Model)
da0$Model = if_else(da0$group==1, lab[1],
                    if_else(da0$group==2, lab[2],
                            if_else(da0$group==3, lab[3],   
                                    if_else(da0$group==4, lab[4],
                                            lab[5]))))
da0$Model <- ordered(da0$Model, levels = c("CMAQ", "UK",
                                           "RF", "SVC", "HDCM"))
label <- c("CMAQ", "UK", "RF", "SVC", "HDCM")
pdf(file = "./figure/Fig4_predict_bias_W.pdf", width = 12, height = 8)
{
  ggplot(data = da0, aes(x = x, y = density)) + 
    # geom_point(aes(shape = Model), size = 5) +
    geom_line(aes(linetype = Model, col = Model), size = 1.5) +
    # scale_shape_manual(name = '', values =  c('*', '+'), labels = label) + 
    scale_linetype_manual(name = '', values=  c("longdash", "dotdash",
                                                "dotted", "dashed",
                                                "solid"),
                          labels = label) +
    scale_color_manual(name = '', values =  c("red", "blue", "#4a1486",
                                              "#31a354", "black"), 
                       labels = label)+
    geom_vline(xintercept = 0, col = "gray80", size = 0.8) +
    theme_bw() +  
    # geom_text(aes(x = min(da0$x), y = max(da0$density)
    #               , label =  "(b)"),
    #           # family = c("sans"),
    #           fontface = 'bold',
    #           color = "black", size = 10) +
    labs(x = TeX("Error (μg/m^3): predicted value minus observed value")) +
    theme(axis.text = element_text(size = 18, colour = "black")
          ,axis.text.x = element_text(hjust = 0.25, size = 16, colour = "black") 
          , axis.title = element_text(size = 18, colour = "black")
          , legend.title = element_text(size = 18, colour = "black")
          , legend.text = element_text(size = 20, colour = "black")
          # , legend.title = element_blank()
          , legend.background = element_rect(colour = 'transparent'
                                             , fill = 'transparent')
          , legend.key.width = unit(5,"line")
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , legend.position = c(0.25, 0.8)
          # , legend.margin = margin(t = -0.1, unit='cm')
          , strip.text =  element_text(size = 16, colour = "black")) +
    guides(linetype = guide_legend(override.aes = list(size = 1.5),
                                   nrow = 3, byrow = TRUE))
}
dev.off()















