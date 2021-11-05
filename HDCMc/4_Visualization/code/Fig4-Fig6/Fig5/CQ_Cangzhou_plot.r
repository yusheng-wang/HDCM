rm(list=ls())
library(stBase)
source("./R/PSTVB_Packages.R")
month = c(201511, 201512, 201601)
day = 1:31
######################################################################
# 1
######################################################################
CITY.Name <- "Cangzhou"
tab <- c("A_HDCM2", "W", "06_22", CITY.Name)
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
pred <- HDCM.Ens[, "Pred.Median"]
obs <- HDCM.Ens[, "REAL_PM25"]
# conditional.quantile(frcst, obs, main = "Sample Conditional Quantile Plot")
Con.HDCM.Ens <- Cond.quantile(pred, obs, 
                              city =  unique(HDCM.Ens$CITY),
                              model = unique(HDCM.Ens$Model))


pred <- SVC[, "Pred.Median"]
obs <- SVC[, "REAL_PM25"] 
# conditional.quantile(pred[, 1], obs[, 1], main = "Sample Conditional Quantile Plot")
Con.SVC <- Cond.quantile(pred, obs,
                         city = unique(SVC$CITY),
                         model = unique(SVC$Model))


pred <- CMAQ_PM25[, "Pred.Median"]
obs <- CMAQ_PM25[, "REAL_PM25"]
# conditional.quantile(pred, obs, main = "Sample Conditional Quantile Plot")
Con.CMAQ_PM25 <- Cond.quantile(pred, obs,
                               city = unique(CMAQ_PM25$CITY),
                               model = unique(CMAQ_PM25$Model))
######################################################################
#                               plot
######################################################################
# CITY.Name <- "Handan"
month = c(201511, 201512, 201601)
# load(paste0(file, CITY.Name, "_", month[1], "_cq.RData"))
da1 <- rbind(Con.CMAQ_PM25$quan.data,
             # Con.HDCM1.Ens$quan.data,
             Con.HDCM.Ens$quan.data,
             # Con.SVC1$quan.data,
             Con.SVC$quan.data)
UP <- max(da1$x, da1$y, na.rm = T) + 20
label <- as.character(unique(da1$group))
da1$group <- ordered(da1$group,
                     levels=label)

da1$Model <- ifelse(da1$Model %in% "HDCM.Ens", "HDCM",
                    ifelse(da1$Model %in% "SVC", "SVC",
                           ifelse(da1$Model %in% "CMAQ", "CMAQ", "Observation")))

da1$Model <- ordered(da1$Model,
                     levels=c("Observation", "CMAQ", "SVC", "HDCM"))


Label <- as_labeller(c(`0` = "CMAQ" , `1` = "SVC", `2` = "HDCM"))
# Label <- as_labeller(c(`0` = "CMAQ" , `1` = "SVC1" 
#                        , `2` = "HDCM1"
#                        , `3` = "SVC2", `4` = "HDCM2"))
size <- c(16, 14)
ls = c(0.8, 0.5, 0.5)
ggplot() +  
  geom_line(da1, mapping =aes(x = x, y = y, 
                             col = group, 
                             group = flag,
                             linetype = group,
                             size = group))+
  geom_abline(intercept = 0, slope = 1, col = "gray", size = 0.5) +
  # geom_histogram(da2, binwidth = 70
  #                , position = "identity"
  #                ,mapping = aes(z, y=(..count..)/5
  #                )) +
  facet_wrap(~ Model, ncol = 3) +
  # facet_grid(city ~ method, scales = "free"
  #            , labeller = labeller(method = Label)
  # )+
  scale_x_continuous(limits = c(0, UP), 
                     expand = c(0, 0),
                     breaks  = seq(0, UP, 150),
                     labels = seq(0, UP, 150)) +
  scale_y_continuous(name = "",
                     limits = c(0, UP), 
                     expand = c(0, 0),
                     breaks  = c(8),
                     position = "right",
                     labels = ""
                     #   function(y) 
                     # { paste0(round(y*50, 0), "")
                     #   }                  
                     , sec.axis = sec_axis(~., 
                                         name = TeX("Observed PM$_{2.5}$ $( μg/m^3 )$"),
                                         breaks  = seq(0, UP, 150),
                                         labels = seq(0, UP, 150)
                                         # function(b) {
                                           #paste0(round(b,0), "")
                                           # }
                     )
  )  + scale_linetype_manual(name='', 
                             values=c("solid", "longdash", "dotted"),
                             labels = label) + 
  scale_size_manual(name='', 
                    values = ls, labels = label) +
  scale_colour_manual(name='', 
                      values = c("black", "black" , "black"), #, "#556B2F" , "blue"
                      labels = label)+
  theme_bw() + 
  labs(x = TeX("Predicted PM$_{2.5}$ $( μg/m^3 )$"),
       y.left = TeX("Observed PM$_{2.5}$ $( μg/m^3 )$")) +
  theme(axis.text = element_text(size = size[2], colour = "black")
        , axis.text.x  = element_text(angle = 0)
        , axis.title.x = element_text(size = size[1], colour = "black")
        , axis.title.y = element_text(size = size[1], colour = "black")
        , legend.title = element_text(size = size[1], colour = "black")
        , legend.text = element_text(size = size[2], colour = "black")
        # , legend.title = element_blank()
        , legend.background = element_rect(fill="transparent")
        , legend.key.width = unit(5,"line")
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , legend.position = "top"#c(0.6, 0.1)
        , legend.margin = margin(t = 1, unit='cm')
        , axis.ticks.length.y.right = unit(-0, "cm")
        , strip.text =  element_text(size = size[2], colour = "black")
        # , axis.text.y.right  = element_text(vjust = -2,
        #                                     hjust = -300,
        #                              margin = margin(l = 15, r = 2))
  )  +  guides(linetype = guide_legend(override.aes = 
                list(size = ls), nrow=1, byrow=TRUE))
######################################################################
######################################################################
ggsave(paste0("./figure/", 'Fig5_', CITY.Name, "_W", ".pdf"),
       dpi= 500, width  = 12, height = 5)
