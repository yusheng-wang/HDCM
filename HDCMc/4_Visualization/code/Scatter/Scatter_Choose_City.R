rm(list=ls())
library(stBase)
data("SiteData", package = "stBase")
#########################################################################
Model_2015_2017_Tab <- Model_Base_Table_Update
setDT(Model_2015_2017_Tab)
Sys.setlocale("LC_TIME", "English")
PM25_CMAQ <- Model_2015_2017_Tab %>% 
  filter(YEAR_MONTH %in% c(201506, 201507, 201508,201511, 201512, 201601)
         , SITEID %nin% c(3, 57, 62, 63, 8, 11,18,72)
  ) %>%
  setorder(SITEID) %>% 
  dplyr::select(SITEID, CITY, CITY_NAME
                , DATE_TIME
                , YEAR_MONTH
                , REAL_PM25
                , CMAQ_PM25
                , LON, LAT)
setDF(PM25_CMAQ)
PM25_CMAQ$FLAG = if_else(PM25_CMAQ$YEAR_MONTH %in% c(201511, 201512, 201601)
                         , "winter of 2015", "summer of 2015")
setDT(PM25_CMAQ)
PM25_CMAQ <- PM25_CMAQ[!is.na(PM25_CMAQ$REAL_PM25)]
da <- PM25_CMAQ %>% 
  plyr::ddply(.(CITY, FLAG)
              , dplyr::summarize
              , Corr = round(mean(cor(CMAQ_PM25, REAL_PM25)), 3)
              , LAT_label = round(mean(LAT), 2)
  )
setorder(da, LAT_label)
# da$City_lat <- paste0(da$CITY, " (lat: ", da$LAT, ")")
da$City_lat <- unique(paste0(da$CITY))
da$Latt <-  paste0("latitude: ", da$LAT_label, "° N")
da$LAT_label = rep(1:13, each = 2)

PM25_CMAQ <- PM25_CMAQ %>% left_join(da, by = c("CITY", "FLAG"))
setDT(PM25_CMAQ)

Label <- as_labeller(c(`1` = da$City_lat[1], `2` =  da$City_lat[2],
                       `3` = da$City_lat[3], `4` =  da$City_lat[4],
                       `5` = da$City_lat[5], `6` =  da$City_lat[6],
                       `7` = da$City_lat[7], `8` =  da$City_lat[8],
                       `9` = da$City_lat[9], `10` =  da$City_lat[10],
                       `11` = da$City_lat[11], `12` =  da$City_lat[12],
                       `13` = da$City_lat[13]))
data_base0 <- PM25_CMAQ[CITY %in% c("Xingtai", "Tangshan")]
# data_base <- data_base0[FLAG == "summer of 2015"]
Up <- max(data_base0$REAL_PM25, data_base0$CMAQ_PM25, na.rm = T) + 50
Low <- 0
library(latex2exp)
p1 <- ggplot(data = data_base0) +
  geom_abline(slope = 1, color = "gray", size = 1) +
  geom_abline(slope = 2, color = "gray",
              size = 0.8) +
  geom_abline(slope = 0.5, color = "gray",
              size = 0.8) +
  scale_x_continuous(limits = c(Low, Up), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(Low, Up), expand = c(0, 0)) + 
  # geom_abline(slope = 1, colour = "gray") +
  geom_point(aes(x = REAL_PM25, y = CMAQ_PM25), 
             colour = "black", size = 1.5)  +
  geom_text(y = 680,
            aes(x = 150, label = paste0("Corr = ", Corr)),
            size = 10, label.size = 18) +
  geom_text(x = (Up - Low)*1.23/2.55,
            y = Up*0.90,
            angle = 60,
            label = "k = 2",
            color = "gray",
            size = 8) +
  geom_text(x = (Up)*0.90,
            y = Up*0.86,
            angle = 38,
            label = "k = 1",
            size = 8) +
  geom_text(x = (Up)*0.88,
            y = Up*0.48,
            angle = 23,
            label = "k = 0.5",
            color = "gray",
            size = 8) +
  labs(x = TeX("Observed PM$_{2.5}$ ($μg/m^3$)"),
       y = TeX("CMAQ outputs ($μg/m^3$)")) +
  facet_wrap(~ FLAG + LAT_label, ncol = 2
             , labeller = labeller(LAT_label = Label)) + theme_bw() +
  # guides(colour = guide_legend(override.aes = list(alpha = 1))) +facet_wrapfacet_grid
  theme(axis.text = element_text(size = 20, colour = "black")
        , axis.title = element_text(size = 22, colour = "black")
        , legend.title = element_text(size = 22, colour = "black")
        , legend.text = element_text(size = 20, colour = "black")
        # , legend.title = element_blank()
        # , legend.position = "none"
        # , legend.key.size = unit(20,'cm')
        # , legend.key.width = unit(3, "line")
        # , legend.key.height = unit(10, "line")
        , strip.text =  element_text(size = 22, colour = "black"))
#Explore_Summer
# Explore_Winter
FILE <- paste0("./4_Visualization/code/Scatter/")
FILE <- "C:/Users/cheny/Dropbox/spCalibration/AOAS-2021-09/Manuscript_21_11/figure/"
ggsave(plot = p1, paste0(FILE, 'Fig3',".png"),
       width = 12, height = 12, dpi = 300)


# data_base <- data_base0[FLAG == "winter of 2015"]
# library(latex2exp)
# Up <- max(data_base$REAL_PM25, data_base$CMAQ_PM25, na.rm = T) + 50
# Low <- 0
# p2 <- ggplot(data = data_base) +
#   geom_point(aes(x = REAL_PM25, y = CMAQ_PM25), 
#              colour = "black", size = 0.5)  +
#   geom_abline(slope = 1, color = "gray", size = 1) +
#   geom_abline(slope = 2, color = "gray",
#               # linetype = "dotted",
#               size = .5) +
#   geom_abline(slope = 0.5, color = "gray",
#               # linetype="dotted", 
#               size = .5) +
#   scale_x_continuous(limits = c(Low, Up), expand = c(0, 0)) + 
#   scale_y_continuous(limits = c(Low, Up), expand = c(0, 0)) + 
#   geom_abline(slope = 1, colour = "gray") +
#   geom_text(y = 690,
#             aes(x = 100, label = paste0("Corr = ", Corr)),
#             size = 6, label.size = 18) +
#   geom_text(x = (Up - Low)*1.23/2.55,
#             y = Up*0.90,
#             angle = 55,
#             label = "k = 2",
#             color = "gray",
#             size = 5) +
#   geom_text(x = (Up)*0.90,
#             y = Up*0.86,
#             angle = 43,
#             label = "k = 1",
#             size = 5) +
#   geom_text(x = (Up)*0.88,
#             y = Up*0.48,
#             angle = 25,
#             label = "k = 0.5",
#             color = "gray",
#             size = 5) +
#   labs(x = TeX("Observed PM$_{2.5}$ ($μg/m^3$)"),
#        y = TeX("CMAQ outputs ($μg/m^3$)")) +
#   facet_grid(FLAG~ Latt + LAT_label,
#              , labeller = labeller(LAT_label = Label)) + theme_bw() +
#   # guides(colour = guide_legend(override.aes = list(alpha = 1))) +
#   theme(axis.text = element_text(size = 16, colour = "black")
#         , axis.title = element_text(size = 18, colour = "black")
#         , legend.title = element_text(size = 16, colour = "black")
#         , legend.text = element_text(size = 18, colour = "black")
#         # , legend.title = element_blank()
#         # , legend.position = "none"
#         # , legend.key.size = unit(20,'cm')
#         # , legend.key.width = unit(3, "line")
#         # , legend.key.height = unit(10, "line")
#         , strip.text =  element_text(size = 14, colour = "black"))
# #Explore_Summer
# # Explore_Winter
# FILE <- paste0("./4_Visualization/code/Scatter/")
# ggsave(plot = p2, paste0(FILE, 'Scatter_Winter_lat',".png"),
#        width = 12, height = 6, dpi = 300)
# 
# library(cowplot)
# p <- cowplot::plot_grid(p1, p2, nrow = 2)
# ggsave(plot = p, paste0(FILE, 'Scatter',".pdf"),
#        dpi= 300, width = 12, height = 10)
