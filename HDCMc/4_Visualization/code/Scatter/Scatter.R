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
data_base <- PM25_CMAQ[FLAG == "summer of 2015"]
# data_base <- PM25_CMAQ[FLAG == "winter of 2015"]
library(latex2exp)
p <- ggplot(data = data_base) +
  geom_point(aes(x = REAL_PM25, y = CMAQ_PM25), 
             colour = "black", size = 0.5)  +
  geom_abline(slope = 1, color = "gray", size = 1) +
  geom_abline(slope = 2, color = "gray",
              linetype = "dotted", size = 1) +
  geom_abline(slope = 0.5, color = "gray",
              linetype="dotted", size = 1) +
  scale_x_continuous(limits = c(0, max(data_base$REAL_PM25
                                       , data_base$CMAQ_PM25, na.rm = T))) + 
  scale_y_continuous(limits = c(0, max(data_base$REAL_PM25
                                       , data_base$CMAQ_PM25, na.rm = T))) + 
  geom_abline(slope = 1, colour = "gray") +
  geom_text(y = 250,
             aes(x = 70, label = paste0("Corr = ", Corr)),
             size = 6, label.size = 18) +
  labs(x = TeX("Observed PM$_{2.5}$ ($μg/m^3$)"),
       y = TeX("CMAQ outputs ($μg/m^3$)")) +
  facet_wrap(~ Latt + LAT_label, ncol = 6
             , labeller = labeller(LAT_label = Label)) + theme_bw() +
  # guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(axis.text = element_text(size = 16, colour = "black")
        , axis.title = element_text(size = 18, colour = "black")
        , legend.title = element_text(size = 16, colour = "black")
        , legend.text = element_text(size = 18, colour = "black")
        # , legend.title = element_blank()
        # , legend.position = "none"
        # , legend.key.size = unit(20,'cm')
        # , legend.key.width = unit(3, "line")
        # , legend.key.height = unit(10, "line")
        , strip.text =  element_text(size = 14, colour = "black"))
#Explore_Summer
# Explore_Winter
FILE <- paste0("./4_Visualization/code/Scatter/")
ggsave(plot = p, paste0(FILE, 'Scatter_Summer_lat',".pdf"),
       width = 16, height = 9)


data_base <- PM25_CMAQ[FLAG == "winter of 2015"]
library(latex2exp)
p <- ggplot(data = data_base) +
  geom_point(aes(x = REAL_PM25, y = CMAQ_PM25), 
             colour = "black", size = 0.5)  +
  geom_abline(slope = 1, color = "gray", size = 1) +
  geom_abline(slope = 2, color = "gray",
              linetype = "dotted", size = 1) +
  geom_abline(slope = 0.5, color = "gray",
              linetype="dotted", size = 1) +
  scale_x_continuous(limits = c(0, max(data_base$REAL_PM25
                                       , data_base$CMAQ_PM25, na.rm = T))) + 
  scale_y_continuous(limits = c(0, max(data_base$REAL_PM25
                                       , data_base$CMAQ_PM25, na.rm = T))) + 
  geom_abline(slope = 1, colour = "gray") +
  geom_text(y = 710,
             aes(x = 200, label = paste0("Corr = ", Corr)),
             size = 6, label.size = 18) +
  labs(x = TeX("Observed PM$_{2.5}$ ($μg/m^3$)"),
       y = TeX("CMAQ outputs ($μg/m^3$)")) +
  facet_wrap(~ Latt + LAT_label, ncol = 6
             , labeller = labeller(LAT_label = Label)) + theme_bw() +
  # guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(axis.text = element_text(size = 16, colour = "black")
        , axis.title = element_text(size = 18, colour = "black")
        , legend.title = element_text(size = 16, colour = "black")
        , legend.text = element_text(size = 18, colour = "black")
        # , legend.title = element_blank()
        # , legend.position = "none"
        # , legend.key.size = unit(20,'cm')
        # , legend.key.width = unit(3, "line")
        # , legend.key.height = unit(10, "line")
        , strip.text =  element_text(size = 14, colour = "black"))
#Explore_Summer
# Explore_Winter
FILE <- paste0("./4_Visualization/code/Scatter/")
ggsave(plot = p, paste0(FILE, 'Scatter_Winter_lat',".pdf"),
       width = 16, height = 9)