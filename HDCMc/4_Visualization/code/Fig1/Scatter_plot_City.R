data("Model_Base_Table_2021", package = "stBase")
colnames(Model_Base_Table_Update)

CITY.Name <- c("Zhangjiakou")
month <- c(201511, 201512, 201601)
Da1 <- Model_Base_Table_Update %>% 
  filter(YEAR_MONTH %in% month
         , CITY %in% CITY.Name
  ) %>%
  setorder(DATE_TIME, SITEID)  

CITY.Name <- c("Beijing", "Hengshui")#c("Tangshan", "Baoding")
month <- c(201506, 201507, 201508)
Da2 <- Model_Base_Table_Update %>% 
  filter(YEAR_MONTH %in% month
         , CITY %in% CITY.Name
  ) %>%
  setorder(DATE_TIME, SITEID) 
Da1$Season <- 1
Da2$Season <- 2
PM25_CMAQ <- rbind(Da1, Da2)

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
Da <- PM25_CMAQ %>% left_join(da, by = c("CITY", "FLAG"))
setDT(Da)




Up <- max(Da$CMAQ_PM25, Da$REAL_PM25, na.rm = T) + 20
Low <- 50

time <- as_labeller(c(`1` = "11/01/15 to 01/31/16", 
                      `2` = "06/01/15 to 08/31/15"))


Da$type <- ifelse(Da$CITY %in% "Zhangjiakou", 1,
                  ifelse(Da$CITY %in% "Beijing", 2, 3))
Label <- as_labeller(c(`1` = "Zhangjiakou", `2` = "Beijing"
                       ,`3` = "Hengshui"))
p <- ggplot(data = Da) +
  geom_point(aes(x= REAL_PM25, y = CMAQ_PM25),
             size = 1, col = "blue") +
  facet_grid(type + Season~., scales = c("fixed")
             , labeller = labeller(Season = time, type = Label)) +   #facet_grid
  theme_bw()  + 
  geom_abline(slope = 1, color = "gray", size = 1) +
  geom_abline(slope = 2, color = "gray",
              size = 0.8) +
  geom_abline(slope = 0.5, color = "gray",
              size = 0.8) +
  scale_x_continuous(limits = c(0, Up)
                     , expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0, Up)
                     , expand = c(0, 0)
  ) +
  geom_text(y = 20,
            aes(x = 230, label = paste0("Corr = ", Corr)),
            size = 7, label.size = 18) +
  geom_text(x = (Up - Low)*1.23/2.10,
            y = Up*0.90,
            angle = 60,
            label = "k = 2",
            color = "gray",
            size = 6) +
  geom_text(x = (Up)*0.90,
            y = Up*0.86,
            angle = 43,
            label = "k = 1",
            size = 6) +
  geom_text(x = (Up)*0.88,
            y = Up*0.50,
            angle = 22,
            label = "k = 0.5",
            color = "gray",
            size = 6) +
  labs(x = TeX("Observed PM$_{2.5}$ $( μg/m^3 )$"), 
       y = TeX("CMAQ output PM$_{2.5}$ $( μg/m^3 )$")) +
  theme(axis.text = element_text(size = 20, colour = "black")
        , axis.title.y = element_text(hjust = 0.5)
        , axis.title= element_text(size = 20, colour = "black")
        # , legend.title = element_text(size = 10, colour = "black")
        , legend.text= element_text(size = 20, colour = "black")
        , legend.title = element_blank()
        , panel.grid.major = element_blank() 
        , panel.grid.minor = element_blank()
        # , legend.position="top"
        , legend.position = "none"
        , legend.key.width = unit(1,"line")
        , legend.key.height = unit(2,"line")
        , strip.text =  element_text(size = 20, colour = "black")
  )
FILE <- "C:/Users/cheny/Dropbox/spCalibration/AOAS-2021-09/Manuscript_21_11/figure/"
ggsave(plot = p, paste0(FILE, 'Fig1',".png"),
       dpi = 400, width  = 5, height = 10)

# ggsave(paste0("./figure/", 'Fig1_Scatter', ".jpg"),
#        dpi = 300, width  = 5, height = 10)
