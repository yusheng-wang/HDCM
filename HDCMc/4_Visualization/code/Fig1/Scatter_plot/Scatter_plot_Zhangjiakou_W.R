load("./data/Model_Base_Table_2021.RData")
colnames(Model_Base_Table_2021)

CITY.Name <- c("Zhangjiakou")
month <- c(201511, 201512, 201601)
Da <- Model_Base_Table_2021 %>% 
  filter(YEAR_MONTH %in% month
          , CITY %in% CITY.Name
  ) %>%
  setorder(DATE_TIME, SITEID)  
Up <- max(Da$CMAQ_PM25, Da$REAL_PM25) +20
Low <- 50
Da$Season <- "In the cool season"

time <- as_labeller(c(`1` = "11/01/15 to 01/31/16", 
                      `2` = "06/01/15 to 08/31/15"))

Da$type <- ifelse(Da$CITY %in% "Zhangjiakou", 1,
                  ifelse(Da$CITY %in% "Tangshan", 2, 3))

 ggplot(data = Da) +
  geom_point(aes(x= REAL_PM25, y = CMAQ_PM25),
             size = 1, col = "blue") +
   facet_grid(CITY~Season) +   #facet_grid
  theme_bw()  + 
   geom_abline(slope = 1, intercept = 0, 
               col = "gray", size = 1) +
   geom_abline(slope = 1, color = "#A9A9A9", size = 1) +
   geom_abline(slope = 2, color = "gray", size = 0.5) +
   geom_abline(slope = 0.5, color = "gray", size = .5) +
   scale_x_continuous(limits = c(0, Up)
                      , expand = c(0, 0)
   ) +
   scale_y_continuous(limits = c(0, Up)
                      , expand = c(0, 0)
   ) +
   geom_text(x = (Up - Low)*1.15/2,
             y = Up*0.90,
             angle = 65,
             label = "k = 2",
             color = "gray",
             size = 6) +
   geom_text(x = (Up)*0.90,
             y = Up*0.86,
             angle = 45,
             label = "k = 1",
             size = 6) +
   geom_text(x = (Up)*0.90,
             y = Up*0.48,
             angle = 28,
             label = "k = 0.5",
             color = "gray",
             size = 6) +
  labs(x = TeX("Observed PM$_{2.5}$ $( μg/m^3 )$"), 
       y = TeX("CMAQ output PM$_{2.5}$ $( μg/m^3 )$")) +
  theme(axis.text = element_text(size = 14, colour = "black")
        , axis.title= element_text(size = 16, colour = "black")
        # , legend.title = element_text(size = 10, colour = "black")
        , legend.text= element_text(size = 14, colour = "black")
        , legend.title = element_blank()
        , panel.grid.major = element_blank() 
        , panel.grid.minor = element_blank()
        # , legend.position="top"
        , legend.position = "none"
        , legend.key.width = unit(1,"line")
        , legend.key.height = unit(2,"line")
        , strip.text =  element_text(size = 16, colour = "black")
  )
 ggsave(paste0("./figure/", "Fig1_", CITY.Name, "_", month[1], ".jpg"), 
        dpi= 800, width  = 5.5, height = 5)