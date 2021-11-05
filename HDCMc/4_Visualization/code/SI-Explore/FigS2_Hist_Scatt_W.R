#########################################################################
Model_Base_Table_2021 <- Model_Base_Table_Update
setDT(Model_Base_Table_2021) 
season = "Winter"
Width = c(25, 1, 0.3) 
library(latex2exp)
file <- paste0("./figure/")
#########################################################################
#########################################################################
{
  PM25_CMAQ <- Model_Base_Table_2021 %>%  #201506,201507,201508
    filter(YEAR_MONTH%in% c(201511,201512,201601),  #,201511,201512,201601
           # SITEID %nin% c(3, 57, 62, 63, 8, 11,18,72)
           # , !is.na(REAL_PM25)
    ) %>%
    setorder(SITEID) %>% 
    dplyr::select(SITEID, CITY, CITY_NAME
                  , DATE_TIME
                  , YEAR, MONTH, DAY
                  , YEAR_MONTH
                  , REAL_PM25
                  , CMAQ_PM25
                  , CMAQ_PM25_30
                  , NA.Spline
                  , NA.Kriging
                  , CMAQ_ID
                  , LON, LAT
                  , LON_X, LAT_Y)
  setDF(PM25_CMAQ)
  PM25_CMAQ$REAL_PM25 = if_else(is.na(PM25_CMAQ$REAL_PM25)
                                , PM25_CMAQ$NA.Kriging
                                , PM25_CMAQ$REAL_PM25)
  # PM25_CMAQ$SQRT_BIAS25 = sqrt(PM25_CMAQ$REAL_PM25) - sqrt(PM25_CMAQ$CMAQ_PM25)
  # PM25_CMAQ$LOG_BIAS25 = log10(PM25_CMAQ$REAL_PM25) - log10(PM25_CMAQ$CMAQ_PM25)
  # PM25_CMAQ$BIAS25 = PM25_CMAQ$REAL_PM25 - PM25_CMAQ$CMAQ_PM25
  setDT(PM25_CMAQ)
  #########################################################################
  # pm25_cmaq_data<- plyr::ddply(PM25_CMAQ
  #                        , .(CITY, SITEID, MONTH, DATE_TIME)
  #                        , plyr::summarize
  #                        , REAL_PM25_Avg = mean(REAL_PM25 , na.rm = TRUE)
  #                        , SQRT_REAL_PM25_Avg = mean(sqrt(REAL_PM25)
  #                                                    , na.rm = TRUE)
  #                        , LOG_REAL_PM25_Avg = mean(log(REAL_PM25)
  #                                                   , na.rm = TRUE)
  #                        , CMAQ_PM25_Avg = mean(CMAQ_PM25 , na.rm = TRUE)
  #                        , SQRT_CMAQ_PM25_Avg = mean(sqrt(CMAQ_PM25)
  #                                                    , na.rm = TRUE)
  #                        , LOG_CMAQ_PM25_Avg = mean(log(CMAQ_PM25)
  #                                                   , na.rm = TRUE))
  pm25_cmaq_data <- PM25_CMAQ
  #####################################################################
  #  correlation map
  #####################################################################
  
  x0 = pm25_cmaq_data$REAL_PM25
  y0 = pm25_cmaq_data$CMAQ_PM25
  
  x1 = sqrt(pm25_cmaq_data$REAL_PM25)
  y1 = sqrt(pm25_cmaq_data$CMAQ_PM25)
  
  x2 = log(pm25_cmaq_data$REAL_PM25)
  y2 = log(pm25_cmaq_data$CMAQ_PM25)
  # par(mfrow = c(1, 3))
  # plot(x0, y0)
  # plot(x1, y1)
  # plot(x2, y2)
  
  r0 = round(cor(x0, y0), 3)
  r1 = round(cor(x1, y1), 3)
  r2 = round(cor(x2, y2), 3)
  
  da <- rbind(data.frame(CMAQ = x0, PM25 = y0, z = abs(x0 - y0),
                         r = r0, FLAG = "original scale", GROUP = 1),
              data.frame(CMAQ = x1, PM25 = y1, z = abs(x1 - y1),
                         r = r1, FLAG = "square root scale", GROUP = 2),
              data.frame(CMAQ = x2, PM25 = y2, z = abs(x2 - y2),
                         r = r2, FLAG = "log scale", GROUP = 3))
  setDT(da)
}
################################################################
library(latex2exp)
size = c(23, 20, 20)
g = 1
Da <- da[GROUP ==g]
Up <- 800
Low <- 5
{
  P1 <- ggplot(data = Da) +
    geom_point(aes(x= CMAQ, y = PM25, color = z), size = 1) +
    # facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    theme_bw() +  
    geom_abline(slope = 1, color = "gray", size = 1) +
    geom_abline(slope = 2, color = "gray", size = 0.5) +
    geom_abline(slope = 0.5, color = "gray", size = .5) +
    annotate(geom = "text", x = (Up - Low)*0.97/2,
             y = Up*0.94,
             angle = 60,
             label = "k = 2",
             color = "gray",
             size = 5) +
    annotate(geom = "text", x = (Up)*0.88,
             y = Up*0.86,
             angle = 40,
             label = "k = 1",
             size = 5,
             color = "gray",
             fontface = 1) +
    annotate(geom = "text", x = (Up)*0.93,
             y = Up*0.45,
             angle = 25,
             label = "k = 0.5",
             color = "gray",
             size = 5) +
    scale_colour_gradient(
      limits = c(min(Da$CMAQ, Da$PM25), 
                 max(Da$CMAQ, Da$PM2))
      , space = "Lab", low = 'green'
      # , mid = 'green'
      , high = 'red'
      # colours =c(upper,lower, upper)
      , na.value = "green") +
    geom_label(x= quantile(Da$CMAQ)[2], y = 780, 
               label = paste0("(a)"),
               size = 12, label.size = 0) +
    geom_label(x= quantile(Da$CMAQ)[4], y = 700, 
               label = paste0("Corr = ", r0),
               size = 10, label.size = 0) +
    geom_label(x= 520, y = 700, 
               label = paste0("11/01/15~01/31/16"),
               size = 10, label.size = 0) +
    scale_x_continuous(limits = c(0, 800)
                       , breaks = seq(0, 800, 150)
                       , labels = seq(0, 800, 150)
                       , expand = c(0, 0)
    ) +
    scale_y_continuous(limits = c(0, 800)
                       , breaks = seq(0, 800, 150)
                       , labels = seq(0, 800, 150)
                       , expand = c(0, 0)
    ) +
    labs(x = "CMAQ output at original scale", 
         y = TeX("Observed PM$_{2.5}$ at original scale")) +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )
  
}
g = 2
Da <- da[GROUP ==g]
Up <- 28
Low <- 0
#####################################################################
{
  P2 <- ggplot(data = Da) +
    geom_point(aes(x= CMAQ, y = PM25, color = z), size = 1) +
    # facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    theme_bw() +  
    geom_abline(slope = 1, color = "gray", size = 1) +
    geom_abline(slope = 2, color = "gray", size = 0.5) +
    geom_abline(slope = 0.5, color = "gray", size = .5) +
    annotate(geom = "text", x = (Up - Low)*0.93/2,
             y = Up*0.90,
             angle = 60,
             label = "k = 2",
             color = "gray",
             size = 5) +
    annotate(geom = "text", x = (Up)*0.90,
             y = Up*0.88,
             angle = 40,
             label = "k = 1",
             size = 5,
             color = "gray",
             fontface = 1) +
    annotate(geom = "text",x = (Up)*0.93,
             y = Up*0.45,
             angle = 25,
             label = "k = 0.5",
             color = "gray",
             size = 5) +
    scale_colour_gradient(
      limits = c(min(Da$CMAQ, Da$PM25), max(Da$CMAQ, Da$PM2))
      , space = "Lab", low = 'green'
      # , mid = 'green'
      , high = 'red'
      # colours =c(upper,lower, upper)
      , na.value = "green") +
    geom_label(x= 1.2, y = 26.3, 
               label = paste0("(b)"),
               size = 12, label.size = 0) +
    geom_label(x= 5, y = 23.8, 
               label = paste0("Corr = ", min(Da$r)),
               size = 10, label.size = 0) +
    scale_x_continuous(limits = c(0, max(Da$CMAQ, Da$PM25))
                       , breaks = (seq(0, 30, 5))
                       , labels = seq(0, 30, 5)
                       , expand = c(0, 0)
    ) +
    scale_y_continuous(limits = c(0, max(Da$CMAQ, Da$PM25))
                       , breaks = (seq(0, 30, 5))
                       , labels = seq(0, 30, 5)
                       , expand = c(0, 0)
    ) +
    labs(x = "CMAQ output at the square root scale",
         y = TeX("Observed PM$_{2.5}$ at the square root scale")) +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )
  }

# g = 3
# Da <- da[GROUP ==g]
# P3 <- ggplot(data = Da) +
#   geom_point(aes(x= CMAQ, y = PM25, color = z), size = 1) +
#   facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
#   theme_bw() +  
#   scale_colour_gradient(
#     limits = c(min(Da$CMAQ, Da$PM25), max(Da$CMAQ, Da$PM2))
#     , space = "Lab", low = 'green'
#     # , mid = 'green'
#     , high = 'red'
#     # colours =c(upper,lower, upper)
#     , na.value = "green") +
#   geom_label(x= quantile(Da$CMAQ)[1] + 1, y = max(Da$PM25), 
#             label = paste0("R = ", min(Da$r)),
#             size = 5) +
#   labs(x= "", y = "") + 
#   # labs(x = TeX("CMAQ PM2.5 ($μg/m^3$)"), y = TeX("Real PM2.5 ($μg/m^3$)")) +
#   theme(axis.text = element_text(size = 15, colour = "black")
#         , axis.title= element_text(size = 18, colour = "black")
#         # , legend.title = element_text(size = 10, colour = "black")
#         , legend.text= element_text(size = 15, colour = "black")
#         , legend.title = element_blank()
#         # , legend.position="top"
#         , legend.position = "none"
#         , legend.key.width = unit(1,"line")
#         , legend.key.height = unit(2,"line")
#         , strip.text =  element_text(size = 18, colour = "black")
#   )

#####################################################################
# library(cowplot)
# p <- cowplot::plot_grid(P1, P2, nrow = 1)
# ggsave(plot = p, paste0(file, 'FigS4_', season,".pdf"),  
#        dpi= 500, width = 20, height = 10)
#####################################################################
#####################################################################
size = c(23, 20, 20)
g = 1
y.h = 0.19
Da <- da[GROUP ==g]
{
  P1 <- ggplot(Da, aes(PM25))  + 
    geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1, 
                   position="identity"
                   ,binwidth = Width[1], colour = "black") +
    geom_label(x= 1, y = 0.19, label = paste0("(a)"),
               size = 12, label.size = 0) +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    theme_bw() + 
    scale_y_continuous(limits = c(0, y.h)) +
    # labs(x = " ", y = "Density") +
    labs(x = TeX("Observed PM$_{2.5}$ concentrations"), y = "Density") +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )
  
  g = 2
  Da <- da[GROUP ==g]
  P2 <- ggplot(Da, aes(PM25))  + 
    geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1, 
                   position="identity"
                   ,binwidth = Width[2], colour = "black") +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    geom_label(x= 1, y = 0.19, 
               label = paste0("(b)"),
               size = 12, label.size = 0) +
    # geom_label(x= 12, y = 0.16, 
    #            label = paste0("11/01/15~01/31/16"),
    #            size = 10, label.size = 0) +
    theme_bw() + 
    scale_y_continuous(limits = c(0, y.h)) +
    # labs(x = TeX("Observed PM$_{2.5}$ at different scales"), y = "") +
    labs(x = TeX("Observed PM$_{2.5}$ concentrations"), y = "Density") +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )
}

#####################################################################
g = 3
Da <- da[GROUP ==g]
{
  P3 <- ggplot(Da, aes(PM25))  + 
    geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1, 
                   position="identity"
                   , binwidth = Width[3], colour = "black")+
    geom_label(x= 0.5, y = 0.19, label = paste0("(c)"),
               size = 12, label.size = 0) +
    facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
    scale_x_continuous(limits = c(0, 8)
                       , breaks = seq(0, 8, 2)
                       , labels = seq(0, 8, 2)
                       , expand = c(0, 0)
    ) +
    theme_bw() + 
    # labs(x = "", y = "") +
    labs(x = TeX("Observed PM$_{2.5}$ concentrations"), y = "Density") +
    scale_y_continuous(limits = c(0, y.h)) +
    theme(axis.text = element_text(size = size[2], colour = "black")
          , axis.title= element_text(size = size[1], colour = "black")
          # , legend.title = element_text(size = 10, colour = "black")
          , legend.text= element_text(size = size[2], colour = "black")
          , legend.title = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          # , legend.position="top"
          , legend.position = "none"
          , legend.key.width = unit(1,"line")
          , legend.key.height = unit(2,"line")
          , strip.text =  element_text(size = size[3], colour = "black")
    )
}
#####################################################################
#####################################################################
library(cowplot)
p <- cowplot::plot_grid(P1, P2, P3, nrow = 1)
#####################################################################
#####################################################################
ggsave(plot = p, paste0(file, 'FigS2_', season,".png"),  
       dpi = 300, width = 20, height = 6.5)
#####################################################################
#####################################################################