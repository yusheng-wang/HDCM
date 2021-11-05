library(stBase)
data("SiteData", package = "stBase")
#########################################################################
setDT(Model_Base_Table_Update) # Summer  #Winter
season = "Winter"
# Width = c(7, 0.5, 0.2)
Width = c(25, 1, 0.3)  # winter

Model_2015_2017_Tab <- Model_Base_Table_Update
PM25_CMAQ <- Model_2015_2017_Tab %>%  #201506,201507,201508
  filter(YEAR_MONTH%in% c(201511,201512,201601),  #,201511,201512,201601
         SITEID %nin% c(3, 57, 62, 63, 8, 11,18,72)
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

pm25_cmaq_data<- ddply(PM25_CMAQ
                       , .(CITY, SITEID, MONTH, DATE_TIME )
                       , dplyr::summarize
                       , REAL_PM25_Avg = mean(REAL_PM25 , na.rm = TRUE)
                       , SQRT_REAL_PM25_Avg = mean(sqrt(REAL_PM25)
                                                   , na.rm = TRUE)
                       , LOG_REAL_PM25_Avg = mean(log(REAL_PM25)
                                                  , na.rm = TRUE)
                       , CMAQ_PM25_Avg = mean(CMAQ_PM25 , na.rm = TRUE)
                       , SQRT_CMAQ_PM25_Avg = mean(sqrt(CMAQ_PM25)
                                                   , na.rm = TRUE)
                       , LOG_CMAQ_PM25_Avg = mean(log(CMAQ_PM25)
                                                  , na.rm = TRUE))
################################################################
# 在月尺度
size = c(23, 20, 20)
P1 <- ggplot(pm25_cmaq_data, aes(REAL_PM25_Avg,
                                 fill = as.factor(MONTH),
                                 colour = as.factor(MONTH))) +
  geom_density(alpha = 0.1, adjust = 2) +
  # facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
  theme_bw() +  
  xlim(0, max(pm25_cmaq_data$REAL_PM25_Avg)) +
  labs(color = "Month", x = "Observed PM2.5 at the original scale",
       y = "Density") +
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
P2 <- ggplot(pm25_cmaq_data, aes(SQRT_REAL_PM25_Avg,
                                 fill = as.factor(MONTH),
                                 colour = as.factor(MONTH))) +
  geom_density(alpha = 0.1, adjust = 2) +
  # facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
  theme_bw() +  
  xlim(0, max(pm25_cmaq_data$SQRT_REAL_PM25_Avg)) +
  labs(fill = "Month", color = "Month", 
       x = "Observed PM2.5 at the square root scale"
       , y = "") +
  theme(axis.text = element_text(size = size[2], colour = "black")
        , axis.title= element_text(size = size[1], colour = "black")
        , legend.title = element_text(size = size[1], colour = "black")
        , legend.text= element_text(size = size[2], colour = "black")
        # , legend.title = element_blank()
        # , legend.position="top"
        # , legend.position = "none"
        , legend.key.width = unit(1,"line")
        , legend.key.height = unit(2,"line")
        , strip.text =  element_text(size = size[3], colour = "black")
  )
library(cowplot)
p <- cowplot::plot_grid(P1, P2, nrow = 1)
# p

FILE <- paste0("./7_Figure/")
ggsave(paste0(FILE, 'Density_Month_', season,".png"),  
       dpi= 500, width = 20, height = 10)
#####################################################################
#####################################################################
# 在City尺度
pm25_cmaq_data$size <- if_else(pm25_cmaq_data$CITY %in% 
                                 c("Zhangjiakou",
                                   "Tangshan"), "black", "red")
size = c(23, 20, 20)
P1 <- ggplot(pm25_cmaq_data) +
  geom_density(alpha = 0.2, adjust = 1.8,
               aes(REAL_PM25_Avg,
                   fill = CITY, 
                   # size = size,
                   color = CITY
               )) +
  # facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
  theme_bw() +  
  scale_x_continuous(limits = c(0,
                                max(pm25_cmaq_data$REAL_PM25_Avg)),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.025),
                     expand = c(0, 0)) +
  scale_size(guide = 'none') +
  labs(color = "City", fill = "City",
       x = "Observed PM2.5 with the original scale", 
       y = "Density") +
  theme(axis.text = element_text(size = size[2], colour = "black")
        , axis.title= element_text(size = size[1], colour = "black")
        # , legend.title = element_text(size = 10, colour = "black")
        # , legend.text= element_text(size = size[2], colour = "black")
        , legend.title = element_blank()
        # , legend.position="top"
        , legend.position = "none"
        # , legend.key.width = unit(1,"line")
        # , legend.key.height = unit(2,"line")
        , strip.text =  element_text(size = size[3], colour = "black")
  )
# P1
P2 <- ggplot(pm25_cmaq_data) +
  geom_density(alpha = 0.1, adjust = 2,
               aes(SQRT_REAL_PM25_Avg,
                   fill = CITY, 
                   # size = size,
                   colour = CITY)) +
  # facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
  theme_bw() +  
  # xlim(0, max(pm25_cmaq_data$SQRT_REAL_PM25_Avg)) +
  scale_x_continuous(limits = c(0,
                                max(pm25_cmaq_data$SQRT_REAL_PM25_Avg)),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.23),
                     expand = c(0, 0)) +
  scale_size(guide = 'none') +
  labs(color = "City", fill = "City", size = "City",
       x = "Observed PM2.5 at the square root scale",
       y = "") +
  theme(axis.text = element_text(size = size[2], colour = "black")
        , axis.title= element_text(size = size[1], colour = "black")
        , legend.title = element_text(size = size[1], colour = "black")
        , legend.text= element_text(size = size[2], colour = "black")
        # , legend.title = element_blank()
        # , legend.position="top"
        # , legend.position = "none"
        , legend.key.width = unit(1,"line")
        , legend.key.height = unit(2,"line")
        , strip.text =  element_text(size = size[3], colour = "black")
  )
library(cowplot)
p <- cowplot::plot_grid(P1, P2, nrow = 1)
# p

FILE <- paste0("./7_Figure/")
ggsave(paste0(FILE, 'Density_City_', season,".png"),  
       dpi= 500, width = 20, height = 10)
#####################################################################
#  相关性图
#####################################################################

x0 = pm25_cmaq_data$REAL_PM25_Avg
y0 = pm25_cmaq_data$CMAQ_PM25_Avg

x1 = pm25_cmaq_data$SQRT_REAL_PM25_Avg
y1 = pm25_cmaq_data$SQRT_CMAQ_PM25_Avg

x2 = pm25_cmaq_data$LOG_REAL_PM25_Avg
y2 = pm25_cmaq_data$LOG_CMAQ_PM25_Avg
# par(mfrow = c(1, 3))
# plot(x0, y0)
# plot(x1, y1)
# plot(x2, y2)

r0=round(cor(x0, y0), 3)
r1=round(cor(x1, y1), 3)
r2=round(cor(x2, y2), 3)

da <- rbind(data.frame(CMAQ = x0, PM25 = y0, z = abs(x0 - y0),
                       r=r0, FLAG = "original scale", GROUP = 1),
            data.frame(CMAQ = x1, PM25 = y1, z = abs(x1 - y1),
                       r=r1, FLAG = "square root scale", GROUP = 2),
            data.frame(CMAQ = x2, PM25 = y2, z = abs(x2 - y2),
                       r=r2, FLAG = "log scale", GROUP = 3))
setDT(da)


################################################################
library(latex2exp)
size = c(23, 20, 20)
g = 1
Da <- da[GROUP ==g]
P1 <- ggplot(data = Da) +
  geom_point(aes(x= CMAQ, y = PM25, color = z), size = 1) +
  # facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
  theme_bw() +  
  scale_colour_gradient(
    limits = c(min(Da$CMAQ, Da$PM25), max(Da$CMAQ, Da$PM2))
    , space = "Lab", low = 'green'
    # , mid = 'green'
    , high = 'red'
    # colours =c(upper,lower, upper)
    , na.value = "green") +
  geom_text(x= quantile(Da$CMAQ)[4], y = max(Da$PM25), 
            label = paste0("R = ", "0.588"),
            size = 10) +
  labs(x = "CMAQ output with original scale", y = "Observed PM2.5 with original scale") +
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

g = 2
Da <- da[GROUP ==g]
P2 <- ggplot(data = Da) +
  geom_point(aes(x= CMAQ, y = PM25, color = z), size = 1) +
  # facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
  theme_bw() +  
  scale_colour_gradient(
    limits = c(min(Da$CMAQ, Da$PM25), max(Da$CMAQ, Da$PM2))
    , space = "Lab", low = 'green'
    # , mid = 'green'
    , high = 'red'
    # colours =c(upper,lower, upper)
    , na.value = "green") +
  geom_text(x= quantile(Da$CMAQ)[2], y = max(Da$PM25), 
            label = paste0("R = ", min(Da$r)),
            size = 10) +
  labs(x = "CMAQ output with the square root scale", y ="Observed PM2.5 with the square root scale") +
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
#   geom_text(x= quantile(Da$CMAQ)[1] + 1, y = max(Da$PM25), 
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


library(cowplot)
p <- cowplot::plot_grid(P1, P2, nrow = 1)
# p


FILE <- paste0("./7_Figure/")
ggsave(paste0(FILE, 'Explore_Cor_', season,".png"),  
       dpi= 500, width = 20, height = 10)
#####################################################################
#####################################################################
size = c(23, 20, 20)
g = 1
y.h = 0.19
Da <- da[GROUP ==g]
P1 <- ggplot(Da, aes(PM25))  + 
  geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1, 
                 position="identity"
                 ,binwidth = Width[1], colour = "black") +
  facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
  theme_bw() + 
  scale_y_continuous(limits = c(0, y.h)) +
  labs(x = " ", y = "Density") +
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
P1
g = 2
Da <- da[GROUP ==g]
P2 <- ggplot(Da, aes(PM25))  + 
  geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1, 
                 position="identity"
                 ,binwidth = Width[2], colour = "black") +
  facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
  theme_bw() + 
  scale_y_continuous(limits = c(0, y.h)) +
  labs(x = "Observed PM2.5 with different scales", y = "") +
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
P2
g = 3
Da <- da[GROUP ==g]
P3 <- ggplot(Da, aes(PM25))  + 
  geom_histogram(aes(y=..density../sum(..density..)), alpha=0.1, 
                 position="identity"
                 ,binwidth = Width[3], colour = "black")+
  facet_wrap(~ FLAG, ncol = 1) +   #facet_grid
  theme_bw() + 
  labs(x = "", y = "") +
  scale_y_continuous(limits = c(0, y.h)) +
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
P3
library(cowplot)
p <- cowplot::plot_grid(P1, P2, P3, nrow = 1)
p

FILE <- paste0("./7_Figure/")
ggsave(paste0(FILE, 'Explore_Density_', season,".png"),  
       dpi= 500, width = 20, height = 10)