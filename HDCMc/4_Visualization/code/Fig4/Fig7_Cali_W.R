file = "./2_Full_Data_Model/"
Tab <- list.files(paste0(file, "/data/"))
data("SiteData", package = "stBase")
CityId <- unique(Site[, 1:2])
CityId$Id <- 1:nrow(CityId)
###################################################################
#                             HDCM
###################################################################
# in summer
City_Name <- "Hengshui"
CityId <- CityId[CityId$CITY %in% City_Name,]
Id <- CityId$Id

HDCM <- Tab[grepl("Fit_HDCM2_W", Tab)]
load(paste0(file, "data/",HDCM))
load(paste0(file, "data/", "train_HDCM2_W.RData"))
y.fit <- test.HDCM(test = train, Ks = Re$Ks, PIU = Re$PIU, seed = 1234)
y.fit <- (apply(y.fit, c(1, 2), quant))
da <- y.fit[,,Id] %>% apply( c(1, 2), mean) %>% as.data.frame()

HDCM2_fit <- data.frame(CITY = unique(CityId$CITY),
                        Fit.L25 = as.numeric(da[1, ]),
                        Fit.Median = as.numeric(da[2, ]),
                        Fit.U95 = as.numeric(da[3, ]),
                        DATE_TIME = as.Date(colnames(da)))

HDCM2_fit$Model <- "HDCM"
#CMAQ and Real data
Model_Base_Table_2021 <- Model_Base_Table_Update
# load("./5_1_Generate_Data/BaseTable/Model_2015_2017_Spline_Tab.Rda")
setDF(Model_Base_Table_2021)
Model_Base_Table_2021$True_REAL_PM25 <- Model_Base_Table_2021$REAL_PM25
# c(201506, 201507, 201508) 
# c(201511, 201512, 201601) 
MODE_BASE_TABLE <- Model_Base_Table_2021 %>% 
  filter(YEAR_MONTH %in% c(201511, 201512, 201601) 
         , CITY %in% c(City_Name)
  ) %>% setorder(DATE_TIME, SITEID) %>%
  ddply(.(CITY, DATE_TIME)
        , .fun = plyr::summarize
        # , LON = mean(LON, na.rm = TRUE)
        # , LAT = mean(LAT, na.rm = TRUE)
        # , FAC2 = mean(Pred.Median/REAL_PM25, na.rm = TRUE)
        , REAL_PM25 = mean(REAL_PM25, na.rm = TRUE)
        , CMAQ_PM25 = mean(CMAQ_PM25, na.rm = TRUE)
        , .progress = "text"
  )

CMAQ_PM25 <- MODE_BASE_TABLE[, c(1, 2, 4)]
CMAQ_PM25$Fit.L25 <- CMAQ_PM25$Fit.U95 <- NA
setnames(CMAQ_PM25, "CMAQ_PM25", "Fit.Median")

CMAQ_PM25$Model <- "CMAQ"
#Real data

Real_PM25 <- MODE_BASE_TABLE[, c(1, 2, 3)]
Real_PM25$Fit.L25 <- Real_PM25$Fit.U95 <- NA
setnames(Real_PM25, "REAL_PM25", "Fit.Median")
Real_PM25$Model = "Observation"
######################################################################
Da <- rbind(HDCM2_fit, Real_PM25, CMAQ_PM25) %>%
  filter(month(DATE_TIME) %in% 12,
         day(DATE_TIME) %in% c(17:22))



######################################################################
#             plot  credible  interval
######################################################################
label <- c("Observation", "Before calibration", "After calibration with 95% interval")

Bcol <- c("black", "grey80", "grey50")
######################################################################
Sys.setlocale("LC_TIME", "English")
######################################################################
size = c(0.4, 0.8, 4)
UP = max(Da$Fit.U95, na.rm = T) + 10
scal = 150
time <- unique(Da$DATE_TIME)
# Da$fill <- ifelse(Da$Model %in% "HDCM1.Ens", "red",
#                    ifelse(Da$Model %in% "HDCM2.Ens", "red",
#                           ifelse(Da$Model %in% "SVC1", "gray",
#                                  ifelse(Da$Model %in% "SVC2", "black",
#                                         ifelse(Da$Model %in% "CMAQ", "transparent", "yellow")))))
alpha <- c("1", "2", "3")
Da$alpha <- ifelse(Da$Model %in% "HDCM", alpha[3],
                   ifelse(Da$Model %in% "CMAQ",
                          alpha[2], alpha[1]))
S = c("1", "2", "3")
Da$size <- ifelse(Da$Model %in% "HDCM", S[3],
                  ifelse(Da$Model %in% "CMAQ",
                         S[2], S[1]))

Da$Model <- ifelse(Da$Model %in% "HDCM", "HDCM",
                   ifelse(Da$Model %in% "CMAQ",
                          "CMAQ", "Observation"))


Da$Model <- ordered(Da$Model, levels = c("Observation", "CMAQ",
                                         "HDCM"))
ls = c(1.3, 1.3, 1.3)
######################################################################
######################################################################
p <- ggplot(Da, aes(x = DATE_TIME, group  = Model)) +
  geom_ribbon(aes(ymin = Fit.L25, ymax = Fit.U95,
                  linetype = Model, fill = Model),
              alpha = 0.3, size = size[1]) +
  geom_line(aes(y = Fit.Median, linetype = Model, col = Model, 
                alpha = alpha, size = size)) +
  theme_bw() + 
  scale_colour_manual(name = '', values = c("black","gray50", 
                                            "#43a2ca"),
                      labels = label) +
  scale_fill_manual(name = '', values = c("transparent",
                                          "transparent",
                                          "#43a2ca"),
                    labels = label) +
  scale_alpha_manual(values = c(1, 1, 1)) + 
  scale_size_manual(values = ls) + 
  scale_linetype_manual(name = '',values=c("dashed", "twodash",
                                           "solid"),
                        labels = label) +
  # scale_x_continuous(expand = c(1e-2, 0)
  #                    , breaks  = unique(Da$DATE_TIME)[c(seq(1, 31, 7))]
  #                    , labels = c("Jun 01", "Jun 08","Jun 15","Jun 22","Jun 29")
  #                    # , labels = c("Nov 01", "Nov 08","Nov 15","Nov 22","Nov 29")
  #                    # , labels = c("Dec 01", "Dec 08","Dec 15","Dec 22","Dec 29")
  # ) +
  scale_x_continuous(
    # expand = c(1e-5, 0),
    breaks  = unique(Da$DATE_TIME)
    , labels = c("Dec 19, 2015", "Dec 20, 2015",
                 "Dec 21, 2015", "Dec 22, 2015",
                 "Dec 23, 2015", "Dec 24, 2015"
    )
    # , labels = c("Nov 01", "Nov 08","Nov 15","Nov 22","Nov 29")
    # , labels = c("Dec 01", "Dec 08","Dec 15","Dec 22","Dec 29")
  ) +
  scale_y_continuous(limits = c(0, UP), 
                     breaks  = seq(0, UP, scal), 
                     labels = seq(0, UP, scal)) +
  labs( x = "Date", fill = "",
        y = latex2exp::TeX("PM$_{2.5}$ $( \\mu g/m^3 )$")) +
  theme(axis.text = element_text(size = 20, colour = "black")
        ,axis.text.x = element_text(hjust = 0.6, size = 20, colour = "black") 
        , axis.title = element_text(size = 22, colour = "black")
        , legend.title = element_text(size = 20, colour = "black")
        , legend.text = element_text(size = 20, colour = "black")
        # , legend.title = element_blank()
        , legend.background = element_rect(colour = 'transparent'
                                           , fill = 'transparent')
        , legend.key.width = unit(5,"line")
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , legend.position = c(0.5, 0.9)
        # , legend.margin = margin(t = -0.1, unit='cm')
        , strip.text =  element_text(size = 16, colour = "black")) +
  guides(col = guide_legend(override.aes = list(size = ls),
                            nrow = 1, byrow = TRUE),
         alpha = "none", size = "none")
p
ggsave(plot = p, paste0("./figure/", 'Fig7_', City_Name, "_W", ".pdf"),
       dpi = 500, width  = 18, height = 5)













