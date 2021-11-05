Credible_plot <- function(file, hdcm_tab, svc_tab, month, day, seed)
{
  if(length(hdcm_tab) == 4){CITY.Name <- hdcm_tab[4]}else{
    CITY.Name <- hdcm_tab[3]
  }
  
  ######################################################################
  HDCM_SVC <- load_HDCM_SVC_data(file = file, hdcm_tab = hdcm_tab, day = day,
                                 month = month, seed = seed,
                                 svc_tab = svc_tab)
  
  # save(HDCM_SVC, file = paste0(file, "/data/HDCM_SVC_", CITY.Name,
  #                                   ifelse(min(month) %in% c(201511, 201512, 201601),
  #                                          "_W", "_S"), ".RData"))
  CMAQ_PM25 <- HDCM_SVC$CMAQ_PM25
  HDCM.Ens <- HDCM_SVC$HDCM.Ens
  SVC <- HDCM_SVC$SVC
  ######################################################################
  ######################################################################
  Real_PM25 <- CMAQ_PM25
  Real_PM25$Pred.Median = CMAQ_PM25$REAL_PM25
  Real_PM25$Model = "Observation"
  Real_PM25[, c(5, 6, 8, 9, 10, 11, 12, 13)] = NA
  CMAQ_PM25[, c(5, 6)] = NA
  ######################################################################
  Da <- rbind(Real_PM25, CMAQ_PM25, SVC, HDCM.Ens)
  ######################################################################
  #             plot  credible  interval
  ######################################################################
  label <- c("Observation", "CMAQ", "SVC with 95% interval", "HDCM with 95% interval")
  Bcol <- c("black", "grey80", "grey50", "red")
  ######################################################################
  Sys.setlocale("LC_TIME", "English")
  ######################################################################
  size = c(0.4, 0.8, 4)
  UP = max(Da$Pred.U95, na.rm = T) + 10
  scal = 60
  time <- unique(Da$DATE_TIME)
  # Da$fill <- ifelse(Da$Model %in% "HDCM1.Ens", "red",
  #                    ifelse(Da$Model %in% "HDCM2.Ens", "red",
  #                           ifelse(Da$Model %in% "SVC1", "gray",
  #                                  ifelse(Da$Model %in% "SVC2", "black",
  #                                         ifelse(Da$Model %in% "CMAQ", "transparent", "yellow")))))
  alpha <- c("1", "2", "3", "4")
  Da$alpha <- ifelse(Da$Model %in% "HDCM.Ens", alpha[4],
                     ifelse(Da$Model %in% "SVC", alpha[3],
                            ifelse(Da$Model %in% "CMAQ",
                                   alpha[2], alpha[1])))
  S = c("1", "2", "3", "4")
  Da$size <- ifelse(Da$Model %in% "HDCM.Ens", S[4],
                    ifelse(Da$Model %in% "SVC", S[3],
                           ifelse(Da$Model %in% "CMAQ",
                                  S[2], S[1])))
  
  Da$Model <- ifelse(Da$Model %in% "HDCM.Ens", "HDCM",
                     ifelse(Da$Model %in% "SVC", "SVC",
                            ifelse(Da$Model %in% "CMAQ", "CMAQ", "Observation")))
  
  
  Da$Model <- ordered(Da$Model, levels = c("Observation", "CMAQ", "SVC", "HDCM"))
  ls = c(1.3, 1.3, 1.3, 1.3)
  ######################################################################
  ######################################################################
  p <- ggplot(Da, aes(DATE_TIME, group  = Model)) +
    geom_ribbon(aes(ymin = Pred.L25, ymax = Pred.U95,
                    linetype = Model, fill = Model), 
                alpha = 0.3, size = size[1]) +
    geom_line(aes(y = Pred.Median, linetype = Model, col = Model, 
                  alpha = alpha, size = size)) +
    theme_bw() + 
    scale_colour_manual(name = '', values = c("gray40","gray50", 
                                              "#fcbba1", "#43a2ca"),
                        labels = label) +
    scale_fill_manual(name = '', values = c("transparent",
                                            "transparent",
                                            "#fcbba1",
                                            "#43a2ca"),
                      labels = label) +
    scale_alpha_manual(values = c(1, 1, 1, 1)) + 
    scale_size_manual(values = ls) + 
    scale_linetype_manual(name = '',values=c("dashed", "solid",
                                             "solid","solid"),
                          labels = label) +
    scale_x_continuous(expand = c(1e-2, 0)
                       , breaks  = unique(Da$DATE_TIME)[c(seq(1, 31, 7))]
                       , labels = c("Jun 01", "Jun 08","Jun 15","Jun 22","Jun 29")
                       # , labels = c("Nov 01", "Nov 08","Nov 15","Nov 22","Nov 29")
                       # , labels = c("Dec 01", "Dec 08","Dec 15","Dec 22","Dec 29")
    ) +
    scale_y_continuous(limits = c(0, UP), 
                       breaks  = seq(0, UP, scal), 
                       labels = seq(0, UP, scal)) +
    labs( x = "Date", fill = "",
          y = TeX("Observed and predicted PM$_{2.5}$ $( μg/m^3 )$")) +
    theme(axis.text = element_text(size = 18, colour = "black")
          ,axis.text.x = element_text(hjust = 0.25, size = 16, colour = "black") 
          , axis.title = element_text(size = 18, colour = "black")
          , legend.title = element_text(size = 18, colour = "black")
          , legend.text = element_text(size = 16, colour = "black")
          # , legend.title = element_blank()
          , legend.background = element_rect(colour = 'transparent'
                                             , fill = 'transparent')
          , legend.key.width = unit(5,"line")
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , legend.position = c(0.4, 0.9)
          # , legend.margin = margin(t = -0.1, unit='cm')
          , strip.text =  element_text(size = 16, colour = "black")) +
    guides(col = guide_legend(override.aes = list(size = ls),
                              nrow = 2, byrow = TRUE),
           alpha = F, size = F)
  return(p)
}
