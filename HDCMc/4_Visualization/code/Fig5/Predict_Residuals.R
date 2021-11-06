library(stBase)
source("./R/PSTVB_Packages.R")
data("SiteData", package = "stBase")
thres = 400
prob = 0.022
source("./4_Visualization/code/Fig5/Pred_Residual/Residual_S.R")
source("./4_Visualization/code/Fig5/Pred_Residual/Residual_W.R")
label <- c("CMAQ", "UK", "RF", "SVC", "HDCM")
{
  p1 <- ggplot(data = summer.Residual, aes(x = x, y = density)) + 
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
    theme_bw() + ylim(c(0, prob)) + xlim(-thres, thres) +
    geom_text(aes(x = -thres,#min(summer.Residual$x), 
                  y = prob,#max(summer.Residual$density)
                  , label =  "(a)"),
              # family = c("sans"),
              fontface = 'bold',
              color = "black", size = 10) +
    labs(x = TeX("Error (μg/m^3)"),
         y = "Density") +
    theme(axis.text = element_text(size = 20, colour = "black")
          ,axis.text.x = element_text(hjust = 0.25, size = 20, colour = "black") 
          , axis.title = element_text(size = 22, colour = "black")
          , legend.title = element_text(size = 20, colour = "black")
          , legend.text = element_text(size = 20, colour = "black")
          # , legend.title = element_blank()
          , legend.background = element_rect(colour = 'transparent'
                                             , fill = 'transparent')
          , legend.key.width = unit(5,"line")
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , legend.position = c(0.78, 0.8)
          # , legend.margin = margin(t = -0.1, unit='cm')
          , strip.text =  element_text(size = 20, colour = "black")) +
    guides(linetype = guide_legend(override.aes = list(size = 1.5),
                                   nrow = 3, byrow = TRUE))
}
# dev.off()

# label <- c("CMAQ", "UK", "RF", "SVC", "HDCM")
# pdf(file = "./figure/Fig4_predict_bias_W.pdf", width = 12, height = 8)
{
  p2 <- ggplot(data = winter.Residual, aes(x = x, y = density)) + 
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
    theme_bw() + ylim(c(0, prob)) + xlim(-thres, thres) +
    geom_text(aes(x = -thres,#min(summer.Residual$x), 
                  y = prob,#max(summer.Residual$density)
                  , label =  "(b)"),
              # family = c("sans"),
              fontface = 'bold',
              color = "black", size = 10) +
    labs(x = TeX("Error (μg/m^3)"),
         y = "Density") +
    theme(axis.text = element_text(size = 20, colour = "black")
          ,axis.text.x = element_text(hjust = 0.25, size = 20, colour = "black") 
          , axis.title = element_text(size = 22, colour = "black")
          , legend.title = element_text(size = 20, colour = "black")
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
# pdf(file = "./figure/Fig4_predict_bias.pdf", width = 20, height = 8)
# p <- ggpubr::ggarrange(p1, p2, nrow = 1, ncol = 2)
# p
# dev.off()

summer.Residual$Season = "summer of 2015"
winter.Residual$Season = "winter of 2015"
da <- rbind(summer.Residual, winter.Residual)
label <- c("CMAQ", "UK", "RF", "SVC", "HDCM")
{
pdf(file = "./figure/Fig5.pdf", width = 20, height = 8)
 ggplot(data = da, aes(x = x, y = density)) + 
    # geom_point(aes(shape = Model), size = 5) +
    geom_line(aes(linetype = Model, col = Model), size = 1.5) +
    geom_hline(yintercept = 0.0, col = "gray80", size = 0.8) +
    facet_wrap(~ Season, ncol = 2) +
    # scale_shape_manual(name = '', values =  c('*', '+'), labels = label) + 
    scale_linetype_manual(name = '', values=  c("longdash", "dotdash",
                                                "dotted", "dashed",
                                                "solid"),
                          labels = label) +
    scale_color_manual(name = '', values =  c("red", "blue", "#4a1486",
                                              "#31a354", "black"), 
                       labels = label)+
    geom_vline(xintercept = 0, col = "gray80", size = 0.8) +
    theme_bw() + ylim(c(0, prob)) + xlim(-250, 250) +
    #xlim(-thres, thres) +
    # geom_text(aes(x = -thres,#min(summer.Residual$x), 
    #               y = prob,#max(summer.Residual$density)
    #               , label =  "(b)"),
    #           # family = c("sans"),
    #           fontface = 'bold',
    #           color = "black", size = 10) +
    labs(x = TeX("Prediction error (μg/m^3)"),
         y = "Density") +
    theme(axis.text = element_text(size = 25, colour = "black")
          # ,axis.text.x = element_text(hjust = 0.25, size = 35, colour = "black") 
          , axis.title = element_text(size = 30, colour = "black")
          , legend.title = element_text(size = 30, colour = "black")
          , legend.text = element_text(size = 25, colour = "black")
          # , legend.title = element_blank()
          , legend.background = element_rect(colour = 'transparent'
                                             , fill = 'transparent')
          , legend.key.width = unit(10,"line")
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , legend.position =  "top"#c(0.25, 0.95)#"top"#
          # , legend.margin = margin(t = -0.1, unit='cm')
          , strip.text =  element_text(size = 30, colour = "black")) +
    guides(linetype = guide_legend(override.aes = list(size = 2.0),
                                   nrow = 1, byrow = TRUE))
}
dev.off()



























