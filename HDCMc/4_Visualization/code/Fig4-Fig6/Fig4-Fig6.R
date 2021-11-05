######################################################################
source("./R/PSTVB_Packages.R")
remove.packages("stBase")
install.packages("E:/Mirror/HDCM/stBase_1.0.zip", repos = NULL, type = "win.binary")
library(stBase)
######################################################################
file <- "./1_CrossValidation/"
day = 1:31
######################################################################
######################################################################

######################################################################
# Fig4-Beijing: credible interval 
######################################################################
CITY.Name <- "Zhangjiakou"
hdcm_tab <- c("HDCM", "S", CITY.Name)
svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
month <- c(201506)
p <- Credible_Beijing(file = file, hdcm_tab = hdcm_tab, 
                   svc_tab = svc_tab, month = month, 
                   day = day, seed = 1234)
ggsave(plot = p, paste0("./figure/", 'Fig4_',
                        CITY.Name, "_", month[1], ".pdf"),
       dpi = 500, width  = 16, height = 5)
######################################################################
# Fig4-Tangshan: credible interval 
######################################################################
CITY.Name <- "Beijing"
hdcm_tab <- c("HDCM", "W", CITY.Name)
svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
month <- c(201511, 201512, 201601)
p <- Credible_Tangshan(file = file, hdcm_tab = hdcm_tab, 
                      svc_tab = svc_tab, month = month, 
                      day = day, seed = 1234)
ggsave(paste0("./figure/", 'Fig4_', CITY.Name, "_", month[1], ".pdf"),
       dpi = 500, width  = 16, height = 5)
######################################################################
######################################################################
# Fig5-Baoding:  Cond.quantile 
######################################################################
CITY.Name <- "Baoding"
hdcm_tab <- c("HDCM", "S", CITY.Name)
svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
month = c(201506, 201507, 201508)
p1 <- CondQuantile_Baoding(file = file, hdcm_tab = hdcm_tab, 
                      svc_tab = svc_tab, month = month, 
                      day = day, seed = 1234)
p1
ggsave(paste0("./figure/", 'Fig5_', CITY.Name, "_S", ".pdf"),
       dpi = 500, width  = 12, height = 7)
######################################################################
# Fig5-Baoding:  Cond.quantile 
######################################################################
# CITY.Name <- "Zhangjiakou"
hdcm_tab <- c("HDCM", "W", CITY.Name)
svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
month = c(201511, 201512, 201601)
p2 <- CondQuantile_Cangzhou(file = file, hdcm_tab = hdcm_tab, 
                          svc_tab = svc_tab, month = month, 
                          day = day, seed = 1234)
p2
ggsave(paste0("./figure/", 'Fig5_', CITY.Name, "_W", ".pdf"),
       dpi = 500, width  = 12, height = 7)


P <- ggpubr::ggarrange(p1, p2, nrow = 2, ncol = 1)
ggsave(plot = P, paste0("./figure/", 'Fig5_', CITY.Name, ".pdf"),
       dpi= 500, width  = 12, height = 14)


######################################################################
######################################################################
# Fig6-Xingtai:  Scatter
######################################################################
data("SiteData", package = "stBase")
p <- list()
for (s in 1:13) {
        CITY.Name <- as.character(unique(Site$CITY))[s]
        hdcm_tab <- c("HDCM", "S", CITY.Name)
        svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
        month = c(201506, 201507, 201508)
       
        p[[s]] <- Scatter_Xingtai(file = file, hdcm_tab = hdcm_tab, 
                             svc_tab = svc_tab, month = month, 
                             day = day, seed = 1234, 
                             x.start = 30, corr_x = c(rep(7.5, 3)),
                             fac2_x = c(rep(6, 3)))
        # ggsave(paste0("./figure/", 'Fig6_', CITY.Name, "_S", ".pdf"),
        #        dpi= 500, width  = 10, height = 5)      
}
P <- ggpubr::ggarrange(p[[1]], p[[2]], p[[3]], p[[4]],
                       p[[5]], p[[6]], p[[7]], 
                       p[[9]], p[[10]], p[[11]], p[[12]],p[[13]],
                       nrow = 4, ncol = 3)
ggsave(plot = P, paste0("./figure/", 'Fig6_', "total", "_S", ".pdf"),
       dpi= 500, width  = 20, height = 16)


######################################################################
# Fig6-Handan:  Scatter
######################################################################
CITY.Name <- "Chengde"
hdcm_tab <- c("HDCM", "W", CITY.Name)
svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
month = c(201511, 201512, 201601)
p <- Scatter_Handan(file = file, hdcm_tab = hdcm_tab, 
                     svc_tab = svc_tab, month = month, 
                     day = day, seed = 1234)
ggsave(paste0("./figure/", 'Fig6_', CITY.Name, "_W", ".pdf"),
       dpi= 500, width  = 11, height = 4)
######################################################################