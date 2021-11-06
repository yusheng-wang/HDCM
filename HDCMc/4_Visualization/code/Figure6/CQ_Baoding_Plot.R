######################################################################
source("./R/PSTVB_Packages.R")
file <- "./1_CrossValidation/"
day = 1:31
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
ggsave(plot = p1, paste0("./figure/", 'Fig6_a', CITY.Name, "_S", ".pdf"),
       dpi = 500, width  = 12, height = 7)
######################################################################
# Fig5-Baoding:  Cond.quantile 
######################################################################
hdcm_tab <- c("HDCM", "W", CITY.Name)
svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
month = c(201511, 201512, 201601)
p2 <- CondQuantile_Cangzhou(file = file, hdcm_tab = hdcm_tab, 
                            svc_tab = svc_tab, month = month, 
                            day = day, seed = 1234)
ggsave(plot = p1, paste0("./figure/", 'Fig6_b', CITY.Name, "_W", ".pdf"),
       dpi = 500, width  = 12, height = 7)
