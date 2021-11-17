######################################################################
source("./R/PSTVB_Packages.R")
source("./4_Visualization/code/Fig6/CondQuantile_Baoding_s.R")
source("./4_Visualization/code/Fig6/CondQuantile_Baoding_w.R")
file <- "./1_CrossValidation/"
day = 1:31
######################################################################
# Fig5-Baoding:  Cond.quantile 
######################################################################
CITY.Name <- "Baoding"
hdcm_tab <- c("HDCM", "S", CITY.Name)
svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
month = c(201506, 201507, 201508)
p1 <- CondQuantile_Baoding_s(file = file, hdcm_tab = hdcm_tab, 
                           svc_tab = svc_tab, month = month, 
                           day = day, seed = 1234)
ggsave(plot = p1, file = "./figure/Fig6_a.pdf",
       width  = 12, height = 7)
######################################################################
# Fig5-Baoding:  Cond.quantile 
######################################################################
hdcm_tab <- c("HDCM", "W", CITY.Name)
svc_tab <- c("SVC", hdcm_tab[2], CITY.Name)
month = c(201511, 201512, 201601)
p2 <- CondQuantile_Baoding_w(file = file, hdcm_tab = hdcm_tab,
                             svc_tab = svc_tab, month = month,
                             day = day, seed = 1234)
ggsave(plot = p2, file = "./figure/Fig6_b.pdf",
       width  = 12, height = 7)
