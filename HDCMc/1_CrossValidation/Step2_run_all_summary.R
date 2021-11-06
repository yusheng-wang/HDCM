# run the following code 
source(paste0("./1_CrossValidation/Summary_for_all_models/Summary_summer.R"))
source(paste0("./1_CrossValidation/Summary_for_all_models/Summary_winter.R"))
# you will get the following table for comparision 
Results
Resultw
# from supporting information
Results.SI.table
Resultw.SI.table

writexl::write_xlsx(Results, path = "./1_CrossValidation/RMSE_CRPS_S.xlsx")
writexl::write_xlsx(Resultw, path = "./1_CrossValidation/RMSE_CRPS_W.xlsx")
writexl::write_xlsx(Results.SI.table, path = "./1_CrossValidation/FAC2_Corr_S.xlsx")
writexl::write_xlsx(Resultw.SI.table, path = "./1_CrossValidation/FAC2_Corr_W.xlsx")