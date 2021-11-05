# run the following code 
source(paste0("./1_CrossValidation/code/Summary/Summary_summer.R"))
source(paste0("./1_CrossValidation/code/Summary/Summary_winter.R"))
# you will get the following table for comparision 
Results
Resultw
# from supporting information
Results.SI.table
Resultw.SI.table

writexl::write_xlsx(Results, path = "./1_CrossValidation/Results.xlsx")
writexl::write_xlsx(Resultw, path = "./1_CrossValidation/Resultw.xlsx")
writexl::write_xlsx(Results.SI.table, path = "./1_CrossValidation/SI_Results.xlsx")
writexl::write_xlsx(Resultw.SI.table, path = "./1_CrossValidation/SI_Resultw.xlsx")