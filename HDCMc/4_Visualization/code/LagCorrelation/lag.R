Model_Base_Table_2021 <- Model_Base_Table_Update
PM25_CMAQ <- Model_Base_Table_2021 %>%  #201506,201507,201508
  filter(YEAR_MONTH%in% c(201506, 201507, 201508),  #,201511,201512,201601
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

setDT(PM25_CMAQ)
Dist <- fields::rdist(Site[, c(6, 7)])
id <- Site$SITEID
n <- nrow(Site)
for(s1 in id){
  x <- PM25_CMAQ[SITEID == s1, REAL_PM25][]
  for(s2 in id){
    if(s2 != s1){
      y <- PM25_CMAQ[SITEID == s2, REAL_PM25]
    }
  }
}