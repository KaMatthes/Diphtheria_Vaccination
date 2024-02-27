population_canton <- read.xlsx("data_raw/Population_Canton.xlsx", detectDates = TRUE) %>%
  gather(., Canton2, population,2:27, factor_key=TRUE)  %>%
  mutate(Canton2=recode(Canton2, "CH" ="Total")) %>%
  rename(Year=Jahr)
  

cases_canton_monthly <- read.xlsx("data_raw/Cases_canton_month.xlsx", detectDates = TRUE) %>%
  gather(., Datum, cases,2:713, factor_key=TRUE) %>%
  mutate(Datum = ymd(Datum),
         Year = year(Datum),
         Month = month(Datum),
         Canton2 = Canton,
         Canton2 = recode(Canton2, 
                          "Zürich" = "ZH",
                          "Bern" = "BE",
                          "Luzern" = "LU",
                          "Uri" = "UR",
                          "Schwyz" = "SZ",
                          "Obwalden" ="OW",
                          "Nidwalden" ="NW",
                          "Glarus" = "GL",
                          "Zug" = "ZG",
                          "Fribourg" = "FR",
                          "Solothurn" = "SO",
                          "Baselstadt" = "BS",
                          "Baselland" = "BL",
                          "Schaffhausen" = "SH",
                          "Appenzell AR" = "AR",
                          "Appenzell IR" = "AI",
                          "St. Gallen" = "SG",
                          "Graubünden" = "GR",
                          "Aargau" = "AG",
                          "Thurgau" = "TG",
                          "Tessin" = "TI",
                          "Vaud" = "VD",
                          "Valais" = "VS",
                          "Neuchâtel" = "NE",
                          "Genève" = "GE"),
         Type_vaccination = Canton2,
         Type_vaccination = recode(Type_vaccination,
                           "ZH" = "Recommendation",
                           "BE" = "Recommendation",
                           "LU" = "Recommendation",
                           "UR" = "Recommendation",
                           "SZ" = "Recommendation",
                           "OW" = "Recommendation",
                           "NW" = "Recommendation",
                           "GL" = "Recommendation",
                           "ZG" = "Recommendation",
                           "FR" = "Obligatory",
                           "SO" = "Recommendation",
                           "BS" = "Recommendation",
                           "BL" = "Recommendation",
                           "SH" = "Recommendation",
                           "AR" = "Recommendation",
                           "AI" = "Recommendation",
                           "SG" = "Recommendation",
                           "GR" = "Recommendation",
                           "AG" = "Recommendation",
                           "TG" = "Recommendation",
                           "TI" = "Obligatory",
                           "VD" = "Obligatory",
                           "VS" = "Recommendation",
                           "NE" = "Obligatory",
                           "GE" = "Obligatory")) %>%
  arrange(Datum) %>%
  group_by(Canton2) %>%
  mutate(cases =  as.integer(round(na_kalman(cases),0))) %>%
  ungroup() %>%
  mutate(Canton3 = Canton,
         Canton3 = recode(Canton3, 
                 "Zürich" = "Total_GE",
                 "Bern" = "Total_GE",
                 "Luzern" = "Total_GE",
                 "Uri" = "Total_GE",
                 "Schwyz" = "Total_GE",
                 "Obwalden" ="Total_GE",
                 "Nidwalden" = "Total_GE",
                 "Glarus" = "Total_GE",
                 "Zug" = "Total_GE",
                 "Fribourg" = "Total_GE",
                 "Solothurn" = "Total_GE",
                 "Baselstadt" = "Total_GE",
                 "Baselland" = "Total_GE",
                 "Schaffhausen" = "Total_GE",
                 "Appenzell AR" = "Total_GE",
                 "Appenzell IR" = "Total_GE",
                 "St. Gallen" = "Total_GE",
                 "Graubünden" = "Total_GE",
                 "Aargau" = "Total_GE",
                 "Thurgau" = "Total_GE",
                 "Tessin" = "Total_GE",
                 "Vaud" = "Total_GE",
                 "Valais" = "Total_GE",
                 "Neuchâtel" = "Total_GE",
                 "Genève" = "GE_2")) %>%
  group_by(Canton3, Datum) %>%
  mutate(cases_c3 =   sum(cases, na.rm =TRUE)) %>%
  ungroup() %>%
  dplyr::group_by(Type_vaccination,Datum) %>%
  dplyr::mutate(cases_l = sum(cases, na.rm =TRUE)) %>%
  ungroup() %>%
  filter(!Year==1909) %>%
  left_join(population_canton) %>%
  dplyr::group_by(Type_vaccination,Datum) %>%
  dplyr::mutate(pop_l =sum(population, na.rm =TRUE)) %>%
  ungroup() %>%
  dplyr::group_by(Canton3,Datum) %>%
  dplyr::mutate(pop_c3 =sum(population, na.rm =TRUE)) %>%
  ungroup()

save(cases_canton_monthly  ,file="data/cases_canton_monthly.RData")
write.xlsx(cases_canton_monthly,file="data/cases_canton_monthly.xlsx",rowNames=FALSE, overwrite = TRUE)

cases_canton_yearly <- cases_canton_monthly %>%
  group_by(Canton2,Year) %>%
  mutate(cases_year = sum(cases, na.rm =TRUE)) %>%
  ungroup() %>% 
  group_by(Canton3,Year) %>%
  mutate(cases_year_c3 = sum(cases_c3, na.rm =TRUE)) %>%
  ungroup() %>% 
  group_by(Type_vaccination, Canton,Year) %>%
  mutate(cases_year_l = sum(cases_l, na.rm =TRUE)) %>%
  ungroup() %>%
  distinct(Year, Canton2, .keep_all = TRUE) 


cases_2010 <- read.xlsx("data_raw/Cases_2010.xlsx", detectDates = TRUE) %>%
  mutate(Canton2 = "Total") %>%
  rename(cases_year=cases) %>%
  left_join(population_canton) %>%
  mutate(population = ifelse(Year==2023, 8902308, population))


cases_canton_yearly  <- cases_canton_yearly %>%
  bind_rows(.,cases_2010) %>%
  arrange(Year)

save(cases_canton_yearly  ,file="data/cases_canton_yearly.RData")
write.xlsx(cases_canton_yearly,file="data/cases_canton_yearly.xlsx",rowNames=FALSE, overwrite = TRUE)

death_canton_year <- read.xlsx("data_raw/Death_Canton.xlsx", detectDates = TRUE) %>%
  gather(., Canton2, death,2:27, factor_key=TRUE) %>%
  filter(Year > 1909) %>%
  mutate( Canton = Canton2,
          Canton = recode(Canton2, 
                           "ZH" = "Zürich",
                           "BE" = "Bern",
                           "LU" = "Luzern",
                           "UR" ="Uri",
                           "SZ" = "Schwyz",
                           "OW" = "Obwalden",
                           "NW" = "Nidwalden",
                           "GL" = "Glarus",
                           "ZG" = "Zug",
                           "FR" = "Fribourg",
                           "SO" = "Solothurn",
                           "BS" = "Baselstadt",
                           "BL" = "Baselland",
                           "SH" = "Schaffhausen",
                           "AR" = "Appenzell AR",
                           "AI" = "Appenzell IR",
                           "SG" = "St. Gallen",
                           "GR" = "Graubünden",
                           "AG" = "Aargau",
                           "TG" = "Thurgau",
                           "TI" = "Tessin",
                           "VD" = "Vaud",
                           "VS" = "Valais",
                           "NE" = "Neuchâtel",
                           "GE" = "Genève"),
          Type_vaccination = Canton2,
          Type_vaccination = recode(Type_vaccination,
                            "ZH" = "Recommendation",
                            "BE" = "Recommendation",
                            "LU" = "Recommendation",
                            "UR" = "Recommendation",
                            "SZ" = "Recommendation",
                            "OW" = "Recommendation",
                            "NW" = "Recommendation",
                            "GL" = "Recommendation",
                            "ZG" = "Recommendation",
                            "FR" = "Obligatory",
                            "SO" = "Recommendation",
                            "BS" = "Recommendation",
                            "BL" = "Recommendation",
                            "SH" = "Recommendation",
                            "AR" = "Recommendation",
                            "AI" = "Recommendation",
                            "SG" = "Recommendation",
                            "GR" = "Recommendation",
                            "AG" = "Recommendation",
                            "TG" = "Recommendation",
                            "TI" = "Obligatory",
                            "VD" = "Obligatory",
                            "VS" = "Recommendation",
                            "NE" = "Obligatory",
                            "GE" = "Obligatory")) %>%
  arrange(Year) %>%
  group_by(Canton2) %>%
  mutate(death =  as.integer(round(na_kalman(death),0))) %>%
  ungroup() %>%
  mutate(Canton3 = Canton,
         Canton3 = recode(Canton3, 
                          "Zürich" = "Total_GE",
                          "Bern" = "Total_GE",
                          "Luzern" = "Total_GE",
                          "Uri" = "Total_GE",
                          "Schwyz" = "Total_GE",
                          "Obwalden" ="Total_GE",
                          "Nidwalden" = "Total_GE",
                          "Glarus" = "Total_GE",
                          "Zug" = "Total_GE",
                          "Fribourg" = "Total_GE",
                          "Solothurn" = "Total_GE",
                          "Baselstadt" = "Total_GE",
                          "Baselland" = "Total_GE",
                          "Schaffhausen" = "Total_GE",
                          "Appenzell AR" = "Total_GE",
                          "Appenzell IR" = "Total_GE",
                          "St. Gallen" = "Total_GE",
                          "Graubünden" = "Total_GE",
                          "Aargau" = "Total_GE",
                          "Thurgau" = "Total_GE",
                          "Tessin" = "Total_GE",
                          "Vaud" = "Total_GE",
                          "Valais" = "Total_GE",
                          "Neuchâtel" = "Total_GE",
                          "Genève" = "GE_2")) %>%
  group_by(Canton3, Year) %>%
  mutate(death_c3 =   sum(death, na.rm =TRUE)) %>%
  ungroup() %>%
  dplyr::group_by(Type_vaccination,Year) %>%
  dplyr::mutate(death_l = sum(death, rm.na=TRUE)) %>%
  ungroup() %>%
  left_join(population_canton ) %>%
  dplyr::group_by(Type_vaccination,Year) %>%
  dplyr::mutate(pop_l =sum(population, rm.na=TRUE)) %>%
  ungroup() %>%
  dplyr::group_by(Canton3,Year) %>%
  dplyr::mutate(pop_c3 =sum(population, rm.na=TRUE)) %>%
  ungroup()


death_1969 <- read.xlsx("data_raw/Deaths_1969.xlsx", detectDates = TRUE) %>%
  mutate(Canton2 = "Total") %>%
  left_join(population_canton) %>%
  mutate(population = ifelse(Year==2023, 8902308, population))


death_canton_year <- death_canton_year %>%
  bind_rows(.,death_1969) %>%
  arrange(Year)

save(death_canton_year ,file="data/death_canton_year.RData")
write.xlsx(death_canton_year,file="data/death_canton_year.xlsx",rowNames=FALSE, overwrite = TRUE)


hospital_weekly <-  read.xlsx("data_raw/Cases_hospital_weekly.xlsx", detectDates = TRUE) %>%
  gather(., Datum, cases,2:1515, factor_key=TRUE) %>%
  mutate(Datum=ymd(Datum),
         Canton2 = Canton,
         Canton2 = recode(Canton2, 
                   "Zürich" = "ZH",
                   "Bern" = "BE",
                   "Luzern" = "LU",
                   "Uri" = "UR",
                   "Schwyz" = "SZ",
                   "Obwalden" ="OW",
                   "Nidwalden" ="NW",
                   "Glarus" = "GL",
                   "Zug" = "ZG",
                   "Fribourg" = "FR",
                   "Solothurn" = "SO",
                   "Baselstadt" = "BS",
                   "Baselland" = "BL",
                   "Schaffhausen" = "SH",
                   "Appenzell AR" = "AR",
                   "Appenzell IR" = "AI",
                   "St. Gallen" = "SG",
                   "Graubünden" = "GR",
                   "Aargau" = "AG",
                   "Thurgau" = "TG",
                   "Tessin" = "TI",
                   "Vaud" = "VD",
                   "Valais" = "VS",
                   "Neuchâtel" = "NE",
                   "Genève" = "GE")) 

hospital_total <- hospital_weekly %>%
  filter(Canton2=="Total")
monthly_total <- weekToMonth(hospital_total$cases, datStart =hospital_total$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="Total",
         cases_month = round(value,2))


hospital_ZH <- hospital_weekly %>%
  filter(Canton2=="ZH")
monthly_ZH <- weekToMonth(hospital_ZH$cases, datStart =hospital_ZH$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="ZH",
         cases_month = round(value,2))

hospital_BE <- hospital_weekly %>%
  filter(Canton2=="BE")
monthly_BE <- weekToMonth(hospital_BE$cases, datStart =hospital_BE$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="BE",
         cases_month = round(value,2))

hospital_LU <- hospital_weekly %>%
  filter(Canton2=="LU")
monthly_LU <- weekToMonth(hospital_LU$cases, datStart =hospital_LU$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="LU",
         cases_month = round(value,2))

hospital_UR <- hospital_weekly %>%
  filter(Canton2=="UR")
monthly_UR <- weekToMonth(hospital_UR$cases, datStart =hospital_UR$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="UR",
         cases_month = round(value,2))

hospital_SZ <- hospital_weekly %>%
  filter(Canton2=="SZ")
monthly_SZ <- weekToMonth(hospital_SZ$cases, datStart =hospital_SZ$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="SZ",
         cases_month = round(value,2))

hospital_OW <- hospital_weekly %>%
  filter(Canton2=="OW")
monthly_OW <- weekToMonth(hospital_OW$cases, datStart =hospital_OW$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="OW",
         cases_month = round(value,2))

hospital_NW <- hospital_weekly %>%
  filter(Canton2=="NW")
monthly_NW <- weekToMonth(hospital_NW$cases, datStart =hospital_NW$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="NW",
         cases_month = round(value,2))

hospital_GL <- hospital_weekly %>%
  filter(Canton2=="GL")
monthly_GL <- weekToMonth(hospital_GL$cases, datStart =hospital_GL$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="GL",
         cases_month = round(value,2))

hospital_ZG <- hospital_weekly %>%
  filter(Canton2=="ZG")
monthly_ZG <- weekToMonth(hospital_ZG$cases, datStart =hospital_ZG$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="ZG",
         cases_month = round(value,2))

hospital_FR <- hospital_weekly %>%
  filter(Canton2=="FR")
monthly_FR <- weekToMonth(hospital_FR$cases, datStart =hospital_FR$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="FR",
         cases_month = round(value,2))

hospital_SO <- hospital_weekly %>%
  filter(Canton2=="SO")
monthly_SO <- weekToMonth(hospital_SO$cases, datStart =hospital_SO$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="SO",
         cases_month = round(value,2))

hospital_BS <- hospital_weekly %>%
  filter(Canton2=="BS")
monthly_BS <- weekToMonth(hospital_BS$cases, datStart =hospital_BS$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="BS",
         cases_month = round(value,2))


hospital_BL <- hospital_weekly %>%
  filter(Canton2=="BL")
monthly_BL <- weekToMonth(hospital_BL$cases, datStart =hospital_BL$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="BL",
         cases_month = round(value,2))

hospital_SH <- hospital_weekly %>%
  filter(Canton2=="SH")
monthly_SH <- weekToMonth(hospital_SH$cases, datStart =hospital_SH$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="SH",
         cases_month = round(value,2))

hospital_AR <- hospital_weekly %>%
  filter(Canton2=="AR")
monthly_AR <- weekToMonth(hospital_AR$cases, datStart =hospital_AR$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="AR",
         cases_month = round(value,2))

hospital_AI <- hospital_weekly %>%
  filter(Canton2=="AI")
monthly_AI <- weekToMonth(hospital_AI$cases, datStart =hospital_AI$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="AI",
         cases_month = round(value,2))

hospital_SG <- hospital_weekly %>%
  filter(Canton2=="SG")
monthly_SG <- weekToMonth(hospital_SG$cases, datStart =hospital_SG$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="SG",
         cases_month = round(value,2))

hospital_GR <- hospital_weekly %>%
  filter(Canton2=="GR")
monthly_GR <- weekToMonth(hospital_GR$cases, datStart =hospital_GR$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="GR",
         cases_month = round(value,2))

hospital_AG <- hospital_weekly %>%
  filter(Canton2=="AG")
monthly_AG <- weekToMonth(hospital_AG$cases, datStart =hospital_AG$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="AG",
         cases_month = round(value,2))

hospital_TG <- hospital_weekly %>%
  filter(Canton2=="TG")
monthly_TG <- weekToMonth(hospital_TG$cases, datStart =hospital_TG$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="TG",
         cases_month = round(value,2))


hospital_TI <- hospital_weekly %>%
  filter(Canton2=="TI")
monthly_TI <- weekToMonth(hospital_TI$cases, datStart =hospital_TI$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="TI",
         cases_month = round(value,2))


hospital_VD <- hospital_weekly %>%
  filter(Canton2=="VD")
monthly_VD <- weekToMonth(hospital_VD$cases, datStart =hospital_VD$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="VD",
         cases_month = round(value,2))

hospital_VS <- hospital_weekly %>%
  filter(Canton2=="VS")
monthly_VS <- weekToMonth(hospital_VS$cases, datStart =hospital_VS$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="VS",
         cases_month = round(value,2))

hospital_NE <- hospital_weekly %>%
  filter(Canton2=="NE")
monthly_NE <- weekToMonth(hospital_NE$cases, datStart =hospital_NE$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="NE",
         cases_month = round(value,2))

hospital_GE <- hospital_weekly %>%
  filter(Canton2=="GE")
monthly_GE <- weekToMonth(hospital_GE$cases, datStart =hospital_GE$Datum, wkMethod = "ISO") %>%
  mutate(Canton2="GE",
         cases_month = round(value,2))

hospital_week_month <- monthly_total %>%
  rbind( monthly_AG) %>%
  rbind(monthly_AI) %>%
  rbind(monthly_AR) %>%
  rbind(monthly_BE) %>%
  rbind(monthly_BL) %>%
  rbind(monthly_BS) %>%
  rbind(monthly_FR) %>%
  rbind(monthly_GE) %>%
  rbind(monthly_GL) %>%
  rbind(monthly_GR) %>%
  rbind(monthly_LU) %>%
  rbind(monthly_NE) %>%
  rbind(monthly_NW) %>%
  rbind(monthly_OW) %>%
  rbind(monthly_SG) %>%
  rbind(monthly_SH) %>%
  rbind(monthly_SO) %>%
  rbind(monthly_SZ) %>%
  rbind(monthly_TG) %>%
  rbind(monthly_TI) %>%
  rbind(monthly_UR) %>%
  rbind(monthly_VD) %>%
  rbind(monthly_VS) %>%
  rbind(monthly_ZG) %>%
  rbind(monthly_ZH) %>%
  rename(Datum=yearMonth) %>%
  select(-value)

hospital_monthly <-  read.xlsx("data_raw/Cases_hospital_monthly.xlsx", detectDates = TRUE) %>%
  gather(., Datum, cases_month,2:18, factor_key=TRUE) %>%
  mutate(Datum=ymd(Datum),
         Datum= format_ISO8601(Datum, precision = "ym"),
         Canton2 = Canton,
         Canton2 = recode(Canton2, 
                          "Zürich" = "ZH",
                          "Bern" = "BE",
                          "Luzern" = "LU",
                          "Uri" = "UR",
                          "Schwyz" = "SZ",
                          "Obwalden" ="OW",
                          "Nidwalden" ="NW",
                          "Glarus" = "GL",
                          "Zug" = "ZG",
                          "Fribourg" = "FR",
                          "Solothurn" = "SO",
                          "Baselstadt" = "BS",
                          "Baselland" = "BL",
                          "Schaffhausen" = "SH",
                          "Appenzell AR" = "AR",
                          "Appenzell IR" = "AI",
                          "St. Gallen" = "SG",
                          "Graubünden" = "GR",
                          "Aargau" = "AG",
                          "Thurgau" = "TG",
                          "Tessin" = "TI",
                          "Vaud" = "VD",
                          "Valais" = "VS",
                          "Neuchâtel" = "NE",
                          "Genève" = "GE")) %>%
  select(-Canton) %>%
  rbind(hospital_week_month) %>%
  mutate( datum = ymd(paste0(Datum, "-", 01)),
    Year = year(datum)) %>%
  left_join(population_canton)


save(hospital_monthly ,file="data/hospital_monthly.RData")
write.xlsx(hospital_monthly,file="data/hospital_monthly.xlsx",rowNames=FALSE, overwrite = TRUE)