load("data/cases_canton_yearly.RData")


data_cases <- cases_canton_yearly %>%
  ungroup() %>%
  mutate(Language= Canton,
         Language = recode(Language, 
                           "Zürich" = "German",
                           "Bern" = "German",
                           "Luzern" = "German",
                           "Uri" = "German",
                           "Schwyz" ="German",
                           "Obwalden" ="German",
                           "Nidwalden" ="German",
                           "Glarus" = "German",
                           "Zug" = "German",
                           "Aargau" ="German",
                           "Appenzell AR" ="German",
                           "Appenzell IR" ="German",
                           "Baselland" = "German",
                           "Baselstadt" ="German",
                           "Graubünden" = "German",
                           "Schaffhausen" = "German",
                           "Solothurn" = "German",
                           "St. Gallen" = "German",
                           "Thurgau" = "German",
                           "Tessin" = "Italian",
                           "Valais" = "French",
                           "Vaud" = "French",
                           "Fribourg" ="French",
                           "Genève" = "French",
                           "Neuchâtel" = "French"),
         Language = factor( Language, levels = c("Total","German", "French", "Italian"))) %>%
  distinct(Year, Canton, .keep_all = TRUE) %>%
  group_by(Year, Language) %>%
  mutate(cases_y = sum(cases_year)) %>%
  ungroup()

pop_l <- data_cases %>%
  distinct(Year, Canton, .keep_all = TRUE) %>%
  select(Year, Language, population) %>%
  group_by(Year, Language) %>%
  mutate(Pop_lang = sum(population)) %>%
  ungroup() %>%
  distinct(Year, Language, .keep_all = TRUE)


data_cases <- data_cases %>%
  left_join(pop_l) %>%
  mutate(inc_cases = cases_y/Pop_lang *100000,
         datum = ymd(paste0(Year, "-01-01"))) %>%
  distinct(Year, Language, .keep_all = TRUE)%>%
  filter(!is.na(Language))


plot_inc <- ggplot()+
  # geom_line(data=data_cases, aes(x=as.POSIXct(datum), y=inc_fit, col="fitting values"), lwd=3) +
  geom_line(data=data_cases, aes(x=as.POSIXct(datum), y= inc_cases, col=Language, lty=Language), lwd=lwd_line) +
  geom_vline(xintercept=as.POSIXct(ymd("1944-01-01")), lwd=1.2, col="black", alpha=0.3) +
  xlab("Year") +
  ylab("Incidence per 100'000 inhabitants") +
  scale_x_datetime( breaks = date_breaks("4 year"),
                    labels = label_date_short(),
                    limits =c(as.POSIXct("1910-01-01"), max(as.POSIXct("1968-01-01"))))+
  scale_color_manual("",
                     values=c(mypalette_c[2],mypalette_c[1],mypalette_c[3],mypalette_c[4]))+
  scale_linetype_manual("",
                        values=c("longdash","solid","solid","solid"))+
  ggtitle("Incidence")+
  theme_bw() +
  theme(
    axis.text.y = element_text(size=30),
    legend.position = c(0.9,0.7),
    legend.text=element_text(size=30),
    legend.key.size = unit(3.5, 'cm'),
    legend.spacing.x = unit(2.5, 'cm'),
    axis.text.x = element_text(size=30),
    axis.title.x  = element_text(size=30),
    axis.title.y  = element_text(size=30),
    plot.title = element_text(size=30))


load("data/death_canton_year.RData")

data_death <- death_canton_year %>%
  ungroup() %>%
  mutate(Language= Canton,
         Language = recode(Language, 
                           "Zürich" = "German",
                           "Bern" = "German",
                           "Luzern" = "German",
                           "Uri" = "German",
                           "Schwyz" ="German",
                           "Obwalden" ="German",
                           "Nidwalden" ="German",
                           "Glarus" = "German",
                           "Zug" = "German",
                           "Aargau" ="German",
                           "Appenzell AR" ="German",
                           "Appenzell IR" ="German",
                           "Baselland" = "German",
                           "Baselstadt" ="German",
                           "Graubünden" = "German",
                           "Schaffhausen" = "German",
                           "Solothurn" = "German",
                           "St. Gallen" = "German",
                           "Thurgau" = "German",
                           "Tessin" = "Italian",
                           "Valais" = "French",
                           "Vaud" = "French",
                           "Fribourg" ="French",
                           "Genève" = "French",
                           "Neuchâtel" = "French"),
         Language = factor( Language, levels = c("Total","German", "French", "Italian"))) %>%
  distinct(Year, Canton, .keep_all = TRUE) %>%
  group_by(Year, Language) %>%
  mutate(death_y = sum(death)) %>%
  ungroup()


data_death <- data_death %>%
  left_join(pop_l) %>%
  mutate(inc_death = death_y/Pop_lang *100000,
         datum = ymd(paste0(Year, "-01-01"))) %>%
  distinct(Year, Language, .keep_all = TRUE) %>%
  filter(!is.na(Language))


plot_death <- ggplot()+
  # geom_line(data=data_cases, aes(x=as.POSIXct(datum), y=inc_fit, col="fitting values"), lwd=3) +
  geom_line(data=data_death, aes(x=as.POSIXct(datum), y= inc_death, col=Language, lty=Language), lwd=lwd_line) +
  geom_vline(xintercept=as.POSIXct(ymd("1944-01-01")), lwd=1.2, col="black", alpha=0.3) +
  xlab("Year") +
  ylab("Mortality per 100'000 inhabitants") +
  scale_x_datetime( breaks = date_breaks("4 year"),
                    labels = label_date_short(),
                    limits =c(as.POSIXct("1910-01-01"), max(as.POSIXct("1968-01-01"))))+
  scale_color_manual("",
                     # values= c(mypalette[1],mypalette[6],mypalette[10],mypalette[12]),
                     values=c(mypalette_c[2],mypalette_c[1],mypalette_c[3],mypalette_c[4]))+
  scale_linetype_manual("",
                     values=c("longdash","solid","solid","solid"))+
  scale_size_manual("",
                        values=c(4,1.5,1.5,1.5))+
  ggtitle("Mortality")+
  theme_bw() +
  theme(
    axis.text.y = element_text(size=30),
    legend.position = "none",
    legend.text=element_text(size=30),
    legend.key.size = unit(2.5, 'cm'),
    legend.spacing.x = unit(2.5, 'cm'),
    axis.text.x = element_text(size=30),
    axis.title.x  = element_text(size=30),
    axis.title.y  = element_text(size=30),
    plot.title = element_text(size=30))

data_death <- data_death %>%
  select(Year, Language, death_y)

data_cases <- data_cases %>%
  select(Year, Language, cases_y, datum) %>%
  left_join(data_death) %>%
  mutate(case_fatality = death_y/cases_y *100) %>%
  filter(!Year > 1952)


plot_case_fatality <- ggplot()+
  # geom_line(data=data_cases, aes(x=as.POSIXct(datum), y=inc_fit, col="fitting values"), lwd=3) +
  geom_line(data=data_cases, aes(x=as.POSIXct(datum), y= case_fatality, col=Language,lty=Language), lwd=lwd_line) +
  geom_vline(xintercept=as.POSIXct(ymd("1944-01-01")), lwd=1.2, col="black", alpha=0.3) +
  xlab("Year") +
  ylab("Incidence per 100'000 inhabitants") +
  scale_x_datetime( breaks = date_breaks("4 year"),
                    labels = label_date_short())+
  # limits =c(as.POSIXct("1910-01-01"), max(as.POSIXct("1969-01-01"))),
  # expand = c(0,0)) +
  scale_color_manual("",
                     values=mypalette_c)+
  scale_linetype_manual("",
                        values=c("longdash","solid","solid","solid"))+
  ggtitle("Case-Fatality-Rate")+
  theme_bw() +
  theme(
    axis.text.y = element_text(size=30),
    legend.position = c(0.9,0.9),
    legend.text=element_text(size=30),
    legend.key.size = unit(2.5, 'cm'),
    legend.spacing.x = unit(2.5, 'cm'),
    axis.text.x = element_text(size=30),
    axis.title.x  = element_text(size=30),
    axis.title.y  = element_text(size=30),
    plot.title = element_text(size=30))




load("data/hospital_monthly.RData")


data_hosp <- hospital_monthly %>%
  ungroup() %>% 
  arrange(Datum) %>%
  mutate(cases_month =ifelse(is.na(cases_month),0, cases_month)) %>%
  group_by(Year, Canton2) %>%
  mutate(hosp_y_c = sum(cases_month)) %>%
  ungroup() %>%
  mutate(Language= Canton2,
         Language = recode(Language, 
                           "ZH" = "German",
                           "BE" = "German",
                           "LU" = "German",
                           "UR" = "German",
                           "SZ" ="German",
                           "OW" ="German",
                           "NW" ="German",
                           "GL" = "German",
                           "ZG" = "German",
                           "AG" ="German",
                           "AR" ="German",
                           "AI" ="German",
                           "BL" = "German",
                           "BS" ="German",
                           "GR" = "German",
                           "SH" = "German",
                           "SO" = "German",
                           "SG" = "German",
                           "TG" = "German",
                           "TI" = "Italian",
                           "VS" = "French",
                           "VD" = "French",
                           "FR" ="French",
                           "GE" = "French",
                           "NE" = "French"),
         Language = factor( Language, levels = c("Total","German", "French", "Italian"))) %>%
  distinct(Year, Canton2, .keep_all = TRUE) %>%
  group_by(Year, Language) %>%
  mutate(hosp_y = round(sum(hosp_y_c),0)) %>%
  ungroup() %>%
  distinct(Year, Language, .keep_all = TRUE) 


data_hosp <- data_hosp %>%
  left_join(pop_l, by=c("Year", "Language")) %>%
  mutate(inc_hosp = hosp_y/Pop_lang *100000) %>%
  distinct(Year, Language, .keep_all = TRUE)%>%
  filter(!is.na(Language))


  

plot_hosp <- ggplot()+
  # geom_line(data=data_cases, aes(x=as.POSIXct(datum), y=inc_fit, col="fitting values"), lwd=3) +
  geom_line(data=data_hosp, aes(x=as.POSIXct(datum), y= inc_hosp, col=Language,lty=Language), lwd=lwd_line) +
  geom_vline(xintercept=as.POSIXct(ymd("1944-01-01")), lwd=1.2, col="black", alpha=0.3) +
  xlab("Year") +
  ylab("Hospitalization per 100'000 inhabitants") +
  scale_x_datetime( breaks = date_breaks("4 year"),
                    labels = label_date_short(),
                    limits =c(as.POSIXct("1910-01-01"), max(as.POSIXct("1968-01-01"))))+
  scale_color_manual("",
                     # values=c( "grey30",cbp1[1],cbp1[2],cbp1[3]),
                     values=c(mypalette_c[2],mypalette_c[1],mypalette_c[3],mypalette_c[4]))+
  scale_linetype_manual("",
                        values=c("longdash","solid","solid","solid"))+
  ggtitle("Hospitalization")+
  theme_bw() +
  theme(
    axis.text.y = element_text(size=30),
    legend.position = "none",
    legend.text=element_text(size=30),
    legend.key.size = unit(2.5, 'cm'),
    legend.spacing.x = unit(2.5, 'cm'),
    axis.text.x = element_text(size=30),
    axis.title.x  = element_text(size=30),
    axis.title.y  = element_text(size=30),
    plot.title = element_text(size=30))


Figure4 <- cowplot::plot_grid(plot_inc, plot_hosp, plot_death,ncol=1)

cowplot::save_plot("output/Figure4.pdf", Figure4 ,base_height=35,base_width=30)
