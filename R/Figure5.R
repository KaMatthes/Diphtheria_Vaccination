function_plot_quartal <- function (Start_year, End_year, Title, legend_no) {

  load("data/cases_canton_monthly.RData")

  
  
data_cases <- cases_canton_monthly %>%
  mutate(Canton = recode(Canton,
                         "Baselland" = "Basel-Landschaft",
                         "Appenzell AR" = "Appenzell Ausserrhoden",
                         "Appenzell IR" = "Appenzell Innerrhoden",
                         "Baselstadt" = "Basel-Stadt",
                         "Tessin" = "Ticino")) %>%
  filter(!Canton =="Total") %>%
  mutate(inc_cases = cases/population*100000,
         year_month = ymd(paste0(Year,"-",Month, "-01")),
         Quarter = as.factor(Month),
         Quarter  = recode(Quarter, 
                           "1"="1",
                           "2"="1",
                           "3"="3",
                           "4"="2",
                           "5"="2",
                           "6"="2",
                           "7"="3",
                           "8"="3",
                           "9"="3",
                           "10"="4",
                           "11"="4",
                           "12"="4"),
         Language= Canton,
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
                           "Appenzell Ausserrhoden" ="German",
                           "Appenzell Innerrhoden" ="German",
                           "Basel-Landschaft" = "German",
                           "Basel-Stadt" ="German",
                           "Graubünden" = "German",
                           "Schaffhausen" = "German",
                           "Solothurn" = "German",
                           "St. Gallen" = "German",
                           "Thurgau" = "German",
                           "Ticino" = "Italian",
                           "Valais" = "French",
                           "Vaud" = "French",
                           "Fribourg" ="French",
                           "Genève" = "French",
                           "Neuchâtel" = "French"),
         Language = factor( Language, levels = c("German", "French", "Italian"))) %>%
  group_by(Year, Month, Language) %>%
  mutate(Quarter_cases = sum(cases)) %>%
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
  distinct(Year, Month, Language,.keep_all = TRUE) %>%
  mutate( Quarter_inc = Quarter_cases/Pop_lang*100000) %>%
  mutate(Year_quarter = paste0(Year,"-",Quarter),
         Year_month = ymd(paste0(Year,"-",Month, "-01")),
         id=1:nrow(.),
         Canton=as.factor(Canton)) %>%
  filter(Year >=Start_year & Year < End_year) 
     



plot_Month <- ggplot()+
  # geom_line(data=data_cases, aes(x=as.POSIXct(datum), y=inc_fit, col="fitting values"), lwd=3) +
  geom_line(data=data_cases, aes(x=as.POSIXct(year_month), y= Quarter_inc, col=Language),lwd= lwd_line) +
 
  xlab("Year") +
  ylab("Incidence per 100'000 inhabitants") +
  scale_x_datetime( breaks = date_breaks("6 month"),
                    labels = label_date_short())+
                    # limits =c(as.POSIXct("1910-01-01"), max(as.POSIXct("1969-01-01"))),
  #                   # expand = c(0,0)) +
  # ylim(c(0,35))+
  scale_color_manual("",
                     values=c(mypalette_c[1],mypalette_c[3],mypalette_c[4]))+
  ggtitle(Title)+
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=30),
    plot.title = element_text(size = 30),
    legend.position = legend_no,
    legend.key.size = unit(3.5, 'cm'),
    legend.spacing.x = unit(2.5, 'cm'),
    legend.text=element_text(size=30),
    axis.text.x = element_text(size=30),
    axis.title.x  = element_text(size=30),
    axis.title.y  = element_text(size=30)) 

return(plot_Month)


}


plot1 <- function_plot_quartal(Start_year=1914, End_year=1924,Title ="1914-1923", legend_no=c(0.9,0.7))
plot2 <- function_plot_quartal(Start_year=1927, End_year=1937,Title ="1927-1936", legend_no="none")
plot3 <- function_plot_quartal(Start_year=1941, End_year=1951,Title ="1941-1950", legend_no="none")

Figure5 <- cowplot::plot_grid(plot1, plot2, plot3, ncol=1)




cowplot::save_plot("output/Figure5.pdf", Figure5 ,base_height=40,base_width=30)


