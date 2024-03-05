function_maps_quartal <- function (Quartal, Start_year, End_year, plot_legend=F) {
load("data/cases_canton_monthly.RData")


data_cases <- cases_canton_monthly %>%
  mutate(Canton = recode(Canton,
                         "Baselland" = "Basel-Landschaft",
                         "Appenzell AR" = "Appenzell Ausserrhoden",
                         "Appenzell IR" = "Appenzell Innerrhoden",
                         "Baselstadt" = "Basel-Stadt",
                         "Tessin" = "Ticino")) %>%
  filter(!Canton =="Total") %>%
  rename(NAME=Canton) %>%
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
                           "12"="4")) %>%
  group_by(Year, Quarter, NAME) %>%
  mutate(Quarter_cases = sum(cases)) %>%
  ungroup() %>%
  mutate( Quarter_inc = Quarter_cases/population*100000) %>%
  distinct(Year, Quarter, NAME,.keep_all = TRUE) %>%
  mutate(Year_quarter = paste0(Year,"-",Quarter),
         Year_month = paste0(Year,"-",Month)) %>%
  filter(Year >=Start_year & Year <= End_year) 
         
jenk_break = getJenksBreaks(data_cases$Quarter_inc, k = 6)
  
  data_cases <- data_cases %>%
    mutate(jenk_quar = cut(Quarter_inc, breaks=jenk_break, include.lowest=TRUE)) %>%
    filter(Year_quarter==Quartal) 

canton_geo <- read_sf("data_raw/Map_Canton/Map_Canton_1918.shp") %>%
  # filter(!is.na(EINWOHNERZ)) %>%
  # filter(!(NAME=="Bern" & HERKUNFT_J==2015)) 
  distinct(geometry, .keep_all = TRUE)


canton_geo_w <- canton_geo  %>%
  left_join(data_cases) 

if (plot_legend==T){
  plot_inc_quartal <- tm_shape(canton_geo_w )  +
    tm_fill("jenk_quar",
            title = "Incidence",
            # palette = "YlOrBr",
            palette = "Reds",
            legend.is.portrait = FALSE,
            # style = "quantile",
            style = "pretty") +
    # legend.hist = TRUE,
    # style = "jenks")+
    tm_borders(alpha = 1, lwd=0.8,col="grey20") +
    # tm_facets(by ="Year_quarter",ncol=5) +
    tm_layout(
      # main.title = "Hotspots",
      main.title = Quartal,
      main.title.position = "left",
      legend.only= T,
      legend.text.size = 2,
      legend.title.size=2,
      legend.width = 4,
      legend.height = 2,
      legend.position = c(0.1,0.2))
  
}

else{
plot_inc_quartal <- tm_shape(canton_geo_w )  +
  tm_fill("jenk_quar",
          legend.show = FALSE,
          # title = "Quartal",
          # palette = "YlOrBr",
          palette = "Reds",
          # legend.is.portrait = FALSE,
          # style = "quantile",
          style = "pretty") +
          # legend.hist = TRUE,
          # style = "jenks")+
  tm_borders(alpha = 1, lwd=0.8,col="grey20") +
  # tm_facets(by ="Year_quarter",ncol=5) +
  tm_layout(
    # main.title = "Hotspots",
    main.title = Quartal,
    main.title.position = "left",
    title.size = 2,
    legend.text.size = 4,
    # legend.width = 5,
    # legend.height = 8,
    legend.position = c(0.1,0.2))
}



return(plot_inc_quartal)

}


plot37 <-  tmap_grob(function_maps_quartal("1923-1", Start_year=1914, End_year=1923))
plot38 <-  tmap_grob(function_maps_quartal("1923-2", Start_year=1914, End_year=1923))
plot39 <-  tmap_grob(function_maps_quartal("1923-3", Start_year=1914, End_year=1923))
plot40 <-  tmap_grob(function_maps_quartal("1923-4", Start_year=1914, End_year=1923))

plot33 <-  tmap_grob(function_maps_quartal("1914-1", Start_year=1914, End_year=1923))
plot34 <-  tmap_grob(function_maps_quartal("1914-2", Start_year=1914, End_year=1923))
plot35 <-  tmap_grob(function_maps_quartal("1914-3", Start_year=1914, End_year=1923))
plot36 <-  tmap_grob(function_maps_quartal("1914-4", Start_year=1914, End_year=1923))

plot1 <-  tmap_grob(function_maps_quartal("1915-1", Start_year=1914, End_year=1923))
plot2 <-  tmap_grob(function_maps_quartal("1915-2", Start_year=1914, End_year=1923))
plot3 <-  tmap_grob(function_maps_quartal("1915-3", Start_year=1914, End_year=1923))
plot4 <-  tmap_grob(function_maps_quartal("1915-4", Start_year=1914, End_year=1923))

plot5 <-  tmap_grob(function_maps_quartal("1916-1", Start_year=1914, End_year=1923))
plot6 <-  tmap_grob(function_maps_quartal("1916-2", Start_year=1914, End_year=1923))
plot7 <-  tmap_grob(function_maps_quartal("1916-3", Start_year=1914, End_year=1923))
plot8 <-  tmap_grob(function_maps_quartal("1916-4", Start_year=1914, End_year=1923))

plot9 <-  tmap_grob(function_maps_quartal("1917-1", Start_year=1914, End_year=1923))
plot10 <-  tmap_grob(function_maps_quartal("1917-2", Start_year=1914, End_year=1923))
plot11 <-  tmap_grob(function_maps_quartal("1917-3", Start_year=1914, End_year=1923))
plot12<-  tmap_grob(function_maps_quartal("1917-4", Start_year=1914, End_year=1923))

plot13 <-  tmap_grob(function_maps_quartal("1918-1", Start_year=1914, End_year=1923))
plot14 <-  tmap_grob(function_maps_quartal("1918-2", Start_year=1914, End_year=1923))
plot15 <-  tmap_grob(function_maps_quartal("1918-3", Start_year=1914, End_year=1923))
plot16 <-  tmap_grob(function_maps_quartal("1918-4", Start_year=1914, End_year=1923))

plot17 <-  tmap_grob(function_maps_quartal("1919-1", Start_year=1914, End_year=1923))
plot18 <-  tmap_grob(function_maps_quartal("1919-2", Start_year=1914, End_year=1923))
plot19 <-  tmap_grob(function_maps_quartal("1919-3", Start_year=1914, End_year=1923))
plot20 <-  tmap_grob(function_maps_quartal("1919-4", Start_year=1914, End_year=1923))

plot21 <-  tmap_grob(function_maps_quartal("1920-1", Start_year=1914, End_year=1923))
plot22 <-  tmap_grob(function_maps_quartal("1920-2", Start_year=1914, End_year=1923))
plot23 <-  tmap_grob(function_maps_quartal("1920-3", Start_year=1914, End_year=1923))
plot24 <-  tmap_grob(function_maps_quartal("1920-4", Start_year=1914, End_year=1923))

plot25 <-  tmap_grob(function_maps_quartal("1921-1", Start_year=1914, End_year=1923))
plot26 <-  tmap_grob(function_maps_quartal("1921-2", Start_year=1914, End_year=1923))
plot27 <-  tmap_grob(function_maps_quartal("1921-3", Start_year=1914, End_year=1923))
plot28 <-  tmap_grob(function_maps_quartal("1921-4", Start_year=1914, End_year=1923))


plot29 <-  tmap_grob(function_maps_quartal("1922-1", Start_year=1914, End_year=1923))
plot30 <-  tmap_grob(function_maps_quartal("1922-2", Start_year=1914, End_year=1923))
plot31 <-  tmap_grob(function_maps_quartal("1922-3", Start_year=1914, End_year=1923))
plot32 <-  tmap_grob(function_maps_quartal("1922-4", Start_year=1914, End_year=1923))

plot_legend <-  tmap_grob(function_maps_quartal("1922-4", Start_year=1914, End_year=1923, plot_legend = T))


plot_maps_1914_1923 <- plot_grid(
  plot33,  plot34, plot35, plot36,
  plot1, plot2, plot3, plot4,
  plot5, plot6, plot7, plot8,
  plot9, plot10,plot11, plot12,
  plot13,  plot14, plot15, plot16,
  plot17,  plot18, plot19, plot20,
  plot21,  plot22, plot23, plot24,
  plot25,  plot26, plot27, plot28,
  plot29,  plot30, plot31, plot32,
  plot37,  plot38, plot39, plot40,
  plot_legend,
  ncol=4)



cowplot::save_plot("output/Figure_Maps_1914_1923.pdf", plot_maps_1914_1923 ,base_height=40,base_width=20)



plot1 <-  tmap_grob(function_maps_quartal("1927-1",Start_year=1927, End_year=1936))
plot2 <-  tmap_grob(function_maps_quartal("1927-2",Start_year=1927, End_year=1936))
plot3 <-  tmap_grob(function_maps_quartal("1927-3",Start_year=1927, End_year=1936))
plot4 <-  tmap_grob(function_maps_quartal("1927-4",Start_year=1927, End_year=1936))

plot5 <-  tmap_grob(function_maps_quartal("1928-1",Start_year=1927, End_year=1936))
plot6 <-  tmap_grob(function_maps_quartal("1928-2",Start_year=1927, End_year=1936))
plot7 <-  tmap_grob(function_maps_quartal("1928-3",Start_year=1927, End_year=1936))
plot8 <-  tmap_grob(function_maps_quartal("1928-4",Start_year=1927, End_year=1936))

plot9 <-  tmap_grob(function_maps_quartal("1929-1",Start_year=1927, End_year=1936))
plot10 <-  tmap_grob(function_maps_quartal("1929-2",Start_year=1927, End_year=1936))
plot11 <-  tmap_grob(function_maps_quartal("1929-3",Start_year=1927, End_year=1936))
plot12<-  tmap_grob(function_maps_quartal("1929-4",Start_year=1927, End_year=1936))


plot13 <-  tmap_grob(function_maps_quartal("1930-1",Start_year=1927, End_year=1936))
plot14 <-  tmap_grob(function_maps_quartal("1930-2",Start_year=1927, End_year=1936))
plot15 <-  tmap_grob(function_maps_quartal("1930-3",Start_year=1927, End_year=1936))
plot16 <-  tmap_grob(function_maps_quartal("1930-4",Start_year=1927, End_year=1936))

plot17 <-  tmap_grob(function_maps_quartal("1931-1",Start_year=1927, End_year=1936))
plot18 <-  tmap_grob(function_maps_quartal("1931-2",Start_year=1927, End_year=1936))
plot19 <-  tmap_grob(function_maps_quartal("1931-3",Start_year=1927, End_year=1936))
plot20 <-  tmap_grob(function_maps_quartal("1931-4",Start_year=1927, End_year=1936))

plot21 <-  tmap_grob(function_maps_quartal("1932-1",Start_year=1927, End_year=1936))
plot22 <-  tmap_grob(function_maps_quartal("1932-2",Start_year=1927, End_year=1936))
plot23 <-  tmap_grob(function_maps_quartal("1932-3",Start_year=1927, End_year=1936))
plot24 <-  tmap_grob(function_maps_quartal("1932-4",Start_year=1927, End_year=1936))

plot25 <-  tmap_grob(function_maps_quartal("1933-1",Start_year=1927, End_year=1936))
plot26 <-  tmap_grob(function_maps_quartal("1933-2",Start_year=1927, End_year=1936))
plot27 <-  tmap_grob(function_maps_quartal("1933-3",Start_year=1927, End_year=1936))
plot28 <-  tmap_grob(function_maps_quartal("1933-4",Start_year=1927, End_year=1936))

plot29 <-  tmap_grob(function_maps_quartal("1934-1",Start_year=1927, End_year=1936))
plot30 <-  tmap_grob(function_maps_quartal("1934-2",Start_year=1927, End_year=1936))
plot31 <-  tmap_grob(function_maps_quartal("1934-3",Start_year=1927, End_year=1936))
plot32 <-  tmap_grob(function_maps_quartal("1934-4",Start_year=1927, End_year=1936))

plot33 <-  tmap_grob(function_maps_quartal("1935-1",Start_year=1927, End_year=1936))
plot34 <-  tmap_grob(function_maps_quartal("1935-2",Start_year=1927, End_year=1936))
plot35 <-  tmap_grob(function_maps_quartal("1935-3",Start_year=1927, End_year=1936))
plot36 <-  tmap_grob(function_maps_quartal("1935-4",Start_year=1927, End_year=1936))

plot37 <-  tmap_grob(function_maps_quartal("1936-1",Start_year=1927, End_year=1936))
plot38 <-  tmap_grob(function_maps_quartal("1936-2",Start_year=1927, End_year=1936))
plot39 <-  tmap_grob(function_maps_quartal("1936-3",Start_year=1927, End_year=1936))
plot40 <-  tmap_grob(function_maps_quartal("1936-4",Start_year=1927, End_year=1936))

plot_legend <-  tmap_grob(function_maps_quartal("1936-4", Start_year=1927, End_year=1936, plot_legend = T))


plot_maps_1927_1936 <- plot_grid(
  plot1, plot2, plot3, plot4,
  plot5, plot6, plot7, plot8,
  plot9, plot10,plot11, plot12,
  plot13,  plot14, plot15, plot16,
  plot17,  plot18, plot19, plot20,
  plot21,  plot22, plot23, plot24,
  plot25,  plot26, plot27, plot28,
  plot29,  plot30, plot31, plot32,
  plot33,  plot34, plot35, plot36,
  plot37,  plot38, plot39, plot40,
  plot_legend,
  ncol=4)



cowplot::save_plot("output/Figure_Maps_1927_1936.pdf", plot_maps_1927_1936 ,base_height=40,base_width=20)



plot37 <-  tmap_grob(function_maps_quartal("1941-1",Start_year=1941, End_year=1950))
plot38 <-  tmap_grob(function_maps_quartal("1941-2",Start_year=1941, End_year=1950))
plot39 <-  tmap_grob(function_maps_quartal("1941-3",Start_year=1941, End_year=1950))
plot40 <-  tmap_grob(function_maps_quartal("1941-4",Start_year=1941, End_year=1950))

plot1 <-  tmap_grob(function_maps_quartal("1942-1",Start_year=1941, End_year=1950))
plot2 <-  tmap_grob(function_maps_quartal("1942-2",Start_year=1941, End_year=1950))
plot3 <-  tmap_grob(function_maps_quartal("1942-3",Start_year=1941, End_year=1950))
plot4 <-  tmap_grob(function_maps_quartal("1942-4",Start_year=1941, End_year=1950))

plot5 <-  tmap_grob(function_maps_quartal("1943-1",Start_year=1941, End_year=1950))
plot6 <-  tmap_grob(function_maps_quartal("1943-2",Start_year=1941, End_year=1950))
plot7 <-  tmap_grob(function_maps_quartal("1943-3",Start_year=1941, End_year=1950))
plot8 <-  tmap_grob(function_maps_quartal("1943-4",Start_year=1941, End_year=1950))

plot9 <-  tmap_grob(function_maps_quartal("1944-1",Start_year=1941, End_year=1950))
plot10 <-  tmap_grob(function_maps_quartal("1944-2",Start_year=1941, End_year=1950))
plot11 <-  tmap_grob(function_maps_quartal("1944-3",Start_year=1941, End_year=1950))
plot12<-  tmap_grob(function_maps_quartal("1944-4",Start_year=1941, End_year=1950))

plot13 <-  tmap_grob(function_maps_quartal("1945-1",Start_year=1941, End_year=1950))
plot14 <-  tmap_grob(function_maps_quartal("1945-2",Start_year=1941, End_year=1950))
plot15 <-  tmap_grob(function_maps_quartal("1945-3",Start_year=1941, End_year=1950))
plot16 <-  tmap_grob(function_maps_quartal("1945-4",Start_year=1941, End_year=1950))

plot17 <-  tmap_grob(function_maps_quartal("1945-1",Start_year=1941, End_year=1950))
plot18 <-  tmap_grob(function_maps_quartal("1945-2",Start_year=1941, End_year=1950))
plot19 <-  tmap_grob(function_maps_quartal("1945-3",Start_year=1941, End_year=1950))
plot20 <-  tmap_grob(function_maps_quartal("1945-4",Start_year=1941, End_year=1950))

plot21 <-  tmap_grob(function_maps_quartal("1947-1",Start_year=1941, End_year=1950))
plot22 <-  tmap_grob(function_maps_quartal("1947-2",Start_year=1941, End_year=1950))
plot23 <-  tmap_grob(function_maps_quartal("1947-3",Start_year=1941, End_year=1950))
plot24 <-  tmap_grob(function_maps_quartal("1947-4",Start_year=1941, End_year=1950))

plot25 <-  tmap_grob(function_maps_quartal("1948-1",Start_year=1941, End_year=1950))
plot26 <-  tmap_grob(function_maps_quartal("1948-2",Start_year=1941, End_year=1950))
plot27 <-  tmap_grob(function_maps_quartal("1948-3",Start_year=1941, End_year=1950))
plot28 <-  tmap_grob(function_maps_quartal("1948-4",Start_year=1941, End_year=1950))

plot29 <-  tmap_grob(function_maps_quartal("1949-1",Start_year=1941, End_year=1950))
plot30 <-  tmap_grob(function_maps_quartal("1949-2",Start_year=1941, End_year=1950))
plot31 <-  tmap_grob(function_maps_quartal("1949-3",Start_year=1941, End_year=1950))
plot32 <-  tmap_grob(function_maps_quartal("1949-4",Start_year=1941, End_year=1950))

plot33 <-  tmap_grob(function_maps_quartal("1950-1",Start_year=1941, End_year=1950))
plot34 <-  tmap_grob(function_maps_quartal("1950-2",Start_year=1941, End_year=1950))
plot35 <-  tmap_grob(function_maps_quartal("1950-3",Start_year=1941, End_year=1950))
plot36 <-  tmap_grob(function_maps_quartal("1950-4",Start_year=1941, End_year=1950))

plot_legend <-  tmap_grob(function_maps_quartal("1950-4", Start_year=1941, End_year=1950, plot_legend = T))

plot_maps_1941_1950 <- plot_grid(
  plot37,  plot38, plot39, plot40,
  plot1, plot2, plot3, plot4,
  plot5, plot6, plot7, plot8,
  plot9, plot10,plot11, plot12,
  plot13,  plot14, plot15, plot16,
  plot17,  plot18, plot19, plot20,
  plot21,  plot22, plot23, plot24,
  plot25,  plot26, plot27, plot28,
  plot29,  plot30, plot31, plot32,
  plot33,  plot34, plot35, plot36,
  plot_legend,
  ncol=4)



cowplot::save_plot("output/Figure_Maps_1941_1950.pdf", plot_maps_1941_1950 ,base_height=40,base_width=20)


