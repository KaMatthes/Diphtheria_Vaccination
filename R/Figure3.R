load("data/cases_canton_monthly.RData")

data_cases <- cases_canton_monthly %>%
  filter(Canton2=="Total") %>%
  ungroup() %>%
  mutate(Year = as.character(Year),
         Month = as.character(Month),
         datum = ymd(paste0(Year, "-",Month, "-", 01)),
         inc_cases = cases/population*100000)

plot_incidence <- ggplot()+
  geom_line(data=data_cases, aes(x=as.POSIXct(datum), y=inc_cases, col="notified cases"),lwd= lwd_line)+
  # geom_vline(xintercept=as.POSIXct("1943-02-01"), lwd=1, col="black", alpha=0.3) +
  xlab("Year") +
  ylab("Incidence per 100'000 inhabitants")+
  scale_x_datetime( breaks = date_breaks("60 month"),
                    labels = label_date_short(),
                    limits =c(as.POSIXct("1910-01-01"), max(as.POSIXct("1969-01-01"))),
                    expand = c(0,0)) +
  scale_color_manual("",
                     values=c( "black"))+
  ggtitle("A) Monthly incidence") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    plot.title = element_text(size = size_text),
    axis.text.y = element_text(size=size_text),
    legend.position = "none",
    legend.text=element_text(size=15),
    axis.text.x = element_text(size=size_text),
    axis.title.x  = element_text(size=size_text),
    axis.title.y  = element_text(size=size_text)) 

  
data.cases.gr <- cases_canton_monthly %>%
  filter(Canton2 == "Total") %>%
  mutate(n_year = 1,
         Year_gr =  Year,
         Year_gr = ifelse(Year_gr>=1965 & Year_gr < 1969, "1965-1968", Year_gr),
         Year_gr = ifelse(Year_gr>=1960 & Year_gr < 1965, "1960-1964", Year_gr),
         Year_gr = ifelse(Year_gr>=1955 & Year_gr < 1960, "1955-1959", Year_gr),
         Year_gr = ifelse(Year_gr>=1950 & Year_gr < 1955, "1950-1954", Year_gr),
         Year_gr = ifelse(Year_gr>=1945 & Year_gr < 1950, "1945-1949", Year_gr),
         Year_gr = ifelse(Year_gr>=1940 & Year_gr < 1945, "1940-1944", Year_gr),
         Year_gr = ifelse(Year_gr>=1935 & Year_gr < 1940, "1935-1939", Year_gr),
         Year_gr = ifelse(Year_gr>=1930 & Year_gr < 1935, "1930-1934", Year_gr),
         Year_gr = ifelse(Year_gr>=1925 & Year_gr < 1930, "1925-1929", Year_gr),
         Year_gr = ifelse(Year_gr>=1920 & Year_gr < 1925, "1920-1924", Year_gr),
         Year_gr = ifelse(Year_gr>=1915 & Year_gr < 1920, "1915-1919", Year_gr),
         Year_gr = ifelse(Year_gr>=1910 & Year_gr < 1915, "1910-1914", Year_gr)) %>%
  group_by(Year_gr, Month) %>%
  summarise(Inc_sum = sum(cases, na.rm =TRUE),
            Pop_gr_sum = sum(population,na.rm =TRUE),
            Inc_cases_sum = Inc_sum/Pop_gr_sum*100000) %>%
  ungroup() 


plot_seasonal <- ggplot() +
  geom_line(data=data.cases.gr, aes(x=Month,y=Inc_cases_sum, col=Year_gr), lwd=lwd_line) +
  scale_color_manual("",values= mypalette)+
  scale_x_continuous(breaks=c(1:12),
                     labels=season.labs)+
  xlab("Month") +
  ylab("Incidence per 100'000 inhabitants")+
  ggtitle("B) Monthly incidence trend by year group") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    plot.title = element_text(size = size_text),
    axis.text.y = element_text(size=size_text),
    legend.position = "bottom",
    legend.text=element_text(size=15),
    axis.text.x = element_text(size=size_text),
    axis.title.x  = element_text(size=size_text),
    axis.title.y  = element_text(size=size_text)) 


data.time <- cases_canton_monthly %>%
  filter(Canton2 == "Total") %>%
  mutate(cases_inc = cases/population*100000) %>%
  filter(!Year > 1949)

cases_timeseries <- ts(data.time$cases_inc, frequency=12, start=c(1910,1))

plot_monthly_trend <- ggsubseriesplot(cases_timeseries, lwd=2)+
  geom_line(aes(y = .data[["avg"]]), col = "red", lwd=2) +
  
  xlab("Month") +
  ylab("Incidence per 100'000 inhabitants")+
  ggtitle("C) Timeseries of monthly incidence") +
  scale_color_manual("",
                     values=c( "red","black"))+
  theme_bw() +
  theme(
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=size_text),
    plot.title = element_text(size = size_text),
    legend.position = "none",
    legend.text=element_text(size=15),
    axis.text.x = element_text(size=size_text),
    axis.title.x  = element_text(size=size_text),
    axis.title.y  = element_text(size=size_text)) 


Figure3 <- cowplot::plot_grid(plot_incidence,NULL,plot_seasonal,NULL,plot_monthly_trend,
                                    ncol=1, nrow=5, align="hv",
                                    rel_heights = c(1,0,1,0,1))


cowplot::save_plot(paste0("output/Figure3.pdf"), Figure3,base_height=20,base_width=15)
