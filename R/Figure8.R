#plot parameter

size_plot <- 30
size_plot_axis <- 30
lwd_line <- 1.5

#data

load("data/cases_canton_monthly.RData")

data_cases <- cases_canton_monthly %>%
  filter(Year >=1940 & Year < 1970) %>%
  filter(!Type_vaccination == "Total") %>%
  distinct(Datum, Type_vaccination, .keep_all = TRUE) %>%
    mutate(Year = as.character(Year),
           Month = as.character(Month),
           datum = ymd(paste0(Year, "-",Month, "-", 01)),
           inc_cases =cases_l/pop_l*100000) %>%
  select( Type_vaccination, Year, datum, inc_cases)


Figure8 <- ggplot()+
  geom_line(data=data_cases, aes(x=as.POSIXct(datum), y=inc_cases, col= Type_vaccination), lwd=lwd_line) +
  geom_vline(xintercept=as.POSIXct(ymd("1944-01-01")), lwd=1, col="black", alpha=0.3) +
  xlab("Year") +
  ylab("Incidence per 100'000 inhabitants")+
  scale_x_datetime( breaks = date_breaks("24 month"),
                    labels = label_date_short(),
                    expand = c(0,0)) +
  scale_color_manual("",
                     labels=c("Compulsory", "Recommended"),
                     values=c("grey70","grey20"))+

  theme_bw() +
  theme(
    axis.line = element_line(),
    axis.text.y = element_text(size=size_plot),
    plot.title = element_text(size = size_plot),
    legend.position = c(0.7,0.7),
    legend.key.width= unit(5, "line"),
    legend.text=element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot),
    axis.title.y  = element_text(size=size_plot)) 


cowplot::save_plot(paste0("output/Figure8.pdf"), Figure8,base_height=10,base_width=20)
