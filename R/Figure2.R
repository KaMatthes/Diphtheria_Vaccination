load("data/death_canton_year.RData")
data_death <- death_canton_year %>%
  filter(Canton2=="Total") %>%
  ungroup() %>%
  mutate(
    inc= death/population*100000,
    datum=ymd(paste0(Year, "01-01")),
    var= "death") %>%
  select(var, Year, datum, inc)

  
load("data/cases_canton_yearly.RData")
data_cases <-  cases_canton_yearly %>%
  filter(Canton2=="Total") %>%
  ungroup() %>%
  distinct(Year, .keep_all = TRUE) %>%
  mutate(inc = cases_year/population*100000,
         datum=ymd(paste0(Year, "01-01")),
         var= "cases") %>%
  select(var, Year, datum, inc)


data_death2 <- death_canton_year %>%
  filter(Canton2=="Total")

data_fat <- cases_canton_yearly %>%
  filter(Canton2=="Total") %>%
  ungroup() %>%
  select(Year, cases_year, population) %>%
  full_join(data_death2) %>%
  mutate(inc = death/cases_year *100,
         datum=ymd(paste0(Year, "01-01")),
         var="case fatality") %>%
  select(var, Year, datum, inc) %>%
  mutate(inc=ifelse(Year > 1952, NA,inc))


         
data_all <- rbind(data_death,data_cases, data_fat) %>%
  mutate(var= as.factor(var))


plot_cases <- ggplot(data_cases, aes(Year, inc, col="Incidence")) + 
  geom_line() +
  ylab("Incidence per 100'000 inhabitants")+
  theme_bw() +
  theme(
    axis.line = element_line(),
    axis.text.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_text(size=20),
    axis.title.y  = element_text(size=20)) 


plot_deaths <- ggplot(data_death, aes(Year, inc,col="Mortality")) + 
  geom_line() +
  ylab("Mortality per 100'000 inhabitants")+
  theme_bw() +
  theme(
    axis.line = element_line(),
    axis.text.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_text(size=20),
    axis.title.y  = element_text(size=20)) 


plot_fat<- ggplot(data_fat, aes(Year, inc,col="Case fatality rate")) + 
  geom_line() +
  ylab("Case fatality rate in %")+
  theme_bw() +
  theme(
    axis.line = element_line(),
    axis.text.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_text(size=20),
    axis.title.y  = element_text(size=20)) 

# find the coefficenct
rebase_coef <- data_all %>% 
  group_by(var) %>% 
  summarise(rebase_coef = max(inc, na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(rebase_coef = rebase_coef / max(rebase_coef))


plot_total <- ggplot()+
  geom_vline(xintercept=1943, lwd=1, col="black", alpha=0.3) +
  geom_line(data=data_cases, aes(Year, inc, col="Incidence"),lwd=lwd_line) +
  geom_line(data=data_death, aes(Year, inc/0.209, col="Mortality"),lwd=lwd_line) +
  geom_line(data=data_fat, aes(Year, inc/0.0420, col="Case fatality rate"),lwd=lwd_line) +
  ylab("Incidence per 100'000 inhabitants") +
  scale_color_manual("", 
                     labels=c("Case fatality rate", "Incidence","Mortality"),
                     values=c(mypalette_c[1],mypalette_c[3],mypalette_c[4]))+

 
  xlab("Year") +

  scale_x_continuous( "Year",
                      labels = c(seq(1880,2020,10)),
                      breaks = c(seq(1880,2020,10)))+

  # scale_x_continuous( "Year", 
  #                     labels = as.character(seq(min(data_all$Year), max(data_all$Year), by = 15)), 
  #                     breaks = seq(min(data_all$Year), max(data_all$Year), by = 15), 
  #                     expand=c(0,0))+

  theme_bw() +
  theme(
    # axis.line = element_line(),
    #       plot.margin = margin(10, 10, 10, 30),
    axis.line = element_line(),
    plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=20),
    legend.position = c(0.8,0.7),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.key.size = unit(2.5, 'cm'),
    legend.spacing.x = unit(1.5, 'cm'),
    legend.text=element_text(size=20),
    # legend.key.size = unit(3.5, 'cm'),
    # legend.spacing.x = unit(3.5, 'cm'),
    axis.text.x = element_text(size=20),
    axis.title.x  = element_text(size=20),
    axis.title.y  = element_text(size=20)) 


Figure2 <- wrap_elements(get_plot_component(plot_fat, "ylab-l")) +
  wrap_elements(get_y_axis(plot_fat)) +
  wrap_elements(get_plot_component(plot_deaths, "ylab-l")) +
  wrap_elements(get_y_axis(plot_deaths)) +
  plot_total + 
  plot_layout(widths = c(1, 1, 1, 1, 40))
# plot_layout(widths = c(3, 1, 3, 1, 40))
#   
  
cowplot::save_plot(paste0("output/Figure2.pdf"), Figure2,base_height=10,base_width=20)
