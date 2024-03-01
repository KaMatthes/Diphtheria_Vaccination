
#plot parameter

size_plot <- 30
size_plot_axis <- 30
lwd_line <- 1.5

#data

load("data/fitted_cases_year_Total_GE.RData")
data_ch_inc  <- fitted_cases_year %>%
  rename(cases = cases_c3)%>%
  rename(population = pop_c3)%>%
  select(Year, population, cases, fit, LL, UL) %>%
  mutate(Var = "Incidence - Switzerland (excluding Geneva)")

load("data/fitted_cases_year_GE_2.RData")
data_ch_ge <- fitted_cases_year %>%
  rename(cases = cases_c3)%>%
  rename(population = pop_c3)%>%
  select(Year, population, cases, fit, LL, UL) %>%
  mutate(Var = "Incidence - Geneva")

load("data/fitted_death_year_Total_GE.RData")
data_ch_mort <- fitted_death_year %>%
  rename(cases = death_c3)%>%
  rename(population = pop_c3)%>%
  select(Year, population, cases, fit, LL, UL) %>%
  mutate(Var = "Mortality - Switzerland (excluding Geneva)")

load("data/fitted_death_year_GE_2.RData")
data_ge_mort  <- fitted_death_year %>%
  rename(cases = death_c3)%>%
  rename(population = pop_c3)%>%
  select(Year, population, cases, fit, LL, UL) %>%
  mutate(Var = "Mortality - Geneva")
  


data_pred <- rbind(data_ch_inc,data_ch_ge,data_ch_mort,data_ge_mort) %>%
  ungroup() %>%
  mutate(inc = cases/population*100000,
         inc_fit = fit/population*100000,
         inc_LL = LL/population*100000,
         inc_UL = UL/population*100000,
         Var= factor(Var, levels = c("Incidence - Switzerland (excluding Geneva)","Mortality - Switzerland (excluding Geneva)","Incidence - Geneva","Mortality - Geneva")))



Figure61 <- ggplot(data=subset(data_pred, Var=="Incidence - Switzerland (excluding Geneva)"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
  # geom_vline(xintercept=as.POSIXct("1945-02-01"), lwd=4, col="green") +
  geom_vline(xintercept=1933, lwd=1, col="black", alpha=0.3) +
  geom_ribbon(aes(ymin=inc_LL, ymax=inc_UL,x=Year, y=inc_fit), linetype=2, alpha=0.1) +
  xlab("Year") +
  ylab("Incidence per 100'000 inhabitants")+
  coord_cartesian(ylim = c(0,50))+
  scale_color_manual("",
                     values=c("grey40","black"))+
  scale_linetype_manual("",
                        values=c("longdash","solid"))+
  ggtitle("Incidence - Switzerland (excluding Geneva)")+
  theme_bw() +
  theme(
    strip.text = element_text(size=size_plot),
    axis.line = element_line(),
    # plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=size_plot),
    legend.position = c(0.8,0.85),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.text=element_text(size=size_plot),
    legend.key.width= unit(5, "line"),
    # legend.spacing.x = unit(1.5, 'cm'),
    # legend.spacing.y = unit(0.5, 'cm'),
    plot.title = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot_axis),
    axis.title.y  = element_text(size=size_plot_axis)) 

Figure62 <- ggplot(data=subset(data_pred, Var=="Incidence - Geneva"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
  # geom_vline(xintercept=as.POSIXct("1945-02-01"), lwd=4, col="green") +
  geom_vline(xintercept=1933, lwd=1, col="black", alpha=0.3) +
  geom_ribbon(aes(ymin=inc_LL, ymax=inc_UL,x=Year, y=inc_fit), linetype=2, alpha=0.1) +
  xlab("Year") +
  ylab("Incidence per 100'000 inhabitants")+
  ggtitle("Incidence - Geneva")+
  coord_cartesian(ylim = c(0,50))+
  scale_color_manual("",
                     values=c("grey40","black"))+
  scale_linetype_manual("",
                        values=c("longdash","solid"))+
  theme_bw() +
  theme(
    strip.text = element_text(size=size_plot),
    axis.line = element_line(),
    # plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=size_plot),
    legend.position = c(0.8,0.85),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.text=element_text(size=size_plot),
    legend.key.width= unit(5, "line"),
    # legend.spacing.x = unit(1.5, 'cm'),
    # legend.spacing.y = unit(0.5, 'cm'),
    plot.title = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot_axis),
    axis.title.y  = element_text(size=size_plot_axis)) 

Figure63 <- ggplot(data=subset(data_pred, Var=="Mortality - Switzerland (excluding Geneva)"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
  # geom_vline(xintercept=as.POSIXct("1945-02-01"), lwd=4, col="green") +
  geom_vline(xintercept=1933, lwd=1, col="black", alpha=0.3) +
  geom_ribbon(aes(ymin=inc_LL, ymax=inc_UL,x=Year, y=inc_fit), linetype=2, alpha=0.1) +
  xlab("Year") +
  ylab("Mortality per 100'000 inhabitants")+
  ggtitle("Mortality - Switzerland (excluding Geneva)")+
  coord_cartesian(ylim = c(0,25))+
  scale_color_manual("",
                     values=c("grey40","black"))+
  scale_linetype_manual("",
                        values=c("longdash","solid"))+
  theme_bw() +
  theme(
    strip.text = element_text(size=size_plot),
    axis.line = element_line(),
    # plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=size_plot),
    legend.position = c(0.8,0.85),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.text=element_text(size=size_plot),
    legend.key.width= unit(5, "line"),
    # legend.spacing.x = unit(1.5, 'cm'),
    # legend.spacing.y = unit(0.5, 'cm'),
    plot.title = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot_axis),
    axis.title.y  = element_text(size=size_plot_axis)) 

Figure64 <- ggplot(data=subset(data_pred, Var=="Mortality - Geneva"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
  # geom_vline(xintercept=as.POSIXct("1945-02-01"), lwd=4, col="green") +
  geom_vline(xintercept=1933, lwd=1, col="black", alpha=0.3) +
  geom_ribbon(aes(ymin=inc_LL, ymax=inc_UL,x=Year, y=inc_fit), linetype=2, alpha=0.1) +
  xlab("Year") +
  ylab("Mortality per 100'000 inhabitants")+
  ggtitle("Mortality - Geneva")+
  coord_cartesian(ylim = c(0,25))+
  scale_color_manual("",
                     values=c("grey40","black"))+
  scale_linetype_manual("",
                        values=c("longdash","solid"))+
  theme_bw() +
  theme(
    strip.text = element_text(size=size_plot),
    axis.line = element_line(),
    # plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=size_plot),
    legend.position = c(0.8,0.85),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.text=element_text(size=size_plot),
    legend.key.width= unit(5, "line"),
    # legend.spacing.x = unit(1.5, 'cm'),
    # legend.spacing.y = unit(0.5, 'cm'),
    plot.title = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot_axis),
    axis.title.y  = element_text(size=size_plot_axis)) 

Figure6 <- cowplot::plot_grid(Figure61, Figure63, Figure62,Figure64,ncol=2)

cowplot::save_plot("output/Figure6.pdf", Figure6 ,base_height=15,base_width=25)

