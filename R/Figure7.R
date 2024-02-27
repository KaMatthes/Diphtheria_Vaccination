load("data/fitted_cases_year_Recommendation.RData")
data_recom_inc  <- fitted_cases_year %>%
  rename(cases = cases_year_l)%>%
  rename(population = pop_l)%>%
  select(Year, population, cases, fit, LL, UL) %>%
  mutate(Var = "Incidence - Recommended vaccination")

load("data/fitted_cases_year_Obligatory.RData")
data_obl_ge <- fitted_cases_year %>%
  rename(cases = cases_year_l)%>%
  rename(population = pop_l)%>%
  select(Year, population, cases, fit, LL, UL) %>%
  mutate(Var = "Incidence - Compulsory vaccination")

load("data/fitted_death_year_Recommendation.RData")
data_recom_mort <- fitted_death_year %>%
  rename(cases = death_l)%>%
  rename(population = pop_l )%>%
  select(Year, population, cases, fit, LL, UL) %>%
  mutate(Var = "Mortality - Recommended vaccination")

load("data/fitted_death_year_Obligatory.RData")
data_obl_mort  <- fitted_death_year %>%
  rename(cases = death_l)%>%
  rename(population = pop_l )%>%
  select(Year, population, cases, fit, LL, UL) %>%
  mutate(Var = "Mortality - Compulsory vaccination")



data_pred <- rbind(data_recom_inc,data_obl_ge,data_recom_mort,data_obl_mort ) %>%
  ungroup() %>%
  mutate(inc = cases/population*100000,
         inc_fit = fit/population*100000,
         inc_LL = LL/population*100000,
         inc_UL = UL/population*100000,
         Var= factor(Var, levels = c("Incidence - Recommended vaccination","Mortality - Recommended vaccination","Incidence - Compulsory vaccination","Mortality - Compulsory vaccination")))



Figure71 <-ggplot(data=subset(data_pred, Var=="Incidence - Recommended vaccination"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
  # geom_vline(xintercept=as.POSIXct("1945-02-01"), lwd=4, col="green") +
  geom_vline(xintercept=1944, lwd=1, col="black", alpha=0.3) +
  geom_ribbon(aes(ymin=inc_LL, ymax=inc_UL,x=Year, y=inc_fit), linetype=2, alpha=0.1) +
  xlab("Year") +
  ylab("Incidence per 100'000 inhabitants")+
  ggtitle("Incidence - Recommended vaccination")+
  coord_cartesian(ylim=c(0,300)) +
  scale_color_manual("",
                     values=c("grey40","black"))+
  scale_linetype_manual("",
                        values=c("longdash","solid"))+
  theme_bw() +
  theme(
    strip.text = element_text(size=25),
    axis.line = element_line(),
    # plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=20),
    legend.position = c(0.35,0.85),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.text=element_text(size=20),
    legend.key.width= unit(5, "line"),
    # legend.spacing.x = unit(1.5, 'cm'),
    # legend.spacing.y = unit(0.5, 'cm'),
    plot.title = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x  = element_text(size=25),
    axis.title.y  = element_text(size=25)) 

Figure72 <-ggplot(data=subset(data_pred, Var=="Incidence - Compulsory vaccination"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
  # geom_vline(xintercept=as.POSIXct("1945-02-01"), lwd=4, col="green") +
  geom_vline(xintercept=1944, lwd=1, col="black", alpha=0.3) +
  geom_ribbon(aes(ymin=inc_LL, ymax=inc_UL,x=Year, y=inc_fit), linetype=2, alpha=0.1) +
  xlab("Year") +
  ylab("Incidence per 100'000 inhabitants")+
  ggtitle("Incidence - Compulsory vaccination")+
  coord_cartesian(ylim=c(0,300)) +
  scale_color_manual("",
                     values=c("grey40","black"))+
  scale_linetype_manual("",
                        values=c("longdash","solid"))+
  theme_bw() +
  theme(
    strip.text = element_text(size=25),
    axis.line = element_line(),
    # plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=20),
    legend.position = c(0.35,0.85),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.text=element_text(size=20),
    legend.key.width= unit(5, "line"),
    # legend.spacing.x = unit(1.5, 'cm'),
    # legend.spacing.y = unit(0.5, 'cm'),
    plot.title = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x  = element_text(size=25),
    axis.title.y  = element_text(size=25)) 


Figure73 <- ggplot(data=subset(data_pred, Var=="Mortality - Recommended vaccination"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
  # geom_vline(xintercept=as.POSIXct("1945-02-01"), lwd=4, col="green") +
  geom_vline(xintercept=1944, lwd=1, col="black", alpha=0.3) +
  geom_ribbon(aes(ymin=inc_LL, ymax=inc_UL,x=Year, y=inc_fit), linetype=2, alpha=0.1) +
  xlab("Year") +
  ylab("Mortality per 100'000 inhabitants")+
  ggtitle("Mortality - Recommended vaccination")+
  coord_cartesian(ylim=c(0,25)) +
  scale_color_manual("",
                     values=c("grey40","black"))+
  scale_linetype_manual("",
                        values=c("longdash","solid"))+
  theme_bw() +
  theme(
    strip.text = element_text(size=25),
    axis.line = element_line(),
    # plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=20),
    legend.position = c(0.35,0.85),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.text=element_text(size=20),
    legend.key.width= unit(5, "line"),
    # legend.spacing.x = unit(1.5, 'cm'),
    # legend.spacing.y = unit(0.5, 'cm'),
    plot.title = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x  = element_text(size=25),
    axis.title.y  = element_text(size=25)) 

Figure74 <- ggplot(data=subset(data_pred, Var=="Mortality - Compulsory vaccination"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
  # geom_vline(xintercept=as.POSIXct("1945-02-01"), lwd=4, col="green") +
  geom_vline(xintercept=1944, lwd=1, col="black", alpha=0.3) +
  geom_ribbon(aes(ymin=inc_LL, ymax=inc_UL,x=Year, y=inc_fit), linetype=2, alpha=0.1) +
  xlab("Year") +
  ylab("Mortality per 100'000 inhabitants")+
  ggtitle("Mortality - Compulsory vaccination")+
  coord_cartesian(ylim=c(0,25)) +
  scale_color_manual("",
                     values=c("grey40","black"))+
  scale_linetype_manual("",
                        values=c("longdash","solid"))+
  theme_bw() +
  theme(
    strip.text = element_text(size=25),
    axis.line = element_line(),
    # plot.margin = margin(10, 10, 10, 30),
    axis.text.y = element_text(size=20),
    legend.position = c(0.35,0.85),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    legend.text=element_text(size=20),
    legend.key.width= unit(5, "line"),
    # legend.spacing.x = unit(1.5, 'cm'),
    # legend.spacing.y = unit(0.5, 'cm'),
    plot.title = element_text(size=25),
    axis.text.x = element_text(size=25),
    axis.title.x  = element_text(size=25),
    axis.title.y  = element_text(size=25)) 

Figure7 <- cowplot::plot_grid(Figure71, Figure73, Figure72,Figure74,ncol=2)

cowplot::save_plot("output/Figure7.pdf", Figure7 ,base_height=15,base_width=25)


