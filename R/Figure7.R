
#plot parameter

size_plot <- 30
size_plot_axis <- 30
lwd_line <- 1.5

#data

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
    strip.text = element_text(size=size_plot),
    axis.line = element_line(),
    axis.text.y = element_text(size=size_plot),
    legend.position = c(0.8,0.85),
    legend.text=element_text(size=size_plot),
    legend.key.width= unit(5, "line"),
    plot.title = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot_axis),
    axis.title.y  = element_text(size=size_plot_axis)) 

Figure72 <-ggplot(data=subset(data_pred, Var=="Incidence - Compulsory vaccination"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
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
    strip.text = element_text(size=size_plot),
    axis.line = element_line(),
    axis.text.y = element_text(size=size_plot),
    legend.position = c(0.8,0.85),
    legend.text=element_text(size=size_plot),
    legend.key.width= unit(5, "line"),
    plot.title = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot_axis),
    axis.title.y  = element_text(size=size_plot_axis)) 

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
    strip.text = element_text(size=size_plot),
    axis.line = element_line(),
    axis.text.y = element_text(size=size_plot),
    legend.position = c(0.8,0.85),
    legend.text=element_text(size=size_plot),
    legend.key.width= unit(5, "line"),
    plot.title = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot_axis),
    axis.title.y  = element_text(size=size_plot_axis)) 

Figure74 <- ggplot(data=subset(data_pred, Var=="Mortality - Compulsory vaccination"))+
  geom_line( aes(x=Year, y=inc_fit, col="fitted",lty="fitted"), lwd=lwd_line) +
  geom_line( aes(x=Year, y=inc, col="observed",lty="observed"),lwd= lwd_line)+
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
    strip.text = element_text(size=size_plot),
    axis.line = element_line(),
    axis.text.y = element_text(size=size_plot),
    legend.position = c(0.8,0.85),
    legend.text=element_text(size=size_plot),
    legend.key.width= unit(5, "line"),
    plot.title = element_text(size=size_plot),
    axis.text.x = element_text(size=size_plot),
    axis.title.x  = element_text(size=size_plot_axis),
    axis.title.y  = element_text(size=size_plot_axis)) 

Figure7 <- cowplot::plot_grid(Figure71, Figure73, Figure72,Figure74,ncol=2)

cowplot::save_plot("output/Figure7.pdf", Figure7 ,base_height=15,base_width=25)


