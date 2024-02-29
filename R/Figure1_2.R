load("data/results_lyl.RData")



Figure1_2 <- ggplot()+
  # geom_line(data=data_cases, aes(x=as.POSIXct(datum), y=inc_fit, col="fitting values"), lwd=3) +
  geom_line(data=results_lyl, aes(x=Year, y= YLL_crude), lwd=lwd_line) +
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



cowplot::save_plot(paste0("output/Figure1_2.pdf"), Figure1_2,base_height=10,base_width=20)