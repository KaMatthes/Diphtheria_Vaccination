load("data/data_age.RData")

data_age <- data_age %>%
  mutate(age_group= factor(age_group, levels =  c("<1","1-4", "5-14","15-19",
                                       "20-29", "30-39", "40-49",
                                       "50-79", ">=80")),
         mortality = round(mortality,2))
# Heatmap plot

data_jenk <- data_age %>%
  filter(!mortality==0)

jenks_breaks <- c(min(data_jenk['mortality']),natural_breaks(k=10, data_jenk['mortality']),max(data_jenk['mortality']))

data_age <- data_age %>%
  mutate(mort_jenk = cut(mortality,
                         breaks=jenks_breaks,
                         include.lowest = TRUE, right = FALSE),
         mort_jenk=as.character(mort_jenk),
         mort_jenk=replace_na(mort_jenk,"0"),
         mort_jenk= factor(mort_jenk, levels =  c("0","[0.01,0.46)", "[0.46,1.43)","[1.43,2.22)","[2.22,3.32)",
                                                  "[3.32,6.95)", "[6.95,10.8)", "[10.8,13.8)","[13.8,21.5)",
                                                  "[21.5,36.3)", "[36.3,43]")),
         mort_jenk=recode(mort_jenk, "[0.01,0.46)" = "(0.00,0.46)" ))


Figure1 <- ggplot(data_age, aes(Year, age_group, fill= mort_jenk)) + 
  geom_tile()+
  scale_fill_viridis("Mortality", discrete=TRUE,guide = guide_legend(reverse = TRUE)) +
  xlim(c(1878,1968)) +
  scale_x_continuous( "Year",
                      labels = c(seq(1880,1965,10)),
                      breaks = c(seq(1880,1965,10)))+

  xlab("Year")+
  ylab("Age groups")+
  ggtitle("Mortality per year & age group")+
  theme_bw()+
  # scale_x_continuous(
  #   breaks = c(1, 13, 25, 37, 49, 61, 73, 84),
  #   label = c("1831", "1832", "1833", "1834", "1835", "1836", "1837", "1938"))+
  theme(text = element_text(size = 40))+     
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 30),
    legend.position = "right",
    legend.key.width= unit(3, "line"),
    legend.key.size= unit(1.5, 'cm'),
    axis.text.x = element_text(size=30),
    axis.title.x  = element_text(size=30),
    axis.title.y  = element_text(size=30)) 

cowplot::save_plot("output/Figure1.pdf", Figure1 ,base_height=15,base_width=25)

