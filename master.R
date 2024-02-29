# .libPaths(c("C:/Users/kmatth/AppData/Local/R/win-library/4.2", "C:/Program Files/R/R-4.2.2/library"))
# test
library(tidyverse)
library(openxlsx)
library(data.table)
library(lubridate)
library(INLA)
library(lillies)
library(scales)
library(rmarkdown)
library(wktmo)
library(sf)
library(sp)
library(spdep)
# library(rgdal)
# library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(ggsci)
library(kableExtra)
library(epiR)
library(imputeTS)
library(TTR)
library(strucchange)
library(seasonal)
library(forecast)
library(paletteer)
library(BAMMtools)
library(cowplot)
library(viridis)
library(patchwork)
library(lillies)
library(rgeoda)
library(conflicted)
library(yll)
library(vcdExtra)

conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("summarise", "dplyr")



# 
col_pal <- pal_jco()(8)
# col_saes <-  paletteer::paletteer_d("colorBlindness::Brown2Blue12Steps", direction = 1)

col_saes <-  paletteer::paletteer_dynamic("cartography::turquoise.pal", 12, direction = -1)
col_greys <- paletteer_d("ggthemes::Seattle_Grays")

col_brown <- paletteer_d("ggthemes::excel_Feathered")[2:6]
col_jama <- paletteer_d("ggsci::default_jama")[2:6]

season.labs <- c("Jan","Feb", "Mar","Apr","May","Jun","Jul", "Aug","Sep","Okt", "Nov","Dec")

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
mypalette <- viridis(12, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
mypalette3 <- viridis(3, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
mypalette4 <- viridis(4, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
mypalette_c <- c("#440154FF", "#238a8dff", "#94D840FF", "#FDE725FF")

mypalette2 <- viridis(2, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")

lwd_line <- 1.5
size_text <-15

# source
source("R/Figure_incidence_total.R")
source("R/Figure_incidence_total_year.R")
source("R/Figure_incidence_cantonal.R")
source("R/Figure_mortality_total.R")
source("R/Figure_mortality_cantonal.R")
source("R/Figure_hospital_total.R")
source("R/Figure_case_fatality_total.R")
source("R/Figure_hospital_cantonal.R")
source("R/Figure_cases_total.R")
source("R/Figure_cases_year_total.R")
source("R/Figure_cases_cantonal.R")
source("R/Figure_cases_year_cantonal.R")
source("R/Figure_death_total.R")
source("R/Figure_death_cantonal.R")
source("R/Figure_cases_type_vaccination_year.R")
source("R/Figure_cases_type_vaccination.R")
source("R/Figure_death_type_vaccination_year.R")
source("R/Incidence_5_years.R")
source("R/Incidence_5_years_type_vaccination.R")
source("R/Figure_cases_year_obl.R")
source("R/Figure_death_year_obl.R")
source("R/Figure_monthly_trend.R")
source("R/Figure_incidence_1952.R")
source("R/Figure_incidence_year_1952.R")
source("R/Figure_mortality_1952.R")
# source("R/Maps_quartal.R")


render(paste0("R/Diphteria_Vaccination.Rmd"), output_file = paste0("../output/",today(),"_Report_Diphteria_Vaccination.html"))