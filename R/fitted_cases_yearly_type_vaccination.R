function_fitted_cases_vacc <- function(Type_vacc,Start_year, End_year, Year_obl,Season_length) {
  
  # 
  load("data/cases_canton_yearly.RData")


dat.cases <- cases_canton_yearly %>%
  filter(Type_vaccination ==Type_vacc) %>%
  distinct(Year, .keep_all = TRUE) %>%
  filter(Year >=Start_year &  Year <= End_year) %>%
  mutate(cases_pre = cases_year_l,
    cases_pre = ifelse(Year > Year_obl, NA, cases_pre))

control.family <- inla.set.control.family.default()

hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

formula <- cases_pre ~ 1 + offset(log(pop_c3)) + Year +
    f(seasID, model='seasonal', season.length = Season_length) 

    reg_data <-  dat.cases %>%
    arrange(Year) %>%
    group_by(Year) %>%
    mutate(trendID = cur_group_id(),
           seasID=trendID,
           MonthID=Month,
           YearID = Year) %>%
    arrange(trendID) %>%
    ungroup()
    
    set.seed(20220421)
   
    inla.mod <- inla(formula,
                     data=reg_data,
                     family="nbinomial",
                     # family="Poisson",
                     # family = "zeroinflatednbinomial1",
                     #verbose = TRUE,
                     control.family = control.family,
                     control.compute = list(config = TRUE),
                     control.mode = list(restart = TRUE),
                     control.predictor = list(compute = TRUE, link = 1))
    
  post.samples <- inla.posterior.sample(n = 1000, result = inla.mod, seed=20220421)
  predlist <- do.call(cbind, lapply(post.samples, function(X)
    exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  rate.drawsMed<-array(unlist( predlist), dim=c(dim(reg_data)[1], 1000)); dim(rate.drawsMed) 
  dM = as.data.frame(rate.drawsMed)
  
  # Add to the data and save
  Data= cbind(reg_data,dM)
  
  fitted_cases_year <- Data %>%
    select(starts_with("V"), "Year", "Type_vaccination", "cases_year_l","cases_pre","pop_l") %>%
    rowwise(Year) %>%
    mutate(fit = median(c_across(V1:V1000)),
           LL = quantile(c_across(V1:V1000), probs= 0.025),
           UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
    select(Type_vaccination, Year, pop_l, cases_year_l,cases_pre,fit, LL, UL) %>%
    arrange(Year)

  write.xlsx(fitted_cases_year ,paste0("data/fitted_cases_year_",Type_vacc,".xlsx"), rowNames=FALSE, overwrite = TRUE)
  save(fitted_cases_year,file=paste0("data/fitted_cases_year_",Type_vacc,".RData"))

}

function_fitted_cases_vacc(Type_vacc="Recommendation",Start_year=1910, End_year=1968,Year_obl=1943, Season_length=12)
function_fitted_cases_vacc(Type_vacc="Obligatory",Start_year=1910, End_year=1968,Year_obl=1943,Season_length=12)



