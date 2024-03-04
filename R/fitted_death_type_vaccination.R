function_fitted_death_vacc <- function(Type_vacc,Start_year, End_year, Year_obl,Season_length) {
  
  
  load("data/death_canton_year.RData")

dat.death <- death_canton_year %>%
  filter(Type_vaccination==Type_vacc) %>%
  distinct(Year, .keep_all = TRUE) %>%
  filter(Year >=Start_year &  Year <= End_year) %>%
  mutate(death_pre = death_l,
         death_pre = ifelse(Year > Year_obl, NA, death_pre))

control.family <- inla.set.control.family.default()

hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

  # formula <- death ~ 1 + offset(log(pop.monthly))  +  as.factor(Month) +
  #   f(YearID, model='iid',hyper=hyper.iid) +
  #   # f(MonthID, model='iid',hyper=hyper.iid) +
  #   f(timeID, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid)
  # # f(timeID, model='seasonal',season.length=12)
  
  # 
  formula <-death_pre ~ 1 + offset(log(pop_l)) + Year +
    # f(trendID, model='ar1')  +
    f(seasID, model='seasonal', season.length = Season_length)


  # formula <- cases_pre ~ 1 + offset(log(population)) + 
  #   f(trendID, model='ar1')+
  #   f(seasID, model='seasonal', season.length = 23)
  # 
  reg_data <- dat.death %>%
    arrange(Year) %>%
    group_by(Year) %>%
    mutate(trendID = cur_group_id(),
           seasID=trendID) %>%
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
                     # num.threads = round(parallel::detectCores() * .2),
                     # verbose=TRUE,
                     control.predictor = list(compute = TRUE, link = 1))
  
    
    # inla.mod$summary.random$t %>% 
    #   ggplot() +
    #   geom_line(aes(ID, mean)) +
    #   geom_ribbon(aes(ID, ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.3)
    
  post.samples <- inla.posterior.sample(n = 1000, result = inla.mod, seed=20220421)
  predlist <- do.call(cbind, lapply(post.samples, function(X)
    exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  rate.drawsMed<-array(unlist( predlist), dim=c(dim(reg_data)[1], 1000)); dim(rate.drawsMed) 
  dM = as.data.frame(rate.drawsMed)
  # Add to the data and save
  Data= cbind(reg_data,dM)
  
  fitted_death_year <- Data %>%
    select(starts_with("V"), "Year", "Type_vaccination", "death_l","death_pre","pop_l") %>%
    rowwise(Year) %>%
    mutate(fit = median(c_across(V1:V1000)),
           LL = quantile(c_across(V1:V1000), probs= 0.025),
           UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
    select(Type_vaccination, Year, pop_l, death_l,death_pre,fit, LL, UL) %>%
    # filter(Year==YEAR) %>%
    arrange(Year)

  write.xlsx(  fitted_death_year ,paste0("data/fitted_death_year_",Type_vacc,".xlsx"), rowNames=FALSE, overwrite = TRUE)
  save(  fitted_death_year,file=paste0("data/fitted_death_year_",Type_vacc,".RData"))

}

function_fitted_death_vacc(Type_vacc="Recommendation",Start_year=1910, End_year=1968,Year_obl=1943, Season_length=12)
function_fitted_death_vacc(Type_vacc="Obligatory",Start_year=1910, End_year=1968,Year_obl=1943,Season_length=12)



