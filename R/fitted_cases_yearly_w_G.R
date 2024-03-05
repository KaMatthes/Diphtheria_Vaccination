function_fitted_cases <- function(Kt, Start_year, End_year, Year_obl,Season_length) {
  
  # 
  load("data/cases_canton_yearly.RData")
  # load("data/cases_canton_monthly.RData")

dat.cases <- cases_canton_yearly %>%
  select(-Canton, -Datum) %>%
  # filter(Canton2 ==Kt) %>%
  filter(Canton3 ==Kt) %>%
  distinct(Year, .keep_all = TRUE) %>%
  filter(Year >=Start_year &  Year <= End_year) %>%
  mutate(cases_pre = cases_c3,
         cases_pre = ifelse(Year > Year_obl, NA, cases_pre))

control.family <- inla.set.control.family.default()

hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

  # formula <- death ~ 1 + offset(log(pop.monthly))  +  as.factor(Month) +
  #   f(YearID, model='iid',hyper=hyper.iid) +
  #   # f(MonthID, model='iid',hyper=hyper.iid) +
  #   f(timeID, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid)
  # # f(timeID, model='seasonal',season.length=12)
  
  
  formula <- cases_pre ~ 1 + offset(log(pop_c3)) + Year+
    # f(MonthID,model='iid',  constr = TRUE,hyper=hyper.iid)+
    # f(MonthID,model='rw1', scale.model = TRUE, cyclic = TRUE)+
    # f(trendID, model='ar1', constr = TRUE) 
    # f(treID, model='ar1', cyclic = TRUE,hyper=hyper.iid) +
    # f(seasID, model='iid',  constr = TRUE,hyper=hyper.iid)
   f(seasID, model='seasonal', season.length = Season_length)
  # #
    # f(seasID2, model='rw1',  constr = TRUE, scale.model = TRUE, cyclic = TRUE)
    # 
    # f(trendID, model='ar1') +
    # f(seasID2, model='seasonal', season.length = Season_length)
    # 

  # formula <- cases_pre ~ 1 + offset(log(population)) + 
  #   f(trendID, model='ar1')+
  #   f(seasID, model='seasonal', season.length = 23)

  reg_data <-  dat.cases %>%
    arrange(Year) %>%
    group_by(Year) %>%
    mutate(trendID = cur_group_id(),
           seasID=trendID,
           MonthID=Month) %>%
    arrange(trendID) %>%
    ungroup()
      
    # n.seas <- 150
    # n <-  nrow(reg_data)
    #   rep(1:n.seas, ceiling(n/n.seas))[1:n]
    #   
      
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
  
  fitted_cases_year <- Data %>%
    select(starts_with("V"), "Year", "Canton3", "cases_c3","cases_pre","pop_c3") %>%
    rowwise(Year) %>%
    mutate(fit = median(c_across(V1:V1000)),
           LL = quantile(c_across(V1:V1000), probs= 0.025),
           UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
    select(Canton3, Year,pop_c3, cases_c3,cases_pre,fit, LL, UL) %>%
    # filter(Year==YEAR) %>%
    arrange(Year)

  write.xlsx(fitted_cases_year,paste0("data/fitted_cases_year_",Kt,".xlsx"), rowNames=FALSE, overwrite = TRUE)
  save(fitted_cases_year,file=paste0("data/fitted_cases_year_",Kt,".RData"))

}

function_fitted_cases(Kt="Total_GE", Start_year= 1910, End_year=1945, Year_obl=1932,Season_length=12)
function_fitted_cases(Kt="GE_2", Start_year= 1910, End_year=1945, Year_obl=1932,Season_length=12)



