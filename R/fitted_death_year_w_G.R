function_fitted_death <- function(Kt, Start_year, End_year, Year_obl,Season_length) {
  
  
  load("data/death_canton_year.RData")

dat.death <- death_canton_year %>%
  filter(Canton3 ==Kt) %>%
  distinct(Year, .keep_all = TRUE) %>%
  filter(Year >=Start_year &  Year <= End_year) %>%
  mutate(death_pre = death_c3,
         death_pre = ifelse(Year > Year_obl, NA, death_pre))

control.family <- inla.set.control.family.default()

  formula <- death_pre ~ offset(log(pop_c3)) + Year +
   f(seasID, model='seasonal', season.length = Season_length) 
  
      reg_data <-  dat.death %>%
        arrange(Year) %>%
        group_by(Year) %>%
        mutate(trendID = cur_group_id(),
               seasID=trendID,
               YearID=Year) %>%
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
  
  fitted_death_year <- Data %>%
    select(starts_with("V"), "Year", "Canton3", "death_c3","death_pre","pop_c3") %>%
    rowwise(Year) %>%
    mutate(fit = median(c_across(V1:V1000)),
           LL = quantile(c_across(V1:V1000), probs= 0.025),
           UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
    select(Canton3, Year, pop_c3, death_c3, death_pre,fit, LL, UL) %>%

    arrange(Year)

  write.xlsx(  fitted_death_year,paste0("data/fitted_death_year_",Kt,".xlsx"), rowNames=FALSE, overwrite = TRUE)
  save(  fitted_death_year,file=paste0("data/fitted_death_year_",Kt,".RData"))

}

function_fitted_death(Kt="Total_GE",Start_year= 1910, End_year=1945,Year_obl=1932,Season_length=12)
function_fitted_death(Kt="GE_2",Start_year= 1910, End_year=1945,Year_obl=1932,Season_length=12)
