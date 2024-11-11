rm(list = ls(all.names = T))

library(future.apply)

# # data
# load("analyses/modeling/00_p17_mod_dat_stan.RData")
# load("analyses/modeling/00_s16_mod_dat_stan.RData")

# modeling results and data
mods_pach17 = readRDS("analyses/modeling/posteriors/stan/p17_rev.rds")
mods_sut16 = readRDS("analyses/modeling/posteriors/stan/s16_rev.rds")

# function to simulate the choices
source('analyses/modeling/functions/01_cpt_mods.R')

# data for choice simulations

# choice simulation -------------------------------------------------------

# models for simulations
mod_sim = list(pa17_sim = mods_pach17[c('cpt_nmon_wtp', 'cpt_nmon_ar', 'cpt_nmon_adw')],
               su16a_sim = mods_sut16[c('cpt_nmon_wtp1', 'cpt_nmon_ar1', 'cpt_nmon_adw1')],
               su16b_sim = mods_sut16[c('cpt_nmon_wtp2', 'cpt_nmon_ar2', 'cpt_nmon_adw2')])
names(mod_sim$su16a_sim) = names(mod_sim$pa17_sim)
names(mod_sim$su16b_sim) = names(mod_sim$pa17_sim)

# add simulation settings
mod_sim = lapply(mod_sim, function(x) {
  
  x$cpt_nmon_wtp$set = c(ar = F, apwf = F)
  x$cpt_nmon_ar$set = c(ar = T, apwf = F)
  x$cpt_nmon_adw$set = c(ar = T, apwf = T)
  
  return(x)
  
})

# simulate choices for each data set of interes
co_pa = lapply(mod_sim, function(dat) {
  
  # for each model of interest
  lapply(dat, function(mm) {
    
    # data
    dd = mm$sampling_info$data
    N = dd$n
    
    # list with ind-lvl post pars
    ipp = mm$pars$i_pars
    
    # no of par
    np = length(ipp)
    
    # par names
    par_n = names(ipp)
    
    # posterior medians
    ip = sapply(ipp, function(x) apply(x, 2, median))
    
    # plan(multisession)
    co_pa = sapply(1:N, FUN = function(i) {
      
      # outcome values
      xy = cbind(dd$xA[,i], dd$xB[,i])
      
      # simulate choices of pa
      pa = cpt_mods_pr(xy = xy,
                       p = dd$p,
                       a = ip[i,'alp'],
                       g = ip[i,'gam'],
                       d = ip[i,'del'],
                       t = ip[i,'theta'],
                       ar = mm$set['ar'],
                       apwf = mm$set['apwf'],
                       out = 'pa')
      
    })
    
  })
  
})

# save the results
saveRDS(co_pa, file = 'analyses/modeling/02_pa_co_sim.rds')