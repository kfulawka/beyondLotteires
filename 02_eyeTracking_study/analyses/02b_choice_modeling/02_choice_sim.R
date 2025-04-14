rm(list = ls(all.names = T))

library(future.apply)

# data
stan_data = readRDS('analyses/00c_data_analyses/stan_data.rds')

# modeling and data
mods = readRDS("analyses/02b_choice_modeling/mods_gam_phi.rds")

# function to simulate the choices
source('analyses/02b_choice_modeling/functions/01_cpt_mods.R')

# data for choice simulations

# choice simulation -------------------------------------------------------

# add simulation settings
mods$cpt_gt1$set = c(ar = T, apwf = F)
mods$cpt_gaw_t1$set = c(ar = T, apwf = T)
mods$cpt_vcov_gt1$set = c(ar = T, apwf = F)

# simulate choices for each data set of interest
co_pa = lapply(names(mods), function(mn) {
  
  mm = mods[[mn]]
  
  # data
  dd = mm$sampling_info$data
  N = dd$N
  
  # list with ind-lvl post pars
  ipp = mm$pars$i_pars
  
  # no of par
  np = length(ipp)
  
  # par names
  par_n = names(ipp)
  
  # sub
  sub = dd$sub
  
  # posterior medians
  ip = sapply(ipp, function(x) apply(x, 2, median))
  
  # if it's the standard or APWF model
  # use the estimated deltas & gammas for both conds
  if(mn != 'cpt_vcov_gt1') {
    
    ip = cbind(gam_ap = ip[,'gam'], gam_ar = ip[,'gam'], theta = ip[,'theta'])
    
  }
  
  # plan(multisession)
  co_pa = lapply(1:N, FUN = function(i) {
    
    # print(i)
    
    # outcome values --- COND: AFFECT-POOR
    xy_poor = cbind(dd$xa[sub==i, 1], dd$xb[sub==i, 1])
    p_poor = cbind(dd$pa[sub==i, 1], dd$pb[sub==i, 1])
    
    # outcome values --- COND: AFFECT-RICH
    xy_rich = cbind(dd$xa[sub==i, 2], dd$xb[sub==i, 2])
    p_rich = cbind(dd$pa[sub==i, 2], dd$pb[sub==i, 2])
    
    pa_co_poor = c()
    pa_co_rich = c()
    
    for(j in 1:nrow(xy_poor)) {
      
      # simulate choices of pa -- COND
      pa_co_poor[j] = cpt_mods_pr(xy = -xy_poor[j,],
                                  p = p_poor[j,],
                                  g = ip[i,'gam_ap'],
                                  d = 1,
                                  t = ip[i,'theta'],
                                  ar = mm$set['ar'],
                                  apwf = mm$set['apwf'],
                                  out = 'pa')
      
      
      
      
      # simulate choices of pa -- COND
      pa_co_rich[j] = cpt_mods_pr(xy = -xy_rich[j,],
                                  p = p_rich[j,],
                                  g = ip[i,'gam_ar'],
                                  d = 1,
                                  t = ip[i,'theta'],
                                  ar = mm$set['ar'],
                                  apwf = mm$set['apwf'],
                                  out = 'pa')
    }
    
    
    
    return(cbind(pa_co_poor, pa_co_rich))
    
  }); 
  
  r = do.call(rbind, co_pa)
  
}); names(co_pa) = names(mods)

# save the results
saveRDS(co_pa, file = 'analyses/02b_choice_modeling/02_pa_co_sim.rds')