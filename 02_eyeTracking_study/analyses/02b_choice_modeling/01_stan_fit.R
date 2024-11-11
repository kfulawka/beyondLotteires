rm(list = ls(all.names = T))

# function to fit the model
source('analyses/02b_choice_modeling/functions/00_stan_diags.R')
source('analyses/02b_choice_modeling/functions/binary_accuracy_loo.R')

library(rstan)
library(loo)
library(bayesplot)

# sampler parameters
n_chains = 4
n_iter = 5e2
n_burn = 2e2
n_thin = 2
n_cores = n_chains

# list for storing the results
mods = list()

# data
stan_data = readRDS('analyses/00c_data_analyses/stan_data.rds')

# models
stan_mods = readRDS('analyses/00c_data_analyses/stan_mods.rds')

# xa = mean(xa1, xa2) BY DEFAULT

# # set pre-affect ratings as data
# stan_data$xa = stan_data$xa1
# stan_data$xb = stan_data$xb1

# # set post-affect ratings as data
# stan_data$xa = stan_data$xa2
# stan_data$xb = stan_data$xb2

stan_data[c('xa1', 'xa2', 'xb1', 'xb2')] = NULL

# loop
for(i in names(stan_mods)[6]) {
  
  # parameters to monitor
  pars = c(stan_mods[[i]]$p_pars,
           stan_mods[[i]]$i_pars,
           'log_lik_ap', 'log_lik_ar')
  
  # stan model
  stan_m = stan_mods[[i]]$m_name
  
  # compile stan model
  trans = stanc(file = paste0('analyses/02b_choice_modeling/stan_m/', stan_m, '.stan'))
  compiled = stan_model(stanc_ret = trans, verbose = F)
  
  # sample from posterior
  stanfit = try(sampling(object = compiled,
                         data = stan_data,
                         pars = pars,
                         init = 0,
                         chains = n_chains,
                         iter = n_iter,
                         warmup = n_burn,
                         thin = n_thin,
                         cores = n_chains,
                         control = NULL))
  
  
  
  # performance by condition ------------------------------------------------
  perf_cond = lapply(list(AP = 1, AR = 2), function(ii) {
    
    # ll = ifelse(ii == 1, 'log_lik_ap', 'log_lik_ar')
    ll = extract_log_lik(stanfit,
                         parameter_name = ifelse(ii == 1, 
                                                 'log_lik_ap', 'log_lik_ar'))
    
    # approximate loo 
    looE = rstan::loo(stanfit,
                      pars = ifelse(ii == 1, 'log_lik_ap', 'log_lik_ar'),
                      moment_match = F)
    
    # loo balanced accuracy
    ba = binary_accuracy_loo(ll,
                             stan_data$co[,ii],
                             binary_cutoff = .5)
    
    # loo ind accuracy
    ind_ba_loo = ID_binary_accuracy_loo(ll,
                                        y = stan_data$co[,ii],
                                        N = stan_data$N,
                                        ncp = 60)
    
    return(list(looE = looE, ba = ba, ind_perf = ind_ba_loo))
    
  })
  
  # total performance -------------------------------------------------------
  
  # combine logliks
  llp = extract(stanfit)[['log_lik_ap']]
  llr = extract(stanfit)[['log_lik_ar']]
  llt = cbind(llp, llr)
  
  # total loo
  reff_t = relative_eff(exp(llt), chain_id = rep(1:4, each = nrow(llt)/4))
  loo_tot = loo(llt, r_eff = reff_t)
  
  # total accuracy
  ba_tot = binary_accuracy_loo(llt,
                               stan_data$co,
                               binary_cutoff = .5)
  
  # total ind accuracy
  ind_ba_loo = ID_binary_accuracy_loo(llt,
                                      y = stan_data$co,
                                      N = stan_data$N,
                                      ncp = 120)
  
  perf = list(looE = loo_tot, 
              ba = ba_tot, 
              ind_perf = ind_ba_loo)
  
  # parameters --------------------------------------------------------------
  
  # parameters to save
  gp_s = stan_mods[[i]]$p_pars
  ip_s = stan_mods[[i]]$i_pars
  
  # print diagnostic plots
  try(stan_diag(stanFit = stanfit,
                n = stan_data$n,
                ind_p = ip_s,
                group_p = gp_s, #[!grepl('sigma', gp_s)],
                write_path = paste0('analyses/02b_choice_modeling/stan_diag/', stan_m)
  ))
  
  # posterior samples
  p_pars = extract(stanfit)[c(gp_s)]
  i_pars = extract(stanfit)[c(ip_s)]
  
  # summary table
  fit_summary = summary(stanfit,
                        pars = c(gp_s, 'lp__'))[[1]]
  fit_summary = round(fit_summary, 3)
  
  # output ------------------------------------------------------------------
  
  # sampling info
  sampling_info = list(
    data = stan_data,
    pars = pars,
    chains = n_chains,
    iter = n_iter,
    warmup = n_burn,
    thin = n_thin,
    control = NULL,
    model = compiled
  )
  
  # list with results
  mods[[i]] = list(pars = list(p_pars = p_pars,
                               i_pars = i_pars),
                   fit_summary = fit_summary,
                   perf = perf,
                   perf_cond = perf_cond,
                   sampling_info = sampling_info)
  

  # env cleaninig -----------------------------------------------------------
  rm(stanfit, pars, sampling_info, fit_summary, 
     perf, perf_cond, llp, llr, llt, reff_t, loo_tot, ba_tot, ind_ba_loo,
     p_pars, i_pars, ip_s, gp_s, trans, compiled)
  
  print(i)
  
}

saveRDS(mods, file = 'analyses/02b_choice_modeling/modsDG.rds')