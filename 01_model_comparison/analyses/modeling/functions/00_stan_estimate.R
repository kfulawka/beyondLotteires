stan_estimate = function(stan_data,
                         stan_mod,
                         g_pars,
                         g_pars_s = g_pars[!grepl('_Phi', g_pars)],
                         i_pars,
                         i_pars_s = i_pars[!grepl('_Phi', i_pars)],
                         l_pars = 'lpd',
                         n_chains = 4,
                         n_cores = n_chains,
                         n_burn = 2e2,
                         n_thin = 2,
                         n_sample = 4e2,
                         control_list = NULL,
                         loo_ap = T,
                         diag = T,
                         write_path) {
  
  # model preparation
  trans = stanc(file = stan_mod)
  compiled = stan_model(stanc_ret = trans, verbose = F)
  
  # parameters to monitor
  pars = c(g_pars, i_pars, l_pars)
  
  # posterior sampling ------------------------------------------------------  
  stanFit = sampling(object = compiled,
                     data = stan_data,
                     pars = pars,
                     init = 0,
                     chains = n_chains,
                     iter = n_sample,
                     warmup = n_burn,
                     thin = n_thin,
                     cores = n_cores,
                     control = NULL)
  
  
  # diagnostic plots --------------------------------------------------------
  if(diag) {
    
    source('analyses/modeling/functions/00_stan_diags.R')
    
    try(stan_diag(stanFit = stanFit,
                  n = stan_data$n,
                  ind_p = i_pars_s,
                  group_p = g_pars_s,
                  write_path = write_path))
    
  }
  
  # model performance -------------------------------------------------------
  if(loo_ap) {
    
    looE = rstan::loo(stanFit,
                      pars = 'lpd',
                      cores = 1,
                      k_threshold = 0.7,
                      moment_match = T)
    
  } else {
    
    looE = 'not estimated'
    
  }
  
  # output ------------------------------------------------------------------

  # posterior samples
  p_pars = extract(stanFit)[c(g_pars_s)]
  i_pars = extract(stanFit)[c(i_pars_s)]

  # summary table
  fit_summary = summary(stanFit,
                        pars = c(g_pars_s, i_pars_s, 'lp__'))[[1]]

  fit_summary = round(fit_summary, 3)


  # sampling info
  sampling_info = list(
    data = stan_data,
    pars = pars,
    chains = n_chains,
    iter = n_sample,
    warmup = n_burn,
    thin = n_thin,
    cores = n_cores,
    control = control_list,
    model = get_stancode(stanFit)
  )


  # list with results
  res = list(pars = list(p_pars = p_pars,
                         i_pars = i_pars),
             fit_summary = fit_summary,
             looE = looE,
             sampling_info = sampling_info)


  # return ------------------------------------------------------------------
  return(res)
  
}