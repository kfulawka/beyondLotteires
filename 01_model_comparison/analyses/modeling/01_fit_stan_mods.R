rm(list = ls(all.names = T))

# function to fit the model
source('analyses/modeling/functions/00_stan_diags.R')
source('analyses/modeling/functions/binary_accuracy_loo.R')


library(rstan)
library(loo)
library(bayesplot)

# sampler parameters
n_chains = 4
n_iter = 5e3
n_burn = 2e3
n_thin = 2
n_cores = n_chains

# s16 loop ----------------------------------------------------------------

# list for storing the results
m_s16 = list()

# data
load("analyses/modeling/00_s16_mod_dat_stan.RData")

# loop
for(i in names(d_s16)[c(16:20)]) {
  
  # data 
  stan_data = d_s16[[i]]$d
  
  # parameters to monitor
  pars = c(d_s16[[i]]$pp$g_pars, 
           d_s16[[i]]$pp$i_pars)
  
  
  # compile stan model
  trans = stanc(file = d_s16[[i]]$mod)
  compiled = stan_model(stanc_ret = trans, verbose = F)
  
  # sample from posterior
  stanfit = try(sampling(object = compiled,
                         data = stan_data,
                         pars = c(pars, 'log_lik'),
                         init = 0,
                         chains = n_chains,
                         iter = n_iter,
                         warmup = n_burn,
                         thin = n_thin,
                         cores = n_chains,
                         control = NULL))
  
  # model performance -------------------------------------------------------
  
  # approximate loo 
  looE = rstan::loo(stanfit,
                    moment_match = F)
  
  # loo balanced accuracy
  ba = binary_accuracy_loo(stanfit,
                           stan_data$co,
                           binary_cutoff = .5)

  # loo ind accuracy
  ind_ba_loo = ID_binary_accuracy_loo(stanfit,
                                      y = stan_data$co,
                                      N = 40,
                                      ncp = 44)
  
  # parameters --------------------------------------------------------------
  
  # parameters to save
  gp_s = d_s16[[i]]$pp$g_pars
  ip_s = d_s16[[i]]$pp$i_pars
  
  # print diagnostic plots
  try(stan_diag(stanFit = stanfit,
                n = stan_data$n,
                ind_p = ip_s,
                group_p = gp_s,
                write_path = d_s16[[i]]$save_path))
  
  # posterior samples
  p_pars = extract(stanfit)[c(gp_s)]
  i_pars = extract(stanfit)[c(ip_s)]
  pa = extract(stanfit)[['pa']]
  
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
  m_s16[[i]] = list(pars = list(p_pars = p_pars,
                                i_pars = i_pars),
                    fit_summary = fit_summary,
                    pa = pa,
                    looE = looE,
                    ba = ba,
                    ind_ba = ind_ba_loo,
                    sampling_info = sampling_info)
  
  # clean the env
  rm(stanfit, pars, looE, sampling_info, fit_summary, ba, ind_ba_loo,
     p_pars, i_pars, ip_s, gp_s, trans, compiled)
  
  print(i)
  
}

saveRDS(m_s16, file = 'analyses/modeling/posteriors/stan/s16_dft.rds')

# p17 loop ----------------------------------------------------------------

# list for storing the results
m_p17 = list()

# data
load("analyses/modeling/00_p17_mod_dat_stan.RData")

# loop
for( i in names(d_pachur2017)[9:12]) {

  # data
  stan_data = d_pachur2017[[i]]$d

  # parameters to monitor
  pars = c(d_pachur2017[[i]]$pp$g_pars,
           d_pachur2017[[i]]$pp$i_pars)


  # compile stan model
  trans = stanc(file = d_pachur2017[[i]]$mod)
  compiled = stan_model(stanc_ret = trans, verbose = F)

  # sample from posterior
  stanfit = sampling(object = compiled,
                     data = stan_data,
                     pars = c(pars, 'log_lik'),
                     init = 0,
                     chains = n_chains,
                     iter = n_iter,
                     warmup = n_burn,
                     thin = n_thin,
                     cores = n_chains,
                     control = NULL)

  # model performance -------------------------------------------------------

  # approximate loo
  looE = rstan::loo(stanfit,
                    moment_match = F)

  # loo balanced accuracy
  ba = binary_accuracy_loo(stanfit,
                           stan_data$co,
                           binary_cutoff = .5)

  # loo ind accuracy
  ind_ba_loo = ID_binary_accuracy_loo(stanfit,
                                      y = stan_data$co,
                                      N = 80,
                                      ncp = 44)
  
  # parameters --------------------------------------------------------------

  # parameters to save
  gp_s = d_pachur2017[[i]]$pp$g_pars
  ip_s = d_pachur2017[[i]]$pp$i_pars

  # print diagnostic plots
  try(stan_diag(stanFit = stanfit,
                n = stan_data$n,
                ind_p = ip_s,
                group_p = gp_s,
                write_path = d_pachur2017[[i]]$save_path))

  # posterior samples
  p_pars = extract(stanfit)[c(gp_s)]
  i_pars = extract(stanfit)[c(ip_s)]
  pa = extract(stanfit)[['pa']]

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
  m_p17[[i]] = list(pars = list(p_pars = p_pars,
                                   i_pars = i_pars),
                       fit_summary = fit_summary,
                       pa = pa,
                       looE = looE,
                       ba = ba,
                       ind_ba = ind_ba_loo,
                       sampling_info = sampling_info)

  # clean the env
  rm(stanfit, pars, looE, sampling_info, fit_summary, ba, ind_ba_loo,
     p_pars, i_pars, ip_s, gp_s, trans, compiled)

  print(i)

}

saveRDS(m_p17, file = 'analyses/modeling/posteriors/stan/p17_dft.rds')

# p14 loop ----------------------------------------------------------------

# list for storing the results
m_p14 = list()

# data
load("analyses/modeling/00_p14_mod_dat_stan.RData")

# loop
for(i in names(d_pachur2014)[9:12]) {
  
  # data
  stan_data = d_pachur2014[[i]]$d
  
  # parameters to monitor
  pars = c(d_pachur2014[[i]]$pp$g_pars,
           d_pachur2014[[i]]$pp$i_pars)
  
  
  # compile stan model
  trans = stanc(file = d_pachur2014[[i]]$mod)
  compiled = stan_model(stanc_ret = trans, verbose = F)
  
  # sample from posterior
  stanfit = sampling(object = compiled,
                     data = stan_data,
                     pars = c(pars, 'log_lik'),
                     init = 0,
                     chains = n_chains,
                     iter = n_iter,
                     warmup = n_burn,
                     thin = n_thin,
                     cores = n_chains,
                     control = NULL)
  
  # model performance -------------------------------------------------------
  
  # approximate loo
  looE = rstan::loo(stanfit,
                    moment_match = F)
  
  # loo balanced accuracy
  ba = binary_accuracy_loo(stanfit,
                           stan_data$co,
                           binary_cutoff = .5)
  
  # loo ind accuracy
  ind_ba_loo = ID_binary_accuracy_loo(stanfit,
                                      y = stan_data$co,
                                      N = 80,
                                      ncp = 13)
  
  # parameters --------------------------------------------------------------
  
  # parameters to save
  gp_s = d_pachur2014[[i]]$pp$g_pars
  ip_s = d_pachur2014[[i]]$pp$i_pars
  
  # print diagnostic plots
  try(stan_diag(stanFit = stanfit,
                n = stan_data$n,
                ind_p = ip_s,
                group_p = gp_s,
                write_path = d_pachur2014[[i]]$save_path))
  
  # posterior samples
  p_pars = extract(stanfit)[c(gp_s)]
  i_pars = extract(stanfit)[c(ip_s)]
  pa = extract(stanfit)[['pa']]
  
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
  m_p14[[i]] = list(pars = list(p_pars = p_pars,
                                   i_pars = i_pars),
                       fit_summary = fit_summary,
                       pa = pa,
                       looE = looE,
                       ba = ba,
                       ind_ba = ind_ba_loo,
                       sampling_info = sampling_info)
  
  # clean the env
  rm(stanfit, pars, looE, sampling_info, fit_summary, ba, ind_ba_loo,
     p_pars, i_pars, ip_s, gp_s, trans, compiled)
  
  print(i)
  
}

saveRDS(m_p14, file = 'analyses/modeling/posteriors/stan/p14_dft.rds')

# # h20 loop ----------------------------------------------------------------
# 
# # list for storing the results
# mods_h20 = list()
# 
# # data
# load("analyses/modeling/00_h20_mod_dat_stan.RData")
# 
# # loop
# for(i in names(d_helversen2020)) {
#   
#   # data
#   stan_data = d_helversen2020[[i]]$d
#   
#   # parameters to monitor
#   pars = c(d_helversen2020[[i]]$pp$g_pars,
#            d_helversen2020[[i]]$pp$i_pars)
#   
#   
#   # compile stan model
#   trans = stanc(file = d_helversen2020[[i]]$mod)
#   compiled = stan_model(stanc_ret = trans, verbose = F)
#   
#   # sample from posterior
#   stanfit = sampling(object = compiled,
#                      data = stan_data,
#                      pars = c(pars, 'log_lik'),
#                      init = 0,
#                      chains = n_chains,
#                      iter = n_iter,
#                      warmup = n_burn,
#                      thin = n_thin,
#                      cores = n_chains,
#                      control = NULL)
#   
#   # model performance -------------------------------------------------------
#   
#   # approximate loo
#   looE = rstan::loo(stanfit,
#                     moment_match = F)
#   
#   # loo balanced accuracy
#   ba = binary_accuracy_loo(stanfit,
#                            stan_data$co,
#                            binary_cutoff = .5)
#   
#   # loo ind accuracy
#   ind_ba_loo = ID_binary_accuracy_loo(stanfit,
#                                       y = stan_data$co,
#                                       N = 48,
#                                       ncp = 42)
#   
#   # parameters --------------------------------------------------------------
#   
#   # parameters to save
#   gp_s = d_helversen2020[[i]]$pp$g_pars
#   ip_s = d_helversen2020[[i]]$pp$i_pars
#   
#   # print diagnostic plots
#   try(stan_diag(stanFit = stanfit,
#                 n = stan_data$n,
#                 ind_p = ip_s,
#                 group_p = gp_s,
#                 write_path = d_helversen2020[[i]]$save_path))
#   
#   # posterior samples
#   p_pars = extract(stanfit)[c(gp_s)]
#   i_pars = extract(stanfit)[c(ip_s)]
#   pa = extract(stanfit)[['pa']]
#   
#   # summary table
#   fit_summary = summary(stanfit,
#                         pars = c(gp_s, 'lp__'))[[1]]
#   fit_summary = round(fit_summary, 3)
#   
#   
#   # output ------------------------------------------------------------------
#   
#   # sampling info
#   sampling_info = list(
#     data = stan_data,
#     pars = pars,
#     chains = n_chains,
#     iter = n_iter,
#     warmup = n_burn,
#     thin = n_thin,
#     control = NULL,
#     model = compiled
#   )
#   
#   
#   # list with results
#   mods_h20[[i]] = list(pars = list(p_pars = p_pars,
#                                    i_pars = i_pars),
#                        fit_summary = fit_summary,
#                        pa = pa,
#                        looE = looE,
#                        ba = ba,
#                        ind_ba = ind_ba_loo,
#                        sampling_info = sampling_info)
#   
#   # clean the env
#   rm(stanfit, pars, looE, sampling_info, fit_summary, ba, ind_ba_loo,
#      p_pars, i_pars, ip_s, gp_s, trans, compiled)
#   
#   print(i)
#   
# }
# 
# saveRDS(mods_h20, file = 'analyses/modeling/posteriors/stan/h20_RN.rds')
# 
# # s15 loop ----------------------------------------------------------------
# 
# # list for storing the results
# mods_s15 = list()
# 
# # data
# load("analyses/modeling/00_s15_mod_dat_stan.RData")
# 
# # loop
# for(i in names(d_s15)[c(4, 6)]) {
#   
#   # data
#   stan_data = d_s15[[i]]$d
#   
#   # parameters to monitor
#   pars = c(d_s15[[i]]$pp$g_pars,
#            d_s15[[i]]$pp$i_pars)
#   
#   
#   # compile stan model
#   trans = stanc(file = d_s15[[i]]$mod)
#   compiled = stan_model(stanc_ret = trans, verbose = F)
#   
#   # sample from posterior
#   stanfit = sampling(object = compiled,
#                      data = stan_data,
#                      pars = c(pars, 'log_lik'),
#                      init = 0,
#                      chains = n_chains,
#                      iter = n_iter,
#                      warmup = n_burn,
#                      thin = n_thin,
#                      cores = n_chains,
#                      control = NULL)
#   
#   # model performance -------------------------------------------------------
#   
#   # approximate loo
#   looE = rstan::loo(stanfit,
#                     moment_match = F)
#   
#   # loo balanced accuracy
#   ba = binary_accuracy_loo(stanfit,
#                            stan_data$co,
#                            binary_cutoff = .5)
#   
#   # loo ind accuracy
#   ind_ba_loo = ID_binary_accuracy_loo(stanfit,
#                                       y = stan_data$co,
#                                       N = 80,
#                                       ncp = 13)
#   
#   # parameters --------------------------------------------------------------
#   
#   # parameters to save
#   gp_s = d_s15[[i]]$pp$g_pars
#   ip_s = d_s15[[i]]$pp$i_pars
#   
#   # print diagnostic plots
#   try(stan_diag(stanFit = stanfit,
#                 n = stan_data$n,
#                 ind_p = ip_s,
#                 group_p = gp_s,
#                 write_path = d_s15[[i]]$save_path))
#   
#   # posterior samples
#   p_pars = extract(stanfit)[c(gp_s)]
#   i_pars = extract(stanfit)[c(ip_s)]
#   
#   # summary table
#   fit_summary = summary(stanfit,
#                         pars = c(gp_s, 'lp__'))[[1]]
#   fit_summary = round(fit_summary, 3)
#   
#   
#   # output ------------------------------------------------------------------
#   
#   # sampling info
#   sampling_info = list(
#     data = stan_data,
#     pars = pars,
#     chains = n_chains,
#     iter = n_iter,
#     warmup = n_burn,
#     thin = n_thin,
#     control = NULL,
#     model = compiled
#   )
#   
#   
#   # list with results
#   mods_s15[[i]] = list(pars = list(p_pars = p_pars,
#                                    i_pars = i_pars),
#                        fit_summary = fit_summary,
#                        looE = looE,
#                        ba = ba,
#                        ind_ba = ind_ba_loo,
#                        sampling_info = sampling_info)
#   
#   # clean the env
#   rm(stanfit, pars, looE, sampling_info, fit_summary, ba, ind_ba_loo,
#      p_pars, i_pars, ip_s, gp_s, trans, compiled)
#   
#   print(i)
#   
# }
# 
# saveRDS(mods_s15, file = 'analyses/modeling/posteriors/stan/s15_rev.rds')