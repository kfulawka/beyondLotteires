rm(list = ls(all.names = T))

library(rstan)
# library(loo)
library(coda)
library(future.apply)

# data prep ---------------------------------------------------------------

# simulated pa (based on mean ind-lvl par values)
pa_co_sim = readRDS("analyses/02b_choice_modeling/02_pa_co_sim.rds")

# data
Ms = readRDS("analyses/02b_choice_modeling/mods_gam_phi.rds")

# models
stan_mods = readRDS('analyses/00c_data_analyses/stan_modsT1.rds')
#

# no of replications
L = 30

# list with simulated choices
Msims = lapply(pa_co_sim, function(x) {
  
  co = replicate(L, apply(x, 1:2, rbinom, n = 1, size = 1),
                 simplify = F)
  
})

# stan fit ----------------------------------------------------------------

# sampler parameters
n_chains = 4
n_iter = 2e3
n_burn = 1e3
n_thin = 2
n_cores = n_chains

# list for storing the results
rec_pars = list()

# loop
for(i in names(Ms)) {
  
  # compile stan model
  trans = stanc(file = paste0('analyses/02b_choice_modeling/stan_m/',
                              stan_mods[[i]]$m_name, '.stan'))
  compiled = stan_model(stanc_ret = trans, verbose = F)
  
  # parameters to monitor
  gp_s = stan_mods[[i]]$p_pars
  ip_s = stan_mods[[i]]$i_pars
  pars = c(gp_s, ip_s)
  
  rec_pars[[i]] = list()
  
  for(l in 1:L) {
    
    # data 
    stan_data = Ms[[i]]$sampling_info$data
    stan_data$co = Msims[[i]][[l]]
    
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
    
    # posterior samples
    p_pars = extract(stanfit)[c(gp_s)]
    i_pars = extract(stanfit)[c(ip_s)]
    
    # list with results
    rec_pars[[i]][[l]] = list(p_pars = p_pars,
                              i_pars = i_pars)
    
    # clean the env
    rm(stanfit, p_pars, i_pars)
    
    print( paste0('ITERATION NO ', l, ' COMPLETED') )
    
  }
  
}

saveRDS(rec_pars, file = 'analyses/02b_choice_modeling/par_recs.rds')

# results -----------------------------------------------------------------

rec_pars = readRDS('analyses/02b_choice_modeling/par_recs.rds')

# modeling results and data
mods = readRDS("analyses/02b_choice_modeling/mods_gam_phi.rds")

L = length(rec_pars$cpt_gt1)

# gather into a single list for easier processing
gen_pop_pars = list(mu_gam_pt = mods$cpt_gt1$pars$p_pars$mu_gam,
                    mu_gam_aw = mods$cpt_gaw_t1$pars$p_pars$mu_gam,
                    mu_gam_ap = mods$cpt_vcov_gt1$pars$p_pars$mu_gam_ap,
                    mu_gam_ar = mods$cpt_vcov_gt1$pars$p_pars$mu_gam_ar,
                    mu_theta_pt = mods$cpt_gt1$pars$p_pars$mu_theta,
                    mu_theta_aw = mods$cpt_gaw_t1$pars$p_pars$mu_theta,
                    mu_theta = mods$cpt_vcov_gt1$pars$p_pars$mu_theta)

#
xlims = c(rep(1, 4), rep(5, 3))
xlabs = c(rep("\u03B3", 4), rep("\u03B8", 3))
mlabs = c('AV', 'AV-APW', 'AV-2PWF', 
          'AV-2PWF', '', '', '')


# pop-lvl fig -------------------------------------------------------------
cairo_pdf(paste0('results_figs/figs/FigB01_Gamma_parrec.pdf'),
          height = 8 * 0.393701,
          width = 16 * 0.393701,
          pointsize = 10)

par(mfrow = c(2, 4),
    mar = c(3, 2, 2, 1))

# pop-lvl densities
for(i in 1:length(gen_pop_pars)) {
  
  pn = names(gen_pop_pars)[i]
  pn = gsub('_pt|_aw', '', pn)
  
  # the generating dist
  g = gen_pop_pars[[i]]
  x = density(g)
  
  # recovered dists
  if(i %in% c(1, 5)) mm = 'cpt_gt1'
  if(i %in% c(2, 6)) mm = 'cpt_gaw_t1'
  if(i %in% c(3, 4, 7)) mm = 'cpt_vcov_gt1'
  
  r = sapply(1:L, function(l) rec_pars[[mm]][[l]]$p_pars[[pn]])
  #
  y = lapply(1:L, function(l) density(r[,l]) )
  # total rec
  y_comb = density(r)
  
  #
  ymax = max(c(x$y, y_comb$y, sapply(y, function(x) max(x$y))) )
  
  plot(x, type = 'l',
       lty = 1,
       xlim = c(0, xlims[i]),
       ylim = c(0, ymax),
       log = '',
       yaxt = 'n',
       ylab = '',
       xlab = '',
       lwd = 2,
       main = mlabs[i],
       col = rgb(.1, .1, .8, .8))
  title(xlab = xlabs[i], line = 2)
  
  sapply(y, function(yy) lines(yy, lty = 1, col = rgb(.8, .1, .1, .2)) )
  # rec tot
  lines(y_comb, lwd = 2, col = rgb(.8, .1, .1, 1))
  lines(x, lwd = 2, col = rgb(.1, .1, .8, 1))
  
  if(i == 2) {

    legend(.3, .9 * ymax,
           title = 'Parameter',
           legend = c('Estimated', 'Recovered'),
           lty = 1,
           col = c(rgb(.1, .1, .8, .8),
                   rgb(.8, .1, .1, .5)),
           bty = 'n')

  }
  
  if(i == 3) text(.8, .95 * ymax, 'affect poor')
  if(i == 4) text(.8, .95 * ymax, 'affect rich')
    
}

dev.off()