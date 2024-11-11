rm(list = ls(all.names = T))

library(rstan)
# library(loo)
library(coda)
library(future.apply)


# data prep ---------------------------------------------------------------

# simulated pa (based on mean ind-lvl par values)
pa_co_sim = readRDS("analyses/modeling/02_pa_co_sim.rds")
names(pa_co_sim) = c('p17', 's16')

# data
load("analyses/modeling/00_p17_mod_dat_stan.RData")
load("analyses/modeling/00_s16_mod_dat_stan.RData")

#
M = lapply(list(p17 = d_pachur2017, s16 = d_suter2016), function(x) x$cpt_nmon_radw)

# no of replications
L = 10

# list with simulated choices
Ms = lapply(names(M), function(x) {
  
  co = replicate(L, apply(pa_co_sim[[x]]$cpt_nmon_radw, 1:2,
                           rbinom, n = 1, size = 1),
                 simplify = F)
  
  M[[x]]$d$co = co
  
  return(M[[x]])
  
}); names(Ms) = names(M)

# clean
rm(d_suter2016, d_pachur2017)

# stan fit ----------------------------------------------------------------

# sampler parameters
n_chains = 4
n_iter = 2e3
n_burn = 1e3
n_thin = 2
n_cores = n_chains

# compile stan model
trans = stanc(file = Ms$p17$mod)
compiled = stan_model(stanc_ret = trans, verbose = F)

# parameters to monitor
gp_s = Ms$p17$pp$g_pars
ip_s = Ms$s16$pp$i_pars

# list for storing the results
rec_pars = list()

# loop
for(i in names(Ms)) {
 
  rec_pars[[i]] = list()
  
  for(l in 1:L) {
    
    # data 
    stan_data = Ms[[i]]$d
    stan_data$co = Ms[[i]]$d$co[[l]]
    
    # parameters to monitor
    pars = c(Ms[[i]]$pp$g_pars, 
             Ms[[i]]$pp$i_pars)
    
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
    
    # posterior samples
    p_pars = extract(stanfit)[c(gp_s)]
    i_pars = extract(stanfit)[c(ip_s)]
    
    # list with results
    rec_pars[[i]][[l]] = list(p_pars = p_pars,
                            i_pars = i_pars)
    
    # clean the env
    rm(stanfit, p_pars, i_pars)
    
    print(l)
    
  }

}

saveRDS(rec_pars, file = 'analyses/modeling/posteriors/stan/rec.rds')

# results -----------------------------------------------------------------

# modeling results and data
p17 = readRDS("analyses/modeling/posteriors/stan/p17.rds")
s16 = readRDS("analyses/modeling/posteriors/stan/s16.rds")

#
gen_pars = list(p17 = p17$cpt_nmon_radw$pars,
                s16 = s16$cpt_nmon_radw$pars)

rm(s16, p17)

# id-level cors
grp_cor = lapply(c(p17 = 'p17', s16 = 's16'), function(d) {

  # the id-lvl figure
  pdf(paste0('analyses/figures_results/figs/par_rec/ID_LVL_', d, '.pdf'),
      height = L * 4 * 0.393701,
      width = 16 * 0.393701,
      pointsize = 8)

  par(mfrow = c(L, 4),
      mar = c(4, 4, 3, 1))
    
  gr_c = sapply(1:L, function(l) {
  
    # get rec id-lvl means
    rip = sapply(rec_pars[[d]][[l]]$i_pars, colMeans)
    
    # get gen id-lvl means
    gip = sapply(gen_pars[[d]]$i_pars, colMeans)
    
    # get correlations
    gr_cor = diag( cor(gip, rip) )
    
    # id-lvl scatter plot
    for(i in 1:4) {
      
      xy = cbind(gip[,i], rip[,i])
      
      plot(xy,
           xlim = c(min(xy), max(xy)),
           ylim = c(min(xy), max(xy)),
           xlab = '',
           ylab = '',
           col = rgb(.7, .1, .9, .5),
           main = paste0(dimnames(gip)[[2]][i], 
                         ' (r = ', round(gr_cor[i], 2), ')')
      )
      mtext('generating', 1, line = 2, cex = .6)
      mtext('recovered', 2, line = 2, cex = .6)
      
      abline(0, 1, lty = 2)

    }
    
    return(gr_cor)
    
  }); dev.off()
  
  # POP-LVL figure
  pdf(paste0('analyses/figures_results/figs/par_rec/POP_LVL_', d, '.pdf'),
      height = 4 * 0.393701,
      width = 16 * 0.393701,
      pointsize = 8)
  
  par(mfrow = c(1, 4),
      mar = c(4, 4, 3, 1))
  
  # pop-lvl densities
  for(i in 1:4) {
    
    # the generating dist
    g = gen_pars[[d]]$p_pars[[i]]
    x = density(g)
    
    # recovered dists
    r = sapply(1:L, function(l) rec_pars[[d]][[l]]$p_pars[[i]])
    
    y = lapply(1:L, function(l) density(r[,l]) )
    
    ymax = max(c(x$y, sapply(y, function(x) max(x$y))) )
    
    plot(x, type = 'l',
         lty = 1,
         main = names(gen_pars[[d]]$p_pars)[i],
         xlim = c(0, max(g, r)),
         ylim = c(0, ymax),
         yaxt = 'n',
         ylab = '',
         xlab = '',
         lwd = 2,
         col = rgb(.1, .1, .8, .8))
    
    sapply(y, function(yy) lines(yy, lty = 1, col = rgb(.8, .1, .1, .5)) )
    
    if(i == 1) {
      
      legend(0, .9 * ymax,
             title = 'Parameter',
             legend = c('Estimated', 'Recovered'),
             lty = 1,
             col = c(rgb(.1, .1, .8, .8),
                     rgb(.8, .1, .1, .5)),
             bty = 'n')
      
    }
    
  }; dev.off()
  
  return(gr_c)
  
})
