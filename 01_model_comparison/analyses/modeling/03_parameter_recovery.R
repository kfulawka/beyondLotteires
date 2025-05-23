rm(list = ls(all.names = T))

library(rstan)
# library(loo)
library(coda)
library(future.apply)

# data prep ---------------------------------------------------------------

# simulated pa (based on mean ind-lvl par values)
pa_co_sim = readRDS("analyses/modeling/02_pa_co_sim.rds")
names(pa_co_sim) = c('p17', 's16a', 's16b', 'p14')

# data
load("analyses/modeling/00_p17_mod_dat_stan.RData")
load("analyses/modeling/00_s16_mod_dat_stan.RData")
load("analyses/modeling/00_p14_mod_dat_stan.RData")

#
M = list(p17 = d_pachur2017$cpt_nmon_adw,
         s16a = d_s16$cpt_nmon_aw1,
         s16b = d_s16$cpt_nmon_aw2,
         p14 = d_pachur2014$cpt_nmon_adw)

# no of replications
L = 30

# list with simulated choices
Ms = lapply(names(M), function(x) {
  
  co = replicate(L, apply(pa_co_sim[[x]]$cpt_nmon_adw, 1:2,
                          rbinom, n = 1, size = 1),
                 simplify = F)
  
  M[[x]]$d$co = co
  
  return(M[[x]])
  
}); names(Ms) = names(M)

# clean
rm(d_s16, d_pachur2017, d_pachur2014)

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
ip_s = Ms$p17$pp$i_pars

# list for storing the results
rec_pars = list()

# loop
for(i in names(Ms)[4]) {
  
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

rec_pars = readRDS('analyses/modeling/posteriors/stan/rec.rds')

# modeling results and data
p17 = readRDS("analyses/modeling/posteriors/stan/p17_pt.rds")
s16 = readRDS("analyses/modeling/posteriors/stan/s16_pt.rds")
p14 = readRDS("analyses/modeling/posteriors/stan/p14_pt.rds")

#
gen_pars = list(p17 = p17$cpt_nmon_adw$pars,
                s16a = s16$cpt_nmon_aw1$pars,
                s16b = s16$cpt_nmon_aw2$pars,
                p14 = p14$cpt_nmon_adw$pars)

L = length(rec_pars$p17)

# pop-lvl fig -------------------------------------------------------------
cairo_pdf(paste0('analyses/figures_results/figs/FigA01_parrec.pdf'),
          height = 12 * 0.393701,
          width = 16 * 0.393701,
          pointsize = 10)

par(mar = c(3, 2, 2, 1))
layout(matrix(1:12, 3, 4))

# pop-lvl figs
lapply(c(p17 = 'p17', s16a = 's16a', s16b = 's16b', p14 = 'p14'), function(d) {
  
  pn = names(gen_pars[[d]]$p_pars)
  xlims = c(1, 10, 2)
  xlabs = c("\u03B3", "\u03B4", "\u03B8")
  
  # pop-lvl densities
  for(i in 1:3) {
    
    # the generating dist
    g = gen_pars[[d]]$p_pars[[i]]
    x = density(g)
    
    # recovered dists
    r = sapply(1:L, function(l) rec_pars[[d]][[l]]$p_pars[[i]])
    #
    y = lapply(1:L, function(l) density(r[,l]) )
    # total rec
    y_comb = density(r)
    
    #
    ymax = max(c(x$y, y_comb$y, sapply(y, function(x) max(x$y))) )
    
    plot(x, type = 'l',
         lty = 1,
         main = '',
         xlim = c(ifelse(i == 2, .1, 0), 
                  xlims[i]),
         ylim = c(0, ymax),
         log = ifelse(i == 2, 'x', ''),
         yaxt = 'n',
         ylab = '',
         xlab = '',
         lwd = 2,
         col = rgb(.1, .1, .8, .8))
    if(i == 1) title(main = d, las = 2, line = 1)
    title(xlab = xlabs[i], line = 2)
    
    # rec samp
    sapply(y, function(yy) lines(yy, lty = 1, col = rgb(.8, .1, .1, .1)) )
    # rec tot
    lines(y_comb, lwd = 2, col = rgb(.8, .1, .1, 1))
    lines(x, lwd = 2, col = rgb(.1, .1, .8, 1))

    if(i == 2 & d == 'p17') {
      
      legend(.1, .9 * ymax,
             title = 'Parameter',
             legend = c('Generating', 'Recovered'),
             lty = 1,
             col = c(rgb(.1, .1, .8, 1),
                     rgb(.8, .1, .1, 1)),
             bty = 'n')
      
    }
    
  }; 
})

dev.off()

# # id-lvl ------------------------------------------------------------------
# 
# cairo_pdf('analyses/figures_results/figs/FigA02_parrec.pdf.pdf',
#     height = 10 * 0.393701,
#     width = 16 * 0.393701,
#     pointsize = 10)
# 
# par(mfrow = c(3, 3),
#     mar = c(2, 4, 2, 1))
# 
# # pop-lvl figs
# lapply(c(p17 = 'p17', s16a = 's16a', s16b = 's16b'), function(d) {
#   
#   # get gen id-lvl means
#   gip = sapply(gen_pars[[d]]$i_pars, function(x) apply(x, 2, median))
#   
#   # recovered means 
#   gr_c = lapply(1:L, function(l) {
#     
#     # get rec id-lvl medians
#     rip = sapply(rec_pars[[d]][[l]]$i_pars, function(x) apply(x, 2, median))
#     
#   })
#   
#   #
#   ylims = c(1, 10, 2)
#   ylabs = c("\u03B3", "\u03B4", "\u03B8")
#   
#   # into parameter grids
#   id_rec = lapply(1:3, function(i) {
#     
#     rip = t( sapply(1:L, function(l) gr_c[[l]][,i] ) )
#     
#     # 
#     ip = gip[,i]
#     o_ip = order(ip)
#     
#     #
#     plot(rep(1:nrow(gip), each = L), rip[,o_ip],
#          col = rgb(.8, .1, .1, .1),
#          pch = 1,
#          ylim = c(ifelse(i == 2, .1, 0), ylims[i]),
#          log = ifelse(i == 2, 'y', ''),
#          xlab = '',
#          ylab = '',
#          main = d,
#          xaxt = 'n')
#     axis(1, at = 1:nrow(gip), tick = T, labels = F)
#     title(xlab = 'individual', line = 1)
#     title(ylab = ylabs[i], line = 2.5)
#     
#     # generating
#     points(1:nrow(gip), ip[o_ip], 
#            col = rgb(.1, .1, .8, .9),
#            pch = 19)
#     
#     # average rec
#     points(1:nrow(gip), apply(rip[,o_ip], 2, median),
#            col = rgb(.8, .1, .1, .9),
#            pch = 19)
#     
#   })
# 
# })
# 
# dev.off()