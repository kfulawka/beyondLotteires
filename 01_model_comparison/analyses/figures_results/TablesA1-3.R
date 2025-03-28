rm(list = ls())

mods_pach17 = readRDS("analyses/modeling/posteriors/stan/p17.rds")
mods_pach14 = readRDS("analyses/modeling/posteriors/stan/p14.rds")
mods_sut16 = readRDS("analyses/modeling/posteriors/stan/s16.rds")

# results for tables
mon_n = grep('_mon_', names(mods_pach17), value = T)
nmon_n = grep('_nmon_', names(mods_pach17), value = T)

# function for plotting ---------------------------------------------------

source('analyses/figures_results/99_table_gl_est.R')

# pachur 2017 data --------------------------------------------------------

# list with results
res = lapply(mods_pach17[c(nmon_n, mon_n)], function(x) {
  
  gp = x$pars$p_pars
  
  gp = gp[1:(length(gp)/2)]
  
  gp = do.call(data.frame, gp)
  
  return(gp)
  
  })

table_gl_est(res,
             a = 6,
             'tables/tab1_pa17.txt')

# pachur 2014 data --------------------------------------------------------

# list with results
res = lapply(mods_pach14[c(nmon_n, mon_n)], function(x) {
  
  gp = x$pars$p_pars
  
  gp = gp[1:(length(gp)/2)]
  
  gp = do.call(data.frame, gp)
  
  return(gp)
  
})

table_gl_est(res,
             a = 6,
             'tables/tab1_pa14.txt')

# suter 2016 data --------------------------------------------------------

# updated names
nmon_n = grep('_nmon_', names(mods_sut16), value = T)

# list with results
res = lapply(mods_sut16[c(nmon_n, mon_n)], function(x) {
  
  gp = x$pars$p_pars
  
  gp = gp[1:(length(gp)/2)]
  
  gp = do.call(data.frame, gp)
  
  return(gp)
  
})

table_gl_est(res,
             a = 6,
             'tables/tab1_s16.txt')