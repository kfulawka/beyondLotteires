rm(list = ls())

# compare models using loo
library(loo)
library(ggplot2)
library(brms)

# data --------------------------------------------------------------------

# modeling results
pt = readRDS('analyses/02b_choice_modeling/mods_gam_phi.rds')
pt_pars = pt$cpt_vcov_gt1$pars
ipt = sapply(pt_pars$i_pars[c('gam_ap', 'gam_ar')], function(x) apply(x, 2, median))

# dwell to p
m_cond_att = readRDS('results_figs/m_cond_sgi_att.rds')
dd = conditional_effects(m_cond_att,
                         re_formula = NULL,
                         conditions = data.frame(sub = rep(1:68, each = 2),
                                                 cond = c('poor', 'rich')))[[1]]
dd = dd[, c('sub', 'cond', 'estimate__', 'lower__', 'upper__')]
dd = unique(dd) # brms bug fix...
colnames(dd)[3:5] = c('y', 'l', 'u')

# combine data
dd$gam = NA
dd$gam[dd$cond == 'poor'] = ipt[,1]
dd$gam[dd$cond == 'rich'] = ipt[,2]


# correlations ------------------------------------------------------------

cor.test(dd$gam[dd$cond == 'poor'], dd$y[dd$cond=='poor'], method = 'p')
cor.test(dd$gam[dd$cond == 'rich'], dd$y[dd$cond=='rich'], method = 'p')

# brms modeling -----------------------------------------------------------

# 
m_cors = lapply(c(poor = 'poor', rich = 'rich'), function(x) {
  
  dp = dd[dd$cond == x, ]
  dp$y_z = scale(dp$y);
  dp$gam_z = scale(dp$gam)
  
  m = brm(y_z ~ 1 + gam_z,
          data = dp,
          cores = 4,
          chains = 4,
          family = student(),
          iter = 2000,
          warmup = 1000,
          thin = 2)
  
  
})
saveRDS(m_cors, 'results_figs/gam_atp_Sz.rds')

# # full dataset ------------------------------------------------------------
# 
# df = read.csv('analyses/00c_data_analyses/df.csv')
# 
# #
# dwells = grep('dwell', colnames(df), value = T)
# 
# # dependent var
# df$dwell_P = (df$dwell_pa + df$dwell_qa + df$dwell_pb + df$dwell_qb) / rowSums(df[,dwells])
# df$dwell_P_z = scale(df$dwell_P)
# 
# # ind var
# af = grep('af', colnames(df), value = T)
# 
# df$ar_max = apply(df[,af[5:6]], 1, max) 
# 
# # merge with ind ps
# ipt_l = data.frame(sub = 1:68, 
#                    cond = rep(c('poor', 'rich'), each = 68),
#                    gam = c(ipt[,1], ipt[,2]))
# 
# df = merge(df, ipt_l, by = c('sub', 'cond'))
# 
# df$gam_z = scale(df$gam)
# 
# # control predictors
# df$ar_max_b = cut(df$ar_max, 
#                   breaks = seq(0, 100, 4),
#                   ordered_result = T)
# df$ar_max_b = as.numeric(df$ar_max_b) * 4
# df$ar_max_z = scale(df$ar_max_b)
# 
# df$pA_z = scale(df$pA)
# 
# m_at = brm(bf(dwell_P ~ gam_z + ar_max_z + pA_z + (ar_max_z||sub),
#                 # coi ~ cond + gam_z,
#                 # zoi ~ cond + gam_z,
#                 # phi ~ cond + (cond||sub)
#                 sigma ~ 1 + (1|sub) 
#                 ),
#              data = df,
#              # family = zero_one_inflated_beta(),
#              family = student(),
#              cores = 4,
#              chains = 4,
#              iter = 4e3,
#              warmup = 2e3,
#              thin = 4)
# m_at
# 
# # plot(m_at)
# # pp_check(m_cond)
# 
# saveRDS(m_at, 'results_figs/m_ar_p_gam_att.rds')