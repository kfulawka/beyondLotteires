rm(list = ls())

library(data.table)

# data --------------------------------------------------------------------

dd = read.csv('analyses/00c_data_analyses/df_swapped.csv')

# aggregate ---------------------------------------------------------------

aa = aggregate(choice ~ cond + sub,
               FUN = mean, 
               data = dd)

co_m = cbind(co_poor = aa$choice[aa$cond == 'poor'],
             co_rich = aa$choice[aa$cond == 'rich'])

saveRDS(co_m, 'results_figs/co_m.rds')

aa = aggregate(choice ~ cond + CPNum,
               FUN = mean, 
               data = dd)

co_m = cbind(co_poor = aa$choice[aa$cond == 'poor'],
             co_rich = aa$choice[aa$cond == 'rich'])

saveRDS(co_m, 'results_figs/co_cp.rds')

# choice prob modeling ----------------------------------------------------

dd$cond = factor(dd$cond,
                 levels = c('poor', 'rich'),
                 ordered = T)

contrasts(dd$cond) = contr.sum(2)

mod = brm(choice ~ cond + (cond || sub) + (cond || CPNum),
          data = dd,
          family = bernoulli(),
          cores = 4,
          chains = 4,
          warmup = 2e3,
          iter = 4e3,
          thin = 2)

saveRDS(mod, 'results_figs/co_cp_mod.rds')

# preference reversals ----------------------------------------------------

df = dd[,c('sub', 'cond', 'CPNum', 'choice', 'xAaf_m', 'xBaf_m')]

df = dcast(data.table(df), sub + CPNum ~ cond,
           value.var = c('choice', 'xAaf_m', 'xBaf_m'))
df = data.frame(df)

# preference reversals
df$risk_safe = df$choice_poor == 1 & df$choice_rich == 0
df$safe_risk = df$choice_poor == 0 & df$choice_rich == 1
df$no_rev = df$choice_poor == df$choice_rich

N = max(df$sub)

# withing sub reversals
revs_i = aggregate(cbind(risk_safe, safe_risk, no_rev) ~ sub,
                   data = df,
                   FUN = mean)

saveRDS(revs_i, 'results_figs/revs_i.rds')

# brms modeling 

library(brms)

df$xD_rich = df$xAaf_m_rich - df$xBaf_m_rich
df$xD_poor = df$xAaf_m_poor - df$xBaf_m_poor

#
df$xD_rich_z = scale(df$xD_rich)
df$xAaf_m_rich_z = scale(df$xAaf_m_rich)

mod = brm(risk_safe ~ xD_rich_z + xAaf_m_rich_z + (xD_rich_z + xAaf_m_rich_z || sub),
          data = df,
          family = bernoulli(),
          cores = 4,
          chains = 4,
          warmup = 2e3,
          iter = 4e3,
          thin = 2)
mod
ci = coef(mod)$sub[,1,]

conditional_effects(mod)