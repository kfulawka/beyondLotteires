
rm(list = ls())

library(brms)

# data --------------------------------------------------------------------

dd = read.csv('analyses/00c_data_analyses/df.csv')

#
dwells = grep('dwell', colnames(dd), value = T)

# dependent var
dd$dwell_P = (dd$dwell_pa + dd$dwell_qa + dd$dwell_pb + dd$dwell_qb) / rowSums(dd[,dwells])
# dd$dwell_P_z = scale(dd$dwell_P)

# ind var
af = grep('af', colnames(dd), value = T)

dd$ar_max = apply(dd[,af[5:6]], 1, max) 

dd$ar_max_b = cut(dd$ar_max, 
                  breaks = seq(0, 100, 4),
                  ordered_result = T)
dd$ar_max_b = as.numeric(dd$ar_max_b) * 4

dd$ar_max_z = scale(dd$ar_max_b)

dd$pA_z = scale(dd$pA)

# attention by condition --------------------------------------------------

dd$cond = factor(dd$cond, ordered = T)
contrasts(dd$cond) = contr.sum(2)/2

m_cond = brm(bf(dwell_P ~ cond + (cond||sub),
                coi ~ cond,
                zoi ~ cond,
                phi ~ cond + (cond||sub) ),
             data = dd,
             family = zero_one_inflated_beta(),
             cores = 4,
             chains = 4,
             iter = 4e3,
             warmup = 2e3,
             thin = 4)
m_cond
plot(m_cond)

pp_check(m_cond)

saveRDS(m_cond, 'results_figs/m_cond_sgi_att.rds')

# attention by ar ---------------------------------------------------------

dd$ar_max_z = scale(dd$ar_max)

m_ar_p = brm(bf(dwell_P ~ ar_max_z + pA_z + (ar_max_z||sub),
              coi ~ ar_max_z,
              zoi ~ ar_max_z,
              phi ~ 1 + (1|sub)
              # sigma ~ 1 + (1|sub)
              ),
           data = dd,
           family = zero_one_inflated_beta(),
           # family = student(),
           cores = 4,
           chains = 4,
           iter = 4e3,
           warmup = 2e3,
           thin = 4)

m_ar_p
bayes_R2(m_ar_p)
bayes_R2(m_ar_p, re_form = NA)
# plot(m_ar_p)

saveRDS(m_ar_p, 'results_figs/m_ar_p_att.rds')

# att by ar agr -----------------------------------------------------------

aa = aggregate(dwell_P ~ ar_max_b, 
               data = dd,
               FUN = function(x) c(m = mean(x), 
                                   n = length(x), 
                                   sd = sd(x)))

aa = do.call(data.frame, aa)
aa$ui = aa$dwell_P.m + aa$dwell_P.sd / sqrt(aa$dwell_P.n)
aa$li = aa$dwell_P.m - aa$dwell_P.sd / sqrt(aa$dwell_P.n)

# brms model
ma_arAt = brm(dwell_P.m ~ ar_max_b,
              data = aa,
              family = student(),
              cores = 4,
              chains = 4,
              iter = 2e3,
              thin = 2)

saveRDS(ma_arAt, 'results_figs/ma_arAt.rds')