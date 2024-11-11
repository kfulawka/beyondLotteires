rm(list = ls())

# data --------------------------------------------------------------------

dd = read.csv('analyses/00c_data_analyses/df.csv')

dd$xAB = apply(dd[,c('xAaf_m', 'xBaf_m')], 1, mean)
dd$xAB_max = apply(dd[,c('xAaf_m', 'xBaf_m')], 1, max)

# aggregate ---------------------------------------------------------------

aa = aggregate(xAB ~ cond + sub,
               FUN = mean, 
               data = dd)

ar_m = cbind(ar_poor = aa$xAB[aa$cond == 'poor'],
             ar_rich = aa$xAB[aa$cond == 'rich'])

saveRDS(ar_m, 'results_figs/ar_m.rds')

aa = aggregate(xAB ~ cond + sub,
               FUN = max, 
               data = dd)

ar_m2 = cbind(ar_poor = aa$xAB[aa$cond == 'poor'],
             ar_rich = aa$xAB[aa$cond == 'rich'])

saveRDS(ar_m2, 'results_figs/ar_max.rds')