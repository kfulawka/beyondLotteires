# rm(list = ls())

# data --------------------------------------------------------------------

# choices
choices <- array(c(as.matrix(read.table('data/PachurEtAl2014/choices_money.txt')),
                   as.matrix(read.table('data/PachurEtAl2014/choices_affect.txt'))),
                 dim = c(13, 80, 2))

# probabilities
probs <- as.matrix(read.table('data/PachurEtAl2014/probabilities.txt'))

# affect ratings
affect_ratings <- array(c(as.matrix(read.table('data/PachurEtAl2014/affective_evaluations_mon.txt')),
                          as.matrix(read.table('data/PachurEtAl2014/affective_evaluations_aff.txt'))),
                        dim = c(15, 80, 2))

# WTP values
wtp <- as.matrix(read.table('data/PachurEtAl2014/monetary_evaluations.txt'))

# events
events <- as.matrix(read.table('data/PachurEtAl2014/events.txt'))

# mapping and re-coding ---------------------------------------------------

# choices into 0-1
choices <- ifelse(choices == 2, 0, 1)

# monetary values
out.A <- matrix(NA, nrow = 13, ncol = 80)
out.B <- matrix(NA, nrow = 13, ncol = 80)

# 
for(i in 1:80) {
  
  for(j in 1:13) {
    
    out.A[j,i] <- wtp[ events[j,1], i]
    out.B[j,i] <- wtp[ events[j,2], i]
    
  }
  
}

# affect ratings
out.Aaf <- array(NA, dim = c(13, 80, 2))
out.Baf <- array(NA, dim = c(13, 80, 2))

# 
for(i in 1:80) {
  
  for(j in 1:13) {
    
    # affect ratings of money
    out.Aaf[j,i,1] <- affect_ratings[ events[j,1], i, 1]
    out.Baf[j,i,1] <- affect_ratings[ events[j,2], i, 1]
    
    # affect ratings of non-money
    out.Aaf[j,i,2] <- affect_ratings[ events[j,1], i, 2]
    out.Baf[j,i,2] <- affect_ratings[ events[j,2], i, 2]
    
  }
  
}

# lists with datasets for modeling ----------------------------------------

d_list = list(mon_wtp = list(xA = out.A, 
                             xB = out.B, 
                             aA = out.Aaf[,,1], 
                             aB = out.Baf[,,1],
                             p = probs,
                             co = choices[,,1],
                             n = 80,
                             n_cp = 13,
                             max_a = 10,
                             s = 1),
              mon_ar = list(xA = out.Aaf[,,1], 
                            xB = out.Baf[,,1], 
                            p = probs, 
                            co = choices[,,1],
                            n = 80,
                            n_cp = 13,
                            max_a = 10,
                            s = 1),
              
              nmon_wtp = list(xA = out.A, 
                              xB = out.B,
                              aA = out.Aaf[,,2], 
                              aB = out.Baf[,,2],
                              p = probs,
                              co = choices[,,2],
                              n = 80,
                              n_cp = 13,
                              max_a = 10,
                              s = 1),
              nmon_ar = list(xA = out.Aaf[,,2], 
                             xB = out.Baf[,,2], 
                             p = probs, 
                             co = choices[,,2],
                             n = 80,
                             n_cp = 13,
                             max_a = 10,
                             s = 1))

pars_set = list(agdt = list(g_pars = c('mu_alp', 'mu_gam', 'mu_del', 'mu_theta',
                                       'sigma_alp','sigma_gam', 'sigma_del', 'sigma_theta'),
                            i_pars = c('alp', 'gam', 'del', 'theta'
                            )),
                
                gdt = list(g_pars = c('mu_gam', 'mu_del', 'mu_theta',
                                      'sigma_gam', 'sigma_del', 'sigma_theta'),
                           i_pars = c( 'gam', 'del', 'theta')))

d_pachur2014 <- list(
  
  cpt_mon_wtp = list(d = d_list$mon_wtp,
                     pp = pars_set$agdt,
                     mod = 'analyses/modeling/computational_models/stan/cpt.stan',
                     save_path = 'analyses/modeling/diagnostics/p14_cpt_mon_wtp'),
  
  cpt_mon_wtp_aw = list(d = d_list$mon_wtp,
                        pp = pars_set$agdt,
                        mod = 'analyses/modeling/computational_models/stan/cpt_v_aw.stan',
                        save_path = 'analyses/modeling/diagnostics/p14_cpt_mon_wtp_aw'),
  
  cpt_mon_ar = list(d = d_list$mon_ar, 
                    pp = pars_set$gdt,
                    mod = 'analyses/modeling/computational_models/stan/cpt_ar.stan',
                    save_path = 'analyses/modeling/diagnostics/p14_cpt_mon_ar'),
  
  cpt_mon_adw = list(d = d_list$mon_ar,
                     pp = pars_set$gdt,
                     mod = 'analyses/modeling/computational_models/stan/cpt_ar_aw.stan',
                     save_path = 'analyses/modeling/diagnostics/p14_cpt_mon_aw'),
  
  cpt_nmon_wtp = list(d = d_list$nmon_wtp,
                      pp = pars_set$agdt,
                      mod = 'analyses/modeling/computational_models/stan/cpt.stan',
                      save_path = 'analyses/modeling/diagnostics/p14_cpt_nmon_wtp'),
  
  cpt_nmon_wtp_aw = list(d = d_list$nmon_wtp,
                         pp = pars_set$agdt,
                         mod = 'analyses/modeling/computational_models/stan/cpt_v_aw.stan',
                         save_path = 'analyses/modeling/diagnostics/p14_cpt_mon_wtp_aw'),
  
  cpt_nmon_ar = list(d = d_list$nmon_ar, 
                     pp = pars_set$gdt,
                     mod = 'analyses/modeling/computational_models/stan/cpt_ar.stan',
                     save_path = 'analyses/modeling/diagnostics/p14_cpt_nmon_ar'),
  
  cpt_nmon_adw = list(d = d_list$nmon_ar,
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/cpt_ar_aw.stan',
                      save_path = 'analyses/modeling/diagnostics/p14_cpt_nmon_aw'),
  
  # DFT
  dft_nmon_wtp = list(d = d_list$nmon_wtp,
                      pp = pars_set$agdt,
                      mod = 'analyses/modeling/computational_models/stan/dft.stan',
                      save_path = 'analyses/modeling/diagnostics/p14_dft_nmon_wtp'),
  
  dft_nmon_wtp_aw = list(d = d_list$nmon_wtp,
                         pp = pars_set$agdt,
                         mod = 'analyses/modeling/computational_models/stan/dft_v_aw.stan',
                         save_path = 'analyses/modeling/diagnostics/p14_dft_mon_wtp_aw'),
  
  dft_nmon_ar = list(d = d_list$nmon_ar, 
                     pp = pars_set$gdt,
                     mod = 'analyses/modeling/computational_models/stan/dft_ar.stan',
                     save_path = 'analyses/modeling/diagnostics/p14_dft_nmon_ar'),
  
  dft_nmon_adw = list(d = d_list$nmon_ar,
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/dft_ar_aw.stan',
                      save_path = 'analyses/modeling/diagnostics/p14_dft_nmon_aw')
  )

rm(list = setdiff(ls(), 'd_pachur2014'))
save.image("analyses/modeling/00_p14_mod_dat_stan.RData")