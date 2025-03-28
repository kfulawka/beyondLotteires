#rm(list = ls())

source('analyses/data_prep/00_readExc.R')

# data --------------------------------------------------------------------

d = read.all.sheets('data/SuterEtAl2016/SuterPachurHertwig2016.xlsx')

# affect ratings are missing for subjects 26 & 27
# I excluded them from the reanalyses

# -------------------------------------------------------------------------

# probabilities
p = matrix(as.matrix(d$`Probabilities & Outcome Ranks`[,c(2:5, 8,9)])
            , ncol = 6
            , dimnames = list(c(1:44), c('pA', 'p0A', 'pB', 'p0B', 'A', 'B') )
)

#---------------------------------------------------

# outcomes MONETARY
out.A = as.matrix(d$`Outcome A`[ ,c(2:25, 28:83)]) 

out.B = as.matrix(d$`Outcome B`[ ,c(2:25, 28:83)])

# # get min values per part
# mo = apply(rbind(out.A, out.B), 2, min)
# 
# out.A = sapply(1:length(mo), function(i) -10 * out.A[,i] / mo[i])
# out.B = sapply(1:length(mo), function(i) -10 * out.B[,i] / mo[i])

#---------------------------------------------------

# outcomes affect
af = array(NA, c(12, 80, 2))
af[,,1] = -as.matrix(d$`affect eval mon`[,c(2:25, 28:83)])
af[,,2] = -as.matrix(d$`affect eval med`[,c(2:25, 28:83)])

# choicesAf = choices
out.Aaf = array(NA, c(44, 80, 2))
out.Baf = array(NA, c(44, 80, 2))

# affective outomes
for (j in 1:80) {
  for (i in 1:44) {
    for (c in 1:2) {
      
      out.Aaf[i,j,c] = af[p[i,5], j, c]
      out.Baf[i,j,c] = af[p[i,6], j, c]
      
    }
  }
}

# -------------------------------------------------------------------------

# choices
choices = array(NA, dim = c(44, ncol(out.A), 2))
choices[,,1] = as.matrix(d$`choices mon`[, c(2:25, 28:83)])
choices[,,2] = as.matrix(d$`choices med`[1:44, c(2:25, 28:83)])

# exp cond
cond = unlist(d$`choices med`[46, c(2:25, 28:83)])

# lists with datasets for modeling ----------------------------------------

d_list = list(mon_wtp = list(xA = out.A, 
                             xB = out.B, 
                             aA = out.Aaf[,,1], 
                             aB = out.Baf[,,1],
                             p = p[,c('pA', 'pB')],
                             co = choices[,,1],
                             n = 80,
                             n_cp = 44,
                             max_a = 10,
                             s = -1),
              
              mon_ar = list(xA = out.Aaf[,,1], 
                            xB = out.Baf[,,1], 
                            p = p[,c('pA', 'pB')], 
                            co = choices[,,1],
                            n = 80,
                            n_cp = 44,
                            max_a = 10,
                            s = -1),
              
              nmon_wtp1 = list(xA = out.A[,cond==1], 
                              xB = out.B[,cond==1],
                              aA = out.Aaf[,cond==1,2], 
                              aB = out.Baf[,cond==1,2],
                              p = p[,c('pA', 'pB')],
                              co = choices[,cond==1,2],
                              n = 40,
                              n_cp = 44,
                              max_a = 10,
                              s = -1),
              
              nmon_ar1 = list(xA = out.Aaf[,cond==1,2], 
                             xB = out.Baf[,cond==1,2], 
                             p = p[,c('pA', 'pB')], 
                             co = choices[,cond==1,2],
                             n = 40,
                             n_cp = 44,
                             max_a = 10,
                             s = -1),
              
              nmon_wtp2 = list(xA = out.A[,cond==2], 
                               xB = out.B[,cond==2],
                               aA = out.Aaf[,cond==2,2], 
                               aB = out.Baf[,cond==2,2],
                               p = p[,c('pA', 'pB')],
                               co = choices[,cond==2,2],
                               n = 40,
                               n_cp = 44,
                               max_a = 10,
                               s = -1),
              
              nmon_ar2 = list(xA = out.Aaf[,cond==2,2], 
                              xB = out.Baf[,cond==2,2], 
                              p = p[,c('pA', 'pB')], 
                              co = choices[,cond==2,2],
                              n = 40,
                              n_cp = 44,
                              max_a = 10,
                              s = -1))

pars_set = list(agdt = list(g_pars = c('mu_alp', 'mu_gam', 'mu_del', 'mu_theta',
                                       'sigma_alp','sigma_gam', 'sigma_del', 'sigma_theta'),
                            i_pars = c('alp', 'gam', 'del', 'theta')),
                
                gdt = list(g_pars = c('mu_gam', 'mu_del', 'mu_theta',
                                      'sigma_gam', 'sigma_del', 'sigma_theta'),
                           i_pars = c( 'gam', 'del', 'theta')))

d_s16 = list(
  
  cpt_mon_wtp = list(d = d_list$mon_wtp,
                     pp = pars_set$agdt,
                     mod = 'analyses/modeling/computational_models/stan/cpt.stan',
                     save_path = 'analyses/modeling/diagnostics/s16_cpt_mon_wtp'),
  
  cpt_mon_wtp_aw = list(d = d_list$mon_wtp,
                        pp = pars_set$agdt,
                        mod = 'analyses/modeling/computational_models/stan/cpt_v_aw.stan',
                        save_path = 'analyses/modeling/diagnostics/s16_cpt_mon_wtp_aw'),
  
  cpt_mon_ar = list(d = d_list$mon_ar, 
                    pp = pars_set$gdt,
                    mod = 'analyses/modeling/computational_models/stan/cpt_ar.stan',
                    save_path = 'analyses/modeling/diagnostics/s16_cpt_mon_ar'),
  
  cpt_mon_aw = list(d = d_list$mon_ar,
                    pp = pars_set$gdt,
                    mod = 'analyses/modeling/computational_models/stan/cpt_ar_aw.stan',
                    save_path = 'analyses/modeling/diagnostics/s16_cpt_mon_aw'),
  
  cpt_nmon_wtp1 = list(d = d_list$nmon_wtp1,
                       pp = pars_set$agdt,
                       mod = 'analyses/modeling/computational_models/stan/cpt.stan',
                       save_path = 'analyses/modeling/diagnostics/s16_cpt_nmon_wtp1'),
  
  cpt_nmon_wtp_aw1 = list(d = d_list$nmon_wtp1,
                       pp = pars_set$agdt,
                       mod = 'analyses/modeling/computational_models/stan/cpt_v_aw.stan',
                       save_path = 'analyses/modeling/diagnostics/s16_cpt_nmon_wtp_aw1'),
  
  cpt_nmon_ar1 = list(d = d_list$nmon_ar1, 
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/cpt_ar.stan',
                      save_path = 'analyses/modeling/diagnostics/s16_cpt_nmon_ar1'),
  
  cpt_nmon_aw1 = list(d = d_list$nmon_ar1,
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/cpt_ar_aw.stan',
                      save_path = 'analyses/modeling/diagnostics/s16_cpt_nmon_aw1'),
  
  cpt_nmon_wtp2 = list(d = d_list$nmon_wtp2,
                       pp = pars_set$agdt,
                       mod = 'analyses/modeling/computational_models/stan/cpt.stan',
                       save_path = 'analyses/modeling/diagnostics/s16_cpt_nmon_wtp2'),
  
  cpt_nmon_wtp_aw2 = list(d = d_list$nmon_wtp2,
                          pp = pars_set$agdt,
                          mod = 'analyses/modeling/computational_models/stan/cpt_v_aw.stan',
                          save_path = 'analyses/modeling/diagnostics/s16_cpt_nmon_wtp_aw2'),
  
  cpt_nmon_ar2 = list(d = d_list$nmon_ar2, 
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/cpt_ar.stan',
                      save_path = 'analyses/modeling/diagnostics/s16_cpt_nmon_ar2'),
  
  cpt_nmon_aw2 = list(d = d_list$nmon_ar2,
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/cpt_ar_aw.stan',
                      save_path = 'analyses/modeling/diagnostics/s16_cpt_nmon_aw2'),
  
  # DFT!!
  dft_nmon_wtp1 = list(d = d_list$nmon_wtp1,
                       pp = pars_set$agdt,
                       mod = 'analyses/modeling/computational_models/stan/dft.stan',
                       save_path = 'analyses/modeling/diagnostics/s16_dft_nmon_wtp1'),
  
  dft_nmon_wtp_aw1 = list(d = d_list$nmon_wtp1,
                          pp = pars_set$agdt,
                          mod = 'analyses/modeling/computational_models/stan/dft_v_aw.stan',
                          save_path = 'analyses/modeling/diagnostics/s16_dft_nmon_wtp_aw1'),
  
  dft_nmon_ar1 = list(d = d_list$nmon_ar1, 
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/dft_ar.stan',
                      save_path = 'analyses/modeling/diagnostics/s16_dft_nmon_ar1'),
  
  dft_nmon_aw1 = list(d = d_list$nmon_ar1,
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/dft_ar_aw.stan',
                      save_path = 'analyses/modeling/diagnostics/s16_dft_nmon_aw1'),
  
  dft_nmon_wtp2 = list(d = d_list$nmon_wtp2,
                       pp = pars_set$agdt,
                       mod = 'analyses/modeling/computational_models/stan/dft.stan',
                       save_path = 'analyses/modeling/diagnostics/s16_dft_nmon_wtp2'),
  
  dft_nmon_wtp_aw2 = list(d = d_list$nmon_wtp2,
                          pp = pars_set$agdt,
                          mod = 'analyses/modeling/computational_models/stan/dft_v_aw.stan',
                          save_path = 'analyses/modeling/diagnostics/s16_dft_nmon_wtp_aw2'),
  
  dft_nmon_ar2 = list(d = d_list$nmon_ar2, 
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/dft_ar.stan',
                      save_path = 'analyses/modeling/diagnostics/s16_dft_nmon_ar2'),
  
  dft_nmon_aw2 = list(d = d_list$nmon_ar2,
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/dft_ar_aw.stan',
                      save_path = 'analyses/modeling/diagnostics/s16_dft_nmon_aw2')
)

rm(list = setdiff(ls(), c('d_s16')))
save.image("analyses/modeling/00_s16_mod_dat_stan.RData")