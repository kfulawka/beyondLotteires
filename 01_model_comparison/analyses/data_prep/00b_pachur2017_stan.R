# rm(list = ls())

source('analyses/data_prep/00_readExc.R')

# data --------------------------------------------------------------------
d <- read.all.sheets('data/PachurEtAl2017/Daten_PachurSuterHertwig2017.xlsx')

# lotteries ---------------------------------------------------------------
p <- matrix(as.matrix(d$`Probabilities & Outcome Ranks`[,c(2:5, 8,9)])
            , ncol = 6
            , dimnames = list(c(1:44), c('pA', 'p0A', 'pB', 'p0B', 'A', 'B') )
)

# monetary outcomes ---------------------------------------------------
out.A <- as.matrix(d$`Outcome A`[2:45,2:81])

out.B <- as.matrix(d$`Outcome B`[2:45,2:81])

# choices -----------------------------------------------------------------------
choices <- array(NA, dim = c(44, ncol(out.A), 2))
choices[,,1] <- as.matrix(d$`choices mon`[2:45,2:81])
choices[,,2] <- as.matrix(d$`choices med`[2:45,2:81])
choices[choices==2] <- 0

# affective outcomes ---------------------------------------------------
af <- array(NA, c(12, 80, 2))

af[,,1] <- -as.matrix(d$`affect eval mon`[2:13,2:81])
af[,,2] <- -as.matrix(d$`affect eval med`[2:13,2:81])

out.Aaf <- array(NA, c(44, 80, 2))
out.Baf <- array(NA, c(44, 80, 2))

# affective outomes
for (j in 1:80) {
  for (i in 1:44) {
    for (c in 1:2) {
      out.Aaf[i,j,c] = af[p[i,5], j, c]
      out.Baf[i,j,c] = af[p[i,6], j, c]
    }
  }
}

rm(d, i, j, af, c, read.all.sheets)

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
              nmon_wtp = list(xA = out.A, 
                              xB = out.B, 
                              aA = out.Aaf[,,2], 
                              aB = out.Baf[,,2],
                              p = p[,c('pA', 'pB')],
                              co = choices[,,2],
                              n = 80,
                              n_cp = 44,
                              max_a = 10,
                              s = -1),
              nmon_ar = list(xA = out.Aaf[,,2], 
                             xB = out.Baf[,,2], 
                             p = p[,c('pA', 'pB')], 
                             co = choices[,,2],
                             n = 80,
                             n_cp = 44,
                             max_a = 10,
                             s = -1))

pars_set = list(agdt = list(g_pars = c('mu_alp', 'mu_gam', 'mu_del', 'mu_theta',
                                       'sigma_alp','sigma_gam', 'sigma_del', 'sigma_theta'),
                            i_pars = c('alp', 'gam', 'del', 'theta'
                            )),
                
                gdt = list(g_pars = c('mu_gam', 'mu_del', 'mu_theta',
                                      'sigma_gam', 'sigma_del', 'sigma_theta'),
                           i_pars = c('gam', 'del', 'theta'
                           )))


d_pachur2017 <- list(
  
  cpt_mon_wtp = list(d = d_list$mon_wtp,
                     pp = pars_set$agdt,
                     mod = 'analyses/modeling/computational_models/stan/cpt.stan',
                     save_path = 'analyses/modeling/diagnostics/p17_cpt_mon_wtp'),
  
  cpt_mon_wtp_aw = list(d = d_list$mon_wtp,
                        pp = pars_set$agdt,
                        mod = 'analyses/modeling/computational_models/stan/cpt_v_aw.stan',
                        save_path = 'analyses/modeling/diagnostics/p17_cpt_mon_wtp_aw'),
  
  cpt_mon_ar = list(d = d_list$mon_ar, 
                    pp = pars_set$gdt,
                    mod = 'analyses/modeling/computational_models/stan/cpt_ar.stan',
                    save_path = 'analyses/modeling/diagnostics/p17_cpt_mon_ar'),
  
  cpt_mon_adw = list(d = d_list$mon_ar,
                     pp = pars_set$gdt,
                     mod = 'analyses/modeling/computational_models/stan/cpt_ar_aw.stan',
                     save_path = 'analyses/modeling/diagnostics/p17_cpt_mon_aw'),
  
  cpt_nmon_wtp = list(d = d_list$nmon_wtp,
                      pp = pars_set$agdt,
                      mod = 'analyses/modeling/computational_models/stan/cpt.stan',
                      save_path = 'analyses/modeling/diagnostics/p17_cpt_nmon_wtp'),
  
  cpt_nmon_wtp_aw = list(d = d_list$nmon_wtp,
                        pp = pars_set$agdt,
                        mod = 'analyses/modeling/computational_models/stan/cpt_v_aw.stan',
                        save_path = 'analyses/modeling/diagnostics/p17_cpt_nmon_wtp_aw'),
  
  cpt_nmon_ar = list(d = d_list$nmon_ar, 
                     pp = pars_set$gdt,
                     mod = 'analyses/modeling/computational_models/stan/cpt_ar.stan',
                     save_path = 'analyses/modeling/diagnostics/p17_cpt_nmon_ar'),

  cpt_nmon_adw = list(d = d_list$nmon_ar,
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/cpt_ar_aw.stan',
                      save_path = 'analyses/modeling/diagnostics/p17_cpt_nmon_aw'),
  
  #DFT
  dft_nmon_wtp = list(d = d_list$nmon_wtp,
                      pp = pars_set$agdt,
                      mod = 'analyses/modeling/computational_models/stan/dft.stan',
                      save_path = 'analyses/modeling/diagnostics/p17_dft_nmon_wtp'),
  
  dft_nmon_wtp_aw = list(d = d_list$nmon_wtp,
                         pp = pars_set$agdt,
                         mod = 'analyses/modeling/computational_models/stan/dft_v_aw.stan',
                         save_path = 'analyses/modeling/diagnostics/p17_dft_nmon_wtp_aw'),
  
  dft_nmon_ar = list(d = d_list$nmon_ar, 
                     pp = pars_set$gdt,
                     mod = 'analyses/modeling/computational_models/stan/dft_ar.stan',
                     save_path = 'analyses/modeling/diagnostics/p17_dft_nmon_ar'),
  
  dft_nmon_adw = list(d = d_list$nmon_ar,
                      pp = pars_set$gdt,
                      mod = 'analyses/modeling/computational_models/stan/dft_ar_aw.stan',
                      save_path = 'analyses/modeling/diagnostics/p17_dft_nmon_aw'))

rm(list = setdiff(ls(), 'd_pachur2017'))
save.image("analyses/modeling/00_p17_mod_dat_stan.RData")