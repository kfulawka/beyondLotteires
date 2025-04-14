#
rm(list = ls())

# choice, ar, and et-indices data combined
df = read.csv('data/df_ct_fix.txt')

# outliers (based on attention check problems and ARs)
source('analyses/00a_choice_dat_prep/00_outliers.R')

df = df[!df$sub %in% outliers, ] # remove outliers
df = df[df$CPNum < 60, ] # remove attention check problems

# sub numbs
df$sub = as.numeric(as.factor(df$sub))

# order
df = df[order(df$sub, df$cond, df$CPNum), ]

# remove '.' symbol from colnames
colnames(df) = gsub('\\.', '_', colnames(df))

# save for statistical analyses
write.table(df, 
            'analyses/00c_data_analyses/df.csv', 
            row.names = F, 
            sep = ',')

# swapped version ---------------------------------------------------------

# in this dataset, A corresponds to the risky drug
# handy for fast calculation of some of the results

# col names for swapping
cn_xpa = grep('xa|pa|qa|withA', colnames(df), ignore.case = T, value = T)
cn_xpb = grep('xb|pb|qb|withB', colnames(df), ignore.case = T, value = T)

# 
df_swapped = df

# function for swapping fin_fix
fin_fix_swap= function(x) {
  
  fin_fix = c('xa', 'pa', 'ya', 'qa', 'xb', 'pb', 'yb', 'qb')
  
  if(x != '') {
    
    ii = which(fin_fix == x)
    
    y = ifelse(ii < 5, fin_fix[ii + 4], fin_fix[ii - 4])
    
    return(y)
  } else { return('') }
  
}

for(i in 1:nrow(df_swapped)) {
  
  if(df_swapped$swap[i]) {
    
    # swap A-B values
    df_swapped[i,cn_xpa] = df[i,cn_xpb]
    df_swapped[i,cn_xpb] = df[i,cn_xpa]
    
    # swap choices
    df_swapped$choice[i] = ifelse(df$choice[i], 0, 1)
    
    # swap last fixation -- option
    df_swapped$fin_fix_o[i] = ifelse(df$fin_fix_o[i] == 'A', 'B', 'A')
    
    # swap last fixation -- info piece
    df_swapped$fin_fix[i] = fin_fix_swap(df$fin_fix[i])
    
  }
  
}

# save for statistical analyses
write.table(df_swapped, 
            'analyses/00c_data_analyses/df_swapped.csv', 
            row.names = F, 
            sep = ',')


# stan data ---------------------------------------------------------------

# we'll keep it in long format
df_p = df[df$cond == 'poor', ]
df_r = df[df$cond == 'rich', ]

stan_dat = list(sub = df_p$sub,
                xa = cbind(df_p$xAaf_m, df_r$xAaf_m) / 10,
                xb = cbind(df_p$xBaf_m, df_r$xBaf_m) / 10,
                xa1 = cbind(df_p$xAaf1, df_r$xAaf1) / 10,
                xb1 = cbind(df_p$xBaf1, df_r$xBaf1) / 10,
                xa2 = cbind(df_p$xAaf2, df_r$xAaf2) / 10,
                xb2 = cbind(df_p$xBaf2, df_r$xBaf2) / 10,
                pa = cbind(df_p$pA, df_r$pA),
                pb = cbind(df_p$pB, df_r$pB),
                co = cbind(df_p$choice, df_r$choice),
                n = nrow(df_p),
                N = max(df_p$sub))

saveRDS(stan_dat, 'analyses/00c_data_analyses/stan_data.rds')

#
ll_pars = c('log_lik_ap', 'log_lik_ar')

stan_mods = list(
  cpt_gt = list(m_name = 'cpt_gt',
                p_pars = c('mu_gam', 'sig_gam',
                           'mu_theta_ap', 'mu_theta_ar', 
                           'sig_theta_ap', 'sig_theta_ar'),
                i_pars = c('gam', 'theta_ap', 'theta_ar')),
  
  
  cpt_gaw_t = list(m_name = 'cpt_gaw_t',
                   p_pars = c('mu_gam', 'sig_gam',
                              'mu_theta_ap', 'mu_theta_ar', 
                              'sig_theta_ap', 'sig_theta_ar'),
                   i_pars = c('gam', 'theta_ap', 'theta_ar')),
  
  cpt_vcov_gt = list(m_name = 'cpt_vcov_gt_sep',
                     p_pars = c('mu_gam_ap', 'mu_gam_ar', 
                                'mu_theta_ap', 'mu_theta_ar',
                                'sig_gam_ap', 'sig_gam_ar',
                                'sig_theta_ap', 'sig_theta_ar',
                                'gam_r', 'theta_r'),
                     i_pars = c('gam_ap', 'gam_ar', 'theta_ap', 'theta_ar')),
  
  cpt_dgt = list(m_name = 'cpt_dgt',
                 p_pars = c('mu_del','mu_gam', 
                            'mu_theta_ap', 'mu_theta_ar',
                            'sig_del', 'sig_gam', 
                            'sig_theta_ap', 'sig_theta_ar'),
                 i_pars = c('del', 'gam', 'theta_ap', 'theta_ar')),
  
  cpt_dgaw_t = list(m_name = 'cpt_dgaw_t',
                    p_pars = c('mu_del','mu_gam', 
                               'mu_theta_ap', 'mu_theta_ar',
                               'sig_del', 'sig_gam', 
                               'sig_theta_ap', 'sig_theta_ar'),
                    i_pars = c('del', 'gam', 'theta_ap', 'theta_ar')),
  
  cpt_vcov_dgt = list(m_name = 'cpt_vcov_dgt_sep',
                      p_pars = c('mu_del_ap', 'mu_del_ar', 
                                 'mu_gam_ap', 'mu_gam_ar', 
                                 'mu_theta_ap', 'mu_theta_ar',
                                 'sig_del_ap', 'sig_del_ar',
                                 'sig_gam_ap', 'sig_gam_ar',
                                 'sig_theta_ap', 'sig_theta_ar',
                                 'del_r', 'gam_r', 'theta_r'),
                      i_pars = c('del_ap', 'del_ar', 'gam_ap', 
                                 'gam_ar', 'theta_ap', 'theta_ar'))
)

saveRDS(stan_mods, 'analyses/00c_data_analyses/stan_mods.rds')

stan_modsT1 = list(
  
  cpt_gt1 = list(m_name = 'cpt_gt1',
                 p_pars = c('mu_gam', 'sig_gam',
                            'mu_theta', 'sig_theta'),
                 i_pars = c('gam', 'theta')),
  
  cpt_gaw_t1 = list(m_name = 'cpt_gaw_t1',
                    p_pars = c('mu_gam', 'sig_gam',
                               'mu_theta', 'sig_theta'),
                    i_pars = c('gam', 'theta')),
  
  cpt_vcov_gt1 = list(m_name = 'cpt_vcov_gt1',
                      p_pars = c('mu_gam_ap', 'mu_gam_ar', 
                                 'mu_theta',
                                 'sig_gam_ap', 'sig_gam_ar',
                                 'sig_theta',
                                 'gam_r'),
                      i_pars = c('gam_ap', 'gam_ar', 'theta'))
)

saveRDS(stan_modsT1, 'analyses/00c_data_analyses/stan_modsT1.rds')