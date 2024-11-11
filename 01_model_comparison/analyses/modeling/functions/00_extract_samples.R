# function to extract logPP and parameters
separate_pars_from_logl <- function(M, 
                                    group_p = c('mu_par', 'sigma_par'),
                                    id_p = 'par',
                                    ll_c = T) {
  
  # matrix of samples from posteriors
  post_samples <- as.matrix(M$mcmc, chains = T)
  
  # vector with names of group-lvl paramteres
  group_pars_n <- sapply(group_p, function(x) {
    
    grep(x, dimnames(post_samples)[[2]], value = T)
    
  }); group_pars_n <- as.vector(group_pars_n)
  
  # vector with names of ind-lvl paramteres
  ind_pars_n <- grep(paste0('^', id_p), dimnames(post_samples)[[2]], value = T)
  
  if(ll_c) {
    
    # vector with names of logPp
    ll_n <- grep('log', dimnames(post_samples)[[2]], value = T)
    
    # matrix with log-likelihoods
    ll <- post_samples[, ll_n]
    
  } else {
    
    ll <- 'loo was not computed'
    
  }
  
  # mcmc.list with group-lvl paramteres
  # group_pars <- M$mcmc[, group_pars_n, ]
  group_pars <- post_samples[, group_pars_n]
  
  # mcmc.list with ind-lvl paramteres
  # ind_pars <- M$mcmc[, ind_pars_n, ]
  ind_pars <- post_samples[, ind_pars_n]
  
  
  # output
  return(list(g_pars = group_pars,
              i_pars = ind_pars,
              ll = ll,
              chains = post_samples[,'CHAIN'],
              g_pars_n = group_pars_n,
              i_pars_n = ind_pars_n))
  
}