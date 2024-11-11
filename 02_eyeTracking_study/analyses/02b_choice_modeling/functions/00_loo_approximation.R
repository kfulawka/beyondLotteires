# function returning loo
loo_comp <- function(ll, chains) {
  
  # relative effective MCMC sample size
  rel_n_eff <- relative_eff(exp(ll), 
                            chain_id = chains,
                            cores = 8)
  
  # leave-one-out estimate
  loo_r <- loo(ll,
               r_eff = rel_n_eff,
               cores = 8)
  
}