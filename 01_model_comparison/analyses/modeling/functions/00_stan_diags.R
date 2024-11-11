stan_diag = function(stanFit,
                     n,
                     ind_p,
                     group_p,
                     write_path = 'analyses/modeling/diagnostics') {
  
  
  # function to print plots to specified directory
  print_jpg <- function(x, 
                        w = 20,
                        h = 10, 
                        pe = 'diag') {
    
    # open jpg
    jpeg(paste(write_path, pe, sep = '_'), 
         width = w, 
         height = h, 
         units = 'cm', 
         res = 200,
         quality = 100)
    
    print(x) # print in the plot
    
    dev.off() # close jpg
    
  }
  
  # diagnostics -------------------------------------------------------------
  
  # ind pars names as returned by stan ( e.g., theta[1] )
  ind_pars <- paste(rep(ind_p, each = n), '[', 1:n, ']', sep = '') 
  
  # parameters for diagnostic
  p_diag <- c(group_p, 'lp__')
  
  # posterior draws as array
  post_draws <- as.array(stanFit)[,, group_p]
  
  # nuts parameters
  nuts_pars <- nuts_params(stanFit)
  
  # log posterior
  lp <- log_posterior(stanFit)
  
  # # parallel coordinates plot
  # color_scheme_set("brightblue")
  # print_jpg(x = mcmc_parcoord(post_draws, 
  #                             np = nuts_pars),
  #           pe = 'paral_p.jpg')
  
  # pairs plot
  print_jpg(x = mcmc_pairs(post_draws, 
                           np = nuts_pars, 
                           pars = group_p, 
                           off_diag_args = list(size = 0.75)),
            h = 20, 
            w = 40, 
            pe = 'pairs.jpg')
  
  # trace plots
  color_scheme_set("mix-brightblue-gray")
  print_jpg(
    x = mcmc_trace(post_draws, 
                   pars = group_p,
                   np = nuts_pars),
    pe = 'chains.jpg')
  
  
  # # nuts accptance
  # color_scheme_set("brightblue")
  # print_jpg(x = mcmc_nuts_divergence(nuts_pars, lp),
  #           pe = 'nuts_ac.jpg')
  # 
  # # nuts energy
  # print_jpg(x = mcmc_nuts_energy(nuts_pars),
  #           pe = 'nuts_energy.jpg')
  
  # r-hats
  rhats <- rhat(stanFit)[c(p_diag, ind_pars)]
  print_jpg(x =  mcmc_rhat(rhats), 
            pe = 'r_hats.jpg')
  
  # effective sample sizes
  ratios_nss <- neff_ratio(stanFit)[c(p_diag, ind_pars)]
  print_jpg(x = mcmc_neff(ratios_nss, 
                          size = 2), 
            pe = 'ess.jpg')
  
  # autocorrelations
  print_jpg(x = mcmc_acf(post_draws, 
                         pars = group_p, 
                         lags = 20),
            h = 20, 
            w = 40, 
            pe = 'acf.jpg')
  
}
  