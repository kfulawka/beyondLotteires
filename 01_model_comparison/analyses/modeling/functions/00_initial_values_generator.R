initial_values_generator <- function(starting_vals = NULL,
                                     scaling_vals = 1,
                                     n_par = 4,
                                     nchains = 8) {
  
  # set starting values to zeros if not provided
  if(is.null(starting_vals)) {
    
    starting_vals <- rep(0, n_par)
    
  }
  
  # transform starting vals to Phi scale
  starting_vals <- qnorm(starting_vals / scaling_vals, 0, 1)
  
  # initial values 
  int_vals <- function () {
    list (
      
      mu_par_phi = rnorm(n_par, starting_vals, .1),
      sigma_par = runif(n_par, .9, 1.1)
      
    )
  }
  
  # OUTPUT
  return(int_vals)
  

}
