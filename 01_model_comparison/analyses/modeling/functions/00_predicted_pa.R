predicted_pa <- function(ll, choice) { 
  
  # ll: vector with log(p[choice])
  # choice: matrix with choice data, with one col per subject

  # event probability
  pa <- exp(ll) 
  
  # recode to p(1|{0,1})
  pa[choice == 0] <- 1 - pa[choice == 0] 
  
  # into matrix
  pa <- array(pa,
              dim = dim(choice))
  
  # choice proportions across choice problems
  cp_pred <- apply(pa, 1, mean) 
  
  # output
  return(list(pa = pa, cp_pred = cp_pred))
  
}