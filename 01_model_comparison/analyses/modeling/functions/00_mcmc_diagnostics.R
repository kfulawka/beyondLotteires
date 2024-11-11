diagnostic_mcmc <- function(M, # runjags object
                            pars = NULL,
                            write_path
                            ) {
  
  # chains
  sapply(c('trace', 'density', 'autocorr'), function(tt) {
    
    jpeg(filename = paste0(write_path, '_', tt, '.jpg'),
         units = 'cm',
         width = 24,
         height = 12,
         res = 700,
         quality = 100)
    
    plot(M, 
         vars = pars,
         plot.type = tt,
         layout = c(2, length(pars) / 2))
    
    dev.off()
    
    return(NULL)
    
  })

  jpeg(filename = paste0(write_path, '_r_hat_seff_autocorr_ALL.jpg'),
       units = 'cm',
       width = 24,
       height = 6,
       res = 700,
       quality = 100)
  
  # r-hat and autocorr at lag 10
  layout(matrix(1:3, nrow = 1))
  
  try(lapply(c('SSeff', grep('^AC.', dimnames(M$summaries)[[2]], value = T), 
               'psrf'), function(x) {
    
    hist(M$summaries[,x], 
         breaks = 'scott',
         main = x,
         xlab = x)
    
    return(NULL)
    
  }))
  
  dev.off()
  
}
