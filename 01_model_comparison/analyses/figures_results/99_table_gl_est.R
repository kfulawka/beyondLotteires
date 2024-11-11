table_gl_est = function(res,
                        a = 6,
                        tab_n = 'tab1.txt') {
  
  # medians and 95% CIs
  ps = lapply(names(res), function(xx) {
    
    x = res[[xx]]

    # quantiles
    pp = apply(x, 2, function(p) quantile(p, c(.5, .025, .975))  )
    
    # scale ardw by AR = 6
    if( grepl('_adw|wtp_aw', xx) ) {
      
      pp[,'mu_gam'] = pp[,'mu_gam']^(a/10)
      pp[,'mu_del'] = pp[,'mu_del']*(a/10)
      
    }
    
    # 
    pp = round(pp, 2)
    
    #
    pr = apply(pp, 2, function(y) {
      
      paste0(y[1], ' [', y[2], ', ', y[3], ']')
      
    })
    
    # 
    if( grepl('_ar|adw', xx) ) {
      
      pr = c('', pr)
      
    }
    
    return( pr ) 
    
  })
  
  ps = do.call(rbind, ps)
  rownames(ps) = names(res)
  
  #
  write.table(ps, 
              file = paste0('analyses/figures_results/', tab_n),
              sep = ' & ',
              quote = F,
              row.names = T,
              col.names = F)
  
}