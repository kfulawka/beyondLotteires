# function to gather estimated elpds
elpd_extract = function(L) {
  
  # extract elpds and 95% CIs
  elpds = sapply(L, function(l) {
    
    e = l$looE$estimates[1,]
    e[2:3] = e[1] + c(-1.96, 1.96) * e[2] 
    
    names(e) = c('elpd', 'e_li', 'e_ui')
    
    return(e)
    
  }); elpds = data.frame( t(elpds) )
  elpds$mod = rownames(elpds)
  
  # extract balanced accuracy
  bas = sapply(L, function(l) {
    
    e = l$ba$ba
    n = nrow(l$looE$pointwise)
    
    ee = c(ba_loo = e,
           ba_li = e - sqrt(e * (1 - e) / n) * 1.96,
           ba_ui = e + sqrt(e * (1 - e) / n) * 1.96
    )    

    return(ee)
    
  }); bas = data.frame( t(bas) )
  bas$mod = rownames(bas)
  
  r = merge(elpds, bas, by = 'mod')
  
  return(r)
  
}


# function to gather estimated individual elpds
i_elpd_w = function(L) {

  # extract elpds and 95% CIs
  i_loo = lapply(L, function(l) {
    
    e = lapply(l$ind_ba$i_loo, function(x) x)
    
    return(e)
    
  })
  
  # set N
  N = length(i_loo[[1]])
  
  # indvidual loo weights
  i_loo_w = sapply(1:N, function(i) {
    
    il = c(cpt = i_loo[[1]][i],
           av = i_loo[[2]][i],
           aw = i_loo[[3]][i])
    
    lw = loo_model_weights(il)
    
    return(as.matrix(lw))
    
  }); i_loo_w = t(i_loo_w)

}

# function to gather estimated individual elpds
i_acc_loo = function(L) {
  
  # individual accuracies
  i_bas = sapply(L, function(l) {
    
    e = L[[l]]$ind_ba$ba
    
    return(e)
    
  })
  
}