source('analyses/modeling/functions/01_cpt_functions.R')

cpt_mods_pr <- function(xy, p, a, d, g, t,
                        ar = T,
                        apwf = T,
                        out = 'pa') {
  
  # subjective outcome values
  if(ar) { sv = xy } else { sv = v(xy, a) }
  
  # subjective probs
  if(apwf) {
    
    max_sv = max(abs(sv))/10
    
    wp1 <- pwf_ge(p[,1], g = g^max_sv, d = d * max_sv )
    wp2 <- pwf_ge(p[,2], g = g^max_sv, d = d * max_sv )
    wp <- cbind(wp1, wp2)

  } else {
    
    wp <- pwf_ge(p, g = g, d = d)
    
  }
  
  # subjective values
  va <- sv[,1] * wp[,1]
  vb <- sv[,2] * wp[,2]
  
  if(out == 'ev') return( cbind(va = va, vb = vb))
  
  
  # choice A probability
  pa <- softmax(va, vb, theta = t)
  
  if(out == 'pa') return(pa)
  
  
  # predicted choice
  co <- rbinom(length(pa), 1, pa)
  
  if(out == 'co') return(co)
  
  
}
