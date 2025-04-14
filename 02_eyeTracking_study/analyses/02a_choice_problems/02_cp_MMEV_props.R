df = read.csv('analyses/00c_data_analyses/df_swapped.csv')

aa = aggregate(cbind(xAaf1, xAaf2, xAaf_m, xBaf1, xBaf2, xBaf_m, pA, pB, choice) ~ cond + CPNum,
               data = df,
               FUN = mean)


aa$mm1 = aa$xAaf1 - aa$xBaf1
aa$mm2 = aa$xAaf2 - aa$xBaf2
aa$ev1 = aa$xAaf1*aa$pA - aa$xBaf1*aa$pB
aa$ev2 = aa$xAaf2*aa$pA - aa$xBaf2*aa$pB

aa$mm3 = aa$xAaf_m - aa$xBaf_m
aa$ev3 = aa$xAaf_m*aa$pA - aa$xBaf_m*aa$pB

par(mfrow = c(2,6))

for(ee in c('poor', 'rich')) {
  
  for(i in c('mm1', 'mm2','mm3', 'ev1', 'ev2', 'ev3')) {
  
    ab = aa[aa$cond == ee, ]
    
    cr = round(cor(ab[,i], ab$choice, method = 's'),2)
    
    plot(ab[,i], ab$choice,
         col = ifelse(ee == 'poor', 'blue', 'red'),
         # xlim = c(1, 100),
         ylim = c(0, 1),
         main = paste(i, cr))
    
  }
  
}


ff = function(xa, xb, pa, pb, co, cond) {
  
  mm = xa - xb
  ev = xa*pa - xb*pb
  
  cr_mm = cor(mm, co)
  
  
}