rm(list = ls())

# data ---------------------------------------------------

# arrays and data frames
df = read.csv('data/df_ct_fix.txt')

df = df[df$CPNum > 59, c('sub', 'choice', 'CPNum', 'cond',
                         'xAr', 'xBr', 'pA', 'pB', 'xAaf_m', 'xBaf_m')]


# swapp -------------------------------------------------------------------

df_s = df

for(i in 1:nrow(df)) {
  
  if(df_s$pA[i] > df_s$pB[i]) {
    
    df_s[i, c('xAr', 'xBr', 'pA', 'pB', 'xAaf_m', 'xBaf_m')] = df[i, c('xBr', 'xAr', 'pB', 'pA', 'xBaf_m', 'xAaf_m')]
    df_s$choice[i] = ifelse(df_s$choice[i] == 1, 0, 1)
    
  }
  
}

# # sanity check
# aa = aggregate(cbind(xAaf_m, xBaf_m, pA, pB, xAr, xBr, choice) ~ CPNum + cond, 
#                data = df_s, FUN = mean)


# within subject dominated choices ----------------------------------------

sub_a = aggregate(choice ~ sub,
                  data = df_s,
                  FUN = function(x) 10 - sum(x) # the number of 'wrong' choices
                  )

sub_a = sub_a[order(sub_a$choice, decreasing = T), ]
head(sub_a)

# 140 for removal

# affect ratings ----------------------------------------------------------

# raw AR data
ar = read.table('data/ar_raw.txt', sep = '\t', header = T)

# within participant sds
ar_a = aggregate(affect ~ sub + part, 
                 FUN = mad,
                 data = ar)

ar_a = ar_a[order(ar_a$affect), ]
head(ar_a)

# plot ratings
par(mfrow = c(8, 9),
    mar = c(1, 1, 3, 1))

ars = lapply(unique(ar$sub), function(i) {
  
  dd = ar[ar$sub == i, ]
  
  d1 = dd[dd$part == 0, ]
  d2 = dd[dd$part == 1, ]
  
  d1 = d1[d1$SE %in% d2$SE, ]
  
  d1 = d1[order(d1$SE), ]
  d2 = d2[order(d2$SE), ]
  
  crs = cor(d1$affect, d2$affect, method = 's')
  
  # figure
  plot(d1$affect, d2$affect,
       col = rgb(.5, .5, .5, .5),
       pch = 19,
       xlab = '', ylab = '',
       main = paste(i, round(crs, 2)),
       xlim = c(0, 100), ylim = c(0, 100),
       xaxt = 'n', yaxt = 'n')
  abline(0, 1, col = 'red')
  
  d = merge(d1[,c('sub', 'SE', 'affect')], 
            d2[,c('sub', 'SE', 'affect')],
            by = c('sub', 'SE'))
  
  return(d)
  # return(crs)
  
}); 

ars = data.frame( data.table::rbindlist(ars) )
layout(1); par(mar = c(5,5,3,1))
# hist(crs)
# 153 and 155 for removal
plot(ars$affect.x, ars$affect.y,
     pch = 19,
     col = rgb(.1, .1, .6, .3)); abline(0, 1, col = 'red')
plot(ars$affect.x, ars$affect.x - ars$affect.y); abline(h = 0, col = 'red')
