# plot raw data ---------------------------------------------------------------

layout(matrix(1:20, ncol = 4, byrow = T))
par(mar = c(1, 2, 3, 0))

# problematic data
ss = c(1, 7, 19, 22, 26, 30, 38, 61, 65, 66)

for (i in c('poor', 'rich')) {
  for (j in ss) {
    for (k in c('horizontal', 'vertical')) {
      
      smoothScatter(x = etClean[[j]][[i]][etClean[[j]][[i]][, 'lay'] == k, 'xav']
                    , y = etClean[[j]][[i]][etClean[[j]][[i]][, 'lay'] == k, 'yav']
                    , xlim = c(0, 1)
                    , ylim = c(1, 0)
                    #, col = rgb(.7, .1, .1, .5)
                    , xlab = '', ylab = '', main = paste(subNums[j], i)
                    , xaxt = 'n'
                    , yaxt = 'n'
      )
      
      if (k == 'horizontal') {
        
        segments(x0 = 0, x1 = 1, y0 = yc, col = 'red')
        segments(x0 = xc, y0 = 0, y1 = 1, col = 'red')
        
      } else {
        
        segments(x0 = 0, x1 = 1, y0 = xc, col = 'red')
        segments(x0 = yc, y0 = 0, y1 = 1, col = 'red')
        
      }
    }
  }
}
