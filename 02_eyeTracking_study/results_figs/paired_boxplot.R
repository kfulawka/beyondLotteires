paired_boxplot = function(xx,
                          box_col = rgb(.5, .5, .5, .7),
                          point_col = rgb(.8, .1, .3, .5),
                          line_col = rgb(.8, .1, .3, .2),
                          yl = '',
                          xl = '',
                          x_labs = c('', ''),
                          ylim = NULL,
                          title = '',
                          grid = F,
                          h_glines = NULL) {
  
  # 
  n = nrow(xx)
  
  if(is.null(ylim)) ylim = c(min(xx), max(xx))
  
  # individual predictions correct
  boxplot(xx,
          ylab = yl,
          xlab = xl,
          ylim = ylim,
          lwd = 1,
          xaxt = 'n',
          staplewex = 0,
          outline = F,
          col  = rgb(1, 1, 1, 0),
          border = box_col,
          main = title
          )
  
  axis(1, at = 1:2, labels = x_labs)
  
  xi = matrix(c(rep(1, n), rep(2, n)), nrow = n)
  
  if(grid) {
    
    abline(h = h_glines,
           col = rgb(.7, .7, .7, .3))
    
  }
  
  if(length(point_col) == 2) point_col = rep(point_col, each = n)
  
  points(xi, xx,
         col = point_col,
         pch = 19)
  
  segments(x0 = 1, x1 = 2, y0 = xx[,1], xx[,2],
           col = line_col)
  
  

  
}

