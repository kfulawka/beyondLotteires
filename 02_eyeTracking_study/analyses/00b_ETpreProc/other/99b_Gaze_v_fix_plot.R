load("C:/Users/kfulawka/Desktop/Psychologia/MPIB/ETstudy/data/ETdata/etRawClean.RData")

# single subject ----------------------------------------------------------

x <- etCleanPix[[sample(1:72, 1)]]$poor

fxp <- fix(x)

par(mar = c(2, 2, 3, 1))
layout(matrix(1:4, nrow = 2, ncol = 2))
#layout(1)

# set to 0 for gaze points
p = 1

for( i in unique(x$CPNum)) {
  xt <- x[x$CPNum == i, ]
  fff <- fxp[fxp$CPNum == i, ]
  
  if (!p) {
    plot(x = xt$xav, y = xt$yav
         , pch  = 19
         , col = rgb(.5, .5, .5, .5)
         , xlim = c(0, 1920)
         , ylim = c(1080, 0)
         , xlab = 'x'
         , ylab = 'y'
         , main = paste(i)
         , cex = .5)
  } else {
    smoothScatter(x = xt$xav, y = xt$yav
                  , xlim = c(0, 1920)
                  , ylim = c(1080, 0)
                  , xlab = 'x'
                  , ylab = 'y'
                  , main = paste(i)
                  , cex = 1) 
  }
  
  points(fff$x[fff$CPNum == i], fff$y[fff$CPNum == i]
         , pch = 19
         , col = rgb(fff$f_num/nrow(fff), .1, .5, .5)
         , cex = sqrt((fff$dur/20)/pi))
  
  segments(x0 = fff$x[1:(nrow(fff)-1)], y0 = fff$y[1:(nrow(fff)-1)]
           , x1 = fff$x[2:nrow(fff)], y1 = fff$y[2:nrow(fff)]
           , col = rgb(fff$f_num[1:(nrow(fff)-1)]/nrow(fff), .1, .5, .5))
}
