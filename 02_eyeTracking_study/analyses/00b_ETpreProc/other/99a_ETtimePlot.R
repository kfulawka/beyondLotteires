
# convert timestamps to ms
x <- etClean[[10]][['poor']]

xt <- x[x$CPNum == 40, ]

# check sampling ----------------------------------------------------------

xt$s_time = xt$s_time/1e+3 - min(xt$s_time)/1e+3
max(xt$s_time)
summary(xt$s_time)

tt <-0

for (i in 2:length(x$s_time)) {
  
  tt[i] = x$s_time[i] - x$s_time[i-1]
  
}
summary(tt)

layout(1)
par(mar = c(4,5,3,2))

plot(0
     , xlim = c(0, 1920)
     , ylim = c(1080, 0)
     , xlab = 'x'
     , ylab = 'y'
     , type = 'n'
)

# horizontal
segments(x0 = 0, x1 = 1, y0 = yc, col = 'red')
segments(x0 = xc, y0 = 0, y1 = 1, col = 'red')
# vertical
segments(x0 = 0, x1 = 1, y0 = xc, col = 'red')
segments(x0 = yc, y0 = 0, y1 = 1, col = 'red')

for (i in xt$s_time) {
  
  points(x = xt$xav[xt$s_time == i]
         , y = xt$yav[xt$s_time == i]
         , pch  = '.'
         , col = rgb(.1, .1, .8, .5)
         , cex = 1
         )
  # Sys.sleep(.1)
}

