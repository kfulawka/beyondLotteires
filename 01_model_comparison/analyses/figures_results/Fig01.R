rm(list = ls())

# figure 1
# example of value and probability weighting functions from CPT

cairo_pdf('analyses/figures_results/figs/Fig01.pdf',
          height = 8 * 0.393701,
          width = 15 * 0.393701,
          pointsize = 9)

# figures' margins
par( mar = c(4, 4, 4, 1))

# layout
layout(matrix(c(1, 2),
              byrow = T,
              nrow = 1))

# Panel A: value function -------------------------------------------------

# value function
v_fun = function(x, a, l = 1, p = 1) {
  
  sv = sapply(x, function(y) {
    if(y > 0)  l = 1 
    
    p * sign(y) * l * abs(y)^a
    
  })
  
  return(sv)
  
}

# set x min value
x_min <- c(-100)

# example alphas
alphas = c(.2, .4, .6, .8)

# set min y val
min_ux = v_fun(x_min, max(alphas))

# colors
ac = viridis::viridis(4, .7)

# set panel
plot(0, 
     type = 'n',
     xlim = c(x_min, 100),
     ylim = c(min_ux, -min_ux),
     # yaxt = 'n',
     # xaxt = 'n',
     xlab = 'x [monetary outcome value]',
     ylab = 'v(x)',
     main = 'Value function')

# # AXES
# axis(1, padj = -1, cex.axis = 1)
# axis(2, padj = 1, cex.axis = 1)
# title(xlab = 'x [monetary outcome value]',
#       ylab = 'v(x)',
#       line = 1.7,
#       cex.lab = 1)
# title(main = 'Value function',
#       line = 1
#       )

# identity line
abline(0, 1, lty = 2, col = rgb(0, 0, 0, .7))

# grid
abline(v = seq(-100, 100, by = 20),
       h = seq(-40, 40, by = 10),
       col = rgb(.7, .7, .7, .5))

abline(v = 0,
       h = 0,
       col = 'black')

for(i in 1:4) {
  
  # add curves
  curve(v_fun(x, a = alphas[i]), 
        from = -100,
        to = 100,
        col = ac[i],
        add = T,
        lwd = 2)
  
}

# add legend
legend(-100, 40,
       legend = alphas,
       title = expression(alpha),
       bty = 'n',
       col = ac,
       lty = 1,
       lwd = 2,
       cex = 1)

mtext('a',
      side = 3,
      line = 3,
      adj = -.2,
      col = 'black',
      font = 2)

# Panel B: PWF ------------------------------------------------------------

# example parameters
gd = list( a = c(.2, 3), b = c(.4, .8), c = c(.9, 1))
pc = viridis::viridis(3, .7)

# PWF
GE <-function(x, gam, del) del*x^gam / ( del*x^gam + (1-x)^gam )

# PANEL
plot(0,
     type = 'n',
     xlim = c(0, 1),
     ylim = c(0, 1),
     main = 'Probability weighting function',
     # yaxt = 'n',
     # xaxt = 'n',
     ylab = 'w(p)',
     xlab = 'p [outcome probability]')

# AXES
# axis(1, padj = -1, cex.axis = 1)
# axis(2, padj = 1, cex.axis = 1)
# title(xlab = 'p [outcome probability]',
#       ylab = 'w(p)',
#       line = 1.7,
#       cex.lab = 1)
# title(main = 'Probability weighting function',
#       line = 1)

# REFERENCE LINE
lines(x = 0:1, 
      y = 0:1, 
      col = 'black', 
      lwd = 1, 
      lty = 2)

# grid
abline(h = seq(0, 1, .1),
       v = seq(0, 1, .1),
       col = rgb(.7, .7, .7, .5))

# PWFs examples
for(i in 1:length(gd)) {
  
  curve(GE(x, gam = gd[[i]][1], del = gd[[i]][2])
        , from = 0, to = 1
        , n = 1000
        , add = T
        , col = pc[i]
        , lty = 1
        , lwd = 2 )
}

# legend
legend(.5, .4,
       legend = unlist(lapply(gd, paste, collapse = ' & ')),
       title = '\u03B3 & \u03B4',
       bty = 'n',
       col = pc,
       lty = 1,
       lwd = 2,
       cex = 1)

mtext('b',
      side = 3,
      line = 3,
      adj = -.2,
      col = 'black',
      font = 2)

# save figure -------------------------------------------------------------

dev.off()