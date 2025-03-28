rm(list = ls())

# colors
cols = c()
for (i in 1:10) cols[i] = rgb(.1, .1, i/10, .5)

ga = function(gam, a) gam^a
# ga = function(gam, a) gam*(1.01-a^2)
# ga = function(gam, a) gam * (cos(a*pi)+1)/2
da = function(del, a) del*a

# PWF
GE <-function(x, a, gam, del) { 
  
  g = ga(gam, a); d = da(del, a)
  
  wp = d*x^g / ( d*x^g + (1-x)^g ); return(wp)
  
}

# affect ratings
aa <- 1:10/10

# gammas
gammas = seq(.1, 1, .1)
deltas = 1:10

# weighting function plots ----------------------------------------

# example parameters
gi <- 4
di <- 10

cairo_pdf('analyses/figures_results/figs/Fig03.pdf',
          height = 5.5 * 0.393701,
          width = 16 * 0.393701,
          pointsize = 9)

# figures' margins
par( mar = c(4, 4, 4, 1))

# layout
layout(matrix(c(1:3),
              byrow = T,
              nrow = 1))

# gamma changes with affect -----------------------------------------------

# PANEL
plot(0,
     type = 'n',
     xlim = c(1, 10), ylim = c(0, 1),
     main = 'Affective probability sensitivity',
     ylab = '',
     xlab = expression(a[i]^max))

title(ylab = expression(gamma[i]^a), line = 2.5)

# grid
abline(h = seq(0, 1, .1),
       v = seq(1, 10, 1),
       col = rgb(.7, .7, .7, .5))

points(1:10, ga(gammas[1], aa), col = cols, pch = 15)
points(1:10, ga(gammas[4], aa), col = cols, pch = 16)
points(1:10, ga(gammas[8], aa), col = cols, pch = 17)

legend(1.5, .4,
       legend = as.expression(c(bquote(gamma == .(gammas[1])),
                                bquote(gamma == .(gammas[4])),
                                bquote(gamma == .(gammas[8])))),
       title = '',
       bty = 'n',
       pch = 15:17)

mtext('a',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)


# delta changes with affect -----------------------------------------------

# PANEL
plot(0,
     type = 'n',
     xlim = c(1, 10), ylim = c(0, 10),
     main = 'Affective pessimism/optimism',
     ylab = '',
     xlab = expression(a[i]^max))

title(ylab = expression(delta[i]^a), line = 2.5)

# grid
abline(h = seq(0, 10, 1),
       v = seq(1, 10, 1),
       col = rgb(.7, .7, .7, .5))

points(1:10, da(deltas[1], aa), col = cols, pch = 15)
points(1:10, da(deltas[5], aa), col = cols, pch = 16)
points(1:10, da(deltas[10], aa), col = cols, pch = 17)

legend(1.5, 9,
       legend = as.expression(c(bquote(delta == .(deltas[1])),
                                bquote(delta == .(deltas[5])),
                                bquote(delta == .(deltas[10])))),
       title = '',
       bty = 'n',
       pch = 15:17)


mtext('b',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)

# ADW ---------------------------------------------------------------------

# PANEL
plot(0,
     type = 'n',
     xlim = c(0, 1), ylim = c(0, 1),
     main = 'Affective probability weighting',
     ylab = '',
     xlab = 'p')

title(ylab = expression('w'[i]^a~'(p)'), line = 2.5)

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


# ADW EXAMPLE

for ( j in 1:10) {
  curve(GE(x, 
           gam = gammas[gi], 
           del = deltas[di], 
           a = aa[j]),
        from = 0, to = 1,
        n = 1000,
        add = T,
        col = cols[j],
        lty = 1,
        lwd = 2)
}



legend(.7, .55,
       legend = as.expression(bquote(gamma == .(gammas[gi]))),
       title = '',
       bty = 'n',
       cex = 1)

legend(.7, .45,
       legend = as.expression(bquote(delta == .(deltas[di]))),
       title = '',
       bty = 'n',
       cex = 1)

# LEGEND
legend(.5, .45, 
       legend = c('1', '5', '10'),
       title = expression(a[i]^max),
       bty = 'n',
       col = c(cols[1], cols[5], cols[10]),
       pch = 16, 
       cex = 1)

mtext('c',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)

# save figure -------------------------------------------------------------

dev.off()