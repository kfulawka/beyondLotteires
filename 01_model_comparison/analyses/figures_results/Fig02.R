rm(list = ls())

# figure 1
# example of value and probability weighting functions from CPT
# example of nonmoentary vs. monetary choice problems
# loglikelihood comparisons from multiple papers

cairo_pdf('analyses/figures_results/figs/Fig02.pdf',
          height = 13.5 * 0.393701,
          width = 15 * 0.393701,
          pointsize = 12)

# layout 1, 3, 2, 3
layout(matrix(c(1, 3, 2, 3, 4, 5),
              byrow = T,
              nrow = 3),
       heights = c(.5, .5, 1))

# colors
cc = c(rgb(.1, 0, 1, alpha = .8), rgb(1, 0, .1, alpha = .8))

# Panel A: choice problems examples ---------------------------------------

# figures' margins
par(mar = c(3,3,3,2))

plot(0,
     type = 'n',
     xlim = 0:1,
     ylim = 0:1,
     xlab = '',
     ylab = '',
     xaxt = 'n',
     yaxt = 'n',
     bty = 'n'
)
title("Nonmonetary choice", 
      line = 1,
      col.main = cc[2])

text(0, 1, 'Drug A',
     col = cc[2],
     adj = c(0, 1))
rect(0, 0, .475, .8, 
     border = cc[2])

text(.01, 
     c(.67, .45, .23), 
     c('Side effect', 'Cough', 'No cough'),
     col = c(cc[2], 'black', 'black'),
     adj = c(0, 1),
     cex = .8)

text(.465, 
     c(.67, .45, .23), 
     c('Probability', '10%', '90%'),
     col = c(cc[2], 'black', 'black'),
     adj = c(1, 1),
     cex = .8)


text(.525, 1, 'Drug B',
     col = cc[2],
     adj = c(0, 1))
rect(.525, 0, 1, .8, 
     border = cc[2])

text(.535, 
     c(.67, .45, .23), 
     c('Side effect', 'Blood clot', 'No blood clot'),
     col = c(cc[2], 'black', 'black'),
     adj = c(0, 1),
     cex = .8)


text(.99, 
     c(.67, .45, .23), 
     c('Probability', '0.01%', '99.99%'),
     col = c(cc[2], 'black', 'black'),
     adj = c(1, 1),
     cex = .8)

mtext('Drugs equally effective',
      side = 1,
      line = .7,
      at = .5,
      adj = c(.5, .5),
      col = 'black',
      cex = .7)

mtext('a',
      side = 3,
      line = 1.75,
      adj = -.2 * 6/7,
      col = 'black',
      font = 2,
      cex = .7)

# figures' margins
par(mar = c(4,3,2,2))

# MONETARY CONTEXT
plot(0,
     type = 'n',
     xlim = 0:1,
     ylim = 0:1,
     xlab = '',
     ylab = '',
     xaxt = 'n',
     yaxt = 'n',
     yaxt = 'n',
     bty = 'n')
title("Monetary choice", 
      line = 1,
      col.main = cc[1])

text(0, 1, 'Lottery A',
     col = cc[1],
     adj = c(0, 1))
rect(0, 0, .475, .8, 
     border = cc[1])

text(.02, 
     c(.67, .45, .23), 
     c('Loss', '2 CHF', '0 CHF'),
     col = c(cc[1], 'black', 'black'),
     adj = c(0, 1),
     cex = .8)

text(.455, 
     c(.67, .45, .23), 
     c('Probability', '10%', '90%'),
     col = c(cc[1], 'black', 'black'),
     adj = c(1, 1),
     cex = .8)

text(.525, 1, 'Lottery B',
     col = cc[1],
     adj = c(0, 1))
rect(.525, 0, 1, .8, 
     border = cc[1])

text(.545, 
     c(.67, .45, .23), 
     c('Loss', '100 CHF', '0 CHF'),
     col = c(cc[1], 'black', 'black'),
     adj = c(0, 1),
     cex = .8)

text(.98, 
     c(.67, .45, .23), 
     c('Probability', '0.01%', '99.99%'),
     col = c(cc[1], 'black', 'black'),
     adj = c(1, 1),
     cex = .8)

mtext('Monetary equivalents of side effects',
      side = 1,
      line = .7,
      at = .5,
      adj = c(.5, .5),
      col = 'black',
      cex = .7)

# Panel B: PWF ------------------------------------------------------------

# figures' margins
par(mar = c(4,4,3,3))

# example parameters
gg <- c(.32, .44)
dd <- c(4.5, 1.5)

# PWF
GE <-function(x, gam, del) del*x^gam / ( del*x^gam + (1-x)^gam )

# PANEL
plot(0,
     type = 'n',
     xlim = c(0, 1),
     ylim = c(0, 1),
     main = "",
     yaxt = 'n',
     xaxt = 'n',
     ylab = '',
     xlab = '')

# AXES
axis(1, padj = -1, cex.axis = 1)
axis(2, padj = 1, cex.axis = 1)
title(xlab = 'p [probability]',
      ylab = 'w(p)',
      line = 1.7,
      cex.lab = 1)

title(main = 'Probability weighting function',
      line = 1)

# plot no
# mtext('A',
#       side = 3,
#       line = 1.5,
#       at = -.15,
#       adj = c(0, 0),
#       col = 'red')

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
for(i in 1:2) {
  
  curve(GE(x, gam = gg[i], del = dd[i])
        , from = 0, to = 1
        , n = 1000
        , add = T
        , col = cc[2:1][i]
        , lty = 1
        , lwd = 2 )
}

# gamma legend
legend(.35, .3,
       legend = paste(gg),
       title = expression(gamma),
       bty = 'n',
       col = cc[2:1],
       lty = 1,
       lwd = 2,
       cex = 1)

# delta legend
legend(.7, .3,
       legend = paste(dd),
       title = expression(delta),
       bty = 'n',
       col = cc[2:1],
       lty = 1,
       lwd = 2,
       cex = 1)

mtext('b',
      side = 3,
      line = 1.75,
      adj = -.2,
      col = 'black',
      font = 2,
      cex = .7)

# #  legend
# legend(0, 1,
#        legend = c('medical (side effects)',
#                   'monetary (losses)'),
#        # title = 'domain',
#        bty = 'n',
#        col = cc,
#        lty = 1,
#        lwd = 2,
#        cex = 1)

# Panel C: value function -------------------------------------------------

par(mar = c(3,3,4,4))

# only in the negative domain and withouth loss aversion
val_fun = function(x, a) sign(x) * abs(x)^a

# set x min value
x_min <- c(-100)

# example alphas
alphas <- c(.26, .37)

# set min y val
min_ux = val_fun(x_min, max(alphas))

# set panel
plot(0, 
     type = 'n',
     xlim = c(x_min, -1),
     ylim = c(min_ux, -1),
     yaxt = 'n',
     xaxt = 'n',
     xlab = '',
     ylab = '',
     main = '')

# AXES
axis(1, padj = -1, cex.axis = 1)
axis(2, padj = 1, cex.axis = 1)
title(xlab = 'x [monetary equivalents]',
      ylab = 'v(x)',
      line = 1.7,
      cex.lab = 1)

title(main = 'Value function',
      line = 1)

# identity line
abline(0, 1, lty = 2, col = rgb(0, 0, 0, .7))

# grid
abline(v = seq(-100, 0, by = 10),
       h = seq(-6, 0, by = .5),
       col = rgb(.7, .7, .7, .5))

for(a in alphas) {
  
  # add curves
  curve(val_fun(x, a = a), 
        from = -100,
        to = -1,
        col = ifelse(a == alphas[2], cc[2], cc[1]),
        add = T,
        lwd = 2)
  
}

# add legend
legend(-100, -1,
       legend = alphas[2:1],
       title = expression(alpha),
       bty = 'n',
       col = cc[2:1],
       lty = 1,
       lwd = 2,
       cex = 1)

mtext('c',
      side = 3,
      line = 1.75,
      adj = -.2,
      col = 'black',
      font = 2,
      cex = .7)

# # Panel D: Loglikelihood comparison ---------------------------------------
# 
# # data
# mod_fits <- data.frame(context = c('monetary', 'medical'),
#                        data = rep(c('Pachur et al. (2014),\nStudy 1',
#                                     'Pachur et al. (2014),\nStudy 2',
#                                     'Pachur et al. (2014),\nStudy 3',
#                                     'Suter et al. (2015)',
#                                     'Suter et al. (2016),\nwithout WTP',
#                                     'Suter et al. (2016)\nwith WTP',
#                                     'Pachur et al.\n(2017)'), each = 2),
#                        rel_logLik = 1 - c(624.74/720.9, 688.41/720.9,
#                                           687.9/1441.7, 1212.49/1441.7,
#                                           1382.08/1730.1, 1375.68/1730.1,
#                                           40.6/116.45, 59.74/116.45,
#                                           24.99/61, 44.32/61, 24.34/61, 44.31/61,
#                                           2397.16/4879.76, 3960.42/4879.76)
#                        )
# 
# par(mar = c(5, 2.5, 3, 1.5))
# 
# # set plot
# plot(c(-.2, .2) + rep(1:(nrow(mod_fits)/2), each = 2),
#      mod_fits$rel_logLik,
#      # xlim = c(0, 15),
#      type = 'h',
#      xaxt = 'n',
#      yaxt = 'n',
#      ylab = '',
#      xlab = '',
#      lwd = 3,
#      main = 'CPT model fit',
#      col = cc)
# # add y-axis
# axis(2, padj = 1, cex.axis = 1)
# 
# # visually separate the studies
# segments(x0 = seq(1.5, 6.5, by = 1), 
#          y0 = min(mod_fits$rel_logLik), 
#          y1 = max(mod_fits$rel_logLik),
#          lty = 2,
#          col = rgb(.5, .5, .5, .5))
# 
# # x-axis
# axis(1, at = 1:7, labels = rep('', 7))
# for(i in 1:2) {
#   axis(1, 
#        at = (1:7)[seq(i, 7, 2)],
#        labels = unique(mod_fits$data)[seq(i, 7, 2)],
#        cex.axis = .85,
#        line = ifelse(i == 1, 
#                      .3, 2.5),
#        tick = F,
#   )
# }
# 
# title(ylab = 'relative log-likelihood',
#       line = 1.7,
#       cex.lab = 1)
# 
# mtext('d',
#       side = 3,
#       line = 1.75,
#       adj = 0,
#       font = 2,
#       col = 'grey')
# # 
# Panel D: Loglikelihood comparison ---------------------------------------

# data
mod_fits <- data.frame(context = c('monetary', 'medical'),
                       data = rep(c('Pachur et al. (2014)\nstudy 1',
                                    'Pachur et al. (2014)\nstudy 2',
                                    'Pachur et al. (2014)\nstudy 3',
                                    'Suter et al. (2015)',
                                    'Suter et al. (2016)\nwithout WTP',
                                    'Suter et al. (2016)\nwith WTP',
                                    'Pachur et al. (2017)'), each = 2),
                       rel_logLik = 1 - c(624.74/720.9, 688.41/720.9,
                                          687.9/1441.7, 1212.49/1441.7,
                                          1382.08/1730.1, 1375.68/1730.1,
                                          40.6/116.45, 59.74/116.45,
                                          24.99/61, 44.32/61, 24.34/61, 44.31/61,
                                          2397.16/4879.76, 3960.42/4879.76)
)

par(mar = c(3, 4, 4, 3))

# set plot
plot(x = 0,
     xlim = c(0, .7),
     ylim = c(.5, 7.5),
     type = 'n',
     yaxt = 'n',
     xaxt = 'n',
     ylab = '',
     xlab = '',
     lwd = 3,
     main = '',
     col = cc)

title(main = 'CPT model fit',
      line = 1)

# results
segments(y0 = c(-.2, .2) + rep(1:(nrow(mod_fits)/2), each = 2),
         x0 = 0,
         x1 = mod_fits$rel_logLik,
         lwd = 2,
         col = cc)

# add x-axis
axis(1, padj = -1, cex.axis = 1)
title(xlab = 'Relative log-likelihood',
      line = 1.7,
      cex.lab = 1)

# visually separate the studies
segments(y0 = seq(1.5, 6.5, by = 1), 
         x0 = -.1, 
         x1 = .8,
         lty = 2,
         col = 'black')

# grid
abline(v = seq(0, .7, .05),
       col = rgb(.7, .7, .7, .5))

# y-axis
for(i in 1:2) {
  axis(2, 
       at = (1:7)[seq(i, 7, 2)],
       labels = unique(mod_fits$data)[seq(i, 7, 2)],
       cex.axis = .8,
       line = -.75,
       tick = F,
       las = 1
  )
}

mtext('d',
      side = 3,
      line = 1.75,
      adj = -.2,
      col = 'black',
      font = 2,
      cex = .7)

# save figure -------------------------------------------------------------

dev.off()
