rm(list = ls())

# set up ------------------------------------------------------------------

# libraries
library(ggplot2)
library(patchwork)
library(data.table)
library(loo)

# modeling results
pach14 = readRDS("analyses/modeling/posteriors/stan/p14.rds")
pach17 = readRDS("analyses/modeling/posteriors/stan/p17.rds")
sut16 = readRDS("analyses/modeling/posteriors/stan/s16.rds")

M = list(p17 = pach17[c(1, 2, 7, 3)],
         s16 = sut16[c(1, 2, 10, 3)],
         p14 = pach14[c(1, 2, 7, 3)])

rm(pach14, sut16, pach17)

source('analyses/figures_results/99_elpd_extract.R')
source('analyses/figures_results/99_elpd_diff.R')

# model performance -------------------------------------------------------

# gather elpds for all datasets
elpds = lapply(names(M), function(m) {
  
  d = elpd_extract(M[[m]])
  
  d$data = m
  
  return(d)
  
}); elpds = data.frame(rbindlist(elpds))


# domain
elpds$domain = ifelse(elpds$data == 'p14', 'Positive', 'Negative')

elpds$mod = factor(elpds$mod,
                   levels = c('cpt_mon_wtp', 'cpt_mon_ar', 'cpt_mon_wtp_aw', 'cpt_mon_adw'),
                   labels = c('CPT', 'AV', 'APW', 'AV-APW'),
                   ordered = T)

elpds = elpds[order(elpds$data, elpds$domain, elpds$mod), ]
# model comparison --------------------------------------------------------

# model comparisons
comps = list(av_cpt = c(2,1), 
             aw_cpt = c(3,1),
             avw_cpt = c(4,1),
             aw_av = c(3,2), # remove the APW v. AV comparison
             avw_av = c(2,4),
             avw_aw = c(3,4))
comps_n = names(comps)
comps_labs = c('AV - CPT', 'APW - CPT', 'AV-APW - CPT', 
               'APW - AV',
               'AV - APW-AV', 'APW - APW-AV')

# gather elpds for all datasets
elpds_d = lapply(names(M), function(m) {
  
  d = elpd_diff(M[[m]],
                p = comps)
  
  d$data = m
  
  return(d)
  
}); elpds_d = data.frame(rbindlist(elpds_d))

elpds_d$comp = factor(elpds_d$comp,
                      levels = comps_n,
                      labels = comps_labs,
                      ordered = T)

# elpds_d = elpds_d[order(elpds_d$data, elpds_d$comp), ]

# Fig 5 top ---------------------------------------------------------------

cairo_pdf('analyses/figures_results/figs/Fig05.pdf',
          height = 10 * 0.393701,
          width = 12 * 0.393701,
          pointsize = 9)

layout(matrix(c(rep(1, 3),
                2,2,3,
                4,5,6,
                7,8,9, 
                rep(10, 3), 
                11,12,13),
              nrow = 6, byrow = T),
       heights = c(1, 1, .5, 5, 1, 5))

# row title
par(mar = c(0, 0, 0, 0))
plot(.5, .5,
     xlim = 0:1, ylim = 0:1,
     type = 'n',
     ylab = '', xlab = '',
     bty = 'n',
     xaxt = 'n', yaxt = 'n')
text(.5, .4, label = 'Model performance', font = 2,
     cex = 1.5)
# text(0, .8, label = 'a', font = 2,
#      cex = 1.2)

plot(.5, .5,
     xlim = 0:1, ylim = 0:1,
     type = 'n',
     ylab = '', xlab = '',
     bty = 'n',
     xaxt = 'n', yaxt = 'n')
text(.5, .4, label = 'Negative outcomes', font = 1,
     col = 'darkred',
     cex = 1.5)

plot(.5, .5,
     xlim = 0:1, ylim = 0:1,
     type = 'n',
     ylab = '', xlab = '',
     bty = 'n',
     xaxt = 'n', yaxt = 'n')
text(.5, .4, label = 'Positive outcomes', font = 1,
     col = 'darkgreen',
     cex = 1.5)

#
tts = c('Monetary losses', 'Monetary losses', 'Monetary gains')

for(i in 1:3) {
  
  plot(.5, .5,
       xlim = 0:1, ylim = 0:1,
       type = 'n',
       ylab = '', xlab = '',
       bty = 'n',
       xaxt = 'n', yaxt = 'n')
  text(.2, .4, label = tts[i], font = 1,
       col = ifelse(i < 3, 'darkred', 'darkgreen'),
       cex = 1,
       adj = c(0, .5))
  
}

par(mar = c(3, 3, 3, .5))

dns = c(p17 = 'Pachur et al., 2017', s16 = 'Suter et al., 2016', p14 = 'Pachur et al., 2014')

for(d in names(dns)) {
  
  dd = elpds[elpds$data == d, ]
  
  xx = 1.15 + .9 * 0:3
  
  # plot accuracies
  plot(x = xx,
       y = dd$ba_loo,
       ylim = c(.75, .95), 
       xlim = c(1, 4),
       yaxt = 'n', xaxt = 'n',
       xlab = '', 
       ylab = '',
       pch = 19,
       main = '',
       col = ifelse(d != 'p14', 'darkred', 'darkgreen'))
  
  if(d == 'p17') title(ylab = expression(accuracy[loo]), line = 2)
  
  segments(y0 = dd$ba_li, y1 = dd$ba_ui,
           x0 = xx,
           col = ifelse(d != 'p14', 'darkred', 'darkgreen'))
  
  axis(2, at = seq(.6, .95, .05))
  
  title(main = dns[d], line = 1)
  
  abline(h = seq(.6, .95, .01), 
         col = c(rgb(.5, .5, .5, .8),
                 rep(rgb(.5, .5, .5, .2), 4)),
         lwd = 1)
  
  axis(1, at = xx, labels = dd$mod)
  
}

# Fig 5 bottom -------------------------------------------------------------

# row title
par(mar = c(0, 0, 0, 0))
plot(.5, .5,
     xlim = 0:1, ylim = 0:1,
     type = 'n',
     ylab = '', xlab = '',
     bty = 'n',
     xaxt = 'n', yaxt = 'n')
text(.5, .4, label = 'Model performance comparison', font = 2,
     cex = 1.5)
# text(0, .8, label = 'b', font = 2,
#      cex = 1.2)

par(mar = c(5, 3, 2, .5))

for(d in unique(elpds_d$data)) {
  
  dd = elpds_d[elpds_d$data == d, ]
  
  xx =  1.1 + .95 * 0:5
  
  # plot elpd_diffr
  plot(x = xx,
       y = dd$elpd_d,
       ylim = c( min(0, min(dd$li)), max(dd$ui, 0)),
       xlim = c(1, 6),
       xaxt = 'n',
       xlab = '', 
       ylab = '',
       pch = 19,
       main = '',
       col = ifelse(d != 'p14', 'darkred', 'darkgreen'))
  
  if(d == 'p17') title(ylab = expression(Delta~elpd[loo]), line = 2)
  
  segments(y0 = dd$li, y1 = dd$ui,
           x0 = xx,
           col = ifelse(d != 'p14', 'darkred', 'darkgreen'))
  
  abline(v = xx[c(3, 4)] + .95/2,
         lty = 1)
  
  abline(h = 0, col = 'black', lty = 2)
  
  axis(1, at = xx, labels = gsub(' - .*', '', comps_labs),
       las = 2, padj = c(.5, .5))
  
  axis(3, at = c(xx[2], xx[4], xx[5] + .95/2), 
       labels = paste(unique(gsub('.* - ', '', comps_labs)), ''), 
       padj = c(1, 1), tick = F,
       line = 0)
  
}

dev.off()