rm(list = ls())

source('results_figs/paired_boxplot.R')
library(brms)

# data --------------------------------------------------------------------

dd = read.csv('analyses/00c_data_analyses/df.csv')
dwells = grep('dwell', colnames(dd), value = T)
# dependent var
dd$dwell_P = (dd$dwell_pa + dd$dwell_qa + dd$dwell_pb + dd$dwell_qb) / rowSums(dd[,dwells])

dp_a = aggregate(dwell_P ~ cond + sub,
                 FUN = median,
                 data = dd)
dp_a = cbind(dp_poor = dp_a$dwell_P[dp_a$cond == 'poor'],
             dp_rich = dp_a$dwell_P[dp_a$cond == 'rich'])

# choice proportions
co_m = readRDS('results_figs/co_cp.rds')
co_mod = readRDS('results_figs/co_cp_mod.rds')

# attention and affect
m_ar_att = readRDS('results_figs/m_ar_p_att.rds')
m_cond_att = readRDS('results_figs/m_cond_sgi_att.rds')
ma_arAt = readRDS('results_figs/ma_arAt.rds')

# attention and pwf
# gam_atp = readRDS('results_figs/gam_atp.rds')
gam_atp = readRDS('results_figs/gam_cond_atp.rds')

# cpt
pt = readRDS('analyses/02b_choice_modeling/mods_gam_phi.rds')
pt_pars = pt$cpt_vcov_gt1$pars
igam = sapply(pt_pars$i_pars[1:2], function(x) apply(x, 2, median))

# POP-LVL PARAMETERS VALS AND COMPS
pgam = sapply(pt_pars$p_pars[1:2], median)
round(sapply(pt_pars$p_pars[1:2], quantile, probs = c(.5, .025, .975)), 2)

gam_d = pt_pars$p_pars$mu_gam_ap  - pt_pars$p_pars$mu_gam_ar
gam_d = round( quantile(gam_d, c(.5, .025, .975)), 2)

N = length(unique(dd$sub))


# model performance comp --------------------------------------------------

sapply(pt, function(x) x$perf$ba$ba)
ll = lapply(pt, function(x) x$perf$looE)

# APW v AV
r = loo_compare(ll[1:2])
r[2] + r[4] * c(-1.96, 1.96) 

# 2PWF v AV
r = loo_compare(ll[c(1,3)])
r[2] + r[4] * c(-1.96, 1.96) 

# plot set up -------------------------------------------------------------
cc = c(rgb(.1, 0, .7, .2), 
       rgb(.7, 0, .1, .2))
cc_p = c(rgb(.1, 0, .7, .8), 
         rgb(.7, 0, .1, .8))

pdf('results_figs/figs/Fig07.pdf',
    height = 5.5 * 0.393701,
    width = 16 * 0.393701,
    pointsize = 9)

par(mar = c(4, 4, 4, 1),
    mfrow = c(1, 3))

# PWFs --------------------------------------------------------------------

pwf = function(x, g, d = 1) d*x^g / (d*x^g + (1-x)^g) 

for(j in 1:2) {
  for(i in 1:N) {
  
    curve(pwf(x, g = igam[i,j], d = 1),
          from = .004, to = .1,
          n = 5e2,
          col = cc[j],
          # ylim = 0:1,
          ylim = c(.001, .7),
          log = 'xy',
          add = ifelse(i == 1 & j == 1, F, T),
          main = 'Probability weighting functions',
          xlab = 'p',
          ylab = 'w(p)')
    
  }
  
  curve(pwf(x, g = pgam[j], d = 1),
        from = .004, to = .1,
        n = 5e2,
        col = cc_p[j],
        lwd = 2,
        # ylim = 0:1,
        ylim = c(.001, .7),
        log = 'xy',
        add = ifelse(i == 1 & j == 1, F, T))
  
}


# legends
legend(.0035, .005, 
       legend = c('Individual', 'Group-level'),
       lwd = 1:2,
       col = 'black',
       bty = 'n')


# legends
legend(.02, .005, 
       # title = 'Condition',
       legend = c('Affect-poor', 'Affect-rich'),
       lwd = 2,
       col = cc_p,
       bty = 'n')

mtext('a',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)

# pwfs box ----------------------------------------------------------------

# exp manipulation
paired_boxplot(igam,
               box_col = cc,
               point_col = c(rgb(0, 0, .5, .2),
                             rgb(.5, 0, 0, .2)),
               line_col = rgb(.5, 0, .5, .1),
               yl = expression(gamma['ind']),
               xl = 'Condition',
               ylim = c(0, 1),
               title = 'Probability sensitivity',
               # title = 'PWF parameter differences',
               x_labs = c('Affect-poor', 'Affect-rich'))
print(gam_d)
title(main = expression(Delta[gamma]=='0.22, 95%CI: [0.15, 0.30]'),
      font.main = 1, cex.main = 1,
      line = .7)

mtext('b',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)


# risky drug choice -------------------------------------------------------

paired_boxplot(co_m,
               box_col = cc,
               point_col = c(rgb(0, 0, .5, .2),
                             rgb(.5, 0, 0, .2)),
               line_col = rgb(.5, 0, .5, .2),
               yl = c('Proportion of individuals'),
               xl = 'Condition',
               ylim = c(0, 1),
               title = 'Risky drug choice proportions',
               x_labs = c('Affect-poor', 'Affect-rich'))

# predicted probs
cond_co = posterior_epred(co_mod,
                          re_formula = NA,
                          newdata = data.frame(cond = c('poor', 'rich'),
                                               sub = NA,
                                               CPNum = NA))
apply(cond_co, 2, quantile, probs = c(.5, .025, .95))

d_cond_co = round(quantile(cond_co[,1] - cond_co[,2], c(.5, .025, .975)), 2)
print(d_cond_co)

title(main = expression(Delta['P(Risky Drug)']== '0.19, 95%CI: [0.14, 0.24]'),
      font.main = 1, cex.main = 1,
      line = .7)

mtext('c',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)

dev.off()

# attention X cond --------------------------------------------------------

pdf('results_figs/figs/Fig08.pdf',
    height = 5.5 * 0.393701,
    width = 16 * 0.393701,
    pointsize = 9)

layout(matrix(c(1,1,2,3,4,4), nrow = 2))

par(mar = c(4, 4, 4, 1))

cond_at_agr = conditional_effects(m_cond_att)[[1]]
cond_at_id = conditional_effects(m_cond_att,
                                 re_formula = NULL,
                                 conditions = data.frame(sub = rep(1:N, each = 2),
                                                         cond = c('poor', 'rich')))[[1]]
cond_at_id = cond_at_id[, c('sub', 'cond', 'estimate__', 'lower__', 'upper__')]
cond_at_id = unique(cond_at_id) # brms bug fix...
colnames(cond_at_id)[3:5] = c('y', 'l', 'u')

# dwell on p
paired_boxplot(cbind(cond_at_id$y[cond_at_id$cond=='poor'],
                     cond_at_id$y[cond_at_id$cond=='rich']),
               box_col = cc,
               point_col = c(rgb(0, 0, .5, .1),
                             rgb(.5, 0, 0, .1)),
               line_col = rgb(.5, 0, .5, .1),
               yl = 'Individual dwell on probabilities',
               xl = 'Condition',
               ylim = c(.1, .6),
               title = 'Attention to P(SE) ~ Affect(Condition)',
               x_labs = c('Affect-poor', 'Affect-rich'))

# predicted probs
cond_at = posterior_epred(m_cond_att,
                          re_formula = NA,
                          newdata = data.frame(cond = c('poor', 'rich'),
                                               sub = NA))
d_cond_at = round(quantile(cond_at[,1] - cond_at[,2], c(.5, .025, .975)), 2)
print(d_cond_at)
title(main = expression(Delta['dwell'] == '-0.03, 95%CI: [-0.05, -0.01]'),
      font.main = 1, cex.main = 1,
      line = .7)

# add the agr estimates
points(x = c(.75, 2.25), y = cond_at_agr$estimate__,
       col = cc_p,
       pch = 19)

segments(x0 = c(.75, 2.25), y0 = cond_at_agr$lower__, y1 = cond_at_agr$upper__,
         col = cc_p)

mtext('a',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)

# attention and affect ind ------------------------------------------------

# ind cond effects
ind_cond = coef(m_cond_att)[[1]]
ind_cond = ind_cond[,,2]

# ind affect effects
ind_ar = coef(m_ar_att)[[1]]
ind_ar = ind_ar[,,2]

sord = order(ind_cond[,1])

par(mar = c(.5,4,4,2))

plot(1:N, ind_cond[sord,1]*-1,
     ylim = c(min(ind_cond*-1), max(ind_cond*-1)),
     # ylim = c(-1.2, 1),
     xlim = c(1, N),
     pch = 12,
     col = rgb(.1, .2, .5, .2),
     xaxt = 'n',
     ylab = '',
     xlab = '',
     main = 'Attention to P(SE) ~ Affect:\nwithin-individual effects')
title(ylab = expression(beta), 
      line = 2)
# axis(1, c(1, seq(10, 60, 10), N), labels = F)

abline(h = 0, lty = 2, lwd = 1)

segments(x0 = 1:N, 
         y0 = ind_cond[sord,3]*-1,
         y1 = ind_cond[sord,4]*-1,
         col = rgb(.1, .2, .5, .6))

# LEGEND
legend(32, 1, 
       legend = c('Affect(Condition)'),
       # title = 'Effect',
       bty = 'n',
       col = rgb(.1, .2, .5, .8),
       pch = 12, 
       cex = 1)
round(fixef(m_cond_att),2)
text(1, -.9, labels = expression(mu[beta]=='-0.1, 95%CI: [-0.18, -0.01]'),
     adj = c(0, .5))

mtext('b',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)

par(mar = c(4,4,.5,2))

plot(1:N, ind_ar[sord,1],
     ylim = c(min(ind_ar), max(ind_ar)),
     # ylim = c(-1.1, 1),
     xlim = c(1, N),
     pch = 6,
     col = rgb(.1, .2, .5, .6),
     xaxt = 'n',
     xlab = 'Participant',
     ylab = '')
axis(1, c(1, seq(10, 60, 10), N))
title(ylab = expression(beta), 
      line = 2)

points(1:N, ind_ar[sord,1],
       pch = 6,
       col = rgb(.6, .2, .2, .2))

segments(x0 = 1:N, 
         y0 = ind_ar[sord,3],
         y1 = ind_ar[sord,4],
         col = rgb(.6, .2, .2, .6))

abline(h = 0, lty = 2, lwd = 1)
round(fixef(m_ar_att),2)
text(1, -.5, labels = expression(mu[beta]=='-0.05, 95%CI: [-0.1, -0.01]'),
     adj = c(0, .5))

# LEGEND
legend(32, .45, 
       legend = 'Affect(SE)',
       # title = 'Effect',
       bty = 'n',
       col = rgb(.6, .2, .2, .8),
       pch = 6, 
       cex = 1)

# mtext('b',
#       side = 3,
#       line = 0,
#       adj = -.2,
#       col = 'black',
#       font = 2)

# attention and affect ----------------------------------------------------

# ind var
af = grep('af', colnames(dd), value = T)
dd$ar_max = apply(dd[,af[5:6]], 1, max)

# arg dwell-to-p ~ affect_rating
dd$ar_max_b = cut(dd$ar_max, 
                  breaks = seq(0, 100, 4),
                  ordered_result = T)
dd$ar_max_b = as.numeric(dd$ar_max_b) * 4

aa = aggregate(dwell_P ~ ar_max_b, 
               data = dd,
               FUN = function(x) c(m = mean(x), 
                                   n = length(x), 
                                   sd = sd(x)))
aa = do.call(data.frame, aa)
aa$ui = aa$dwell_P.m + aa$dwell_P.sd / sqrt(aa$dwell_P.n)
aa$li = aa$dwell_P.m - aa$dwell_P.sd / sqrt(aa$dwell_P.n)

#
par(mar = c(4, 4, 4, 1))

plot(aa$ar_max, aa$dwell_P.m,
     pch = 19,
     ylim = c(min(aa$li, na.rm = T), max(aa$ui, na.rm = T)),
     xlim = c(1, 100),
     col = rgb(.1, .1, .7, .2),
     ylab = 'Mean dwell on probabilities',
     xlab = 'Affect rating [binned]',
     main = 'Attention to P(SE) ~ Affect(SE)',
     cex = sqrt(aa$dwell_P.n) * .2)

round(brms::bayes_R2(ma_arAt),2)
title(main = expression('R'^'2'== '0.35, 95%CI: [0.08, 0.54]'),
      font.main = 1, cex.main = 1,
      line = .7)

# cor.test(aa$ar_max, aa$dwell_P.m, method = 's')

segments(x0 = aa$ar_max,
         y0 = aa$li, y1 = aa$ui,
         col = rgb(.1, .1, .7, .3))

# add CIs
cis = conditional_effects(ma_arAt,
                          effects = 'ar_max_b')[[1]]

lines(cis$ar_max_b, cis$estimate__, 
      col = rgb(.5, .1, .9, 1),
      lwd = 2)

polygon(c(cis$ar_max_b, rev(cis$ar_max_b)), c(cis$lower__, rev(cis$upper__)),
        col = rgb(.5, .1, .9, .2),
        border = NA)

mtext('c',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)

# save figure -------------------------------------------------------------

dev.off()

# prob sensitivity x dwell ------------------------------------------------

plot(igam[,1], cond_at_id$y[cond_at_id$cond=='poor'],
     col = cc[1],
     xlim = 0:1,
     ylim = c(.1, .6),
     pch = 19,
     ylab = 'Individual dwell on probabilities',
     xlab = expression(gamma['ind']),
     main = 'Attention to P(SE) ~ P. Sensitivity')

round(brms::bayes_R2(gam_atp, re_form = NA), 2)
title(main = expression('R'^'2'== '0.07, 95%CI: [0.02, 0.14]'),
      font.main = 1, cex.main = 1,
      line = .7)

points(igam[,2],
       cond_at_id$y[cond_at_id$cond=='rich'],
       col = cc[2],
       pch = 19)

# add CIs
cis = conditional_effects(gam_atp,
                          conditions = data.frame(cond = NA),
                          effects = 'gam')[[1]]

lines(cis$gam, cis$estimate__,
      col = rgb(.6, .1, .6, 1),
      lwd = 2)

polygon(c(cis$gam, rev(cis$gam)), c(cis$lower__, rev(cis$upper__)),
        col = rgb(.6, .1, .6, .2),
        border = NA)

mtext('a',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)

# # attention and affect ind ------------------------------------------------
# 
# # ind cond effects
# ind_cond = coef(m_cond_att)[[1]]
# ind_cond = ind_cond[,,2]
# 
# # ind affect effects
# ind_ar = coef(m_ar_att)[[1]]
# ind_ar = ind_ar[,,2]
# 
# sord = order(ind_cond[,1])
# 
# plot(1:N - .25, ind_cond[sord,1]*-1,
#      ylim = c(min(ind_cond*-1), max(ind_cond*-1)),
#      xlim = c(1, N),
#      pch = 12,
#      col = rgb(.1, .2, .5, .6),
#      xaxt = 'n',
#      xlab = 'Participant',
#      ylab = 'Regression weight (logit)',
#      main = 'Within-indvidual effects of\naffect on P(SE) attention ')
# axis(1, c(1, seq(10, 60, 10), N))
# 
# segments(x0 = 1:N - .25, 
#          y0 = ind_cond[sord,3]*-1,
#          y1 = ind_cond[sord,4]*-1,
#          col = rgb(.1, .2, .5, .6))
# 
# points(1:N+.25, ind_ar[sord,1],
#        pch = 6,
#        col = rgb(.6, .2, .2, .6))
# 
# segments(x0 = 1:N + .25, 
#          y0 = ind_ar[sord,3],
#          y1 = ind_ar[sord,4],
#          col = rgb(.6, .2, .2, .6))
# 
# abline(h = 0, lty = 2, lwd = 2)
# 
# # LEGEND
# legend(35, 1.2, 
#        legend = c('Exp condition', 'Affect rating'),
#        title = 'Effect',
#        bty = 'n',
#        col = c(rgb(.1, .2, .5, .8), rgb(.6, .2, .2, .8)),
#        pch = c(12, 6), 
#        cex = 1)
# 