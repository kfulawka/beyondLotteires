rm(list = ls())

source('results_figs/paired_boxplot.R')


# data --------------------------------------------------------------------

# exp design
ar_m = readRDS('results_figs/ar_m.rds')

# exp design --------------------------------------------------------------

pdf('results_figs/figs/Fig06.pdf',
    height = 5 * 0.393701,
    width = 16 * 0.393701,
    pointsize = 9)

cc = c(rgb(.1, 0, .7, alpha = .8), 
       rgb(.7, 0, .1, alpha = .8))

# layout 1, 3, 2, 3
layout(matrix(c(1, 1, 2, 2, 3, 4),
              byrow = F,
              nrow = 2))

# AR task -----------------------------------------------------------------

# AFFECT RATING TASK
par(mar = c(3,2,4,1))
plot(0,
     type = 'n',
     xlim = 0:1,
     ylim = 0:1,
     xlab = '',
     ylab = '',
     xaxt = 'n',
     yaxt = 'n',
     main = "Affect rating task")

text(.5, .8, 'How much would you be\nupset by this side effect?',
     cex = 1)

text(.5, .55, 'Insomnia',
     font = 2,
     cex = 1.5)

segments(x0 = .1, x1 = .9, y0 = .3, col = rgb(.5, 0, .5, .8))

text(c(.2, .8), .22, c('1 - not at all', '100 - extremely'),
     font = 2,
     cex = 1)

points(.8, .33, pch = 6, col = rgb(.5, 0, .5, .8))

text(.5, .1, '90', col = 'black',
     cex = 1.2,
     font = 2)

rect(.42, .05, .58, .15, border = rgb(.5, 0, .5, .8))

mtext('a',
      side = 3,
      line = 2.75,
      adj = -.05,
      col = 'black',
      font = 2)

# AFFECT RATINGS ----------------------------------------------------------

par(mar = c(4,4,4,1))

# exp manipulation
paired_boxplot(ar_m,
               box_col = cc,
               point_col = c(rgb(0, 0, .5, .2),
                             rgb(.5, 0, 0, .2)),
               line_col = rgb(.5, 0, .5, .2),
               yl = c('Mean individual affect ratings'),
               xl = 'Condition',
               ylim = c(1, 100),
               title = 'Affect ratings',
               x_labs = c('Affect-poor', 'Affect-rich'))

mtext('b',
      side = 3,
      line = 2.75,
      adj = -.2,
      col = 'black',
      font = 2)

# CHOICE TASK -------------------------------------------------------------

# figures' margins
par(mar = c(2,1,3,1))

plot(0,
     type = 'n',
     xlim = 0:1,
     ylim = 0:1,
     xlab = '',
     ylab = '',
     xaxt = 'n',
     yaxt = 'n',
     bty = 'n')
title("Affect-poor choice task", 
      line = 1,
      col.main = cc[1])

text(0, 1, 'Drug A (safe)',
     col = cc[1],
     adj = c(0, 1))
rect(0, 0, .475, .8, 
     border = cc[1])

text(.01, 
     c(.67, .45, .23), 
     c('Side effect', 'Cough', 'No cough'),
     col = c(cc[1], 'black', 'black'),
     adj = c(0, 1),
     cex = .8)

text(.465, 
     c(.67, .45, .23), 
     c('Probability', '1%', '99%'),
     col = c(cc[1], 'black', 'black'),
     adj = c(1, 1),
     cex = .8)


text(.525, 1, 'Drug B (risky)',
     col = cc[1],
     adj = c(0, 1))
rect(.525, 0, 1, .8, 
     border = cc[1])

text(.535, 
     c(.67, .45, .23), 
     c('Side effect', 'Fever', 'No fever'),
     col = c(cc[1], 'black', 'black'),
     adj = c(0, 1),
     cex = .8)


text(.99, 
     c(.67, .45, .23), 
     c('Probability', '0.01%', '99.99%'),
     col = c(cc[1], 'black', 'black'),
     adj = c(1, 1),
     cex = .8)

# mtext('Drugs equally effective',
#       side = 1,
#       line = 0,
#       at = .5,
#       adj = c(.5, .5),
#       col = 'black',
#       cex = .5)

mtext('c',
      side = 3,
      line = 1.75,
      adj = 0,
      col = 'black',
      font = 2)

# figures' margins
par(mar = c(3,1,2,1))

# Affect-rich
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
title("Affect-rich choice task", 
      line = 1,
      col.main = cc[2])

text(0, 1, 'Drug A (safe)',
     col = cc[2],
     adj = c(0, 1))
rect(0, 0, .475, .8, 
     border = cc[2])

text(.01, 
     c(.67, .45, .23), 
     c('Side effect', 'Insomnia', 'No insomnia'),
     col = c(cc[2], 'black', 'black'),
     adj = c(0, 1),
     cex = .8)

text(.465, 
     c(.67, .45, .23), 
     c('Probability', '1%', '99%'),
     col = c(cc[2], 'black', 'black'),
     adj = c(1, 1),
     cex = .8)


text(.525, 1, 'Drug B (risky)',
     col = cc[2],
     adj = c(0, 1))
rect(.525, 0, 1, .8, 
     border = cc[2])

text(.535, 
     c(.67, .45, .23), 
     c('Side effect', 'Blood clots', 'No blood clots'),
     col = c(cc[2], 'black', 'black'),
     adj = c(0, 1),
     cex = .8)


text(.99, 
     c(.67, .45, .23), 
     c('Probability', '0.01%', '99.99%'),
     col = c(cc[2], 'black', 'black'),
     adj = c(1, 1),
     cex = .8)

# mtext('Side effects matched between conditions\nwith within-person ranks',
#       side = 1,
#       line = .9,
#       at = .5,
#       adj = c(.5, .5),
#       col = 'black',
#       cex = .5,
#       font = 1)


# save fig ----------------------------------------------------------------


dev.off()

# eye tracking res --------------------------------------------------------

# function for aggregation
agr_fun = function(x) median(x, na.rm = T)

# probably hierarchically estimated means would be best...

# data --------------------------------------------------------------------

# wide format
df_w = read.csv('analyses/00c_data_analyses/df_swapped.csv')

# colnames with subject an trial info
info <- colnames(df_w)[c(1:5, 8, 11, 22, 23)]

# measures of attention: total dwell per trial -------------------------------------------------------

# colnames with total dwells
dwells_s <- grep('^dwell', colnames(df_w), value = T)

# relevant columns
dwell <- df_w[, c(info, dwells_s)]

# relative dwells within trial
dwell_rel <- as.data.frame(t(apply(dwell[,dwells_s], 1, FUN = function(x) x / sum(x))))
colnames(dwell_rel) <- paste(dwells_s, 'rel', sep = '_')

# merge
dwell <- cbind(dwell, dwell_rel)
rm(dwell_rel)

dwells_s <- grep('^dwell_.*_rel', colnames(dwell), value = T)

# descriptive plots: raw data --------------------------------------------

# function to convert psychopy coordinate system to 0-1 system
psyConv <- function(x) x/2 + .5

# coordinates of horizontal layout
xc <- psyConv(c(-.65, -.3, .3, .65))
yc <- psyConv(c(-.3, .3))

# screen parameters
x_s <- 1920
y_s <- 1080

# stimulus coordinates (in pixels)
cors <- list(x = xc * x_s, y = yc * y_s)

agr_fsum <- do.call(data.frame, aggregate(
  . ~ cond,
  # ordering below is important, always check!!!!
  data = dwell[,c(dwells_s[c(5, 1, 7, 3, 6, 2, 8, 4)], 'cond')], 
  FUN = function(x) c(m = agr_fun(x))))

# size of points
p_size = sqrt(agr_fsum[,2:9]/pi) * 10 # for relative probs
# p_size = sqrt((agr_fsum[,2:9]/20)/pi) # for miliseconds

# titles
tt = c('Affect-poor', 'Affect-rich')

# colors
cc = c(rgb(.1, .1, 1, .5), rgb(1, .1, .1, .5))

pdf('results_figs/figs/et_exp_et.pdf',
    height = 5 * 0.393701,
    width = 16 * 0.393701,
    pointsize = 8)

layout(matrix(1:2, ncol = 2))
par(mar = c(1, 1, 3, .2))

for(i in 1:2) {
  
  plot(i, type = 'n'
       , xlab = ''
       , ylab = ''
       , main = tt[i]
       , xaxt = 'n'
       , yaxt = 'n'
       , ylim = c(1080, 0), xlim = c(0, 1920))
  
  points(x = cors$x, y = rep(cors$y[1], 4), 
         cex = c(p_size[i,1], p_size[i,2], p_size[i,3], p_size[i,4])
         , col = cc[i], pch = 19)
  points(x = cors$x, y = rep(cors$y[2], 4), 
         cex = c(p_size[i,5], p_size[i,6], p_size[i,7], p_size[i,8])
         , col = cc[i], pch = 19)
  
  text(c(100, 100), cors$y, c('A', 'B'))
  text(cors$x, rep(200, 4), c('SE', 'p(SE)', 'no SE', 'p(no SE)'))
  
  text(cors$x, rep(500, 4), round(agr_fsum[i,2:5], 2))
  text(cors$x, rep(844, 4), round(agr_fsum[i,6:9], 2))
  
  segments(x0 = rep(170, 4), x1 = rep(1700, 4),
           y0 = c(320, 550, 650, 900))
  
  segments(x0 = c(170, 1700, 170, 1700),
           y0 = c(320, 320, 650, 650), y1 = c(550, 550, 900, 900))
  
  
}

dev.off()

