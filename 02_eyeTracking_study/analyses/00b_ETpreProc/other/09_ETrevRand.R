load("C:/Users/kfulawka/Desktop/Psychologia/MPIB/ETstudy/data/ETdata/etRawClean.RData")

# mapping: horizontal -----------------------------------------------------

clasv = ifelse(gd_m[,'xav'] > .05 & gd_m[,'xav'] < .25, 1
               , ifelse(gd_m[,'xav'] > .25 & gd_m[,'xav'] < .5, 2
                        , ifelse(gd_m[,'xav'] > .5 & gd_m[,'xav'] < .75, 3, 4)))

# x's up
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 0 & cc$xp == 1 & clasv == 2] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 0 & cc$xp == 1 & clasv == 2] -.25
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 0 & clasv == 3] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 0 & clasv == 3] -.5
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 1 & clasv == 4] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 1 & clasv == 4] -.75

clasv = ifelse(gd_m[,'xav'] > .05 & gd_m[,'xav'] < .25, 1
               , ifelse(gd_m[,'xav'] > .25 & gd_m[,'xav'] < .5, 2
                        , ifelse(gd_m[,'xav'] > .5 & gd_m[,'xav'] < .75, 3, 4)))
# prob's
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 0 & cc$xp == 1 & clasv == 1] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 0 & cc$xp == 1 & clasv == 1] +.25
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 0 & clasv == 3] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 0 & clasv == 3] -.25
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 1 & clasv == 4] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 1 & clasv == 4] -.5

clasv = ifelse(gd_m[,'xav'] > .05 & gd_m[,'xav'] < .25, 1
               , ifelse(gd_m[,'xav'] > .25 & gd_m[,'xav'] < .5, 2
                        , ifelse(gd_m[,'xav'] > .5 & gd_m[,'xav'] < .75, 3, 4)))

# y's
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 0 & cc$xp == 1 & clasv == 1] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 0 & cc$xp == 1 & clasv == 1] +.5
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 0 & clasv == 2] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 0 & clasv == 2] +.25
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 1 & clasv == 4] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 1 & clasv == 4] -.25

clasv = ifelse(gd_m[,'xav'] > .05 & gd_m[,'xav'] < .25, 1
               , ifelse(gd_m[,'xav'] > .25 & gd_m[,'xav'] < .5, 2
                        , ifelse(gd_m[,'xav'] > .5 & gd_m[,'xav'] < .75, 3, 4)))

# q's
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 0 & cc$xp == 1 & clasv == 1] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 0 & cc$xp == 1 & clasv == 1] +.75
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 0 & clasv == 2] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 0 & clasv == 2] +.5
gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 1 & clasv == 3] <- gd_m[,'xav'][cc$lay == 'horizontal' & cc$xy == 1 & cc$xp == 1 & clasv == 3] +.25


smoothScatter(gd_m[,'xav'][cc$lay == 'horizontal'], gd_m[,'yav'][cc$lay == 'horizontal']
              , xlim = c(0, 1)
              , ylim = c(1, 0)
              , col = rgb(.7, .1, .1, .5)
              , xlab = 'x', ylab = 'y', main = 'average'
              , nbin = 500
              , nrpoints = 1000)
segments(x0 = 0, x1 = 1, y0 = yc, col = 'red')
segments(x0 = xc, y0 = 0, y1 = 1, col = 'red')
