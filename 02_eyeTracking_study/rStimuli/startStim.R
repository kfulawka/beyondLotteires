rm(list = ls())

dd <- read.table('C:/Users/kfulawka/Desktop/Psychologia/MPIB/ETstudy/Experiment/stim/SEgerm.txt'
                 , header = T
                 , sep = '\t'
                 , fill = T
                 , stringsAsFactors = F)

dd$nLenGe <- nchar(dd$SEnamGer)

dd <- dd[dd$SEnamGer != 'Vertigo', ]

# histogram of side effects names
hist(dd[, 'nLenGe'], xlab = 'number of characters in SE names'
     , main = '')

# number of SEs with less than 10 characters
table(dd[, 'nLenGe'] < 13)
# 87

# number of cases experienced by at least 20 people (almost 50% of raters)
# table(dd[, 'exp'] > 19)
# 47

# combined
# table(dd[, 'nLength'] < 11 & dd[, 'exp'] > 19)
# 37 cases...

# mean plot ---------------------------------------------------------------

# exp data
d <- dd[dd$nLenGe < 12, ]
d <- d[complete.cases(d), ]
d <- d[order(d$afect.mean, d$afect.median), ]
#d <- d[order(d$afect.median, d$afect.mean), ]
hist(d$afect.mean)
# 

ccls = c(rgb(1, .1, .1, .5), rgb(.5, .5, .5, .5), rgb(.1, .1, 1, .5))
nn = c('affect rich', 'excluded', ' affect poor')

boxplot(d$afect.mean[31:47], d$afect.mean[18:30], d$afect.mean[1:17]
        , ylim = c(1, 10)
        , names = nn
        , xlab = 'set'
        , staplewex = 0
        , border = ccls
        , ylab = 'mean affect'
        , main = 'Mean affect ratings of exp stimuli'
        )
for (i in 1:3) {
  points(jitter(rep(i,17), amount = .1), exd[,i]
         , col = ccls[i]
         , pch = 19)
}
t.test(exd[,1], exd[,3], var.equal = F)
var.test(exd[,1], exd[,3])
write.table(d, file = 'C:/Users/kfulawka/Desktop/Psychologia/MPIB/ETstudy/Experiment/stim/mARpilot.txt', sep = '\t', row.names = F)

nlength = cbind(d$nLenGe[1:17], d$nLenGe[31:47])

apply(nlength, 2, summary)
