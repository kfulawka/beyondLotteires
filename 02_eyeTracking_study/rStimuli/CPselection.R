# script for choice porblems random generation
rm(list = ls())

# initial set -------------------------------------------------------------
A <- rep(1:17, times = 20)
B <- rep(1:17, times = 20)

# all posibble combinations
AB <- expand.grid(A = A, B = B)

# option A has always worse side effect
AB <- AB[AB[,'A'] > AB[,'B'], ]

# insert probabilities
# beta: 1.066945, 21.998621
# lnorm: -3.588606, 1.02898
AB$pA <- round( rlnorm(nrow(AB), -3.588606, 1.02898), 3)
AB$pB <- round( rlnorm(nrow(AB), -3.588606, 1.02898), 3)

# filter out probabilites higher than 1
AB <- AB[AB$pA < 1, ]
AB <- AB[AB$pB < 1, ]

# A should always have lower probability to avoid dominance
AB <- AB[AB[,'pA'] < AB[,'pB'], ]

# probabilities of 'nothing'
AB$qA <- 1 - AB$pA
AB$qB <- 1 - AB$pB


# combine with ARs from pilot study ---------------------------------------

d <- read.table('C:/Users/kfulawka/Desktop/Psychologia/MPIB/ETstudy/Experiment/stim/mARpilot.txt'
                , sep = '\t'
                , header = T)

d$rank <- c(1:17, 1:13, 1:17)
d$set <- c(rep(0, 17), rep(1, 13), rep(2, 17))

# 
AB$Ah <- NA
AB$Bh <- NA
AB$Al <- NA
AB$Bl <- NA

for (i in 1:nrow(AB)) {
  
  # affect rich
  AB$Ah[i] <- d$afect.mean[d$rank == AB$A[i] & d$set == 2] * -1
  AB$Bh[i] <- d$afect.mean[d$rank == AB$B[i] & d$set == 2] * -1
  
  # affect poor
  AB$Al[i] <- d$afect.mean[d$rank == AB$A[i] & d$set == 0] * -1
  AB$Bl[i] <- d$afect.mean[d$rank == AB$B[i] & d$set == 0] * -1

}

# filter out CPs were medians are equal
# AB = AB[AB$Ah != AB$Bh & AB$Al != AB$Bl, ]

# selection based on EVs and minimax --------------------------------------

# compute EVs
AB$EVAh <- AB$Ah * AB$pA
AB$EVBh <- AB$Bh * AB$pB

AB$EVAl <- AB$Al * AB$pA
AB$EVBl <- AB$Bl * AB$pB

# select only similar problems

# firs, compute ratios
for (i in 1:nrow(AB)) {
  
  # affect rich
  # AB$EVrH[i] <- abs(AB$EVAh[i] - AB$EVBh[i]) / min(abs(AB$EVAh[i]), abs(AB$EVBh[i]))
  AB$EVrH[i] <- min(AB$EVAh[i], AB$EVBh[i]) / max(AB$EVAh[i], AB$EVBh[i])
  
  # affect poor
  # AB$EVrL[i] <- abs(AB$EVAl[i] - AB$EVBl[i]) / min(abs(AB$EVAl[i]), abs(AB$EVBl[i]))
  AB$EVrL[i] <- min(AB$EVAl[i], AB$EVBl[i]) / max(AB$EVAl[i], AB$EVBl[i])
  
}

# select CPs where EV ratios are lower than 1
AB <- AB[AB$EVrH < 2 & AB$EVrL < 2, ]

# finally, select only problems where EV and minimax predicitons differ
# minmax always predicts B choice, so only EVA > EVB need to be filtered out to meet this criterion

AB <- AB[AB$EVAh > AB$EVBh & AB$EVAl > AB$EVBl, ]

# additional steps to make the setes between condtions more simialr -------

# # select only choice problems where EV ratios are similar
# AB <- AB[round(AB$EVrH, 1) == round(AB$EVrL, 1), ]
# 
# # histogram of ranks
# hist(c(AB$A, AB$B), main = 'number of ranks in the AB set', xlab = 'rank')
# 
# #
# boxplot(AB$EVrH, AB$EVrL, names = c('rich', 'poor'))
# 
# # variance check
# var.test(AB$Al-AB$Bl, AB$Ah-AB$Bh)
# var.test(AB$EVrH, AB$EVrL)

# random set of 60 choice problems + 5 dominated ----------------------------------------

#AB2 <- AB[AB$A - AB$B < 8, ]

CP <- AB[sample(1:nrow(AB), 60), ]

CP[61, ] <- c(17, 1, .1, .05, .9, .95, 60, 0)
CP[62, ] <- c(15, 3, .153, .091, .847, .909, 61, 0)
CP[63, ] <- c(16, 2, .055, .023, .945, .977, 62, 0)
CP[64, ] <- c(14, 8, .032, .004, .968, .996, 63, 0)
CP[65, ] <- c(9, 4, .048, .015, .952, .985, 64, 0)

for (i in 1:nrow(CP)) {
  
  # affect rich
  CP$Ah[i] <- d$afect.mean[d$rank == CP$A[i] & d$set == 2] * -1
  CP$Bh[i] <- d$afect.mean[d$rank == CP$B[i] & d$set == 2] * -1
  
  # affect poor
  CP$Al[i] <- d$afect.mean[d$rank == CP$A[i] & d$set == 0] * -1
  CP$Bl[i] <- d$afect.mean[d$rank == CP$B[i] & d$set == 0] * -1
  
}

# compute EVs
CP$EVAh <- CP$Ah * CP$pA
CP$EVBh <- CP$Bh * CP$pB

CP$EVAl <- CP$Al * CP$pA
CP$EVBl <- CP$Bl * CP$pB

# select only similar problems

# firs, compute ratios
for (i in 1:nrow(CP)) {
  
  # affect rich
  # CP$EVrH[i] <- CPs(CP$EVAh[i] - CP$EVBh[i]) / min(CPs(CP$EVAh[i]), CPs(CP$EVBh[i]))
  CP$EVrH[i] <- min(CP$EVAh[i], CP$EVBh[i]) / max(CP$EVAh[i], CP$EVBh[i])
  
  # affect poor
  # CP$EVrL[i] <- CPs(CP$EVAl[i] - CP$EVBl[i]) / min(CPs(CP$EVAl[i]), CPs(CP$EVBl[i]))
  CP$EVrL[i] <- min(CP$EVAl[i], CP$EVBl[i]) / max(CP$EVAl[i], CP$EVBl[i])
  
}

# histogram of ranks
hist(c(CP$A, CP$B), main = 'number of ranks in the CP set', xlab = 'rank')

#
boxplot(CP$EVrH, CP$EVrL, names = c('rich', 'poor'))

# variance check
var.test(CP$Al-CP$Bl, CP$Ah-CP$Bh)
var.test(CP$EVrH, CP$EVrL)

write.table(CP, file = 'C:/Users/kfulawka/Desktop/Psychologia/MPIB/ETstudy/Experiment/stim/CPsetPars.txt', sep = '\t', row.names = F)

# this set will be used with py scripts
CPs = CP[,1:6]

# convert to percentages
CPs[,3:6] <- CPs[,3:6] * 100
for( i in 3:6) {
  CPs[,i] <- paste(CPs[,i], '%', sep = '')
}
CPs$CPnum <- 0:(nrow(CP)-1) # Cp number
CPs$swap = 0 # swap indexing
write.table(CPs, file = 'C:/Users/kfulawka/Desktop/Psychologia/MPIB/ETstudy/Experiment/stim/CPset.txt'
            , sep = '\t', row.names = F, col.names = F
            , quote = F)
