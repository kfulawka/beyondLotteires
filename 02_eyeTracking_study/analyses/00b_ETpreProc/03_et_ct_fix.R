rm(list = ls())

# source('analyses/00_ETpreProc/02_fix_AOIs.R')
fixAOIs = readRDS('analyses/00b_ETpreProc/fixAOIs.rds')

# choice data -------------------------------------------------------------

# read pre-preapred choice task (ct) data
ct = read.table('data/ct_dat.txt', header = T)

# use this if the above file does not exist
# source('analyses/00_choice_dat_prep/01_choice_dat_prep.R')

# prepare data ------------------------------------------------------------

# insetr subject numbers into data with fixations
sub <- substr(names(fixAOIs), 1, 3) # vecotr with subjects' nums

for (i in 1:length(fixAOIs)) {
  for (j in c('poor', 'rich')) {
    fixAOIs[[i]][[j]]$sub <- as.numeric(sub[i])
    fixAOIs[[i]][[j]]$cond <- j
  }
}

# fixations
for (i in 1:length(fixAOIs)){
  fixAOIs[[i]] <- data.table::rbindlist( fixAOIs[[i]] )
}

fixes <- data.table::rbindlist(fixAOIs)

#fixes <- fixes[fixes$sub != 162, ]

# add info about fixation cross
fix_cr <- function(x) {
  if ((x['x'] > 1920/2-91.8 & x['x'] < 1920/2+91.8 & x['y'] > 1080/2-91.8 & x['y'] < 1080/2+91.8)) {
    return('+') 
  } else {
    return('o')  
  }
}
fixes$fixed_s <- as.character(fixes$fixed_s)
oo <- fixes[fixes$fixed_s == 'o', c('x', 'y')]
fixes$fixed_s[fixes$fixed_s == 'o'] <- apply(oo, 1, fix_cr)
rm(oo)

# filter out unclassidied fixations and fix to corss
fixes_c <- fixes[!fixes$fixed_s %in% c('o', '+'), ]

# scatter plots of fixations ----------------------------------------------

with(fixes[fixes$fixed_s!='o', ], plot(x, y, xlim = c(0, 1980), ylim = c(1080, 0), pch = '.', col = rgb(1, .1, .1, .1, .5)))
with(fixes[fixes$fixed_s=='o', ], points(x, y, xlim = c(0, 1980), ylim = c(1080, 0), pch = '.', col = rgb(.1, .1, 1, .5)))

source('analyses/00b_ETpreProc/02a_aois.R')

layout(matrix(1:4, nrow = 2))
for (j in 2) {
  for (i in 1:4) {
    plot(fixes_c$x[fixes_c$layout==j &fixes_c$layT==i], fixes_c$y[fixes_c$layout==j &fixes_c$layT==i]
         , pch = '.', xlim = c(0, 1920), ylim = c(1080, 0), col = rgb(.1, .1, 1, .3), xlab = 'x', ylab = 'y')
    rect(aois[,1,i,j], aois[,3,i,j], aois[,2,i,j], aois[,4,i,j], border = rgb(1, .1, .1, .7))
  }
}

# collapse consecutive fixations into one fixation ------------------------------------------------------

dw_temp <- fixes[, c('dur', 'f_num', 'fixed_s', 'CPNum', 'trialNum', 'cond', 'sub'), ]

## Use rle() and inverse.rle() to give each run of "for"s a distinct name
r <- rle(dw_temp$fixed_s)
r$values <- paste0(r$values, seq_along(r$values))
r <- inverse.rle(r)

## Use data.table to subset by run of "for"s *and* by id, collapsing only
## sub-data.tables consisting of consecutive "for"s within an id.
library(data.table)
dt <- data.table(dw_temp)

stims <- unique(dw_temp$fixed_s)

dwells <- dt[ , if(fixed_s[1] %in% stims) {
  X <- .SD[1,]       
  X$dur <- sum(dur) 
  X
} else {.SD}, 
by=list(r, sub, cond, CPNum)][,-1,with=FALSE]

# remove
rm(dt, dw_temp)

dwells <- dwells[!dwells$fixed_s %in% c('o', '+'), ]

# transitions -------------------------------------------------------------

dd <- dwells[, c('f_num', 'fixed_s', 'CPNum', 'trialNum', 'cond', 'sub'), ]

ct$betwP <- NA
ct$betwO <- NA
# ct$betwTot <- NA
ct$withA <- NA
ct$withB <- NA

for (s in unique(dd$sub)) {
  for (j in c('poor', 'rich')) {
    for (i in 0:64) {
      
      tt <- dd$fixed_s[dd$sub == s & dd$cond == j & dd$CPNum == i]
      
      ct$betwP[ct$sub == s & ct$cond == j & ct$CPNum == i] <- 0
      ct$betwO[ct$sub == s & ct$cond == j & ct$CPNum == i] <- 0
      # ct$betwTot[ct$sub == s & ct$cond == j & ct$CPNum == i] <- 0
      ct$withA[ct$sub == s & ct$cond == j & ct$CPNum == i] <- 0
      ct$withB[ct$sub == s & ct$cond == j & ct$CPNum == i] <- 0
      
      try(
        for (k in 2:length(tt)) {
          if(tt[k-1] %in% c('pa', 'xa', 'qa', 'ya') & tt[k] %in% c('pa', 'xa', 'qa', 'ya')) {
            ct$withA[ct$sub == s & ct$cond == j & ct$CPNum == i] = ct$withA[ct$sub == s & ct$cond == j & ct$CPNum == i] + 1
          }
          if(tt[k-1] %in% c('pb', 'xb', 'qb', 'yb') & tt[k] %in% c('pb', 'xb', 'qb', 'yb')) {
            ct$withB[ct$sub == s & ct$cond == j & ct$CPNum == i] = ct$withB[ct$sub == s & ct$cond == j & ct$CPNum == i] + 1 
          }
          if(tt[k-1] == 'xa' & tt[k] == 'xb' | tt[k-1] == 'xb' & tt[k] == 'xa' |
             tt[k-1] == 'ya' & tt[k] == 'yb' | tt[k-1] == 'yb' & tt[k] == 'ya') {
            ct$betwO[ct$sub == s & ct$cond == j & ct$CPNum == i] = ct$betwO[ct$sub == s & ct$cond == j & ct$CPNum == i] + 1          
          }
          if(tt[k-1] == 'pa' & tt[k] == 'pb' | tt[k-1] == 'pb' & tt[k] == 'pa' |
             tt[k-1] == 'qa' & tt[k] == 'qb' | tt[k-1] == 'qb' & tt[k] == 'qa') {
            ct$betwP[ct$sub == s & ct$cond == j & ct$CPNum == i] = ct$betwP[ct$sub == s & ct$cond == j & ct$CPNum == i] + 1          
          }
          # if(tt[k-1] %in% c('pa', 'xa', 'qa', 'ya') & tt[k] %in% c('pb', 'xb', 'qb', 'yb') |
          #    tt[k-1] %in% c('pb', 'xb', 'qb', 'yb') & tt[k] %in% c('pa', 'xa', 'qa', 'ya')) {
          #   ct$betwTot[ct$sub == s & ct$cond == j & ct$CPNum == i] = ct$betwTot[ct$sub == s & ct$cond == j & ct$CPNum == i] + 1
          # }
        }
      )
    }
  }
}

# fixations and dwells --------------------------------

# total inspection, number of...
# fix_summary <- function(x) {
#   c(m = mean(x), s = sum(x), n = length(x))
# }
# 
# #
# fix_agr <- do.call(data.frame
#                    , aggregate(dur ~ sub + cond + CPNum + fixed_s
#                                , data = fixes_c
#                                , FUN = fix_summary))

# dwells
dwell_summary <- function(x) {
  c(m = mean(x), s = sum(x), n = length(x))
}

#
dwell_agr <- do.call(data.frame
                     , aggregate(dur ~ sub + cond + CPNum + fixed_s
                                 , data = dwells
                                 , FUN = dwell_summary))
colnames(dwell_agr)[5:7] <- c('fix.m', 'dwell', 'fix.n')

# order data frame
dwell_agr <- dwell_agr[order(dwell_agr$sub, dwell_agr$cond, dwell_agr$CPNum, dwell_agr$fixed_s), ]

# into wide format (using data.table package)
dwell_agr_w <- dcast(setDT(dwell_agr), sub + cond + CPNum ~ fixed_s, value.var = c('fix.m', 'dwell', 'fix.n'))
dwell_agr_w[which(is.na(dwell_agr_w), arr.ind = T)] <- 0

# final fixation ----------------------------------------------------------

ct$fin_fix <- ''

for (s in unique(fixes_c$sub)) {
  for (j in c('poor', 'rich')) {
    for (i in 0:64) {
      tt <- fixes_c[fixes_c$sub == s & fixes_c$cond == j & fixes_c$CPNum == i, c('f_num', 'fixed_s')]
      try(ct$fin_fix[ct$sub == s & ct$cond == j & ct$CPNum == i] <-  tt$fixed_s[tt$f_num == max(tt$f_num)]
      )
    }
  }
}

# convert into option
ct$fin_fix_o = ifelse(ct$fin_fix %in% c('xa', 'pa', 'ya', 'qa'), 'A', 'B')

# merge ET data with choice task in wide format
df_w <- merge(ct, dwell_agr_w, by = c('sub', 'cond', 'CPNum'), all = T)

#
table(df_w$choice, df_w$fin_fix) / sum(table(df_w$choice, df_w$fin_fix))

# save final data file ----------------------------------------------------

write.table(df_w, 'data/df_ct_fix.txt', row.names = F, sep = ',')

rm(list = setdiff(ls(), 'df_w'))