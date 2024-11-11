rm(list = ls())

mods <- readRDS("S:/arc_research/Affect_prob_ET/analyses/02b_choice_modeling/mods_posteriors.rds")

llp = sapply(mods, function(m) m$performance$AP$looE$estimates[1,])
llp = rbind(llp, li = llp[1,] - 1.96*llp[2,])
llp = rbind(llp, ui = llp[1,] + 1.96*llp[2,])


llr = sapply(mods, function(m) m$performance$AR$looE$estimates[1,])
llr = rbind(llr, li = llr[1,] - 1.96*llr[2,])
llr = rbind(llr, ui = llr[1,] + 1.96*llr[2,])

plot(1:6, 
     llr[1,],
     ylim = c(min(llr[-2,]), max(llr[-2,])),
     pch = 19,
     col = 'red',
     xaxt = 'n')
segments(1:6,
         y0 = llr[3,], y1 = llr[4,],
         col = 'red')
axis(1, at = 1:6, dimnames(llr)[[2]])

loo_compare(lapply(mods, function(m) m$performance$AR$looE))

plot(1:6, 
     llp[1,],
     ylim = c(min(llp[-2,]), max(llp[-2,])),
     pch = 19,
     col = 'blue',
     xaxt = 'n')
segments(1:6,
         y0 = llp[3,], y1 = llp[4,],
         col = 'blue')
axis(1, at = 1:6, dimnames(llp)[[2]])

loo_compare(lapply(mods, function(m) m$performance$AP$looE))