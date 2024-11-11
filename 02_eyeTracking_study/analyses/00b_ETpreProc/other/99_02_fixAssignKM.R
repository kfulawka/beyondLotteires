
# fixation data -----------------------------------------------------------

load("C:/Users/kfulawka/Desktop/Psychologia/MPIB/ETstudy/data/workSpaces/ET/01_fixDetected.RData")

# Beh data ----------------------------------------------------------------

# CT data
setwd('C:/Users/kfulawka/Desktop/Psychologia/MPIB/ETstudy/data/CTdata')

# list of all files in the CT folder
CTnames = list.files(pattern = "*.tsv")

# read CT data
CTdata = lapply(CTnames, read.table, sep = '\t', stringsAsFactors = F, header = T)
rm(CTnames)
names(CTdata) <- names(fixDetPix)

# prepare choice data
for( i in 1:72) {
  
  CTdata[[i]]$layout <- ifelse(CTdata[[i]]$layout == 'horizontal', 2, 1)
  
  CTdata[[i]]$pA <- as.numeric(sub('%', '', CTdata[[i]]$pA))/100
  CTdata[[i]]$pB <- as.numeric(sub('%', '', CTdata[[i]]$pB))/100
  
  CTdata[[i]]$layT <- ifelse(CTdata[[i]]$posXY == 0 & CTdata[[i]]$posXP == 0, 1
                             , ifelse(CTdata[[i]]$posXY == 1 & CTdata[[i]]$posXP == 0, 2
                                      , ifelse(CTdata[[i]]$posXY == 0 & CTdata[[i]]$posXP == 1, 3, 4)))
  
}

# merge with trial info -------------------------------------------------------------------------

# function for handling the merge
t_info <- function(x, beh) {
  xx <- list()
  for (i in c('poor', 'rich')) {
    xx[[i]] <- merge(x[[i]], beh[beh$cond == i, c(9:14, 17)], by = 'CPNum')
  }
  return(xx)
}

# apply to each participant
for (i in 1:72) {
  fixDetPix[[i]] <- t_info(fixDetPix[[i]], beh = CTdata[[i]])
}

# convert CPNum back to numeric
for( i in 1:72) {
  for (j in c('poor', 'rich')) {
    fixDetPix[[i]][[j]]$CPNum <- as.numeric(as.character(fixDetPix[[i]][[j]]$CPNum ))
  }
}


# centers -----------------------------------------------------------------

# function to convert psychopy coordinate system to 0-1 system
psyConv <- function(x) x/2 + .5

# coordinates of horizontal layout
xc <- psyConv(c(-.65, -.3, .3, .65))
yc <- psyConv(c(-.3, .3))

# screen parameters
x_s <- 1920
y_s <- 1080


# centers for horizontal layout 
# (numbers are row numbers in the coordinates matrix):
# 
# 1   2     3   4
#        9
# 5   6     7   8
c_h <- matrix(NA, nrow = 9, ncol = 2)

# x coordinates
c_h[,1] <- c(rep(xc * x_s, 2), x_s/2)

# y coordinates
c_h[,2] <- c(rep(yc[1] * y_s, 4), rep(yc[2] * y_s, 4), y_s/2)


# centers for vertical layout 
# (numbers are row numbers in the coordinates matrix):
# 
# 1     5  
# 2     6
#    9
# 3     7
# 4     8
c_v <- matrix(NA, nrow = 9, ncol = 2)

# x coordinates
c_v[,1] <- c(rep(yc[1] * x_s, 4), rep(yc[2] * x_s, 4), x_s/2)

# y coordinates
c_v[,2] <- c(rep(xc * y_s, 2), y_s/2)

cent <- list(c_v, c_h)

# fix data: kmeans ----------------------------------------------------------------

xx <- fixDetPix$`139_PR`$poor

# for each type of layout
# perform cluster analysis

# list to store results
ca <- list('1' = list()
           , '2' = list())


for (i in 1:2) { # for vertical and horizontal
  for (j in 1:4) { # for layouts 1:4
    ca[[i]][[j]] <- kmeans(x = xx[xx$layout == i & xx$layT == j, c('x', 'y')], centers = cent[[i]]
                           , iter.max = 1000, algorithm = 'MacQueen')$cluster
  }
}

ccs <- c(rgb(.1, 0, .25, .7), rgb(.1, 0, .5, .7), rgb(.1, 0, .75, .7), rgb(.1, 0, 1, .7)
         , rgb(.25, 0, .1, .7), rgb(.5, 0, .1, .7), rgb(.75, 0, .1, .7), rgb(1, 0, .1, .7)
         , 'grey')

layout(matrix(1:4, nrow = 2))
par(mar = c(4, 4, 4, 1))

for (i in 1:2) {
  
  for (j in 1:4) {
    
    plot(i+j, type = 'n', xlim = c(0, 1920), ylim = c(1080, 0), xlab = 'x', ylab = 'y'
         , main = paste(i, j, sep = '_'))
    
    points(xx[xx$layout == i & xx$layT == j, c('x', 'y')], pch = 19, cex = .5, col = ccs[ca[[i]][[j]]]
    )
    
    rect(aois[,1,j,i], aois[,3,j,i], aois[,2,j,i], aois[,4,j,i]
         , border = 'lightblue', lwd = 2)
    
  }
  
}