rm(list = ls())

# source('analyses/00_ETpreProc/01_fixDetect.R')
fixDetPix = readRDS('analyses/00b_ETpreProc/fixDetPix.rds')

# Beh data ----------------------------------------------------------------

# list of all files in the folder with beh data
ctDat_path = paste(getwd(), 'data', 'CTdata', sep = '/')
temp = list.files(path = ctDat_path, pattern = "*.tsv")

# path to files
temp = paste(ctDat_path, temp, sep = '/')
rm(ctDat_path)

# read CT data
CTdata = lapply(temp, read.table, sep = '\t', stringsAsFactors = F, header = T)

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

# fix to aois --------------------------------------------------------------------

source('analyses/00b_ETpreProc/02a_aois.R')

# Function to handle AOIS assignment
stim_fixed <- function(x, aois) {
  # store currently fixated stimuli in z
  z <- dimnames(aois)[[1]][which(x['x'] > aois[,'x0', x["layT"], x["layout"]]
             & x['x'] < aois[,'x1', x["layT"], x["layout"]]
             & x['y'] > aois[,'y0', x["layT"], x["layout"]]
             & x['y'] < aois[,'y1', x["layT"], x["layout"]])]
  # if fixation is not in any of aois, set z to 'o
  if(length(z) == 0) {
    z <- 'o'
  } 
  # return the assigned aoi
  return(z)
}

# for each participant
for (i in 1:length(fixDetPix)) {
  fixDetPix[[i]] <- lapply(fixDetPix[[i]]
                            , FUN = function (x) cbind(x, fixed_s = apply(x, 1, FUN = stim_fixed, aois = aois))
                            )
}
rm(i, j, stim_fixed, t_info)

saveRDS(fixDetPix, 'analyses/00b_ETpreProc/fixAOIs.rds')