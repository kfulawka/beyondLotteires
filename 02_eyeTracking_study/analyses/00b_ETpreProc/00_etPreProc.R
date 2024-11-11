rm(list = ls())

library(future.apply)

# read data ---------------------------------------------------------------

# list of all files in the working directory
etDat_path = paste(getwd(), 'data', 'ETdata', sep = '/')
temp = list.files(path = etDat_path, pattern = "*.csv")

# participants numbers
subNums = substr(temp, 1, 3)

# paths to files
temp = paste(etDat_path, temp, sep = '/')
rm(etDat_path)

# read listed files into list
plan(multisession)
ETrawDat = future_lapply(temp, read.csv, sep = ';', stringsAsFactors = F)
plan(sequential)

# number of participants
n <- length(ETrawDat)

# order of conditions
ord = c()
for (i in 1:length(ETrawDat)) {
  if (ETrawDat[[i]][1,1] == 'poor_BLOCKSTART') {
    ord[i] <- 'PR'
  } else {
    ord[i] <- 'RP'
  }
}

# combine with sub nums
subNums <- paste(subNums, ord, sep = '_')

# set data frames' names as participants numbers with order of conditions
names(ETrawDat) <- subNums
rm(temp, ord, i)

# prepare ET data -----------------------------------------------------------------------

# read in function for data preparation
source('analyses/00b_ETpreProc/00_F_prepETfun.R')

# apply the function to each element of ETrawDat data
plan(multisession)
etClean <- future_lapply(ETrawDat, etPrepare)
plan(sequential)

rm(etPrepare)

# compute average gaze -----------------------------------------------------------

# function for lapply
avGaze <- function(x) {
  
  # average gaze
  x$xav = ( (x$lx + x$rx) / 2 ) * 1920
  x$yav = ( (x$ly + x$ry) / 2 ) * 1080
  
  return(x)
}

# do the calculation
for (i in 1:length(etClean)) {
  etClean[[i]] <- lapply(etClean[[i]], avGaze)
}

rm(avGaze, i)

# test --------------------------------------------------------------------

# test number of trials within each cond:subject
t_num <- matrix(NA, nrow = length(etClean), ncol = 2)

# function returning number of unique trial numbers
un_tn <- function(x) { length(unique(x$CPNum))  }

#
for (i in 1:length(etClean)) {
  t_num[i, ] = sapply(etClean[[i]], un_tn)
}

sum(t_num != 65) 
# one person closed her/his eye in one trial
# subject: 108, trial: 35, cond: poor; rows in raw data: 23830-24266
rm(i, un_tn, t_num, ETrawDat)

# 
saveRDS(etClean, 'analyses/00b_ETpreProc/etClean.rds')