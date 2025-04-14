# FUNCTION for preparing ET daat for furrther processing
# input is raw ET data from Side Effects study
etPrepare <- function(x) {
  
  # copy first column
  x$event <- x$l_g_p_val
  
  # remove validation information
  x$event[x$event == "1 " | x$event == "0 "] <- NA
  
  # fill remaining NAs with preceeding event
  x$event <- zoo::na.locf(x$event)
  
  # CLEAN DATA
  x <- x[complete.cases(x), ]
  # convert valid indexes to numerics
  x[, 1:2] <- apply(x[, 1:2], 2, as.numeric)
  
  # filter not valid records and remove pupil size and valid info
  x <- x[x$l_g_p_val == 1 & x$r_g_p_val == 1, c(3:6, 11)]
  
  # remove paretheses
  x[,1] <- gsub('\\s\\(', '', x[,1])
  x[,1] <- gsub('\\)\\s', '', x[,1])
  
  x[,2] <- gsub('\\s\\(', '', x[,2])
  x[,2] <- gsub('\\)\\s', '', x[,2])
  
  # extract gaze data
  x <- tidyr::separate(x, 'l_gaze_p', c('lx','ly'), ',')
  x <- tidyr::separate(x, 'r_gaze_p', c('rx', 'ry'), ',')
  
  # convert into numerics
  x[,1:4] <- apply(x[,1:4], 2, as.numeric)
  
  # split into affect poor and rich
  
  # affect poor starts:
  a_poor = which(x$event == 'poor_BLOCKSTART')[1]
  # affect_rich starts:
  a_rich = which(x$event == 'rich_BLOCKSTART')[1]
  
  # assign subsets to proper dataframes
  if (a_poor < a_rich) {
    x_poor = x[a_poor:(a_rich-1), ]
    x_rich = x[a_rich:nrow(x), ]
  } else {
    x_poor = x[a_poor:nrow(x), ]
    x_rich = x[a_rich:(a_poor-1), ]
  }
  
  x <- list(poor = x_poor, rich = x_rich)
  
  # clean a bit
  rm(x_poor, x_rich, a_poor, a_rich)
  
  # SUBSET PREDECISION PHASE
  # function for subsetting (to input into lapply)
  # THIS FUNCTION SHOULD RETURN TRUE FOR ROWS OF INTEREST, FALSE OTHERWISE!!
  ss <- function(x) {
    x[grepl('\\d+_onset_', x$event), ]
    
  }
  
  # overwrite original list with subset of et data (using lapply)
  x <- lapply(x, ss)
  rm(ss)
  
  # extract information about choice 
  # function for lapply
  cInfo <- function(x) {
    tidyr::separate(x, 'event', c('cp', 'st', 'tn', 'swap', 'lay', 'xy', 'xp'), '_')
  }
  x <- lapply(x, cInfo)
  rm(cInfo)
  
  # remove one unnecessary colum
  for (i in 1:2) {
    x[[i]]$st <- NULL
  }
  
  # get subject number 
  subN = paste('s', names(x)[1], sep = '')
  
  # name resulting list with subject number
  return(assign(subN, x))
  rm(x)
  
}