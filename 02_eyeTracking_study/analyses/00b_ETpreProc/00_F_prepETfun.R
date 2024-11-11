# FUNCTION for preparing ET daat for furrther processing
# input is raw ET data from Side Effects study
etPrepare <- function(x) {
  
  # copy first column
  x$CPNum <- x$l_g_p_val
  
  # remove validation information
  x$CPNum[x$CPNum == "1 " | x$CPNum == "0 "] <- NA
  
  # remove parentheses
  x[, 'l_gaze_p'] <- gsub('\\s\\(', '', x[,'l_gaze_p'])
  x[,'l_gaze_p'] <- gsub('\\)\\s', '', x[,'l_gaze_p'])
  
  x[,'r_gaze_p'] <- gsub('\\s\\(', '', x[,'r_gaze_p'])
  x[,'r_gaze_p'] <- gsub('\\)\\s', '', x[,'r_gaze_p'])
  
  # extract gaze data
  x <- tidyr::separate(x, 'l_gaze_p', c('lx','ly'), ',')
  x <- tidyr::separate(x, 'r_gaze_p', c('rx', 'ry'), ',')
  
  # split into affect poor and rich
  
  # affect poor starts:
  a_poor = which(x$CPNum == 'poor_BLOCKSTART')[1]
  # affect_rich starts:
  a_rich = which(x$CPNum == 'rich_BLOCKSTART')[1]
  
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
  
  # function to extract the pre decision phase
  t_sub <- function(x, n_t = 65) {
    
    # create vectors with names of stamps indexing trial start and choice
    f_start <- paste(0:(n_t-1), 'fixstart', sep = '_')
    t_start <- paste(0:(n_t-1), 'onset', sep = '_')
    t_choice <- paste(0:(n_t-1), 'choice', sep = '_')
    
    si <- c()
    se <- c()
    
    for ( i in 1:n_t) {
      
      try(si[i] <- grep(paste('^',t_start[i],sep=''), x$CPNum), silent = T)
      
      if ( is.na(si[i]) ) {
        si[i] <- which(x$CPNum == f_start[i])
        t_start[i] <- f_start[i]
      }
      
      try(se[i] <- which(x$CPNum == t_choice[i]), silent = T)
      
      if ( is.na(se[i]) ) {
        se[i] <- grep(paste('^', i-1, '_fdbON', sep = ''), x$CPNum)
        t_choice[i] <- x$CPNum[grepl(paste('^', i-1, '_fdbON', sep = ''), x$CPNum)]
      }
      
      if (i == 1) {
        
        # subset first choice problem
        x_dec <- x[si[i]:se[i], ]
        
        # fill CPNum column with choice problem number
        x_dec$CPNum[grep(paste('^',t_start[i],sep=''), x_dec$CPNum):which(x_dec$CPNum == t_choice[i])] <- i-1
        
      } else {
        # subset next choice problems and add to previous
        x_dec <- rbind(x_dec, x[si[i]:se[i], ])
        
        # fill CPNum column with choice problem number
        x_dec$CPNum[grep(paste('^',t_start[i],sep=''), x_dec$CPNum):which(x_dec$CPNum == t_choice[i])] <- i-1
      }
    }
    
    # CLEAN DATA
    x_dec <- x_dec[complete.cases(x_dec), ]
    # convert valid indexes to numerics
    x_dec[, 1:2] <- apply(x_dec[, 1:2], 2, as.numeric)
    
    # filter not valid records and remove pupil size and valid info
    x_dec <- x_dec[x_dec$l_g_p_val == 1 & x_dec$r_g_p_val == 1, c(3:7, 13)]
    
    # convert into numerics
    x_dec[,1:5] <- apply(x_dec[,1:5], 2, as.numeric)
    
    return(x_dec)
  }
  
  x <- lapply(x, t_sub)
  
  # get subject number 
  subN = paste('s', names(x)[1], sep = '')
  
  # name resulting list with subject number
  return(assign(subN, x))
  rm(x)
}