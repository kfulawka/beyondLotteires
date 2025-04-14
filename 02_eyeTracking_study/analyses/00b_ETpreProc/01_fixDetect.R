rm(list = ls())
#source('analyses/00_ETpreProc/00_etPreProc.R')

etClean = readRDS('analyses/00_ETpreProc/etClean.rds')

# compute # of pixels in one degree of visual angle -----------------------

# screen width (in cm)
scr_w <- 531.36 / 10

# resolution
res_w <- 1920
res_h <- 1080

# distance from the screen (cm)
scr_d <- 60

# pixels in 1 degree of visual angle
vis_ang_pix <- ( tan(pi/180) * scr_d ) * res_w / scr_w
# approx 38 pixels

# function to detect fixations and return database for each subject|condition -------------------------------------------------------

# x should be dataframe with gaze coordinates and time within trial
# vis_ang is max dispersion of fixation in pixels (set to be equal to 1 degree of visual angle)
# min_fd is min duration of fixation, in number of samples from et (set to be 5 * 11 ms = 55 ms)
fix <- function(x, vis_ang = 38, min_fd = 5) {
  
  for(i in unique(x$CPNum) ) {
    
    # select trial
    xx <- x[x$CPNum == i, ]
    
    # time within trial (starts at 0)
    xx$s_time = xx$s_time/1e+3 - min(xx$s_time)/1e+3
    
    if(i == 0) {
      
      # fixations for CP = 0
      xf <- emov::emov.idt(xx$s_time, xx$xav, xx$yav, vis_ang, min_fd)
      xf <- cbind(xf, f_num = 1:nrow(xf), CPNum = i)
      
    } else {
      
      # fixations for the rest of CPs
      xf2 <- emov::emov.idt(xx$s_time, xx$xav, xx$yav, vis_ang, min_fd)
      xf2 <- cbind(xf2, f_num = 1:nrow(xf2), CPNum = i)
      
      # combine with already computed fixations
      xf <- rbind(xf, xf2)
    }
  }
  
  return(xf)
}

# detect fixes for each subject -------------------------------------------

# empty list for storage
fixDetPix <- list()

# for each participant
for (i in 1:length(etClean)) {
  
  fixDetPix[[i]] <- lapply(etClean[[i]], fix)

}

# assign names
names(fixDetPix) <- substr(names(etClean), 1, 3)

# histogram of fixations' durations
f_durs <- unlist(sapply(fixDetPix, function(x) lapply(x, function(x) x$dur)))

hist(f_durs
     , breaks = 100, col = 'lightblue', border = 'white'
     , xlab = 'Fixation duration')

rm(f_durs, i, res_h, res_w, scr_d, scr_w, vis_ang_pix, fix, etClean)

saveRDS(fixDetPix, 'analyses/00_ETpreProc/fixDetPix.rds')