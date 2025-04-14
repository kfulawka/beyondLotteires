rm(list = ls())

# read list with choice task data ---------------------------------------------------

source('analyses/00a_choice_dat_prep/00_choice_dat_read.R')

# convert into data.frame
ct <- data.table::rbindlist(CTdata)
rm(CTdata)
n <- length(unique(ct$sub))

# combine CT with AR data ----------------------------------------------------

# AR data path
arDat_path = 'data/ARdata'

# list of all files in the AR folder
temp = list.files(path = arDat_path, pattern = "*.tsv", full.names = T)

AR1 = sort(grep('AR_', temp, value = T))
AR2 = sort(grep('AR2_', temp, value = T))

# read listed files into list
AR1data = lapply(AR1, read.table, sep = '\t', stringsAsFactors = F, header = T)
AR1data <- data.table::rbindlist(AR1data)

AR2data = lapply(AR2, read.table, sep = '\t', stringsAsFactors = F, header = T)
AR2data <- data.table::rbindlist(AR2data)
colnames(AR2data)[1:2] <- c('affect2', 'rt2')

ar <- merge(AR1data, AR2data, by = c('sub', 'SE'))

ar$affMean <- rowMeans(ar[,c('affect', 'affect2')])

AR1data$part <- 0
AR2data$part <- 1
names(AR2data)[1:2] <- names(AR1data)[1:2]
ar_raw <- rbind(AR1data, AR2data)
write.table(ar_raw, 'data/ar_raw.txt', 
            row.names = F, sep = '\t')

rm(AR1data, AR2data, AR1, AR2, temp, ar_raw)

# map onto SE nums in ct data
ar_ct <- matrix(NA, nrow = nrow(ct), ncol = 6,
                dimnames = list(NULL, c('xAaf1', 'xBaf1', 'xAaf2', 'xBaf2', 'xAaf_m', 'xBaf_m'))
                )
ct <- cbind(ct, ar_ct)
rm(ar_ct)

# for each subject
for (i in unique(ct$sub)) {
  
  # for each unique side effect index
  for (j in unique(ct$xA[ct$sub == i])) {
    
    # insert the corresponding affect ratings
    ct$xAaf1[ct$sub == i & ct$xA == j] <- ar$affect[ar$sub == i & ar$SE == j]
    ct$xAaf2[ct$sub == i & ct$xA == j] <- ar$affect2[ar$sub == i & ar$SE == j]
    ct$xAaf_m[ct$sub == i & ct$xA == j] <- ar$affMean[ar$sub == i & ar$SE == j]
  }
  
  for (j in unique(ct$xB[ct$sub == i])) {
    ct$xBaf1[ct$sub == i & ct$xB == j] <- ar$affect[ar$sub == i & ar$SE == j]
    ct$xBaf2[ct$sub == i & ct$xB == j] <- ar$affect2[ar$sub == i & ar$SE == j]
    ct$xBaf_m[ct$sub == i & ct$xB == j] <- ar$affMean[ar$sub == i & ar$SE == j]
    
  }
}

# save data ---------------------------------------------------------------

write.table(ct, 
            'data/ct_dat.txt', 
            row.names = F, 
            sep = '\t')