rm(list = ls())

# read raw data -----------------------------------------------------------

# list of all files in the folder with beh data
ctDat_path = paste(getwd(), 'data', 'CTdata', sep = '/')
temp = list.files(path = ctDat_path, pattern = "*.tsv")

# subject numbers
subN = substr(temp, 1, 3)

# path to files
temp = paste(ctDat_path, temp, sep = '/')
rm(ctDat_path)

# read CT data
CTdata = lapply(temp, read.table, sep = '\t', stringsAsFactors = F, header = T)
names(CTdata) = subN
rm(temp)

# prepare 
for( i in 1:72) {
  
  # conde layout type with numeric, 2 for horizontal, 1 for vertical
  CTdata[[i]]$layout = ifelse(CTdata[[i]]$layout == 'horizontal', 2, 1)
  
  
  # percentages to probabilities
  CTdata[[i]]$pA = as.numeric(sub('%', '', CTdata[[i]]$pA))/100
  CTdata[[i]]$pB = as.numeric(sub('%', '', CTdata[[i]]$pB))/100
  
  # recode layout
  CTdata[[i]]$layT = ifelse(CTdata[[i]]$posXY == 0 & CTdata[[i]]$posXP == 0, 1
                             , ifelse(CTdata[[i]]$posXY == 1 & CTdata[[i]]$posXP == 0, 2
                                      , ifelse(CTdata[[i]]$posXY == 0 & CTdata[[i]]$posXP == 1, 3, 4)))
  
}

rm(i, subN)