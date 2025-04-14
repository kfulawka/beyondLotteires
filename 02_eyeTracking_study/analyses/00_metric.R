rm(list = ls())

# list of all files in the working directory
temp = list.files(path = 'data/Mdata', pattern = "*.tsv",
                  full.names = T)

# read listed files into list
Metric = lapply(temp, read.csv, sep = '\t', stringsAsFactors = F)

Metric = data.table::rbindlist(Metric)

table(Metric$sex)

mean(Metric$age, na.rm = T)
sd(Metric$age, na.rm = T)

# outliers (based on attention check problems and ARs)
source('analyses/00a_choice_dat_prep/00_outliers.R')

#
Metric = Metric[!Metric$sub %in% outliers, ]

table(Metric$sex)
mean(Metric$age, na.rm = T)
sd(Metric$age, na.rm = T)
summary(Metric$age)
