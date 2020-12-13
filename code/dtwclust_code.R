# Data prep for time series clustering 
rm(list=ls())
library(tidyverse)
library(dtwclust)

# Read in data

data_dir <- dir("./data/",full.names = T)

# The first column will contain the data labels 
# Data is formatred rowise i.e. each row is a time series 

ts_df_list <- lapply(data_dir,read.table,sep = '\t')

names(ts_df_list) <- gsub(".tsv","",dir("./data/"))


# Make a label list and

get_labels <- function(df_list){
  
  df_labels_list <- list()
  
  for(df in seq_along(df_list)){
    
    df_labels_list[[df]] <- df_list[[df]][,1]
    
    names(df_labels_list)[df] <- names(df_list)[df] 
    
  }
  
  return(df_labels_list)
  
}

ts_labels <- get_labels(ts_df_list)

compare_labels <- function(cluster,ts_labels){
  
  sum(ts_labels ==  cluster@cluster)/length(ts_labels)
  
}


# Run a dtwclust model 
# tsclust require data matrix to be in rowise format or in a tslist format (if series are not of equal length)

#################################################################################################################################
# soft-dtw
#################################################################################################################################
# cbf 

sdtw_cbf <- tsclust(ts_df_list$CBF_TRAIN[,-1], k = 3L,
                  distance = 'sdtw', centroid = "sdtw_cent",
                  trace = TRUE, preproc = zscore,type = 'partitional',
                  args = tsclust_args(cent = list(trace = TRUE),dist = list(gamma = 0.1)),
                  control = partitional_control(nrep = 100L))



best_sdtw_cbf <- unlist(lapply(sdtw_cbf, compare_labels,ts_labels = ts_labels$CBF_TRAIN))


best_sdtw_cbf[which(best_sdtw_cbf== max(best_sdtw_cbf))]

plot(sdtw_cbf[[which(best_sdtw_cbf == max(best_sdtw_cbf))[1]]],type = 'centroid')


# 
# Gunpoint

sdtw_gunpoint <-  tsclust(ts_df_list$GunPoint_TRAIN[,-1], k = 2L,
                                   distance = 'sdtw', centroid = "sdtw_cent",
                                   trace = TRUE, preproc = zscore,type = 'partitional',
                                   args = tsclust_args(cent = list(trace = TRUE),dist = list(gamma = 1)),
                                   control = partitional_control(nrep = 100L))


best_sdtw_gunpoint <- unlist(lapply(sdtw_gunpoint, compare_labels,ts_labels = ts_labels$GunPoint_TRAIN))


best_sdtw_gunpoint[which(best_sdtw_gunpoint == max(best_sdtw_gunpoint))]

plot(sdtw_gunpoint[[which(best_sdtw_gunpoint == max(best_sdtw_gunpoint))[1]]],type = 'centroid')

# Italy power demand 
sdtw_power <-  tsclust(ts_df_list$ItalyPowerDemand_TRAIN[,-1], k = 2L,
                          distance = 'sdtw', centroid = "sdtw_cent",
                          trace = TRUE, preproc = zscore,type = 'partitional',
                          args = tsclust_args(cent = list(trace = TRUE),dist = list(gamma = 0.1)),
                          control = partitional_control(nrep = 100L))


best_sdtw_power <- unlist(lapply(sdtw_power, compare_labels,ts_labels = ts_labels$ItalyPowerDemand_TRAIN))


best_sdtw_power[which(best_sdtw_power == max(best_sdtw_power))]

plot(sdtw_power[[which(best_sdtw_power == max(best_sdtw_power))[1]]],type = 'centroid')


# ECG  

sdtw_ecg <-  tsclust(ts_df_list$ECG5000_TRAIN[,-1], k = 5L,
                       distance = 'sdtw', centroid = "sdtw_cent",
                       trace = TRUE, preproc = zscore,type = 'partitional',
                       args = tsclust_args(cent = list(trace = TRUE),dist = list(gamma = 0.1)),
                       control = partitional_control(nrep = 100L))


best_sdtw_ecg <- unlist(lapply(sdtw_ecg, compare_labels,ts_labels = ts_labels$ECG5000_TRAIN))


best_sdtw_ecg[which(best_sdtw_ecg == max(best_sdtw_ecg))]

plot(sdtw_ecg[[which(best_sdtw_ecg == max(best_sdtw_ecg))[1]]],type = 'centroid')

#################################################################################################################################
# DTW-DBA
#################################################################################################################################

# cbf

dba_cbf <- tsclust(ts_df_list$CBF_TRAIN[,-1], k = 3L,
                    distance = 'dtw_basic', centroid = "dba",
                    trace = TRUE, preproc = zscore,type = 'partitional',
                    args = tsclust_args(cent = list(trace = TRUE)),
                    control = partitional_control(nrep = 100L))


best_dba_cbf <- unlist(lapply(dba_cbf, compare_labels,ts_labels = ts_labels$CBF_TRAIN))


best_dba_cbf[which(best_dba_cbf== max(best_dba_cbf))]

plot(dba_cbf[[which(best_dba_cbf == max(best_dba_cbf))[1]]],type = 'centroid')

# gunpoint

dba_gunpoint <- tsclust(ts_df_list$GunPoint_TRAIN[,-1], k = 2L,
                   distance = 'dtw_basic', centroid = "dba",
                   trace = TRUE, preproc = zscore,type = 'partitional',
                   args = tsclust_args(cent = list(trace = TRUE)),
                   control = partitional_control(nrep = 100L))



best_dba_gunpoint <- unlist(lapply(dba_gunpoint, compare_labels,ts_labels = ts_labels$GunPoint_TRAIN))

best_dba_gunpoint[which(best_dba_gunpoint == max(best_dba_gunpoint))]

plot(dba_gunpoint[[which(best_dba_gunpoint == max(best_dba_gunpoint))[1]]],type = 'centroid')

# power demand

dba_power <- tsclust(ts_df_list$ItalyPowerDemand_TRAIN[,-1], k = 2L,
                        distance = 'dtw_basic', centroid = "dba",
                        trace = TRUE, preproc = zscore,type = 'partitional',
                        args = tsclust_args(cent = list(trace = TRUE)),
                        control = partitional_control(nrep = 100L))



best_dba_power <- unlist(lapply(dba_power, compare_labels,ts_labels = ts_labels$ItalyPowerDemand_TRAIN))

best_dba_power[which(best_dba_power == max(best_dba_power))]

plot(dba_power[[which(best_dba_power == max(best_dba_power))[1]]],type = 'centroid')

# ecg 

dba_ecg <- tsclust(ts_df_list$ECG5000_TRAIN[,-1], k = 5L,
                     distance = 'dtw_basic', centroid = "dba",
                     trace = TRUE, preproc = zscore,type = 'partitional',
                     args = tsclust_args(cent = list(trace = TRUE)),
                     control = partitional_control(nrep = 100L),window.size = 60L)



best_dba_ecg <- unlist(lapply(dba_ecg, compare_labels,ts_labels = ts_labels$ECG5000_TRAIN))

best_dba_ecg[which(best_dba_ecg == max(best_dba_ecg))]

plot(dba_ecg[[which(best_dba_ecg == max(best_dba_ecg))[1]]],type = 'centroid')






