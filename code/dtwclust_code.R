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

# Run a dtwclust model 
# tsclust require data matrix to be in rowise format or in a tslist format (if series are not of equal length)
# DTW-DBA 

dtw_dba <- tsclust(ts_df_list$ECG5000_TRAIN[,-1], k = 5L,
                  distance = 'sbd', centroid = "shape",
                  trace = TRUE, preproc = zscore,type = 'partitional',
                  args = tsclust_args(cent = list(trace = TRUE)),
                  control = partitional_control(nrep = 200L))

plot(dtw_dba,type='centroids')
 

compare_labels <- function(cluster,ts_labels){
  
  sum(ts_labels ==  cluster@cluster)/length(ts_labels)

}


compare_labels(dtw_dba,ts_labels$ECG5000_TRAIN)/length(ts_labels$ECG5000_TRAIN)

best_pct_correct <- unlist(lapply(dtw_dba, compare_labels,ts_labels = ts_labels$ECG5000_TRAIN))

which(best_pct_correct == max(best_pct_correct))

dtw_dba20