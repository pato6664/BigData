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

dtw_dba <- tsclust(ts_df_list$GunPoint_TRAIN[,-1], k = 2L,
                  distance = 'dtw_basic', centroid = "dba",
                  trace = TRUE, preproc = NULL,type = 'partitional',
                  args = tsclust_args(cent = list(trace = TRUE)))
plot(dtw_dba)
sum((ts_labels$GunPoint_TRAIN == dtw_dba@cluster))


