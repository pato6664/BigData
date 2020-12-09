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



# Run a dtwclust model 

dtw_dba <- tsclust(ts_df_list$CBF_TRAIN[,-1], k = 3L,
                  distance = "euclidean", centroid = "mean",
                  trace = TRUE, seed = 8, norm = "L2",args = tsclust_args(cent = list(trace = TRUE)))










