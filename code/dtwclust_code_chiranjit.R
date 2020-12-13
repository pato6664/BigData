# Data prep for time series clustering 
rm(list=ls())
library(tidyverse)
library(dtwclust)

# Read in data

data_dir <- "G:/Shared drives/Big Data Analytics Project/Data"

# The first column will contain the data labels 
# Data is formatred rowise i.e. each row is a time series 

all_files <- list.files(data_dir)

all_files_full <- list.files(data_dir,full.names = T)

ts_df_list <- lapply(data_dir,read.table,sep='\t')

names(ts_df_list) <- gsub(".tsv","",all_files)

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


#####################################################
# K-Shape Clustering algorithm for GunPoint_TRAIN
#####################################################

# Initialization:
cvi_metric_l <- list()
time_l <- list()
nrep=100
dat <- ts_df_list$GunPoint_TRAIN

start=Sys.time()

for(i in 1:nrep){

  
  # Run a dtwclust model 
  # tsclust require data matrix to be in rowise format or in a tslist format (if series are not of equal length)
  # DTW-k-shape 
  
  dtw_kshape <- tsclust(dat[,-1], k = 2L,
                        distance = 'sbd', centroid = "shape",
                        trace = TRUE, preproc = NULL,
                        args = tsclust_args(cent = list(trace = TRUE)))
  
  # Comparing the actual vs estimated classes:
  classify_tab <- table(actual=dat[,1],estimated=dtw_kshape@cluster)
  
  # Accuracy of classification:
  matches <- sum(diag(classify_tab))
  accuracy <- matches/sum(classify_tab)
  
  # External CVI calculation:
  cvi_metric <- c(cvi(a = dtw_kshape,b=dat[,1],type="external"),accuracy)
  names(cvi_metric) <- c("ARI","RI","J","FM","VI","Accuracy")
  
  cvi_metric_l[[i]] <- cvi_metric
  
  # Time taken for each iteration:
  time_l[[i]] <- dtw_kshape@proctime
  
}

end=Sys.time()

all_cvi1 <- do.call(rbind,cvi_metric_l)
all_time1 <- do.call(rbind,time_l)

mean(all_cvi1[,'Accuracy'])

#####################################################
# K-Shape Clustering algorithm for CBF_TRAIN
#####################################################

# Initialization:
cvi_metric_l <- list()
time_l <- list()
nrep=100
dat <- ts_df_list$CBF_TRAIN

start=Sys.time()

for(i in 1:nrep){
  
  
  # Run a dtwclust model 
  # tsclust require data matrix to be in rowise format or in a tslist format (if series are not of equal length)
  # DTW-k-shape 
  
  dtw_kshape <- tsclust(dat[,-1], k = 3L,
                        distance = 'sbd', centroid = "shape",
                        trace = TRUE, preproc = NULL,
                        args = tsclust_args(cent = list(trace = TRUE)))
  
  # Comparing the actual vs estimated classes:
  classify_tab <- table(actual=dat[,1],estimated=dtw_kshape@cluster)
  
  # Accuracy of classification:
  matches <- sum(diag(classify_tab))
  accuracy <- matches/sum(classify_tab)
  
  # External CVI calculation:
  cvi_metric <- c(cvi(a = dtw_kshape,b=dat[,1],type="external"),accuracy)
  names(cvi_metric) <- c("ARI","RI","J","FM","VI","Accuracy")
  
  cvi_metric_l[[i]] <- cvi_metric
  
  # Time taken for each iteration:
  time_l[[i]] <- dtw_kshape@proctime
  
}

end=Sys.time()

all_cvi2 <- do.call(rbind,cvi_metric_l)
all_time2 <- do.call(rbind,time_l)


#####################################################
# K-Shape Clustering algorithm for CBF_TRAIN
#####################################################

# Initialization:
cvi_metric_l <- list()
time_l <- list()
nrep=100
dat <- ts_df_list$ECG5000_TRAIN

start=Sys.time()

for(i in 1:nrep){
  
  
  # Run a dtwclust model 
  # tsclust require data matrix to be in rowise format or in a tslist format (if series are not of equal length)
  # DTW-k-shape 
  
  dtw_kshape <- tsclust(dat[,-1], k = 5L,
                        distance = 'sbd', centroid = "shape",
                        trace = TRUE, preproc = NULL,
                        args = tsclust_args(cent = list(trace = TRUE)))
  
  # Comparing the actual vs estimated classes:
  classify_tab <- table(actual=dat[,1],estimated=dtw_kshape@cluster)
  
  # Accuracy of classification:
  matches <- sum(diag(classify_tab))
  accuracy <- matches/sum(classify_tab)
  
  # External CVI calculation:
  cvi_metric <- c(cvi(a = dtw_kshape,b=dat[,1],type="external"),accuracy)
  names(cvi_metric) <- c("ARI","RI","J","FM","VI","Accuracy")
  
  cvi_metric_l[[i]] <- cvi_metric
  
  # Time taken for each iteration:
  time_l[[i]] <- dtw_kshape@proctime
  
}

end=Sys.time()

all_cvi3 <- do.call(rbind,cvi_metric_l)
all_time3 <- do.call(rbind,time_l)

max(all_cvi3[,'Accuracy'])

#####################################################
# K-Shape Clustering algorithm for CBF_TRAIN
#####################################################

# Initialization:
cvi_metric_l <- list()
time_l <- list()
nrep=100
dat <- ts_df_list$ItalyPowerDemand_TRAIN

start=Sys.time()

for(i in 1:nrep){
  
  
  # Run a dtwclust model 
  # tsclust require data matrix to be in rowise format or in a tslist format (if series are not of equal length)
  # DTW-k-shape 
  
  dtw_kshape <- tsclust(dat[,-1], k = 2L,
                        distance = 'sbd', centroid = "shape",
                        trace = TRUE, preproc = NULL,
                        args = tsclust_args(cent = list(trace = TRUE)))
  
  # Comparing the actual vs estimated classes:
  classify_tab <- table(actual=dat[,1],estimated=dtw_kshape@cluster)
  
  # Accuracy of classification:
  matches <- sum(diag(classify_tab))
  accuracy <- matches/sum(classify_tab)
  
  # External CVI calculation:
  cvi_metric <- c(cvi(a = dtw_kshape,b=dat[,1],type="external"),accuracy)
  names(cvi_metric) <- c("ARI","RI","J","FM","VI","Accuracy")
  
  cvi_metric_l[[i]] <- cvi_metric
  
  # Time taken for each iteration:
  time_l[[i]] <- dtw_kshape@proctime
  
}

end=Sys.time()

all_cvi4 <- do.call(rbind,cvi_metric_l)
all_time4 <- do.call(rbind,time_l)
