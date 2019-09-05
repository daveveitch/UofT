#### Extract Features from the Time Series #### 
#### Load R package with data ####
library(M4comp2018)
library(itsmr)
library(forecast)
library(changepoint)
library(lmtest)
library(tsfeatures)
library(foreach)
library(doParallel)
cl <- parallel::makeCluster(10)
registerDoParallel(cl,cores=10)

data("M4")

#### Extract only Monthly Series ####
M4_monthly <- list()
for(i in seq(1,100000)){
  if(M4[[i]]$period=='Monthly'){
    M4_monthly[[length(M4_monthly)+1]] <- M4[[i]]  
  } else{}
}

set.seed(24) # just to make it reproducible
M4_monthly <- sample(M4_monthly,replace=FALSE)


#### Run loop computing features of time series
#### parallelize so do loops of 1000

master_feature_df <- foreach (i=1:48,.packages=c('M4comp2018','forecast','tsfeatures'),.combine=rbind) %dopar% {
  
  start_index <- i*1000-999
  end_index <- i*1000
  
  feature_df <- data.frame()
  
  for(j in seq(start_index,end_index)){
    
    acf_feat <- acf_features(M4_monthly[[j]]$x)
    entropy_feat <- entropy(M4_monthly[[j]]$x)
    stl_feat <- stl_features(M4_monthly[[j]]$x)
    
    new_row <-  data.frame("M4 ID"=M4_monthly[[j]]$st,
                           'TS.ID.Number'=j,
                           "x_acf1"=acf_feat[1],
                           "x_acf10"=acf_feat[2],
                           "diff1_acf1"=acf_feat[3],
                           "diff1_acf10"=acf_feat[4],
                           "diff2_acf1"=acf_feat[5],
                           "diff2_acf10"=acf_feat[6],
                           "seas_acf1"=acf_feat[7],
                           "entropy"=entropy_feat,
                           "nperiods"=stl_feat[1],
                           "trend"=stl_feat[3],
                           "spike"=stl_feat[4],
                           "linearity"=stl_feat[5],
                           "curvature"=stl_feat[6],
                           "e_acf1"=stl_feat[7],
                           "e_acf10"=stl_feat[8],
                           "seasonal_strength"=stl_feat[9],
                           "peak"=stl_feat[10],
                           "trough"=stl_feat[11])        
    feature_df <- rbind(feature_df,new_row)
    
    
  }           
  
  feature_df
  
}

# Write to CSV
setwd("/u/veitch")
write.csv(master_feature_df,file='TimeSeriesFeatures.csv', row.names = FALSE)

parallel::stopCluster(cl)