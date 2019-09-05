library(M4comp2018)
library(doParallel)

data("M4")

setwd("/u/veitch")
combined_forecast_df <- read.csv('CombinedForecastDF.csv', header = TRUE, sep = ",")

#### Extract only Monthly Series ####
M4_monthly <- list()
for(i in seq(1,100000)){
  if(M4[[i]]$period=='Monthly'){
    M4_monthly[[length(M4_monthly)+1]] <- M4[[i]]  
  } else{}
}

# Randomize List so that when iterating through list
# we look at different types of time series
set.seed(24) # just to make it reproducible
M4_monthly <- sample(M4_monthly,replace=FALSE)

cl <- parallel::makeCluster(24)
registerDoParallel(cl,cores=24)

performance_df <- foreach (i=1:48,.combine=rbind) %dopar% {
  
  start_index <- i*1000-999
  end_index <- i*1000

  
  # a df with partial performance data
  partial_perf_df <- data.frame()
  
  for(ts_id in seq(start_index,end_index)){
    ts_id_df <- combined_forecast_df[combined_forecast_df$TS.ID.Number==ts_id,]  
    
    # calculate sMAPE
    sMAPE_numerators = 2*abs(ts_id_df[c(1,2),1:18]
                             -c(M4_monthly[[ts_id]]$xx))
    sMAPE_denominators = (abs(ts_id_df[c(1,2),1:18])
                          +abs(c(M4_monthly[[ts_id]]$xx)))
    sMAPE <- data.frame('M4ID'=rep(M4_monthly[[ts_id]]$st,2),'TSID'=rep(ts_id,2),'Measure'=rep('sMAPE',2),
                        'Value'=(1/18)*rowSums(sMAPE_numerators/sMAPE_denominators),
                        "Method"=c('Optimal','Naive'))
    
    # calculate MASE
    MASE_numerators = rowSums(abs(ts_id_df[c(1,2),1:18]-c(M4_monthly[[ts_id]]$xx)))
    full_ts = c(as.numeric(M4_monthly[[ts_id]]$x),as.numeric(M4_monthly[[ts_id]]$xx))
    MASE_denominator = (1/(length(full_ts)-12))*sum(abs(diff(full_ts,12)))
    MASE <- data.frame('M4ID'=rep(M4_monthly[[ts_id]]$st,2),'TSID'=rep(ts_id,2),'Measure'=rep('MASE',2),
                       'Value'=(1/18)*MASE_numerators/MASE_denominator,
                       "Method"=c('Optimal','Naive'))
    
    partial_perf_df <- rbind(partial_perf_df,sMAPE,MASE)
    
  }
  
          
  partial_perf_df
    
  
}

# Write to CSV
setwd("/u/veitch")
write.csv(performance_df,file='CombinedForecastPerformance.csv', row.names = FALSE)

parallel::stopCluster(cl)




# 
# 
# ts_id=2550
# ts_id_df <- combined_forecast_df[combined_forecast_df$TS.ID.Number==ts_id,]  
# 
# 
# # calculate sMAPE
# sMAPE_numerators = 2*abs(ts_id_df[c(1,2),1:18]
#                          -c(M4_monthly[[ts_id]]$xx))
# sMAPE_denominators = (abs(ts_id_df[c(1,2),1:18])
#                       +abs(c(M4_monthly[[ts_id]]$xx)))
# sMAPE <- data.frame('M4ID'=rep(M4_monthly[[ts_id]]$st,2),'TSID'=rep(ts_id,2),'Measure'=rep('sMAPE',2),
#                     'Value'=(1/18)*rowSums(sMAPE_numerators/sMAPE_denominators),
#                     "Method"=c('Optimal','Naive'))
# 
# 
# sMAPE
