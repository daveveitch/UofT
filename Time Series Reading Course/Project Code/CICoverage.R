library(foreach)
library(doParallel)
        
cl <- parallel::makeCluster(24)
registerDoParallel(cl,cores=24)

forecast_df <- read.csv('CombinedForecastDF.csv', header = TRUE, sep = ",")


combined_CI_df <- foreach (i=1:48,.combine=rbind) %dopar% {
  start_index <- i*1000-999
  end_index <- i*1000
  
  # a df with partial performance data
  partial_combined_df <- data.frame()  
  
  for(ts_id in seq(start_index,end_index)){
    ts_id_naive_df <- forecast_df[(forecast_df$TS.ID.Number==ts_id)&(forecast_df$Type=='Naive'),] 
    ts_id_optimal_df <- forecast_df[(forecast_df$TS.ID.Number==ts_id)&(forecast_df$Type=='Optimal'),] 
    
    naive_coverage <- sum((c(ts_id_naive_df[1,19:36])>M4_monthly[[ts_id]]$xx)&(c(ts_id_naive_df[1,37:54])<M4_monthly[[ts_id]]$xx))
    optimal_coverage <- sum((c(ts_id_optimal_df[1,19:36])>M4_monthly[[ts_id]]$xx)&(c(ts_id_optimal_df[1,37:54])<M4_monthly[[ts_id]]$xx))
    
    partial_combined_df <- rbind(partial_combined_df,data.frame('Naive'=naive_coverage,'Optimal'=optimal_coverage))
  } 
  partial_combined_df
}


write.csv(combined_CI_df,file='CICoverage.csv', row.names = FALSE)
