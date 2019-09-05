library(doParallel)
library(foreach)
cl <- parallel::makeCluster(24)
registerDoParallel(cl,cores=24)

forecast_df <- read.csv('MasterForecast.csv', header = TRUE, sep = ",")
weight_df <- read.csv('OptimalWeights.csv', header = TRUE, sep = ",")

combined_forecast_df <- data.frame()

combined_forecast_df <- foreach (i=1:48,.combine=rbind) %dopar% {
  
  start_index <- i*1000-999
  end_index <- i*1000
  
  # a df with partial performance data
  partial_combined_df <- data.frame()  
  
  for(ts_id in seq(start_index,end_index)){
    
    ts_id_df <- forecast_df[forecast_df$TS.ID.Number==ts_id,]  
    ts_id_weights <- weight_df[weight_df$TS.ID.Number==ts_id,]
    
    avg_mean <- (ts_id_weights$ARIMA*ts_id_df[ts_id_df$Description=='Arima Mean',][1:18]+
                   ts_id_weights$RW*ts_id_df[ts_id_df$Description=='RW Mean',][1:18]+
                   ts_id_weights$RWDRIFT*ts_id_df[ts_id_df$Description=='RWDrift Mean',][1:18]+
                   ts_id_weights$HW*ts_id_df[ts_id_df$Description=='HW Mean',][1:18])
    
    avg_upper <- (ts_id_weights$ARIMA*ts_id_df[ts_id_df$Description=='Arima Upper',][1:18]+
                    ts_id_weights$RW*ts_id_df[ts_id_df$Description=='RW Upper',][1:18]+
                    ts_id_weights$RWDRIFT*ts_id_df[ts_id_df$Description=='RWDrift Upper',][1:18]+
                    ts_id_weights$HW*ts_id_df[ts_id_df$Description=='HW Upper',][1:18])
    avg_lower <- (ts_id_weights$ARIMA*ts_id_df[ts_id_df$Description=='Arima Lower',][1:18]+
                    ts_id_weights$RW*ts_id_df[ts_id_df$Description=='RW Lower',][1:18]+
                    ts_id_weights$RWDRIFT*ts_id_df[ts_id_df$Description=='RWDrift Lower',][1:18]+
                    ts_id_weights$HW*ts_id_df[ts_id_df$Description=='HW Lower',][1:18])
    
    naive_mean <-(0.25*ts_id_df[ts_id_df$Description=='Arima Mean',][1:18]+
                    0.25*ts_id_df[ts_id_df$Description=='RW Mean',][1:18]+
                    0.25*ts_id_df[ts_id_df$Description=='RWDrift Mean',][1:18]+
                    0.25*ts_id_df[ts_id_df$Description=='HW Mean',][1:18])
    naive_upper<-(0.25*ts_id_df[ts_id_df$Description=='Arima Upper',][1:18]+
                    0.25*ts_id_df[ts_id_df$Description=='RW Upper',][1:18]+
                    0.25*ts_id_df[ts_id_df$Description=='RWDrift Upper',][1:18]+
                    0.25*ts_id_df[ts_id_df$Description=='HW Upper',][1:18])
    naive_lower<-(0.25*ts_id_df[ts_id_df$Description=='Arima Lower',][1:18]+
                    0.25*ts_id_df[ts_id_df$Description=='RW Lower',][1:18]+
                    0.25*ts_id_df[ts_id_df$Description=='RWDrift Lower',][1:18]+
                    0.25*ts_id_df[ts_id_df$Description=='HW Lower',][1:18])
    
    
    
    series_combined_forecast <- data.frame('Mean'=avg_mean,'Upper'=avg_upper,'Lower'=avg_lower,'TS ID Number'=ts_id, 'M4 ID Number'=ts_id_df$M4.ID.Number[1],'Type'='Optimal')  
    series_naive_forecast <- data.frame('Mean'=naive_mean,'Upper'=naive_upper,'Lower'=naive_lower,'TS ID Number'=ts_id, 'M4 ID Number'=ts_id_df$M4.ID.Number[1],'Type'='Naive')  
    partial_combined_df <- rbind(partial_combined_df,series_combined_forecast)
    partial_combined_df <- rbind(partial_combined_df,series_naive_forecast)
  }
  
  partial_combined_df
  
}

setwd("/u/veitch")
write.csv(combined_forecast_df,file='CombinedForecastDF.csv', row.names = FALSE)

parallel::stopCluster(cl)