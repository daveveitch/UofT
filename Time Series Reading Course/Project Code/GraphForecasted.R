#### Load R package with data ####
library(M4comp2018)
library(itsmr)
library(forecast)
library(changepoint)
library(lmtest)
library(tsfeatures)

data("M4")

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

setwd("C:/Users/David/Google Drive/Documents/UofT/MSc/Time Series Reading Course/Project/Code") 
#forecasted_results <- read.csv('MasterForecast.csv', header = TRUE, sep = ",")
forecasted_results <- read.csv('MasterForecast.csv', header = TRUE, sep = ",")
comb_forc <- read.csv('CombinedForecastDF.csv',header=TRUE,sep=",")

# TS ID Number of Series to be Graphed
ts_id_number <-24249

par(mfrow=c(3,2))
#### TS ID NUMBER AS CALL ###

for(forecast_method in c('Arima','RW','RWDrift','HW')){

  method <- forecast_method
  M4_id <- M4_monthly[[ts_id_number]]$st
  
  unioned_time_series <- ts.union(c(M4_monthly[[ts_id_number]]$x,M4_monthly[[ts_id_number]]$xx), 
                                  dframe = FALSE)
  unioned_times <- c(time(M4_monthly[[ts_id_number]]$x),time(M4_monthly[[ts_id_number]]$xx))
  forecast_start_date <-time(M4_monthly[[ts_id_number]]$xx)[1]
  forecast_end_date <- time(M4_monthly[[ts_id_number]]$xx)[length(M4_monthly[[ts_id_number]]$xx)]
  
  forecast_ts <- ts(data=as.numeric(as.vector(forecasted_results[(forecasted_results$TS.ID.Number==ts_id_number)&(forecasted_results$Description==paste(method,'Mean')),][1,1:18])),
                    frequency=12,
                    start=forecast_start_date,
                    end=forecast_end_date)
  forecast_upper <- ts(data=as.numeric(as.vector(forecasted_results[(forecasted_results$TS.ID.Number==ts_id_number)&(forecasted_results$Description==paste(method,'Upper')),][1,1:18])),
                    frequency=12,
                    start=forecast_start_date,
                    end=forecast_end_date)
  forecast_lower <- ts(data=as.numeric(as.vector(forecasted_results[(forecasted_results$TS.ID.Number==ts_id_number)&(forecasted_results$Description==paste(method,'Lower')),][1,1:18])),
                       frequency=12,
                       start=forecast_start_date,
                       end=forecast_end_date)
  
  plot(x=unioned_times,y=unioned_time_series,type='l',
       ylim=c(min(unioned_time_series,forecast_ts,forecast_lower),
              max(unioned_time_series,forecast_ts,forecast_upper)),
       main=paste('Forecast',M4_id,forecast_method))
  
  
  abline(v=time(M4_monthly[[ts_id_number]]$x)[length(M4_monthly[[ts_id_number]]$x)],lwd=2,lty=3)
  
  polygon(c(time(forecast_ts),rev(time(forecast_ts))),
          c(forecast_lower,rev(forecast_upper)),
          col=rgb(0, 0, 0,0.1),border=NA)
  lines(forecast_ts,col='blue')
  lines(forecast_lower,col='red')
  lines(forecast_upper,col='red')
}



#### PLOT OPTIMAL WEIGHTINGS ###

for(forecast_method in c('Optimal','Naive')){
  
  method <- forecast_method
  M4_id <- M4_monthly[[ts_id_number]]$st
  
  unioned_time_series <- ts.union(c(M4_monthly[[ts_id_number]]$x,M4_monthly[[ts_id_number]]$xx), 
                                  dframe = FALSE)
  unioned_times <- c(time(M4_monthly[[ts_id_number]]$x),time(M4_monthly[[ts_id_number]]$xx))
  forecast_start_date <-time(M4_monthly[[ts_id_number]]$xx)[1]
  forecast_end_date <- time(M4_monthly[[ts_id_number]]$xx)[length(M4_monthly[[ts_id_number]]$xx)]
  
  forecast_ts <- ts(data=as.numeric(as.vector(comb_forc[(comb_forc$TS.ID.Number==ts_id_number)&(comb_forc$Type==forecast_method),][1:18])),
                    frequency=12,
                    start=forecast_start_date,
                    end=forecast_end_date)
  forecast_upper <- ts(data=as.numeric(as.vector(comb_forc[(comb_forc$TS.ID.Number==ts_id_number)&(comb_forc$Type==forecast_method),][19:36])),
                       frequency=12,
                       start=forecast_start_date,
                       end=forecast_end_date)
  forecast_lower <- ts(data=as.numeric(as.vector(comb_forc[(comb_forc$TS.ID.Number==ts_id_number)&(comb_forc$Type==forecast_method),][37:54])),
                       frequency=12,
                       start=forecast_start_date,
                       end=forecast_end_date)
  
  plot(x=unioned_times,y=unioned_time_series,type='l',
       ylim=c(min(unioned_time_series,forecast_ts,forecast_lower),
              max(unioned_time_series,forecast_ts,forecast_upper)),
       main=paste('Forecast',M4_id,forecast_method))
  
  
  abline(v=time(M4_monthly[[ts_id_number]]$x)[length(M4_monthly[[ts_id_number]]$x)],lwd=2,lty=3)
  
  polygon(c(time(forecast_ts),rev(time(forecast_ts))),
          c(forecast_lower,rev(forecast_upper)),
          col=rgb(0, 0, 0,0.1),border=NA)
  lines(forecast_ts,col='blue')
  lines(forecast_lower,col='red')
  lines(forecast_upper,col='red')
}
