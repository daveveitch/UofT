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

#

#### Function - Determine Optimal Transformation ####
optimal_transformation <- function(time_series){
  # Purpose:    Determines what transform (none, square root, log) to apply to the
  #             data to ensure the residuals have equal variance
  
  # Input:      A time series
  # Output:     1 if no transform, 0.5 if square root, 0 if log
  #             This corresponds to lambda values which can be used in time series function calls
  
  
  # Begin with no transformation applied
  optimal_lambda <- c(1,0.00)
  
  # Loop through no transform, square root, and log transform
  for(transform_lambda in c(1,0.5,0)){
    arima_model <- auto.arima(time_series, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2,
                                    max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
                                    start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
                                    seasonal = TRUE, ic = c("aicc"), stepwise = TRUE,
                                    nmodels = 100, trace = FALSE, approximation = (length(time_series) > 150 | frequency(time_series) > 6), 
                                    method = NULL, truncate = NULL, xreg = NULL,
                                    test = c("adf"), test.args = list(),
                                    seasonal.test.args = list(), allowdrift = TRUE, allowmean = FALSE,
                                    lambda = transform_lambda, biasadj = FALSE, parallel = FALSE, num.cores = 2,x=time_series)
    
    # conduct Breusch-Pagan test to make sure variances of errors are equal  
    bp_df <- data.frame("residuals" = arima_model$residuals, 
                        "fitted_values" = arima_model$fitted)
    
    bp_test_p <- bptest(residuals**2~fitted_values, studentize = TRUE, data = bp_df)$p.value
    
    if(bp_test_p > optimal_lambda[2]){
      optimal_lambda <- c(transform_lambda,bp_test_p)
    }
    
    # Once we have a p-value on the BP test greater than 0.05
    # we take it that the variance of the residuals is constant
    if(bp_test_p>0.05){
      break
    }
  }
  
  return(optimal_lambda[1])
}

# example_transform <- optimal_transformation(M4_monthly[[22000]]$x)

#

##### Function - Identify Changepoints ####
identify_changepoints <- function(time_series,optimal_lambda){
  # Purpose:    This function takes in a time series and estimates
  #             where changepoints occur
  
  # Input:      A vector containing a time series, optimal_lambda for transformation
  # Output:     A vector which is equl to 'numeric(0)' if
  #             no changepoint exists, or a list of changepoints
  #             if they do exist
  
  test_model <- auto.arima(time_series, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2,
                           max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
                           start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
                           seasonal = TRUE, ic = c("aicc"), stepwise = TRUE,
                           nmodels = 100, trace = FALSE, approximation = (length(time_series) > 150 |
                           frequency(time_series) > 6), method = NULL, truncate = NULL, xreg = NULL,
                           test = c("adf"), test.args = list(),
                           seasonal.test.args = list(), allowdrift = TRUE, allowmean = TRUE,
                           lambda = optimal_lambda, biasadj = FALSE, parallel = FALSE, num.cores = 2,x=time_series)
  
  # Extract following values
  # Periodicity s 
  # Non-seasonal degree of differencing d
  # Season degree of differencing D
  test_model_s <- test_model$arma[5]
  test_model_d <- test_model$arma[6]
  test_model_D <- test_model$arma[7]
  
  # Transform time series
  Y_t <- BoxCox(time_series,optimal_lambda)
  
  # Seasonal Differencing
  if(test_model_s>0){
    if(test_model_D>0){
      Y_t = diff(Y_t,lag=test_model_s,differences=test_model_D)
    }
  }
  
  # Non-Seasonal Differencing
  if(test_model_d>0){
    Y_t = diff(Y_t,lag=1,differences=test_model_d)
  }
  
  
  ansmeanvar = cpt.meanvar(Y_t,penalty="MBIC",
                           pen.value=0,method="AMOC",
                           Q=5,test.stat="Normal",
                           class=TRUE,param.estimates=TRUE,
                           shape=1,minseglen=2)
  par(mfrow=c(1,1))
  plot(ansmeanvar,cpt.col='blue')
  
  return(cpts(ansmeanvar))
}
determine_starting_index <- function(time_series,changepoint_list){
  # Purpose:    This function takes in a time series and its estimated
  #             changepoints and returns the index with which to begin
  #             the modelling at
  
  # Input:      A vector containing a time series, and a list of changepoints
  # Output:     An index correponding to the first index after the last recorded
  #             changepoint (note it will not output a series less than 30 indicies
  #             from the end)
  
  start_index = 1
  
  if(length(changepoint_list)==0){
    # Keep Start Index as 1
  } else if (length(changepoint_list)==1){
    # Check that exist 60 more observations
    if((length(time_series)-changepoint_list)<60){
      start_index = 1
    }else{
      start_index= changepoint_list + 1
    }
  } else if (length(changepoint_list>1)){
    for(i in seq(length(changepoint_list),1,by=-1)){
      if((length(time_series)-changepoint_list[i])<60){
        # Check next changepoint
      }else{
        start_index= changepoint_list[i] + 1 
        break # stop the loop
      }
    }
  }
  return(start_index)
}

# test_cp <- identify_changepoints(M4_monthly[[26300]]$x,1)

#

#### Fit an ARIMA Model ####
arima_forecast <- function(time_series, optimal_lambda,start_index){
  # Purpose:    This function fits an ARIMA model to a given time series
  
  # Input:      A vector containing the raw time series from the M4 dataset
  # Output:     A model object as well as the forecasted values
  
  time_series <- window(time_series,start=time(time_series)[start_index])
  
  fitted_arima <- auto.arima(time_series, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2,
                             max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
                             start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
                             seasonal = TRUE, ic = c("aicc"), stepwise = TRUE,
                             nmodels = 100, trace = FALSE, approximation = TRUE, 
                             method = NULL, 
                             xreg = NULL,
                             test = c("kpss", "adf", "pp"), test.args = list(),
                             seasonal.test.args = list(), allowdrift = TRUE, allowmean = TRUE,
                             lambda = optimal_lambda, biasadj = FALSE, parallel = FALSE, num.cores = 2)
    
  return(fitted_arima)
}

# example_arima_model <- arima_forecast(M4_monthly[[203]]$x,1,36)

#

#### Fit a random walk model ####
rw_forecast <- function(time_series, optimal_lambda,start_index){
  # Purpose:    This function fits an random walk model without drift to a given time series
  
  # Input:      A vector containing the raw time series from the M4 dataset
  # Output:     A model object as well as the forecasted values
  
  time_series <- window(time_series,start=time(time_series)[start_index])
  
  fitted_rw <- auto.arima(time_series, d = 1, D = 0, max.p = 0, max.q = 0, max.P = 0,
                          max.Q = 0, max.order = 5, max.d = 1, max.D = 0, start.p = 0,
                          start.q = 0, start.P = 0, start.Q = 0, stationary = FALSE,
                          seasonal = FALSE, ic = c("aicc"), stepwise = TRUE,
                          nmodels = 100, trace = FALSE, approximation = (length(time_series) > 150 | frequency(time_series) > 6), 
                          method = NULL,  xreg = NULL,
                          test = c("kpss", "adf", "pp"), test.args = list(),
                          seasonal.test.args = list(), allowdrift = FALSE, allowmean = TRUE,
                          lambda = optimal_lambda, biasadj = FALSE, parallel = FALSE, num.cores = 2,x=time_series)
  
  
  return(fitted_rw)
}

# example_rw<-rw_forecast(M4_monthly[[4521]]$x,0,1)

#

#### Fit a random walk with drift model ####
rwdrift_forecast <- function(time_series,optimal_lambda,start_index,forecast_length){
  # Purpose:    This function fits an radom walk model with drift to a given time series
  
  # Input:      A vector containing the raw time series from the M4 dataset
  # Output:     A model object as well as the forecasted values
  
  time_series <- window(time_series,start=time(time_series)[start_index])
  
  fitted_rwdrift <- rwf(time_series, h=forecast_length,
                        drift = TRUE, level = c(95), fan = FALSE,
                        lambda = optimal_lambda, biasadj = FALSE)

  return(fitted_rwdrift)
}

#example_rwdrift <- rwdrift_forecast(M4_monthly[[2]]$x,1,1,18)

#

#### Fit a Holt Winters model #### 
hw_forecast <- function(time_series,optimal_lambda,start_index,forecast_length){
  # Purpose:    This function fits a few variations of a Holt-Winters
  #             model and returns the best one
  
  # Input:      A vector containing the raw time series from the M4 dataset
  # Output:     A model object as well as the forecasted values


  time_series <- window(time_series,start=time(time_series)[start_index])

  fitted_hw_add <- hw(time_series, h = forecast_length, 
                      seasonal = "additive",alpha=NULL,beta=NULL,gamma = NULL,phi=NULL,
                      damped = FALSE, level = c(95), 
                      fan = FALSE,
                      initial = c("optimal","simple"),
                      exponential = FALSE,
                      lambda = optimal_lambda, biasadj = FALSE)
  
  fitted_hw_mult <- hw(time_series, h = forecast_length, 
                      seasonal = "multiplicative",
                      damped = FALSE, level = c(95), 
                      fan = FALSE,alpha=NULL,beta=NULL,gamma = NULL,phi=NULL,
                      initial = c("optimal","simple"),
                      exponential = FALSE, biasadj = FALSE)
  
  
  # Returns model (additive or multiplicative) which
  # has the lowest AICC
  if(fitted_hw_add$model$aicc<fitted_hw_mult$model$aicc){
    return(fitted_hw_add)
  } else {
    return(fitted_hw_mult)
  }
  
}

# example_hw <- hw_forecast(M4_monthly[[4517]]$x,1,180,18)

#

#### Compute all types of forecasts for a single time series ####
compute_all_forecasts <- function(time_series,optimal_lambda,start_index,forecast_length,M4_monthly_id,M4_id){
  # Purpose:    This function computes the forecasts across all models for a given time series
  
  # Input:      Time series, optimal lambda parameter, starting index, desired forecast length
  #             and two ID numbers (an index of M4 monthly, and index of just the M4) to identify the time series
  # Output:     A dataframe containing all of the forecasts (18 for monthly series),
  #             plus upper and lower CIs
  print(paste('M4 Monthly ID',M4_monthly_id))
  
  arima_model <- arima_forecast(time_series, optimal_lambda,start_index)
  rw_model <- rw_forecast(time_series, optimal_lambda,start_index)
  rwdrift_model <- rwdrift_forecast(time_series,optimal_lambda,start_index,forecast_length)
  hw_model <- hw_forecast(time_series,optimal_lambda,start_index,forecast_length)
  
  arima_forecasted_values <- forecast(arima_model,h=forecast_length,level=c(95))
  rw_forecasted_values <- forecast(rw_model,h=forecast_length,level=c(95))
  rwdrift_forecasted_values <- forecast(rwdrift_model,h=forecast_length,level=c(95))
  hw_forecasted_values <- forecast(hw_model,h=forecast_length,level=c(95))
  
  # In case where model has just forecasted a constant value with 0, the variance
  # is incredibly small...this checks to see if this is happening
  if((arima_model$sigma2)<0.0000001){
    arima_model <- arima_forecast(time_series, optimal_lambda,1)
    arima_forecasted_values <- forecast(arima_model,h=forecast_length,level=c(95))
  }
  
  if(rw_model$sigma2<0.0000001){
    rw_model <- rw_forecast(time_series, optimal_lambda,1)
    rw_forecasted_values <- forecast(rw_model,h=forecast_length,level=c(95))
  }
  
  if(rwdrift_model$model$sigma2<0.0000001){
    rwdrift_model <- rwdrift_forecast(time_series,optimal_lambda,1,forecast_length)
    rwdrift_forecasted_values <- forecast(rwdrift_model,h=forecast_length,level=c(95))
  }
  
  if(hw_model$model$sigma2<0.0000001){
    hw_model <- hw_forecast(time_series,optimal_lambda,1,forecast_length)
    hw_forecasted_values <- forecast(hw_model,h=forecast_length,level=c(95))
  }
  
  forecast_df <- data.frame(matrix(,nrow=12,ncol=18))
  
  # Fill values of the dataframe with forecasts
  forecast_df[1,]=replace(arima_forecasted_values$mean,arima_forecasted_values$mean<0,0)
  forecast_df[2,]=replace(arima_forecasted_values$lower,arima_forecasted_values$lower<0,0)
  forecast_df[3,]=replace(arima_forecasted_values$upper,arima_forecasted_values$upper<0,0)
  
  forecast_df[4,]=replace(rw_forecasted_values$mean,rw_forecasted_values$mean<0,0)
  forecast_df[5,]=replace(rw_forecasted_values$lower,rw_forecasted_values$lower<0,0)
  forecast_df[6,]=replace(rw_forecasted_values$upper,rw_forecasted_values$upper<0,0)
  
  forecast_df[7,]=replace(rwdrift_forecasted_values$mean,rwdrift_forecasted_values$mean<0,0)
  forecast_df[8,]=replace(rwdrift_forecasted_values$lower,rwdrift_forecasted_values$lower<0,0)
  forecast_df[9,]=replace(rwdrift_forecasted_values$upper,rwdrift_forecasted_values$upper<0,0)
  
  forecast_df[10,]=replace(hw_forecasted_values$mean,hw_forecasted_values$mean<0,0)
  forecast_df[11,]=replace(hw_forecasted_values$lower,hw_forecasted_values$lower<0,0)
  forecast_df[12,]=replace(hw_forecasted_values$upper,hw_forecasted_values$upper<0,0)
  
  # Add column showing what the M4 series is
  forecast_df['TS ID Number']=M4_monthly_id
  forecast_df['M4 ID Number']=M4_id
  
  # Add a column which says what the row represents
  forecast_df['Description'] = c('Arima Mean','Arima Lower','Arima Upper',
                                 'RW Mean','RW Lower','RW Upper',
                                 'RWDrift Mean','RWDrift Lower','RWDrift Upper',
                                 'HW Mean','HW Lower','HW Upper') 
  
  # Add a column showing if optimal arima model has drift
  forecast_df['OptARIMADrift'] = arima_model$coef['drift']
  
  # Column for optimal ARIMA p,d,q,P,D,Q,s
  forecast_df['OptArimap'] = arima_model$arma[1]
  forecast_df['OptArimaq'] = arima_model$arma[2]
  forecast_df['OptArimaP'] = arima_model$arma[3]
  forecast_df['OptArimaQ'] = arima_model$arma[4]
  forecast_df['OptArimas'] = arima_model$arma[5]
  forecast_df['OptArimad'] = arima_model$arma[6]
  forecast_df['OptArimaD'] = arima_model$arma[7]
  
  # Plot the ARIMA forecast
  par(mfrow=c(3,2))
  plot(time_series,main=paste('Full TS',M4_id))
  plot(arima_forecasted_values,main=paste('M4 ID','ARIMA',M4_id,M4_monthly_id))
  plot(rw_forecasted_values,main=paste('M4 ID','RW',M4_id))
  plot(rwdrift_forecasted_values,main=paste('M4 ID','RWDRIFT',M4_id))
  plot(hw_forecasted_values,main=paste('M4 ID','HW',M4_id))
  
  return(forecast_df)
  
}

#### Compute Forecasts for Multiple Time Series in the M4 dataset ####
M4_forecast_generator <- function(M4_time_series,start_series,end_series){
  
  # A dataframe where we will hold all of the forecasts
  master_forecast_df <- data.frame()
  
  # Loop through time series
  for(series_index in seq(start_series,end_series)){
    time_series <- M4_time_series[[series_index]]$x
    optimal_lambda <- optimal_transformation(time_series)
    changepoint_list <- identify_changepoints(time_series,optimal_lambda)
    start_index <- determine_starting_index(time_series,changepoint_list)
    forecast_length <- M4_time_series[[series_index]]$h
    
    series_forecast_df <- compute_all_forecasts(time_series,optimal_lambda,start_index,
                                                forecast_length,series_index,M4_time_series[[series_index]]$st)
    
    series_forecast_df['NumChangepoints'] = length(changepoint_list)
    series_forecast_df['StartIndex'] = start_index
    series_forecast_df['TSLength'] = length(time_series)
    series_forecast_df['OptLambda'] = optimal_lambda
    
    
    # Append the forecast df for this individual time series to the master df
    master_forecast_df <- rbind(master_forecast_df,series_forecast_df)
    
  }

  return(master_forecast_df)  
}

# test_forecast_gen <- M4_forecast_generator(M4_monthly,1,5)

#

library(foreach)
library(doParallel)
cl <- parallel::makeCluster(22)
registerDoParallel(cl,cores=22)

foreach (i=1:44,.packages=c('M4comp2018','forecast','changepoint',
                           'lmtest','tsfeatures'),
         .inorder=FALSE) %dopar% {
  setwd("/u/veitch")
  print(paste("Start Series",i,"Step Size", 1000))
  forecasted_df <- M4_forecast_generator(M4_monthly,(i*1000)-999,i*1000)
  write.csv(forecasted_df,file=paste((i*1000)-999,'-',i*1000,'.csv',sep=''), row.names = FALSE)
}

# Do tail end 44001-44609
setwd("/u/veitch")
print(paste("Start Series",i,"Step Size", 1000))
forecasted_df <- M4_forecast_generator(M4_monthly,44001,44609)
write.csv(forecasted_df,file=paste(44001,'-',44609,'.csv',sep=''), row.names = FALSE)

parallel::stopCluster(cl)

