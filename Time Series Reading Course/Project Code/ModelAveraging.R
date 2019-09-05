library(rpart)
library(rpart.plot)
library(dplyr)

#### Create train/val/test set ####
setwd("C:/Users/David/Google Drive/Documents/UofT/MSc/Time Series Reading Course/Project/Code") 
forecasted_results <- read.csv('MasterForecast.csv', header = TRUE, sep = ",")
performance_df <- read.csv('ForecastPerformance.csv', header = TRUE, sep = ",")
ts_feature_df <- read.csv('TimeSeriesFeatures.csv', header = TRUE, sep = ",")

# Naive 2 MASE and MAPE # 
# Use these to evaluate relative sMAPE and MASE #
# From https://github.com/M4Competition/M4-methods EvaluationandRanks.csv #
naive2_MASE <- 1.063
naive2_sMAPE <- .14427

perform_owa_df <- data.frame('M4 ID'=performance_df[(performance_df$Measure=='sMAPE')&(performance_df$Method=='ARIMA'),]$M4ID,
                             'TS.ID.Number'=performance_df[(performance_df$Measure=='sMAPE')&(performance_df$Method=='ARIMA'),]$TSID,
                             'ARIMA MASE'=performance_df[(performance_df$Measure=='MASE')&(performance_df$Method=='ARIMA'),]$Value,
                            'ARIMA sMAPE'=performance_df[(performance_df$Measure=='sMAPE')&(performance_df$Method=='ARIMA'),]$Value,
                             'RW MASE'=performance_df[(performance_df$Measure=='MASE')&(performance_df$Method=='RW'),]$Value,
                             'RW sMAPE'=performance_df[(performance_df$Measure=='sMAPE')&(performance_df$Method=='RW'),]$Value,
                             'RWDRIFT MASE'=performance_df[(performance_df$Measure=='MASE')&(performance_df$Method=='RWDrift'),]$Value,
                             'RWDRIFT sMAPE'=performance_df[(performance_df$Measure=='sMAPE')&(performance_df$Method=='RWDrift'),]$Value,
                             'HW MASE'=performance_df[(performance_df$Measure=='MASE')&(performance_df$Method=='HW'),]$Value,
                             'HW sMAPE'=performance_df[(performance_df$Measure=='sMAPE')&(performance_df$Method=='HW'),]$Value)

perform_owa_df['ARIMA OWA'] <- 0.5*(perform_owa_df$ARIMA.MASE/naive2_MASE)+0.5*(perform_owa_df$ARIMA.sMAPE/naive2_sMAPE)
perform_owa_df['RW OWA'] <- 0.5*(perform_owa_df$RW.MASE/naive2_MASE)+0.5*(perform_owa_df$RW.sMAPE/naive2_sMAPE)
perform_owa_df['RWDRIFT OWA'] <- 0.5*(perform_owa_df$RWDRIFT.MASE/naive2_MASE+perform_owa_df$RWDRIFT.sMAPE/naive2_sMAPE)
perform_owa_df['HW OWA'] <- 0.5*(perform_owa_df$HW.MASE/naive2_MASE+perform_owa_df$HW.sMAPE/naive2_sMAPE)

perform_owa_df <- merge(ts_feature_df,perform_owa_df,by='TS.ID.Number')

# create a column with the minimum OWA for each row
min_owa <- select(perform_owa_df,'ARIMA OWA','RW OWA','RWDRIFT OWA','HW OWA')
min_owa <- apply(min_owa,1,min)

perform_owa_df['MinOWA'] <- min_owa

# Vector of the best models found
best_model <- c()
for(i in seq(1,480000)){
  row <- perform_owa_df[i,]
  min_owa <- row$MinOWA 
  
  if(row$`ARIMA OWA`==min_owa){
    best_model <- append(best_model,'ARIMA')
  } else if(row$`RW OWA`==min_owa){
    best_model <- append(best_model,'RW') 
  } else if(row$`RWDRIFT OWA`==min_owa){
    best_model <- append(best_model,'RWDRIFT')
  } else if(row$`HW OWA`==min_owa){
    best_model <- append(best_model,'HW')
  }
}

perform_owa_df['BestModel'] <- best_model
perform_owa_df['BestModel'] <- as.factor(perform_owa_df$BestModel)

perform_owa_df['EqualWeightOWA'] <- (.25*perform_owa_df$`ARIMA OWA`+
                                     .25*perform_owa_df$`RW OWA`+
                                     .25*perform_owa_df$`RWDRIFT OWA`+
                                     .25*perform_owa_df$`HW OWA`)

# Train/Test/Val #
train_df <- perform_owa_df[1:24000,]
val_df <- perform_owa_df[24001:36000,]
test_df <- perform_owa_df[36001:48000,]



opt_criteria <- ''
opt_max_depth <- 0
best_mean_owa <- 10

for(i in c(2,3,4,5)){
  for(crit in c('gini','information')){
    tree <- rpart(BestModel ~ x_acf1 + diff1_acf1 + diff2_acf1 +
                    x_acf10 + diff1_acf10 + diff2_acf10 + seas_acf1 +
                    entropy,
                  data = train_df,minsplit=4,minbucket=240,maxdepth=i,
                  method='class',cp=-1,
                  parms = list(split = crit))
    rpart.plot(tree,box.palette='Blues')
    
    predicted_probs <- predict(tree, newdata = val_df, type='prob')
    
    mean_owa <- mean(rowSums(predicted_probs*val_df[,c('ARIMA OWA','RW OWA','RWDRIFT OWA','HW OWA')]))  
    
    print(paste('Max depth',i,'Mean OWA',mean_owa,'Criteria',crit))  
    
    # Check if we have found an optimum
    if(mean_owa < best_mean_owa){
      opt_criteria <- crit
      opt_max_depth <- i
      best_mean_owa <- mean_owa
    }
    
  }

}

print(paste('On validation set we find the optimal depth to be',opt_max_depth,
            'with criteria',opt_criteria,
            'producing a mean owa of',best_mean_owa))

# Run on test set
tree <- rpart(BestModel ~ x_acf1 + diff1_acf1 + diff2_acf1 +
                x_acf10 + diff1_acf10 + diff2_acf10 + seas_acf1 +
                entropy,
              data = train_df,minsplit=4,minbucket=240,maxdepth=opt_max_depth,
              method='class',cp=-1,
              parms = list(split = opt_criteria))

predicted_probs <- predict(tree, newdata = test_df, type='prob')

mean_owa <- mean(rowSums(predicted_probs*test_df[,c('ARIMA OWA','RW OWA','RWDRIFT OWA','HW OWA')]))  

print(paste('Max depth',i,'Mean OWA',mean_owa))  


predicted_probs <- predict(tree, newdata = perform_owa_df, type='prob')

mean_owa <- mean(rowSums(predicted_probs*perform_owa_df[,c('ARIMA OWA','RW OWA','RWDRIFT OWA','HW OWA')]))  

print(paste('Max depth',i,'Mean OWA',mean_owa))  

optimal_weights <- data.frame(predicted_probs)
optimal_weights['M4 ID'] <- perform_owa_df$M4.ID.x
optimal_weights['TS ID Number'] <- perform_owa_df$TS.ID.Number

setwd("C:/Users/David/Google Drive/Documents/UofT/MSc/Time Series Reading Course/Project/Code") 
write.csv(optimal_weights,file='OptimalWeights.csv', row.names = FALSE)




