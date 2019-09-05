setwd("C:/Users/David/Google Drive/Documents/UofT/MSc/Time Series Reading Course/Project/Code")
comb_forc <- read.csv('ForecastPerformance.csv',header=TRUE,sep=",")

naive2_MASE <- 1.063
naive2_sMAPE <- .14427

arima_mase_all <- mean(comb_forc[(comb_forc$Measure=='MASE')&(comb_forc$Method=='ARIMA'),]$Value)
arima_sMAPE_all <- mean(comb_forc[(comb_forc$Measure=='sMAPE')&(comb_forc$Method=='ARIMA'),]$Value)
arima_mase_test <- mean(comb_forc[(comb_forc$Measure=='MASE')&(comb_forc$Method=='ARIMA'),]$Value[36000:48000])
arima_sMAPE_test <- mean(comb_forc[(comb_forc$Measure=='sMAPE')&(comb_forc$Method=='ARIMA'),]$Value[36000:48000])
arima_owa_all <- .5*arima_mase_all/naive2_MASE+.5*arima_sMAPE_all/naive2_sMAPE
arima_owa_test <- .5*arima_mase_test/naive2_MASE+.5*arima_sMAPE_test/naive2_sMAPE

rw_mase_all <- mean(comb_forc[(comb_forc$Measure=='MASE')&(comb_forc$Method=='RW'),]$Value)
rw_sMAPE_all <- mean(comb_forc[(comb_forc$Measure=='sMAPE')&(comb_forc$Method=='RW'),]$Value)
rw_mase_test <- mean(comb_forc[(comb_forc$Measure=='MASE')&(comb_forc$Method=='RW'),]$Value[36000:48000])
rw_sMAPE_test <- mean(comb_forc[(comb_forc$Measure=='sMAPE')&(comb_forc$Method=='RW'),]$Value[36000:48000])
rw_owa_all <- .5*rw_mase_all/naive2_MASE+.5*rw_sMAPE_all/naive2_sMAPE
rw_owa_test <- .5*rw_mase_test/naive2_MASE+.5*rw_sMAPE_test/naive2_sMAPE

rwdrift_mase_all <- mean(comb_forc[(comb_forc$Measure=='MASE')&(comb_forc$Method=='RWDrift'),]$Value)
rwdrift_sMAPE_all <- mean(comb_forc[(comb_forc$Measure=='sMAPE')&(comb_forc$Method=='RWDrift'),]$Value)
rwdrift_mase_test <- mean(comb_forc[(comb_forc$Measure=='MASE')&(comb_forc$Method=='RWDrift'),]$Value[36000:48000])
rwdrift_sMAPE_test <- mean(comb_forc[(comb_forc$Measure=='sMAPE')&(comb_forc$Method=='RWDrift'),]$Value[36000:48000])
rwdrift_owa_all <- .5*rwdrift_mase_all/naive2_MASE+.5*rwdrift_sMAPE_all/naive2_sMAPE
rwdrift_owa_test <- .5*rwdrift_mase_test/naive2_MASE+.5*rwdrift_sMAPE_test/naive2_sMAPE

hw_mase_all <- mean(comb_forc[(comb_forc$Measure=='MASE')&(comb_forc$Method=='HW'),]$Value)
hw_sMAPE_all <- mean(comb_forc[(comb_forc$Measure=='sMAPE')&(comb_forc$Method=='HW'),]$Value)
hw_mase_test <- mean(comb_forc[(comb_forc$Measure=='MASE')&(comb_forc$Method=='HW'),]$Value[36000:48000])
hw_sMAPE_test <- mean(comb_forc[(comb_forc$Measure=='sMAPE')&(comb_forc$Method=='HW'),]$Value[36000:48000])
hw_owa_all <- .5*hw_mase_all/naive2_MASE+.5*hw_sMAPE_all/naive2_sMAPE
hw_owa_test <- .5*hw_mase_test/naive2_MASE+.5*hw_sMAPE_test/naive2_sMAPE