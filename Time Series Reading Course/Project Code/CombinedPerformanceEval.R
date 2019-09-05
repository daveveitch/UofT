setwd("C:/Users/David/Google Drive/Documents/UofT/MSc/Time Series Reading Course/Project/Code") 
comb_forc_perf_df <- read.csv('CombinedForecastPerformance.csv', header = TRUE, sep = ",")

# Naive 2 MASE and MAPE # 
# Use these to evaluate relative sMAPE and MASE #
# From https://github.com/M4Competition/M4-methods EvaluationandRanks.csv #
naive2_MASE <- 1.063
naive2_sMAPE <- .14427

naive_avg_MASE_all <- mean(comb_forc_perf_df[(comb_forc_perf_df$Measure=='MASE')&
                    (comb_forc_perf_df$Method=='Naive')  ,]$Value)
naive_avg_sMAPE_all <- mean(comb_forc_perf_df[(comb_forc_perf_df$Measure=='sMAPE')&
                                          (comb_forc_perf_df$Method=='Naive')  ,]$Value)

optimal_avg_MASE_all<- mean(comb_forc_perf_df[(comb_forc_perf_df$Measure=='MASE')&
                                          (comb_forc_perf_df$Method=='Optimal')  ,]$Value)
optimal_avg_sMAPE_all<- mean(comb_forc_perf_df[(comb_forc_perf_df$Measure=='sMAPE')&
                                           (comb_forc_perf_df$Method=='Optimal')  ,]$Value)


naive_avg_MASE_test <- mean(comb_forc_perf_df[(comb_forc_perf_df$Measure=='MASE')&
                                               (comb_forc_perf_df$Method=='Naive'),][36000:48000,]$Value)
naive_avg_sMAPE_test <- mean(comb_forc_perf_df[(comb_forc_perf_df$Measure=='sMAPE')&
                                                (comb_forc_perf_df$Method=='Naive')  ,][36000:48000,]$Value)

optimal_avg_MASE_test<- mean(comb_forc_perf_df[(comb_forc_perf_df$Measure=='MASE')&
                                                (comb_forc_perf_df$Method=='Optimal')  ,][36000:48000,]$Value)
optimal_avg_sMAPE_test<- mean(comb_forc_perf_df[(comb_forc_perf_df$Measure=='sMAPE')&
                                                 (comb_forc_perf_df$Method=='Optimal')  ,][36000:48000,]$Value)


naive_owa_all <- .5*naive_avg_MASE_all/naive2_MASE+.5*naive_avg_sMAPE_all/naive2_sMAPE
optimal_owa_all <- .5*optimal_avg_MASE_all/naive2_MASE+.5*optimal_avg_sMAPE_all/naive2_sMAPE
naive_owa_test <- .5*naive_avg_MASE_test/naive2_MASE+.5*naive_avg_sMAPE_test/naive2_sMAPE
optimal_owa_test <- .5*optimal_avg_MASE_test/naive2_MASE+.5*optimal_avg_sMAPE_test/naive2_sMAPE
