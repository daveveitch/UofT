setwd("C:/Users/David/Google Drive/Documents/UofT/MSc/Time Series Reading Course/Project/Code")
perf_data <- read.csv('ForecastResultssMAPEMASE.csv', header = TRUE, sep = ",")

# Delete first row of the dataframe which is a placeholder
perf_data <- perf_data[-1,]

# Example subsetting the dataframe
# perf_data[(perf_data$Method=='RW')&(perf_data$Measure=='MASE'),]

perf_1 <- c(perf_data[(perf_data$Method=='ARIMA')&(perf_data$Measure=='sMAPE'),][,'Value'])
perf_2 <- c(perf_data[(perf_data$Method=='HW')&(perf_data$Measure=='sMAPE'),][,'Value'])

hist(perf_2-perf_1)

p_1 <- perf_data[(perf_data$Method=='RW')&(perf_data$Measure=='sMAPE'),]

p_1[order(p_1$Value,decreasing=TRUE),]
