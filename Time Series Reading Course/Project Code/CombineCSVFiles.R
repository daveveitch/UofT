# Combine all individual forecast CSVs into one big CSV file
setwd("C:/Users/David/Google Drive/Documents/UofT/MSc/Time Series Reading Course/Project/Code") 

big_df <- read.csv('1-1000.csv', header = TRUE, sep = ",")

### MAKE
### SURE
### TO
### ADJUST
### LOOP
### INDICIES
### FOR
### NEW
### FILE
for(i in seq(1001,48000,by=1000)){
  start_index = i
  end_index = i+999
  print(i)
  csv_name <- paste0(start_index,'-',end_index,'.csv')
  new_csv <- read.csv(csv_name, header = TRUE, sep = ",")
  big_df <- rbind(big_df,new_csv)
}


# write CSV
write.csv(big_df,file='MasterForecast.csv', row.names = FALSE)