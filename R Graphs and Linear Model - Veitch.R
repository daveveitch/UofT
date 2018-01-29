setwd("C:/Users/David/Desktop/Datathon/Code")

# IMPORT LIBRARIES NEEDED
import_libraries <- function(){
  library(dplyr)
  library(chron)
  library(plotly)
  library(ggplot2)
  library(gridExtra)
}

import_libraries()

# Chart of Population/Cars in GTA

ont_population <- read.csv("ont_population.csv")
plot(ont_population$Year, ont_population$Toronto.Population/1000000, xlab = "Year", ylab="millions", 
     ylim = c(2, 10), col=c("red"), main="Population and Cars in the GTA")
points(ont_population$Year, ont_population$Estimated.Toronto.Vehicle.Registrations/1000000, col=c("blue"))
legend("bottomright","",c("Toronto Population (Statscan + Ontario Govt Projections)",
                          "Toronto Vehicle Registrations (Statscan +  Estimates)"), fill=c("red","blue"))

# Chart of Canadian Communiting Times
op <- par(mar = c(10,4,4,2) + 0.1)
commute_time <- read.csv("cad_commuting_time.csv")
barplot(commute_time$Average.time,names.arg=commute_time$CMA, 
        main="Commuting Times Across Canada (Statscan)",ylab="Average Commuting Time (Minutes)",las=2)

# Subset Road Impediments Data to Toronto
road_impediments <- read.csv("road_impediments.csv")
toronto_impediments <- subset(road_impediments, city=="Toronto" & average_monthly_vehicles>10000 & average_acceleration > 1.5)

# Fit a linear regression
truck_accel_fit <- lm(log(toronto_impediments$average_acceleration) ~ toronto_impediments$percent_hdt)
summary(truck_accel_fit)

# Plot graph
plot(toronto_impediments$percent_hdt,log(toronto_impediments$average_acceleration),
     xlab="% Heavy Duty Trucks",  ylab="Log Average Acceleration", 
     main="Toronto - Heavy Duty Trucks vs. Average Acceleration\n (Average Monthly Vehicles > 10,000, Average Acceleration > 1.5)\n log(Average Acceleration) = 0.48 + 0.48*PercentHeavyDutyTrucks", cex.main=.75 )
abline(truck_accel_fit)              


        

