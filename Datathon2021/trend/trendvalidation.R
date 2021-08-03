library(Matrix)
library(foreach)
library(doParallel)
library(plyr)
library(ggplot2)
require(gridExtra)

# SET DIRECTORIES
# data_dir=
# valid_dir= # directory of bandwidthvalidation.csv

setwd(data_dir)
ts_data=read.csv('maxweeklytemps36countries.csv')
seasonal_adjustments=read.csv('seasonaladjustments36countries.csv')


setwd(valid_dir)
valid_results=read.csv('bandwidthvalidation.csv')

countries=names(ts_data)[3:38]

bandwidth_to_test=unique(valid_results$V5)

# Vector of the average mean square error of the countries
# we are forecasting
avg_mean_sq=rep(0,length(bandwidth_to_test))
total_countries_accepted=rep(0,length(bandwidth_to_test))
count=1

for(b_n in bandwidth_to_test){
  total_mean_square=0
  total_countries=0
  
  for(country in countries){
    country_result_df=valid_results[(valid_results$V1==country)&
                                    (valid_results$V5)==b_n,]
    
    x=seq(1,2028)
    x_sq=x**2
    x_cu=x**3
    seasonally_adjusted_data=ts_data[,country]-seasonal_adjustments[,country]
    
    
    # Go through and fit the simplest model not rejected
    if(country_result_df[country_result_df$V3=='constant','V10']=='ACCEPT'){
      fit=lm(seasonally_adjusted_data[1:2028]~1)
      
      mean_square=sum(seasonally_adjusted_data[2029:2132]-fit$coefficients[1])**2
      total_mean_square=total_mean_square+mean_square
      total_countries=total_countries+1
      
    }else if(country_result_df[country_result_df$V3=='linear','V10']=='ACCEPT'){
      fit=lm(seasonally_adjusted_data[1:2028]~x)
      
      mean_square=sum(seasonally_adjusted_data[2029:2132]-
                        fit$coefficients[1]-
                        fit$coefficients[2]*seq(2029,2132))**2
      
      total_mean_square=total_mean_square+mean_square
      total_countries=total_countries+1
      
    }else if(country_result_df[country_result_df$V3=='quadratic','V10']=='ACCEPT'){
      fit=lm(seasonally_adjusted_data[1:2028]~x+x_sq)
      
      mean_square=sum(seasonally_adjusted_data[2029:2132]-
                        fit$coefficients[1]-
                        fit$coefficients[2]*seq(2029,2132)-
                        fit$coefficients[3]*seq(2029,2132)**2)**2
      
      total_mean_square=total_mean_square+mean_square
      total_countries=total_countries+1
      
    }else if(country_result_df[country_result_df$V3=='cubic','V10']=='ACCEPT'){
      # fit=lm(seasonally_adjusted_data[1:2028]~x+x_sq+x_cu)
      # 
      # mean_square=sum(seasonally_adjusted_data[2029:2132]-
      #                   fit$coefficients[1]-
      #                   fit$coefficients[2]*seq(2029,2132)-
      #                   fit$coefficients[3]*seq(2029,2132)**2-
      #                     fit$coefficients[4]*seq(2029,2132)**3)**2
      # 
      # total_mean_square=total_mean_square+mean_square
      # total_countries=total_countries+1
      
    }else{
      # if not constant/linear/quadratic/cubic dont model it
    }
    
    if(is.na(total_mean_square)){print(country)}  
  }
  

  
  avg_mean_sq[count]=total_mean_square/total_countries
  total_countries_accepted[count]=total_countries
  
  
  count=count+1
}

data_to_plot=data.frame(Average_Mean_Square=avg_mean_sq,
                        Bandwidth=bandwidth_to_test,
                        Total_Countries=total_countries_accepted)

p_1<-ggplot(data=data_to_plot,aes(x=Bandwidth,y=Average_Mean_Square))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ylab('Validation Error')+
  scale_colour_manual(name="Error Bars",values='red')+
  geom_vline(xintercept = .21, color = "blue", size=1)+
  geom_vline(xintercept = .23, color = "green", size=1)+
  ggtitle('Validation Results of Different Bandwidths \n (Green=Optimal, Blue=Author Suggested)')+
  theme(plot.title = element_text(hjust = 0.5,size=10))

p_2<-ggplot(data=data_to_plot,aes(x=Bandwidth,y=Total_Countries))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ylab('Validation Error')+
  scale_colour_manual(name="Error Bars",values='red')+
  geom_vline(xintercept = .21, color = "blue", size=1)+
  geom_vline(xintercept = .23, color = "green", size=1)+
  ggtitle('Number of Countries Where \n Parametric Trend Found\n (Green=Optimal, Blue=Author Suggested)')+
  theme(plot.title = element_text(hjust = 0.5,size=10))

grid.arrange(p_1, p_2, ncol=2)
