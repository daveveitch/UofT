library(Matrix)
library(foreach)
library(doParallel)
library(plyr)
library(ggplot2)
require(gridExtra)
library(ggrepel)
library(dplyr)

# SET WORKING DIRECTORIES
# data_dir=
# valid_dir= # directory of bandwidthvalidation.csv

setwd(data_dir)
ts_data=read.csv('maxweeklytemps36countries.csv')
seasonal_adjustments=read.csv('seasonaladjustments36countries.csv')


setwd(valid_dir)
valid_results=read.csv('bandwidthvalidation.csv')

countries=names(ts_data)[3:38]

# Bandwidth selected from corss validation
b_n=.23

countries_to_forecast=data.frame(country_name='a',trend='a')

for(country in countries){
  
  country_result_df=valid_results[(valid_results$V1==country)&
                                    (valid_results$V5)==b_n,]
  
  if(country_result_df[country_result_df$V3=='constant','V10']=='ACCEPT'){
    countries_to_forecast=rbind(countries_to_forecast,c(country,'constant'))
  }else if(country_result_df[country_result_df$V3=='linear','V10']=='ACCEPT'){
    countries_to_forecast=rbind(countries_to_forecast,c(country,'linear'))
  }else if(country_result_df[country_result_df$V3=='quadratic','V10']=='ACCEPT'){
    countries_to_forecast=rbind(countries_to_forecast,c(country,'quadratic')) 
  }else if(country_result_df[country_result_df$V3=='cubic','V10']=='ACCEPT'){
    #countries_to_forecast=rbind(countries_to_forecast,c(country,'cubic')) 
  }
}

countries_to_forecast=countries_to_forecast[-1,]

x=seq(1,2132)
x_sq=x**2
x_cu=x**3

#Glimpse of what trends will look like

for(i in seq(1,dim(countries_to_forecast)[1])){
  country=countries_to_forecast[i,1]
  trend_type=countries_to_forecast[i,2]

  seasonally_adjusted_data=ts_data[,country]-seasonal_adjustments[,country]

  if(trend_type=='constant'){
    fit=lm(seasonally_adjusted_data~1)
    trend_fit=rep(fit$coefficients[1],2652)
  }else if(trend_type=='linear'){
    fit=lm(seasonally_adjusted_data~x)
    trend_fit=rep(fit$coefficients[1],2652)+fit$coefficients[2]*seq(1,2652)
  }else if(trend_type=='quadratic'){
    fit=lm(seasonally_adjusted_data~x+x_sq)
    trend_fit=rep(fit$coefficients[1],2652)+fit$coefficients[2]*seq(1,2652)+
      fit$coefficients[3]*seq(1,2652)**2
  }else if(trend_type=='cubic'){
    fit=lm(seasonally_adjusted_data~x+x_sq+x_cu)
    trend_fit=rep(fit$coefficients[1],2652)+fit$coefficients[2]*seq(1,2652)+
    fit$coefficients[3]*seq(1,2652)**2+fit$coefficients[4]*seq(1,2652)**3
  }

  plot(trend_fit,main=country)
  abline(v=2132)
}


# Create a forecast
countries_to_forecast$avg_max=0
countries_to_forecast$chg_to_trend=0

# data.frame of trend fits
trend_collection=data.frame(week=1,forecast=2,country='c')
week_list=seq(as.Date("1980/1/1"), as.Date("2030/12/31"), "weeks")[1:2652]


for(i in seq(1,dim(countries_to_forecast)[1])){
  country=countries_to_forecast[i,1]
  trend_type=countries_to_forecast[i,2]
  
  seasonally_adjusted_data=ts_data[,country]-seasonal_adjustments[,country]
  seasonally_adjusted_forecasted=rep(0,2652)
  seasonally_adjusted_forecasted[1:2132]=seasonally_adjusted_data
  
  if(trend_type=='constant'){
    fit=lm(seasonally_adjusted_data~1)
    trend_fit=rep(fit$coefficients[1],2652)
  }else if(trend_type=='linear'){
    fit=lm(seasonally_adjusted_data~x)
    trend_fit=rep(fit$coefficients[1],2652)+fit$coefficients[2]*seq(1,2652)
    trend_collection=rbind(trend_collection,data.frame(week=week_list,
                                                       country=rep(country,2652),
                                                       forecast=trend_fit-trend_fit[1]))
  }else if(trend_type=='quadratic'){
    fit=lm(seasonally_adjusted_data~x+x_sq)
    trend_fit=rep(fit$coefficients[1],2652)+fit$coefficients[2]*seq(1,2652)+
      fit$coefficients[3]*seq(1,2652)**2
    trend_collection=rbind(trend_collection,data.frame(week=week_list,
                                                       country=rep(country,2652),
                                                       forecast=trend_fit-trend_fit[1]))
  }else if(trend_type=='cubic'){
    fit=lm(seasonally_adjusted_data~x+x_sq+x_cu)
    trend_fit=rep(fit$coefficients[1],2652)+fit$coefficients[2]*seq(1,2652)+
      fit$coefficients[3]*seq(1,2652)**2+fit$coefficients[4]*seq(1,2652)**3
    trend_collection=rbind(trend_collection,data.frame(week=week_list,
                                                       country=rep(country,2652),
                                                       forecast=trend_fit-trend_fit[1]))
  }
  
  # find average of max temperature over 28 hottest days for a country
  #countries_to_forecast[i,'avg_max']=mean(ts_data[,country])
  countries_to_forecast[i,'hottest_30']=quantile(ts_data[,country],.917)
  countries_to_forecast[i,'chg_to_trend']=trend_fit[2652]-trend_fit[2132]
  
}

trend_collection=trend_collection[-1,]


# # add in seasonally adjusted trend
# seasonally_adjusted_forecasted[2133:2652]=trend_fit[2133:2652]-trend_fit[2132]
# 
# # add back the seasonal adjustments
# country_seasonal_adjustments=seasonal_adjustments[1:52,country]
# country_seasonal_adjustments=rep(country_seasonal_adjustments,51)
# 
# # add back seasonals
# forecasted_data=seasonally_adjusted_forecasted+country_seasonal_adjustments
# 
# # estimate a noise sequence
# noise_fit=seasonally_adjusted_data[round((1-b_n)*2132):2132]
# arima_est=auto.arima(noise_fit)
# innovations=arima_est$residuals
# 
# noise_sim=arima.sim(arima_est$model,
#                     n=520,
#                     innov=sample(innovations,520,replace=TRUE))
# 
# add_noise=c(rep(0,2132),noise_sim)
# 
# plot(forecasted_data+add_noise)

### this add the labels to the points using their rownames
### font = 2 is bold

p_3=ggplot(countries_to_forecast, aes(x=hottest_30, y=chg_to_trend)) + 
  geom_point(size=1)+
  theme_minimal()+
  ylab('Forecasted 10yr Change in \nMax Temperature Trend')+
  xlab('30 Days a Year Hotter Than This Temperature')+
  geom_text(label=countries_to_forecast$country_name,size=2,
            nudge_y=.1,angle=90,hjust=0)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(c(26,44))+
  ylim(c(-1,2))+
  geom_hline(yintercept = 0, color = "black",linetype='dotted')
  
ggsave('atriskcountries.PNG',plot=p_3,device='png',width=4,height=3,units='in')
p_3


# countries with hottest 30 days over 35 degrees, note that
# Toronto sets extreme heat warnings when daytime max's >31 C and
# humidex expected to be over 40c so 35 seems OK
at_risk_countries=countries_to_forecast[(countries_to_forecast$hottest_30>35)&
                                        (countries_to_forecast$chg_to_trend>0),]

# add uganda since its forecasted change is so high


# plot of various trends

trend_collection$week=rep(seq(as.Date("1980/1/1"), as.Date("2030/12/31"), "weeks")[1:2652],16)

p_1<-ggplot() + 
  geom_line(data = trend_collection, aes(week, forecast, color = country))+
  geom_text(data = trend_collection %>% filter(week == last(week)), aes(label = country, 
                                                               x = week + 500, 
                                                               y = forecast, 
                                                               color = country,
                                                               ),size=3,hjust=0)+
  xlim(as.Date(c('1980/1/1', '2050/12/31'), format="%Y/%m/%d") )+
  guides(color = 'none') + theme_minimal()+
  ggtitle('Estimated Nonconstant Historical and Forecasted Trends\nSeasonally Adjusted Maximum Temperature')+
  theme(plot.title = element_text(hjust = 0.5,size=10))+
  geom_vline(xintercept =as.numeric(as.Date("2020/12/31")))+
  ylab('Trend in Max Temperature (Celsius)')+
  xlab('Date')+
  geom_rect(aes(xmin=as.Date("2020/12/31"), xmax=as.Date("2030/12/31"), ymin=-Inf, ymax=Inf),alpha=.4,fill='grey')

ggsave('trendtemps.PNG',plot=p_1,device='png')
p_1

+
  scale_x_continuous(breaks = scales::pretty_breaks(10))
  
  
  ggplot(trend_collection)+
  geom_line(aes(x = week, y = forecast, group = country, colour = country))+ 
  geom_text(data = subset(trend_collection, week == 22209), aes(label = country, colour = country, x = Inf, y = forecast), hjust = -.1)+ 
  scale_colour_discrete(guide = 'none')  
geom_line()+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme(legend.position = "none")

p_1

+
  geom_line(aes(y=parametric,col='Parametric\nEstimate'),size=2)+
  geom_point(aes(y=ts_data,col='Data'),size=.1)+
  xlab('Date')+
  ylab('Seasonally Adjusted\nMax Temperature (Celsius)')+
  ylim(c(-2,2))+
  scale_colour_manual(values=c("black","red","blue"))+
  labs(color = "")+
  ggtitle('Gambia\n Comparison of Estimated Trend')+
  theme(plot.title = element_text(hjust = 0.5,size=10))


p_1


