library(quantreg)

# SET WD OF DATASET
# setwd('')

precip_data=read.csv('maxweeklyprecip36countries.csv')
seasonal_adjustments=read.csv('seasonaladjustments36countriesprecip.csv')

countries_to_test=names(precip_data[3:(dim(precip_data)[2]-1)])

# New column dummy variable
precip_data[,'four_week_period_1']=1*(precip_data$four_week_period==1)
precip_data[,'four_week_period_2']=1*(precip_data$four_week_period==2)
precip_data[,'four_week_period_3']=1*(precip_data$four_week_period==3)
precip_data[,'four_week_period_4']=1*(precip_data$four_week_period==4)
precip_data[,'four_week_period_5']=1*(precip_data$four_week_period==5)
precip_data[,'four_week_period_6']=1*(precip_data$four_week_period==6)
precip_data[,'four_week_period_7']=1*(precip_data$four_week_period==7)
precip_data[,'four_week_period_8']=1*(precip_data$four_week_period==8)
precip_data[,'four_week_period_9']=1*(precip_data$four_week_period==9)
precip_data[,'four_week_period_10']=1*(precip_data$four_week_period==10)
precip_data[,'four_week_period_11']=1*(precip_data$four_week_period==11)
precip_data[,'four_week_period_12']=1*(precip_data$four_week_period==12)
precip_data[,'four_week_period_13']=1*(precip_data$four_week_period==13)

# create a new column to be the affect of time on the four_week_period for quantile regression
precip_data[,'four_week_period_linear_1']=0
precip_data[,'four_week_period_linear_2']=0
precip_data[,'four_week_period_linear_3']=0
precip_data[,'four_week_period_linear_4']=0
precip_data[,'four_week_period_linear_5']=0
precip_data[,'four_week_period_linear_6']=0
precip_data[,'four_week_period_linear_7']=0
precip_data[,'four_week_period_linear_8']=0
precip_data[,'four_week_period_linear_9']=0
precip_data[,'four_week_period_linear_10']=0
precip_data[,'four_week_period_linear_11']=0
precip_data[,'four_week_period_linear_12']=0
precip_data[,'four_week_period_linear_13']=0

precip_data[precip_data$four_week_period==1,'four_week_period_linear_1']=precip_data[precip_data$four_week_period==1,'year']-1979
precip_data[precip_data$four_week_period==2,'four_week_period_linear_2']=precip_data[precip_data$four_week_period==2,'year']-1979
precip_data[precip_data$four_week_period==3,'four_week_period_linear_3']=precip_data[precip_data$four_week_period==3,'year']-1979
precip_data[precip_data$four_week_period==4,'four_week_period_linear_4']=precip_data[precip_data$four_week_period==4,'year']-1979
precip_data[precip_data$four_week_period==5,'four_week_period_linear_5']=precip_data[precip_data$four_week_period==5,'year']-1979
precip_data[precip_data$four_week_period==6,'four_week_period_linear_6']=precip_data[precip_data$four_week_period==6,'year']-1979
precip_data[precip_data$four_week_period==7,'four_week_period_linear_7']=precip_data[precip_data$four_week_period==7,'year']-1979
precip_data[precip_data$four_week_period==8,'four_week_period_linear_8']=precip_data[precip_data$four_week_period==8,'year']-1979
precip_data[precip_data$four_week_period==9,'four_week_period_linear_9']=precip_data[precip_data$four_week_period==9,'year']-1979
precip_data[precip_data$four_week_period==10,'four_week_period_linear_10']=precip_data[precip_data$four_week_period==10,'year']-1979
precip_data[precip_data$four_week_period==11,'four_week_period_linear_11']=precip_data[precip_data$four_week_period==11,'year']-1979
precip_data[precip_data$four_week_period==12,'four_week_period_linear_12']=precip_data[precip_data$four_week_period==12,'year']-1979
precip_data[precip_data$four_week_period==13,'four_week_period_linear_13']=precip_data[precip_data$four_week_period==13,'year']-1979

# Create matrix of which will be validation results for different countries
regularization_parameters=c(.25,.5,2**seq(1,15))
val_results=matrix(nrow=length(countries_to_test),ncol=length(regularization_parameters))
colnames(val_results)=regularization_parameters
rownames(val_results)=countries_to_test

# A common x_matrix used for all of the regressions
x_matrix=precip_data[1:13140,c("four_week_period_1", "four_week_period_2","four_week_period_3",
                               "four_week_period_4", "four_week_period_5","four_week_period_6",
                               "four_week_period_7", "four_week_period_8","four_week_period_9", 
                               "four_week_period_10", "four_week_period_11","four_week_period_linear_12", "four_week_period_13",
                               "four_week_period_linear_1", "four_week_period_linear_2","four_week_period_linear_3",
                               "four_week_period_linear_4", "four_week_period_linear_5","four_week_period_linear_6",
                               "four_week_period_linear_7", "four_week_period_linear_8","four_week_period_linear_9", 
                               "four_week_period_linear_10", "four_week_period_linear_11","four_week_period_linear_12", "four_week_period_linear_13")]

# Note when the x matrix is duplicated it increases the chances that when fitting
# quantile regression you get a singularity, so here I am adding a tiny amount of noise
# to the design matrix to get around this...will see in validation is this works appropriately
jitter_x=matrix(data=rnorm(341640,sd=.0001),nrow=13140)
x_matrix=x_matrix+jitter_x

x_val=precip_data[13140:14965,c("four_week_period_1", "four_week_period_2","four_week_period_3",
                                "four_week_period_4", "four_week_period_5","four_week_period_6",
                                "four_week_period_7", "four_week_period_8","four_week_period_9", 
                                "four_week_period_10", "four_week_period_11","four_week_period_linear_12", "four_week_period_13",
                                "four_week_period_linear_1", "four_week_period_linear_2","four_week_period_linear_3",
                                "four_week_period_linear_4", "four_week_period_linear_5","four_week_period_linear_6",
                                "four_week_period_linear_7", "four_week_period_linear_8","four_week_period_linear_9", 
                                "four_week_period_linear_10", "four_week_period_linear_11","four_week_period_linear_12", "four_week_period_linear_13")]

jitter_x_val=matrix(data=rnorm(47476,sd=.0001),nrow=1826)
x_val=x_val+jitter_x_val

for(i in seq(1,length(countries_to_test))){
  for(j in seq(1,length(regularization_parameters))){
    reg_param=regularization_parameters[j]
    country=countries_to_test[i]
    
    # Take first 36 years, and use last 5 for cross validation
    y=precip_data[1:13140,country]

    fit=rq.fit.lasso(as.matrix(x_matrix), y, tau = 0.965, 
                     lambda = c(rep(0,13),rep(reg_param,13)), beta = .99995, eps = 1e-06)
    
    y_val=precip_data[13140:14965,country]
    
    # create a matrix of same size as x_val to multiply x_val by to estimate quantiles
    qr_coefs=matrix(data=rep(fit$coefficients,dim(x_val)[1]),
                    nrow=dim(x_val)[1],
                    ncol=dim(x_val)[2],
                    byrow=TRUE)
    est_quantiles=rowSums(qr_coefs*x_val)
    
    val_results[i,j]=mean(est_quantiles<y_val)
  }
  print(paste(country,'DONE'))
}

for(i in seq(1,length(countries_to_test))){
  plot(val_results[i,],main=countries_to_test[i],xaxp  = c(1, 17, 16))
}

heatmap(val_results,Colv=NA,Rowv=NA)

# For choosing optimal parameter, choose lowest regularization that gives a validation
# accuracy that is somewhat OK
optimal_regularization=data.frame(country=countries_to_test,
                                  lambda=c(.25,#afghan
                                           64, #angola
                                           8, #bangladesh
                                           .25, #benin
                                           32, #bhutan
                                           .25, #burkina faso
                                           .25, # Burundi
                                           .25, # Cambodia
                                           64, # central african republic
                                           256, # chad
                                           256, #drc
                                           256, # djibouti
                                           512, #eritrea
                                           128, #ethiopia
                                           128, # gambia
                                           64, #guinea
                                           .25, #guinea bissau
                                           128, #haiti
                                           128, # Laos
                                           256, #lesotho  
                                           128, #liberia
                                           64, #madagascar
                                           256, #malawi
                                           .25, #mali
                                           256, #mozambique
                                           256, #nepal
                                           128, #niger
                                           .25, #rwanda
                                           .25, #somalia
                                           16, #south sudan
                                           1024, #sudan
                                           .25, #togo
                                           .25, #uganda
                                           512, # tanzania
                                           512, #zambia
                                           512
                                  ))


# Get coefficients from quantile regression with optimal reg paramaetrs
final_qr_coefficients=matrix(data=NA,nrow=dim(optimal_regularization)[1],ncol=26)

x_matrix=precip_data[,c("four_week_period_1", "four_week_period_2","four_week_period_3",
                        "four_week_period_4", "four_week_period_5","four_week_period_6",
                        "four_week_period_7", "four_week_period_8","four_week_period_9", 
                        "four_week_period_10", "four_week_period_11","four_week_period_linear_12", "four_week_period_13",
                        "four_week_period_linear_1", "four_week_period_linear_2","four_week_period_linear_3",
                        "four_week_period_linear_4", "four_week_period_linear_5","four_week_period_linear_6",
                        "four_week_period_linear_7", "four_week_period_linear_8","four_week_period_linear_9", 
                        "four_week_period_linear_10", "four_week_period_linear_11","four_week_period_linear_12", "four_week_period_linear_13")]

for(i in seq(1,dim(optimal_regularization)[1])){
  reg_param=optimal_regularization[i,2]
  country=optimal_regularization[i,1]
  
  y=precip_data[,country]
  
  fit=rq.fit.lasso(as.matrix(x_matrix), y, tau = 0.965, 
                     lambda = c(rep(0,13),rep(optimal_regularization[i,2],13)), beta = .99995, eps = 1e-06)
  
  final_qr_coefficients[i,]=fit$coefficients

}

hm_df=data.frame(country=character(),week_period_4=numeric(),coefficient=numeric())
hm_df[1,]=c('a',0,0)

for(i in seq(1,length(countries_to_test))){
  country_name=countries_to_test[i]
  temp_df=data.frame(country=rep(country_name,13),week_period_4=seq(1,13),coefficient=final_qr_coefficients[i,14:26])
  hm_df=rbind(hm_df,temp_df)
}
hm_df=hm_df[-1,]
hm_df$coefficient=as.numeric(hm_df$coefficient)
hm_df$week_period_4=as.numeric(hm_df$week_period_4)

ggplot(data=hm_df,aes(x=week_period_4,y=country,fill=coefficient)) + 
  geom_tile()+
  xlab('4 Week Period')+
  ylab('Country')+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1),
                       name = "mm/yr change")+
  ggtitle('Forecasted Yearly Increase \nMaximum Rainfall Experienced')+
  scale_x_continuous(breaks=seq(1,13), labels=1:13)+
  theme(plot.title = element_text(hjust = 0.5,size=10))+
  theme(axis.text.y = element_text(size=8))

# Top 10% of months with high precipitation (measured at end)
ten_pct_quantile=quantile(final_qr_coefficients[,1:13]+41*final_qr_coefficients[,14:26],.9)

# Create list of countries and 4 week period where the max exceeds
# this ten pct quantile
high_rain_periods=data.frame(country_name='a',four_week_period=0)
high_rain_periods_matrix=matrix(data=0,nrow=36,ncol=13)

for(i in seq(1,36)){
  for(j in seq(1,13)){
    # i=row of country
    # j=column corresponding to 
    if((final_qr_coefficients[i,j]+41*final_qr_coefficients[i,j+13])>ten_pct_quantile){
      high_rain_periods=rbind(high_rain_periods,c(countries_to_test[i],j))
      high_rain_periods_matrix[i,j]=1
    }
    
  }
  
}
high_rain_periods=high_rain_periods[-1,]

# Plot high rain periods with increasing rain
hm_df_2=data.frame(country=character(),week_period_4=numeric(),coefficient=numeric())
hm_df_2[1,]=c('a',0,0)

for(i in seq(1,length(countries_to_test))){
  country_name=countries_to_test[i]
  temp_df=data.frame(country=rep(country_name,13),week_period_4=seq(1,13),coefficient=round(final_qr_coefficients[i,14:26]*high_rain_periods_matrix[i,],4))
  hm_df_2=rbind(hm_df_2,temp_df)
}
hm_df_2=hm_df_2[-1,]
hm_df_2$coefficient=as.numeric(hm_df_2$coefficient)
hm_df_2$week_period_4=as.numeric(hm_df_2$week_period_4)

ggplot(data=hm_df_2,aes(x=week_period_4,y=country,fill=1*(coefficient>0.1))) + 
  geom_tile()+
  xlab('4 Week Period')+
  ylab('Country')+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1))+
  ggtitle('4 Week Periods With High and Increasing Rain')+
  scale_x_continuous(breaks=seq(1,13), labels=1:13)+
  theme(legend.position='none')+
  theme(plot.title = element_text(hjust = 0.5,size=10))+
  theme(axis.text.y = element_text(size=8))

# create df of number of months that have high rain and are increasing
# per coutnry
month_df=hm_df_2[(hm_df_2$coefficient>.1),]
table(month_df$country)


# Plot of two countries val results
val_plot_df=data.frame(reg_amount=c('.25','.5','2','4','8','16','32','64',
                                    '128','256','512','1024','2048','4096',
                                    '8192','16384','32768'),
                       Eritrea=val_results['Eritrea',],
                       Bangladesh=val_results['Bangladesh',],
                       reg_amount_2=regularization_parameters)


p_6=ggplot(data=val_plot_df,aes(x=reg_amount_2))+
  scale_x_continuous(trans='log2')+
  geom_line(aes(y=Eritrea,col='.95 Percentile'))+
  xlab('Regularization Amount')+
  ylab('% Validation Set Observations\nExceeding Quantile')+
  ggtitle('Eritrea Validation\n(Red=Error,Green=Ideal,Black=Lambda)')+
  geom_vline(xintercept=512)+
  geom_hline(yintercept=.035,color='green')+
  scale_y_continuous(breaks=seq(-.05,.55,by=.05))+
  theme(plot.title = element_text(hjust = 0.5,size=8),legend.position='none')

p_7=ggplot(data=val_plot_df,aes(x=reg_amount_2))+
  scale_x_continuous(trans='log2')+
  geom_line(aes(y=Bangladesh,col='.95 Percentile'))+
  xlab('Regularization Amount')+
  ylab('% Validation Set Observations\nExceeding Quantile')+
  ggtitle('Bangladesh Validation\n(Red=Error,Green=Ideal,Black=Lambda)')+
  geom_hline(yintercept=.035,color='green')+
  geom_vline(xintercept=8)+
  ylim(0,.1)+
  theme(plot.title = element_text(hjust = 0.5,size=10),legend.position='none')

grid.arrange(p_6,p_7,ncol=2)
