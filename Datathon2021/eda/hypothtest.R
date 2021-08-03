rm(list = ls())
library(Matrix)
library(foreach)
library(doParallel)
library(plyr)

# SET PATH OF DATASET
#  data_dir=
# local_linear_dir= # Where .MM files of local linear smoother found made by locallinear.R


# Kernel Function
bartlett_kernel<-function(v){
  # input - v a sequence with each value in [-1,1]
  # output - a sequence of weights integrating to 1
  return(pmax(0,1-abs(v)))
}

w_i<-function(t,b_n,n,i){
  # input - t - the time in whole numbers
  #         b_n - the bandwidth 
  #         n - the sample size
  # output - w_i(t) a weight for time t
  
  t=t/n
  
  kernel_values=bartlett_kernel((seq(1,n)/n-t)/b_n)
  
  S_0_t=sum(kernel_values)
  S_1_t=sum((t-seq(1,n)/n)*kernel_values)
  S_2_t=sum((t-seq(1,n)/n)**2*kernel_values)
  
  w_i_t=kernel_values[i]*(S_2_t-(t-i/n)*S_1_t)/(S_2_t*S_0_t-S_1_t**2)
  
  return(w_i_t)
}

lr_variance_est<-function(residuals,n,b_n){
  # input
  # residuals - estimate of the TS residuals
  # n         - sample size
  # b_n       - bandwidth size
  # output
  # g - estimate of LR variance for every value of t
  
  g=rep(0,n)
  
  m_n=round((n*b_n)**(1/3)) # authors suggestion
  
  # Calculate values of Q_i
  Q=rep(0,n)
  for(i in seq(1,n)){
    # get indicies of residuals in the Q_i sum
    sum_indicies=seq(1,n)[abs(i-seq(1,n))<=m_n]
    Q[i]=residuals[i]*sum(residuals[sum_indicies])
  }
  
  for(t in seq(1,n)){
    indicator=(abs(seq(1,n)/n-t/n)<=(b_n))
    
    g[t]=sum(Q*indicator)/sum(indicator)
  }
 
  return(g) 
}

f_theta_hat_generate<-function(y_values,n,type_of_function){
  # input
  # y_values - data generating function for
  # n       - number of observations
  # type_of_function - 'constant', 'linear', 'quadratic', 'cubic'
  # output
  # a vector of length n representing a function of desired form
  if(type_of_function=='constant'){
    f_theta_hat=rep(mean(y_values),n)
  }else if(type_of_function=='linear'){
    x=seq(1,n)
    fit<-lm(y_values~x)
    f_theta_hat=rep(fit$coefficients[1],n)+x*fit$coefficients[2]  
  }else if(type_of_function=='quadratic'){
    x=seq(1,n)
    x_sq=seq(1,n)**2
    fit<-lm(y_values~x+x_sq)
    f_theta_hat=rep(fit$coefficients[1],n)+x*fit$coefficients[2]+x_sq*fit$coefficients[3]
  }else if(type_of_function=='cubic'){
    x=seq(1,n)
    x_sq=seq(1,n)**2
    x_cu=seq(1,n)**3
    fit<-lm(y_values~x+x_sq+x_cu)
    f_theta_hat=rep(fit$coefficients[1],n)+x*fit$coefficients[2]+x_sq*fit$coefficients[3]+x_cu*fit$coefficients[4]
    
  }
  return(as.numeric(f_theta_hat))
  
}

experiment_df=expand.grid(c('constant','linear','quadratic','cubic'), 
                          seq(.1,.3,by=.01),
                          c("Afghanistan", 
                            "Angola", "Bangladesh", "Benin", "Bhutan", "Burkina.Faso", "Burundi", 
                            "Cambodia", "Central.African.Republic", "Chad", "Democratic.Republic.of.the.Congo", 
                            "Djibouti", "Eritrea", "Ethiopia", "Gambia", "Guinea", "Guinea.Bissau", 
                            "Haiti", "Laos", "Lesotho", "Liberia", "Madagascar", "Malawi", 
                            "Mali", "Mozambique", "Nepal", "Niger", "Rwanda", "Somalia", 
                            "South.Sudan", "Sudan", "Togo", "Uganda", "United.Republic.of.Tanzania", 
                            "Zambia", "Yemen"))

cl <- parallel::makeCluster(4,outfile="")
doParallel::registerDoParallel(cl)

results_df=foreach(params=t(experiment_df), 
                   .packages = c("Matrix",'plyr'),
                   .combine='rbind')%dopar% {
  
  # Specify type of function want to test and country
  #type_of_function='quadratic'
  #country_to_test='Central.African.Republic'
  #b_n=0.18
  
  country_to_test=as.character(params[3])
  type_of_function=as.character(params[1])
  b_n=as.numeric(params[2])
  
  setwd(data_dir)
  ts_data=read.csv('maxweeklytemps36countries.csv')
  seasonal_adjustments=read.csv('seasonaladjustments36countries.csv')
  
  ts_data=ts_data[1:2132,country_to_test]-seasonal_adjustments[1:2132,country_to_test]
  
  # Set bandwidth
  n=length(ts_data)
  # b_n=n**(-1/5) # suggested by authors
  
  setwd(local_linear_dir)
  weight_vals=readMM(paste(b_n,'weights.MM',sep=''))
  
  # Get local linear estimator
  local_linear_est=weight_vals%*%as.matrix(ts_data,ncol=1)
  
  # Find estimated residuals using local linear estimate of mean
  e_tilde=as.numeric(ts_data-local_linear_est)
  
  # Get estimate of LR variance g
  g=lr_variance_est(e_tilde,n,b_n)
  
  # Get an estiamte for the LS minimizing parameter (given the 
  # parametric form you are testing)
  f_theta_hat=f_theta_hat_generate(ts_data,n,type_of_function)
  
  # Calculate e-hat using the fit
  e_hat=ts_data-f_theta_hat
  
  # Calculate test statistic
  mu_m_hat=weight_vals%*%as.matrix(f_theta_hat,ncol=1)
  mu_n_e_hat=local_linear_est-mu_m_hat
  ISE_m_hat=sum(mu_n_e_hat**2)
  
  ISE_bootstrap=rep(0,100)
  
  for(k in seq(1,length(ISE_bootstrap))){
    Z=rnorm(n)
    Y_i_diamond=g**(1/2)*Z
    
    # Calculate f-theta-hat
    f_theta_hat=f_theta_hat_generate(Y_i_diamond,n,type_of_function)
    
    e_hat_diamond=Y_i_diamond-f_theta_hat
    mu_m_hat_diamond=weight_vals%*%as.matrix(f_theta_hat,ncol=1)
    local_linear_est_diamond=weight_vals%*%as.matrix(Y_i_diamond,ncol=1)
    
    mu_n_e_hat_diamond=local_linear_est_diamond-mu_m_hat_diamond
    ISE_m_hat_diamond=sum(mu_n_e_hat_diamond**2)
    
    ISE_bootstrap[k]=ISE_m_hat_diamond
  }
  
  # Check quantile
  if(ISE_m_hat>as.numeric(round(quantile(ISE_bootstrap,.99),2))){
    accept_reject='REJECT'
  }else{
    accept_reject='ACCEPT'
  }
  
  result_vector=c(country_to_test,'Model Type',type_of_function,'Bandwidth',b_n,'Test Stat of',ISE_m_hat,'Cuttoff of',
                        as.numeric(round(quantile(ISE_bootstrap,.99),2)),accept_reject)

  t(as.data.frame(result_vector))
}


stopCluster(cl)

write.csv(results_df,file='results.csv')

# Create a matrix where each row is different values of t [1,n]
# and each col is different values of i [1,n]
# weight_vals=matrix(data=0,nrow=n,ncol=n)
# weight_vals=Matrix(weight_vals,sparse=TRUE)
# 
# for(t in seq(1,n)){
#   weight_vals_row=rep(0,n)
#   # Get weights
#   for(i in seq(1,n)){
#     weight_vals_row[i]=w_i(t,b_n,n,i)  
#   }
#   weight_vals[t,]=weight_vals_row
# }

# Example plotting Gamiba's nonparametric vs. parametric trend
gambia_local_linear_est=local_linear_est
gambia_parametric=f_theta_hat

gambia_data=data.frame(week=seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "weeks")[1:2132],
                       local_linear=gambia_local_linear_est[,1],
                       parametric=gambia_parametric,
                       raw_data=ts_data)

  
  
p_1<-ggplot(data=gambia_data,aes(x=week))+
  geom_line(aes(y=local_linear,col='Nonparametric\nEstimate'),size=2)+
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

# Example of different nonparametric trends
low_bw=local_linear_est[,1] # made with bw .06
high_bw=local_linear_est[,1] # made with bw .23

cv_graph_df=data.frame(week=seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "weeks")[1:2132],low_bw=low_bw,high_bw=high_bw)

p_2<-ggplot(data=cv_graph_df,aes(x=week))+
  geom_line(aes(y=low_bw,col='Bandwidth .06'),size=2)+
  geom_line(aes(y=high_bw,col='Bandwidth .23'),size=2)+
  xlab('Date')+
  ylab('Seasonally Adjusted\nMax Temperature (Celsius)')+
  ylim(c(-1,1.5))+
  scale_colour_manual(values=c("black","red","blue"))+
  labs(color = "")+
  ggtitle('Gambia\n Comparison of Estimated Trends')+
  theme(plot.title = element_text(hjust = 0.5,size=10))


p_2

