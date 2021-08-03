library(Matrix)
library(foreach)
library(doParallel)

# DIRECTORY WRITE LOCAL LINEAR WEIGHTS TO
# MM_dir=

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

n=2132
#n=2028

cl <- parallel::makeCluster(4,outfile="")
doParallel::registerDoParallel(cl)

foreach(b_n=seq(.1,.3,by=.01),.packages = "Matrix")%dopar%{
  
  print(paste(b_n,'beginning'))
  
  weight_vals=matrix(data=0,nrow=n,ncol=n)
  weight_vals=Matrix(weight_vals,sparse=TRUE)
  
  for(t in seq(1,n)){
    if(t%%100==0){
      print(paste(b_n,t))
      print(Sys.time())
    }
    weight_vals_row=rep(0,n)  
    if(t %in% seq(1,round(n*b_n+1))){
      for(i in seq(1,n)){
        weight_vals_row[i]=w_i(t,b_n,n,i)   
      }
    }else if(t %in% seq(round(n*b_n+1),n-round(n*b_n+1))){
      weight_vals_row=rep(0,n)
      weight_vals_row[2:n]=weight_vals[t-1,1:(n-1)]
    }else{
      # Index at start corresponding to this index
      flipped_index=n+1-t
      weight_vals_row=rev(as.numeric(weight_vals[flipped_index,]))
    }
    
    weight_vals[t,]=weight_vals_row
  }
  
  setwd(MM_dir)
  writeMM(weight_vals,paste(b_n,'weightsshort.MM',sep=''))  
  
}



