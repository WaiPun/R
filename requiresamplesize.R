required.size = function(delta1, sigma, rho = 0.5,alpha = 0.05, beta = 0.05){
  #############################################################################
  # This function calculates the required sample size for a trial, required the following inputs
  # Inputs: 
  # delta - difference required (no constraints).
  # sigma: pre-assumed standard deviation (>0).
  # rho: correlation (-1, 1).
  # alpha: significance (0,1) default 0.05.
  # beta: type II error (0,1) default 0.05.
  errorcode<-0
  errorstr<-c("Invalid delta1, delta1 must be a scalar."
              ,"sigma is not positive"
              ,"Invalid rho, rho must lie in (-1,1)"
              ,"Invalid alpha, significance level must lie in (0,1)."
              ,"Invalid beta, beta must lie in (0,1)")
  if( length(delta)!=1) errorcode<-1 # 
  else if( length(sigma)!=1 | sigma<0 ) errorcode<-2 # 
  else if( length(rho)!=1 | (rho>1 | rho < -1)) errorcode<-3 # 
  else if( length(alpha)!=1 | ( alpha>1 | alpha <0)) errorcode<-4 # 
  else if( length(beta)!=1 | ( alpha>1 | alpha <0)) errorcode<-5
  else errorcode<-0 # all OK
  if(errorcode>0) return(cat(errorstr[errorcode],"\n"))
  
  ######### negative delta
  delta1 = abs(delta1)
  ######### Normal approximation
  n0 = 2*(sigma/delta1)^2*(1-rho)*(qnorm(1-alpha/2)+qnorm(1-beta))^2 
  cat(paste("The sample size required using the normal approximation is", ceiling(n0),".\n"))
  
  ######### Set up a function to run the fixed point algorithm "n = f(n)"
  check.n = function(delta1, sigma, rho = 0.5, n , alpha = 0.05, beta = 0.1){
    ind = 2*(sigma/delta1)^2*(1-rho)*(qt(1-alpha/2,df = n-1)+qt(1-beta, df = n-1))^2
    return(ind)  }
    
  ######### 1000 iterations OR using normal approximation 
  # Maximum 1000 fixed point iterations
  # if we do not have convergence in 1000 iterations, suggest normal a approximation
  n = check.n(delta1, sigma, rho = 0.5, n0 , alpha = 0.05, beta = 0.1)
  for (i in 1:1000){
    n.new = check.n(delta1, sigma, rho = 0.5,n , alpha = 0.05, beta = 0.1)
    if (abs(n-n.new)<0.005){
      cat(paste("Number of fixed point iterations is ", i,".\n"))
      cat(paste("The sample size required is", ceiling(n),".\n"))
        break }else{n = n.new}}
  if( i  == 1000){cat(paste("We attempted 1000 iteration and there is no 
                            convergence.\n Use the normal approximation."))}
  }# end of the function: required.size(.)


