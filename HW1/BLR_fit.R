
##
#
# Logistic regression
# 
# Y_{i} | \beta \sim \textrm{Bin}\left(n_{i},e^{x_{i}^{T}\beta}/(1+e^{x_{i}^{T}\beta})\right)
# \beta \sim N\left(\beta_{0},\Sigma_{0}\right)
#
##

library(MASS)
library(coda)

########################################################################################
########################################################################################
## Handle batch job arguments:

# 1-indexed version is used now.
args <- commandArgs(TRUE)

cat(paste0("Command-line arguments:\n"))
print(args)

####
# sim_start ==> Lowest simulation number to be analyzed by this particular batch job
###

#######################
sim_start <- 1000
length.datasets <- 200
#######################

if (length(args)==0){
  sinkit <- FALSE
  sim_num <- sim_start + 1
  set.seed(1330931)
} else {
  # Sink output to file?
  sinkit <- TRUE
  # Decide on the job number, usually start at 1000:
  sim_num <- sim_start + as.numeric(args[1])
  # Set a different random seed for every job number!!!
  set.seed(762*sim_num + 1330931)
}

# Simulation datasets numbered 1001-1200

########################################################################################
########################################################################################




log.eva = function(X , Y , N , coef ){
  # return the log value of posterior given beta = coef
  # coef1: newly updated value, coef2: current value
  p = exp( X%*%coef )
  p = p/(1 + p)   
  
  value = sum ( Y*log( p ) + ( N - Y )*log( 1 - p ) ) - 0.5 * sum(coef^2)
  return(value)
}


bayes.logreg <- function(n,y,X,beta.0,Sigma.0.inv,niter=10000,burnin=1000,
                           print.every=1000,retune=100,verbose=TRUE)
{
  num.it = niter + burnin 
  beta = matrix(0, 2, num.it)
  v = 1
  accept.count = 0
  for (i in 2:num.it){
    # The candidator
    
    alpha = matrix(rnorm(2)*v,2,1) + beta[ ,i-1]
    temp.value1 = log.eva(X = X, Y = y, N = n, coef = as.matrix(alpha) ) 
    
    temp.value2 = log.eva(X = X, Y = y, N = n, coef = as.matrix(beta[,i-1]) )
    # Compute the ratio
    rho = min( exp( temp.value1 - temp.value2 )  ,1)
    
    flag = runif(1)
    beta[,i] = beta[,i-1] + (alpha - beta[,i-1]) * ( flag < rho)
    
    if ( flag<rho & i<=burnin){
      accept.count = accept.count + 1
    }
    if (i %% retune == 0 & i<=burnin){
      accept.rate = accept.count/retune
      #print(c(accept.rate,v))
      if (accept.rate<=0.3){
        v = v/2
      }
      if (accept.rate>=0.6)
        v = 2*v
      }
      accept.count = 0
      
    }
  }
  return(beta)
}

#################################################
# Set up the specifications:
p=2
beta.0 <- matrix(c(0,0))
Sigma.0.inv <- diag(rep(1.0,p))
niter <- 10000
# etc... (more needed here)
#################################################

# Read data corresponding to appropriate sim_num:

filename = paste("data/blr_data_", as.character(sim_num), ".csv",sep='') 
#filename = paste('blr_data_', sim_num, '.csv',sep='') 
data.set = read.csv(filename)

# Extract X and y:
x1 = data.set$X1
x2 = data.set$X2
X = cbind(x1,x2)
y = data.set$y
n = data.set$n
# Fit the Bayesian model:
result = bayes.logreg(n = n,y = y, X = X, beta.0 = beta.0, Sigma.0.inv = Sigma.0.Inv)
# Extract posterior quantiles...
test1 = result[1,1001:11000]
test2 = result[2,1001:11000]

percent.beta1 = sapply(seq(from = 0.01, to = 0.99, by =0.01), function(x) quantile(test1, prob = x))
percent.beta2 = sapply(seq(from = 0.01, to = 0.99, by =0.01), function(x) quantile(test2, prob = x))

quantile.beta = cbind(percent.beta1, percent.beta2)

# Write results to a (99 x p) csv file...

result.filename = paste("results/blr_res_", as.character(sim_num), ".csv",sep='')
#result.filename = paste('blr_result_', sim_num, '.csv',sep='')

write.table(quantile.beta, result.filename, row.names=FALSE, col.names=FALSE, na="", sep=",")

# Go celebrate.
 
cat("done. :)\n")






