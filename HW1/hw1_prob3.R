

bc.data = read.table('breast_cancer.txt')
names(bc.data) = c('area', 'compactness', 'concavepts', 'concavity', 'fracdim', 'perimeter', 
                   'radius', 'smoothness', 'symmetry', 'texture', 'diagnosis')
bc.data = bc.data[-1,] 
bc.res = matrix(1, nrow(bc.data),1)
bc.res[bc.data$diagnosis == 'B'] = 0
bc.data$response = bc.res 
design.m = as.matrix(bc.data[1:nrow(bc.data),1:10])
design.m = as.numeric(unlist(design.m))
design.m = matrix(design.m, nrow(bc.data),10)

design.m = cbind(matrix(1, 569, 1), design.m)

y = bc.res

# 1: Bad, 0: Good mvtnorm

niter = 102000
burnin = 2000
retune = 100


design.mean = sapply(c(1:11), function(x) mean(design.m[,x]))
design.sd = sapply(c(1:11), function(x) sd(design.m[,x]))

norm.design = sapply(c(2:11), function(x) (design.m[,x]-design.mean[x])/design.sd[x] )
norm.X = cbind(matrix(1,569,1), norm.design)

X = norm.X

fun.eva = function( coef ){
  # return the log value of posterior given beta = coef
  # coef1: newly updated value, coef2: current value
  prob = exp( X%*%coef )
  prob = prob/(1 + prob)   
  index = which( prob>10e-6 & abs(prob-1)> 10e-6 )
  value = exp(sum ( y[index]*log( prob[index] ) + ( 1 - y[index] )*log( 1 - prob[index] ) ) - 5e-4 * sum(coef^2) )
  # value = log( prod(prob^y * (1 - prob)^(1-y) ))  - 5e-4*sum(coef^2)
    
  return(value)
}



num.it = niter + burnin 
beta = matrix(0, 11, num.it)
v = 1

# The counting number of acceptance in each retune period
accept.count = 0

for (i in 2:num.it){
  
  alpha = beta[,i-1] + v*matrix(rnorm(11),11,1)
  
  rho = min(  fun.eva(alpha)/fun.eva(beta[,i-1]) , 1)

  flag = runif(1)
  # If accepted, the next current value is the now updated value.
  # Otherwise the same.
  
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
    if(accept.rate>=0.6){ 
      v = 2*v
    }
    accept.count = 0
    
  }
  
}


library(lattice)
library(coda)
sapply(c(1:11), function(x) effectiveSize(beta[x, burnin:num.it]))


#effectiveSize(test1)

#densityplot(test1)



