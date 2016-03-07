##----------------- Metropolis Hastings Functions. ------------------------------------------
##
##  1. Likelihood/Target distribution: Log unnormalized posterior distribution (called from )
##  2. Proposal function
##  3. MH sampling algorithm




# -------------------- 1. LogLikelihood Function --------------------------------------------------
  
  # log unnormalized posterior distribution: target/equilibrium distribution 
  ## wMat is weights matrix of size = Predictors x Predictors

  # initial for testing
#     wMat <- matrix(rep(0,Predictors*Predictors), nrow = Predictors)
#      penalty <- 100 # just for python test



  
LogLikelihood <- function(wMat, x, y, penalty, sigma) {
  
    n <- nrow(x)# no. of observations   
    yHat <- x %*% wMat #swap around from what Brad did, gives nxp matrix right away    
    
    # first row times first column, first row times second column etc. 

   # sigma is square covariance matrix from error term   
    #logl <-  tr((y - yHat)%*% solve(sigma)%*%t(y - yHat))  # never just use sigma without 1/2!
   
   # tr((y - yHat)%*%t(y - yHat)) # changed the transpose and took the trace :)
   ##EQUAL TO:  sum((yHat - y)^2) #in python: fit=np.sum(pow(np.transpose(yHat)-y,2))   
   
  logl <-  1/2 * tr((y - yHat)%*% solve(sigma)%*%t(y - yHat))
   #logl <- (n/2 * log(det(sigma))) + (n/2 * log(2*pi)) + 1/2 * tr((y - yHat)%*% solve(sigma)%*%t(y - yHat))
   
  
    ##------- Log prior definition: penalty * cross_connnections
    crossPenalty <- sum(wMat^2) - sum(diag(wMat)^2) # sum of squared cross-connections
    logprior=penalty*(crossPenalty)                # equal to sum(beta[lower.tri(beta)]^2) + sum(beta[upper.tri(beta)]^2)
    

    ## log unnormalized posterior density:  -1*( logl(fit) + logprior) 
    lupost <- -1*(logl + logprior)
    
  return (lupost)# return log prob, unnormalized (thus MCMC which uses ratios to evaluate samples)

} 
  

# -------------------- 2. Proposal Function --------------------------------------------------

#adds gaussian noise times factor. this generates next candidate in the chain.
Proposal <- function(w,factor,Predictors){
  
  # draw as many Normal numbers (m=1,sd=1) as there are matrix elements in w
    w_new <- w + (factor*matrix(rnorm(length(w),mean=0,sd=1), nrow = Predictors))
    return (w_new)
}




# -------------------- 3. Metropolis-Hastings sampling Function --------------------------------------------------

    # weight matrix "initial" is rows for y1, y2, etc. and columns for x1, x2,, etc.
#     initial <- matrix(rep(0,Predictors*Predictors), nrow = Predictors)
#     n <- 11000
#     penalty_param <- 100
#     factor <- .015
#     x <- x
#     y <- y
#     burnin <- 1000


  metropolis_hastings <- function(x, y, initial, n, penalty_param, factor, Predictors,burnin,sigma)
  {

#   seed_simulation <- 42
#   set.seed(seed_simulation) # to get reproducible results
    results <- vector('list',n) 
    x0 <- initial  # weight vector # is either zero matrix or random Gaussian
    count <- 0
    
    for (i in 1:n){       
      
      u <- runif(1) # generate one random variable between min=0 and max=1 (default)      
      x1 <- Proposal(x0,factor,Predictors) #factor=.15
      x1Logprob <- LogLikelihood(x1,x,y,penalty_param,sigma)
      x0Logprob <- LogLikelihood(x0,x,y,penalty_param,sigma)	#*** can make this go almost twice as fast caching past value here...
      prob <- exp(x1Logprob-x0Logprob) # exp(x1Logprob)/exp(x0Logprob) = target_probability(new)/target_probability(old)
                                        # this is the acceptance probability function 
      
      
      # if new is more likely than old, the exponent is positive, and prob is > 1, so A = 1, and it is more likely for u < A, so new state gets accepted
      A <- min(1,prob)	# can ignore the state transition function as Proposal() is symetrical      
        if (u<A){
          x0 <- x1 #update the chain  
          count <- count + 1 # counts how often new sample was accepted
          
        }    ###### NEW PART THAT WAS MISSING:
          
          results[[i]]<-x0   # otherwise store the old sample, if updated this is the new sample
                   
    }     
     # acceptance rate before cutting of the burn-in
    accept <- count/n     #79883! :)  how many out of n= 100000 samples were accepted 
    
    
    # get rid of the burnin samples
    results <- results[-(1:burnin)]  ### this is were thinning should take place, e.g., only very 10th element
    
    # change list to an array with same dimensions
    tmp <-unlist(results)
    height <-length(results)
    tmp2 <-array(tmp, dim=c(Predictors,Predictors,height)) # make array but reshape in same dimensions as tmp; checked that  
    # thinning
    tmp2 <- tmp2[ , ,seq(1, height, 5)]# only take every 5th element of the whole array
    
    return (list(results= tmp2, accept=accept, factor = factor))
    
    }



metropolis_hastings2 <- function(x, y, initial, n, penalty_param, factor, Predictors,burnin,sigma)
{
  
  #   seed_simulation <- 42
  #   set.seed(seed_simulation) # to get reproducible results
  results <- vector('list',n) 
  x0 <- initial  # weight vector # is either zero matrix or random Gaussian
  count <- 0
  
  for (i in 1:n){       
    
    u <- runif(1) # generate one random variable between min=0 and max=1 (default)      
    x1 <- Proposal(x0,factor,Predictors) #factor=.15
    x1Logprob <- LogLikelihood(x1,x,y,penalty_param,sigma)
    x0Logprob <- LogLikelihood(x0,x,y,penalty_param,sigma)  #*** can make this go almost twice as fast caching past value here...
    prob <- exp(x1Logprob-x0Logprob) # exp(x1Logprob)/exp(x0Logprob) = target_probability(new)/target_probability(old)
    # this is the acceptance probability function 
    
    # if new is more likely than old, the exponent is positive, and prob is > 1, so A = 1, and it is more likely for u < A, so new state gets accepted
    A <- min(1,prob)	# can ignore the state transition function as Proposal() is symetrical      
    if (u<A){
      x0 <- x1 #update the chain  
      count <- count + 1 # counts how often new sample was accepted
      
    }    ###### NEW PART THAT WAS MISSING:
    
    results[[i]]<-x0   # otherwise store the old sample, if updated this is the new sample
    
  }     
  # acceptance rate before cutting of the burn-in
  accept <- count/n     #79883! :)  how many out of n= 100000 samples were accepted 
  
  # get rid of the burnin samples
  #results <- results[-(1:burnin)]  
  
  return (list(accept=accept, factor = factor))
  
}