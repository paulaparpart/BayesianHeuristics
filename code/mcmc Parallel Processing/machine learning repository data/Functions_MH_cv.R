


#####  1.)  Covariance Function     ########################################
##
##
##
##
covariance <- function(level, Predictors, noise)
{
  
  
  if (level == 1) { cov.level<- 0
    } else if (level== 2) {cov.level<-.1
    } else if (level == 3) { cov.level<- .2                        
    } else if (level == 4) { cov.level<- .3                              
    } else if (level == 5) { cov.level<-.4                           
    } else if (level == 6){ cov.level<-.5                            
    } else if (level == 7){ cov.level<-.6                                                             
    } else if (level == 8){ cov.level<-.7  
    } else if (level == 9){ cov.level<-.8 
    } else if (level == 10){ cov.level<-.9 
    } else if (level == 11){ cov.level<-1.0 
    }
                          
  covmat<-matrix(1, Predictors, Predictors)
  
  covmat[upper.tri(covmat)]<-cov.level
  
  covmat<-covmat+ matrix(rnorm(Predictors^2, 0 , noise), Predictors, Predictors)
  
  covmat[!upper.tri(covmat)]<-0
  
  covmat<-covmat+t(covmat)
  
  diag(covmat)<-1
  
  
  return(covmat)
  
}



###           2.) Sampling Function       #################################
##
##   Sample artificial data for given environment 
##  function 'sampling' samples binary x and binary y
##
#seed_int <- seed_sampling

sampling <- function(Predictors, N, CovMat, noise, betas, seed_int)
{
  
  #seed_int <- seed_sampling
  ##   Sampling is a function where:
  ##  	for each trial, 
  ##    	a. sample values of x based on the covariance matrix of step 1
  ##    	b. sample y based on the regression weights from step 2
  ##
  ##	Arguments:
  ##
  ##	betas			- sampled regression weights (Gaussian/Laplace)
  ##	Predictors 		- no. of predictors
  ##	N	    	 	- sample size 
  ##	CovMat		- covariance matrix
  ##	seed			- current seed (for current simulation)
  ##	Noise 		- noise level (small, medium, large)
  ##
  ##	Return values: the matrix of binary x's (NxPredictors) and classified Ys
  ##	vector (Nx1 vector) as a combined data frame
  
  ##	mvrnorm simulates N samples from a multivariate normal distribution 
  ##	with the covariances between Predictors according to Sigma matrix
  ##	mu is a vector of means=0 for the number of Predictors
  ##	CovMat uses covariance matrix with 1's in diagonal, and covariances between 0 and 1
  
  ## 	setting the seed for mvrnorm() gives the same binary data marix
  ##	'bdata' each time,but be careful not to use the same as for betas!
  
  
  mu <- rep(0,Predictors) # all means are zero
  
  
  ##    Create a differences matrix beween cue matrix 1 and 2 (option A and B)
  ##    pairs of items are collapsed down into one matrix
  
  
  # 1. sample cue matrix 1 (option A) with covaraiance sigma = CovMat
  set.seed(seed_int)
  mdata1 <- mvrnorm(n=N, mu = mu, Sigma = CovMat, empirical = TRUE) 
  bdata1 <-(mdata1 > 0)*1 ## generate binary data from multivariate data: First 10 raw cues
# bdata is like the raw data matrix (0,1) that is used to create all pairwise comparisons
  
  
  # 2. sample cue matrix 2 (option B) with covariance sigma = CovMat
  set.seed(70)
  mdata2 <- mvrnorm(n=N, mu = mu, Sigma = CovMat, empirical = TRUE) 
  bdata2 <-(mdata2 > 0)*1 ## generate binary data from multivariate data 
  
  # Compute the covariance of ALL raw cue data
  all_cues <- rbind(bdata1, bdata2)   # concatenate the 2 matrices below each other
  cor_mat <- cor(all_cues,method = "pearson")
  
  # 3. Creating binary comparisons between bdata1 and bdata2 cue matrix != all possible binary combinations  
  bdata_diff <- bdata1 - bdata2
  ## 	4. matrix multiplication
  Xw <- bdata_diff %*% betas
  ##	2. add Gaussian noise  
  Ymulti <- Xw + noise
  
  ## thresholding Ymulti at 0 for differences matrix bdata_diff 
  Ymulti[Ymulti > 0]  <- 1
  Ymulti[Ymulti < 0]  <- - 1
  
  
  # when tallying would guess
  for (j in 1:length(Ymulti)){  
    if (Ymulti[j]==0)
    {
      Ymulti[j] <- sign(rnorm(1))
    }
  }   ## all +1 and -1 now!
  
  
  y <- Ymulti
  #y <- (Ymulti > 0) * 1
  
  environment <- data.frame(bdata_diff, dependent=y)
  
  return(list(environment = environment, cor_mat = cor_mat))
  
}





## 	        3.) Function cv.indexing
##
##	cross-validation partitioning for current environment dataset,
##	takes seed as input to be able to reproduce results,
##	However, can use different seeds here to get different cross 
##	validation results for the same data set 

## Generates 'k' sets of indices, where each set represents a split
## of a data set of size 'N' into two disjoint data sets (sometimes called a
## 'folder').
##  
## Within each split, part of the data is called a 'test' data set, and the
## remaining data points form the 'training' data. None of the 'test' sets,
## overlap, and their union corresponds to the complete data set.
##
## This function returns a k-by-n binary matrix 'cv'. Each row in 'cv' 
## represents a split. If cv[i, j] == 1, then data point 'j' is in the 
## training set of partition 'i'. If cv[i, j] == 0, then data point 'j' is 
## in the test set of partition 'i'.
##
## The number of test points in the last partition will be larger than in 
## the other partitions if 'N' is not a multiple of 'k'.

cv.indexing <- function(k, N, percent)
{
  ## 	Arguments:
  ##	k 	number of partitions (set above)
  ##	N	size of the current data set (set above)
  ##	seed 	seed used for random partitions (set above) 
  
  #   if (k >= N)
  #     stop('Number of partitions should be less than number of data points!')   
  #   
  
  #set.seed(seed) # For reproducibility of results, only
  # dont use it now for the perm function!
  
  test.set<- round(percent * N)
  # the size of  a test set is 20% of N
  
  
  cv <- matrix(rep(1, k * N), nrow = k) # Allocate output matrix
  
  
  for (i in 1:k){
    
    # generate new permuation for each row i 
    perm <- sample(1:N, N)
    
    # define the test set items from columns in perm, to be marked by 0 
    # while the training set items stay 1
    cv[i,perm[1:test.set]] <- 0
    
    
  }
  
  return(cv)                              
}


################# Linear Regression Function ############################################
##
# y.pos <- ncol(dataset)

Linear.regression <- function(dataset, cv, y.pos, Predictors) 
{
  
  k <- nrow(cv) 
  pred.linear <- vector('list', k)
  
  for (i in 1:k){
     
    trainset <- dataset[cv[i,]==1, ] 
    testset <- dataset[cv[i,]==0, ]   
    test <- as.matrix(testset[ ,-y.pos]) #  without dependent   
#     # the actual test items used by the other models for prediction:
#     indifferent <- apply(test,1,function(x) identical(as.vector(x),rep(0,Predictors)) )
#     test <- test[!indifferent, ] 

    # cov <- as.matrix(trainset)
    # cor_mat <- cor(cov,method = "pearson") # redundancy check on trainset

##---------------------- Fitting
    Xdata <- as.matrix(trainset[ ,1:Predictors])
    Ydata <- as.matrix(trainset[ ,y.pos]) # just one y vector is enough, no need to duplicate
    fit <- lm.fit(Xdata,Ydata)   # does not use intercept!

# but intercept is necessary when the classes are not evenly distributed
# or make even classes of data
    coef_LR <-fit$coefficients # only 1 row this time, no need to transpose
    
    ##------------------- Prediction -------------------------------------
    LR_output <- sign(test  %*%  coef_LR)  #NxPredictors %*% Predictorsx1  = Nx1
    

#     if (is.na(LR_output[1]) == FALSE){  # only run if it is not NA  (FALSE == FALSE is TRUE)
#     for (j in 1:length(LR_output)){  
#       if (LR_output[j]==0) LR_output[j] <- sign(rnorm(1))  
#       }
#     }
        
    pred.linear[[i]] <- LR_output
       
  }
  
  return(pred.linear)   
  
}




#############       4.) Regression Prediction Graph (accuracies)########################

regression.graph <- function(predictions.regr, test.labels)
{
  
  #predictions.regr <- predictions.regression[[1]]
  
  # predictions.regression      is a vector from a list containing the 0/1 predictions
  # test.labels                 is a vector of the correct outcomes from the current testset
  
  n <- length(predictions.regr)
  m <- length(test.labels)
  all.equal(n,m)
  
  prop_regr <- sum(test.labels==predictions.regr)/m
  
  
  return(prop_regr)
  
}






########    5.) Regular Ridge Function (Gaussian, non truncated, non Bayesian) ###############################################
#        

regular.ridge <- function(dataset, cv, y.pos, penalty) 
{
  
  
  attach(dataset)
  labels <- names(dataset)
  if(!exists(labels[y.pos])){
    stop('the dataset does not contain the binary dependent variable!')
  }
  
  
  k <- nrow(cv) 
  
  pred.logridge <- vector('list', k)
  
  
  for (i in 1:k){
    
    testset <- dataset[cv[i,]==0, ]
    trainset <- dataset[cv[i,]==1, ]
    
    # Design matrix X (bdata_diff)
    X <- as.matrix(trainset[-y.pos])
    
    # dependent variable Y (vector)
    y <- as.factor(as.matrix(trainset[y.pos]))     
    
    
    #     variables <- names(trainset) 
    #     #formula without intercept
    #     fmla <- as.formula(paste(variables[y.pos]," ~ ", 
    #                             paste(variables[-y.pos], collapse= "+"), paste(" -1")))
    #     
    
    ## Location in the loop to the prompt
    cat(paste("\n\nTEST SET NUMBER  = ", i, "\n\n"))
    
    
    log.predict <- matrix(nrow=length(penalty),ncol=nrow(testset))     
    
    
    for (p in 1:length(penalty))
    {
      
      
      #### FITTING REGULAR RIDGE REGRESSION (to get weights) ######################################    
      
      ## logistic ridge regression with vector lambda
      #logRidge.fit <- logisticRidge(fmla, data=trainset, lambda = penalty[p])
      
      # alpha = 0 indicates ridge
      # family = binomial indicates logistic regression
      logRidge.fit <- glmnet(X, y, family="binomial", alpha = 0, lambda=penalty[p],
                             standardize = FALSE, intercept=FALSE)
      
      
      cat(paste("\n\nlambda  = ",penalty[p], "\n"))
      print(logRidge.fit$beta)
      
      
      
      #### PREDICTING With logistic model (predicted probabilities) ######################################
      
      #logRidge.predictions <- predict(logRidge.fit, newdata=testset, type="response")
      
      
      # get test data from current test set
      testmatrix <- as.matrix(testset[-y.pos])
      
      
      log.predict[p, ] <- predict(logRidge.fit, newx = testmatrix,type="response")
      ## "class" gives wrong classification because of decimal places! 
      
      # threshold the predicted probabilities into y=1, y=0, if p=0.5 (rounded to 7 decimals) that is class 0
      
      for (n in 1:nrow(testmatrix)){
        
        
        if (round(log.predict[p,n], digits= 7) == 0.5000000){
          
          log.predict[p,n] <- 0 
          
        } else if (log.predict[p,n] > 0.5){
          
          log.predict[p,n] <- 1 
          
        } else if (log.predict[p,n] < 0.5){
          
          log.predict[p,n] <- 0 
        }
        
      }
      
    }
    
    #print(log.predict)
    
    pred.logridge[[i]] <- log.predict
    
  }
  
  
  return(pred.logridge)
}








#####     6.) Pred.Graph Function (Ridge accuracies)    #########################
##
##
##
pred.graph <- function(predictions, test.labels){
  
  
  # predictions <- predictions.regridge[[i]]
  
  # predictions is matrix with penalty x testpredictions
  
  
  # 	get the number of test predictions 
  n <- ncol(predictions)
  
  
  # Thresholding the predictions first:
  # can vary the threshold point!
  #       thres <- 0.5
  #       
  #       predictions <- (predictions > thres)*1 
  #       
  
  
  
  raw.acc <- vector("numeric",nrow(predictions))
  
  for (l in 1:nrow(predictions)){
    
    
    raw.acc[l] <- sum(predictions[l, ] == test.labels)/n
    
    
  }
  
  # put the vector raw_acc into a list each time, and return it	
  
  propList <- list(raw.acc=raw.acc)
  return(propList)
  
}



############ 7.) LASSO PREDICTION FUNCTION ####################################
##
##
##

lasso.predictions <- function(dataset, cv,  y.pos, penalty) 
{
  ##
  ##
  
  
  k <- nrow(cv) 
  
  ##   initiate empty lists
  pred.lasso <- vector('list', k)
  
  
  for (i in 1:k){
    
    
    testset <- dataset[cv[i,]==0, ] # is data frame
    trainset <- dataset[cv[i,]==1, ]
    
    
    
    # Design matrix X (bdata_diff)
    X <- as.matrix(trainset[-y.pos])
    
    # dependent variable Y (vector)
    y <- as.factor(as.matrix(trainset[y.pos]))     
    
    
    #lars.model <- lars(X, y, type = "lasso", trace = TRUE, normalize = TRUE, intercept = FALSE)  
    
    
    
    lasso_predict <- matrix(nrow=length(penalty),ncol=nrow(testset))     
    
    for (p in 1:length(penalty))
    {
      
      ###### FITTING TRAINING DATA (to get weights)
      
      # alpha = 1 indicates lasso, family = binomial indicates logistic regression
      ### USE vector form for lambda! a sequence of decreasing lambda values!
      
      loglasso <- glmnet(X, y, family="binomial", alpha = 1, lambda=penalty[p],
                         standardize = FALSE, intercept=FALSE)
      
      
      cat(paste("\n\n regression weights for lasso penalty  = ",penalty[p], "\n"))
      print(loglasso$beta)
      
      
      
      ####### PREDICTING TEST SETS  ############################################
      
      # get test data from current test set
      testmatrix <- as.matrix(testset[-y.pos])
      
      # store predictions as a function of penalty parameter p
      lasso_predict[p, ] <- strtoi(predict(loglasso, newx = testmatrix,type="class"))
      
      
      #lasso_predict[p, ] <- predict(loglasso, newx = testmatrix,type="response") #gives p=0.5000000 quite early on
      
      
    }
    
    
    
    pred.lasso[[i]] <- lasso_predict   
    
  }
  
  
  return(pred.lasso)
  
  
}


#####     8.) Lasso.Graph Function (lasso accuracies)    #########################
##
##
##
lasso.graph <- function(predictions, test.labels){
  
  
  # predictions <- predictions.lasso[[i]]
  # predictions is matrix with penalty x testpredictions
  #
  ##  ncol(predictions) refers to no. of test predictions
  ##  nrow(predictions) refers to number of penalty paramters
  
  #   get the number of test predictions 
  n <- ncol(predictions)
  
  # lasso accuracies as a function of the penalty parameters 
  lasso.acc <- vector("numeric",nrow(predictions))
  
  
  for (l in 1:nrow(predictions)){
    
    
    lasso.acc[l] <- sum(predictions[l, ] == test.labels)/n
    
    
  }
  
  # put the vector raw_acc into a list each time, and return it	
  propList <- list(lasso.acc=lasso.acc)
  return(propList)
  
  
}




#####     9.) Accordance Function (Heuristic and Regularized)    #########################
##
##
##
accordance <- function(predictions.heuristic, predictions.regularized){
  
  ## Input can be:
  
  #predictions.heuristic <- predictions.ttb[[i]]
  #predictions.regularized <- predictions.lasso[[i]]
  
  # or can be: 
  
  #predictions.heuristic <- predictions.tallyLearn[[i]]
  #predictions.regularized <- predictions.regridge[[i]]
  
  
  
  # number of penalties
  n_penalty <- nrow(predictions.regularized)
  
  #number of predictions
  n <- ncol(predictions.regularized)
  
  acc_rate <- vector("numeric",n_penalty)
  
  for (l in 1:n_penalty){
    
    
    acc_rate[l] <- sum(predictions.regularized[l, ]== predictions.heuristic)/n
    
    
  }
  
  
  return(acc_rate)
  
}




########## 9.) Tallying Learning Function ##############################
##

#y.pos <- ncol(dataset)

tallying.learning <- function(dataset, cv, y.pos , Predictors)
{ 
#   
  #  number k of different test sets
  k <- nrow(cv) 
  
  ## 	initiate empty lists
  pred.tallying <- vector('list', k)
  null.pos <- vector('list', k)
  for (i in 1:k){
    
    ####  Fitting Training Data (get unit weights) ###################################
    trainset <- dataset[cv[i,]==1, ] 
    testset <- dataset[cv[i,]==0, ]   
    test <- as.matrix(testset[ ,-y.pos]) #  without dependent
    
#   this is a special case of the test below
#     indifferent <- apply(test,1,function(x) identical(as.vector(x),rep(0,Predictors)) )
#     test <- test[!indifferent, ] 

    cue_validities_raw <- vector('numeric', Predictors) 
    cue_validities <- vector('numeric', Predictors)
    for (c in 1:Predictors){
      # estimate the ecological cue validity of each cue as v = R/(R+W)
      if (sum(trainset[,c]==trainset[ ,ncol(trainset)]) == 0) { cue_validities[c] <- 0 
      } else  
        cue_validities_raw[c] <- sum(trainset[,c]==trainset[ ,ncol(trainset)])/(sum(trainset[,c]==1)+sum(trainset[,c]==-1))              
        cue_validities[c] <- cue_validities_raw[c] - 0.50    
    }
    
    # the validities - 0.50 are automatically same sign as regression betas
    unitweights <- sign(cue_validities) 

# delete ties AFTER matrix multiplication with unit weights
#     null_pos <- which((test %*% unitweights) == 0) # indexes of null positions
#     test <- test[-null_pos, ] # delete all the null positions
#     
    
  ############ Prediction Tallying (Testdata) ###########################################    
   
    # matrix multiplication of unit weights and testdata, if > 0, then +1 (A), if < 0, then B(-1)
    tl_predict <- sign(test %*% unitweights)

# THE 0 CASES HAVE TO BE DELETED FROM OTHER MODELS TOO!
#     ## if the sum of all cue differences is still 0, then guess, different each time!!
#     for (j in 1:length(tl_predict)){  
#       if ( tl_predict[j]==0)
#       {
#         tl_predict[j] <- sign(rnorm(1))
#       }
#     }   ## all +1 and -1 now!
       
    # feed vector for one testset into predictions list
    pred.tallying[[i]] <-  as.vector(tl_predict)
    #null.pos[[i]] <- null_pos
 
  }  
 
  AllList <- list(pred.tallying=pred.tallying)
  
  return(AllList)

}




#####         9.) Tallying.graph (accuracies) Function    ######################################
##
###
##
##

tallying.graph <- function(predictions.tallying, test.labels)
{
  
  ## function provides a summary of the performance (accuracy) of the tallying 
  ## heuristic
  
  # get the number of test predictions/length of predictions vector
  
  
  n <- length(predictions.tallying)
  m <- length(test.labels)
  all.equal(n,m)
  
  
  
  prop <- sum(test.labels==predictions.tallying)/m
  
  # prop contains accuracies
  
  return(prop) # a single number 
  
}




#### 10. TTB Learning Function ##########################################################################


# data <- paired_data
#  y.pos <- ncol(paired_data)

ttb.predictions <- function(dataset, cv, y.pos, Predictors)
{
  ##   This function is called to derive the predictions for Take-the-Best 
  ## Arguments:
  ##
  ## dataset   	dataset is data frame of current environment with n rows = data points
  ## cv 	  	  a binary matrix with k x n; k rows correspond to 
  ##			      different test and training set separations, 
  ##			      n columns correspond to the n data points in the dataset
  ## y.pos 		  integer indicating which of the columns in dataset 
  ##			      contains the binary dependant of the data points (0/1)
  
  #  number k of different test sets
  k <- nrow(cv) 
  
  ## 	initiate empty lists
  pred.ttb <- vector('list', k) # because we have several test sets here
  
  #-------------------------- get the cue validities and order (Trainset) ---------------------------------------
  for (i in 1:k){
    
    ####  Fitting Training Data (get unit weights) ###################################
    trainset <- dataset[cv[i,]==1, ] 
    testset <- dataset[cv[i,]==0, ]   
    test <- as.matrix(testset[ ,-y.pos]) #  without dependent
    
#     # the actual test items used by the other models for prediction:
#     indifferent <- apply(test,1,function(x) identical(as.vector(x),rep(0,Predictors)) )
#     test <- test[!indifferent, ] 
#     
    cue_validities_raw <- vector('numeric', Predictors) 
    cue_validities <- vector('numeric', Predictors)
    for (c in 1:Predictors){
      # estimate the ecological cue validity of each cue as v = R/(R+W)
      if (sum(trainset[,c]==trainset[ ,ncol(trainset)]) == 0) { cue_validities_raw[c] <- 0 
      } else  
        cue_validities_raw[c] <- sum(trainset[,c]==trainset[ ,ncol(trainset)])/(sum(trainset[,c]==1)+sum(trainset[,c]==-1))              
      cue_validities[c] <- cue_validities_raw[c] - 0.50    
    }
    # the validities - 0.50 are automatically same sign as regression betas   
    
    
  # now absolute order
    cue_order <- order(abs(cue_validities), decreasing = TRUE)
    
    ############ PREDICTING TESTDATA WITH TTB (PREDICTION) ###########################################      
  
    ttb_predict <- vector("numeric",nrow(test))   
    for(r in 1:nrow(test)){
      v <- 1
      # circulates through elements 1:5 of cue_order in order of abs. vailidity 
      while (test[r,cue_order[v]]== 0){            
        
        if (v == length(cue_order)){ 
          ttb_predict[r] <- 0 # TTB guesses 
          #stop('The test data set still includes ties!')   
          break  
        }       
        v <- v + 1  # go to the next element in the cue_order
        
      } # while loop only breaks when test ~= 0 , and then the loop is halted completely!!!
      
      if (test[r,cue_order[v]]== 1 & cue_validities[cue_order[v]] >= 0){           
        ttb_predict[r] <- 1  # prediction is sign
        
      } else if (test[r,cue_order[v]]== 1 & cue_validities[cue_order[v]] < 0){ 
        ttb_predict[r] <- -1  # 
        
      } else if (test[r,cue_order[v]]== -1 & cue_validities[cue_order[v]] >= 0){
        ttb_predict[r] <- -1 #
        
      } else if (test[r,cue_order[v]]== -1 &  cue_validities[cue_order[v]] < 0){
        ttb_predict[r] <- 1  #           
      }
    } # end of r loop

    pred.ttb[[i]] <- ttb_predict # vector of length testset
    
  } # end of k for loop
  
  return(pred.ttb) # return the whole list with 20 elements
  
} # end of function



######  11.) TTB graph function (proportion correct) ######################
##
##
ttb.graph <- function(predictions.ttb, test.labels)
{
  
  ## function provides a summary of the performance (accuracy) of the tallying 
  ## heuristic
  ## input 'predictions.tbb' is always only 1 element of the list (vector)
  
  #predictions.ttb <- predictions.ttb[[i]]
  
  n <- length(predictions.ttb)
  m <- length(test.labels)
  #all.equal(n,m)
  
  
  prop.ttb <- sum(test.labels==predictions.ttb)/m
  
  # prop contains accuracies
  
  return(prop.ttb) # a single number 
  
  
}


