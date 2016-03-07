    #Minimal parallel R example
    
    rm(list=ls())
    
    #Parallel libraries
    library(foreach)
    library(doMC)
    
    
    #Set to the number of cores (Brad has 12)
    registerDoMC(12)
    
    ##### source script  ##########################################################################################
    
    # 12 overall 
    ALL_DATA <-c("skincolour", "vertebral", "wholesale", "iris_vv", "TicTacToe", 
                 "banana", "bupa", "appendicitis", "phoneme", "titanic","banknotes", "banknotes_interaction")
    
    s <- 11 # choose a data set indice
    
    #Load any libraries you need your parallel jobs to use inside the loop here!
    require(clusterGeneration)
    #require(Matrix)
    library(MASS)
    require(ridge)
    library(scatterplot3d)
    #library(msm)
    #library(tmvtnorm)
    library(lars)
    #library(glmnet)
    library(mcmc)
    require(rJava)
    library(xlsx)
    library(psych) 
    library(matrixcalc)
    library(abind)
    require(lattice)
    library(coda)
    source("Functions_MH_cv.R")
    source("metropolis_hastings_covariance.R")
    
    
    ##-------------------------- Imread datasets  ---------------------------------------------------------
    dataset <- read.table(paste(ALL_DATA[s],".txt", sep = ""), header = TRUE) # no space allowed
    
    
    y.pos <- ncol(dataset)
    colnames(dataset)[ncol(dataset)] <- "dependent"
    #dataset$dependent[dataset$dependent==2] <- -1  # non-skin samples 
    
    
    lambda_range <- read.xlsx(file="lambda_range.xlsx",sheetIndex = 1, header = TRUE)
    penalty <- lambda_range[ ,1] # vector
    penalty <- penalty[c(TRUE, FALSE)] # takes only every other element of penalty vector
    penalty <- penalty[c(TRUE, FALSE)] # takes only every other element of penalty vector
    penalty <- penalty[c(TRUE, FALSE)] # takes only every other element of penalty vector
    
    # attach to be able to access headers
    labels <- names(dataset)[-ncol(dataset)]
    # columns of cues
    col_cues <- c(1:(y.pos-1))
    ## assess the number of predictor X's, from column 5 to the end
    Predictors <- length(col_cues)
    
    ## Choose the number of partitions for cross-validation
    k <-100
    
    
    ## even out the 2 class frequencies: 
    ### have prior of 0.5 for either class, its like taking into account an intercept, should make performance  of linear model better, and training sizes perform normal
    classA <- sum(dataset$dependent == 1) # 100
    classA_data <- dataset[dataset$dependent == 1,]    
    classB <- sum(dataset$dependent == -1) # 210 
    classB_data <- dataset[dataset$dependent == -1,] 
    
    if (classA > classB) {
      n <- classA -classB
      perm <- sample(1:classA, classA) # as long as classA (1) data part
      classA_data <- classA_data[-perm[1:n], ] 
      
    } else if (classB > classA){
      n <- classB - classA  # difference
      perm <- sample(1:classB, classB) # as long as classB (-1) data part
      classB_data <- classB_data[-perm[1:n], ] # sample exactly n rows from the overall dataset and delete
      dataset <- rbind(classA_data, classB_data)
    }
    
    # number of objects
    N <- nrow(dataset)
    
    # -----------------Covariance matrix among cues (raw data)-----------------------------
    cov <- as.matrix(dataset[,col_cues])
    cor_mat <- cor(cov,method = "pearson")
    min_cor <- min(abs(cor_mat[lower.tri(cor_mat)])) # of ABSOLUTE correlations
    max_cor <- max(abs(cor_mat[lower.tri(cor_mat)]))
    abs_mean_cor <- mean(abs(cor_mat[lower.tri(cor_mat)])) 
    # -------------------------------------------------------------------------------------
    
    ## --------------- Multivariate Error Term  ----------------------------
    xItems <- as.matrix(dataset[ ,1:Predictors]) 
    yLabelMat <- replicate(Predictors, dataset$dependent) 
    ## -------------------- Regression coefficients at penalty = 0, including covariance ------------------------------------------
    lm_multivariate <- lm.fit(xItems,yLabelMat) # xItems is already WITHOUT INTERCEPT (no column of1s)
    betas <- lm_multivariate$coefficients[ ,1] # coefficients are naturally in the columns
    betas_LR <- t(replicate(length(penalty), betas[ ,1]))
    
    
    ##------------------- Individual coefficients (no covariance) at penalty = inf. ---------------------------  
    ind_coef <- vector('numeric', Predictors)
    for (c in 1:Predictors){
      # run through each predictor variable/no intercept
      fmla <- paste("dependent ~ ", paste(labels[c], collapse= "+"), paste(" -1"))
      single_weights <- lm(fmla, data = dataset)
      ind_coef[c] <- single_weights$coefficients #assign ind. weights to vector       
    }  
    ind_coeffs <- t(replicate(length(penalty), ind_coef))
    colnames(ind_coeffs) <- labels
    
    
    # ERROR TERM; 4 equivalent error vectors (residuals) since the same LR is repeated 4 times
    sigma <- diag(Predictors) # identity matrix as suggested by Takao (non-zero cov can alter shrinkage parameter)
    
    # define training size samples to loop through
    half <- round(N/2)
    if (N < 500){
      training_size <- c(10, 20, 50, half) 
    } else if (N<100){
      training_size <- c(10, 20, half) 
    } else {
      training_size <- c(10, 20, 100, 500) 
    }
    

for (v in 1:length(training_size)){
  
  
  percent_training  <-  training_size[v]/N
  # ---------  Generate the cross-validation partitions: -----------------------------------------------------------
  percent <-(1 - percent_training)  #### Hold the testset (distinct from random training set) 
  training_sample_size <- percent_training*N
  cv <- cv.indexing(k, nrow(dataset), percent)
  
  ###--- ------------------  Test Linear regression to get convegence at 0? ------------------------------------##################################### 
  predictions.LR <- Linear.regression(dataset, cv, ncol(dataset), Predictors)
  
  ###-------------------------------------------- 1) MODEL FITTING ---------------------------------------------------------------------
  acc_dec_TTB <- vector('numeric',length(penalty)) 
  acc_dec_tallying <- vector('numeric',length(penalty)) 
  se_acc_TTB <- vector('numeric',length(penalty)) 
  se_acc_tallying <- vector('numeric',length(penalty)) 
  acc_dec_class <- vector('numeric',length(penalty)) 
  se_acc_class <- vector('numeric',length(penalty)) 
  avg.weight.mat <- vector('list', length(penalty))
  acc.LR  <- vector('numeric',length(penalty)) 
  se_acc_LR  <- vector('numeric',length(penalty)) 
  
  diagonal_weights <- matrix(nrow = length(penalty), ncol=Predictors) # average diagonal
  avg_offdiagonal <- vector('numeric',length(penalty))
  
  for (p in 1: length(penalty)){
    # penalty: from largest to smallest
    
    ## Location in the loop to the prompt
    cat(paste("\n\nPenalty no.  = ", p, "\n\n"))     
    
    dec_TTB_accs <- vector('numeric',k) 
    dec_tallying_accs <- vector('numeric',k)
    dec_class_acc  <- vector('numeric', k)
    results.linear.regression <- vector('numeric', k)
    meanW <- vector('list',k) 
    for (i in 1:k){
      
      ## Location in the loop to the prompt
      cat(paste("\n\nTest set no.  = ", i, "\n\n"))     
      
      trainset <- dataset[cv[i,]==1, ]   
      testset <- dataset[cv[i,]==0, ]   
      test <- as.matrix(testset[ ,-ncol(dataset)]) #  without dependent
      test.labels <- testset[ ,ncol(testset)]
      # XItems and YLabelMat
      x <- as.matrix(trainset[ ,1:Predictors])    
      y <- replicate(Predictors, trainset$dependent) 
      
      
#       # getting LR coefficients (penalty =0) for this trainset to test
#       lm_multivariate <- lm.fit(x,y) # xItems is already WITHOUT INTERCEPT (no column of1s)
#       betas <- lm_multivariate$coefficients # coefficients are naturally in the columns
#       #the values in each column is what COR has to  converge to when penalty = 0
#       #betas_LR <- t(replicate(length(penalty), betas[ ,1]))
#   
#       # getting individual coefficients for this training set to test convergence
#       ind_coef <- vector('numeric', Predictors)
#       for (c in 1:Predictors){
#         # run through each predictor variable/no intercept
#         #fmla <- paste("dependent ~ ", paste(labels[c], collapse= "+"), paste(" -1"))
#         single_weights <-  lm.fit(as.matrix(x[ ,c]), y[,1])
#         ind_coef[c] <- single_weights$coefficients #assign ind. weights to vector       
#       }  
#             

      #####################------- MCMC: optimizing the scaling parameter first so that acceptance rate is ~0.20  -------------------------------------------            
      # using previous mean "out" as new starting point with n=1000 seems to work for penalty = 0, penalty = 7 and penalty = 700
      # works less well for n=500 samples, but still ok for n=700, i.e. acceptance rate for actual runs is ok.
      # tested both for training sample size N=500 and N = 10, and below works for both
      
      initial <-  matrix(rep(0,Predictors*Predictors), nrow = Predictors)
      factor <- 0.01
      samples <- metropolis_hastings(x,y, initial = initial,
                                     n=700,penalty_param = penalty[p],factor = factor,Predictors,
                                     burnin=0,sigma) 
      
      out <- apply(samples$result, c(1,2), mean)  
      samples$accept 
      
      timeout <- Sys.time() + 120 # + 2 minutes  
      ## while the condition holds that we dont want
      while (samples$accept < 0.18 || samples$accept > 0.22){
        
        if (samples$accept < 0.18 ){     ## if need to be higher, make scale smaller  
          # multiply the previous scale with some fucking factor
          factor <- samples$factor * 0.8
          # run again with new factor
          samples <- metropolis_hastings(x,y, initial= out,  
                                         n=700,penalty_param = penalty[p],factor = factor,Predictors,
                                         burnin=0,sigma)  # whole first 1e3 is burnin        
          out <- apply(samples$result, c(1,2), mean)  # this is the overall posterior mean across all chains!
          
          #cat(paste("\n\nAcceptance rate:\n"))
          print(samples$accept)
        }
        
        if(samples$accept > 0.22){    ## if need to be smaller, make scale larger
          factor <- samples$factor * 1.2
          samples <- metropolis_hastings(x,y, initial= out,
                                         n=700,penalty_param = penalty[p],factor = factor,Predictors,
                                         burnin=0,sigma)  # whole first 1e3 is burnin  
          out <- apply(samples$result, c(1,2), mean)  # this is the overall posterior mean across all chains!
          
          cat(paste("\n\nAcceptance rate:\n"))
          print(samples$accept)    
        }
        
        time <- Sys.time() 
        if (time > timeout) {
          break
          stop('the optimization takes longer than 2 minutes')  
        }       
      }
      # set factor for mcmc below
      cat(paste("\n\nFinal Acceptance rate:\n"))
      print(samples$accept) 
      
      Final_Factor <- samples$factor
      cat(paste("\n\nFinal Factor:\n"))
      print(Final_Factor) 
      #---------------------------------------------------------------------------------------------------------------------------------
      
      
      start <- vector('list', 4)      
      # range of rnorm is -inf to inf, so these are big steps, could start with sth smaller as well
      start[[1]] <- matrix(rep(0,Predictors*Predictors), nrow = Predictors) # all zeros
      start[[2]] <- matrix(rnorm(Predictors*Predictors,mean=0,sd=0.01), nrow = Predictors) 
      start[[3]] <- matrix(rnorm(Predictors*Predictors,mean=0,sd=0.01), nrow = Predictors)
      start[[4]] <- matrix(rnorm(Predictors*Predictors,mean=0,sd=0.01), nrow = Predictors)
      # ------- or sample from prior distribution -------------------------------------
      
  
      #Create a list to store the results of your simulations in
      all_results<- vector("list", 4)

    #Run a parallel loop, each iteration should be a different job you need doing (e.g. fitting your model with different starting parameters)
    all_results<-foreach(d=1:4) %dopar%   
    {   
      
      ## better to run 12 chains (12 cores) in parallel of length 2000 - 1000 (burnin) = 1000
      ## Big Insight: even with training size N= 500 (very reliable), convergence is easily reached with chain length of n=2000 for penalty = 0 but
      ## not for larger penalties, e.g.,not for penalty = 700,  
      # but when I try n=20000 chain length for penalty = 700, off-diagonals still dont converge to 0. why?
      
      
            samples <- metropolis_hastings(x,y, initial= start[[d]],
                                           n = 2000,penalty_param = penalty[p],factor = Final_Factor,Predictors,
                                           burnin=1000,sigma) 
                                          # now running 4 chains a 2000 samples, after burnin = 4x1000 = 4000 samples to average across
                                          # looking better with n=20000 for large penalty parameters and N=10.
                                          # cues are in columns of samples$results matrices
                                          # samples$results is now an array object with dimensions 4x4x1000
                                          
          samples$results # return object: contains the (thinned or non-thinned) chain as array
                    
    } # 
    
    # check convergence across chains: Gelman - Rubin diagnostic
   
    
    
    
    
    
        # cancatenate the arrays into one big array with 12 *1000= 12000 matrix slices, so dim = 4x4x12000
        all_chains <- abind(all_results[[1]], all_results[[2]], all_results[[3]], all_results[[4]], rev.along=1)
  
        
        ###------- this is the stats that need to happen outside the dopar loop; accessing the chains (list elements) ---------------
        meanW[[i]] <- apply(all_chains, c(1,2), mean)  # this is the overall posterior mean across all chains!
        
    
#         #x11(width=20, height=10) # unable to display when on server
#         par(mfrow=c(Predictors,Predictors))
#         
#         for (r in 1:Predictors){  #rows of matrix    
#           for (j in 1:Predictors){ # cols of matrix contain LR coefficients (at 0: equal to LR)
#             
#             #title has reversed order (consistent with SI) to R indices
#             plot(ts(all_results[[1]][r,j, ]), main = paste("w = ", j,r) ) # access each element of the Predictors xPredictors matrix, and plot time series
#             
#           }
#         }


        ##---------------------- PREDICTION: ---------------------------------------------      
        W_mat  <- meanW[[i]] # cols contain the coefficients 
        
        #  like in mcmc Loglikelihood function: yHat <- x %*% wMat 
        outcomes <- test %*% W_mat     #  outcomes matrix = 1210x4 %*% 4x4 = 1210x4.
        
        ## Linear classification rule: take the sign of the fitted values 
        ## (after matrix mulitplication: sign(test %*% W_mat)) with test containing continuous values, but y being 
        ## binary class label (-1/+1) therfore results can be thresholded at 0
        
        ## COR Linear Model: Binary classification  decision rule
        dec_class <- sign(rowSums(outcomes))   # every row contains a test item with all 4 cues, outcomes contain weighted cues
        
        dec_class_acc[i] <- ttb.graph(dec_class, test.labels)
        
        # get LR accuracy
        results.linear.regression[i] <- regression.graph(predictions.LR[[i]], test.labels)
        
      } #k loop
      
      acc_dec_class[p] <- sum(dec_class_acc)/k
      se_acc_class[p] <- sd(dec_class_acc)/sqrt(k)
      acc.LR[p] <- sum(results.linear.regression)/k
      se_acc_LR[p] <- sd(results.linear.regression)/sqrt(k)
      
      
      #across k=10 train sets: average matrix for that penalty p: elementwise mean.
      avg.weight.mat[[p]] <- apply(simplify2array(meanW), c(1,2), mean)
      weight_mat <- avg.weight.mat[[p]]
      diagonal_weights[p, ] <- diag(weight_mat) # average diagonal and make a matrix out of it with dims: penalty x Predictors
      avg_offdiagonal[p] <- mean(c(weight_mat[upper.tri(weight_mat)],weight_mat[lower.tri(weight_mat)])) #
    } # p loop
    
    
    Lambda_Max_Class <- penalty[which.max(acc_dec_class)]
    
    
    ## --------------------- Write all results to a data frame and file ----------------------------------------------------
    # storing failed because of length of betas
    
    cv.run <- data.frame(data = paste(ALL_DATA[s]), training_N = round(training_sample_size,0),  
                         penalty = penalty, Class_dec = acc_dec_class, se_acc_class = se_acc_class,
                         Lambda_Max_Class = Lambda_Max_Class, 
                         acc_LR = acc.LR,  se_acc_LR = se_acc_LR, 
                         # all diagonals next to each other, one weight per column
                         diagonal_weights_p =  diagonal_weights, avg_offdiagonal_p = avg_offdiagonal, 
                         final_factor = samples$factor, final_accept = samples$accept, 
                         betas_LR = betas_LR ,#betas of whole dataset without penalization (matrix)
                         ind_coef  = ind_coeffs, # independent predictors at 0 for whole dataset (matrix)
                         Predictors, N,  k,  v,  test_size = N - training_sample_size,  percent_training, 
                         cov_mean = abs_mean_cor,cov_min = min_cor,cov_max = max_cor)                              
    

    if (v > 1) cv.run <- rbind(results, cv.run) # append
    results  <- cv.run  
    
    
    # updates for every v loop advance
    save(results, file= paste(ALL_DATA[s],"_corrected.RData", sep = ""))
    write.xlsx(results, file= paste(ALL_DATA[s],"_corrected.xlsx", sep =""), row.names=TRUE)  
    
    
  } # v loop
  
  

  