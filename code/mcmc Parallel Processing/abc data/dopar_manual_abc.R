    #Minimal parallel R example
    
    rm(list=ls())
    
    #Parallel libraries
#     library(foreach)
#     library(doMC)
#     #Set to the number of cores (Brad has 12)
#     registerDoMC(12)
#     
    ##### source script  ##########################################################################################
    

    ALL_DATA <- c("ozone" , "mortality", "prf.world","bodyfat", "car.world","cloud","cit",
                "dropout","fat.world", "fuel", "galapagos",
                "homeless", "house.world","landrent", "mammal", "oxidants",
                 "attractiveness.men", "attractiveness.women", "fish.fertility") # not ozone or oxygen (too small)
    
    #for (s in 1:18){
    
    s <- 13 # 
    
    
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
    #require(rJava)  # not working on home pc!
    #library(xlsx)
    library(psych) 
    library(matrixcalc)
    library(abind)
    require(lattice)
    library(coda)
    library(mcmc)
    library(foreach)
    library(doParallel)
    
    
    #Set to the number of cores (Brad has 12)
    #registerDoMC(2)
    registerDoParallel(cores=4)   #8 cores
    
    
    source("Functions_MH_cv_abc.R")
    source("metropolis_hastings_covariance.R")
    
    ##-------------------------- Imread datasets  ---------------------------------------------------------
    dataset <- read.table(paste(ALL_DATA[s],".txt", sep = ""), header = TRUE) # no space allowed
    
    
    # quicker than excel
    penalty <- c(1000, 700.00000000, 330.07641174, 156.81303712,  74.49889703,  35.39301171,  16.81454797,   7.98827254,
                 3.79507663,   1.80296886, 0.85655628,   0.40693363,   0.19332644,   0.09184572,   0.00000000)
    
    
    ## CHECK whether DV is in column 3 for every dataset!
    y.pos <- 3 
  
    
    # make all datasets to contain cues in columns 5 - end
    col_cues <- c(5:ncol(dataset))
      
    # attach to be able to access headers
    labels <- names(dataset)
    labels <- labels[col_cues] # only the cues now
    
    ## assess the number of predictor X's, from column 5 to the end
    Predictors <- length(5:ncol(dataset))
    
    # number of objects
    N <- nrow(dataset)
    
    ## Choose the number of partitions for cross-validation
    k <- 1000 # test run with 5 to see if it works # larger but also less training set size  (5)
    
    # -----------------------------------------------------------------------------------------------------
    ## BEFORE creating paired_data: average redundancy between cues
    cov <- as.matrix(dataset[,col_cues])
    cor_mat <- cor(cov,method = "pearson")
    # the lower triangle of the cov matrix contains pairwise correlations
    min_cor <- min(cor_mat[lower.tri(cor_mat)])
    max_cor <- max(cor_mat[lower.tri(cor_mat)])
    abs_mean_cor <- mean(abs(cor_mat[lower.tri(cor_mat)]))
    # -------------------------------------------------------------------------------------------------
    
    ## ----------- Create Paired Data (ALL binary comparisons of objects) -----------------------------------------
    # take all possible 2-element combinations of N rows, and re-shuffle each combination by row to randomise order
    ## that way the dependent=y contains both 0's and 1's -> variation for correlation
    comb <- combn(N,m=2, FUN= function(x, ...) x[sample(length(x))] ) 
    y <- vector('numeric',ncol(comb)) # classification; A(+1) or B(-1)
    difference <- vector('numeric',ncol(comb))
    bdata_diff <- matrix(nrow=ncol(comb), ncol=length(col_cues))    
    for (i in 1:ncol(comb)){
      # takes out only the 2 rows from dataset that are compared at step i 
      binary <- dataset[comb[ ,i], ] # data.frame
      
      
      if (i == 1) {comparisons <- binary
      }else { comparisons <- rbind(comparisons,binary)
      }
      ## if          A(1)          >       B(-1) 
      ## always compare row 1 with row 2 (no matter which ones has the higher criterion value) upper row - lower row
      if (binary[1,y.pos] > binary[2,y.pos]){
        y[i] <- 1 #(A)
      } else y[i] <- -1 #(B)
      
      ## cue values (row 1) - cue values (row 2) 
      bdata_diff[i, ] <- as.matrix(binary[1,col_cues] - binary[2,col_cues]) #     
      #difference[i] <- (binary[1,y.pos] - binary[2,y.pos]) # this difference is always positive? 
    }    
    bdata_diff <- as.data.frame(bdata_diff)
    names(bdata_diff) <- labels# give it the cue labels
    ## combine cue data and dependent in a data frame
    paired_data <- data.frame(bdata_diff, dependent=y)
 
    
    # ------- Assess paired_data cue validities and order as v= R/R+W  -----------------
    cue_validities_raw <- vector('numeric', Predictors)  
    cue_validities <- vector('numeric', Predictors)  
    
    for (c in 1:Predictors){
      if (sum(paired_data[,c]==paired_data[ ,ncol(paired_data)]) == 0) { cue_validities[c] <- 0 # stays 0 now if it was 0 
      } else  
        cue_validities_raw[c] <- sum(paired_data[,c]==paired_data[ ,ncol(paired_data)])/(sum(paired_data[,c]==1)+sum(paired_data[,c]==-1)) 
        cue_validities[c] <- cue_validities_raw[c] - 0.5# back to same scale as regression weights as otherwise order can be different!
    } 
    cue_order <- order(abs(cue_validities), decreasing = TRUE) # values are between 0-1 anyway.
    
    cue_names <- names(bdata_diff)
    Cue <- cue_names[cue_order] ## cue names in order
    ecological_val <- round(cue_validities[cue_order],2) ## validities in order rounded to 2 decimals
    print(Cue)
    print(ecological_val) # gives same values as in previous sims  
    #--------------------------------------------------------------------------------------------------------
    dataset <- paired_data # is the new dataset now for below
    
      
    # number of objects after evening out
    N <- nrow(dataset)

    ## --------------- Multivariate Error Term  ----------------------------
    xItems <- as.matrix(dataset[ ,1:Predictors]) 
    yLabelMat <- replicate(Predictors, dataset$dependent) 
    ## -------------------- Regression coefficients at penalty = 0, including covariance ------------------------------------------
    lm_multivariate <- lm.fit(xItems,yLabelMat) # xItems is already WITHOUT INTERCEPT (no column of1s)
    betas <- lm_multivariate$coefficients[ ,1] # coefficients are naturally in the columns
    betas_LR <- t(replicate(length(penalty), betas))
  
    
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
    #training_size <- c(0.05, 0.1, 0.3, 0.5, 0.7,0.9) # for ozone and oxygen, 0.1 is too small to have it work   
    training_size <- c(5, 10, 12, 20, 115)/N

    #all_training <-  vector('list',length = length(training_size))  
    
  for (v in 1:length(training_size)){

      percent_training  <-  training_size[v]
      # ---------  Generate the cross-validation partitions: -----------------------------------------------------------
      percent <-(1 - percent_training)  #### Hold the testset (distinct from random training set) 
      training_sample_size <- percent_training*N
      cv <- cv.indexing(k, nrow(dataset), percent) # is random, after that i refers to the same thing always  

      
      ###-------------------------------------------- 1) MODEL FITTING ---------------------------------------------------------------------
      acc.LR  <- vector('numeric',length(penalty)) 
      se_acc_LR  <- vector('numeric',length(penalty)) 
      acc.ttb <- vector('numeric',length(penalty)) 
      se_ttb <- vector('numeric',length(penalty)) 

      dec_class_acc <- matrix(nrow=length(penalty), ncol = k)
      dec_TTB_accs <- matrix(nrow=length(penalty), ncol = k)
      dec_tallying_accs <- matrix(nrow=length(penalty), ncol = k)
      
      throwout_output_tallying <-  matrix(nrow=length(penalty), ncol = k)
      throwout_output_ttb <- matrix(nrow=length(penalty), ncol = k)
       
      A_TTB_BayesianTTB <- matrix(nrow=length(penalty), ncol = k)
      A_Tallying_BayesianTallying <- matrix(nrow=length(penalty), ncol = k)
      A_LR_BayesianTTB <- matrix(nrow=length(penalty), ncol = k)
      A_LR_BayesianTallying <- matrix(nrow=length(penalty), ncol = k)
      

      #all_pen <-  vector('list',length = length(penalty))  
      chain_length <- c(8000, 8000, rep(6000, 3), rep(3000,3), rep(2000, 7)) # 14 chain lengths for 14 penalties 
     
  for (p in 1: length(penalty)){
    # penalty: from largest to smallest
    
    ## Location in the loop to the prompt
    cat(paste("\n\nPenalty no.  = ", p, "\n\n"))     
    
    
    # take an example dataset of that size to find the optimal scaling parameter once, and use again
    train_trial <- dataset[cv[1,]==1, ]   
    # XItems and YLabelMat
    x <- as.matrix(train_trial[ ,1:Predictors])    
    y <- replicate(Predictors, train_trial$dependent) 
        
    #####################------- MCMC: optimizing the scaling parameter first so that acceptance rate is ~0.20  -------------------------------------------            
    # using previous mean "out" as new starting point with n=1000 seems to work for penalty = 0, penalty = 7 and penalty = 700
    # works less well for n=500 samples, but still ok for n=700, i.e. acceptance rate for actual runs is ok.
    # tested both for training sample size N=500 and N = 10, and below works for both
    initial <-  matrix(rep(0,Predictors*Predictors), nrow = Predictors)
    factor <- 0.01
    samples <- metropolis_hastings(x,y, initial = initial,
                                   n=1000,penalty_param = penalty[p],factor = factor,Predictors,
                                   burnin=0,sigma) 
    
    out <- apply(samples$result, c(1,2), mean)  
    
    timeout <- Sys.time() + 120 # + 2 minutes  
    ## while the condition holds that we dont want
    while (samples$accept < 0.18 || samples$accept > 0.22){
      
      if (samples$accept < 0.18 ){     ## if need to be higher, make scale smaller  
        # multiply the previous scale with some fucking factor
        factor <- samples$factor * 0.8
        # run again with new factor
        samples <- metropolis_hastings(x,y, initial= out,  
                                       n=1000,penalty_param = penalty[p],factor = factor,Predictors,
                                       burnin=0,sigma)  # whole first 1e3 is burnin        
        out <- apply(samples$result, c(1,2), mean)  # this is the overall posterior mean across all chains!
        
        #cat(paste("\n\nAcceptance rate:\n"))
        print(samples$accept)
      }
      
      if(samples$accept > 0.22){    ## if need to be smaller, make scale larger
        factor <- samples$factor * 1.2
        samples <- metropolis_hastings(x,y, initial= out,
                                       n=1000,penalty_param = penalty[p],factor = factor,Predictors,
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

    Final_Factor <- samples$factor #- 0.003 # instead of running 5000 in optimization
    print(Final_Factor)
    
    all_envs <- vector('list',k)  
    results.linear.regression <- vector('numeric', k)
    results.ttb <- vector('numeric', k)
    results.tallying <- vector('numeric', k)
    meanW <- vector('list',k) 
    for (i in 623:k){
      
      ## Location in the loop to the prompt
      cat(paste("\n\nTest set no.  = ", i, "\n\n"))     
      
      
      # the same 500 training sets are used for  each penalty paramter as cv matrix is pre-determined above
      trainset <- dataset[cv[i,]==1, ]   
      testset <- dataset[cv[i,]==0, ]   
      test <- as.matrix(testset[ ,-ncol(dataset)]) #  without dependent
      test.labels <- testset[ ,ncol(testset)]
      # XItems and YLabelMat
      x <- as.matrix(trainset[ ,1:Predictors])    
      y <- replicate(Predictors, trainset$dependent) 
       

      ###--- ------------------  Regular Linear Regression  ------------------------------------##################################### 
      predictions.LR <- Linear.regression(dataset, cv, ncol(dataset), Predictors, labels) # CAN GUESS NOW
      ###--- ------------------  Regular TTB Heuristic ------------------------------------##################################### 
      predictions.ttb <- ttb.predictions(dataset, cv, ncol(dataset), Predictors)
      ###--- ------------------  Regular Tallying Heuristic ------------------------------------##################################### 
      predictions.tallying <- tallying.learning(dataset, cv, ncol(dataset) , Predictors)
      
      
      
      #---------------------------------------------------------------------------------------------------------------------------------
      start <- vector('list', 4)      
      # range of rnorm is -inf to inf, so these are big steps, could start with sth smaller as well
      start[[1]] <- matrix(rnorm(Predictors*Predictors,mean=0,sd=0.01), nrow = Predictors) 
      start[[2]] <- matrix(rnorm(Predictors*Predictors,mean=0,sd=0.01), nrow = Predictors) 
      start[[3]] <- matrix(rnorm(Predictors*Predictors,mean=0,sd=0.01), nrow = Predictors)
      start[[4]] <- matrix(rnorm(Predictors*Predictors,mean=0,sd=0.01), nrow = Predictors)
      # ------- or sample from prior distribution: as long as they are different enough it is fine -------------------------------------

      #Create a list to store the results of your simulations in
      all_results<- vector("list", 5)
     #Run a parallel loop, each iteration should be a different job you need doing (e.g. fitting your model with different starting parameters)
        all_results<-foreach(d=1:4) %dopar%   
        {   
          library(psych)
          samples <- metropolis_hastings(x,y, initial= start[[d]],
                                                       n = chain_length[p],penalty_param = penalty[p],factor = Final_Factor,Predictors,
                                                       burnin=1000,sigma) 
          
        
         samples$results # return object: contains the (thinned or non-thinned) chain as array
                        
        }   
        # always store the current trainset data.frame in 5th list element
        #all_results[[5]] <-  trainset
        # list with 5 elements is put into a nested list
        #all_envs[[i]] <- all_results 
              
        ##--------- Convergence Test: Between Chains based on different outputs ------------------------------------------------
        mean_chain1 <- apply(all_results[[1]], c(1,2), mean)  # this is the overall posterior mean across all chains!
        mean_chain2 <- apply(all_results[[2]], c(1,2), mean)  # this is the overall posterior mean across all chains!
        mean_chain3 <- apply(all_results[[3]], c(1,2), mean)  # this is the overall posterior mean across all chains!
        mean_chain4 <- apply(all_results[[4]], c(1,2), mean)  # this is the overall posterior mean across all chains!
        
        # try without any artificial cut-off for the off-diagonal elements (going to zero)
        outcomes1 <- test %*% mean_chain1    #
        outcomes2 <- test %*% mean_chain2    # 
        outcomes3 <- test %*% mean_chain3    # 
        outcomes4 <- test %*% mean_chain4    # 
        
        ## 4 different Dec Tallying vectors
        dec_tallying1 <- sign(rowSums(sign(outcomes1)))  # is automatically zero when all cues are zero (before matrix mulitplication)
        dec_tallying2 <- sign(rowSums(sign(outcomes2)))  # is automatically zero when all cues are zero (before matrix mulitplication)
        dec_tallying3 <- sign(rowSums(sign(outcomes3)))  # is automatically zero when all cues are zero (before matrix mulitplication)
        dec_tallying4 <- sign(rowSums(sign(outcomes4)))  # is automatically zero when all cues are zero (before matrix mulitplication)    
        
        # if any 2 of the 4 vectors are NOT identical, sapply gives FALSE, and the negation makes it TRUE for throwing out later
        throwout_output_tallying[p,i]  <- (!(all(sapply(list(dec_tallying2, dec_tallying3,dec_tallying4), FUN = identical, dec_tallying1))))*1
        
        
        ## 4 different TTB output vector to compare
        dec_TTB1 <- vector('numeric',nrow(outcomes1))     
        for (l in 1:nrow(outcomes1)){          
          
          if (identical(outcomes1[l,],rep(0, Predictors))){
            dec_TTB1[l] <- 0# sign(rnorm(1)) #0        # TTB guesses
          }else {
            pos <- which(abs(outcomes1[l, ]) == max(abs(outcomes1[l, ])))      
            dec_TTB1[l] <- sign(outcomes1[l,pos[1]]) # the sign of that output (y = wX) is the prediction, it is the adjusted cue value                  
          }       
        }
        
        ## Dec TTB
        dec_TTB2 <- vector('numeric',nrow(outcomes2))     
        for (l in 1:nrow(outcomes2)){          
          
          if (identical(outcomes2[l,],rep(0, Predictors))){
            dec_TTB2[l] <- 0# sign(rnorm(1)) #0        # TTB guesses
          }else {
            pos <- which(abs(outcomes2[l, ]) == max(abs(outcomes2[l, ])))      
            dec_TTB2[l] <- sign(outcomes2[l,pos[1]]) # the sign of that output (y = wX) is the prediction, it is the adjusted cue value                  
          }       
        }
        
        ## Dec TTB
        dec_TTB3 <- vector('numeric',nrow(outcomes3))     
        for (l in 1:nrow(outcomes3)){          
          
          if (identical(outcomes3[l,],rep(0, Predictors))){
            dec_TTB3[l] <- 0# sign(rnorm(1)) #0        # TTB guesses
          }else {
            pos <- which(abs(outcomes3[l, ]) == max(abs(outcomes3[l, ])))      
            dec_TTB3[l] <- sign(outcomes3[l,pos[1]]) # the sign of that output (y = wX) is the prediction, it is the adjusted cue value                  
          }       
        }
        
        ## Dec TTB
        dec_TTB4 <- vector('numeric',nrow(outcomes4))     
        for (l in 1:nrow(outcomes4)){          
          
          if (identical(outcomes4[l,],rep(0, Predictors))){
            dec_TTB4[l] <- 0# sign(rnorm(1)) #0        # TTB guesses
          }else {
            pos <- which(abs(outcomes4[l, ]) == max(abs(outcomes4[l, ])))      
            dec_TTB4[l] <- sign(outcomes4[l,pos[1]]) # the sign of that output (y = wX) is the prediction, it is the adjusted cue value                  
          }       
        }
        
        
        # if any 2 of the 4 vectors are NOT identical, sapply gives FALSE, and the negation makes it TRUE for throwing out later
        throwout_output_ttb[p,i] <- (!(all(sapply(list(dec_TTB2, dec_TTB3,dec_TTB4), FUN = identical, dec_TTB1))))*1  # turn into 1 if FALSE
        
##---------------------------------------- -Prediction -------------------------------------------------------------------------------------------------

        # Long array: cancatenate the arrays into one big array with 4 *1000= 4000 matrix slices, so dim = 4x4x4000
        all_chains <- abind(all_results[[1]], all_results[[2]],all_results[[3]],all_results[[4]], rev.along=1)
          
        ###------- this is the stats that need to happen outside the dopar loop; accessing the chains (list elements) ---------------
        meanW[[i]] <- apply(all_chains, c(1,2), mean)  # this is the overall posterior mean across all chains!

        ##---------------------- PREDICTION: ---------------------------------------------      
        W_mat  <- meanW[[i]] # cols contain the coefficients 

#       # cut-off for the off-diagonal elements (going to zero)
        epsilon <-0.007 # cut-off 
        W_mat[!diag(dim(W_mat)[1]) & abs(W_mat) < epsilon] <- 0  # if the absolute distance from 0 is smaller than epsilon, make zero      
        outcomes <- test %*% W_mat     #like in mcmc Loglikelihood function: yHat <- x %*% wMat 
           

        ## COR Linear Model: Binary classification  decision rule
        dec_class <- sign(rowSums(outcomes))   # every row contains a test item with all 4 cues, outcomes contain weighted cues        
        ## Dec Tallying 
        dec_tallying <- sign(rowSums(sign(outcomes)))  # is automatically zero when all cues are zero (before matrix mulitplication)
        ## Dec TTB
        dec_TTB <- vector('numeric',nrow(outcomes))     
        for (l in 1:nrow(outcomes)){          
          
          if (identical(outcomes[l,],rep(0, Predictors))){
            dec_TTB[l] <- 0# sign(rnorm(1)) #0        # TTB guesses
          }else {
            pos <- which(abs(outcomes[l, ]) == max(abs(outcomes[l, ])))      
            dec_TTB[l] <- sign(outcomes[l,pos[1]]) # if there is more than one max, it chooses the first one (Earlier in vector)              
          }       
        }

      ## Performance of the TTB decision rules
        
        dec_TTB_accs[p,i] <- sum(dec_TTB == test.labels)/length(test.labels)
        dec_tallying_accs[p,i] <- sum(dec_tallying == test.labels) /length(test.labels)
        dec_class_acc[p,i] <- sum(dec_class == test.labels)/length(test.labels)

        A_TTB_BayesianTTB[p, i] <- sum(predictions.ttb[[i]] == dec_TTB)/length(test.labels) #is 1 when penalty = 100!!!!!!! wooooooooooooooop!   
        A_Tallying_BayesianTallying[p, i] <- sum(predictions.tallying[[i]]==dec_tallying)/length(test.labels) #should be 1 when penalty = 100       
        ## agreement with actual Linear Regression:
        A_LR_BayesianTTB[p, i]  <- sum(predictions.LR[[i]]==dec_TTB)/length(test.labels) # in the limit (penalty=100) LR_dec = TTB_dec?            
        A_LR_BayesianTallying[p, i]  <- sum(predictions.LR[[i]]==dec_tallying)/length(test.labels) # this one is good. is 1 when penalty = 0 :)               
        
       # get LR accuracy
        results.linear.regression[i] <- regression.graph(predictions.LR[[i]], test.labels)
        # get TTB Heuristic accuracy
        results.ttb[i] <- ttb.graph(predictions.ttb[[i]], test.labels)
        # get TTB Heuristic accuracy
        results.tallying[i] <- ttb.graph(predictions.tallying[[i]], test.labels)


      } #k loop
      
    ## put the list with 500 environments (their arrays) into the pth list element
      #all_pen[[p]] <- all_envs
  

      } # p loop
    
      #all_training[[v]] <-  all_pen

        
##---------------- Summary of results:  TTB Output based: any penalty parameter
        indice1 <- colSums(throwout_output_ttb) # vector , if greater than 0, throw out environment.
        indice1 <- !(indice1 > 0) # logical vector: TRUE/FALSE -> inverse to exclude below
        # count k that was used across all 500 environments!
        k1 <- sum(indice1 * 1)
        throwout_percent_ttboutput <- rowSums(throwout_output_ttb)/k1
    
## performance as a function of penalty parameter minus non-converged environments
        acc_dec_class <- apply(dec_class_acc[ ,indice1], 1, mean)
        se_acc_class <- apply(dec_class_acc[ ,indice1], 1, function(x) sd(x)/sqrt(k1)) 
        acc_dec_TTB <- apply(dec_TTB_accs[ ,indice1], 1, mean)
        se_acc_TTB <- apply(dec_TTB_accs[ ,indice1], 1, function(x) sd(x)/sqrt(k1)) 
        acc_dec_tallying <- apply(dec_tallying_accs[ ,indice1], 1, mean)
        se_acc_tallying <- apply(dec_tallying_accs[ ,indice1], 1, function(x) sd(x)/sqrt(k1)) 

## performance as a function of penalty parameter minus non-converged environments
      acc.LR <- sum(results.linear.regression[indice1])/k1
      se_acc_LR <- sd(results.linear.regression[indice1])/sqrt(k1)
      acc.ttb <- sum(results.ttb[indice1])/k1 # always the same independent of p 
      se_ttb <- sd(results.ttb[indice1])/sqrt(k1)
      acc.tallying <- sum(results.tallying[indice1])/k1 # always the same independent of p 
      se_tallying <- sd(results.tallying[indice1])/sqrt(k1)
    

      Agreement_TTB_BayesianTTB <- apply(A_TTB_BayesianTTB[ ,indice1], 1, mean) #rowSums(A_TTB_BayesianTTB)/k  
      se_TTB_BayesianTTB <- apply(A_TTB_BayesianTTB[ ,indice1], 1, function(x) sd(x)/sqrt(k1)) # sd(A_TTB_BayesianTTB[1, ])/sqrt(k1)
      Agreement_Tallying_BayesianTallying <- apply(A_Tallying_BayesianTallying[ ,indice1], 1, mean)  # is vector of length(penalty)
      se_Tallying_BayesianTallying <- apply(A_Tallying_BayesianTallying[ ,indice1], 1, function(x) sd(x)/sqrt(k1)) # sd(A_TTB_BayesianTTB[1, ])/sqrt(k1)
      Agreement_LR_BayesianTTB <- apply(A_LR_BayesianTTB[ ,indice1], 1, mean)  #
      se_LR_BayesianTTB <- apply(A_LR_BayesianTTB[ ,indice1], 1, function(x) sd(x)/sqrt(k1)) # sd(A_TTB_BayesianTTB[1, ])/sqrt(k1)
      Agreement_LR_BayesianTallying <- apply(A_LR_BayesianTallying[ ,indice1], 1, mean)  # is vector of length(penalty)
      se_LR_BayesianTallying <- apply(A_LR_BayesianTallying[ ,indice1], 1, function(x) sd(x)/sqrt(k1))

     
      ## --------------------- Write all results to a data frame and file ----------------------------------------------------    
      ttboutput <- data.frame(data = paste(ALL_DATA[s]), v, percent_training, training_N = round(training_sample_size,0),  
                              throwout_percent_ttboutput= throwout_percent_ttboutput, 
                              penalty = penalty, Class_dec = acc_dec_class, se_acc_class = se_acc_class,
                          
                           acc_LR = acc.LR,  se_acc_LR = se_acc_LR, TTB_dec = acc_dec_TTB, se_acc_TTB = se_acc_TTB, 
                           Tallying_Dec = acc_dec_tallying, se_acc_tallying = se_acc_tallying,
                           TTB_Heuristic = acc.ttb, se_ttb_heuristic = se_ttb,
                           Tallying_Heuristic =  acc.tallying, se_ttb_heuristic = se_tallying,
                                                      
                           Agreement_TTB_BayesianTTB = Agreement_TTB_BayesianTTB,  se_TTB_BayesianTTB =  se_TTB_BayesianTTB, 
                           Agreement_Tallying_BayesianTallying = Agreement_Tallying_BayesianTallying, 
                           se_Tallying_BayesianTallying = se_Tallying_BayesianTallying, 
                           Agreement_LR_BayesianTTB = Agreement_LR_BayesianTTB,
                           se_LR_BayesianTTB = se_LR_BayesianTTB, 
                           Agreement_LR_BayesianTallying = Agreement_LR_BayesianTallying, 
                           se_LR_BayesianTallying = se_LR_BayesianTallying, 
                           
         
                           final_factor = samples$factor, final_accept = samples$accept, 
                           betas_LR = betas_LR ,#betas of whole dataset without penalization (matrix)
                           ind_coef  = ind_coeffs, # independent predictors at 0 for whole dataset (matrix)
                           Predictors, N,  k_used = k1, k_overall = k,  test_size = N - training_sample_size,
                           cov_mean = abs_mean_cor,cov_min = min_cor,cov_max = max_cor)                              
      
      
      if (v > 1) ttboutput <- rbind(results1, ttboutput) # append
      results1  <- ttboutput
      
      # updates for every v loop advance
      save(results1, file= paste("COR_", ALL_DATA[s],"_throwout_TTB_allN.RData", sep = ""))
      write.csv(results1, file= paste("COR_", ALL_DATA[s],"_throwout_TTB_allN.csv", sep =""), row.names=TRUE)  
      
      

##---------------- Summary of results:  Tallying Output based: any penalty parameter
          indice2 <- colSums(throwout_output_tallying) # vector , if greater than 0, throw out environment.
          indice2 <- !(indice2 > 0) # logical vector: TRUE/FALSE -> inverse to exclude below
          # count k that was used across all 500 environments!
          k2 <- sum(indice2 * 1)
          throwout_percent_tallyingoutput <- rowSums(throwout_output_tallying)/k2

          ## performance as a function of penalty parameter minus non-converged environments
          acc_dec_class <- apply(dec_class_acc[ ,indice2], 1, mean)
          se_acc_class <- apply(dec_class_acc[ ,indice2], 1, function(x) sd(x)/sqrt(k2)) 
          acc_dec_TTB <- apply(dec_TTB_accs[ ,indice2], 1, mean)
          se_acc_TTB <- apply(dec_TTB_accs[ ,indice2], 1, function(x) sd(x)/sqrt(k2)) 
          acc_dec_tallying <- apply(dec_tallying_accs[ ,indice2], 1, mean)
          se_acc_tallying <- apply(dec_tallying_accs[ ,indice2], 1, function(x) sd(x)/sqrt(k2)) 
          
          ## performance as a function of penalty parameter minus non-converged environments
          acc.LR <- sum(results.linear.regression[indice2])/k2
          se_acc_LR <- sd(results.linear.regression[indice2])/sqrt(k2)
          acc.ttb <- sum(results.ttb[indice2])/k2 # always the same independent of p 
          se_ttb <- sd(results.ttb[indice2])/sqrt(k2)
          acc.tallying <- sum(results.tallying[indice2])/k2 # always the same independent of p 
          se_tallying <- sd(results.tallying[indice2])/sqrt(k2)
          
          
          Agreement_TTB_BayesianTTB <- apply(A_TTB_BayesianTTB[ ,indice2], 1, mean) #rowSums(A_TTB_BayesianTTB)/k  
          se_TTB_BayesianTTB <- apply(A_TTB_BayesianTTB[ ,indice2], 1, function(x) sd(x)/sqrt(k2)) # sd(A_TTB_BayesianTTB[1, ])/sqrt(k2)
          Agreement_Tallying_BayesianTallying <- apply(A_Tallying_BayesianTallying[ ,indice2], 1, mean)  # is vector of length(penalty)
          se_Tallying_BayesianTallying <- apply(A_Tallying_BayesianTallying[ ,indice2], 1, function(x) sd(x)/sqrt(k2)) # sd(A_TTB_BayesianTTB[1, ])/sqrt(k2)
          Agreement_LR_BayesianTTB <- apply(A_LR_BayesianTTB[ ,indice2], 1, mean)  #
          se_LR_BayesianTTB <- apply(A_LR_BayesianTTB[ ,indice2], 1, function(x) sd(x)/sqrt(k2)) # sd(A_TTB_BayesianTTB[1, ])/sqrt(k2)
          Agreement_LR_BayesianTallying <- apply(A_LR_BayesianTallying[ ,indice2], 1, mean)  # is vector of length(penalty)
          se_LR_BayesianTallying <- apply(A_LR_BayesianTallying[ ,indice2], 1, function(x) sd(x)/sqrt(k2))



tallyingoutput <- data.frame(data = paste(ALL_DATA[s]), v, percent_training, training_N = round(training_sample_size,0),  
                        throwout_percent_tallyingoutput = throwout_percent_tallyingoutput, 
                        penalty = penalty, Class_dec = acc_dec_class, se_acc_class = se_acc_class,
                        
                        acc_LR = acc.LR,  se_acc_LR = se_acc_LR, TTB_dec = acc_dec_TTB, se_acc_TTB = se_acc_TTB, 
                        Tallying_Dec = acc_dec_tallying, se_acc_tallying = se_acc_tallying,
                        TTB_Heuristic = acc.ttb, se_ttb_heuristic = se_ttb,
                        Tallying_Heuristic =  acc.tallying, se_ttb_heuristic = se_tallying,
                        
                        Agreement_TTB_BayesianTTB = Agreement_TTB_BayesianTTB,  se_TTB_BayesianTTB =  se_TTB_BayesianTTB, 
                        Agreement_Tallying_BayesianTallying = Agreement_Tallying_BayesianTallying, 
                        se_Tallying_BayesianTallying = se_Tallying_BayesianTallying, 
                        Agreement_LR_BayesianTTB = Agreement_LR_BayesianTTB,
                        se_LR_BayesianTTB = se_LR_BayesianTTB, 
                        Agreement_LR_BayesianTallying = Agreement_LR_BayesianTallying, 
                        se_LR_BayesianTallying = se_LR_BayesianTallying, 
                        
                        
                        final_factor = samples$factor, final_accept = samples$accept, 
                        betas_LR = betas_LR ,#betas of whole dataset without penalization (matrix)
                        ind_coef  = ind_coeffs, # independent predictors at 0 for whole dataset (matrix)
                        Predictors, N,  k_used = k2, k_overall = k,  test_size = N - training_sample_size,
                        cov_mean = abs_mean_cor,cov_min = min_cor,cov_max = max_cor)    
                          
      
      if (v > 1) tallyingoutput <- rbind(results2, tallyingoutput) # append
      results2  <- tallyingoutput  
      # updates for every v loop advance
      save(results2, file= paste("COR_", ALL_DATA[s],"_throwout_Tallying_allN.RData", sep = ""))
      write.csv(results2, file= paste("COR_", ALL_DATA[s],"_throwout_Tallying_allN.csv", sep =""), row.names=TRUE)  


## have to run everything below here, as it never got there! 
    ## NO FILTER: this is without filtering averaging across all environments


    ## performance as a function of penalty parameter minus non-converged environments
    acc_dec_class <- apply(dec_class_acc, 1, mean)
    se_acc_class <- apply(dec_class_acc, 1, function(x) sd(x)/sqrt(k)) 
    acc_dec_TTB <- apply(dec_TTB_accs, 1, mean)
    se_acc_TTB <- apply(dec_TTB_accs, 1, function(x) sd(x)/sqrt(k)) 
    acc_dec_tallying <- apply(dec_tallying_accs, 1, mean)
    se_acc_tallying <- apply(dec_tallying_accs, 1, function(x) sd(x)/sqrt(k)) 
    
    ## performance as a function of penalty parameter minus non-converged environments
    acc.LR <- sum(results.linear.regression)/k
    se_acc_LR <- sd(results.linear.regression)/sqrt(k)
    acc.ttb <- sum(results.ttb)/k # always the same independent of p 
    se_ttb <- sd(results.ttb)/sqrt(k)
    acc.tallying <- sum(results.tallying)/k # always the same independent of p 
    se_tallying <- sd(results.tallying)/sqrt(k)
    
    
    Agreement_TTB_BayesianTTB <- apply(A_TTB_BayesianTTB, 1, mean) #rowSums(A_TTB_BayesianTTB)/k  
    se_TTB_BayesianTTB <- apply(A_TTB_BayesianTTB, 1, function(x) sd(x)/sqrt(k)) # sd(A_TTB_BayesianTTB[1, ])/sqrt(k)
    Agreement_Tallying_BayesianTallying <- apply(A_Tallying_BayesianTallying, 1, mean)  # is vector of length(penalty)
    se_Tallying_BayesianTallying <- apply(A_Tallying_BayesianTallying, 1, function(x) sd(x)/sqrt(k)) # sd(A_TTB_BayesianTTB[1, ])/sqrt(k)
    Agreement_LR_BayesianTTB <- apply(A_LR_BayesianTTB, 1, mean)  #
    se_LR_BayesianTTB <- apply(A_LR_BayesianTTB, 1, function(x) sd(x)/sqrt(k)) # sd(A_TTB_BayesianTTB[1, ])/sqrt(k)
    Agreement_LR_BayesianTallying <- apply(A_LR_BayesianTallying, 1, mean)  # is vector of length(penalty)
    se_LR_BayesianTallying <- apply(A_LR_BayesianTallying, 1, function(x) sd(x)/sqrt(k))
    


    ## --------------------- Write all results to a data frame and file ----------------------------------------------------    
    cv.run <- data.frame(data = paste(ALL_DATA[s]), v, percent_training, training_N = round(training_sample_size,0),  
                         throwout_percent = 'none', 
                         penalty = penalty, Class_dec = acc_dec_class, se_acc_class = se_acc_class,
                         
                         acc_LR = acc.LR,  se_acc_LR = se_acc_LR, TTB_dec = acc_dec_TTB, se_acc_TTB = se_acc_TTB, 
                         Tallying_Dec = acc_dec_tallying, se_acc_tallying = se_acc_tallying,
                         TTB_Heuristic = acc.ttb, se_ttb_heuristic = se_ttb,
                         Tallying_Heuristic =  acc.tallying, se_ttb_heuristic = se_tallying,
                         
                         Agreement_TTB_BayesianTTB = Agreement_TTB_BayesianTTB,  se_TTB_BayesianTTB =  se_TTB_BayesianTTB, 
                         Agreement_Tallying_BayesianTallying = Agreement_Tallying_BayesianTallying, 
                         se_Tallying_BayesianTallying = se_Tallying_BayesianTallying, 
                         Agreement_LR_BayesianTTB = Agreement_LR_BayesianTTB,
                         se_LR_BayesianTTB = se_LR_BayesianTTB, 
                         Agreement_LR_BayesianTallying = Agreement_LR_BayesianTallying, 
                         se_LR_BayesianTallying = se_LR_BayesianTallying, 
                         
                         final_factor = samples$factor, final_accept = samples$accept, 
                         betas_LR = betas_LR ,#betas of whole dataset without penalization (matrix)
                         ind_coef  = ind_coeffs, # independent predictors at 0 for whole dataset (matrix)
                         Predictors, N, k_used = k, k_overall = k,  test_size = N - training_sample_size,
                         cov_mean = abs_mean_cor,cov_min = min_cor,cov_max = max_cor)    
                            
    

    if (v > 1) cv.run <- rbind(results, cv.run) # append
    results  <- cv.run  
    
    # updates for every v loop advance
    save(results, file= paste("COR_", ALL_DATA[s],"_throwout_none_allN.RData", sep = ""))
    write.csv(results, file= paste("COR_", ALL_DATA[s],"_throwout_none_allN.csv", sep =""), row.names=TRUE)  
       
  } # v loop
  
   

    ##----- store the mcmc chains for all environments for later:list of a list of a list of arrays ------------------------------------
    #save(all_training, file = "mcmc_chains_abc_alltraining.RData")
    
    
    
    
#} # s loop through 18 data sets
