### 3-panel plots for N, performance, and covariance -------------------------------

library(ggplot2)
library(gridExtra)

pdf(file='2panels.pdf', width=9, height=15)
#par(mfrow=(c(3,1)))
#x11(width=30, height=20) 

##----------------------- 1) Agreement plot ---------------------------
## Data agreement 
    pdf(file='panel1_new.pdf', width=8, height=4)

    load("agreement_s77_identity_scaling.RData") ##most recent correct version
    agreement <- agreement_s77_identity_scaling   # assign new data to agreement data frame
    agreement$agreement_TTB_model <- agreement$agreement_TTB_model* 100 # transform into percentages
    agreement$agreement_TTBdec_LR <- agreement$agreement_TTBdec_LR* 100
    
    #agreement$penalty[agreement$penalty == 0.01] <- 0.000001 # just for presentation


    #options(scipen=999)
    #manual setting of the legend
    cols <- c("TTB Heuristic"="darkorchid1","Linear Regression"="goldenrod1")
    p1 <- ggplot(agreement, aes(y=agreement_TTB_model, x=penalty,
        ymin = 91, ymax = 100))  +
        scale_x_log10(breaks = c(0.01, 0.1, 1, 5, 30, 100)) +
        geom_line(aes(penalty,agreement_TTB_model, colour="TTB Heuristic"), linetype = "longdash",data = agreement, size = 1.5) + 
        geom_line(aes(penalty,agreement_TTBdec_LR, colour="Linear Regression"),linetype = "dotted", data = agreement, size = 1.5) +
        geom_point(aes(penalty,agreement_TTB_model, colour="TTB Heuristic"),shape = 22, size = 4, fill = "white") +
        geom_point(aes(penalty,agreement_TTBdec_LR, colour="Linear Regression"), shape = 21, size = 4, fill = "white") +  
        labs(x = NULL, y = NULL) +
        scale_colour_hue(name="Model", l=30, guide = guide_legend(fill = NULL,colour = NULL))  +  # can add name here too! :) this somehow uses old colours 
        theme_bw() +
        theme(legend.title= element_text(size=14, face="bold"),legend.position=c(.8,.5), legend.text=element_text(size=12)) + 
        theme(panel.background = element_rect(colour = "black"), axis.text= element_text(size=12)) 

  
        #scale_colour_manual(name="Model",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) + 
      
#         geom_point(aes(colour="TTB Heuristic"),shape = 17, size = 2) + 
#         geom_point(aes(x = agreement$penalty, y = agreement$agreement_TTBdec_LR, colour="Linear Regression"), 
#                     shape = 16, size = 2) 

    dev.off()


###------- make the same plot for Tallying Dec --------------------------------------------------------------------
      load("agreement_s77_identity_scaling.RData") ##most recent correct version
      agreement <- agreement_s77_identity_scaling   # assign new data to agreement data frame
      agreement$agreement_Tallying_model <- agreement$agreement_Tallying_model* 100 # transform into percentages
      agreement$agreement_Tallyingdec_LR <- agreement$agreement_Tallyingdec_LR* 100  
    
      pdf(file='S2_Tallying_Agreement.pdf', width=8, height=4)
  
      #options(scipen=999)
      #manual setting of the legend
      cols <- c("Tallying Heuristic"="darkorchid1","Linear Regression"="goldenrod1")
      p2 <- ggplot(agreement, aes(y=agreement_Tallying_model, x=penalty,
                                  ymin = 91, ymax = 100))  +
            scale_x_log10(breaks = c(0.01, 0.1, 1, 5, 30, 100)) +
            geom_line(aes(penalty,agreement_Tallying_model, colour="Tallying Heuristic"), linetype = "longdash",data = agreement, size = 1.5) + 
            geom_line(aes(penalty,agreement_Tallyingdec_LR, colour="Linear Regression"),linetype = "dotted", data = agreement, size = 1.5) +
            geom_point(aes(penalty,agreement_Tallying_model, colour="Tallying Heuristic"),shape = 22, size = 4, fill = "white") +
            geom_point(aes(penalty,agreement_Tallyingdec_LR, colour="Linear Regression"), shape = 21, size = 4, fill = "white") +  
            labs(x = NULL, y = NULL) +
            scale_colour_hue(name="Model", l=30, guide = guide_legend(fill = NULL,colour = NULL))  +  # can add name here too! :) this somehow uses old colours 
            theme_bw() +
            theme(legend.title= element_text(size=14, face="bold"),legend.position=c(.25,.5), legend.text=element_text(size=14), 
                  legend.key.size = unit(1, "cm")) + 
            theme(panel.background = element_rect(colour = "black"), axis.text= element_text(size=12)) 
          
      dev.off()

#0.000001,0.1,0.5,1,2,5,10,30, 100



##----------------------- 2) Performance N plot ---------------------------
### Data performance
    pdf(file='panel2_new.pdf', width=8, height=4)
    
    load("performance_N3500_all_training_Pred5_inclTies.RData") ##
    perf <- performance_N3500_all_training_Pred5
    penalty <- perf$penalty[1:15]
    penalty[penalty == 0] <- 0.000001
    perf$TTB_dec <- perf$TTB_dec * 100 # percentages 
    
    TTB_dec1 <- round(perf$TTB_dec[1:15],3) # small N = 20
    TTB_dec2 <- round(perf$TTB_dec[16:30],3) # N = 50 (medium)
    TTB_dec3 <- round(perf$TTB_dec[31:45],3) # N = 100 (large)
    
    # new df with necessary variables 
    df <- data.frame(penalty, TTB_dec1, TTB_dec2, TTB_dec3)
    
    #options(scipen=999)
    #manual setting of the legend
    
  #cols <- c("Large"="darkgoldenrod1","Small"="cyan4") 
   cols <- c("Large"="dimgrey","Small"="deepskyblue4") 
    p3 <- ggplot(df, aes(x=penalty, y = TTB_dec1, ymin = 75, ymax = 90)) +  
          scale_x_log10(breaks = c(0.000001, 0.1, 1, 5, 20,100))  +
          geom_line(aes(penalty,y = TTB_dec1, colour="Small"), data = df, linetype = "longdash",linetype = 1, size = 1.5) + 
          geom_line(aes(penalty,y = TTB_dec3, colour="Large"), data = df, linetype = "dotted", size = 1.5) +
          geom_point(aes(x= penalty, y = TTB_dec1, colour="Small"),shape = 22, size = 4,fill = "white") +
          geom_point(aes(x= penalty, y = TTB_dec3, colour="Large"),shape = 24, size = 4,fill = "white") +  
          labs(x =NULL, y = NULL) +
          scale_colour_manual(name="Training Sample Size",labels = c("Large", "Small"), values=cols) + 
           theme_bw() +
          theme(legend.title= element_text(size=14, face="bold"),legend.position=c(.75,.25), legend.text=element_text(size=12)) + 
          theme(panel.background = element_rect(colour = "black"), axis.text= element_text(size=12)) 

  dev.off()




#       + geom_point(aes(x= penalty, y = TTB_dec1, colour="Small Training"),shape = 17, size = 2) +
#        geom_point(aes(x = penalty, y = TTB_dec3, colour="Large Training"), shape = 16, size = 2) 
    pdf(file='2panels.pdf', width=9, height=10)
    grid.arrange(p1, p2, ncol=1)
    dev.off()

###------ ----- Performance N plot for Tallying Dec -------------------------------------------------------------------
    pdf(file='S3_Tallying_N.pdf', width=8, height=4)
    
    load("performance_N3500_all_training_Pred5_inclTies.RData") ##
    perf <- performance_N3500_all_training_Pred5
    penalty <- perf$penalty[1:15]
    penalty[penalty == 0] <- 0.000001
    perf$tallying_dec <- perf$tallying_dec * 100 # percentages 

    tallying_dec1 <- round(perf$tallying_dec[1:15],3) # small N = 20
    tallying_dec2 <- round(perf$tallying_dec[16:30],3) # N = 50 (medium)
    tallying_dec3 <- round(perf$tallying_dec[31:45],3) # N = 100 (large)
    
    # new df with necessary variables 
    df <- data.frame(penalty, tallying_dec1, tallying_dec2, tallying_dec3)
    
    #options(scipen=999)
    #manual setting of the legend
    #cols <- c("Large"="darkmagenta","Small"="cyan4") 
    cols <- c("Large"="dimgrey","Small"="deepskyblue4") 
    p4 <- ggplot(df, aes(y=tallying_dec1, x=penalty,
                         ymin = 75, ymax = 90)) +  scale_x_log10(breaks = c(0.000001, 0.1, 1, 5, 20,100))  +
      geom_line(aes(penalty,tallying_dec1, colour="Small"), data = df, linetype = "longdash",linetype = 1, size = 1.5) + 
      geom_line(aes(penalty,tallying_dec3, colour="Large"), data = df, linetype = "dotted", size = 1.5) +
      geom_point(aes(x= penalty, y = tallying_dec1, colour="Small"),shape = 22, size = 4,fill = "white") +
      geom_point(aes(x= penalty, y = tallying_dec3, colour="Large"),shape = 24, size = 4,fill = "white") +  
      labs(x =NULL, y = NULL) +
      scale_colour_manual(name="Training Sample Size",labels = c("Large", "Small"), values=cols) + 
      theme_bw() +
      theme(legend.title= element_text(size=14, face="bold"),legend.position=c(.75,.25), legend.text=element_text(size=12)) + 
      theme(panel.background = element_rect(colour = "black"), axis.text= element_text(size=12)) 
    
      dev.off()

##----------------------- 3) Covariance plot ---------------------------
### Data covariance
#pdf(file='panel3.pdf', width=8, height=4)


## TTB decision rule #######
    optimal_penalty <- c(1,5,5,5,20) # best performing penalty parameter in that covariance env.
    covariance_levels <- c(0.06,0.14,0.20,0.34,0.6) # sample statisic avg. covariance 
    cov.df <- data.frame(optimal_penalty, covariance_levels)
    
    #options(scipen=999)
    
    p3 <- ggplot(cov.df, aes(x=covariance_levels, y=optimal_penalty)) + 
                geom_point(aes(x= covariance_levels, y=optimal_penalty, colour="darkgoldenrod1"),shape = 13, size = 5) +
                geom_line(aes(x= covariance_levels, y=optimal_penalty, colour="darkgoldenrod1"), data = cov.df, linetype = 1, size = 0.8) + 
                labs(x = "", y = "") + 
                scale_colour_discrete(guide = FALSE)

      grid.arrange(p1, p2, p3, ncol=1)

# "Grob" the dimensions of each plot 
      gA <- ggplotGrob(p1)
      gB <- ggplotGrob(p2)
      gC <- ggplotGrob(p3) 

      maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5],
                                 gC$widths[2:5])
      gA$widths[2:5] <- as.list(maxWidth)
      gB$widths[2:5] <- as.list(maxWidth)
      gC$widths[2:5] <- as.list(maxWidth)

      grid.arrange(gA, gB, gC, ncol=1)


dev.off()



#----------------------- 4) Covariance x N plots ---------------------------
### Data performance

  pdf(file='Nbycovariance.pdf', width=8, height=4)

  load("allcov_N561_Pred3_training100.RData") ##
  load("allcov_N561_Pred3_training20.RData") ##
  large <- allcov_N561_Pred3_training100
  small <- allcov_N561_Pred3_training20
  
  large$TTB_dec <- large$TTB_dec *100
  small$TTB_dec <- small$TTB_dec *100
  
  x <- 14 # number of penalties
  
  small0 <- small$TTB_dec[1:x]
  large0 <- large$TTB_dec[1:x]
  small0.1 <- small$TTB_dec[(x+1):(2*x)]
  large0.1 <- large$TTB_dec[(x+1):(2*x)]
  small0.2 <- small$TTB_dec[(2*x+1):(3*x)]
  large0.2 <- large$TTB_dec[(2*x+1):(3*x)]
  small0.3 <- small$TTB_dec[(3*x+1):(4*x)]
  large0.3 <- large$TTB_dec[(3*x+1):(4*x)]
  small0.4 <- small$TTB_dec[(4*x+1):(5*x)]
  large0.4 <- large$TTB_dec[(4*x+1):(5*x)]
  small0.5 <- small$TTB_dec[(5*x+1):(6*x)]
  large0.5 <- large$TTB_dec[(5*x+1):(6*x)]
  small0.6 <- small$TTB_dec[(6*x+1):(7*x)]
  large0.6 <- large$TTB_dec[(6*x+1):(7*x)]
  small0.7 <- small$TTB_dec[(7*x+1):(8*x)]
  large0.7 <- large$TTB_dec[(7*x+1):(8*x)]
  small0.8 <- small$TTB_dec[(8*x+1):(9*x)]
  large0.8 <- large$TTB_dec[(8*x+1):(9*x)]
  small0.9 <- small$TTB_dec[(9*x+1):(10*x)]
  large0.9 <- large$TTB_dec[(9*x+1):(10*x)]
  
  penalty <- small$penalty[1:x]
  
  
  # new df with necessary variables 
  df <- data.frame(penalty, small0, large0,small0.1, large0.1,small0.2, large0.2, small0.3, large0.3, small0.4, large0.4, small0.5, large0.5,
                   small0.6, large0.6,small0.7, large0.7, small0.8, large0.8, small0.9, large0.9)
  
  options(scipen=999)
  #manual setting of the legend
  cols <- c("Large Training"="brown","Small Training"="brown1")

p1 <- ggplot(df, aes(y=small0, x=penalty,
        ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
        geom_line(aes(penalty,small0, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
        geom_line(aes(penalty,large0, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
        labs(x = "penalty", y = "") +
        scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
        theme(legend.position="right")+
        ggtitle("cov = 0")

p2 <- ggplot(df, aes(y=small0.1, x=penalty,
                     ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
      geom_line(aes(penalty,small0.1, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
      geom_line(aes(penalty,large0.1, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
      labs(x = "", y = "") +
      scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
      theme(legend.position="right")+
      ggtitle("cov = 0.1")

p3 <- ggplot(df, aes(y=small0.2, x=penalty,
                     ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
      geom_line(aes(penalty,small0.2, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
      geom_line(aes(penalty,large0.2, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
      labs(x = "", y = "") +
      scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
      theme(legend.position="right")+
      ggtitle("cov = 0.2")

p4 <- ggplot(df, aes(y=small0.3, x=penalty,
                     ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
      geom_line(aes(penalty,small0.3, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
      geom_line(aes(penalty,large0.3, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
      labs(x = "", y = "") +
      scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
      theme(legend.position="right") +
      ggtitle("cov = 0.3")

p5 <- ggplot(df, aes(y=small0.4, x=penalty,
                     ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
      geom_line(aes(penalty,small0.4, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
      geom_line(aes(penalty,large0.4, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
      labs(x = "", y = "") +
      scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
      theme(legend.position="right") +
      ggtitle("cov = 0.4") 


p6 <- ggplot(df, aes(y=small0.5, x=penalty,
                     ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
      geom_line(aes(penalty,small0.5, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
      geom_line(aes(penalty,large0.5, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
      labs(x = "", y = "") +
      scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
      theme(legend.position="right")+
      ggtitle("cov = 0.5")


p7 <- ggplot(df, aes(y=small0.6, x=penalty,
                     ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
      geom_line(aes(penalty,small0.6, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
      geom_line(aes(penalty,large0.6, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
      labs(x = "", y = "") +
      scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
      theme(legend.position="right")+
      ggtitle("cov = 0.6")

p8 <- ggplot(df, aes(y=small0.7, x=penalty,
                     ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
      geom_line(aes(penalty,small0.7, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
      geom_line(aes(penalty,large0.7, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
      labs(x = "", y = "") +
      scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
      theme(legend.position="right")+
      ggtitle("cov = 0.7")


p9 <- ggplot(df, aes(y=small0.8, x=penalty,
                     ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
      geom_line(aes(penalty,small0.8, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
      geom_line(aes(penalty,large0.8, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
      labs(x = "", y = "") +
      scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
      theme(legend.position="right")+
      ggtitle("cov = 0.8")


p10 <- ggplot(df, aes(y=small0.9, x=penalty,
                     ymin = 50, ymax = 70)) +  scale_x_log10(breaks = c(0.01,0.1,0.5,1,2,5,10,30, 100))  +
      geom_line(aes(penalty,small0.9, colour="Small Training"), data = df, linetype = 1, size = 0.8) + 
      geom_line(aes(penalty,large0.9, colour="Large Training"), data = df, linetype = 1, size = 0.8) +
      labs(x = "", y = "") +
      scale_colour_manual(name="",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) +
      theme(legend.position="right")+
      ggtitle("cov = 0.9")
    

