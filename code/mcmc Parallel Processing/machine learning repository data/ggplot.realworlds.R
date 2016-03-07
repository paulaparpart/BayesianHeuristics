#### --------------  GGPlot of real world UCI data sets ------------------------------------------------------------------
    
    rm(list=ls())  
        
    #####################
    library(ggplot2)
    library(gridExtra)

##----------------------- Performance N plot ---------------------------
    
  ## specify data set
    load ("COR_banknotes.RData") # re-done
    load("COR_survival.rdata") # re-done
    load("COR_bloodtransfer.rdata") # re-done
    load("COR_iris.rdata") # re-done
    load("COR_diabetes.rdata")  # re-done
    load("COR_iris_2ndclass.rdata") # has already been done

    load("banknotes.RData") # done  (repitition of the old banknotes)
    load("bupa.RData") # done
    load("phoneme.RData") # done
    load("iris_vv.RData") # done
    load("banana.RData") # done
    load("wholesale.RData")   ### done
    load("appendicitis.RData") # done  
    load("banknotes_interaction.RData") # done
    load("skincolour.RData")# done
    load("titanic.RData") # done
    load("TicTacToe.RData") #done
    load("vertebral.RData") # run this still!

   
##---------------------------- New runs with corrected mcmc ---------------------------------------------------------
    load("banknotes_corrected.RData") 
    load("survival_corrected.RData") 
    load("phoneme_corrected.RData") ## takes fucking ages. use optimization and compare 



    perf <- results
    penalty <- perf$penalty[perf$training_N==10]
    penalty[penalty == 0] <- 0.000001

    perf$Class_dec <- perf$Class_dec * 100 # percentages 
    perf$se_acc_class <- perf$se_acc_class * 100 # percentages 


    training10 <- perf$Class_dec[perf$training_N==10] 
    #perf$acc_LR[perf$training_N==10]
#    training20 <- perf$Class_dec[perf$training_N==20]
#     training30 <- perf$Class_dec[perf$training_N==30]
#     training50 <- perf$Class_dec[perf$training_N==50]
#     training81 <- perf$Class_dec[perf$training_N==81]

    training100 <- perf$Class_dec[perf$training_N==100]
#    training500 <- perf$Class_dec[perf$training_N==500]

    se_10 <- perf$se_acc_class[perf$training_N==10] 
#    se_20 <- perf$se_acc_class[perf$training_N==20] 
#     se_30 <- perf$se_acc_class[perf$training_N==30] 
 
#     se_50 <- perf$se_acc_class[perf$training_N==50] 
#     se_81 <- perf$se_acc_class[perf$training_N==81] 
    se_100 <- perf$se_acc_class[perf$training_N==100] 
#    se_500 <- perf$se_acc_class[perf$training_N==500] 


    # new df with necessary variables 
    #df <- data.frame(penalty, training10, training20, training30,  training50)
    df <- data.frame(penalty, training10, training100)
    
  #options(scipen=999)
  
    # Define the top and bottom of the errorbars
    limits10 <- aes(ymax = training10 + se_10, ymin= training10 - se_10)
#    limits20 <- aes(ymax = training20 + se_20, ymin= training20 - se_20)
#     limits30 <- aes(ymax = training30 + se_30, ymin= training30 - se_30)
#     limits50 <- aes(ymax = training50 + se_50, ymin= training50 - se_50)
#     limits81 <- aes(ymax = training81 + se_81, ymin= training81 - se_81)

    limits100 <- aes(ymax = training100 + se_100, ymin= training100 - se_100)
#    limits500 <- aes(ymax = training500 + se_500, ymin= training500 - se_500)

     #cols <- c("81"="darkblue", "50"="darkgoldenrod1", "20" = "yellow", "10" = "green") 
    cols <- c("500"="dimgrey","100"="darkblue", "20" = "yellow", "10" = "green") 
    #cols <- c("500"="dimgrey","100"="darkblue","50"="darkgoldenrod1","30"="cyan4", "20" = "yellow", "10" = "green") 

    cols <- c("Large"="dimgrey","Small"="deepskyblue4") 

p1 <- ggplot(df, aes(x=penalty, y = training10, ymin = 80, ymax = 95)) +  
      #scale_x_log10(breaks = round(c(0.01, penalty[25], penalty[20], penalty[15], penalty[10], penalty[5], penalty[1]),2))  + #c(0.01, penalty[25], penalty[20], penalty[15], penalty[10], penalty[5], penalty[1]) #c(0.000000000001, 0.01, 0.06, 0.1, 1, 5, 30, 100, 300, 700) breaks = c(0.000000000001, 0.01, 0.06, 0.1, 1, 5, 30, 100,600)
      scale_x_log10()  +  
      #geom_line(aes(penalty,y = training500, colour="500"), data = df, size = 1.5) +
      geom_line(aes(penalty,y = training100, colour="Large"), data = df, linetype = "twodash", size = 1) + 
      #geom_line(aes(penalty,y = training50, colour="50"), data = df, size = 1.5) +
      #geom_line(aes(penalty,y = training30, colour="30"), data = df, size = 1.5) + 
      #geom_line(aes(penalty,y = training20, colour="20"), data = df, size = 1.5) +
      geom_line(aes(penalty,y = training10, colour="Small"), data = df, linetype = "longdash", size = 1) + 
  
      geom_point(aes(x= penalty, y = training10, colour="Small"),shape = 22, size = 4,fill = "white") +
      geom_point(aes(x= penalty, y = training100, colour="Large"),shape = 21, size = 4,fill = "white") +  
      
  
      geom_errorbar(limits10, width=0.1) +
      #geom_errorbar(limits20, width=0.1) + 
      #geom_errorbar(limits30, width=0.1) +
      #eom_errorbar(limits50, width=0.1) +
      geom_errorbar(limits100, width=0.1) +
      #geom_errorbar(limits500, width=0.1) +   
      ggtitle("Banknotes") +
      labs(x =NULL, y = NULL) +
      scale_colour_manual(name="Training Sample Size", values = cols) + 
      theme_bw() +
      theme(axis.title = element_text(size=15), plot.title = element_text(size =17, face="bold"), 
      legend.title= element_text(size=14, face="bold"),
      legend.position=c(.5,.2), legend.text=element_text(size=12)) + 
      theme(panel.background = element_rect(colour = "black"), axis.text= element_text(size=12)) 
      
    
        x11(width=20, height=15) 
        p1  
    
        pdf(file='Panel_Banknotes.pdf', width=8, height=4)  # same size as panels before
          
        p1
        dev.off()



#   geom_point(aes(penalty,y = training500, colour="500"),  shape = 22, size = 4,fill = "white") +
#   geom_point(aes(penalty,y = training100, colour="100"),shape = 22, size = 4,fill = "white") + 
#   geom_point(aes(penalty,y = training50, colour="50"), shape = 22, size = 4,fill = "white") +
#   geom_point(aes(penalty,y = training30, colour="30"), shape = 22, size = 4,fill = "white") + 
#   geom_point(aes(penalty,y = training20, colour="20"), shape = 22, size = 4,fill = "white") +
#   geom_point(aes(penalty,y = training10, colour="10"), shape = 22, size = 4,fill = "white") +    # shape 24
#labs(x =NULL, y = NULL) +


