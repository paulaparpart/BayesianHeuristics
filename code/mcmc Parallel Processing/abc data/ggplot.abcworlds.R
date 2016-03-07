#### --------------  GGPlot of real world UCI data sets ------------------------------------------------------------------
    
    rm(list=ls())  
        
    #####################
    library(ggplot2)
    library(gridExtra)


#-------------------------------- ABC COR Modeling plots -------------------------------------------------------

    load("COR_ozone.RData") 
    
#- ---- including guessing -------------------------------
    load("COR_ozone_guess.RData") 
    load("COR_mortality_guess.RData") 
    
#------- not includign guessing --------------------------------------
# gives the results we want: more raw
    load("COR_ozone_noguessing.RData")
    load("COR_house.world_noguessing.RData")

    load("COR_ozone_cutoff_noguessing.RData") # gets perfect cross-over but no peak in the middle
    load("COR_mortality_noguessing.RData") # case for showig that OLS > Heuristic, but still incorrect end points
    


##------- -TWO FINAL DATA SETS --------------------------------------------------------------------

    #perf <- read.csv("COR_mortality_cutoff_noguessing_integrated.csv", header = TRUE) # no space allowed
    
    load("COR_mortality_exactBayesian.RData")
    perf <- results

    penalty <- perf$penalty[perf$training_N==5]
    penalty[penalty == 0] <- 0.000001
    perf$TTB_dec <- perf$TTB_dec * 100 # percentages 
    perf$se_acc_TTB <- perf$se_acc_TTB * 100 # percentages 
    perf$se_ttb_heuristic <- perf$se_ttb_heuristic * 100

# ttb_dec performance
    training05 <- perf$TTB_dec[perf$training_N==5] 
    training10 <- perf$TTB_dec[perf$training_N==10] 
    training12 <- perf$TTB_dec[perf$training_N==12]
    training15 <- perf$TTB_dec[perf$training_N==15]
    training19 <- perf$TTB_dec[perf$training_N==19]
    training20 <- perf$TTB_dec[perf$training_N==20]
    training50 <- perf$TTB_dec[perf$training_N==50]
    training115 <- perf$TTB_dec[perf$training_N==115]

    # ttb_dec se training05 <- perf$TTB_dec[perf$training_N==5] 
    se_05 <- perf$se_acc_TTB[perf$training_N==5] 
    se_10 <- perf$se_acc_TTB[perf$training_N==10] 
    se_12 <- perf$se_acc_TTB[perf$training_N==12]
    se_15 <- perf$se_acc_TTB[perf$training_N==15]
    se_19 <- perf$se_acc_TTB[perf$training_N==19]
    se_20 <- perf$se_acc_TTB[perf$training_N==20]
    se_50 <- perf$se_acc_TTB[perf$training_N==50]
    se_115 <- perf$se_acc_TTB[perf$training_N==115]



## add in the performance of the heuristics for the penalty parameter at beginning = inf. 
    heuristic05 <- c(perf$TTB_Heuristic[perf$training_N==5][1]*100, training05)
    heuristic12 <- c(perf$TTB_Heuristic[perf$training_N==12][1]*100, training12)
    heuristic20 <- c(perf$TTB_Heuristic[perf$training_N==20][1]*100, training20)
    heuristic115 <- c(perf$TTB_Heuristic[perf$training_N==115][1]*100, training115)
    heuristic_se_05 <- c(perf$se_ttb_heuristic[perf$training_N==5][1], se_05)
    heuristic_se_12 <-  c(perf$se_ttb_heuristic[perf$training_N==12][1], se_12)
    heuristic_se_20 <-  c(perf$se_ttb_heuristic[perf$training_N==20][1], se_20)
    heuristic_se_115 <-  c(perf$se_ttb_heuristic[perf$training_N==115][1], se_115)

    limits05 <- aes(ymax = heuristic05 + heuristic_se_05 , ymin= heuristic05 -  heuristic_se_05) # colour = "5%"
    limits12 <- aes(ymax = heuristic12 + heuristic_se_12, ymin= heuristic12 - heuristic_se_12)
    limits20 <- aes(ymax = heuristic20 + heuristic_se_20, ymin= heuristic20 - heuristic_se_20)
    limits115 <- aes(ymax = heuristic115 + heuristic_se_115, ymin= heuristic115 - heuristic_se_115)

    penalty <- c(100000000,penalty) # cannot plot Inf. so need to write it over afterwards somehow
    
    df <- data.frame(penalty, heuristic05, heuristic12, heuristic20, heuristic115)


    # new df with necessary variables 
    #df <- data.frame(penalty, training05, training12, training20, training115)
    
    
    #options(scipen=999)
    
    # Define the top and bottom of the errorbars
#     limits05 <- aes(ymax = training05 + se_05, ymin= training05 - se_05) # colour = "5%"
#     limits10 <- aes(ymax = training10 + se_10, ymin= training10 - se_10)
#     limits12 <- aes(ymax = training12 + se_12, ymin= training12 - se_12)
#     limits15 <- aes(ymax = training15 + se_15, ymin= training15 - se_15)
#     limits19 <- aes(ymax = training19 + se_19, ymin= training19 - se_19)
#     limits20 <- aes(ymax = training20 + se_20, ymin= training20 - se_20)
#     limits50 <- aes(ymax = training50 + se_50, ymin= training50 - se_50)
#     limits115 <- aes(ymax = training115 + se_115, ymin= training115- se_115)
    
    #cols <- c("115"="dimgrey","50"="darkblue","20"="darkgoldenrod1","19"="cyan4", "15" = "yellow", "12" = "red", "10" = "green", "5" = "deepskyblue4") 
   
    cols <- c("115"="dimgrey", "20"="darkgoldenrod1", "12"="red", "5" = "deepskyblue4")

    #cols <- c("5%" = "deepskyblue4") 
  

    p1 <- ggplot(df, aes(x=penalty, y = heuristic05, ymin = 50, ymax = 85)) +  
      #scale_x_log10(breaks = round(c(0.01, penalty[25], penalty[20], penalty[15], penalty[10], penalty[5], penalty[1]),2))  + #c(0.01, penalty[25], penalty[20], penalty[15], penalty[10], penalty[5], penalty[1]) #c(0.000000000001, 0.01, 0.06, 0.1, 1, 5, 30, 100, 300, 700) breaks = c(0.000000000001, 0.01, 0.06, 0.1, 1, 5, 30, 100,600)
      scale_x_log10(breaks = c(1e-04,1e-01,1e+02,1e+06, 1e+08), labels=c("1e-04","1e-01","1e+02","1e+06", "Inf"))  +  
    
      
      geom_line(aes(penalty,y = heuristic05, colour="5"), data = df, linetype = "longdash", size = 1.5) +
      geom_line(aes(penalty,y = heuristic12, colour="12"), data = df, linetype = "longdash", size = 1.5) +
      geom_line(aes(penalty,y = heuristic20, colour="20"), data = df, linetype = "longdash", size = 1.5) +
      geom_line(aes(penalty,y = heuristic115, colour="115"), data = df, linetype = "longdash", size = 1.5) +
         
      geom_point(aes(penalty,y = heuristic05, colour="5"),shape = 22, size = 4,fill = "white") + 
      geom_point(aes(penalty,y = heuristic12, colour="12"),shape = 22, size = 4,fill = "white") + 
      geom_point(aes(penalty,y = heuristic20, colour="20"),shape = 22, size = 4,fill = "white") + 
      geom_point(aes(penalty,y = heuristic115, colour="115"),shape = 22, size = 4,fill = "white") + 
    
      geom_errorbar(limits05, width=0.1) +
      geom_errorbar(limits12, width=0.1) + 
      geom_errorbar(limits20, width=0.1) + 
      geom_errorbar(limits115, width=0.1) +
      
#       geom_line(aes(penalty,y = training05, colour="5"), data = df, linetype = "longdash", size = 1.5) +
#       geom_line(aes(penalty,y = training10, colour="10"), data = df, linetype = "longdash", size = 1.5) +
#       geom_line(aes(penalty,y = training12, colour="12"), data = df, linetype = "longdash", size = 1.5) +
#       geom_line(aes(penalty,y = training15, colour="15"), data = df, linetype = "longdash", size = 1.5) +
#       geom_line(aes(penalty,y = training19, colour="19"), data = df, linetype = "longdash", size = 1.5) +
#       geom_line(aes(penalty,y = training20, colour="20"), data = df, linetype = "longdash", size = 1.5) +
#       geom_line(aes(penalty,y = training50, colour="50"), data = df, linetype = "longdash", size = 1.5) +
#       geom_line(aes(penalty,y = training115, colour="115"), data = df, linetype = "longdash", size = 1.5) +
#       
#       geom_point(aes(penalty,y = training05, colour="5"),shape = 22, size = 4,fill = "white") + 
#       geom_point(aes(penalty,y = training10, colour="10"),shape = 22, size = 4,fill = "white") + 
#       geom_point(aes(penalty,y = training12, colour="12"),shape = 22, size = 4,fill = "white") + 
#       geom_point(aes(penalty,y = training15, colour="15"),shape = 22, size = 4,fill = "white") + 
#       geom_point(aes(penalty,y = training19, colour="19"),shape = 22, size = 4,fill = "white") + 
#       geom_point(aes(penalty,y = training20, colour="20"),shape = 22, size = 4,fill = "white") + 
#       geom_point(aes(penalty,y = training50, colour="50"),shape = 22, size = 4,fill = "white") + 
#       geom_point(aes(penalty,y = training115, colour="115"),shape = 22, size = 4,fill = "white") + 
# 
#       geom_errorbar(limits05, width=0.2) +
#       geom_errorbar(limits10, width=0.1) + 
#       geom_errorbar(limits12, width=0.1) + 
#       geom_errorbar(limits15, width=0.1) +
#       geom_errorbar(limits19, width=0.1) + 
#       geom_errorbar(limits20, width=0.1) + 
#       geom_errorbar(limits50, width=0.1) + 
#       geom_errorbar(limits115, width=0.1) + 
          
      ggtitle("Mortality") +
      labs(x =NULL, y = NULL) +
      scale_colour_manual(name="Training Sample Size", values = cols) + 
      theme_bw() +
      theme(axis.title = element_text(size=16), plot.title = element_text(size =20, face="bold"), 
            legend.title= element_text(size=12, face="bold"),
            legend.position="top", legend.text=element_text(size=10)) + 
      theme(panel.background = element_rect(colour = "black"), axis.text= element_text(size=17, face="bold")) 
    
    
    x11(width=20, height=15) 
    
    p1  
    
    pdf(file='Panel_Mortality_exactBayesain.pdf', width=5, height=5)
    
    p1
    dev.off()



      #----------------------------------- House -----------------------------------------------------------------


      perf <- read.csv("COR_house.world_cutoff_noguessing_integrated.csv", header = TRUE) # no space allowed
      
      
      penalty <- perf$penalty[perf$training_N==5]
      penalty[penalty == 0] <- 0.000001
      perf$TTB_dec <- perf$TTB_dec * 100 # percentages 
      perf$se_acc_TTB <- perf$se_acc_TTB * 100 # percentages 
      perf$se_ttb_heuristic <- perf$se_ttb_heuristic * 100

      # ttb_dec performance
      training05 <- perf$TTB_dec[perf$training_N==5] 
      training10 <- perf$TTB_dec[perf$training_N==10] 
      training12 <- perf$TTB_dec[perf$training_N==12]
      training15 <- perf$TTB_dec[perf$training_N==15]
      training20 <- perf$TTB_dec[perf$training_N==20]
      training23 <- perf$TTB_dec[perf$training_N==23]
      training50 <- perf$TTB_dec[perf$training_N==50]
      training115 <- perf$TTB_dec[perf$training_N==115]
      training200 <- perf$TTB_dec[perf$training_N==200]

  
      # ttb_dec se training05 <- perf$TTB_dec[perf$training_N==5] 
      se_05 <- perf$se_acc_TTB[perf$training_N==5] 
      se_10 <- perf$se_acc_TTB[perf$training_N==10] 
      se_12 <- perf$se_acc_TTB[perf$training_N==12]
      se_15 <- perf$se_acc_TTB[perf$training_N==15]
      se_20 <- perf$se_acc_TTB[perf$training_N==20]
      se_23 <- perf$se_acc_TTB[perf$training_N==23]
      se_50 <- perf$se_acc_TTB[perf$training_N==50]
      se_115 <- perf$se_acc_TTB[perf$training_N==115]
      se_200 <- perf$se_acc_TTB[perf$training_N==200]
# 
      
      # new df with necessary variables 
      #df <- data.frame(penalty, training05, training10, training12, training15, training20, training23, training50, training115, training200)
       
      #options(scipen=999)    
      # Define the top and bottom of the errorbars
#       limits05 <- aes(ymax = training05 + se_05, ymin= training05 - se_05) # colour = "5%"
#       limits10 <- aes(ymax = training10 + se_10, ymin= training10 - se_10)
#       limits12 <- aes(ymax = training12 + se_12, ymin= training12 - se_12)
#       limits15 <- aes(ymax = training15 + se_15, ymin= training15 - se_15)
#       limits20 <- aes(ymax = training20 + se_20, ymin= training20 - se_20)
#       limits23 <- aes(ymax = training23 + se_23, ymin= training23 - se_23)
#       limits50 <- aes(ymax = training50 + se_50, ymin= training50 - se_50)
#       limits115 <- aes(ymax = training115 + se_115, ymin= training115- se_115)
#       limits200 <- aes(ymax = training200 + se_200, ymin= training200- se_200)

      ## add in the performance of the heuristics for the penalty parameter at beginning = inf. 
      heuristic05 <- c(perf$TTB_Heuristic[perf$training_N==5][1]*100, training05)
      heuristic12 <- c(perf$TTB_Heuristic[perf$training_N==12][1]*100, training12)
      heuristic20 <- c(perf$TTB_Heuristic[perf$training_N==20][1]*100, training20)
      heuristic115 <- c(perf$TTB_Heuristic[perf$training_N==115][1]*100, training115)
      heuristic_se_05 <- c(perf$se_ttb_heuristic[perf$training_N==5][1], se_05)
      heuristic_se_12 <-  c(perf$se_ttb_heuristic[perf$training_N==12][1], se_12)
      heuristic_se_20 <-  c(perf$se_ttb_heuristic[perf$training_N==20][1], se_20)
      heuristic_se_115 <-  c(perf$se_ttb_heuristic[perf$training_N==115][1], se_115)
      
      limits05 <- aes(ymax = heuristic05 + heuristic_se_05 , ymin= heuristic05 -  heuristic_se_05) # colour = "5%"
      limits12 <- aes(ymax = heuristic12 + heuristic_se_12, ymin= heuristic12 - heuristic_se_12)
      limits20 <- aes(ymax = heuristic20 + heuristic_se_20, ymin= heuristic20 - heuristic_se_20)
      limits115 <- aes(ymax = heuristic115 + heuristic_se_115, ymin= heuristic115 - heuristic_se_115)
      
      penalty <- c(100000000,penalty) # cannot plot Inf. so need to write it over afterwards somehow
      
      df <- data.frame(penalty, heuristic05, heuristic12, heuristic20, heuristic115)



     # cols <- c("200"="dimgrey", "115"="dimgrey","50"="darkblue","23"="darkgoldenrod1","20"="cyan4", "15" = "yellow", "12" = "red", "10" = "green", "5" = "deepskyblue4") 
      #cols <- c("50%"="darkgoldenrod1","30%"="cyan4", "10%" = "yellow", "5%" = "green") 
      #cols <- c("90%"="dimgrey", "50%"="darkgoldenrod1", "5%" = "green")
      #cols <- c("5%" = "deepskyblue4") 

      cols <- c("115"="dimgrey", "20"="darkgoldenrod1", "12"="red", "5" = "deepskyblue4")

      
      p1 <- ggplot(df, aes(x=penalty, y = heuristic05, ymin = 50, ymax = 100)) +  
        #scale_x_log10(breaks = round(c(0.01, penalty[25], penalty[20], penalty[15], penalty[10], penalty[5], penalty[1]),2))  + #c(0.01, penalty[25], penalty[20], penalty[15], penalty[10], penalty[5], penalty[1]) #c(0.000000000001, 0.01, 0.06, 0.1, 1, 5, 30, 100, 300, 700) breaks = c(0.000000000001, 0.01, 0.06, 0.1, 1, 5, 30, 100,600)
        scale_x_log10(breaks = c(1e-04,1e-01,1e+02,1e+08), labels=c("1e-04","1e-01","1e+02","Inf"))  +  
  
        geom_line(aes(penalty,y = heuristic05, colour="5"), data = df, linetype = "longdash", size = 1.5) +
        geom_line(aes(penalty,y = heuristic12, colour="12"), data = df, linetype = "longdash", size = 1.5) +
        geom_line(aes(penalty,y = heuristic20, colour="20"), data = df, linetype = "longdash", size = 1.5) +
        geom_line(aes(penalty,y = heuristic115, colour="115"), data = df, linetype = "longdash", size = 1.5) +
        
        geom_point(aes(penalty,y = heuristic05, colour="5"),shape = 22, size = 4,fill = "white") + 
        geom_point(aes(penalty,y = heuristic12, colour="12"),shape = 22, size = 4,fill = "white") + 
        geom_point(aes(penalty,y = heuristic20, colour="20"),shape = 22, size = 4,fill = "white") + 
        geom_point(aes(penalty,y = heuristic115, colour="115"),shape = 22, size = 4,fill = "white") + 
        
        geom_errorbar(limits05, width=0.1) +
        geom_errorbar(limits12, width=0.1) + 
        geom_errorbar(limits20, width=0.1) + 
        geom_errorbar(limits115, width=0.1) +
        

#         geom_line(aes(penalty,y = training05, colour="5"), data = df, linetype = "longdash", size = 1.5) +
#         geom_line(aes(penalty,y = training10, colour="10"), data = df, linetype = "longdash", size = 1.5) +
#         geom_line(aes(penalty,y = training12, colour="12"), data = df, linetype = "longdash", size = 1.5) +
#         geom_line(aes(penalty,y = training15, colour="15"), data = df, linetype = "longdash", size = 1.5) +
#         geom_line(aes(penalty,y = training20, colour="20"), data = df, linetype = "longdash", size = 1.5) +
#         geom_line(aes(penalty,y = training23, colour="23"), data = df, linetype = "longdash", size = 1.5) +
#         geom_line(aes(penalty,y = training50, colour="50"), data = df, linetype = "longdash", size = 1.5) +
#         geom_line(aes(penalty,y = training115, colour="115"), data = df, linetype = "longdash", size = 1.5) +
#         geom_line(aes(penalty,y = training200, colour="200"), data = df, linetype = "longdash", size = 1.5) +
#   
#         geom_point(aes(penalty,y = training05, colour="5"),shape = 22, size = 4,fill = "white") + 
#         geom_point(aes(penalty,y = training10, colour="10"),shape = 22, size = 4,fill = "white") + 
#         geom_point(aes(penalty,y = training12, colour="12"),shape = 22, size = 4,fill = "white") + 
#         geom_point(aes(penalty,y = training15, colour="15"),shape = 22, size = 4,fill = "white") + 
#         geom_point(aes(penalty,y = training20, colour="20"),shape = 22, size = 4,fill = "white") + 
#         geom_point(aes(penalty,y = training23, colour="23"),shape = 22, size = 4,fill = "white") + 
#         geom_point(aes(penalty,y = training50, colour="50"),shape = 22, size = 4,fill = "white") + 
#         geom_point(aes(penalty,y = training115, colour="50"),shape = 22, size = 4,fill = "white") + 
#         geom_point(aes(penalty,y = training200, colour="200"),shape = 22, size = 4,fill = "white") + 
#         
#         geom_errorbar(limits05, width=0.2) +
#         geom_errorbar(limits10, width=0.1) + 
#         geom_errorbar(limits12, width=0.1) + 
#         geom_errorbar(limits15, width=0.1) +
#         geom_errorbar(limits20, width=0.1) + 
#         geom_errorbar(limits23, width=0.1) + 
#         geom_errorbar(limits50, width=0.1) + 
#         geom_errorbar(limits115, width=0.1) + 
#         geom_errorbar(limits200, width=0.1) + 
        
        ggtitle("House") +
        labs(x =NULL, y = NULL) +
        scale_colour_manual(name="Training Sample Size", values = cols) + 
        theme_bw() +
        theme(axis.title = element_text(size=16), plot.title = element_text(size =20, face="bold"), 
        legend.title= element_text(size=12, face="bold"),
        legend.position="top", legend.text=element_text(size=10), legend.key.size = unit(0.04, "cm")) + 
        theme(panel.background = element_rect(colour = "black"), axis.text= element_text(size=17, face="bold")) 

      
      x11(width=20, height=15) 
      
      p1  
      
      pdf(file='Panel_House_all2.pdf', width=5, height=5)
      
      p1
      dev.off()
      



      ##------- -THE OLD VERSION for PNAS ---------------------------------------------------------------------
      load("COR_mortality_cutoff.RData") # better end-points but different dataset: 5%
      
      perf <- results
      penalty <- perf$penalty[perf$training_N==0.05]
      penalty[penalty == 0] <- 0.000001
      perf$TTB_dec <- perf$TTB_dec * 100 # percentages 
      
      perf$se_acc_TTB <- perf$se_acc_TTB * 100 # percentages 
      
      
      # ttb_dec performance
      training05 <- perf$TTB_dec[perf$training_N==0.05] 
      training10 <- perf$TTB_dec[perf$training_N==0.1] 
      training30 <- perf$TTB_dec[perf$training_N==0.3]
      training50 <- perf$TTB_dec[perf$training_N==0.5]
      training70 <- perf$TTB_dec[perf$training_N==0.7]
      training90 <- perf$TTB_dec[perf$training_N==0.9]
      
      #     training05 <- perf$Class_dec[perf$training_N==0.05] 
      #     training10 <- perf$Class_dec[perf$training_N==0.1] 
      #     training30 <- perf$Class_dec[perf$training_N==0.3]
      #     training50 <- perf$Class_dec[perf$training_N==0.5]
      #     training70 <- perf$Class_dec[perf$training_N==0.7]
      #     training90 <- perf$Class_dec[perf$training_N==0.9]
      
      # ttb_dec se
      se_05 <- perf$se_acc_TTB[perf$training_N==0.05] 
      se_10 <- perf$se_acc_TTB[perf$training_N==0.1] 
      se_30 <- perf$se_acc_TTB[perf$training_N==0.3]
      se_50 <- perf$se_acc_TTB[perf$training_N==0.5]
      se_70 <- perf$se_acc_TTB[perf$training_N==0.7]
      se_90 <- perf$se_acc_TTB[perf$training_N==0.9]
      
      # class_dec   
      #     se_05 <- perf$se_acc_class[perf$training_N==0.05] 
      #     se_10 <- perf$se_acc_class[perf$training_N==0.1] 
      #     se_30 <- perf$se_acc_class[perf$training_N==0.3]
      #     se_50 <- perf$se_acc_class[perf$training_N==0.5]
      #     se_70 <- perf$se_acc_class[perf$training_N==0.7]
      #     se_90 <- perf$se_acc_class[perf$training_N==0.9]
      
      # new df with necessary variables 
      #df <- data.frame(penalty, training05, training10, training30,  training50)
      df <- data.frame(penalty, training05)
      
      #options(scipen=999)
      
      # Define the top and bottom of the errorbars
      limits05 <- aes(ymax = training05 + se_05, ymin= training05 - se_05) # colour = "5%"
      limits10 <- aes(ymax = training10 + se_10, ymin= training10 - se_10)
      limits30 <- aes(ymax = training30 + se_30, ymin= training30 - se_30)
      limits50 <- aes(ymax = training50 + se_50, ymin= training50 - se_50)
      limits70 <- aes(ymax = training70 + se_70, ymin= training70 - se_70)
      limits90 <- aes(ymax = training90 + se_90, ymin= training90 - se_90)
      
      #cols <- c("500"="dimgrey","100"="darkblue","50"="darkgoldenrod1","30"="cyan4", "20" = "yellow", "10" = "green") 
      #cols <- c("50%"="darkgoldenrod1","30%"="cyan4", "10%" = "yellow", "5%" = "green") 
      #cols <- c("90%"="dimgrey", "50%"="darkgoldenrod1", "5%" = "green")
      
      cols <- c("5%" = "deepskyblue4") 
      #cols <- c("10%" = "green4") # grey70
      #cols <- c("5%" = "dimgrey")
      
      p1 <- ggplot(df, aes(x=penalty, y = training05, ymin = 55, ymax = 85)) +  
        #scale_x_log10(breaks = round(c(0.01, penalty[25], penalty[20], penalty[15], penalty[10], penalty[5], penalty[1]),2))  + #c(0.01, penalty[25], penalty[20], penalty[15], penalty[10], penalty[5], penalty[1]) #c(0.000000000001, 0.01, 0.06, 0.1, 1, 5, 30, 100, 300, 700) breaks = c(0.000000000001, 0.01, 0.06, 0.1, 1, 5, 30, 100,600)
        scale_x_log10()  +  
        
        #geom_line(aes(penalty,y = training90, colour="90%"), data = df, size = 1.5) + 
        #geom_line(aes(penalty,y = training70, colour="70%"), data = df, size = 1.5) + 
        #geom_line(aes(penalty,y = training50, colour="50%"), data = df, size = 1.5) +
        #geom_line(aes(penalty,y = training30, colour="30%"), data = df, size = 1.5) +
        geom_line(aes(penalty,y = training05, colour="5%"), data = df, linetype = "longdash", size = 1.5) +
        #geom_point(aes(penalty,y = training10, colour="10%"),shape = 8, size = 2, fill = "white") +
        
        #       geom_line(aes(penalty,y = training05, colour="5%"), data = df, size = 1.2) +    # longdash
        geom_point(aes(penalty,y = training05, colour="5%"),shape = 22, size = 4,fill = "white") +     # shape = 18, size = 2?
        #   
        #geom_errorbar(limits05, width=0.05) + 
        geom_errorbar(limits05, width=0.2) +
        #geom_errorbar(limits30, width=0.1) + 
        #geom_errorbar(limits50, width=0.1) + 
        #geom_errorbar(limits70, width=0.1) +
        #geom_errorbar(limits90, width=0.1) + 
        
        ggtitle("Mortality") +
        labs(x =NULL, y = NULL) +
        scale_colour_manual(name="Training Sample Size", values = cols) + 
        theme_bw() +
        theme(axis.title = element_text(size=16), plot.title = element_text(size =20, face="bold"), 
              legend.title= element_text(size=17, face="bold"),
              legend.position=c(.45,.8), legend.text=element_text(size=17)) + 
        theme(panel.background = element_rect(colour = "black"), axis.text= element_text(size=17, face="bold")) 
      
      
      x11(width=20, height=15) 
      
      p1  
      
      pdf(file='Panel_Mortality_longy.pdf', width=5, height=5)
      
      p1
      dev.off()



#----------------------------------- House -----------------------------------------------------------------
load("COR_house.world_longmcmc_cutoff_noguessing.RData") # looks better than regular house (no guessing): 10%

perf <- results
penalty <- perf$penalty[perf$percent_training==0.05]
penalty[penalty == 0] <- 0.000001
perf$TTB_dec <- perf$TTB_dec * 100 # percentages 

perf$se_acc_TTB <- perf$se_acc_TTB * 100 # percentages 


# ttb_dec performance
training05 <- perf$TTB_dec[perf$percent_training==0.05] 

# ttb_dec se
se_05 <- perf$se_acc_TTB[perf$percent_training==0.05] 

# new df with necessary variables 
#df <- data.frame(penalty, training05, training10, training30,  training50)
df <- data.frame(penalty, training05)

#options(scipen=999)

# Define the top and bottom of the errorbars
limits05 <- aes(ymax = training05 + se_05, ymin= training05 - se_05) # colour = "5%"

cols <- c("5%" = "dimgrey")

p1 <- ggplot(df, aes(x=penalty, y = training05, ymin = 55, ymax = 85)) +  
  scale_x_log10()  +  
  
  #geom_line(aes(penalty,y = training90, colour="90%"), data = df, size = 1.5) + 
  #geom_line(aes(penalty,y = training70, colour="70%"), data = df, size = 1.5) + 
  #geom_line(aes(penalty,y = training50, colour="50%"), data = df, size = 1.5) +
  #geom_line(aes(penalty,y = training30, colour="30%"), data = df, size = 1.5) +
  geom_line(aes(penalty,y = training05, colour="5%"), data = df, linetype = "longdash", size = 2) +
  #geom_point(aes(penalty,y = training10, colour="10%"),shape = 8, size = 2, fill = "white") +
  
  geom_point(aes(penalty,y = training05, colour="5%"),shape = 23, size = 4,fill = "white") +     # shape = 23, size = 2?
  
  geom_errorbar(limits05, width=0.2) +
  
  
  ggtitle("House") +
  labs(x =NULL, y = NULL) +
  scale_colour_manual(name="Training Sample Size", values = cols) + 
  theme_bw() +
  theme(axis.title = element_text(size=16), plot.title = element_text(size =20, face="bold"), 
        legend.title= element_text(size=17, face="bold"),
        legend.position=c(.45,.2), legend.text=element_text(size=17)) + 
  theme(panel.background = element_rect(colour = "black"), axis.text= element_text(size=17, face="bold")) 


x11(width=20, height=15) 

p1  

pdf(file='Panel_House_longy.pdf', width=5, height=5)

p1
dev.off()







