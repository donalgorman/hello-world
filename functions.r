
################################################
#                                              #
#   Operating Characteristic Curve Generator   #
#                [Version 2.0]                 #
#                                              #
################################################

#############
# Functions #
#############

######################
###   Utilities:   ###
######################

computerSaysNo <- function(input="Option currently not supported"){
  return(p(h3(em(div(input,style="color:red")))))
}

determineSignDirection <- function(input){
  if(input){
    output <- ">"
  } else {
    output <- "<"
  }
  return(output)
}

determineEmpty <- function(input){
  out <- FALSE
  if(is.null(input) || is.na(input) || input==""){out <- TRUE}
  return(out)
}

create_decision_criteria_text <- function(input1,input2,input3,input4,input5,input6){
  treatText <- "effect"
  if(input5 %in% c(label_study_2Normal,label_study_1Normal)){
    if(input6==label_study_abs){
      endText <- input3
    } else if(input6==label_study_perc){
      endText <- paste0(input3,"%")
    } else if(input6==label_study_ratio){
      endText <- paste0(input3,"")
      treatText <- "ratio"
    }
  } else if(input5 %in% c(label_study_2Binomial,label_study_1Binomial)){
    endText <- paste0(input3,"%")
  }
  output <- paste0(input1,": At least ",100*input4,"% confident treatment ",treatText," ",determineSignDirection(input2)," ",endText)  
  return(output)
}

create_interim_criteria_text <- function(input1,input2,input3,input4=TRUE){
  success <- paste0("Success: Predictive probability for ",input1," > ",100*input2,"%")
  futile <- paste0("Futility: Predictive probability for ",input1," < ",100*input3,"%")
  if(input4){
    #HTML output:
    output <- HTML(paste0(success,"<br/>",futile))
  } else {
    output <- paste0(success,"; ",futile)
  }
  return(output)
}


#################################
###   Validation of inputs:   ###
#################################

`%then%` <- shiny:::`%OR%`  

validate_generic_function <- function(input1,input2){
  validate(need(input1,paste0("Need to enter a value for ",input2)))
  return(input1)
}

validate_logical_function <- function(input1,input2){
  validate(
    need(is.logical(input1) && !is.na(input1),paste0("Need to enter a value for ",input2))
  )
  return(input1)
}

validate_integer_function <- function(input1,input2){
  validate(
    need(input1,paste0("Need to enter an integer for ",input2) ) %then%
    need(input1>0,paste0(input2," must be greater than zero")) %then%
    need((input1 %% 1)==0,paste0(input2," must be an integer"))
  )
  return(input1)
}

validate_prec_function <- function(input1,input2){
  #Can also use for ratio
  validate(
    need(input1,paste0("Need to enter a numeric value for ",input2)) %then%
    need(input1>0,paste0(input2," must be greater than zero"))
  )
  return(input1)
}

validate_sig_function <- function(input1,input2){
  validate(
    need(input1,paste0("Need to enter a level of confidence for ",input2)) %then%
    need(input1 > 0 & input1 < 100,paste0(input2," confidence is a percentage between 0% and 100%"))
  )
  return(input1)
}

#validate_prob_function <- function(input1,input2){
#  validate(
#    need(input1,paste0("Need to enter a proportion for ",input2)) %then%
#    need(input1 >= 0 & input1 <= 1,paste0(input2," is a probability between 0 and 1"))
#  )
#  return(input1)
#}

validate_numeric_function <- function(input1,input2){
  validate(
    need(input1,paste0("Need to enter a numeric value for ",input2))
  )
  return(input1)
}

validate_numeric_pos_function <- function(input1,input2){
  validate(
    need(input1,paste0("Need to enter a numeric value for ",input2)) %then%
    need(input1 > 0,paste0(input2," must be greater than zero"))
  )
  return(input1)
}

validate_percDiff_function <- function(input1,input2){
  validate(
    need(input1,paste0("Need to enter a percentage difference for ",input2)) %then%
    need(input1 > -100 & input1 < 100,paste0(input2," is a percentage difference between -100% and 100%"))
  )
  return(input1)
}

validate_percDiffAxis_function <- function(input1,input2){
  validate(
    need(input1,paste0("Need to enter a percentage difference for ",input2)) %then%
    need(input1 >= -100 & input1 <= 100,paste0(input2," is a percentage difference between (and including) -100% and 100%"))
  )
  return(input1)
}

validate_perc100_function <- function(input1,input2){
  validate(
    need(input1,paste0("Need to enter a numeric value for ",input2)) %then%
    need(input1 >= 0,paste0(input2," must be greater than or equal to 0%")) %then%
    need(input1 <= 100,paste0(input2," must be less than or equal to 100%"))
  )
  return(input1)
}

validate_percDiffNorm_function <- function(input1,input2){
  validate(
    need(input1,paste0("Need to enter a percentage difference for ",input2)) %then%
    need(input1 > -100,paste0(input2," is a percentage difference greater than -100%"))
  )
  return(input1)
}


######################################
###   Creation of input options:   ###
######################################

widC_MultipleNumericRowEntries <- function(input1,input2,input3,input4=NA){
  outList <- list()
  if(is.null(input3)){
    return(NULL)
  } else {
    for(i in 1:as.numeric(input3)){
      if(is.na(input4)){
        addText <- ""
      } else {
        addText <- paste0(" ",input4)
      }
      outList[[i]] <- column(12/as.numeric(input3),numericInput(paste0(input1,i),paste0(input2," ",i,addText,":"),get(paste0("init_",input1,i))))
    }
    outRow <- fluidRow(outList)
    return(outRow)
  }
}


###############################################################################
###############################################################################
###                                                                         ###
###                      Normally Distributed Outcomes                      ###
###                                                                         ###
###############################################################################
###############################################################################

################################################################
###   Generic functions for normally distributed outcomes:   ###
################################################################

normalMeansDF <- function(input1,input2){
  # Determine DF if not using a normal approximation (otherwise DF = normal_df_default):
  if(input1){
    return(normal_df_default)
  } else {
    return(input2)
  }
}

normalMeansData <- function(se,df_in,nCriteria,direction,c1_tv,c1_sig,c2_tv,c2_sig,xlow,xupp,xgap){
# Create OC data for plotting based on inputs:
  
  if(length(se)>1){validate(need(NULL,"WARNING: There is an issue with normalMeansData - more than 1 SE provided"))}
  c1_tv1 <- c1_tv
  c1_sig1 <- c1_sig
  c2_tv1 <- c2_tv
  c2_sig1 <- c2_sig
  if(direction){
    xlow1 <- xlow
    xupp1 <- xupp
  } else {
    xlow1 <- -xupp
    xupp1 <- -xlow
    c1_tv1 <- -c1_tv
    c1_sig1 <- c1_sig
    if(nCriteria==2){
      c2_tv1 <- -c2_tv
      c2_sig1 <- c2_sig
    }
  }
  delta <- seq(xlow1,xupp1,length.out=xgap)
  if(nCriteria==1){
    probs <- normalMeansFunction1dec(delta,c1_tv1,c1_sig1,se,df_in)
    allResults <- data.frame(delta=delta,GO=probs[[1]],STOP=probs[[2]],stringsAsFactors=F)
    allResults <- formatPlotData1criteria(allResults)
  } else {
    ### Determine whether C2 or C1 curve is needed for 'GO':
    curveC1 <- determine_minimum_prec_sub("Normal",se,df_in,TRUE,c1_tv1,c1_sig1,c2_tv1,c2_sig1,precT=FALSE)
    ### Determine probabilities for curves:
    probs <- normalMeansFunction2dec(delta,c1_tv1,c1_sig1,c2_tv1,c2_sig1,se,df_in,curveC1)
    allResults <- data.frame(delta=delta,GO=probs[[1]],STOP=probs[[3]],PAUSE=probs[[2]],stringsAsFactors=F)
    allResults <- formatPlotData2criteria(allResults)
  }
  if(!direction){
    allResults$delta <- -as.numeric(allResults$delta)
  }
  allResults <- cbind(SE=se,allResults)
  return(allResults)
}

normalMeansFunction2dec <- function(delta,c1_tv,c1_sig,c2_tv,c2_sig,se,df_in,curveC1=FALSE){
  if(curveC1){ #C1 curve is go curve:
    probGo <- normalMeansPowerCurve(delta,c1_tv,c1_sig,se,df_in,TRUE)
  } else { #C2 curve is go curve:
    probGo <- normalMeansPowerCurve(delta,c2_tv,c2_sig,se,df_in,TRUE)
  }
  probStop <- normalMeansPowerCurve(delta,c1_tv,c1_sig,se,df_in,FALSE)
  probDisc <- 1 - probStop - probGo
  return(list(probGo,probDisc,probStop))
}

normalMeansFunction1dec <- function(delta,c1_tv,c1_sig,se,df_in,lowerT=TRUE){
  probGo <- normalMeansPowerCurve(delta,c1_tv,c1_sig,se,df_in,lowerT)
  probStop <- 1-probGo
  return(list(probGo,probStop))
}

normalMeansPowerCurve <- function(delta,tv,sig,se,df_in,lowerT=TRUE){
  return(pt(qt(1-sig,df=df_in),df=df_in,ncp=(tv-delta)/se,lower.tail=lowerT))
}

normalMeansPowerPoint <- function(tv,sig,direction,se,df_in,prob){
  if(direction){
    tv1 <- tv
  } else {
    tv1 <- -tv
  }
  output <- tv1-se*(qnorm(1-prob,lower.tail=TRUE)+qt(1-sig,df=df_in))    
  if(!direction){output <- -output}
  return(output)
}


##############################################
###   Difference between 2 normal means:   ###
##############################################

normal2MeansSE <- function(precision,n1,n2,sigma,se){
  # Determine SE based on inputs selected:
  print(n1)
  print(n2)
  print(sigma)
  print(se)
  outSE <- NULL
  if(precision==2){
    # Use SE:
    outSE <- se
  } else if(precision==1){
    outSE <- sqrt(sigma*sigma/n1 + sigma*sigma/n2)
  }
  print(outSE)
  return(outSE)
}


########################################
###   Normal mean vs. fixed value:   ###
########################################

normal1MeansSE <- function(precision,n1,sigma,se){
  # Determine SE based on inputs selected:
  outSE <- NULL
  if(precision==2){
    # Use SE:
    outSE <- se
  } else if(precision==1){
    outSE <- sqrt(sigma*sigma/n1)
  }
  return(outSE)
}



###############################################################################
###############################################################################
###                                                                         ###
###                      Binomial Distributed Outcomes                      ###
###                                                                         ###
###############################################################################
###############################################################################

################################################################
###   Generic functions for binomial distributed outcomes:   ###
################################################################


binomialMeansPrec <- function(studyDesign,precision,equalN,n1,n2,probRef){
  # Create summary of precision inputs:
  outVec <- NULL
  if(precision==3){
    if(studyDesign==label_study_Parallel){
      if(equalN){
        outVec <- c(n1,n1,probRef)
      } else {
        outVec <- c(n1,n2,probRef)
      }
    } else if(studyDesign==label_study_Single){
      outVec <- c(n1,NA,probRef)
    } else if(studyDesign==label_study_CrossOver){
      outVec <- c(n1,n1,probRef)
    }
  }
  return(outVec)
}

binomialMeansData <- function(precTable,df_in,nCriteria,direction,c1_tv,c1_sig,c2_tv,c2_sig,xlow,xupp,xgap){
  
  if(nrow(precTable)!=1){validate(need(NULL,"WARNING: There is an issue with binomialMeansData - precision table is not 1 row"))}
  # Create data for binomial OC curves:
  c1_tv1 <- c1_tv
  c1_sig1 <- c1_sig
  c2_tv1 <- c2_tv
  c2_sig1 <- c2_sig
  
  if(direction){
    xlow1 <- xlow
    xupp1 <- xupp
  } else {
    xlow1 <- -xupp
    xupp1 <- -xlow
    c1_tv1 <- -c1_tv
    c1_sig1 <- c1_sig
    if(nCriteria==2){
      c2_tv1 <- -c2_tv
      c2_sig1 <- c2_sig
    }
  }
  delta <- seq(xlow1,xupp1,length.out=xgap)
  allResults <- data.frame(delta=as.numeric(),GO=as.numeric(),STOP=as.numeric(),stringsAsFactors=F)
  for(dI in 1:length(delta)){
    if(direction){
      probTrt <- precTable$ProbRef + delta[dI]
    } else {
      probTrt <- precTable$ProbRef - delta[dI]
    }
    if(precTable$Outcomes==2){
      ### Comparison between 2 outcomes:
      seI <- sqrt(((probTrt*(1-probTrt))/precTable$N1) + ((precTable$ProbRef*(1-precTable$ProbRef))/precTable$N2))
    } else if(precTable$Outcomes==1){
      ### Comparision between 1 outcome and a fixed value:
      seI <- sqrt(((probTrt*(1-probTrt))/precTable$N1))
    }
    if(nCriteria==1){
      probs <- normalMeansFunction1dec(delta[dI],c1_tv1,c1_sig1,seI,df_in)
      allResults <- rbind(allResults,data.frame(delta=delta[dI],GO=probs[[1]],STOP=probs[[2]],stringsAsFactors=F))
    } else {
      probs <- normalMeansFunction2dec(delta[dI],c1_tv1,c1_sig1,c2_tv1,c2_sig1,seI,df_in)
      allResults <- rbind(allResults,data.frame(delta=delta[dI],GO=probs[[1]],STOP=probs[[3]],PAUSE=probs[[2]],stringsAsFactors=F))
    }
  }
  if(nCriteria==1){
    allResults <- formatPlotData1criteria(allResults)
  } else {
    allResults <- formatPlotData2criteria(allResults)
  }
  if(!direction){
    allResults$delta <- -as.numeric(allResults$delta)
  }
  allResults <- cbind(N1=precTable$N1,N2=precTable$N2,ProbRef=precTable$ProbRef,allResults)
  return(allResults)
}

binomialMeansPowerPoint_All <- function(probValue,precTable,df_in,direction,c1_tv,c1_sig,gapSize,stepN){
  deltas <- NULL
  for(i in 1:nrow(precTable)){
    deltas <- c(deltas,binomialMeansPowerPoint(probValue,precTable[i,],df_in,direction,c1_tv,c1_sig,gapSize,stepN))
  }
  return(deltas)
}

binomialMeansPowerPoint <- function(probValue,precTable,df_in,direction,c1_tv,c1_sig,gapSize,stepN,lowStart=-1,uppStart=1){
  #Initiate algorithm:
  stepI <- binomialMeansPowerPoint_Sub(probValue,precTable,df_in,direction,c1_tv,c1_sig,lowStart,uppStart,gapSize)
  lowOld <- lowStart
  uppOld <- uppStart
  
  if(stepN > 1){
    for(i in 1:(stepN-1)){
      if(length(stepI) > 0 & !is.na(stepI)){
        if(direction){
          uppI <- stepI
          lowI <- stepI-(uppOld-lowOld)/(gapSize-1)
        } else {
          uppI <- stepI+(uppOld-lowOld)/(gapSize-1)
          lowI <- stepI
        }
        stepI <- binomialMeansPowerPoint_Sub(probValue,precTable,df_in,direction,c1_tv,c1_sig,lowI,uppI,gapSize)
        lowOld <- lowI
        uppOld <- uppI
      }
    }
  }
  if(is.na(stepI)){
    stepI <- NULL
  }
  return(stepI)
}

binomialMeansPowerPoint_Sub <- function(probValue,precTable,df_in,direction,c1_tv,c1_sig,lowStart,uppStart,gapSize){
  stepX <- binomialMeansData(precTable,df_in,1,direction,c1_tv,c1_sig,NULL,NULL,lowStart,uppStart,gapSize)
  stepX <- stepX[stepX$variable==label_decision_1_one,] #1 DC
  stepX <- stepX[!is.na(stepX$value),]
  stepX <- stepX[stepX$value>probValue,]
  stepX <- stepX[order(stepX$value),]
  stepX <- stepX[1,"delta"]
  return(stepX)
}


######################################################
###   Difference between 2 binomial proportions:   ###
######################################################

binomial2MeansData_Sim <- function(n1,n2,probRef,direction,c1_tv,c1_sig,xlow,xupp,xgap,nSim=5000,test=1){
# Create data through simulation for binomial OC curves:
# Testing method: 1 [Normal]
  delta <- seq(xlow,xupp,length.out=xgap)  
  allResults <- data.frame(delta=as.numeric(),GO=as.numeric(),STOP=as.numeric(),stringsAsFactors=F)
  for(dI in 1:length(delta)){
    #For each delta:
    probTrt <- probRef + delta[dI]
    sim1 <- rbinom(nSim,n1,probTrt)
    sim2 <- rbinom(nSim,n2,probRef)
    prob1 <- sim1/n1
    prob2 <- sim2/n2
    simSE <- sqrt(((prob1*(1-prob1))/n1) + ((prob2*(1-prob2))/n2))
    
    ### TESTING: UP TO HERE!
    simVec <- NULL
    for(sJ in 1:nSim){
      #For each simulation
      yIJ <- seIJ*rnorm(1)+delta[dI]
      z_GO <- (yIJ-c1_tv)/seIJ
      if(direction){
        lower <- TRUE
      } else{
        lower <- FALSE
      }
      probGO <- pnorm(z_GO,lower.tail=lower)
      if(probGO > c1_sig){
        resIJ <- "GO"
      } else {
        resIJ <- "STOP"
      }
      simVec <- c(simVec,resIJ)
    }
    allResults <- rbind(allResults,data.frame(delta=delta[dI],GO=sum(simVec=="GO")/nSim,STOP=sum(simVec=="STOP")/nSim))
  }
  allResults <- formatPlotData1criteria(allResults)
  return(allResults)
}

binomial2MeansData_Sim_OLD <- function(n1,n2,probRef,direction,c1_tv,c1_sig,xlow,xupp,xgap,nSim=5000,test=1){
  # Create data through simulation for binomial OC curves:
  # Testing method: 1 [Normal]
  delta <- seq(xlow,xupp,length.out=xgap)  
  allResults <- data.frame(delta=as.numeric(),GO=as.numeric(),STOP=as.numeric(),stringsAsFactors=F)
  for(dI in 1:length(delta)){
    #For each delta:
    probTrt <- probRef + delta[dI]
    seIJ <- sqrt(((probTrt*(1-probTrt))/n1) + ((probRef*(1-probRef))/n2))
    simVec <- NULL
    for(sJ in 1:nSim){
      #For each simulation
      yIJ <- seIJ*rnorm(1)+delta[dI]
      z_GO <- (yIJ-c1_tv)/seIJ
      if(direction){
        lower <- TRUE
      } else{
        lower <- FALSE
      }
      probGO <- pnorm(z_GO,lower.tail=lower)
      if(probGO > c1_sig){
        resIJ <- "GO"
      } else {
        resIJ <- "STOP"
      }
      simVec <- c(simVec,resIJ)
    }
    allResults <- rbind(allResults,data.frame(delta=delta[dI],GO=sum(simVec=="GO")/nSim,STOP=sum(simVec=="STOP")/nSim))
  }
  allResults <- formatPlotData1criteria(allResults)
  return(allResults)
}


############################################################################
###   Determine whether planned precision means C2 could be redundant:   ###
############################################################################

determine_minimum_prec_sub <- function(studyComp,tempPrec,tempDF,direction,
                                       c1_tv,c1_sig,c2_tv,c2_sig,precT=TRUE){
  warn <- FALSE
  if(studyComp=="Normal"){
    if(precT){tempPrec <- tempPrec$SE}
    c1_50 <- normalMeansPowerPoint(c1_tv,c1_sig,direction,as.numeric(tempPrec),tempDF,0.5)
    c2_50 <- normalMeansPowerPoint(c2_tv,c2_sig,direction,as.numeric(tempPrec),tempDF,0.5)
  } else if(studyComp=="Binomial"){
    c1_50 <- binomialMeansPowerPoint_All(0.5,tempPrec,tempDF,direction,c1_tv,c1_sig,alg_binomial_power_gap,alg_binomial_power_step)
    c2_50 <- binomialMeansPowerPoint_All(0.5,tempPrec,tempDF,direction,c2_tv,c2_sig,alg_binomial_power_gap,alg_binomial_power_step)
  } else {
    validate(need(NULL,"WARNING: There is an issue with determine_minimum_prec_sub - not normal?"))
  }
  if(direction){
    diff50 <- c2_50-c1_50  
  } else {
    diff50 <- c1_50-c2_50
  }
  if(sum(diff50 < 0)>0){
    warn <- TRUE
  }
  return(warn)
}


###############################
###   Formatting results:   ###
###############################

formatPercDiffNorm <- function(value,logScale){
  if(logScale==1){ ### Log[e]
    return(log((100+value)/100))
  } else if(logScale==2){ ### Log10
    return(log10((100+value)/100))
  }
}
formatPercDiffNorm_Inv <- function(value,logScale){
  if(logScale==1){ ### Log[e]
    return(exp(value)*100-100)
  } else if(logScale==2){ ### Log10
    return((10^value)*100-100)
  }
}

formatPercDiffBin <- function(value){
  return(value/100)
}
formatPercDiffBin_Inv <- function(value){
  return(value*100)
}

formatRatioNorm <- function(value,logScale){
  if(logScale==1){ ### Log[e]
    return(log(value))
  } else if(logScale==2){ ### Log10
   return(log10(value)) 
  }
}

formatRatioNorm_Inv <- function(value,logScale){
  if(logScale==1){ ### Log[e]
    return(exp(value))
  } else if(logScale==2){ ### Log10
    return(10^value) 
  }
}

formatPlotData2criteria <- function(allResults,interim=FALSE){
  plotData <- melt(allResults,id=c("delta"))
  plotData$value <- as.numeric(plotData$value)
  plotData$variable <- as.character(plotData$variable)
  if(interim){
    plotData[plotData$variable=="GO","variable"] <- label_decision_interim_2_both
    plotData[plotData$variable=="PAUSE","variable"] <- label_decision_interim_2_one
    plotData[plotData$variable=="STOP","variable"] <- label_decision_interim_2_none
    plotData$variable <- factor(as.character(plotData$variable),levels=c(label_decision_interim_2_both,label_decision_interim_2_one,label_decision_interim_2_none))
  } else {
    plotData[plotData$variable=="GO","variable"] <- label_decision_2_both
    plotData[plotData$variable=="PAUSE","variable"] <- label_decision_2_one
    plotData[plotData$variable=="STOP","variable"] <- label_decision_2_none
    plotData$variable <- factor(as.character(plotData$variable),levels=c(label_decision_2_both,label_decision_2_one,label_decision_2_none))
  }
  return(plotData)
}

formatPlotData1criteria <- function(allResults){
  plotData <- melt(allResults,id=c("delta"))
  plotData$value <- as.numeric(plotData$value)
  plotData$variable <- as.character(plotData$variable)
  plotData[plotData$variable=="GO","variable"] <- label_decision_1_one
  plotData[plotData$variable=="STOP","variable"] <- label_decision_1_none
  plotData$variable <- factor(as.character(plotData$variable),levels=c(label_decision_1_one,label_decision_1_none))
  return(plotData)
}

formatPlotData <- function(allResults,dist,diff=NA,logScale=NA){
  ### Set key columns to be factors:
  colChange <- c("N1","N2")
  if(dist=="Normal"){
    colChange <- c(colChange,"Sigma","SE")
  } else if(dist=="Binomial"){
    colChange <- c(colChange,"ProbRef")
  }
  for(colI in 1:length(colChange)){
    levelsI <- unique(allResults[,colChange[colI]])
    if(!is.na(levelsI[1])){
      allResults[,colChange[colI]] <- factor(allResults[,colChange[colI]],levels=levelsI)
    }
  }
  ### Re-format results:
  if(dist=="Normal"){
    if(diff==label_study_perc){
      allResults$delta <- formatPercDiffNorm_Inv(allResults$delta,logScale)
    } else if(diff==label_study_ratio){
      allResults$delta <- formatRatioNorm_Inv(allResults$delta,logScale)
    }
  } else if(dist=="Binomial"){
    allResults$delta <- formatPercDiffBin_Inv(allResults$delta)
  }
  return(allResults)
}


##############################
###   Summary key table:   ###
##############################

create_summary_key_table_conv_normal <- function(inputTable,info,tempDF,nDec,direction,c1_tv,c1_sig,c2_tv,c2_sig,delta,title){
  tempTable <-  normalMeansData(as.numeric(info$SE),tempDF,nDec,direction,
                                 c1_tv,c1_sig,c2_tv,c2_sig,delta,delta,1)
  if(nDec==1){
    goName <- label_decision_1_one
    diName <- NA
    stName <- label_decision_1_none
  } else {
    goName <- label_decision_2_both
    diName <- label_decision_2_one
    stName <- label_decision_2_none
  }
  outTable <- rbind(inputTable,data.frame(Graph=title,N1=info$N1,N2=info$N2,Sigma=info$Sigma,SE=info$SE,Delta=delta,
                                          Prob_Go=tempTable[tempTable$variable==goName,"value"],
                                          Prob_Discuss=if(is.na(diName)){NA}else{tempTable[tempTable$variable==diName,"value"]},
                                          Prob_Stop=tempTable[tempTable$variable==stName,"value"],stringsAsFactors=F))
  return(outTable)
}

create_summary_key_table_conv_binomial <- function(inputTable,precTable,tempDF,nDec,direction,c1_tv,c1_sig,c2_tv,c2_sig,delta,title){
  tempTable <-  binomialMeansData(precTable,as.numeric(tempDF),as.numeric(nDec),
                                   direction,c1_tv,c1_sig,c2_tv,c2_sig,delta,delta,1)
  if(nDec==1){
    goName <- label_decision_1_one
    diName <- NA
    stName <- label_decision_1_none
  } else {
    goName <- label_decision_2_both
    diName <- label_decision_2_one
    stName <- label_decision_2_none
  }
  outTable <- rbind(inputTable,data.frame(Graph=title,N1=precTable$N1,N2=precTable$N2,ProbRef=precTable$ProbRef,Delta=delta,
                                          Prob_Go=tempTable[tempTable$variable==goName,"value"],
                                          Prob_Discuss=if(is.na(diName)){NA}else{tempTable[tempTable$variable==diName,"value"]},
                                          Prob_Stop=tempTable[tempTable$variable==stName,"value"],stringsAsFactors=F))
  return(outTable)
}

key_table_utility <- function(kTable,input_var,label_input){
  if(!is.null(input_var) && !is.na(input_var) && input_var != key_lab_table_all){
    kTable <- kTable[kTable[,label_input] == input_var,]
  }
 return(kTable)
}


#########################
###   Create plots:   ###
#########################

createPlot <- function(plotData,tval,titles,xValues,yValues,
                       lColours,vertLines=NULL,horzLines=NULL,
                       lType=c("N1","Sample size1",TRUE),sType=c("Sigma","SD",TRUE),aType=c("N2","Sample size2",TRUE),
                       aOptions=c(1)){
#Generic function to create plots based on following inputs:
#INPUTS:
# plotData  - Data for plotting in OC format
# tval      - Target value
# titles    - Vector of titles: c(Main title,X-axis title,Y-axis title,Legend title)
# xValues   - Vector of x axis options: c(minimum,maximum,breaks)
# yValues   - Vector of y axis options: c(breaks,minimum,maximum)
# lColours  - Which lines to plot
# vertLines - Information on adding vertical lines
# horzLines - Information on adding horizontal lines
# lType     - Information on line type
# sType     - Information on shape type
# aType     - Information on alpha type
# aOptions  - Advanced options: c(Size multiplier)
  
  ### Format data:
  if(nrow(plotData)>0){
    if(as.logical(lType[3])){plotData$lType <- plotData[,lType[1]]} else {plotData$lType <- 1}
    if(as.logical(sType[3])){plotData$sType <- plotData[,sType[1]]} else {plotData$sType <- 1}
    if(as.logical(aType[3])){plotData$aType <- plotData[,aType[1]]} else {plotData$aType <- 1}
  } else {
    plotData$lType <- as.numeric()
    plotData$sType <- as.numeric()
    plotData$aType <- as.numeric()
  }
  
  ### Create plot template:
  mainPlot <- ggplot(plotData,aes(x=delta,y=100*as.numeric(value),by=variable,colour=variable,
                                  linetype=as.factor(lType),size=as.factor(sType),alpha=as.factor(aType)))
  mainPlot <- mainPlot + geom_hline(yintercept=0,colour="black") + geom_vline(xintercept=0,colour="black")
  
  ### Add oc curves:
  mainPlot <- mainPlot + geom_line()
  
  ### Add key probabilities:
  mainPlot <- mainPlot + geom_hline(yintercept=c(20,50,80),linetype="dashed")
  mainPlot <- mainPlot + geom_vline(xintercept=tval,linetype="dashed")
  
  ### Add additional vertical lines:
  if(!is.null(vertLines)){
    if(length(vertLines)>0){
      for(i in 1:length(vertLines)){
        mainPlot <- mainPlot + geom_vline(xintercept=as.numeric(vertLines[[i]][1]),colour=vertLines[[i]][2],linetype="dashed",size=2)
      }
    }
  }
  ### Add additional horizontal lines:
  if(!is.null(horzLines)){
    if(length(horzLines)>0){
      for(i in 1:length(horzLines)){
        mainPlot <- mainPlot + geom_hline(yintercept=as.numeric(horzLines[[i]][1]),colour=horzLines[[i]][2],linetype="dashed",size=1.25)
      }
    }
  }
  ### Tidy-up plot:
  mainPlot <- mainPlot + theme(plot.title=element_text(size=20),axis.line=element_line(colour="black"),panel.grid.major=element_line(colour="lightgrey"),
                               panel.grid.minor=element_blank(),panel.border=element_blank(),panel.background=element_blank(),
                               axis.text.x=element_text(colour="black",size=20),axis.text.y=element_text(colour="black",size=20),
                               axis.title.x=element_text(size=25,vjust=0.05),axis.title.y=element_text(size=25,vjust=0.25),
                               strip.text.y = element_text(size=12, face="bold"))
  
  ### Add manual scales:
  mainPlot <- mainPlot + scale_colour_manual(values=lColours)
  mainPlot <- mainPlot + scale_alpha_manual(values=c(1,0.75,0.5,0.25))
  mainPlot <- mainPlot + scale_linetype_manual(values=c("solid","longdash","dotted","dotdash"))
  if(length(unique(plotData$sType))==1){
    mainPlot <- mainPlot + scale_size_manual(values=c(2*aOptions[1]))  
  } else {
    mainPlot <- mainPlot + scale_size_manual(values=c(1*aOptions[1],1.75*aOptions[1],2.25*aOptions[1],3*aOptions[1]))  
  }
  
  ### Titles:
  mainPlot <- mainPlot + ggtitle(paste(titles[1],"\n",sep=""))
  mainPlot <- mainPlot + labs(x=paste0("\n",titles[2]),y=paste0(titles[3],"\n"))
  
  ### Legends:
  mainPlot <- mainPlot + labs(colour=as.character(titles[4]))
  mainPlot <- mainPlot + theme(legend.key.width = unit(20, "line"))
  mainPlot <- mainPlot + guides(colour=guide_legend(order=1,title.theme=element_text(size=18,angle=0),
                                                    label.theme=element_text(size=16,angle=0),
                                                    keywidth = 3, keyheight = 1.5,
                                                    override.aes=list(size=1.5)))
  mainPlot <- mainPlot + guides(linetype=guide_legend(order=2,title.theme=element_text(size=18,angle=0),
                                                      label.theme=element_text(size=16,angle=0),
                                                      keywidth = 4, keyheight = 1.5,
                                                      override.aes=list(size=1)))
  mainPlot <- mainPlot + guides(size=guide_legend(order=3,title.theme=element_text(size=18,angle=0),
                                                  label.theme=element_text(size=16,angle=0),
                                                  keywidth = 3, keyheight = 1.5))
  mainPlot <- mainPlot + guides(alpha=guide_legend(order=4,title.theme=element_text(size=18,angle=0),
                                                   label.theme=element_text(size=16,angle=0),
                                                   keywidth = 3, keyheight = 1.5,
                                                   override.aes=list(size=1.5)))
  
  ### Line, shape and alpha types:
  if(length(unique(plotData$lType))!=1 & as.logical(lType[3])){
    mainPlot <- mainPlot + labs(linetype=lType[2])
  } else {
    mainPlot <- mainPlot + guides(linetype=FALSE)
  }
  if(length(unique(plotData$sType))!=1 & as.logical(sType[3])){
    mainPlot <- mainPlot + labs(size=sType[2])
  } else {
    mainPlot <- mainPlot + guides(size=FALSE)
  }
  if(length(unique(plotData$aType))!=1 & as.logical(aType[3])){
    mainPlot <- mainPlot + labs(alpha=aType[2])
  } else {
    mainPlot <- mainPlot + guides(alpha=FALSE)
  }
  
  ### Axes:
  mainPlot <- mainPlot + scale_y_continuous(lim=c(yValues[2],yValues[3]),breaks=seq(yValues[2],yValues[3],yValues[1]))
  if(is.null(xValues[3]) | is.na(xValues[3])){
    mainPlot <- mainPlot + scale_x_continuous(lim=c(xValues[1],xValues[2]))
  } else {
    mainPlot <- mainPlot + scale_x_continuous(lim=c(xValues[1],xValues[2]),breaks=seq(xValues[1],xValues[2],by=xValues[3]))
  }
  
  ### Return plot:
  return(mainPlot)
}







#End of big bad functions
