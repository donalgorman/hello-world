
################################################
#                                              #
#   Operating Characteristic Curve Generator   #
#                [Version 2.0]                 #
#                                              #
################################################

### Search: 'TESTING' for notes as updating program - and UP TO HERE

### NOTE: Potential problem when switch from a completed run in normal to binomial getting un-informative error message 
###       - Similarly when go from completed binomial to normal
#           - NEED TO HAVE BETTER DEFAULTS FOR PRECISION IF USING BINOMIAL OR NORMAL - i.e. NOT DEFAULT (Although may not be an
#             issue when have blank initial values!)
###       - Likely partly due to not having any prob ref and other design criteria
###       - Looks like not having "N & Reference" for 'select precision option' causing the issue
###       - Only occurs when have default value for 'precision' - need to add in a change to the inital value of precision != 3

### NEXT STEPS:

#server script

#Creator D Gorman
#Date: 30 Dec 2015

shinyServer(function(input,output){

  ########################################
  ########################################
  ######   Dynamic UI for Inputs:   ######
  ########################################
  ########################################
  
  #
  #   Study Section:
  #
  output$studyUI_ocType <- renderUI({
    if(is.null(input$study_comparison)){
      return(NULL)
    } else {
      outList <- NULL
      if(input$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
        outList <- list(selectInput("study_comparison_type","Select output scale",choices=c(label_study_abs,label_study_perc,label_study_ratio),selected=init_study_comparison_type))
      }
      return(c(outList,list(selectInput("study_ocType","Select OC Type:",choices=c(label_study_conv,label_study_interim),selected=init_study_ocType),
                  uiOutput("studyUI_studyDesign"))))
    }
  })
  
  output$studyUI_studyDesign <- renderUI({
    if(is.null(input$study_ocType)){
      return(NULL)
    } else {
      if(input$study_ocType==label_study_interim){
        return(computerSaysNo())
      } else if (!(input$study_comparison %in% c(label_study_1Normal,label_study_1Binomial))){
        return(list(selectInput("study_studyDesign","Select Study Design:",choices=c(label_study_CrossOver,label_study_Parallel),selected=init_study_studyDesign)))
      } else {
        return(list(selectInput("study_studyDesign","Select Study Design:",choices=c(label_study_Single),selected=init_study_studyDesign)))  
      }
    }
  })
  
  
  #
  #   Decision Criteria Section:
  #
  output$decisionUI_start <- renderUI({
    if(is.null(input$study_ocType)){
      return(NULL)
    } else {
      if(input$study_ocType==label_study_conv){
        return(list(uiOutput("decisionUI_convCriteriaStart")))
      } else {
        return(computerSaysNo())
      }
    }
  })
  
  ### Conventional 1 or 2 decision criteria for end of study: ###
  output$decisionUI_convCriteriaStart <- renderUI({
    if(is.null(input$study_ocType)){
      return(NULL)
    } else {
      if(input$study_ocType==label_study_conv){
        return(list(radioButtons("decision_nCriteria",label=("Number of criteria"),choices=list("1 decision criterion"=1,"2 decision criteria"=2),selected=init_decision_nCriteria),
                    radioButtons("decision_direction",label=("Direction of treatment effect"),choices=list("Greater than TV"=1,"Less than TV"=2),selected=init_decision_direction,inline=TRUE),
                    uiOutput("decisionUI_convCriteria")))
      } else {
        return(list(radioButtons("decision_nCriteria",label=("Number of criteria"),choices=list("1 decision criterion"=1),selected=init_decision_nCriteria),
                    radioButtons("decision_direction",label=("Direction of treatment effect"),choices=list("Greater than TV"=1,"Less than TV"=2),selected=init_decision_direction,inline=TRUE),
                    uiOutput("decisionUI_convCriteria")))
      }
    }
  })
  
  output$decisionUI_convCriteria <- renderUI({
    if(is.null(input$decision_nCriteria)){
      return(NULL)
    } else {
      addText <- ""
      addText_zero <- "0"
      if(input$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial) || (input$study_comparison %in% c(label_study_2Normal,label_study_1Normal) & input$study_comparison_type==label_study_perc)){
        addText <- " (%)"
        addText_zero <- "0%"
      } else if(input$study_comparison %in% c(label_study_2Normal,label_study_1Normal) && input$study_comparison_type==label_study_ratio){
        addText <- " (Ratio)"
        addText_zero <- "1"
      }
      defVec1 <- list(numericInput("decision_c1_tv",paste0("C1 value",addText,":"),init_decision_c1_tv),
                      numericInput("decision_c1_sig","C1 Confidence (%):",init_decision_c1_sig))
     if(input$decision_nCriteria==1){
        return(c(list(h4(strong("C1 criterion options:")),
                      h5(em(paste0("Typically 'C1 value' is ",addText_zero," and 'C1 confidence' is 95%")))),
                 defVec1))
      } else if(input$decision_nCriteria==2){
        return(c(list(h4(strong("C1 criterion options:")),
                      h5(em(paste0("Typically 'C1 value' is ",addText_zero," (or the LRV) and 'C1 confidence' is 95%")))),
                 defVec1,
                 list(h4(strong("C2 criterion options:")),
                      h5(em("Typically 'C2 value' is the target value and 'C2 confidence' is 50%")),
                      numericInput("decision_c2_tv",paste0("C2 value",addText,":"),init_decision_c2_tv),
                      numericInput("decision_c2_sig","C2 Confidence (%):",init_decision_c2_sig))))
      }
    }
  })

  
  #
  #   Design Section:
  #
  output$designUI_start <- renderUI({
    if(is.null(input$study_ocType)){
      return(NULL)
    } else {
      if(input$study_ocType==label_study_conv){
        return(uiOutput("designUI_conv"))
      } else {
        return(computerSaysNo())
      }
    }
  })
  
  ### Conventional design options for a study: ###
  output$designUI_conv <- renderUI({
    if(is.null(input$study_studyDesign)){
      return(NULL)
    } else {
      if(input$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
        return(list(radioButtons("design_precision",label="Select precision option:",choices=list("N & SD"=1,"SE"=2),selected=init_design_precision),
                    uiOutput("designUI_precision_start")))
      } else if(input$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
        return(list(radioButtons("design_bin_method",label="Estimation method:",choices=list("Formula"=1,"Simulation"=2),
                                 selected=init_design_bin_method),
                    uiOutput("designUI_conv_binomial")))
      }
    }
  })

  output$designUI_conv_binomial <- renderUI({
    if(is.null(input$design_bin_method)){
      return(NULL)
    } else {
      if(input$design_bin_method==1){
        return(list(checkboxInput("design_normApprox",label="Normal Approximation",value=init_design_normApprox),
                    uiOutput("designUI_conv_binomial_sub")))
      } else if(input$design_bin_method==2){
        return(computerSaysNo())
        #return(list(radioButtons("design_bin_test",label="Testing method:",choices=list("Normal evaluation"=1),selected=init_design_bin_test),
        #            uiOutput("designUI_conv_binomial_sub")))
      }
    }
  })
  
  output$designUI_conv_binomial_sub <- renderUI({
    if(is.null(input$design_bin_method)){
      return(NULL)
    } else {
      if(input$design_bin_method==1 && !input$design_normApprox){
        return(computerSaysNo())
      } else {
        return(list(radioButtons("design_precision",label="Select precision option:",choices=list("N & Reference"=3),selected=init_design_precision),
                    uiOutput("designUI_precision_start")))
      }
    }
  })
  
  ### Generic UI outputs: ###
  output$designUI_precision_start <- renderUI({
    if(is.null(input$design_precision)){
      return(NULL)
    } else {
      if(input$study_studyDesign==label_study_Parallel){
        return(list(checkboxInput("design_equalN",label="Equal N in each arm?",value=init_design_equalN),
                    uiOutput("designUI_precision")))
      } else {
        return(uiOutput("designUI_precision"))
      }
    }
  })
  
  output$designUI_precision <- renderUI({
    if(is.null(input$design_precision) || (input$study_studyDesign==label_study_Parallel & is.null(input$design_equalN))){
      return(NULL)
    } else {
      choices <- list(parallel=(input$study_studyDesign==label_study_Parallel),
                      n1=FALSE,n2=FALSE,sigma=FALSE,probRef=FALSE,SE=FALSE,log=FALSE)
      outWidgets <- NULL
      
      ### Determine which widgets to return:
      if(input$design_precision==1 | input$design_precision==3){ ### N & SD or N & Prob Ref precision selected
        if(!choices$parallel){n1Name <- "total"
        } else if(input$design_equalN){n1Name <- "per arm"
        } else {
          n1Name <- "treatment"
          choices$n2 <- TRUE
        }
        choices$n1 <- TRUE
        if(input$design_precision==1){choices$sigma <- TRUE
        } else {choices$probRef <- TRUE}
      }
      if(input$design_precision==2){ ### SE precision selected
        choices$parallel <- FALSE
        choices$SE <- TRUE
      }
      if(input$study_comparison %in% c(label_study_2Normal,label_study_1Normal) && (input$study_comparison_type==label_study_perc || input$study_comparison_type==label_study_ratio)){
        choices$log <- TRUE
      }
      
      ### Return applicable widgets:
      if(choices$log){outWidgets <- c(outWidgets,list(uiOutput("designUI_log_start")))}
      if(choices$n1){
        outWidgets <- c(outWidgets,list(radioButtons("design_n1_n",label=paste0("Number of sample sizes (",n1Name,") to plot:"),
                                                     choices=list("1"=1,"2"=2,"3"=3,"4"=4),selected=init_design_n1_n,inline=TRUE),
                                        uiOutput("designUI_n1_multiple")))
      }
      if(choices$n2){outWidgets <- c(outWidgets,list(uiOutput("designUI_n2_start")))}
      if(choices$sigma){outWidgets <- c(outWidgets,list(uiOutput("designUI_sigma_start")))}
      if(choices$probRef){outWidgets <- c(outWidgets,list(uiOutput("designUI_probRef_start")))}
      if(choices$SE){outWidgets <- c(outWidgets,list(uiOutput("designUI_SE_start")))}
      if(input$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
        outWidgets <- c(outWidgets,list(uiOutput("designUI_normalApprox_prompt"),
                                        uiOutput("designUI_normalApprox")))
      }
      return(outWidgets)
    }
  })
  
  ### Log:
  output$designUI_log_start <- renderUI({
    if(is.null(input$study_comparison_type)){
      return(NULL)
    } else {
      if(input$design_precision==1){
        addText <- "Standard Deviation"
      } else if(input$design_precision==2){
        addText <-   "Standard Error"
      } else {
        addText <- "ERROR"
      }
      return(list(radioButtons("design_log",label=paste0("Scale for ",addText,":"),
                               choices=list("Log[e]"=1,"Log10"=2),selected=init_design_log,inline=TRUE)))
    }
  })
  
  ### N1:
  output$designUI_n1_multiple <- renderUI({
    if(is.null(input$design_n1_n)){
      return(NULL)
    } else {
      return(widC_MultipleNumericRowEntries("design_n1_","Sample size",input$design_n1_n))
    }
  })
  
  ### N2:
  output$designUI_n2_start <- renderUI({
    if(is.null(input$design_precision)){
      return(null)
    } else{
      return(list(radioButtons("design_n2_n",label="Number of sample sizes (control) to plot:",
                               choices=list("1"=1,"2"=2,"3"=3,"4"=4),selected=init_design_n2_n,inline=TRUE),
                  uiOutput("designUI_n2_multiple")))
    }
  })
  output$designUI_n2_multiple <- renderUI({
    if(is.null(input$design_n2_n)){
      return(NULL)
    } else {
      return(widC_MultipleNumericRowEntries("design_n2_","Sample size",input$design_n2_n))
    }
  })
  
  ### SD:
  output$designUI_sigma_start <- renderUI({
    if(is.null(input$design_precision)){
      return(null)
    } else{
      if(input$study_comparison_type==label_study_abs){
        addText <- ""
      } else {
        addText <- " (log scale)"
      }
      return(list(radioButtons("design_sigma_n",label=paste0("Number of standard deviations",addText," to plot:"),
                               choices=list("1"=1,"2"=2,"3"=3,"4"=4),selected=init_design_sigma_n,inline=TRUE),
                  uiOutput("designUI_sigma_multiple")))
    }
  })
  output$designUI_sigma_multiple <- renderUI({
    if(is.null(input$design_sigma_n)){
      return(NULL)
    } else {
      return(widC_MultipleNumericRowEntries("design_sigma_","Standard Deviation",input$design_sigma_n))
    }
  })
  
  ### Reference Percentage:
  output$designUI_probRef_start <- renderUI({
    if(is.null(input$design_precision)){
      return(NULL)
    } else {
      return(list(radioButtons("design_probRef_n",label="Number of reference percentages to plot:",
                               choices=list("1"=1,"2"=2,"3"=3,"4"=4),selected=init_design_probRef_n,inline=TRUE),
                  uiOutput("designUI_probRef_multiple")))
    }
  })
  output$designUI_probRef_multiple <- renderUI({
    if(is.null(input$design_probRef_n)){
      return(NULL)
    } else {
      return(widC_MultipleNumericRowEntries("design_probRef_","Reference Percentage",input$design_probRef_n,"(%)"))
    }
  })
  
  ### SE:
  output$designUI_SE_start <- renderUI({
    if(is.null(input$design_precision)){
      return(null)
    } else{
      if(input$study_comparison_type==label_study_abs){
        addText <- ""
      } else {
        addText <- " (log scale)"
      }
      return(list(radioButtons("design_se_n",label=paste0("Number of standard errors",addText," to plot:"),
                               choices=list("1"=1,"2"=2,"3"=3,"4"=4),selected=init_design_se_n,inline=TRUE),
                  uiOutput("designUI_SE_multiple")))
    }
  })
  output$designUI_SE_multiple <- renderUI({
    if(is.null(input$design_se_n)){
      return(NULL)
    } else {
      return(widC_MultipleNumericRowEntries("design_se_","Standard Error",input$design_se_n))
    }
  })
  
  ### Normal approximation:
  output$designUI_normalApprox_prompt <- renderUI({
    if(is.null(input$design_precision)){
      return(NULL)
    } else {
      return(checkboxInput("design_normApprox",label="Normal Approximation (DF= 999)",value=init_design_normApprox))
    }
  })
  output$designUI_normalApprox <- renderUI({
    if(is.null(input$design_normApprox)){
      return(NULL)
    } else if(!input$design_normApprox){
      return(numericInput("design_df","Degrees of freedom:",init_design_df))
    } else {
      return(NULL)
    }
  })
  
  
  #
  #   Output Options Section:
  #
  output$optionsOutUI_start <- renderUI({
    if(is.null(input$study_comparison) || is.null(input$study_ocType)){
      return(NULL)
    } else {
      if(input$study_ocType==label_study_interim){
        return(computerSaysNo())
      } else {
        addText <- ""
        if(input$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial) || (input$study_comparison %in% c(label_study_2Normal,label_study_1Normal) & input$study_comparison_type==label_study_perc)){
          addText <- " (%)"
        } else if(input$study_comparison %in% c(label_study_2Normal,label_study_1Normal) && input$study_comparison_type==label_study_ratio){
          addText <- " (Ratio)"
        }
        return(list(textInput("plot_title","Title of plot:",init_plot_title),
                    textInput("plot_userID","User ID:",init_plot_userID),
                    numericInput("plot_xlow",paste0("X lower limit",addText,":"),init_plot_xlow),
                    numericInput("plot_xupp",paste0("X upper limit",addText,":"),init_plot_xupp)))
      }
    }
  })
  
  #
  #   Additional Options Section:
  #
  output$advanced_legendUI <- renderUI({
    if(is.null(input$design_precision)){
      return(NULL)
    } else {
      uiList <- list()
      if(input$decision_nCriteria==2){
        uiList <- list(uiList,textInput("advanced_legend_label_dec_2_both","Two decision criteria plot - Label for green curve (leave blank for default):",init_advanced_legend_label_dec_2_both),
                       textInput("advanced_legend_label_dec_2_one","Two decision criteria plot - Label for orange curve (leave blank for default):",init_advanced_legend_label_dec_2_one),
                       textInput("advanced_legend_label_dec_2_none","Two decision criteria plot - Label for red curve (leave blank for default):",init_advanced_legend_label_dec_2_none))
      }
      uiList <- list(uiList,textInput("advanced_legend_label_dec_1_one","One decision criterion plot(s) - Label for green curve (leave blank for default):",init_advanced_legend_label_dec_1_one),
                     textInput("advanced_legend_label_dec_1_none","One decision criterion plot(s) - Label for red curve (leave blank for default):",init_advanced_legend_label_dec_1_none))
      if(input$design_precision==1 | input$design_precision==3){
        if(as.numeric(input$design_n1_n)>1){
          uiList <- list(uiList,textInput("advanced_legend_title_n1","Sample size legend title:",init_advanced_legend_title_n1))
        }
        if(input$study_studyDesign==label_study_Parallel && !input$design_equalN){
          if(as.numeric(input$design_n2_n)>1){
            uiList <- list(uiList,textInput("advanced_legend_title_n2","Sample size (control) legend title:",init_advanced_legend_title_n2))
          }
        }
        if(input$design_precision==1){
          if(as.numeric(input$design_sigma_n)>1){
            uiList <- list(uiList,textInput("advanced_legend_title_sigma","Standard deviation legend title:",init_advanced_legend_title_sigma))
          }   
        } else {
          if(as.numeric(input$design_probRef_n)>1){
            uiList <- list(uiList,textInput("advanced_legend_title_probRef","Reference percentage legend title:",init_advanced_legend_title_probRef))
          }
        }
      }
      if(input$design_precision==2){
        if(as.numeric(input$design_se_n)>1){
          uiList <- list(uiList,textInput("advanced_legend_title_se","Standard error legend title:",init_advanced_legend_title_se))
        }
      }
      return(uiList)
    }
  })
  
  output$advanced_linesVertUI <- renderUI({
    if(is.null(input$advanced_lines_vert_number)){
      return(NULL)
    } else if(input$advanced_lines_vert_number!=0){
      uiList <- list()
      for(i in 1:input$advanced_lines_vert_number){
        uiList[[(2*i)-1]] <- numericInput(paste0("advanced_lines_vert_pos",i),paste0("Vertical line position ",i,":"),get(paste0("init_advanced_lines_vert_pos",i)))
        uiList[[2*i]] <- selectInput(paste0("advanced_lines_vert_col",i),paste0("Vertical line colour ",i,":"),
                                     choices=label_advanced_lines_vert_colours,selected=init_advanced_lines_vert_colour)
      }
      return(uiList)
    } else {
      return(NULL)
    }
  })
  
  output$advanced_linesHorzUI <- renderUI({
    if(is.null(input$advanced_lines_horz_number)){
      return(NULL)
    } else if (input$advanced_lines_horz_number!=0){
      uiList <- list()
      for(i in 1:input$advanced_lines_horz_number){
        uiList[[(2*i)-1]] <- numericInput(paste0("advanced_lines_horz_pos",i),paste0("Horizontal line position ",i,":"),get(paste0("init_advanced_lines_horz_pos",i)))
        uiList[[2*i]] <- selectInput(paste0("advanced_lines_horz_col",i),paste0("Horizontal line colour ",i,":"),
                                     choices=label_advanced_lines_horz_colours,selected=init_advanced_lines_horz_colour)
      }
      return(uiList)
    } else {
      return(NULL)
    }
  })
  
  output$advanced_plotSimUI <- renderUI({
    if(is.null(input$study_ocType) || (input$study_ocType==label_study_conv)){
      return(NULL)
    } else {
      return(numericInput("advanced_plot_sim","Number of simulations:",init_advanced_plot_sim))
    }
  })
  
  
  ########################################
  ########################################
  ######   Observe input changes:   ######
  ########################################
  ########################################
  
  # This section ensures that when update button is pressed all inputs are appropriately updated for downstream outputs
  
  study_obs <- reactiveValues(study_comparison=NULL,study_comparison_type=NULL,study_ocType=NULL,study_studyDesign=NULL)
  
  decision_obs <- reactiveValues(decision_nCriteria=NULL,decision_direction=NULL,decision_c1_tv=NULL,decision_c1_sig=NULL,
                                 decision_c2_tv=NULL,decision_c2_sig=NULL)
  
  design_obs <- reactiveValues(design_precision=NULL,design_bin_method=NULL,design_bin_test=NULL,
                               design_equalN=NULL,design_log=NULL,design_n1_n=NULL,
                               design_n1_1=NULL,design_n1_2=NULL,design_n1_3=NULL,design_n1_4=NULL,
                               design_n2_n=NULL,design_n2_1=NULL,design_n2_2=NULL,design_n2_3=NULL,
                               design_n2_4=NULL,design_sigma_n=NULL,design_sigma_1=NULL,design_sigma_2=NULL,
                               design_sigma_3=NULL,design_sigma_4=NULL,design_probRef_n=NULL,design_probRef_1=NULL,
                               design_probRef_2=NULL,design_probRef_3=NULL,design_probRef_4=NULL,design_se_n=NULL,
                               design_se_1=NULL,design_se_2=NULL,design_se_3=NULL,design_se_4=NULL,design_interim_prop=NULL,
                               design_normApprox=NULL,design_df=NULL)
  
  plot_obs <- reactiveValues(plot_title=NULL,plot_userID=NULL,plot_xlow=NULL,plot_xupp=NULL)
  
  advanced_obs <- reactiveValues(advanced_yaxis_title=NULL,advanced_yaxis_break=NULL,advanced_yaxis_low=NULL,
                                 advanced_yaxis_upp=NULL,advanced_xaxis_title=NULL,advanced_xaxis_break=NULL,
                                 advanced_legend_title=NULL,
                                 advanced_legend_label_dec_2_both=NULL,advanced_legend_label_dec_2_one=NULL,advanced_legend_label_dec_2_none=NULL,
                                 advanced_legend_label_dec_1_one=NULL,advanced_legend_label_dec_1_none=NULL,
                                 advanced_legend_title_interim=NULL,advanced_legend_title_se=NULL,
                                 advanced_legend_title_n1=NULL,advanced_legend_title_n2=NULL,advanced_legend_title_sigma=NULL,
                                 advanced_legend_title_probRef=NULL,advanced_lines_vert_number=NULL,advanced_lines_vert_pos1=NULL,
                                 advanced_lines_vert_col1=NULL,advanced_lines_vert_pos2=NULL,advanced_lines_vert_col2=NULL,
                                 advanced_lines_vert_pos3=NULL,advanced_lines_vert_col3=NULL,advanced_lines_horz_number=NULL,
                                 advanced_lines_horz_pos1=NULL,advanced_lines_horz_col1=NULL,advanced_lines_horz_pos2=NULL,
                                 advanced_lines_horz_col2=NULL,advanced_lines_horz_pos3=NULL,advanced_lines_horz_col3=NULL,
                                 advanced_footnote_choice=NULL,advanced_plot_gap=NULL,advanced_plot_sim=NULL,
                                 advanced_plot_width=NULL,advanced_plot_height=NULL,advanced_plot_curves=NULL,advanced_plot_size=NULL)
  
  observeEvent(input$updateOutput,{
    study_obs$study_comparison <- input$study_comparison
    study_obs$study_comparison_type <- input$study_comparison_type
    study_obs$study_ocType <- input$study_ocType
    study_obs$study_studyDesign <- input$study_studyDesign

    decision_obs$decision_nCriteria <- input$decision_nCriteria
    decision_obs$decision_direction <- input$decision_direction
    decision_obs$decision_c1_tv <- input$decision_c1_tv
    decision_obs$decision_c1_sig <- input$decision_c1_sig
    decision_obs$decision_c2_tv <- input$decision_c2_tv
    decision_obs$decision_c2_sig <- input$decision_c2_sig

    design_obs$design_precision <- input$design_precision
    design_obs$design_bin_method <- input$design_bin_method
    design_obs$design_bin_test <- input$design_bin_test
    design_obs$design_equalN <- input$design_equalN
    design_obs$design_log <- input$design_log
    design_obs$design_n1_n <- input$design_n1_n
    design_obs$design_n1_1 <- input$design_n1_1
    design_obs$design_n1_2 <- input$design_n1_2
    design_obs$design_n1_3 <- input$design_n1_3
    design_obs$design_n1_4 <- input$design_n1_4
    design_obs$design_n2_n <- input$design_n2_n
    design_obs$design_n2_1 <- input$design_n2_1
    design_obs$design_n2_2 <- input$design_n2_2
    design_obs$design_n2_3 <- input$design_n2_3
    design_obs$design_n2_4 <- input$design_n2_4
    design_obs$design_sigma_n <- input$design_sigma_n
    design_obs$design_sigma_1 <- input$design_sigma_1
    design_obs$design_sigma_2 <- input$design_sigma_2
    design_obs$design_sigma_3 <- input$design_sigma_3
    design_obs$design_sigma_4 <- input$design_sigma_4
    design_obs$design_probRef_n <- input$design_probRef_n
    design_obs$design_probRef_1 <- input$design_probRef_1
    design_obs$design_probRef_2 <- input$design_probRef_2
    design_obs$design_probRef_3 <- input$design_probRef_3
    design_obs$design_probRef_4 <- input$design_probRef_4
    design_obs$design_se_n <- input$design_se_n
    design_obs$design_se_1 <- input$design_se_1
    design_obs$design_se_2 <- input$design_se_2
    design_obs$design_se_3 <- input$design_se_3
    design_obs$design_se_4 <- input$design_se_4
    design_obs$design_normApprox <- input$design_normApprox
    design_obs$design_df <- input$design_df

    plot_obs$plot_title <- input$plot_title
    plot_obs$plot_userID <- input$plot_userID
    plot_obs$plot_xlow <- input$plot_xlow
    plot_obs$plot_xupp <- input$plot_xupp

    advanced_obs$advanced_yaxis_title <- input$advanced_yaxis_title
    advanced_obs$advanced_yaxis_break <- input$advanced_yaxis_break
    advanced_obs$advanced_yaxis_low <- input$advanced_yaxis_low
    advanced_obs$advanced_yaxis_upp <- input$advanced_yaxis_upp
    advanced_obs$advanced_xaxis_title <- input$advanced_xaxis_title
    advanced_obs$advanced_xaxis_break <- input$advanced_xaxis_break
    advanced_obs$advanced_legend_title <- input$advanced_legend_title
    advanced_obs$advanced_legend_label_dec_2_both <- input$advanced_legend_label_dec_2_both
    advanced_obs$advanced_legend_label_dec_2_one <- input$advanced_legend_label_dec_2_one
    advanced_obs$advanced_legend_label_dec_2_none <- input$advanced_legend_label_dec_2_none
    advanced_obs$advanced_legend_label_dec_1_one <- input$advanced_legend_label_dec_1_one
    advanced_obs$advanced_legend_label_dec_1_none <- input$advanced_legend_label_dec_1_none
    
    advanced_obs$advanced_legend_title_interim <- input$advanced_legend_title_interim
    advanced_obs$advanced_legend_title_se <- input$advanced_legend_title_se
    advanced_obs$advanced_legend_title_n1 <- input$advanced_legend_title_n1
    advanced_obs$advanced_legend_title_n2 <- input$advanced_legend_title_n2
    advanced_obs$advanced_legend_title_sigma <- input$advanced_legend_title_sigma
    advanced_obs$advanced_legend_title_probRef <- input$advanced_legend_title_probRef
    advanced_obs$advanced_lines_vert_number <- input$advanced_lines_vert_number
    advanced_obs$advanced_lines_vert_pos1 <- input$advanced_lines_vert_pos1
    advanced_obs$advanced_lines_vert_col1 <- input$advanced_lines_vert_col1
    advanced_obs$advanced_lines_vert_pos2 <- input$advanced_lines_vert_pos2
    advanced_obs$advanced_lines_vert_col2 <- input$advanced_lines_vert_col2
    advanced_obs$advanced_lines_vert_pos3 <- input$advanced_lines_vert_pos3
    advanced_obs$advanced_lines_vert_col3 <- input$advanced_lines_vert_col3
    advanced_obs$advanced_lines_horz_number <- input$advanced_lines_horz_number
    advanced_obs$advanced_lines_horz_pos1 <- input$advanced_lines_horz_pos1
    advanced_obs$advanced_lines_horz_col1 <- input$advanced_lines_horz_col1
    advanced_obs$advanced_lines_horz_pos2 <- input$advanced_lines_horz_pos2
    advanced_obs$advanced_lines_horz_col2 <- input$advanced_lines_horz_col2
    advanced_obs$advanced_lines_horz_pos3 <- input$advanced_lines_horz_pos3
    advanced_obs$advanced_lines_horz_col3 <- input$advanced_lines_horz_col3
    advanced_obs$advanced_footnote_choice <- input$advanced_footnote_choice
    advanced_obs$advanced_plot_gap <- input$advanced_plot_gap
    advanced_obs$advanced_plot_sim <- input$advanced_plot_sim
    advanced_obs$advanced_plot_width <- input$advanced_plot_width
    advanced_obs$advanced_plot_height <- input$advanced_plot_height
    advanced_obs$advanced_plot_curves <- input$advanced_plot_curves
    advanced_obs$advanced_plot_size <- input$advanced_plot_size
  })
  
  ##################################
  ##################################
  ######   Validate inputs:   ######
  ##################################
  ##################################
  #
  # Tab names:
  # - Study 
  # - Decision
  # - Design
  # - Plot
  # - Advanced
  
  `%then%` <- shiny:::`%OR%`  
  
  #
  #   Study inputs:
  #
  study_comparison_val <- reactive({
    validate_generic_function(study_obs$study_comparison,"Study comparison [Study Tab]")
  })
  study_comparison_type_val <- reactive({
    validate_generic_function(study_obs$study_comparison_type,"Study comparison type [Study Tab]")
  })
  study_ocType_val <- reactive({
    validate_generic_function(study_obs$study_ocType,"Study OC Type [Study Tab]")
  })
  study_studyDesign_val <- reactive({
    validate_generic_function(study_obs$study_studyDesign,"Study Design [Study Tab]")
  })

  
  #
  #   Decision criteria inputs:
  #
  decision_nCriteria_val <- reactive({
    validate_generic_function(decision_obs$decision_nCriteria,"Number of decision criteria [Criteria Tab]")
  })
  decision_direction_val <- reactive({
    if(decision_obs$decision_direction==1){decision_direction <- TRUE  
    } else {decision_direction <- FALSE}
    return(decision_direction)
  })
  
  decision_c1_tv_val_sub <- reactive({
    if(study_obs$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
      if(study_obs$study_comparison_type==label_study_abs){
        validate_numeric_function(decision_obs$decision_c1_tv,"C1 Value  [Criteria Tab]")  
      } else if(study_obs$study_comparison_type==label_study_perc){
        validate_percDiffNorm_function(decision_obs$decision_c1_tv,"C1 Value [Criteria Tab]")  
      } else if(study_obs$study_comparison_type==label_study_ratio){
        validate_prec_function(decision_obs$decision_c1_tv,"C1 Value [Criteria Tab]")  
      }
    } else if(study_obs$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
      validate_percDiff_function(decision_obs$decision_c1_tv,"C1 Value [Criteria Tab]")
    }
    return(decision_obs$decision_c1_tv)
  })
  decision_c1_tv_val <- reactive({
    return(decision_c1_tv_val_sub())
  })
  decision_c1_tv_val_for <- function(){
    formatInputConvert(decision_c1_tv_val())
  }
  decision_c1_sig_val <- reactive({
    validate_sig_function(decision_obs$decision_c1_sig,"C1 [Criteria Tab]")
    return(decision_obs$decision_c1_sig/100)
  })
  
  decision_c2_tv_val_sub <- reactive({
    if(study_obs$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
      if(study_obs$study_comparison_type==label_study_abs){
        validate_numeric_function(decision_obs$decision_c2_tv,"C2 Value [Criteria Tab]")  
      } else if(study_obs$study_comparison_type==label_study_perc){
        validate_percDiffNorm_function(decision_obs$decision_c2_tv,"C2 Value [Criteria Tab]")  
      } else if(study_obs$study_comparison_type==label_study_ratio){
        validate_prec_function(decision_obs$decision_c2_tv,"C2 Value [Criteria Tab]")  
      }
    } else if(study_obs$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
      validate_percDiff_function(decision_obs$decision_c2_tv,"C2 Value [Criteria Tab]")
    }
    return(decision_obs$decision_c2_tv)
  })
  decision_c2_tv_val <- reactive({
    if(decision_direction_val()){
      validate(
        need(decision_c2_tv_val_sub() > decision_c1_tv_val(),"Target value C2 should be greater than Target value C1")
      )
    } else {
      validate(
        need(decision_c2_tv_val_sub() < decision_c1_tv_val(),"Target value C2 should be less than Target value C1")
      )     
    }
    return(decision_c2_tv_val_sub())
  })
  decision_c2_tv_val_for <- function(){
    formatInputConvert(decision_c2_tv_val())
  }
  
  decision_c2_sig_val_sub <- reactive({
    validate_sig_function(decision_obs$decision_c2_sig,"C2 [Criteria Tab]")
  })
  decision_c2_sig_val <- reactive({
    validate(
      need(decision_c2_sig_val_sub()/100 <= decision_c1_sig_val(),"C2 confidence should be smaller than or equal to C1 confidence")
    )
    return(decision_c2_sig_val_sub()/100)
  })
  
  
  #
  #   Design inputs:
  #
  design_precision_val <- reactive({
    validate_generic_function(design_obs$design_precision,"the precision option of the design [Design Tab]")
  })
  design_bin_method_val <- reactive({
    return(design_obs$design_bin_method)
  })
  design_bin_test_val <- reactive({
    return(design_obs$design_bin_test)
  })
  design_equalN_val <- reactive({
    #validate_generic_function(design_obs$design_equalN,"whether equal N in each study arm [Design Tab]")
    return(design_obs$design_equalN)
  })
  design_log_val <- reactive({
    validate_generic_function(design_obs$design_log,"Scale for percentage difference [Design Tab]")
    if(design_obs$design_log==3){validate(need(FALSE,"CV Scale not currently supported [Design Tab]"))}
    return(design_obs$design_log)
  })
  
  design_n1_n_val <- reactive({
    validate_generic_function(design_obs$design_n1_n,"Number of N1 sample sizes [Design Tab]")
  })
  design_n1_1_val <- reactive({
    validate_integer_function(design_obs$design_n1_1,"Sample size 1 [Design Tab]")
  })
  design_n1_2_val <- reactive({
    validate_integer_function(design_obs$design_n1_2,"Sample size 2 [Design Tab]")
  })
  design_n1_3_val <- reactive({
    validate_integer_function(design_obs$design_n1_3,"Sample size 3 [Design Tab]")
  })
  design_n1_4_val <- reactive({
    validate_integer_function(design_obs$design_n1_4,"Sample size 4 [Design Tab]")
  })
  
  design_n2_n_val <- reactive({
    validate_generic_function(design_obs$design_n2_n,"Number of N2 sample sizes [Design Tab]")
  })
  design_n2_1_val <- reactive({
    validate_integer_function(design_obs$design_n2_1,"Sample size 1 [Design Tab]")
  })
  design_n2_2_val <- reactive({
    validate_integer_function(design_obs$design_n2_2,"Sample size 2 [Design Tab]")
  })
  design_n2_3_val <- reactive({
    validate_integer_function(design_obs$design_n2_3,"Sample size 3 [Design Tab]")
  })
  design_n2_4_val <- reactive({
    validate_integer_function(design_obs$design_n2_4,"Sample size 4 [Design Tab]")
  })
  
  design_sigma_n_val <- reactive({
    validate_generic_function(design_obs$design_sigma_n,"Number of standard deviations [Design Tab]")
  })
  design_sigma_1_val <- reactive({
    validate_prec_function(design_obs$design_sigma_1,"Standard Deviation 1 [Design Tab]")
  })
  design_sigma_2_val <- reactive({
    validate_prec_function(design_obs$design_sigma_2,"Standard Deviation 2 [Design Tab]")
  })
  design_sigma_3_val <- reactive({
    validate_prec_function(design_obs$design_sigma_3,"Standard Deviation 3 [Design Tab]")
  })
  design_sigma_4_val <- reactive({
    validate_prec_function(design_obs$design_sigma_4,"Standard Deviation 4 [Design Tab]")
  })
  
  design_probRef_n_val <- reactive({
    validate_generic_function(design_obs$design_probRef_n,"Number of reference percentage [Design Tab]")
  })
  design_probRef_1_val <- reactive({
    validate_perc100_function(design_obs$design_probRef_1,"Reference Percentage 1 [Design Tab]")
  })
  design_probRef_2_val <- reactive({
    validate_perc100_function(design_obs$design_probRef_2,"Reference Percentage 2 [Design Tab]")
  })
  design_probRef_3_val <- reactive({
    validate_perc100_function(design_obs$design_probRef_3,"Reference Percentage 3 [Design Tab]")
  })
  design_probRef_4_val <- reactive({
    validate_perc100_function(design_obs$design_probRef_4,"Reference Percentage 4 [Design Tab]")
  })
  # Create formatted (i.e. as a proportion) for use with calculations:
  design_probRef_1_val_for <- function(){
    formatInputConvert(design_probRef_1_val())
  }
  design_probRef_2_val_for <- function(){
    formatInputConvert(design_probRef_2_val())
  }
  design_probRef_3_val_for <- function(){
    formatInputConvert(design_probRef_3_val())
  }
  design_probRef_4_val_for <- function(){
    formatInputConvert(design_probRef_4_val())
  }
  
  design_se_n_val <- reactive({
    validate_generic_function(design_obs$design_se_n,"Number of standard errors [Design Tab]")
  })
  design_se_1_val <- reactive({
    validate_prec_function(design_obs$design_se_1,"Standard Error 1 [Design Tab]")
  })
  design_se_2_val <- reactive({
    validate_prec_function(design_obs$design_se_2,"Standard Error 2 [Design Tab]")
  })
  design_se_3_val <- reactive({
    validate_prec_function(design_obs$design_se_3,"Standard Error 3 [Design Tab]")
  })
  design_se_4_val <- reactive({
    validate_prec_function(design_obs$design_se_4,"Standard Error 4 [Design Tab]")
  })
  
  design_normApprox_val <- reactive({
    validate_logical_function(design_obs$design_normApprox,"Design Normal Approximation [Design Tab]")
  })
  design_df_val <- reactive({
    validate_integer_function(design_obs$design_df,"Degrees of freedom [Design Tab]")
  })
  
  
  #
  #   Plot output inputs:
  #
  plot_title_val <- reactive({
    return(plot_obs$plot_title)
  })

  plot_userID_val <- reactive({
    return(plot_obs$plot_userID)
  })
  
  plot_xlow_val_sub <- reactive({
    if(determineEmpty(plot_obs$plot_xlow) & decision_direction_val() & FALSE){
      #### TESTING: If it appears interim wouldn't be too difficult can incorporate these automatic calculations for now
      ####          HOWEVER, this potentially has impact on graph re-drawing each time update is hit for some reason - i.e. 
      ###                    maybe not such a good idea!
      plot_obs$plot_xlow <- decision_c1_tv_val()
    } else {
      ### TESTING: Will need to write stand alone function for this, housed within server to begin with
      ###         - This will allow dealing with i.e. interim a lot easier!
      if(study_obs$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
        if(study_obs$study_comparison_type==label_study_abs){
          validate_numeric_function(plot_obs$plot_xlow,"X lower limit [Output Tab]")  
        } else if(study_obs$study_comparison_type==label_study_perc){
          validate_percDiffNorm_function(plot_obs$plot_xlow,"X lower limit [Output Tab]")  
        } else if(study_obs$study_comparison_type==label_study_ratio){
          validate_prec_function(plot_obs$plot_xlow,"X lower limit [Output Tab]")
        }
      } else if(study_obs$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
        validate_percDiffAxis_function(plot_obs$plot_xlow,"X lower limit [Output Tab]")
      }
    }
  })
  plot_xlow_val <- reactive({
    if(study_obs$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
      lowXlowValue <- design_probRef_1_val()
      if(design_probRef_n_val()>2){
        for(i in 2:design_probRef_n_val()){
          lowXlowValue <- max(lowXlowValue,get(paste0("design_probRef_",i,"_val"))())
        }
      }
      validate(
        need(plot_xlow_val_sub() + lowXlowValue>= 0,paste0("X-axis lower limit cannot be less than -",lowXlowValue,"% (based on difference to reference percentage) [Output Tab]"))
      )
    }
    return(plot_xlow_val_sub())
  })
  plot_xlow_val_for <- function(){
    formatInputConvert(plot_xlow_val())
  }
  
  plot_xupp_val_sub <- reactive({
    if(determineEmpty(plot_obs$plot_xupp) & !decision_direction_val() & FALSE){
      plot_obs$plot_xupp <- decision_c1_tv_val()
    } else {
      ### TESTING: Similar to plot_xlow_val as above:
      if(study_obs$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
        if(study_obs$study_comparison_type==label_study_abs){
          validate_numeric_function(plot_obs$plot_xupp,"X upper limit [Output Tab]")  
        } else if(study_obs$study_comparison_type==label_study_perc){
          validate_percDiffNorm_function(plot_obs$plot_xupp,"X upper limit [Output Tab]")  
        } else if(study_obs$study_comparison_type==label_study_ratio){
          validate_prec_function(plot_obs$plot_xupp,"X upper limit [Output Tab]")
        }
      } else if(study_obs$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
        validate_percDiffAxis_function(plot_obs$plot_xupp,"X upper limit [Output Tab]")
      }
    }
  })
  plot_xupp_val <- reactive({
    validate(
      need(plot_xupp_val_sub() > plot_xlow_val(),"X-axis upper limit must be greater than lower limit [Output Tab]")
    )
    if(study_obs$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
      uppXuppValue <- design_probRef_1_val()
      if(design_probRef_n_val()>2){
        for(i in 2:design_probRef_n_val()){
          uppXuppValue <- min(uppXuppValue,get(paste0("design_probRef_",i,"_val"))())
        }
      }
      validate(
        need(plot_xupp_val_sub() + uppXuppValue<= 100,paste0("X-axis upper limit cannot be greater than ",100-uppXuppValue,"% (based on difference to reference percentage) [Output Tab]"))
      )
    }
    return(plot_xupp_val_sub())
  })
  plot_xupp_val_for <- function(){
    formatInputConvert(plot_xupp_val())
  }
  
  
  #
  #   Advanced options:
  #
  advanced_yaxis_title_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_yaxis_title)){
      advanced_obs$advanced_yaxis_title <- "Probability of Passing Criteria (%)"
    }
    return(advanced_obs$advanced_yaxis_title)
  })
  advanced_yaxis_break_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_yaxis_break)){
      advanced_obs$advanced_yaxis_break <- 20
    }
    validate(
      need(advanced_obs$advanced_yaxis_break > 0,paste0("Y-axis breaks must be greater than zero [Advanced Tab/Y-axis]")) %then%
      need(advanced_obs$advanced_yaxis_break <= 100,paste0("Y-axis lower breaks must be less than or equal to 100 [Advanced Tab/Y-axis]"))  
    )
    return(advanced_obs$advanced_yaxis_break)
  })
  advanced_yaxis_low_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_yaxis_low)){
      advanced_obs$advanced_yaxis_low <- 0
    }
    validate(
      need(advanced_obs$advanced_yaxis_low >= 0,paste0("Y-axis lower limit must be greater than or equal to zero [Advanced Tab/Y-axis]")) %then%
      need(advanced_obs$advanced_yaxis_low < 100,paste0("Y-axis lower limit must be less than 100 [Advanced Tab/Y-axis]"))  
    )
    return(advanced_obs$advanced_yaxis_low)
  })
  advanced_yaxis_upp_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_yaxis_upp)){
      advanced_obs$advanced_yaxis_upp <- 100
    }
    validate(
      need(advanced_obs$advanced_yaxis_upp > 0,paste0("Y-axis upper limit must be greater than zero [Advanced Tab/Y-axis]")) %then%
      need(advanced_obs$advanced_yaxis_upp <= 100,paste0("Y-axis upper limit must be less than or equal to 100 [Advanced Tab/Y-axis]")) %then%
      need(advanced_obs$advanced_yaxis_upp > advanced_obs$advanced_yaxis_low,paste0("Y-axis upper limit must be greater than Y-axis lower limit [Advanced Tab/Y-axis]"))
    )
    return(advanced_obs$advanced_yaxis_upp)
  })
  
  advanced_xaxis_title_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_xaxis_title)){
      if(study_obs$study_comparison==label_study_2Normal){
        if(study_obs$study_comparison_type==label_study_abs){
          advanced_obs$advanced_xaxis_title <- "True Effect over Placebo"
        } else if(study_obs$study_comparison_type==label_study_perc){
          advanced_obs$advanced_xaxis_title <- "True Effect over Placebo (%)"
        } else if(study_obs$study_comparison_type==label_study_ratio){
          advanced_obs$advanced_xaxis_title <- "True Ratio to Placebo"
        }
      } else if(study_obs$study_comparison==label_study_1Normal){
        if(study_obs$study_comparison_type==label_study_abs){
          advanced_obs$advanced_xaxis_title <- "True Effect"
        } else if(study_obs$study_comparison_type==label_study_perc){
          advanced_obs$advanced_xaxis_title <- "True Effect (%)"
        } else if(study_obs$study_comparison_type==label_study_ratio){
          advanced_obs$advanced_xaxis_title <- "True Ratio"
        }
      } else if(study_obs$study_comparison==label_study_2Binomial){
        advanced_obs$advanced_xaxis_title <- "True Difference to Placebo (%)"
      } else if(study_obs$study_comparison==label_study_1Binomial){
        advanced_obs$advanced_xaxis_title <- "True Difference to Reference Proportion (%)"
      }
    }
    return(advanced_obs$advanced_xaxis_title)
  })
  advanced_xaxis_break_val <- reactive({
    validate(
      if(!is.null(advanced_obs$advanced_xaxis_break) && !is.na(advanced_obs$advanced_xaxis_break)){
        if(advanced_obs$advanced_xaxis_break != ""){
          need(advanced_obs$advanced_xaxis_break > 0,paste0("X-axis breaks must be greater than zero [Advanced Tab/X-axis]"))
        }
      }
    )
    return(advanced_obs$advanced_xaxis_break)
  })
  
  advanced_legend_title_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_legend_title)){
      advanced_obs$advanced_legend_title <- "Criteria Passed"
    }
    return(advanced_obs$advanced_legend_title)
  })
  
  advanced_legend_label_dec_2_both_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_legend_label_dec_2_both)){
      advanced_obs$advanced_legend_label_dec_2_both <- label_decision_2_both
    }
    return(advanced_obs$advanced_legend_label_dec_2_both)
  })
  advanced_legend_label_dec_2_one_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_legend_label_dec_2_one)){
      advanced_obs$advanced_legend_label_dec_2_one <- label_decision_2_one
    }
    return(advanced_obs$advanced_legend_label_dec_2_one)
  })
  advanced_legend_label_dec_2_none_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_legend_label_dec_2_none)){
      advanced_obs$advanced_legend_label_dec_2_none <- label_decision_2_none
    }
    return(advanced_obs$advanced_legend_label_dec_2_none)
  })
  
  advanced_legend_label_dec_1_one_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_legend_label_dec_1_one)){
      advanced_obs$advanced_legend_label_dec_1_one <- label_decision_1_one
    }
    return(advanced_obs$advanced_legend_label_dec_1_one)
  })
  advanced_legend_label_dec_1_none_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_legend_label_dec_1_none)){
      advanced_obs$advanced_legend_label_dec_1_none <- label_decision_1_none
    }
    return(advanced_obs$advanced_legend_label_dec_1_none)
  })
  
  advanced_legend_title_interim_val <- reactive({
    return(advanced_obs$advanced_legend_title_interim)
  })
  advanced_legend_title_se_val <- reactive({
    if(is.null(advanced_obs$advanced_legend_title_se)){
      return(init_advanced_legend_title_se)
    } else {
      return(advanced_obs$advanced_legend_title_se)
    }
  })
  advanced_legend_title_n1_val <- reactive({
    if(is.null(advanced_obs$advanced_legend_title_n1)){
      return(init_advanced_legend_title_n1)
    } else {
      return(advanced_obs$advanced_legend_title_n1)
    }
  })
  advanced_legend_title_n2_val <- reactive({
    if(is.null(advanced_obs$advanced_legend_title_n2)){
      return(init_advanced_legend_title_n2)
    } else {
      return(advanced_obs$advanced_legend_title_n2)
    }
  })
  advanced_legend_title_sigma_val <- reactive({
    if(is.null(advanced_obs$advanced_legend_title_sigma)){
      return(init_advanced_legend_title_sigma)
    } else {
      return(advanced_obs$advanced_legend_title_sigma)
    }
  })
  advanced_legend_title_probRef_val <- reactive({
    if(is.null(advanced_obs$advanced_legend_title_probRef)){
      return(init_advanced_legend_title_probRef)
    } else {
      return(advanced_obs$advanced_legend_title_probRef)
    }
  })
  
  advanced_lines_vert_number_val <- reactive({
    validate_generic_function(advanced_obs$advanced_lines_vert_number,"Number of vertical lines [Advanced Tab/Add lines]")
  })
  advanced_lines_vert_pos1_val <- reactive({
    if(study_obs$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
      validate_numeric_function(advanced_obs$advanced_lines_vert_pos1,"Vertical line position 1 [Advanced Tab/Add lines]")
    } else if(study_obs$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
      validate_percDiffAxis_function(advanced_obs$advanced_lines_vert_pos1,"Vertical line position 1 [Advanced Tab/Add lines]")
    }
  })
  advanced_lines_vert_col1_val <- reactive({
    validate_generic_function(advanced_obs$advanced_lines_vert_col1,"Vertical line colour 1 [Advanced Tab/Add lines]")
  })
  advanced_lines_vert_pos2_val <- reactive({
    if(study_obs$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
      validate_numeric_function(advanced_obs$advanced_lines_vert_pos2,"Vertical line position 2 [Advanced Tab/Add lines]")
    } else if(study_obs$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
      validate_percDiffAxis_function(advanced_obs$advanced_lines_vert_pos2,"Vertical line position 2 [Advanced Tab/Add lines]")
    }
  })
  advanced_lines_vert_col2_val <- reactive({
    validate_generic_function(advanced_obs$advanced_lines_vert_col2,"Vertical line colour 2 [Advanced Tab/Add lines]")
  })
  advanced_lines_vert_pos3_val <- reactive({
    if(study_obs$study_comparison %in% c(label_study_2Normal,label_study_1Normal)){
      validate_numeric_function(advanced_obs$advanced_lines_vert_pos3,"Vertical line position 3 [Advanced Tab/Add lines]")
    } else if(study_obs$study_comparison %in% c(label_study_2Binomial,label_study_1Binomial)){
      validate_percDiffAxis_function(advanced_obs$advanced_lines_vert_pos3,"Vertical line position 3 [Advanced Tab/Add lines]")
    }
  })
  advanced_lines_vert_col3_val <- reactive({
    validate_generic_function(advanced_obs$advanced_lines_vert_col3,"Vertical line colour 3 [Advanced Tab/Add lines]")
  })
  
  advanced_lines_horz_number_val <- reactive({
    validate_generic_function(advanced_obs$advanced_lines_horz_number,"Number of horizontal lines [Advanced Tab/Add lines]")
  })
  advanced_lines_horz_pos1_val <- reactive({
    validate_sig_function(advanced_obs$advanced_lines_horz_pos1,"Horizontal line position 1 [Advanced Tab/Add lines]")
  })
  advanced_lines_horz_col1_val <- reactive({
    validate_generic_function(advanced_obs$advanced_lines_horz_col1,"Horizontal line colour 1 [Advanced Tab/Add lines]")
  })
  advanced_lines_horz_pos2_val <- reactive({
    validate_sig_function(advanced_obs$advanced_lines_horz_pos2,"Horizontal line position 2 [Advanced Tab/Add lines]")
  })
  advanced_lines_horz_col2_val <- reactive({
    validate_generic_function(advanced_obs$advanced_lines_horz_col2,"Horizontal line colour 2 [Advanced Tab/Add lines]")
  })
  advanced_lines_horz_pos3_val <- reactive({
    validate_sig_function(advanced_obs$advanced_lines_horz_pos3,"Horizontal line position 3 [Advanced Tab/Add lines]")
  })
  advanced_lines_horz_col3_val <- reactive({
    validate_generic_function(advanced_obs$advanced_lines_horz_col3,"Horizontal line colour 3 [Advanced Tab/Add lines]")
  })
  
  advanced_footnote_choice_val <- reactive({
    validate_generic_function(advanced_obs$advanced_footnote_choice,"Footnote options [Advanced Tab/Footnote]")
  })
  
  advanced_plot_gap_val <- reactive({
    validate_integer_function(advanced_obs$advanced_plot_gap,"Number of points [Advanced Tab/Plots]")
    
    validate(
      need(advanced_obs$advanced_plot_gap < 5000,paste0("Behave yourself (EASTER EGG)!\nNumber of points should be less than 5,000 [Advanced Tab/Plots]"))
    )
    return(advanced_obs$advanced_plot_gap)
    
  })
  
  advanced_plot_sim_val_sub <- reactive({
    if(is.null(advanced_obs$advanced_plot_sim)){
      return(init_advanced_plot_sim)
    } else {
      return(advanced_obs$advanced_plot_sim)
    }
  })
  advanced_plot_sim_val <- reactive({
    validate_integer_function(advanced_plot_sim_val_sub(),"Number of simulations [Advanced Tab/Plots]")
  })
  
  advanced_plot_width_val <- reactive({
    validate_integer_function(advanced_obs$advanced_plot_width,"Width of plot [Advanced Tab/Plots]")
  })
  
  advanced_plot_height_val <- reactive({
    validate_integer_function(advanced_obs$advanced_plot_height,"Height of plot [Advanced Tab/Plots]")
  })
  
  advanced_plot_curves_val <- reactive({
    validate(
      need(advanced_obs$advanced_plot_curves,paste0("Need to select at least one colour to plot [Advanced Tab/Plots]"))
    )
    return(advanced_obs$advanced_plot_curves)
  })
  
  advanced_plot_size_val <- reactive({
    if(determineEmpty(advanced_obs$advanced_plot_size)){
      advanced_obs$advanced_plot_size <- 1
    } else {
      validate_prec_function(advanced_obs$advanced_plot_size,"Line size [Advanced Tab/Plots]")
    }
    return(advanced_obs$advanced_plot_size)
  })
  
  
  #########################################
  #########################################
  ######   Format input functions:   ######
  #########################################
  #########################################
  
  formatInputConvert <- function(value){
  ### Function to convert input onto scale for analysis (if applicable)
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal) && study_comparison_type_val()==label_study_abs){
      ### Normal absolute difference:
      return(value)
    } else if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal) && study_comparison_type_val()==label_study_perc){
      ### Normal percentage difference:
      return(formatPercDiffNorm(value,design_log_val()))
    } else if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal) && study_comparison_type_val()==label_study_ratio){
      ### Normal ratio difference:
      return(formatRatioNorm(value,design_log_val()))
    } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      ### Binomial difference:
      return(formatPercDiffBin(value))
    }
  }

  
  #########################################
  #########################################
  ######   Dynamic UI for Outputs:   ######
  #########################################
  #########################################
  
  output$outputUI_start <- renderUI({
    if(!is.null(study_obs$study_ocType)){
      if(study_ocType_val()==label_study_conv){
        ### Conventional OC output:
        optList4 <- tabPanel("Summary Table",h3("Summary of key points of interest"),br(),
                             downloadButton("download_table_summary_key","Download"),br(),
                             br(),tableOutput("table_summary_key"),br(),
                             h4("Use drop-downs below to filter table (or download as csv and open in excel)"),br(),
                             uiOutput("outputUI_table_summary_key"))
        optList5 <- tabPanel("Options Summary",h3("Summary of options used to create curves"),br(),
                             downloadButton("download_table_summary_options","Download"),br(),br(),tableOutput("table_summary_options"))
        if(decision_nCriteria_val()==1){
          optList1 <- tabPanel("OC Curves",h3(textOutput("text_1dec_c1_text")),br(),
                               plotOutput("plot_OC_1dec_C1",width=advanced_plot_width_val(),height=advanced_plot_height_val()))
          optList6 <- tabPanel("Data Downloads",h3("1. Data behind main OC curve:"),
                               downloadButton("download_data_c1","Download"))
          ### Create tab panel:
          tabsetPanel(type = "tabs",optList1,optList4,optList5,optList6)
        } else if(decision_nCriteria_val()==2){
          optList1 <- tabPanel("OC Curves",h3(textOutput("text_2dec_c1_text")),h3(textOutput("text_2dec_c2_text")),
                               h3(htmlOutput("text_warning_prec")),
                               plotOutput("plot_OC_2dec",width=advanced_plot_width_val(),height=advanced_plot_height_val()))
          optList2 <- tabPanel("C1 Curve",h3(textOutput("text_1dec_c1_text")),br(),
                               plotOutput("plot_OC_1dec_C1",width=advanced_plot_width_val(),height=advanced_plot_height_val()))
          optList3 <- tabPanel("C2 Curve",h3(textOutput("text_1dec_c2_text")),br(),
                               plotOutput("plot_OC_1dec_C2",width=advanced_plot_width_val(),height=advanced_plot_height_val()))
          optList6 <- tabPanel("Data Downloads",h3("1. Data behind main OC curve:"),
                               downloadButton("download_data_main","Download"),
                               h3("2. Data behind C1 Curve:"),
                               downloadButton("download_data_c1","Download"),
                               h3("3. Data behind C2 Curve:"),
                               downloadButton("download_data_c2","Download"))
          ### Create tab panel:
          tabsetPanel(type = "tabs",optList1,optList2,optList3,optList4,optList5,optList6)
        }
      } else {
        return(NULL)
      }
    }
  })
  
  ### Interactive key summary table control panel:
  output$outputUI_table_summary_key <- renderUI({
    keyTable <- eRec_table_key()
    tempDeltaLabel <- names(keyTable)[grepl("Delta",names(keyTable))]
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
      tempList <- list(selectInput("keyTable_input_sigma",paste0(key_lab_sigma,":"),c(key_lab_table_all,sort(unique(as.numeric(keyTable[,key_lab_sigma]))))),
                       selectInput("keyTable_input_se",paste0(key_lab_se,":"),c(key_lab_table_all,sort(unique(as.numeric(keyTable[,key_lab_se]))))))
    } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      tempList <- list(selectInput("keyTable_input_probref",paste0(key_lab_probref,":"),c(key_lab_table_all,sort(unique(as.numeric(keyTable[,key_lab_probref]))))))
    }
    return(list(selectInput("keyTable_input_graph",paste0(key_lab_graph,":"),c(key_lab_table_all,unique(as.character(keyTable[,key_lab_graph])))),
                selectInput("keyTable_input_ntreat",paste0(key_lab_ntreat,":"),c(key_lab_table_all,sort(unique(as.numeric(keyTable[,key_lab_ntreat]))))),
                selectInput("keyTable_input_ncontrol",paste0(key_lab_ncontrol,":"),c(key_lab_table_all,sort(unique(as.numeric(keyTable[,key_lab_ncontrol]))))),
                tempList,
                selectInput("keyTable_input_delta",paste0(tempDeltaLabel,":"),c(key_lab_table_all,sort(unique(as.numeric(keyTable[,tempDeltaLabel]))))),
                selectInput("keyTable_input_go",paste0(key_lab_go,":"),c(key_lab_table_all,sort(unique(as.numeric(keyTable[,key_lab_go]))))),
                selectInput("keyTable_input_discuss",paste0(key_lab_discuss,":"),c(key_lab_table_all,sort(unique(as.numeric(keyTable[,key_lab_discuss]))))),
                selectInput("keyTable_input_stop",paste0(key_lab_stop,":"),c(key_lab_table_all,sort(unique(as.numeric(keyTable[,key_lab_stop]))))))
           )

    })
  
  
  #################################
  #################################
  ######   Output objects:   ######
  #################################
  #################################
  
  #################
  # TEXT objects: #
  #################
  output$text_2dec_c1_text <- renderText({
    eRec_text_decCrit1()
  })
  output$text_2dec_c2_text <- renderText({
    eRec_text_decCrit2()
  })
  ### For some reason SHINY doesn't like to access the same render object twice so have to re-name if referencing more than once:
  output$text_1dec_c1_text <- renderText({
    eRec_text_decCrit1()
  })
  output$text_1dec_c2_text <- renderText({
    eRec_text_decCrit2()
  })
  
  output$text_warning_prec <- renderText({
    eRec_text_warning()
  })
  
  ###################
  # FIGURE objects: #
  ###################
  output$plot_OC_2dec <- renderPlot({
    warn <- determine_minimum_prec()
    if(warn & study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      return(NULL)
    } else {
      return(eRec_plot_2_0()$outPlot)
    }
  })
  
  output$plot_OC_1dec_C1 <- renderPlot({
    eRec_plot_1_1()$outPlot
  })
  
  output$plot_OC_1dec_C2 <- renderPlot({
    eRec_plot_1_2()$outPlot
  })
  
  ##################
  # TABLE objects: #
  ##################
  output$table_summary_options <- renderTable({
    sumTable <- eRec_table_options()
    head(sumTable,n=nrow(sumTable))
  })
  
  output$table_summary_key <- renderTable({
    keyTable <- eRec_table_key()
    tempDeltaLabel <- names(keyTable)[grepl("Delta",names(keyTable))]
    # Limit printed key summary table based on user-options:
    keyTable <- key_table_utility(keyTable,input$keyTable_input_graph,key_lab_graph)
    keyTable <- key_table_utility(keyTable,input$keyTable_input_ntreat,key_lab_ntreat)
    keyTable <- key_table_utility(keyTable,input$keyTable_input_ncontrol,key_lab_ncontrol)
    keyTable <- key_table_utility(keyTable,input$keyTable_input_probref,key_lab_probref)
    keyTable <- key_table_utility(keyTable,input$keyTable_input_sigma,key_lab_sigma)
    keyTable <- key_table_utility(keyTable,input$keyTable_input_se,key_lab_se)
    keyTable <- key_table_utility(keyTable,input$keyTable_input_delta,tempDeltaLabel)
    keyTable <- key_table_utility(keyTable,input$keyTable_input_go,key_lab_go)
    keyTable <- key_table_utility(keyTable,input$keyTable_input_discuss,key_lab_discuss)
    keyTable <- key_table_utility(keyTable,input$keyTable_input_stop,key_lab_stop)
    head(keyTable,n=nrow(keyTable))
  })
  
  
  #####################
  # DOWNLOAD objects: #
  #####################
  ### Main OC downloads:
  output$download_data_main <- downloadHandler(
    filename="Data_main.csv",
    content = function(file) {
      if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
        tempPlotData <- eRec_data_normal_plot()$data_2_0
      } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
        tempPlotData <- eRec_data_binomial_plot()$data_2_0
      }
      write.csv(tempPlotData,file,row.names=F,quote=T)
    }
  )
  output$download_data_c1 <- downloadHandler(
    filename="Data_C1.csv",
    content = function(file) {
      if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
        tempPlotData <- eRec_data_normal_plot()$data_1_1
      } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
        tempPlotData <- eRec_data_binomial_plot()$data_1_1
      }
      write.csv(tempPlotData,file,row.names=F,quote=T)
    }
  )  
  output$download_data_c2 <- downloadHandler(
    filename="Data_C2.csv",
    content = function(file) {
      if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
        tempPlotData <- eRec_data_normal_plot()$data_1_2
      } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
        tempPlotData <- eRec_data_binomial_plot()$data_1_2
      }
      write.csv(tempPlotData,file,row.names=F,quote=T)
    }
  )

  ### Summary tables:
  output$download_table_summary_key <- downloadHandler(
    filename="Summary_Table_Key points.csv",
    content = function(file) {
      write.csv(eRec_table_key(),file,row.names=F,quote=T)
    }
  )
  output$download_table_summary_options <- downloadHandler(
    filename="Summary_Table_Options.csv",
    content = function(file) {
      write.csv(eRec_table_options(),file,row.names=F,quote=T)
    }
  )
  

  
  #########################################
  #########################################
  ######   Event Reactive Objects:   ######
  #########################################
  #########################################
  
  ###########
  # Figures #
  ###########
  eRec_plot_2_0 <- reactive({
    ocPlot <- create_OC_Plot(2,0)
    outPlot <- create_OC_dec_Text(ocPlot,2)
    return(list(outPlot=outPlot))
  })
  
  eRec_plot_1_1 <- reactive({
    ocPlot <- create_OC_Plot(1,1)
    outPlot <- create_OC_dec_Text(ocPlot,1,1)
    return(list(outPlot=outPlot))
  })
  
  eRec_plot_1_2 <- reactive({
    ocPlot <- create_OC_Plot(1,2)
    outPlot <- create_OC_dec_Text(ocPlot,1,2)
    return(list(outPlot=outPlot))
  })
  
  ############
  # DATASETS #
  ############
  ### Conventional OC, normal outcomes:
  eRec_data_normal_plot <- reactive({
    if(decision_nCriteria_val()==2){
      data_2_0 <- recD_normal_2_0()
      data_1_1 <- recD_normal_1_1()
      data_1_2 <- recD_normal_1_2()
      return(list(data_2_0=data_2_0,data_1_1=data_1_1,data_1_2=data_1_2))
    } else if(decision_nCriteria_val()==1){
      data_1_1 <- recD_normal_1_1()
      return(list(data_1_1=data_1_1))
    }
  })
  
  ### Conventional OC, binomial outcomes:
  eRec_data_binomial_plot <- reactive({
    if(decision_nCriteria_val()==2){
      data_2_0 <- recD_binomial_2_0()
      data_1_1 <- recD_binomial_1_1()
      data_1_2 <- recD_binomial_1_2()
      return(list(data_2_0=data_2_0,data_1_1=data_1_1,data_1_2=data_1_2))
    } else if(decision_nCriteria_val()==1){
      data_1_1 <- recD_binomial_1_1()
      return(list(data_1_1=data_1_1))
    }
  })
  
  ##########
  # TABLES #
  ##########
  eRec_table_options <- reactive({
    return(create_summary_options_table())
  })
  
  eRec_table_key <- reactive({
    return(create_summary_key_table())
  })
  
  ########
  # TEXT #
  ########
  ### Decision criteria text:
  eRec_text_decCrit1 <- reactive({
    return(create_decision_criteria_text("C1",decision_direction_val(),decision_c1_tv_val(),decision_c1_sig_val(),study_comparison_val(),study_comparison_type_val()))
  })
  
  eRec_text_decCrit2 <- reactive({
    return(create_decision_criteria_text("C2",decision_direction_val(),decision_c2_tv_val(),decision_c2_sig_val(),study_comparison_val(),study_comparison_type_val()))
  })
  ### Warning (precision) text:
  eRec_text_warning <- reactive({
    warn <- determine_minimum_prec()
    if(warn & study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
      return(HTML(paste0("WARNING: Given decision criteria and precision, C2 criteria has become redundant<br/>",
                         "<br/>'OC Curves' tab should be used with caution<br/>")))
    } else if(warn & study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      return(HTML(paste0("WARNING: Given decision criteria and precision, C2 criteria has become redundant<br/>",
                         "<br/>'OC Curves' tab has not been generated - simulation should be used instead<br/>")))
    } else {
      return(NULL)
    }
  })
  
  
  ###################################
  ###################################
  ######   Reactive datasts:   ######
  ###################################
  ###################################
  ### NOTE: For whatever reason can't nest these reactive commands inside an event reactive without it re-calculating regardless of no changes
  
  ### Conventional OC, normal outcomes:
  recD_normal_2_0 <- reactive({
    tempPrec <- create_prec_table()
    tempDF <- getDFinfo()
    return(normalMeansPlotData(tempPrec,tempDF,2,0))
  })
  recD_normal_1_1 <- reactive({
    tempPrec <- create_prec_table()
    tempDF <- getDFinfo()
    return(normalMeansPlotData(tempPrec,tempDF,1,1))
  })
  recD_normal_1_2 <- reactive({
    tempPrec <- create_prec_table()
    tempDF <- getDFinfo()
    return(normalMeansPlotData(tempPrec,tempDF,1,2))
  })
  
  ### Conventional OC, binomial outcomes:
  recD_binomial_2_0 <- reactive({
    tempPrec <- create_prec_table()
    return(binomialMeansPlotData(tempPrec,2,0))
  })
  recD_binomial_1_1 <- reactive({
    tempPrec <- create_prec_table()
    return(binomialMeansPlotData(tempPrec,1,1))
  })
  recD_binomial_1_2 <- reactive({
    tempPrec <- create_prec_table()
    return(binomialMeansPlotData(tempPrec,1,2))
  })
  

  ###############################################
  ###############################################
  ######   Functions for output objects:   ######
  ###############################################
  ###############################################
  
  create_OC_Plot <- function(nDec=NULL,tvInfo=NULL){
    if(study_ocType_val()==label_study_conv){
      ### Conventional design:
      return(create_conv_plot(nDec,tvInfo))
    } else {
      ### Interim analysis:
      return(NULL)
    }
  }
  
  create_conv_plot <- function(nDec=NULL,tvInfo=NULL){
    ### Generate plot data:
    ### TESTING - it seems to be something to do with this function that is causing headaches moving from 1 to 2 or vice versus
    #             decision criteria. Presumably something to do with the nature of being a function, but not sure
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
      tempPlotData <- eRec_data_normal_plot()[[paste0("data_",nDec,"_",tvInfo)]]
    } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      tempPlotData <- eRec_data_binomial_plot()[[paste0("data_",nDec,"_",tvInfo)]]
    }
    
    ### Format line lines for legend:
    tempPlotData$variable <- as.character(tempPlotData$variable)
    if(nDec==2){
      tempPlotData[tempPlotData$variable==label_decision_2_both,"variable"] <- advanced_legend_label_dec_2_both_val()
      tempPlotData[tempPlotData$variable==label_decision_2_one,"variable"] <- advanced_legend_label_dec_2_one_val()
      tempPlotData[tempPlotData$variable==label_decision_2_none,"variable"] <- advanced_legend_label_dec_2_none_val()
      if(length(unique(c(advanced_legend_label_dec_2_both_val(),advanced_legend_label_dec_2_one_val(),advanced_legend_label_dec_2_none_val())))!=3){
        validate(need(NULL,paste0("Can't have the same label twice for plotting - see two decision criteria plot labels [Advanced Tab/Legend]")))  
      }
      tempPlotData$variable <- factor(tempPlotData$variable,levels=c(advanced_legend_label_dec_2_both_val(),advanced_legend_label_dec_2_one_val(),advanced_legend_label_dec_2_none_val()))
    } else {
      tempPlotData[tempPlotData$variable==label_decision_1_one,"variable"] <- advanced_legend_label_dec_1_one_val()
      tempPlotData[tempPlotData$variable==label_decision_1_none,"variable"] <- advanced_legend_label_dec_1_none_val()
      if(length(unique(c(advanced_legend_label_dec_1_one_val(),advanced_legend_label_dec_1_none_val())))!=2){
        validate(need(NULL,paste0("Can't have the same label twice for plotting - see one decision criteria plot labels [Advanced Tab/Legend]")))  
      }
      tempPlotData$variable <- factor(tempPlotData$variable,levels=c(advanced_legend_label_dec_1_one_val(),advanced_legend_label_dec_1_none_val()))
    }

    ### Plot options (curves to plot):
    lcolours <- c("green","orange","red")[as.numeric(advanced_plot_curves_val())]
    if(nDec==2){
      in_tv <- decision_c2_tv_val()
      selection <- c(advanced_legend_label_dec_2_both_val(),advanced_legend_label_dec_2_one_val(),advanced_legend_label_dec_2_none_val())[as.numeric(advanced_plot_curves_val())]
      tempPlotData <- tempPlotData[tempPlotData$variable %in% selection,]
    } else {
      in_tv <- getTVinfo(tvInfo)$tv
      selection <- c(advanced_legend_label_dec_1_one_val(),advanced_legend_label_dec_1_none_val())[c("1" %in% advanced_plot_curves_val(),"3" %in% advanced_plot_curves_val())]
      tempPlotData <- tempPlotData[tempPlotData$variable %in% selection,]
      lcolours <- lcolours[lcolours %in% c("green","red")]
      if(length(lcolours)==0){
        lcolours <- c("green","red")
      }
    }
    
    ### Plot options (style of curves):
    multDec <- create_plot_options_curveStyle()
    
    ### Create plot:
    tempPlot <- createPlot(tempPlotData,in_tv,
                           c(plot_title_val(),advanced_xaxis_title_val(),advanced_yaxis_title_val(),advanced_legend_title_val()),
                           c(plot_xlow_val(),plot_xupp_val(),advanced_xaxis_break_val()),
                           c(advanced_yaxis_break_val(),advanced_yaxis_low_val(),advanced_yaxis_upp_val()),
                           lcolours,create_vert_lines_info(),create_horz_lines_info(),
                           multDec[[1]],multDec[[2]],multDec[[3]],
                           c(advanced_plot_size_val()))
    return(tempPlot)
  }

  create_prec_table <- function(){
    outTable <- NULL
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
      outTable <- create_normal_prec_table()
    } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      outTable <- create_binomial_prec_table()
    }
    return(outTable)
  }

  create_normal_prec_table <- function(){
    seTable <- data.frame(N1=NULL,N2=NULL,Sigma=NULL,SE=NULL,stringsAsFactors=F)
    tempSE <- NULL
    if(design_precision_val()==2){
      for(i in 1:design_se_n_val()){
        if(study_comparison_val()==label_study_2Normal){
          tempSE <- normal2MeansSE(design_precision_val(),NULL,NULL,NULL,get(paste0("design_se_",i,"_val"))())
        } else if(study_comparison_val()==label_study_1Normal){
          tempSE <- normal1MeansSE(design_precision_val(),NULL,NULL,get(paste0("design_se_",i,"_val"))())
        }
        seTable <- rbind(seTable,data.frame(N1=NA,N2=NA,Sigma=NA,SE=tempSE,stringsAsFactors=F))
      }
    } else {
      for(i in 1:design_n1_n_val()){
        for(j in 1:design_sigma_n_val()){
          if(study_comparison_val()==label_study_1Normal){
            tempSE <- normal1MeansSE(design_precision_val(),get(paste0("design_n1_",i,"_val"))(),get(paste0("design_sigma_",j,"_val"))(),NULL)
            seTable <- rbind(seTable,data.frame(N1=get(paste0("design_n1_",i,"_val"))(),N2=NA,Sigma=get(paste0("design_sigma_",j,"_val"))(),SE=tempSE,stringsAsFactors=F))
          } else {
            if(study_studyDesign_val()==label_study_Parallel && !design_equalN_val()){
              for(k in 1:design_n2_n_val()){
                tempSE <- normal2MeansSE(design_precision_val(),get(paste0("design_n1_",i,"_val"))(),get(paste0("design_n2_",k,"_val"))(),get(paste0("design_sigma_",j,"_val"))(),NULL)
                seTable <- rbind(seTable,data.frame(N1=get(paste0("design_n1_",i,"_val"))(),N2=get(paste0("design_n2_",k,"_val"))(),Sigma=get(paste0("design_sigma_",j,"_val"))(),SE=tempSE,stringsAsFactors=F))
              }
            } else {
              tempSE <- normal2MeansSE(design_precision_val(),get(paste0("design_n1_",i,"_val"))(),get(paste0("design_n1_",i,"_val"))(),get(paste0("design_sigma_",j,"_val"))(),NULL)
              seTable <- rbind(seTable,data.frame(N1=get(paste0("design_n1_",i,"_val"))(),N2=get(paste0("design_n1_",i,"_val"))(),Sigma=get(paste0("design_sigma_",j,"_val"))(),SE=tempSE,stringsAsFactors=F))
            }
          }
        }
      }
    }
    return(seTable)
  }
  
  create_binomial_prec_table <- function(){
    precTable <- data.frame(N1=NULL,N2=NULL,ProbRef=NULL,stringsAsFactors=F)
    if(design_precision_val()==3){
      for(i in 1:design_n1_n_val()){
        for(j in 1:design_probRef_n_val()){
          if(study_studyDesign_val()==label_study_Parallel && !design_equalN_val()){
            for(k in 1:design_n2_n_val()){
              tempPrec <- binomialMeansPrec(study_studyDesign_val(),design_precision_val(),design_equalN_val(),get(paste0("design_n1_",i,"_val"))(),get(paste0("design_n2_",k,"_val"))(),get(paste0("design_probRef_",j,"_val_for"))())
              precTable <- rbind(precTable,data.frame(N1=tempPrec[1],N2=tempPrec[2],ProbRef=tempPrec[3],stringsAsFactors=F))
            }
          } else {
            tempPrec <- binomialMeansPrec(study_studyDesign_val(),design_precision_val(),design_equalN_val(),get(paste0("design_n1_",i,"_val"))(),NULL,get(paste0("design_probRef_",j,"_val_for"))())
            precTable <- rbind(precTable,data.frame(N1=tempPrec[1],N2=tempPrec[2],ProbRef=tempPrec[3],stringsAsFactors=F))
          }
        }
      }
      if(study_comparison_val()==label_study_2Binomial){
        precTable <- cbind(Outcomes=2,precTable)
      } else if(study_comparison_val()==label_study_1Binomial){
        precTable <- cbind(Outcomes=1,precTable)
      }
    } else {
      validate(need(NULL,paste0("Need to select precision option [Design Tab]")))
    }
    return(precTable)
  }
  
  normalMeansPlotData <- function(precTable,tempDF,nDec,tvInfo=NULL){
    withProgress(message="Generating data",detail="",value=0,{
      if(nDec==2){
        tv1 <- decision_c1_tv_val_for()
        sig1 <- decision_c1_sig_val()
        tv2 <- decision_c2_tv_val_for()
        sig2 <- decision_c2_sig_val()
      } else {
        tvInfo <- getTVinfo_for(tvInfo)
        tv1 <- tvInfo$tv
        sig1 <- tvInfo$sig
        tv2 <- NULL
        sig2 <- NULL
      }
      outTable <- data.frame(N1=NULL,N2=NULL,Sigma=NULL,SE=NULL,delta=NULL,variable=NULL,value=NULL,stringsAsFactors=F)
      for(i in 1:nrow(precTable)){
        setProgress(0.10,detail=paste0("Starting curve set ",i," of ",nrow(precTable)))
        tempTable <- normalMeansData(as.numeric(precTable[i,"SE"]),tempDF,nDec,decision_direction_val(),tv1,sig1,tv2,sig2,
                                      plot_xlow_val_for(),plot_xupp_val_for(),advanced_plot_gap_val())
        tempTable <- cbind(N1=precTable[i,"N1"],N2=precTable[i,"N2"],Sigma=precTable[i,"Sigma"],tempTable)
        outTable <- rbind(outTable,tempTable)
      }
      setProgress(0.90,detail="Completed creating data")
      
      #Format results:
      outTable <- formatPlotData(outTable,"Normal",study_comparison_type_val(),design_log_val())
    })
    return(outTable)
  }
    
  binomialMeansPlotData <- function(precTable,nDec,tvInfo=NULL){
    withProgress(message="Generating data",detail="",value=0,{
      if(nDec==2){
        tv1 <- decision_c1_tv_val_for()
        sig1 <- decision_c1_sig_val()
        tv2 <- decision_c2_tv_val_for()
        sig2 <- decision_c2_sig_val()
      } else {
        tvInfo <- getTVinfo_for(tvInfo)
        tv1 <- tvInfo$tv
        sig1 <- tvInfo$sig
        tv2 <- NULL
        sig2 <- NULL
      }
      outTable <- data.frame(N1=NULL,N2=NULL,ProbRef=NULL,delta=NULL,variable=NULL,value=NULL,stringsAsFactors=F)
      for(i in 1:nrow(precTable)){
        setProgress(0.10,detail=paste0("Starting curve set ",i," of ",nrow(precTable)))
        if(design_bin_method_val()==1){
          ### Use formula with normal approximation:
          tempDF <- getDFinfo()
          tempTable <- binomialMeansData(precTable[i,],tempDF,nDec,decision_direction_val(),tv1,sig1,tv2,sig2,
                                         plot_xlow_val_for(),plot_xupp_val_for(),advanced_plot_gap_val())
        } else if(design_bin_method_val()==2){
          ### Use simulation method:
          validate(NULL,"WIP!")
          tempTable <- binomial2MeansData_Sim(as.numeric(precTable[i,"N1"]),as.numeric(precTable[i,"N2"]),as.numeric(precTable[i,"ProbRef"]),
                                              tempDF,nDec,decision_direction_val(),tv1,sig1,tv2,sig2,
                                              plot_xlow_val_for(),plot_xupp_val_for(),advanced_plot_gap_val(),
                                              advanced_plot_sim_val(),design_bin_test_val())
        }
        outTable <- rbind(outTable,tempTable)
      }
      setProgress(0.90,detail="Completed creating data")
      #Format results:
      outTable <- formatPlotData(outTable,"Binomial")
    })
    return(outTable)
  }
  
  create_summary_options_table <- reactive({
    ### Study options:
    sumTable <- data.frame(VarName=c("study_comparison"),Value=c(study_comparison_val()),stringsAsFactors=F)
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
      sumTable <- rbind(sumTable,data.frame(VarName=c("study_comparison_type"),Value=c(study_comparison_type_val()),stringsAsFactors=F))
    }
    sumTable <- rbind(sumTable,data.frame(VarName=c("study_ocType","study_studyDesign"),
                                          Value=c(study_ocType_val(),study_studyDesign_val()),stringsAsFactors=F))
    
    ### Criteria options:
    sumTable <- rbind(sumTable,data.frame(VarName=c("decision_nCriteria","decision_direction","decision_c1_tv","decision_c1_sig"),
                                          Value=c(decision_nCriteria_val(),decision_direction_val(),
                                                  decision_c1_tv_val(),decision_c1_sig_val()),stringsAsFactors=F))
    if(decision_nCriteria_val()==2){
      sumTable <- rbind(sumTable,data.frame(VarName=c("decision_c2_tv","decision_c2_sig"),Value=c(decision_c2_tv_val(),decision_c2_sig_val()),stringsAsFactors=F))
    }
    
    ### Design options:
    sumTable <- rbind(sumTable,data.frame(VarName=c("design_precision"),Value=c(design_precision_val()),stringsAsFactors=F))
    if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      sumTable <- rbind(sumTable,data.frame(VarName=c("design_bin_method"),Value=c(design_bin_method_val()),stringsAsFactors=F))
    }
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal) && (study_comparison_type_val() %in% c(label_study_perc,label_study_ratio))){
      sumTable <- rbind(sumTable,data.frame(VarName=c("design_log"),Value=c(design_log_val()),stringsAsFactors=F))
    }
    if(design_precision_val()==2){
      sumTable <- create_summary_multiple_values(sumTable,"design_se_")
    } else {
      if(study_studyDesign_val()==label_study_Parallel){
        sumTable <- rbind(sumTable,data.frame(VarName=c("design_equalN"),Value=c(design_equalN_val()),stringsAsFactors=F))
        if(design_equalN_val()){
          sumTable <- create_summary_multiple_values(sumTable,"design_n1_")
          if(study_comparison_val()==label_study_2Normal){
            sumTable <- create_summary_multiple_values(sumTable,"design_sigma_")
          } else if(study_comparison_val()==label_study_2Binomial){
            sumTable <- create_summary_multiple_values(sumTable,"design_probRef_")
          }
        } else {
          sumTable <- create_summary_multiple_values(sumTable,"design_n1_")
          sumTable <- create_summary_multiple_values(sumTable,"design_n2_")
          if(study_comparison_val()==label_study_2Normal){
            sumTable <- create_summary_multiple_values(sumTable,"design_sigma_")
          } else if(study_comparison_val()==label_study_2Binomial){
            sumTable <- create_summary_multiple_values(sumTable,"design_probRef_")
          }
        }
      } else {
        sumTable <- create_summary_multiple_values(sumTable,"design_n1_")
        if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
          sumTable <- create_summary_multiple_values(sumTable,"design_sigma_")
        } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
          sumTable <- create_summary_multiple_values(sumTable,"design_probRef_")
        }
      } 
    }
    ### Normal approximation - TESTING with simulation based methods for e.g. binomial will need this to be updated:
    sumTable <- rbind(sumTable,data.frame(VarName=c("design_normApprox","design_df"),
                                          Value=c(design_normApprox_val(),as.character(getDFinfo())),
                                                  stringsAsFactors=F))
    
    ### Output options:
    sumTable <- rbind(sumTable,data.frame(VarName=c("plot_title","plot_userID","plot_xlow","plot_xupp"),
                                          Value=c(plot_title_val(),plot_userID_val(),plot_xlow_val(),plot_xupp_val()),stringsAsFactors=F))
    
    ### Advanced options:
    sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_yaxis_title","advanced_yaxis_break",
                                                    "advanced_yaxis_low","advanced_yaxis_upp",
                                                    "advanced_xaxis_title","advanced_xaxis_break",
                                                    "advanced_legend_title"),
                                          Value=c(advanced_yaxis_title_val(),advanced_yaxis_break_val(),
                                                  advanced_yaxis_low_val(),advanced_yaxis_upp_val(),
                                                  advanced_xaxis_title_val(),advanced_xaxis_break_val(),
                                                  advanced_legend_title_val()),stringsAsFactors=F))
    if(decision_nCriteria_val()==2){
      sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_legend_label_dec_2_both","advanced_legend_label_dec_2_one",
                                                      "advanced_legend_label_dec_2_none"),
                                            Value=c(advanced_legend_label_dec_2_both_val(),advanced_legend_label_dec_2_one_val(),
                                                    advanced_legend_label_dec_2_none_val()),stringsAsFactors=F))
    }
    sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_legend_label_dec_1_one","advanced_legend_label_dec_1_none"),
                                          Value=c(advanced_legend_label_dec_1_one_val(),advanced_legend_label_dec_1_none_val()),stringsAsFactors=F))
    
    if(design_precision_val()==2){
      if(as.numeric(design_se_n_val())>1){
        sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_legend_title_se"),
                                              Value=c(advanced_legend_title_se_val()),stringsAsFactors=F))
      }
    } else {
      if(as.numeric(design_n1_n_val())>1){
        sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_legend_title_n1"),
                                              Value=c(advanced_legend_title_n1_val()),stringsAsFactors=F))
      }
      if(study_studyDesign_val()==label_study_Parallel && !design_equalN_val()){
        if(!is.null(design_n2_n_val()) && as.numeric(design_n2_n_val())>1){
          sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_legend_title_n2"),
                                                Value=c(advanced_legend_title_n2_val()),stringsAsFactors=F))
        }
      }
      if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
        if(as.numeric(design_sigma_n_val())>1){
          sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_legend_title_sigma"),
                                                Value=c(advanced_legend_title_sigma_val()),stringsAsFactors=F))
        }
      } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
        if(as.numeric(design_probRef_n_val())>1){
          sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_legend_title_probRef"),
                                                Value=c(advanced_legend_title_probRef_val()),stringsAsFactors=F))
        }
      }
    }
    sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_lines_vert_number"),Value=c(advanced_lines_vert_number_val()),stringsAsFactors=F))
    if(advanced_lines_vert_number_val() != 0){
      for(i in 1:advanced_lines_vert_number_val()){
        sumTable <- rbind(sumTable,data.frame(VarName=c(paste0("advanced_lines_vert_pos",i),paste0("advanced_lines_vert_col",i)),
                                              Value=c(get(paste0("advanced_lines_vert_pos",i,"_val"))(),
                                                      get(paste0("advanced_lines_vert_col",i,"_val"))()),stringsAsFactors=F))
      }
    }
    sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_lines_horz_number"),Value=c(advanced_lines_horz_number_val()),stringsAsFactors=F))
    if(advanced_lines_horz_number_val() != 0){
      for(i in 1:advanced_lines_horz_number_val()){
        sumTable <- rbind(sumTable,data.frame(VarName=c(paste0("advanced_lines_horz_pos",i),paste0("advanced_lines_horz_col",i)),
                                              Value=c(get(paste0("advanced_lines_horz_pos",i,"_val"))(),
                                                      get(paste0("advanced_lines_horz_col",i,"_val"))()),stringsAsFactors=F))
      }
    }
    sumTable <- rbind(sumTable,data.frame(VarName=c("advanced_footnote_choice","advanced_plot_gap",
                                                    "advanced_plot_sim","advanced_plot_width",
                                                    "advanced_plot_height","advanced_plot_curves",
                                                    "advanced_plot_size","version"),
                                          Value=c(advanced_footnote_choice_val(),advanced_plot_gap_val(),
                                                  advanced_plot_sim_val(),advanced_plot_width_val(),
                                                  advanced_plot_height_val(),paste(advanced_plot_curves_val(),collapse=","),
                                                  advanced_plot_size_val(),versionNumber),stringsAsFactors=F))
    return(sumTable)
  })
  
  create_summary_key_table <- reactive({
    if(study_ocType_val()==label_study_conv){
      ### Conventional design:
      keyTable <- create_summary_key_table_conv()
    } else {
      ### Interim analysis:
      keyTable <- create_summary_key_table_interim()
    }
    return(keyTable)
  })
  
  create_summary_key_table_conv <- reactive({
    ### Set-up table:
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
      keyTable <- data.frame(Graph=as.character(),N1=as.numeric(),N2=as.numeric(),Sigma=as.numeric(),SE=as.numeric(),Delta=as.numeric(),Prob_Go=as.numeric(),Prob_Discuss=as.numeric(),Prob_Stop=as.numeric(),stringsAsFactors=F)
    } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      keyTable <- data.frame(Graph=as.character(),N1=as.numeric(),N2=as.numeric(),ProbRef=as.numeric(),Delta=as.numeric(),Prob_Go=as.numeric(),Prob_Discuss=as.numeric(),Prob_Stop=as.numeric(),stringsAsFactors=F)
    }
    
    ### Determine study characteristics to loop through:
    tempPrec <- create_prec_table()
    tempDF <- getDFinfo()
    
    ### Determine deltas to loop through:
    delta <- c(0,decision_c1_tv_val_for())
    #50% and 80% points (C1 criteria):
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
      delta <- c(delta,normalMeansPowerPoint(decision_c1_tv_val_for(),decision_c1_sig_val(),decision_direction_val(),as.numeric(tempPrec$SE),tempDF,0.5))
      delta <- c(delta,normalMeansPowerPoint(decision_c1_tv_val_for(),decision_c1_sig_val(),decision_direction_val(),as.numeric(tempPrec$SE),tempDF,0.8))
    } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      delta <- c(delta,binomialMeansPowerPoint_All(0.5,tempPrec,tempDF,decision_direction_val(),decision_c1_tv_val_for(),decision_c1_sig_val(),alg_binomial_power_gap,alg_binomial_power_step))
      delta <- c(delta,binomialMeansPowerPoint_All(0.8,tempPrec,tempDF,decision_direction_val(),decision_c1_tv_val_for(),decision_c1_sig_val(),alg_binomial_power_gap,alg_binomial_power_step))
    }
    
    #Include lines of interest:
    if(advanced_lines_vert_number_val() != 0){
      for(i in 1:advanced_lines_vert_number_val()){
        delta <- c(delta,formatInputConvert(get(paste0("advanced_lines_vert_pos",i,"_val"))()))
      }
    }
    
    if(advanced_lines_horz_number_val() != 0){
      for(i in 1:advanced_lines_horz_number_val()){
        if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
          delta <- c(delta,normalMeansPowerPoint(decision_c1_tv_val_for(),decision_c1_sig_val(),decision_direction_val(),as.numeric(tempPrec$SE),tempDF,get(paste0("advanced_lines_horz_pos",i,"_val"))()/100))
        } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
          delta <- c(delta,binomialMeansPowerPoint_All(get(paste0("advanced_lines_horz_pos",i,"_val"))()/100,tempPrec,tempDF,decision_direction_val(),decision_c1_tv_val_for(),decision_c1_sig_val(),alg_binomial_power_gap,alg_binomial_power_step))
        }
      }
    }
    
    if(decision_nCriteria_val()==2){
      ### 2 decision criteria selected:
      delta <- c(delta,decision_c2_tv_val_for())
      #50% and 80% points (C2 criteria):
      if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
        delta <- c(delta,normalMeansPowerPoint(decision_c2_tv_val_for(),decision_c2_sig_val(),decision_direction_val(),as.numeric(tempPrec$SE),tempDF,0.5))
        delta <- c(delta,normalMeansPowerPoint(decision_c2_tv_val_for(),decision_c2_sig_val(),decision_direction_val(),as.numeric(tempPrec$SE),tempDF,0.8))
      } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
        delta <- c(delta,binomialMeansPowerPoint_All(0.5,tempPrec,tempDF,decision_direction_val(),decision_c2_tv_val_for(),decision_c2_sig_val(),alg_binomial_power_gap,alg_binomial_power_step))
        delta <- c(delta,binomialMeansPowerPoint_All(0.8,tempPrec,tempDF,decision_direction_val(),decision_c2_tv_val_for(),decision_c2_sig_val(),alg_binomial_power_gap,alg_binomial_power_step))
      }
      #Include lines of interest:
      if(advanced_lines_horz_number_val() != 0){
        for(i in 1:advanced_lines_horz_number_val()){
          if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
            delta <- c(delta,normalMeansPowerPoint(decision_c2_tv_val_for(),decision_c2_sig_val(),decision_direction_val(),as.numeric(tempPrec$SE),tempDF,get(paste0("advanced_lines_horz_pos",i,"_val"))()/100))
          } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
            delta <- c(delta,binomialMeansPowerPoint_All(get(paste0("advanced_lines_horz_pos",i,"_val"))()/100,tempPrec,tempDF,decision_direction_val(),decision_c2_tv_val_for(),decision_c2_sig_val(),alg_binomial_power_gap,alg_binomial_power_step))
          }
        }
      }
    }
    
    ### Loop through delta and determine probabilities of decisions:
    delta <- sort(unique(delta))
    for(i in 1:length(delta)){
      #For each delta:
      for(j in 1:nrow(tempPrec)){
        #For each study characteristic:
        if(decision_nCriteria_val()==2){
          ### Both decision criteria:
          if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
            ### C1 & C2:
            keyTable <- create_summary_key_table_conv_normal(keyTable,tempPrec[j,],tempDF,2,decision_direction_val(),decision_c1_tv_val_for(),decision_c1_sig_val(),
                                                        decision_c2_tv_val_for(),decision_c2_sig_val(),
                                                        delta[i],"OC Curves")
            ### Criteria C1:
            keyTable <- create_summary_key_table_conv_normal(keyTable,tempPrec[j,],tempDF,1,decision_direction_val(),decision_c1_tv_val_for(),decision_c1_sig_val(),NULL,NULL,
                                                        delta[i],"C1 Curve")
            ### Criteria C2:
            keyTable <- create_summary_key_table_conv_normal(keyTable,tempPrec[j,],tempDF,1,decision_direction_val(),decision_c2_tv_val_for(),decision_c2_sig_val(),NULL,NULL,
                                                        delta[i],"C2 Curve")
          } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
            ### C1 & C2:
            keyTable <- create_summary_key_table_conv_binomial(keyTable,tempPrec[j,],tempDF,2,decision_direction_val(),decision_c1_tv_val_for(),decision_c1_sig_val(),
                                                          decision_c2_tv_val_for(),decision_c2_sig_val(),
                                                          delta[i],"OC Curves")
            ### Criteria C1:
            keyTable <- create_summary_key_table_conv_binomial(keyTable,tempPrec[j,],tempDF,1,decision_direction_val(),decision_c1_tv_val_for(),decision_c1_sig_val(),NULL,NULL,
                                                          delta[i],"C1 Curve")
            ### Criteria C2:
            keyTable <- create_summary_key_table_conv_binomial(keyTable,tempPrec[j,],tempDF,1,decision_direction_val(),decision_c2_tv_val_for(),decision_c2_sig_val(),NULL,NULL,
                                                          delta[i],"C2 Curve")
          }
        } else if(decision_nCriteria_val()==1){
          ### Single criteria:
          if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
            keyTable <- create_summary_key_table_conv_normal(keyTable,tempPrec[j,],tempDF,1,decision_direction_val(),decision_c1_tv_val_for(),decision_c1_sig_val(),NULL,NULL,
                                                        delta[i],"OC Curves")
          } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
            keyTable <- create_summary_key_table_conv_binomial(keyTable,tempPrec[j,],tempDF,1,decision_direction_val(),decision_c1_tv_val_for(),decision_c1_sig_val(),NULL,NULL,
                                                          delta[i],"OC Curves")
          }
        }
      }
    }
    
    ### Format table:
    # Format delta (if applicable):
    deltaAdd <- ""
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal) && study_comparison_type_val()==label_study_perc){
      keyTable$Delta <- formatPercDiffNorm_Inv(keyTable$Delta,design_log_val())
      deltaAdd <- "(%)"
    } else if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal) && study_comparison_type_val()==label_study_ratio){
      keyTable$Delta <- formatRatioNorm_Inv(keyTable$Delta,design_log_val())
      deltaAdd <- "(Ratio)"
    } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      keyTable$Delta <- formatPercDiffBin_Inv(keyTable$Delta)
      keyTable$ProbRef <- 100*keyTable$ProbRef
      deltaAdd <- "(%)"
    }
    if(deltaAdd != ""){
      names(keyTable)[which(names(keyTable)=="Delta")] <- paste("Delta",deltaAdd)
    }
    
    # Format probabilities of decisions:
    keyTable$Prob_Go <- 100*keyTable$Prob_Go
    names(keyTable)[which(names(keyTable)=="Prob_Go")] <- key_lab_go
    keyTable$Prob_Discuss <- 100*keyTable$Prob_Discuss
    names(keyTable)[which(names(keyTable)=="Prob_Discuss")] <- key_lab_discuss
    keyTable$Prob_Stop <- 100*keyTable$Prob_Stop
    names(keyTable)[which(names(keyTable)=="Prob_Stop")] <- key_lab_stop

    # Format column names (if applicable):
    names(keyTable)[which(names(keyTable)=="Graph")] <- key_lab_graph
    names(keyTable)[which(names(keyTable)=="N1")] <- key_lab_ntreat
    names(keyTable)[which(names(keyTable)=="N2")] <- key_lab_ncontrol
    names(keyTable)[which(names(keyTable)=="Sigma")] <- key_lab_sigma
    names(keyTable)[which(names(keyTable)=="SE")] <- key_lab_se
    names(keyTable)[which(names(keyTable)=="ProbRef")] <- key_lab_probref
    
    ### Remove results (if applicable):
    if(decision_nCriteria_val()==2){
      warn <- determine_minimum_prec()
      if(warn & study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
        keyTable <- keyTable[keyTable$Graph != "OC Curves",]
      }
    }
    
    ### Return completed table:
    return(keyTable)
  })
 
  
  ###################################################################
  ###################################################################
  ######   Key functions for extracting generic information:   ######
  ###################################################################
  ###################################################################

  getTVinfo <- function(tvInfo){
    if(tvInfo==1){
      tvOut <- list(tvName="C1",tv=decision_c1_tv_val(),sig=decision_c1_sig_val())
    } else if (tvInfo==2){
      tvOut <- list(tvName="C2",tv=decision_c2_tv_val(),sig=decision_c2_sig_val())
    } else {
      stop("This is not a valid option for decision criteria")
    }
    return(tvOut)
  }
  
  getTVinfo_for <- function(tvInfo){
    if(tvInfo==1){
      tvOut <- list(tvName="C1",tv=decision_c1_tv_val_for(),sig=decision_c1_sig_val())
    } else if (tvInfo==2){
      tvOut <- list(tvName="C2",tv=decision_c2_tv_val_for(),sig=decision_c2_sig_val())
    } else {
      stop("This is not a valid option for decision criteria")
    }
    return(tvOut)
  }
  
  getDFinfo <- function(interim=FALSE){
    if(interim){
      return(normalMeansDF(design_interim_normApprox_val(),design_interim_df_val()))
    } else {
      return(normalMeansDF(design_normApprox_val(),design_df_val()))
    }
  }
  
  
  #######################################################
  #######################################################
  ######   Utility functions for output objects:   ######
  #######################################################
  #######################################################

  determine_minimum_prec <- function(){
    ### Determine 50% points to determine whether precision needs to be increased
    warn <- FALSE
    tempPrec <- create_prec_table()
    tempDF <- getDFinfo()
    if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
      tempComp <- "Normal"
    } else if (study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
      tempComp <- "Binomial"
    }
    warn <- determine_minimum_prec_sub(tempComp,tempPrec,tempDF,decision_direction_val(),
                                       decision_c1_tv_val_for(),decision_c1_sig_val(),
                                       decision_c2_tv_val_for(),decision_c2_sig_val())
    return(warn)
  }
  
  create_OC_dec_Text <- function(plot,nCriteria,tvInfo=1){  
    if(advanced_footnote_choice_val()==1){
      grid.newpage()
      mainView <- viewport(layout=grid.layout(nrow=2,ncol=1,heights=unit(c(1,8),c("null","lines"))))
      topView <- viewport(layout.pos.row=1,layout.pos.col=1,name="top1")
      botView <- viewport(layout.pos.row=2,layout.pos.col=1,name="bottom1")
      splot <- vpTree(mainView,vpList(topView,botView))
      pushViewport(splot)
      seekViewport("top1")
      print(plot,vp="top1")
      seekViewport("bottom1")
      
      ### OC type description:
      comparisonText <- study_comparison_val()
      if(study_comparison_val()==label_study_2Normal){
        comparisonText <- paste0(unlist(strsplit(study_comparison_type_val()," "))[1]," ",tolower(comparisonText))
      }
      grid.text(paste0("OC Type: ",study_ocType_val()," (",comparisonText,")"),
                x=0.01,y=0.75,gp=gpar(fontsize=footnote_font_size,col="Black"),just=c("left"))
      
      if(design_normApprox_val()){
        dfInfo <- ""
      } else {
        dfInfo <- paste0(", DF = ",getDFinfo())
      }
      
      ### Design description:
      precVec <- create_prec_table()
      precVec <- precVec$SE
      precVec <- paste(signif(as.numeric(precVec),3),collapse=",")
      
      precPref <- ""
      if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal) && (study_comparison_type_val() %in% c(label_study_perc,label_study_ratio))){
        if(design_log_val()==1){
          precPref <- "log[e] scale: "
        } else if(design_log_val()==2){
          precPref <- "log10 scale: "
        } else if (design_log_val()==3){
          precPref <- "log[e] scale: "
        }
      }
      if(design_precision_val()==2){
        precInfo <- paste0("(",precPref,"SE = ",precVec,dfInfo,")")
      } else {
        n1Vec <- NULL
        for(i in 1:design_n1_n_val()){
          n1Vec <- c(n1Vec,get(paste0("design_n1_",i,"_val"))())
        }
        n1Vec <- paste(n1Vec,collapse=",")
        if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
          sdVec <- NULL
          for(i in 1:design_sigma_n_val()){
            sdVec <- c(sdVec,get(paste0("design_sigma_",i,"_val"))())
          }
          sdVec <- paste(sdVec,collapse=",")
          endVec <- paste0(", ",precPref,"SD = ",sdVec,", SE = ",precVec,dfInfo,")")
        } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
          pRVec <- NULL
          for(i in 1:design_probRef_n_val()){
            pRVec <- c(pRVec,get(paste0("design_probRef_",i,"_val"))())
          }
          pRVec <- paste(as.numeric(pRVec),collapse=",")
          endVec <- paste0(", Reference Percentage = ",pRVec,"%",dfInfo,")")
        }
        if(study_studyDesign_val() %in% c(label_study_CrossOver,label_study_Single)){
          precInfo <- paste0("(N total = ",n1Vec,endVec)  
        } else {
          if(design_equalN_val()){
            precInfo <- paste0("(N per arm = ",n1Vec,endVec)    
          } else {
            n2Vec <- NULL
            for(i in 1:design_n2_n_val()){
              n2Vec <- c(n2Vec,get(paste0("design_n2_",i,"_val"))())
            }
            n2Vec <- paste(n2Vec,collapse=",")
            precInfo <- paste0("(N treatment = ",n1Vec,", N control = ",n2Vec,endVec)
          }
        }
      }
      grid.text(paste0("Design: ",study_studyDesign_val()," ",precInfo),
                x=0.01,y=0.525,gp=gpar(fontsize=footnote_font_size,col="Black"),just=c("left"))
      
      ### Criteria description:
      if(nCriteria==2){
        grid.text(eRec_text_decCrit1(),
                  x=0.01,y=0.30,gp=gpar(fontsize=footnote_font_size,col="Black"),just=c("left"))
        grid.text(eRec_text_decCrit2(),
                  x=0.01,y=0.10,gp=gpar(fontsize=footnote_font_size,col="Black"),just=c("left"))
      } else {
        tvData <- getTVinfo(tvInfo)
        grid.text(get(paste0("eRec_text_decCrit",tvInfo))(),
                  x=0.01,y=0.30,gp=gpar(fontsize=footnote_font_size,col="Black"),just=c("left"))
      }
      if(study_ocType_val()==label_study_interim){
        grid.text(eRec_text_intCrit1()$text,
                  x=0.01,y=0.10,gp=gpar(fontsize=footnote_font_size,col="Black"),just=c("left"))
      }
      
      ### Plot description:
      grid.text(paste0("Plot created: ",format(Sys.time(), "%d-%b-%Y %H:%M")),
                x=0.99,y=0.75,gp=gpar(fontsize=footnote_font_size,col="Black"),just=c("right"))
      
      ### User ID:
      grid.text(paste0("Created by: ",plot_userID_val()),
                x=0.99,y=0.30,gp=gpar(fontsize=footnote_font_size,col="Black"),just=c("right"))
      
      ### Version Number:
      grid.text(paste0("Version ",versionNumber),
                x=0.99,y=0.10,gp=gpar(fontsize=footnote_font_size,col="Black"),just=c("right"))
    } else {
      return(plot)
    }
  }
  
  create_vert_lines_info <- function(){
    if(is.null(advanced_lines_vert_number_val())){
      return(NULL)
    } else if(advanced_lines_vert_number_val()!= 0){
      outList <- list()
      for(i in 1:advanced_lines_vert_number_val()){
        outList[[i]] <- c(get(paste0("advanced_lines_vert_pos",i,"_val"))(),get(paste0("advanced_lines_vert_col",i,"_val"))())
      }
      return(outList)
    } else {
      return(NULL)
    }
  }
  
  create_horz_lines_info <- function(){
    if(is.null(advanced_lines_horz_number_val())){
      return(NULL)
    } else if(advanced_lines_horz_number_val()!= 0){
      outList <- list()
      for(i in 1:advanced_lines_horz_number_val()){
        outList[[i]] <- c(get(paste0("advanced_lines_horz_pos",i,"_val"))(),get(paste0("advanced_lines_horz_col",i,"_val"))())
      }
      return(outList)
    } else {
      return(NULL)
    }
  }
  
  create_summary_multiple_values <- function(dataset,name){
    outData <- data.frame(VarName=c(paste0(name,"n")),Value=c(get(paste0(name,"n_val"))()),stringsAsFactors=F)
    for(i in 1:get(paste0(name,"n_val"))()){
      outData <- rbind(outData,data.frame(VarName=paste0(name,i),
                                          Value=get(paste0(name,i,"_val"))(),stringsAsFactors=F))
    }
    outData <- rbind(dataset,outData)
    return(outData)
  }
  
  create_plot_options_curveStyle <- function(){
    noType <- c("variable","Nothing",FALSE)
    multDec <- list()
    if(design_precision_val()==2){
      multDec[[1]] <- c("SE",advanced_legend_title_se_val(),TRUE)
      multDec[[2]] <- noType
      multDec[[3]] <- noType
    } else {
      n1Type <- c("N1",advanced_legend_title_n1_val(),TRUE)
      n2Type <- c("N2",advanced_legend_title_n2_val(),TRUE)
      if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
        sdType <- c("Sigma",advanced_legend_title_sigma_val(),TRUE)
      } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
        sdType <- c("ProbRef",advanced_legend_title_probRef_val(),TRUE)
      }
      multDec <- list(n1Type,sdType,n2Type)
      if(as.numeric(design_n1_n_val())==1){multDec[[1]] <- NULL}
      if(study_comparison_val() %in% c(label_study_2Normal,label_study_1Normal)){
        if(as.numeric(design_sigma_n_val())==1){multDec[[length(multDec)-1]] <- NULL}
      } else if(study_comparison_val() %in% c(label_study_2Binomial,label_study_1Binomial)){
        if(as.numeric(design_probRef_n_val())==1){multDec[[length(multDec)-1]] <- NULL}
      }
      if(study_studyDesign_val()!=label_study_Parallel || design_equalN_val() || as.numeric(design_n2_n_val())==1){
        multDec[[length(multDec)]] <- NULL
      }
      if(length(multDec)<3){
        for(i in (length(multDec)+1):3){
          multDec[[i]] <- noType
        }
      }
    }
    return(multDec)
  }
  
})






#End of big bad server script
