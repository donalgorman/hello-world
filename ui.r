
################################################
#                                              #
#   Operating Characteristic Curve Generator   #
#                [Version 2.0]                 #
#                                              #
################################################

#ui script

#Creator D Gorman
#Date: 30 Dec 2015

### Set-up shop:
rm(list=ls())
graphics.off()

### Load required libraries:
loadLibraries <- function(x){
  for(i in x){
    require(i,character.only=TRUE)
  }
}
loadLibraries(c("shiny","ggplot2","reshape2","grid"))

### Read in functions and variables:
source("functions.r")
source("variables.r")
source("initial.r")

shinyUI(fluidPage(
  
  #############
  ### Title ###
  #############
  titlePanel("Operating Characteristic Curve Generator"),
  h3(em(paste0("Version ",versionNumber))),
  br(),
  
  sidebarLayout(
    ##############################
    ### Side Panel for inputs: ###
    ##############################
    sidebarPanel(
      tabsetPanel(type="tabs", 
                  tabPanel("Study",h3(strong("Study Options:")),
                           selectInput("study_comparison","Select Comparison:",choices=c(label_study_2Normal,label_study_2Binomial,label_study_1Normal),selected=init_study_comparison),
                           uiOutput("studyUI_ocType")),
                  tabPanel("Criteria",h3(strong("Decision criteria:")),
                           uiOutput("decisionUI_start")),
                  tabPanel("Design",h3(strong("Design options:")),
                           uiOutput("designUI_start")),
                  tabPanel("Output",h3(strong("Output Options:")),
                           uiOutput("optionsOutUI_start")),
                  tabPanel("Advanced",h3(strong("Advanced Options:")),
                           tabsetPanel(type = "tabs", 
                                       tabPanel("Y-axis",br(),
                                                textInput("advanced_yaxis_title","Y-axis title (leave blank for default):",init_advanced_yaxis_title),
                                                numericInput("advanced_yaxis_break","Y-axis breaks (leave blank for default):",init_advanced_yaxis_break),
                                                numericInput("advanced_yaxis_low","Y-axis lower limit (leave blank for default):",init_advanced_yaxis_low),
                                                numericInput("advanced_yaxis_upp","Y-axis upper limit (leave blank for default):",init_advanced_yaxis_upp)),
                                       tabPanel("X-axis",br(),
                                                textInput("advanced_xaxis_title","X-axis title (leave blank for default):",init_advanced_xaxis_title),
                                                numericInput("advanced_xaxis_break","X-axis breaks (leave blank for default):",init_advanced_xaxis_break)),
                                       tabPanel("Legend",br(),
                                                textInput("advanced_legend_title","Main legend title (leave blank for default):",init_advanced_legend_title),
                                                uiOutput("advanced_legendUI")),
                                       tabPanel("Add lines",br(),
                                                radioButtons("advanced_lines_vert_number","Number of vertical lines to add:",
                                                             choices=list("0"=0,"1"=1,"2"=2,"3"=3),selected=init_advanced_lines_vert_number,inline=TRUE),
                                                uiOutput("advanced_linesVertUI"),
                                                radioButtons("advanced_lines_horz_number","Number of horizontal lines to add:",
                                                             choices=list("0"=0,"1"=1,"2"=2,"3"=3),selected=init_advanced_lines_horz_number,inline=TRUE),
                                                uiOutput("advanced_linesHorzUI")),
                                       tabPanel("Footnote",br(),
                                                radioButtons("advanced_footnote_choice","Include in footnote:",
                                                             choices=list("All"=1,"None"=2),selected=init_advanced_footnote_choice,inline=TRUE)),
                                       tabPanel("Plots",br(),
                                                numericInput("advanced_plot_gap","Number of points:",init_advanced_plot_gap),
                                                #numericInput("advanced_plot_sim","Number of simulations (if applicable):",init_advanced_plot_sim),
                                                numericInput("advanced_plot_width","Width of plot:",init_advanced_plot_width),
                                                numericInput("advanced_plot_height","Height of plot:",init_advanced_plot_height),
                                                checkboxGroupInput("advanced_plot_curves",label="Select curves to plot:",
                                                                   choices=list("Green"=1,"Orange"=2,"Red"=3),selected=c(1,2,3),inline=T),
                                                numericInput("advanced_plot_size",label="Line size multiplier (leave blank for default):",init_advanced_plot_size))
                           ),
                           br(),
                           p("Generator created using ",a("SHINY",href = "http://www.rstudio.com/shiny")))),
                  #tabPanel("TEST",h3(strong("Session Info")),
                  #         br(),
      #tabPanel("TEST",h3(strong("Session Info")),
                  #         h3(paste0("Shiny: ",packageVersion("shiny"))),
                  #         h3(paste0("ggplot2: ",packageVersion("ggplot2"))),
                  #         h3(paste0("Reshape: ",packageVersion("reshape2"))),
                  #         h3(paste0("grid: ",packageVersion("grid"))),
                  #         h3(print(sessionInfo())))),
      actionButton(inputId="updateOutput",label="Update Output")
    ),
    
    ###############################
    ### Main Panel for outputs: ###
    ###############################
    mainPanel(
      uiOutput("outputUI_start")
    )
  ),
  
  ################################
  ### Style of errors/warnings ###
  ################################
  includeCSS('styles.css')

))



#End of big bad ui script
