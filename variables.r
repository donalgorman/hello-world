
################################################
#                                              #
#   Operating Characteristic Curve Generator   #
#                [Version 2.0]                 #
#                                              #
################################################

####################
# Global variables #
####################

versionNumber <- "2.0"

####################
### Input Labels ###
####################

#Labels for comparisons:
label_study_2Normal <- "Difference of 2 normal means"
label_study_2Binomial <- "Difference of 2 binomial proportions"
label_study_1Normal <- "One sample normal mean vs. fixed value"
label_study_1Binomial <- "Binomial proportion vs. fixed value (use with caution!)"

#Labels for comparison type:
label_study_abs <- "Absolute scale"
label_study_perc <- "Percentage scale"
label_study_ratio <- "Ratio scale"

#Labels for OC Type:
label_study_conv <- "Conventional"
label_study_interim <- "Interim"

#Labels for study design:
label_study_Parallel <- "Parallel Group"
label_study_CrossOver <- "Cross-over"
label_study_Single <- "Single Group"

#Internal labels for 2DC OC Curve:
label_decision_2_both <- "C1 & C2"
label_decision_2_one <- "C1 only"
label_decision_2_none <- "None"

#Internal labels for 1DC OC Curve:
label_decision_1_one <- "Yes"
label_decision_1_none <- "No"

#Labels for log formats:
label_design_loge <- "Log_e"
label_design_log10 <- "Log10"

#Colour options for vertical lines:
label_advanced_lines_vert_colours <- c("Black","Grey","Purple","Red","Orange","Yellow","Green","Blue")

#Colour options for horizontal lines:
label_advanced_lines_horz_colours <- label_advanced_lines_vert_colours


########################
### Footnote options ###
########################

footnote_font_size <- 15


##########################
### Algorithm defaults ###
##########################

alg_binomial_power_gap <- 11
alg_binomial_power_step <- 5
normal_df_default <- 999


#################################
### Key table variable labels ###
#################################

key_lab_graph <- "Graph"
key_lab_ntreat <- "N (Treat)"
key_lab_ncontrol <- "N (Control)"
key_lab_go <- "Go (%)"
key_lab_discuss <- "Discuss (%)"
key_lab_stop <- "Stop (%)"
key_lab_sigma <- "Sigma"
key_lab_se <- "SE"
key_lab_probref <- "ProbRef (%)"
key_lab_table_all <- "All"




#End of big bad variables
