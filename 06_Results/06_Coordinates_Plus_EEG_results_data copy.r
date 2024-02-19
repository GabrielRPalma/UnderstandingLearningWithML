####################################################################################################
###
### File:    06_EEG_results_data.r
### Purpose: Loading the results data to obtain the statistics 
###         and get differences among the work.
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    08/04/23
###
####################################################################################################
# Loading packages and functions -----
source(here('00_source.r'))

# Loading the files -----
########################################################################################
################################ All ############################################
##############################################################################################################
Minmax_scaled_trial_1_acc_all <- c(0.564103, 0.743590, 0.641026, 0.589744, 0.692308, 0.692308)
Minmax_scaled_trial_1_auroc_all <- c(0.500000, 0.759358, 0.628342, 0.596257, 0.673797, 0.687166)
Minmax_scaled_trial_12_acc_all <- c(0.589744, 0.538462, 0.589744, 0.512821, 0.666667, 0.564103)
Minmax_scaled_trial_12_auroc_all <- c(0.529412, 0.517380, 0.569519, 0.501337, 0.631016, 0.540107)
  
zscored_scaled_trial_1_acc_all <- c(0.564103, 0.743590, 0.666667, 0.641026, 0.564103, 0.615385)
zscored_scaled_trial_1_auroc_all <- c(0.500000, 0.752674, 0.651070, 0.648396, 0.546791, 0.612299)
zscored_scaled_trial_12_acc_all <- c(0.589744, 0.641026, 0.615385, 0.743590, 0.717949, 0.794872)
zscored_scaled_trial_12_auroc_all <- c(0.529412, 0.614973, 0.592246, 0.739305, 0.709893, 0.791444)

raw_midline_trial_1_acc_all <- c(0.564103, 0.410256, 0.615385, 0.461538, 0.666667, 0.564103)
raw_midline_trial_1_auroc_all <- c(0.500000, 0.423797, 0.598930, 0.449198, 0.651070, 0.500000)
raw_midline_trial_12_acc_all  <- c(0.538462, 0.538462, 0.564103, 0.564103, 0.717949, 0.564103)
raw_midline_trial_12_auroc_all <- c(0.477273, 0.517380, 0.540107, 0.566845, 0.703209, 0.500000)

########################################################################################
################################ HMM (m = 2) ############################################
##############################################################################################################
Minmax_scaled_trial_1_acc_m2 <- c(0.564103, 0.435897, 0.717949, 0.461538, 0.461538, 0.564103)
Minmax_scaled_trial_1_auroc_m2 <- c(0.500000, 0.433155, 0.709893, 0.449198, 0.435829, 0.500000)
Minmax_scaled_trial_12_acc_m2 <- c(0.538462, 0.589744, 0.717949, 0.564103, 0.743590, 0.538462)
Minmax_scaled_trial_12_auroc_m2 <- c(0.477273, 0.569519, 0.709893, 0.566845, 0.725936, 0.477273)

zscored_scaled_trial_1_acc_m2 <- c(0.564103, 0.717949, 0.692308, 0.641026, 0.435897, 0.692308)
zscored_scaled_trial_1_auroc_m2 <- c(0.500000, 0.723262, 0.687166, 0.648396, 0.419786, 0.680481)
zscored_scaled_trial_12_acc_m2 <- c(0.589744, 0.641026, 0.666667, 0.743590, 0.717949, 0.743590)
zscored_scaled_trial_12_auroc_m2 <- c(0.529412, 0.614973, 0.657754, 0.739305, 0.709893, 0.732620)

raw_midline_trial_1_acc_m2 <- c(0.564103, 0.435897, 0.717949, 0.461538, 0.461538, 0.564103)
raw_midline_trial_1_auroc_m2 <- c(0.500000, 0.433155, 0.709893, 0.449198, 0.435829, 0.500000)
raw_midline_trial_12_acc_m2  <- c(0.538462, 0.589744, 0.717949, 0.564103, 0.743590, 0.538462)
raw_midline_trial_12_auroc_m2 <- c(0.477273, 0.569519, 0.709893, 0.566845, 0.725936, 0.477273)

########################################################################################
################################ HMM (m = 3) ############################################
##############################################################################################################
Minmax_scaled_trial_1_acc_m3 <- c(0.564103, 0.743590, 0.717949, 0.589744, 0.384615, 0.743590)
Minmax_scaled_trial_1_auroc_m3 <- c(0.500000, 0.759358, 0.716578, 0.596257, 0.347594, 0.745989)
Minmax_scaled_trial_12_acc_m3 <-c(0.589744, 0.538462, 0.666667, 0.512821, 0.666667, 0.692308)
Minmax_scaled_trial_12_auroc_m3 <- c(0.529412, 0.517380, 0.664439, 0.501337, 0.631016, 0.687166)

zscored_scaled_trial_1_acc_m3 <- c(0.564103, 0.743590, 0.666667, 0.641026, 0.435897, 0.743590)
zscored_scaled_trial_1_auroc_m3 <- c(0.500000, 0.752674, 0.664439, 0.648396, 0.419786, 0.739305)
zscored_scaled_trial_12_acc_m3 <- c(0.589744, 0.641026, 0.666667, 0.743590, 0.717949, 0.846154)
zscored_scaled_trial_12_auroc_m3 <- c(0.529412, 0.614973, 0.664439, 0.739305, 0.709893, 0.843583)

raw_midline_trial_1_acc_m3 <- c(0.564103, 0.512821, 0.692308, 0.461538, 0.358974, 0.564103)
raw_midline_trial_1_auroc_m3 <- c(0.500000, 0.534759, 0.693850, 0.449198, 0.338235, 0.500000)
raw_midline_trial_12_acc_m3  <- c(0.538462, 0.615385, 0.692308, 0.564103, 0.769231, 0.589744)
raw_midline_trial_12_auroc_m3 <- c(0.477273, 0.585561, 0.693850, 0.566845, 0.755348, 0.562834)

  
########################################################################################
################################ HMM (m = 4) ############################################
##############################################################################################################
Minmax_scaled_trial_1_acc_m4 <- c(0.564103, 0.743590, 0.743590, 0.589744, 0.692308, 0.820513)
Minmax_scaled_trial_1_auroc_m4 <- c(0.500000, 0.759358, 0.732620, 0.596257, 0.673797, 0.820856)
Minmax_scaled_trial_12_acc_m4 <- c(0.589744, 0.589744, 0.692308, 0.512821, 0.666667, 0.564103)
Minmax_scaled_trial_12_auroc_m4 <- c(0.529412, 0.562834, 0.680481, 0.501337, 0.631016, 0.560160)

zscored_scaled_trial_1_acc_m4 <- c(0.564103, 0.743590, 0.717949, 0.641026, 0.564103, 0.641026)
zscored_scaled_trial_1_auroc_m4 <- c(0.500000, 0.752674, 0.709893, 0.648396, 0.546791, 0.628342)
zscored_scaled_trial_12_acc_m4 <- c(0.589744, 0.641026, 0.666667, 0.743590, 0.717949, 0.897436)
zscored_scaled_trial_12_auroc_m4 <- c(0.529412, 0.614973, 0.657754, 0.739305, 0.709893, 0.895722)

raw_midline_trial_1_acc_m4 <- c(0.564103, 0.410256, 0.692308, 0.461538, 0.666667, 0.564103)
raw_midline_trial_1_auroc_m4 <- c(0.500000, 0.417112, 0.687166, 0.449198, 0.651070, 0.500000)
raw_midline_trial_12_acc_m4  <- c(0.538462, 0.589744, 0.743590, 0.564103, 0.717949, 0.564103)
raw_midline_trial_12_auroc_m4 <- c(0.477273, 0.562834, 0.739305, 0.566845, 0.703209, 0.500000)
  
########################################################################################
################################ HMM (m = 5) ############################################
##############################################################################################################
Minmax_scaled_trial_1_acc_m5 <- c(0.564103, 0.743590, 0.743590, 0.589744, 0.512821, 0.769231)
Minmax_scaled_trial_1_auroc_m5 <- c(0.500000, 0.759358, 0.732620, 0.596257, 0.481283, 0.775401)
Minmax_scaled_trial_12_acc_m5 <- c(0.589744, 0.564103, 0.717949, 0.512821, 0.666667, 0.512821)
Minmax_scaled_trial_12_auroc_m5 <- c(0.529412, 0.540107, 0.703209, 0.501337, 0.631016, 0.501337)

zscored_scaled_trial_1_acc_m5 <- c(0.564103, 0.743590, 0.692308, 0.641026, 0.435897, 0.794872)
zscored_scaled_trial_1_auroc_m5 <- c(0.500000, 0.752674, 0.673797, 0.648396, 0.419786, 0.791444)
zscored_scaled_trial_12_acc_m5 <- c(0.589744, 0.641026, 0.692308, 0.743590, 0.717949, 0.871795)
zscored_scaled_trial_12_auroc_m5 <- c(0.529412, 0.614973, 0.680481, 0.739305, 0.709893, 0.872995)

raw_midline_trial_1_acc_m5 <- c(0.564103, 0.564103, 0.666667, 0.461538, 0.589744, 0.564103)
raw_midline_trial_1_auroc_m5 <- c(0.500000, 0.566845, 0.651070, 0.449198, 0.569519, 0.500000)
raw_midline_trial_12_acc_m5 <- c(0.538462, 0.615385, 0.692308, 0.564103, 0.743590, 0.512821)
raw_midline_trial_12_auroc_m5 <- c(0.477273, 0.605615, 0.673797, 0.566845, 0.725936, 0.454545)

# Mergin all the results -----
coorinates_plus_eeg_results <- data.frame(Preprocessing = rep(c(rep('Minmax \n scaled', 6*4), 
                                                    rep('Zscored \n scaled', 6*4), 
                                                    rep('Raw \n midline', 6*4)), 5), 
                                  Trial = rep(c(rep('1', 12), rep('12', 12)), 3*5), 
                                  Metric = rep(c(rep('Accuracy', 6), rep('AUROC', 6), 
                                                 rep('Accuracy', 6), rep('AUROC', 6)), 3*5),
                                  HMM_parameters = c(rep("All", 6*12), 
                                                     rep("m(2)", 6*12), 
                                                     rep("m(3)", 6*12), 
                                                     rep("m(4)", 6*12), 
                                                     rep("m(5)", 6*12)),
                                  Models = rep(c("Polynomial \n SVM", "Non linear \n SVM",
                                             "RF", "KNN", "Lasso", "DNN"), 12*5),
                                  Value = c(Minmax_scaled_trial_1_acc_all,
                                            Minmax_scaled_trial_1_auroc_all, 
                                            Minmax_scaled_trial_12_acc_all,
                                            Minmax_scaled_trial_12_auroc_all, 
                                            zscored_scaled_trial_1_acc_all, 
                                            zscored_scaled_trial_1_auroc_all,
                                            zscored_scaled_trial_12_acc_all, 
                                            zscored_scaled_trial_12_auroc_all,
                                            raw_midline_trial_1_acc_all, 
                                            raw_midline_trial_1_auroc_all,
                                            raw_midline_trial_12_acc_all, 
                                            raw_midline_trial_12_auroc_all, 
                                            
                                            Minmax_scaled_trial_1_acc_m2,
                                            Minmax_scaled_trial_1_auroc_m2, 
                                            Minmax_scaled_trial_12_acc_m2,
                                            Minmax_scaled_trial_12_auroc_m2, 
                                            zscored_scaled_trial_1_acc_m2, 
                                            zscored_scaled_trial_1_auroc_m2,
                                            zscored_scaled_trial_12_acc_m2, 
                                            zscored_scaled_trial_12_auroc_m2,
                                            raw_midline_trial_1_acc_m2, 
                                            raw_midline_trial_1_auroc_m2,
                                            raw_midline_trial_12_acc_m2, 
                                            raw_midline_trial_12_auroc_m2, 
                                            
                                            Minmax_scaled_trial_1_acc_m3,
                                            Minmax_scaled_trial_1_auroc_m3, 
                                            Minmax_scaled_trial_12_acc_m3,
                                            Minmax_scaled_trial_12_auroc_m3, 
                                            zscored_scaled_trial_1_acc_m3, 
                                            zscored_scaled_trial_1_auroc_m3,
                                            zscored_scaled_trial_12_acc_m3, 
                                            zscored_scaled_trial_12_auroc_m3,
                                            raw_midline_trial_1_acc_m3, 
                                            raw_midline_trial_1_auroc_m3,
                                            raw_midline_trial_12_acc_m3, 
                                            raw_midline_trial_12_auroc_m3,
                                            
                                            Minmax_scaled_trial_1_acc_m4,
                                            Minmax_scaled_trial_1_auroc_m4, 
                                            Minmax_scaled_trial_12_acc_m4,
                                            Minmax_scaled_trial_12_auroc_m4, 
                                            zscored_scaled_trial_1_acc_m4, 
                                            zscored_scaled_trial_1_auroc_m4,
                                            zscored_scaled_trial_12_acc_m4, 
                                            zscored_scaled_trial_12_auroc_m4,
                                            raw_midline_trial_1_acc_m4, 
                                            raw_midline_trial_1_auroc_m4,
                                            raw_midline_trial_12_acc_m4, 
                                            raw_midline_trial_12_auroc_m4,
                                            
                                            Minmax_scaled_trial_1_acc_m5,
                                            Minmax_scaled_trial_1_auroc_m5, 
                                            Minmax_scaled_trial_12_acc_m5,
                                            Minmax_scaled_trial_12_auroc_m5, 
                                            zscored_scaled_trial_1_acc_m5, 
                                            zscored_scaled_trial_1_auroc_m5,
                                            zscored_scaled_trial_12_acc_m5, 
                                            zscored_scaled_trial_12_auroc_m5,
                                            raw_midline_trial_1_acc_m5, 
                                            raw_midline_trial_1_auroc_m5,
                                            raw_midline_trial_12_acc_m5, 
                                            raw_midline_trial_12_auroc_m5
                                            ))

coorinates_plus_eeg_results$HMM_parameters <- factor(coorinates_plus_eeg_results$HMM_parameters)
coorinates_plus_eeg_results[coorinates_plus_eeg_results$Models == "Polynomial \n SVM",]$Models <- 'Poly \n SVM'
coorinates_plus_eeg_results[coorinates_plus_eeg_results$Trial == "1",]$Trial <- 'Trial 1'
coorinates_plus_eeg_results[coorinates_plus_eeg_results$Trial == "12",]$Trial <- 'Trial 12'

