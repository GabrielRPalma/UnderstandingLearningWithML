####################################################################################################
###
### File:    06_coordinates_results_data.r
### Purpose: Loading the results data to obtain the statistics 
###         and get differences among the work.
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    07/04/23
###
####################################################################################################
# Loading packages and functions -----
source(here('00_source.r'))

# Loading the files -----
Minmax_scaled_trial_1_acc <- c(0.693878, 0.795918, 0.918367, 0.775510, 0.510204, 0.857143)
Minmax_scaled_trial_1_auroc <- c(0.713805, 0.806397, 0.921717, 0.775253, 0.462963, 0.857744)
Minmax_scaled_trial_12_acc <- c(0.897959, 0.836735, 0.857143, 0.836735, 0.612245, 0.897959) 
Minmax_scaled_trial_12_auroc <-c(0.886364, 0.830808, 0.853535, 0.830808, 0.585017, 0.890572)

zscored_scaled_trial_1_acc <- c(0.510204, 0.795918, 0.918367, 0.775510, 0.306122, 0.897959)
zscored_scaled_trial_1_auroc <- c(0.547138, 0.806397, 0.921717, 0.775253, 0.286195, 0.903199)
zscored_scaled_trial_12_acc <- c(0.591837, 0.816327, 0.857143, 0.836735, 0.775510, 0.938776)
zscored_scaled_trial_12_auroc <-c(0.600168, 0.812290, 0.853535, 0.835017, 0.779461, 0.940236)

raw_midline_trial_1_acc <- c(0.55102, 0.510204, 0.918367, 0.510204, 0.326531, 0.55102)
raw_midline_trial_1_auroc <- c(0.50000, 0.500842, 0.921717, 0.505051, 0.296296, 0.50000)
raw_midline_trial_12_acc  <- c(0.530612, 0.775510, 0.857143, 0.551020, 0.795918, 0.489796)
raw_midline_trial_12_auroc <- c(0.481481, 0.766835, 0.853535, 0.542088, 0.785354, 0.452862)

coordinates_results <- data.frame(Preprocessing = c(rep('Minmax scaled', 6*4), 
                                                    rep('Zscored scaled', 6*4), 
                                                    rep('Raw midline', 6*4)), 
                                  Trial = rep(c(rep('1', 12), rep('12', 12)), 3), 
                                  Metric = rep(c(rep('Accuracy', 6), rep('AUROC', 6), 
                                                 rep('Accuracy', 6), rep('AUROC', 6)), 3),
                                  HMM_parameters = rep("None", 72),
                                  Models = rep(c("Polynomial \n SVM", "Non linear \n SVM",
                                             "RF", "KNN", "Lasso", "DNN"), 12),
                                  Value = c(Minmax_scaled_trial_1_acc,
                                            Minmax_scaled_trial_1_auroc, 
                                            Minmax_scaled_trial_12_acc,
                                            Minmax_scaled_trial_12_auroc, 
                                            zscored_scaled_trial_1_acc, 
                                            zscored_scaled_trial_1_auroc,
                                            zscored_scaled_trial_12_acc, 
                                            zscored_scaled_trial_12_auroc,
                                            raw_midline_trial_1_acc, 
                                            raw_midline_trial_1_auroc,
                                            raw_midline_trial_12_acc, 
                                            raw_midline_trial_12_auroc
                                            ))

coordinates_results
coordinates_results[coordinates_results$Models == "Polynomial \n SVM",]$Models <- 'Poly \n SVM'
coordinates_results[coordinates_results$Trial == "1",]$Trial <- 'Trial 1'
coordinates_results[coordinates_results$Trial == "12",]$Trial <- 'Trial 12'

