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
Minmax_scaled_trial_1_acc_all <- c(0.553191, 0.638298, 0.425532, 0.595745, 0.404255, 0.787234)
Minmax_scaled_trial_1_auroc_all <- c(0.522727, 0.627273, 0.419091, 0.587273, 0.380000, 0.786364)
Minmax_scaled_trial_12_acc_all <- c(0.531915, 0.595745, 0.553191, 0.489362, 0.361702, 0.638298)
Minmax_scaled_trial_12_auroc_all <- c(0.500000, 0.603636, 0.541818, 0.490000, 0.340000, 0.638182)
  
zscored_scaled_trial_1_acc_all <- c(0.531915, 0.489362, 0.382979, 0.425532, 0.404255, 0.382979)
zscored_scaled_trial_1_auroc_all <- c(0.500000, 0.490000, 0.379091, 0.419091, 0.380000, 0.379091)
zscored_scaled_trial_12_acc_all <- c(0.531915, 0.765957, 0.531915	, 0.638298, 0.340426, 0.787234)
zscored_scaled_trial_12_auroc_all <- c(0.500000, 0.774545, 0.521818, 0.640909, 0.320000, 0.789091)

raw_midline_trial_1_acc_all <- c(0.297872, 0.446809, 0.382979, 0.425532, 0.489362, 0.553191)
raw_midline_trial_1_auroc_all <- c(0.293636, 0.444545, 0.376364, 0.413636, 0.487273, 0.544545)
raw_midline_trial_12_acc_all  <- c(0.468085, 0.553191, 0.510638, 0.425532, 0.531915, 0.553191)
raw_midline_trial_12_auroc_all <- c(0.459091, 0.550000, 0.499091, 0.413636, 0.521818, 0.547273)

########################################################################################
################################ HMM (m = 2) ############################################
##############################################################################################################
Minmax_scaled_trial_1_acc_m2 <- c(0.531915, 0.595745, 0.574468, 0.595745, 0.510638, 0.553191)
Minmax_scaled_trial_1_auroc_m2 <- c(0.500000, 0.584545, 0.572727, 0.587273, 0.480000, 0.541818) 
Minmax_scaled_trial_12_acc_m2 <- c(0.531915, 0.659574, 0.510638, 0.489362, 0.361702, 0.617021)
Minmax_scaled_trial_12_auroc_m2 <- c(0.500000, 0.666364, 0.515455, 0.490000, 0.340000, 0.620909) 

zscored_scaled_trial_1_acc_m2 <- c(0.531915, 0.468085, 0.468085, 0.425532, 0.510638, 0.255319)
zscored_scaled_trial_1_auroc_m2 <- c(0.500000, 0.475455, 0.470000, 0.419091, 0.480000, 0.242727)
zscored_scaled_trial_12_acc_m2 <- c(0.531915, 0.787234, 0.531915, 0.638298, 0.340426, 0.808511)
zscored_scaled_trial_12_auroc_m2 <- c(0.500000, 0.794545, 0.540909, 0.640909, 0.320000, 0.809091)

raw_midline_trial_1_acc_m2 <- c(0.425532, 0.531915, 0.446809, 0.446809, 0.531915, 0.531915)
raw_midline_trial_1_auroc_m2 <- c(0.413636, 0.524545, 0.441818, 0.450000, 0.500000, 0.521818)
raw_midline_trial_12_acc_m2  <- c(0.340426, 0.489362, 0.382979, 0.319149, 0.531915, 0.553191)
raw_midline_trial_12_auroc_m2 <- c(0.325455, 0.490000, 0.379091, 0.316364, 0.500000, 0.555455)

########################################################################################
################################ HMM (m = 3) ############################################
##############################################################################################################
Minmax_scaled_trial_1_acc_m3 <- c(0.531915, 0.617021, 0.574468, 0.638298, 0.510638, 0.680851)
Minmax_scaled_trial_1_auroc_m3 <- c(0.500000, 0.612727, 0.564545, 0.627273, 0.480000, 0.670000)
Minmax_scaled_trial_12_acc_m3 <- c(0.531915, 0.680851, 0.574468, 0.468085, 0.361702, 0.638298)
Minmax_scaled_trial_12_auroc_m3 <- c(0.500000, 0.686364, 0.570000, 0.467273, 0.340000, 0.638182)

zscored_scaled_trial_1_acc_m3 <- c(0.531915, 0.340426, 0.425532, 0.425532, 0.510638, 0.297872)
zscored_scaled_trial_1_auroc_m3 <- c(0.500000, 0.344545, 0.419091, 0.419091, 0.48000,	0.290909)
zscored_scaled_trial_12_acc_m3 <- c(0.531915, 0.744681, 0.574468, 0.617021, 0.340426, 0.829787)
zscored_scaled_trial_12_auroc_m3 <- c(0.500000, 0.754545, 0.570000, 0.618182, 0.320000, 0.826364)

raw_midline_trial_1_acc_m3 <- c(0.212766, 0.553191, 0.382979, 0.617021, 0.531915, 0.468085)
raw_midline_trial_1_auroc_m3 <- c(0.200000, 0.544545, 0.376364, 0.615455, 0.500000, 0.461818)
raw_midline_trial_12_acc_m3  <- c(0.489362, 0.574468, 0.489362, 0.489362, 0.531915, 0.425532)
raw_midline_trial_12_auroc_m3 <- c(0.492727, 0.567273, 0.484545, 0.481818, 0.500000, 0.419091)

  
########################################################################################
################################ HMM (m = 4) ############################################
##############################################################################################################
Minmax_scaled_trial_1_acc_m4 <- c(0.531915, 0.595745, 0.574468, 0.595745, 0.404255, 0.638298)
Minmax_scaled_trial_1_auroc_m4 <-c(0.500000, 0.590000, 0.567273, 0.587273, 0.380000, 0.638182)
Minmax_scaled_trial_12_acc_m4 <- c(0.531915, 0.638298, 0.659574, 0.468085, 0.361702, 0.574468)
Minmax_scaled_trial_12_auroc_m4 <- c(0.500000, 0.651818, 0.655455, 0.467273, 0.340000, 0.570000)

zscored_scaled_trial_1_acc_m4 <- c(0.531915, 0.531915, 0.531915, 0.425532, 0.404255, 0.319149)
zscored_scaled_trial_1_auroc_m4 <- c(0.500000, 0.535455, 0.524545, 0.419091, 0.380000, 0.319091)
zscored_scaled_trial_12_acc_m4 <- c(0.531915, 0.723404, 0.638298, 0.617021, 0.340426, 0.851064)
zscored_scaled_trial_12_auroc_m4 <- c(0.500000, 0.731818, 0.635455, 0.618182, 0.320000, 0.854545)

raw_midline_trial_1_acc_m4 <- c(0.574468, 0.595745, 0.510638, 0.553191, 0.489362, 0.510638)
raw_midline_trial_1_auroc_m4 <- c(0.586364, 0.598182, 0.507273, 0.550000, 0.487273, 0.504545)
raw_midline_trial_12_acc_m4  <- c(0.297872, 0.617021, 0.531915, 0.531915, 0.361702, 0.510638)
raw_midline_trial_12_auroc_m4 <- c(0.282727, 0.618182, 0.530000, 0.532727, 0.340000, 0.510000)
  
########################################################################################
################################ HMM (m = 5) ############################################
##############################################################################################################
Minmax_scaled_trial_1_acc_m5 <- c(0.531915, 0.680851, 0.425532, 0.595745, 0.510638, 0.723404)
Minmax_scaled_trial_1_auroc_m5 <- c(0.500000, 0.675455, 0.416364, 0.587273, 0.480000, 0.718182)
Minmax_scaled_trial_12_acc_m5 <- c(0.531915, 0.638298, 0.680851, 0.468085, 0.361702, 0.680851)
Minmax_scaled_trial_12_auroc_m5 <- c(0.500000, 0.649091, 0.678182, 0.467273, 0.340000, 0.680909)

zscored_scaled_trial_1_acc_m5 <- c(0.531915, 0.468085, 0.319149, 0.425532, 0.510638, 0.340426)
zscored_scaled_trial_1_auroc_m5 <- c(0.500000, 0.470000, 0.313636, 0.419091, 0.480000, 0.333636)
zscored_scaled_trial_12_acc_m5 <- c(0.531915, 0.787234, 0.617021, 0.595745, 0.340426, 0.851064)
zscored_scaled_trial_12_auroc_m5 <- c(0.500000, 0.794545, 0.612727, 0.595455, 0.320000, 0.851818)

raw_midline_trial_1_acc_m5 <- c(0.510638, 0.404255, 0.340426, 0.382979, 0.319149, 0.489362)
raw_midline_trial_1_auroc_m5 <- c(0.504545, 0.404545, 0.336364, 0.381818, 0.300000, 0.476364)
raw_midline_trial_12_acc_m5 <- c(0.553191, 0.638298, 0.574468, 0.489362, 0.510638, 0.510638)
raw_midline_trial_12_auroc_m5 <- c(0.536364, 0.630000, 0.570000, 0.484545, 0.490909, 0.504545)

# Mergin all the results -----
eeg_results <- data.frame(Preprocessing = rep(c(rep('Minmax \n scaled', 6*4), 
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

eeg_results$HMM_parameters <- factor(eeg_results$HMM_parameters)
eeg_results[eeg_results$Models == "Polynomial \n SVM",]$Models <- 'Poly \n SVM'
eeg_results[eeg_results$Trial == "1",]$Trial <- 'Trial 1'
eeg_results[eeg_results$Trial == "12",]$Trial <- 'Trial 12'

