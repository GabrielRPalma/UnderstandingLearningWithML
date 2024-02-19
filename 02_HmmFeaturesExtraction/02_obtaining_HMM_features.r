####################################################################################################
###
### File:    04_obtaining_HMM_performance.R
### Purpose: Obtain the proportion that different Hidden Markov
###          Models with m = 2, 3, 4, 5 were the best based on
###          AIC metric.
### Authors: Gabriel Rodrigues Palma, Rafael De Andrade Moral
### Date:    08/12/22
###
####################################################################################################

# Load the packages ----
source(here('00_source.r'))

# Load the data -----
EEG_data_control <- read_excel(here("input_data/FrontalThetaData/ControlFrontalThetaData.xlsx"))
EEG_data_control<- EEG_data_control %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                               Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))
EEG_data_experiment <- read_excel(here("input_data/FrontalThetaData/ExperimentFrontalThetaData.xlsx"))
EEG_data_experiment<- EEG_data_experiment %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                                     Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))

# Working with Trial 1 -----
#################################### Control data ######################################################
### Trial 1
EEG_data_control_trial1 <- EEG_data_control %>% 
                            filter(Trial == 1) %>%
                            dplyr::select(Time, RawMidline, 
                                          ZscoredScaled, Minmaxscale, 
                                          Subject)
EEG_data_control_trial1_subjects <- EEG_data_control_trial1 %>% 
                    dplyr::select(Subject) %>%
                    unique() %>% pull(Subject)
#### Zscored scaled
control_trial1_zscored_scaled_simulation <- get_hmm_data(EEG_data = EEG_data_control_trial1, 
                                                         features_class = 'control',
                                                         features_trials = 1,
                                                         features_scale = 'ZscoredScaled', 
                                                         scalar = 1, 
                                                         eeg_data_subjects = EEG_data_control_trial1_subjects)
control_trial1_zscored_scaled_performance <- control_trial1_zscored_scaled_simulation$hmm_performance
control_trial1_zscored_scaled_features <- control_trial1_zscored_scaled_simulation$hmm_features

#### Max min scaled
control_trial1_maxmin_scaled_simulation <- get_hmm_data(EEG_data = EEG_data_control_trial1, 
                                                        features_class = 'control',
                                                        features_trials = 1,
                                                        features_scale = 'Minmaxscale', 
                                                        scalar = 1, 
                                                        eeg_data_subjects = EEG_data_control_trial1_subjects)
control_trial1_maxmin_scaled_performance <- control_trial1_maxmin_scaled_simulation$hmm_performance
control_trial1_maxmin_scaled_features <- control_trial1_maxmin_scaled_simulation$hmm_features


#### Raw midline
control_trial1_raw_simulation <- get_hmm_data(EEG_data = EEG_data_control_trial1, 
                                              features_class = 'control',
                                              features_trials = 1,
                                              features_scale = 'RawMidline', 
                                              scalar = 1E6, 
                                              eeg_data_subjects = EEG_data_control_trial1_subjects)
control_trial1_raw_scaled_performance <- control_trial1_raw_simulation$hmm_performance
control_trial1_raw_scaled_features <- control_trial1_raw_simulation$hmm_features

### Trial 12
EEG_data_control_trial12 <- EEG_data_control %>% 
  filter(Trial == 12) %>%
  dplyr::select(Time, RawMidline, 
                ZscoredScaled, Minmaxscale, 
                Subject)
EEG_data_control_trial12_subjects <- EEG_data_control_trial12 %>% 
  dplyr::select(Subject) %>%
  unique() %>% pull(Subject)
#### z-scored scaled
control_trial12_zscored_scaled_simulation <- get_hmm_data(EEG_data = EEG_data_control_trial12, 
                                                          features_class = 'control',
                                                          features_trials = 12,
                                                          features_scale = 'ZscoredScaled', 
                                                          scalar = 1, 
                                                          eeg_data_subjects = EEG_data_control_trial12_subjects)
control_trial12_zscored_scaled_performance <- control_trial12_zscored_scaled_simulation$hmm_performance
control_trial12_zscored_scaled_features <- control_trial12_zscored_scaled_simulation$hmm_features

#### Max min scaled
control_trial12_maxmin_scaled_simulation <- get_hmm_data(EEG_data = EEG_data_control_trial12, 
                                                         features_class = 'control',
                                                         features_trials = 12,
                                                         features_scale = 'Minmaxscale', 
                                                         scalar = 1, 
                                                         eeg_data_subjects = EEG_data_control_trial12_subjects)
control_trial12_maxmin_scaled_performance <- control_trial12_maxmin_scaled_simulation$hmm_performance
control_trial12_maxmin_scaled_features <- control_trial12_maxmin_scaled_simulation$hmm_features

#### Raw midline
control_trial12_raw_simulation <- get_hmm_data(EEG_data = EEG_data_control_trial12, 
                                               features_class = 'control',
                                               features_trials = 12,
                                               features_scale = 'RawMidline', 
                                               scalar = 1E6, 
                                               eeg_data_subjects = EEG_data_control_trial12_subjects)
control_trial12_raw_scaled_performance <- control_trial12_raw_simulation$hmm_performance
control_trial12_raw_scaled_features <- control_trial12_raw_simulation$hmm_features


#################################### Experiment data ######################################################
### Trial 1
EEG_data_experiment_trial1 <- EEG_data_experiment %>% 
  filter(Trial == 1) %>%
  dplyr::select(Time, RawMidline, 
                ZscoredScaled, Minmaxscale, 
                Subject)
EEG_data_experiment_trial1_subjects <- EEG_data_experiment_trial1 %>% 
  dplyr::select(Subject) %>%
  unique() %>% pull(Subject)
#### z-scored scaled
experiment_trial1_zscored_scaled_simulation <- get_hmm_data(EEG_data = EEG_data_experiment_trial1, 
                                                            features_class = 'experiment',
                                                            features_trials = 1,
                                                            features_scale = 'ZscoredScaled', 
                                                            scalar = 1, 
                                                            eeg_data_subjects = EEG_data_experiment_trial1_subjects)
experiment_trial1_zscored_scaled_performance <- experiment_trial1_zscored_scaled_simulation$hmm_performance
experiment_trial1_zscored_scaled_features <- experiment_trial1_zscored_scaled_simulation$hmm_features

#### Max min scaled
experiment_trial1_maxmin_scaled_simulation <- get_hmm_data(EEG_data = EEG_data_experiment_trial1, 
                                                           features_class = 'experiment',
                                                           features_trials = 1,
                                                           features_scale = 'Minmaxscale', 
                                                           scalar = 1, 
                                                           eeg_data_subjects = EEG_data_experiment_trial1_subjects)
experiment_trial1_maxmin_scaled_performance <- experiment_trial1_maxmin_scaled_simulation$hmm_performance
experiment_trial1_maxmin_scaled_features <- experiment_trial1_maxmin_scaled_simulation$hmm_features

#### Raw midline
experiment_trial1_raw_simulation <- get_hmm_data(EEG_data = EEG_data_experiment_trial1, 
                                                 features_class = 'experiment',
                                                 features_trials = 1,
                                                 features_scale = 'RawMidline', 
                                                 scalar = 1E6, 
                                                 eeg_data_subjects = EEG_data_experiment_trial1_subjects)
experiment_trial1_raw_scaled_performance <- experiment_trial1_raw_simulation$hmm_performance
experiment_trial1_raw_scaled_features <- experiment_trial1_raw_simulation$hmm_features

### Trial 12
EEG_data_experiment_trial12 <- EEG_data_experiment %>% 
  filter(Trial == 12) %>%
  dplyr::select(Time, RawMidline, 
                ZscoredScaled, Minmaxscale, 
                Subject)
EEG_data_experiment_trial12_subjects <- EEG_data_experiment_trial12 %>% 
  dplyr::select(Subject) %>%
  unique() %>% pull(Subject)
#### z-scored scaled
experiment_trial12_zscored_scaled_simulation <- get_hmm_data(EEG_data = EEG_data_experiment_trial12, 
                                                             features_class = 'experiment',
                                                             features_trials = 12,
                                                             features_scale = 'ZscoredScaled', 
                                                             scalar = 1, 
                                                             eeg_data_subjects = EEG_data_experiment_trial12_subjects)
experiment_trial12_zscored_scaled_performance <- experiment_trial12_zscored_scaled_simulation$hmm_performance
experiment_trial12_zscored_scaled_features <- experiment_trial12_zscored_scaled_simulation$hmm_features

#### Max min scaled
experiment_trial12_maxmin_scaled_simulation <- get_hmm_data(EEG_data = EEG_data_experiment_trial12, 
                                                            features_class = 'experiment',
                                                            features_trials = 12,
                                                            features_scale = 'Minmaxscale', 
                                                            scalar = 1, 
                                                            eeg_data_subjects = EEG_data_experiment_trial12_subjects)
experiment_trial12_maxmin_scaled_performance <- experiment_trial12_maxmin_scaled_simulation$hmm_performance
experiment_trial12_maxmin_scaled_features <- experiment_trial12_maxmin_scaled_simulation$hmm_features

#### Raw midline
experiment_trial12_raw_simulation <- get_hmm_data(EEG_data = EEG_data_experiment_trial12, 
                                                  features_class = 'experiment',
                                                  features_trials = 12,
                                                  features_scale = 'RawMidline', 
                                                  scalar = 1E6, 
                                                  eeg_data_subjects = EEG_data_experiment_trial12_subjects)
experiment_trial12_raw_scaled_performance <- experiment_trial12_raw_simulation$hmm_performance
experiment_trial12_raw_scaled_features <- experiment_trial12_raw_simulation$hmm_features

################################## Get data together and save ##################################
features_data <- rbind(control_trial1_zscored_scaled_features,
                       control_trial1_maxmin_scaled_features,
                       control_trial1_raw_scaled_features,
                       control_trial12_zscored_scaled_features,
                       control_trial12_maxmin_scaled_features,
                       control_trial12_raw_scaled_features,
                       experiment_trial1_zscored_scaled_features,
                       experiment_trial1_maxmin_scaled_features,
                       experiment_trial1_raw_scaled_features,
                       experiment_trial12_zscored_scaled_features,
                       experiment_trial12_maxmin_scaled_features,
                       experiment_trial12_raw_scaled_features)
sink('output_data/features_data.csv')
write.csv(features_data)
sink()

performance_data <- rbind(control_trial1_zscored_scaled_performance,
                       control_trial1_maxmin_scaled_performance,
                       control_trial1_raw_scaled_performance,
                       control_trial12_zscored_scaled_performance,
                       control_trial12_maxmin_scaled_performance,
                       control_trial12_raw_scaled_performance,
                       experiment_trial1_zscored_scaled_performance,
                       experiment_trial1_maxmin_scaled_performance,
                       experiment_trial1_raw_scaled_performance,
                       experiment_trial12_zscored_scaled_performance,
                       experiment_trial12_maxmin_scaled_performance,
                       experiment_trial12_raw_scaled_performance)
sink('output_data/performance_data.csv')
write.csv(performance_data)
sink()



















################################################## Old code ###########################################################################

# Extracting the features of interest -----
# control_subjects_names <- EEG_data_control %>% pull(Subject)
# control_subjects_names <- control_subjects_names %>% strsplit('')
# control_subjects_names <- lapply(control_subjects_names, FUN = function(a){
#   paste(a[c(1, length(a)-1, length(a))], collapse = '')})
# control_subjects_names <- unlist(control_subjects_names)
# 
# experiment_subjects_names <- EEG_data_experiment %>% pull(Subject)
# experiment_subjects_names <- experiment_subjects_names %>% strsplit('')
# experiment_subjects_names <- lapply(experiment_subjects_names, FUN = function(a){
#   paste(a[c(1, length(a)-1, length(a))], collapse = '')})
# experiment_subjects_names <- unlist(experiment_subjects_names)
# 
# EEG_data_control$Subject <- control_subjects_names
# control_subjects <- EEG_data_control %>% pull(Subject) %>% unique()
# EEG_data_experiment$Subject <- experiment_subjects_names
# experiment_subjects <- EEG_data_experiment %>% pull(Subject) %>% unique()
# 
# # Obtaining Hidden Markov Models parameters ----
# control_scaled <- get_hmm_data(EEG_data = EEG_data_control, 
#                                eegclass = 'control',
#                                response = 'Scaled', 
#                                eeg_data_subjects = control_subjects)$hmm_performance
# 
# experiment_scaled <- get_hmm_data(EEG_data = EEG_data_experiment, 
#                                   eegclass = 'experiment', 
#                                   response = 'Scaled',
#                                   eeg_data_subjects = experiment_subjects)$hmm_performance
# 
# hmm_performance_scaled <- rbind(control_scaled, experiment_scaled)
# winner_indices <- apply(as.matrix(hmm_performance_scaled), 1, FUN = function(x) { which(x == min(x)) })
# 
# scaled_performance <- table(winner_indices)
# names(scaled_performance)
# base_performance <- c(0, 0, 0, 0)
# names(base_performance) <- c('1', '2', '3', '4')
# base_performance[names(scaled_performance)] <- scaled_performance/sum(scaled_performance)
# 
# control_maxmin <- get_hmm_data(EEG_data = EEG_data_control, 
#                                eegclass = 'control',
#                                response = 'Maxmin', 
#                                eeg_data_subjects = control_subjects)$hmm_performance
# experiment_maxmin <- get_hmm_data(EEG_data = EEG_data_experiment, 
#                                   eegclass = 'experiment', 
#                                   response = 'Maxmin',
#                                   eeg_data_subjects = experiment_subjects)$hmm_performance
# 
# hmm_features_maxmin <- rbind(control_maxmin, experiment_maxmin)
# 
# sink('hmm_features_maxmin.csv')
# write.csv(hmm_features_maxmin)
# sink()