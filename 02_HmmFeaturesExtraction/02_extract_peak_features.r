####################################################################################################
###
### File:    05_extract_peak_features.R
### Purpose: Extracting peak features from each time series 
### Authors: Gabriel Rodrigues Palma, Rafael De Andrade Moral
### Date:    09/03/23
###
####################################################################################################

# Load the packages ----
source(here('00_source.r'))

# Load the data -----
hmm_features_data <- read.csv(here('output_data/features_data.csv'))[,-1]

EEG_data_control <- read_excel(here("input_data/FrontalThetaData/ControlFrontalThetaData.xlsx"))
EEG_data_control <- EEG_data_control %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                               Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))
EEG_data_experiment <- read_excel(here("input_data/FrontalThetaData/ExperimentFrontalThetaData.xlsx"))
EEG_data_experiment<- EEG_data_experiment %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                                     Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))

####################################################################################################
eeg_experiment_trial_1 <- EEG_data_experiment %>% 
                            filter(Trial == 1)
collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                 eeg_experiment_trial_1, 
                                               features_class = 'experiment', 
                                               features_scale = 'RawMidline',
                                               features_trials = 1)
subject <- 'CTEEG058'
dat <- collected_peaks_curvatures$dat
peak <- collected_peaks_curvatures$peak
peak_index <- collected_peaks_curvatures$peak_index
eeg_experiment_trial_1 %>%
  filter(Subject == subject) %>%
  ggplot(aes(x = Time, y = RawMidline)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~ Subject, scales = "free", nrow = 5) +
  geom_point(data = data.frame(RawMidline = peak[[subject]],
                               Time = dat[[subject]]$Time[peak_index[[subject]]]),
             col = 2, alpha = .4)

  
# fit <- lmer(data = test, peak ~ hess + (hess | Subject))
# ranef(fit)

# Obtaining the peak data from control and experiment datasets -----
######################## RawMidline ####################################
## Control dataset
### Trial 1
eeg_control_trial_1 <- EEG_data_control %>% 
  filter(Trial == 1)
RawMidline_eeg_control_trial_1_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                               eeg_control_trial_1, 
                                               features_class = 'control', 
                                               features_scale = 'RawMidline',
                                               features_trials = 1)
RawMidline_peak_hess_data_control_trial_1 <- collect_peak_parameters(
  RawMidline_eeg_control_trial_1_collected_peaks_curvatures, 
  features_class = 'control', 
  features_scale = 'RawMidline', 
  features_trials = 1)

### Trial 1
eeg_control_trial_12 <- EEG_data_control %>% 
  filter(Trial == 12)
RawMidline_eeg_control_trial_12_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                    eeg_control_trial_12, 
                                                                    features_class = 'control', 
                                                                    features_scale = 'RawMidline',
                                                                    features_trials = 12)
RawMidline_peak_hess_data_control_trial_12 <- collect_peak_parameters(
  RawMidline_eeg_control_trial_12_collected_peaks_curvatures, 
  features_class = 'control', 
  features_scale = 'RawMidline', 
  features_trials = 12)

## Experiment data
### Trial 1
eeg_experiment_trial_1 <- EEG_data_experiment %>% 
  filter(Trial == 1)
RawMidline_eeg_experiment_trial_1_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                     eeg_experiment_trial_1, 
                                                                     features_class = 'experiment', 
                                                                     features_scale = 'RawMidline',
                                                                     features_trials = 1)
RawMidline_peak_hess_data_experiment_trial_1 <- collect_peak_parameters(
  RawMidline_eeg_experiment_trial_1_collected_peaks_curvatures, 
  features_class = 'experiment', 
  features_scale = 'RawMidline', 
  features_trials = 1)

### Trial 1
eeg_experiment_trial_12 <- EEG_data_experiment %>% 
  filter(Trial == 12)
RawMidline_eeg_experiment_trial_12_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                      eeg_experiment_trial_12, 
                                                                      features_class = 'experiment',
                                                                      features_scale = 'RawMidline',
                                                                      features_trials = 12)
RawMidline_peak_hess_data_experiment_trial_12 <- collect_peak_parameters(
  RawMidline_eeg_experiment_trial_12_collected_peaks_curvatures, 
  features_class = 'experiment', 
  features_scale = 'RawMidline', 
  features_trials = 12)

######################## Scaled ####################################
## Control dataset
### Trial 1
eeg_control_trial_1 <- EEG_data_control %>% 
  filter(Trial == 1)
Scaled_eeg_control_trial_1_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                     eeg_control_trial_1, 
                                                                   features_class = 'control', 
                                                                   features_scale = 'ZscoredScaled',
                                                                   features_trials = 1)
Scaled_peak_hess_data_control_trial_1 <- collect_peak_parameters(
  Scaled_eeg_control_trial_1_collected_peaks_curvatures, 
  features_class = 'control', 
  features_scale = 'ZscoredScaled', 
  features_trials = 1)

### Trial 1
eeg_control_trial_12 <- EEG_data_control %>% 
  filter(Trial == 12)
Scaled_eeg_control_trial_12_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                      eeg_control_trial_12, 
                                                                    features_class = 'control', 
                                                                    features_scale = 'ZscoredScaled',
                                                                    features_trials = 12)
Scaled_peak_hess_data_control_trial_12 <- collect_peak_parameters(
  Scaled_eeg_control_trial_12_collected_peaks_curvatures, 
  features_class = 'control', 
  features_scale = 'ZscoredScaled', 
  features_trials = 12)

## Experiment data
### Trial 1
eeg_experiment_trial_1 <- EEG_data_experiment %>% 
  filter(Trial == 1)
Scaled_eeg_experiment_trial_1_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                        eeg_experiment_trial_1, 
                                                                      features_class = 'experiment', 
                                                                      features_scale = 'ZscoredScaled',
                                                                      features_trials = 1)
Scaled_peak_hess_data_experiment_trial_1 <- collect_peak_parameters(
  Scaled_eeg_experiment_trial_1_collected_peaks_curvatures, 
  features_class = 'experiment', 
  features_scale = 'ZscoredScaled', 
  features_trials = 1)

### Trial 1
eeg_experiment_trial_12 <- EEG_data_experiment %>% 
  filter(Trial == 12)
Scaled_eeg_experiment_trial_12_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                         eeg_experiment_trial_12, 
                                                                       features_class = 'experiment',
                                                                       features_scale = 'ZscoredScaled',
                                                                       features_trials = 12)
Scaled_peak_hess_data_experiment_trial_12 <- collect_peak_parameters(
  Scaled_eeg_experiment_trial_12_collected_peaks_curvatures, 
  features_class = 'experiment', 
  features_scale = 'ZscoredScaled', 
  features_trials = 12)

######################## Max min scaled ####################################
## Control dataset
### Trial 1
eeg_control_trial_1 <- EEG_data_control %>% 
  filter(Trial == 1)
MaxminScaled_eeg_control_trial_1_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                     eeg_control_trial_1, 
                                                                   features_class = 'control', 
                                                                   features_scale = 'Minmaxscale',
                                                                   features_trials = 1)
MaxminScaled_peak_hess_data_control_trial_1 <- collect_peak_parameters(
  MaxminScaled_eeg_control_trial_1_collected_peaks_curvatures, 
  features_class = 'control', 
  features_scale = 'Minmaxscale', 
  features_trials = 1)

### Trial 1
eeg_control_trial_12 <- EEG_data_control %>% 
  filter(Trial == 12)
MaxminScaled_eeg_control_trial_12_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                      eeg_control_trial_12, 
                                                                    features_class = 'control', 
                                                                    features_scale = 'Minmaxscale',
                                                                    features_trials = 12)
MaxminScaled_peak_hess_data_control_trial_12 <- collect_peak_parameters(
  MaxminScaled_eeg_control_trial_12_collected_peaks_curvatures, 
  features_class = 'control', 
  features_scale = 'Minmaxscale', 
  features_trials = 12)

## Experiment data
### Trial 1
eeg_experiment_trial_1 <- EEG_data_experiment %>% 
  filter(Trial == 1)
MaxminScaled_eeg_experiment_trial_1_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                        eeg_experiment_trial_1, 
                                                                      features_class = 'experiment', 
                                                                      features_scale = 'Minmaxscale',
                                                                      features_trials = 1)
MaxminScaled_peak_hess_data_experiment_trial_1 <- collect_peak_parameters(
  MaxminScaled_eeg_experiment_trial_1_collected_peaks_curvatures, 
  features_class = 'experiment', 
  features_scale = 'Minmaxscale', 
  features_trials = 1)

### Trial 1
eeg_experiment_trial_12 <- EEG_data_experiment %>% 
  filter(Trial == 12)
MaxminScaled_eeg_experiment_trial_12_collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                                         eeg_experiment_trial_12, 
                                                                       features_class = 'experiment',
                                                                       features_scale = 'Minmaxscale',
                                                                       features_trials = 12)
MaxminScaled_peak_hess_data_experiment_trial_12 <- collect_peak_parameters(
  MaxminScaled_eeg_experiment_trial_12_collected_peaks_curvatures, 
  features_class = 'experiment', 
  features_scale = 'Minmaxscale', 
  features_trials = 12)

################## Creating the final datasets ##########################################
final_peaks_dataset <- rbind(MaxminScaled_peak_hess_data_control_trial_1,
                             MaxminScaled_peak_hess_data_control_trial_12, 
                             MaxminScaled_peak_hess_data_experiment_trial_1, 
                             MaxminScaled_peak_hess_data_experiment_trial_12, 
                             
                             Scaled_peak_hess_data_control_trial_1,
                             Scaled_peak_hess_data_control_trial_12, 
                             Scaled_peak_hess_data_experiment_trial_1, 
                             Scaled_peak_hess_data_experiment_trial_12, 
                             
                             RawMidline_peak_hess_data_control_trial_1,
                             RawMidline_peak_hess_data_control_trial_12, 
                             RawMidline_peak_hess_data_experiment_trial_1, 
                             RawMidline_peak_hess_data_experiment_trial_12)

Final_data <- inner_join(final_peaks_dataset, 
              hmm_features_data, 
              by = c('Subject', 'features_class', 
                     'features_trials', 'features_scale'), 
              multiple = "all")
sink('output_data/New_data/All_features.csv')
write.csv(Final_data)
sink()

##################### Split datasets ######################
RawMidlineTrial1 <- Final_data %>% 
                    filter(features_scale == 'RawMidline', 
                           features_trials == 1) %>%
  dplyr::select(-c(features_scale, 
                   features_trials))
sink('output_data/New_data/SplitDatasets/RawMidlineTrial1.csv')
write.csv(RawMidlineTrial1)
sink()


MinmaxscaleTrial1 <- Final_data %>% 
  filter(features_scale == 'Minmaxscale', 
         features_trials == 1) %>%
  dplyr::select(-c(features_scale, 
                   features_trials))
sink('output_data/New_data/SplitDatasets/MinmaxscaleTrial1.csv')
write.csv(MinmaxscaleTrial1)
sink()

ZscoredScaledTrial1 <- Final_data %>% 
  filter(features_scale == 'ZscoredScaled', 
         features_trials == 1) %>%
  dplyr::select(-c(features_scale, 
                   features_trials))
sink('output_data/New_data/SplitDatasets/ZscoredScaledTrial1.csv')
write.csv(ZscoredScaledTrial1)
sink()

#### Trial 12
RawMidlineTrial12 <- Final_data %>% 
  filter(features_scale == 'RawMidline', 
         features_trials == 12) %>%
  dplyr::select(-c( features_scale, 
                   features_trials))
sink('output_data/New_data/SplitDatasets/RawMidlineTrial12.csv')
write.csv(RawMidlineTrial12)
sink()


MinmaxscaleTrial12 <- Final_data %>% 
  filter(features_scale == 'Minmaxscale', 
         features_trials == 12) %>%
  dplyr::select(-c(features_scale, 
                   features_trials))
sink('output_data/New_data/SplitDatasets/MinmaxscaleTrial12.csv')
write.csv(MinmaxscaleTrial12)
sink()

ZscoredScaledTrial12 <- Final_data %>% 
  filter(features_scale == 'ZscoredScaled', 
         features_trials == 12) %>%
  dplyr::select(-c(features_scale, 
                   features_trials))
sink('output_data/New_data/SplitDatasets/ZscoredScaledTrial12.csv')
write.csv(ZscoredScaledTrial12)
sink()
