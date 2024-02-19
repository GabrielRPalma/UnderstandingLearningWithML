####################################################################################################
###
### File:    01_data_visualisation.R
### Purpose: Obtaining the features related to the coordinates.
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    05/04/23
###
####################################################################################################
# Loading packages and functions -----
source(here('00_source.r'))

## Visualising the HMM parameters -----
hmm_features <- read.csv('output_data/New_data/All_features.csv', header = T)[,-1]
all_features_t1 <- read.csv('output_data/New_data/CoordinatesPlusEEGData/ZscoredScaledPlusCoordinateTrial1.csv', header = T)[,-1]
all_features_t1$Trial <- rep('Trial 1', nrow(all_features_t1))
all_features_t12 <- read.csv('output_data/New_data/CoordinatesPlusEEGData/ZscoredScaledPlusCoordinateTrial12.csv', header = T)[,-1]
all_features_t12$Trial <- rep('Trial 12', nrow(all_features_t12))

########################################################################################################
################################ LMM slope and mu m 4 ################################################################
################################################################################################
slope_m4 <- rbind(all_features_t1, 
                  all_features_t12) %>%
  dplyr::select(Subject, Class, Trial,
                MM_Hess, dis_sum)

slope_m4$Class <- factor(slope_m4$Class)
levels(slope_m4$Class) <- c("Non-learner", "Learner")
slope_m4 %>%
  ggplot(mapping = aes(x = MM_Hess, y = dis_sum, colour = Class)) +
  geom_point() + theme_new() +
  ylab("Random slopes predicted \n by the linear mixed-effects model") +
  xlab('Path length (Vm)') +
  facet_wrap(~Trial, scales="free") +
  scale_color_manual(values = c("#4dbd05", "#232323")) +
  scale_fill_manual(values = c("#4dbd05", "#232323"))

ggsave('Plots/LEMSlopeVSPathLengthPerTrial.png', dpi = 400, width = 8, height = 6)
########################################################################################################
################################ Peak features trial 1 ################################################################
################################################################################################
## Minmaxscale
peak_features_t1 <- hmm_features %>%
  filter(features_scale == 'Minmaxscale', features_trials == '1') %>%
  dplyr::select(Subject, features_class, MM_Intercept, 
                MM_Hess, average_peak_magnitude, 
                average_peak_curvature)
ggpairs(peak_features_t1, columns = 3:6, ggplot2::aes(colour = features_class))+
  theme_new() +
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

## RawMidline
peak_features_t1 <- hmm_features %>%
  filter(features_scale == 'RawMidline', features_trials == '1') %>%
  dplyr::select(Subject, features_class, MM_Intercept, 
                MM_Hess, average_peak_magnitude, 
                average_peak_curvature)
ggpairs(peak_features_t1, columns = 3:6, ggplot2::aes(colour = features_class))+
  theme_new() +
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

## ZscoredScaled
peak_features_t1 <- hmm_features %>%
  filter(features_scale == 'ZscoredScaled', features_trials == '1') %>%
  dplyr::select(Subject, features_class, MM_Intercept, 
                MM_Hess, average_peak_magnitude, 
                average_peak_curvature)
ggpairs(peak_features_t1, columns = 3:6, ggplot2::aes(colour = features_class))+
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))
########################################################################################################
################################ Peak features trial 12 ################################################################
################################################################################################
## Minmaxscale
peak_features_t12 <- hmm_features %>%
  filter(features_scale == 'Minmaxscale', features_trials == '12') %>%
  dplyr::select(Subject, features_class, MM_Intercept, 
                MM_Hess, average_peak_magnitude, 
                average_peak_curvature)
ggpairs(peak_features_t12, columns = 3:6, ggplot2::aes(colour = features_class)) +
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

## RawMidline
peak_features_t12 <- hmm_features %>%
  filter(features_scale == 'RawMidline', features_trials == '12') %>%
  dplyr::select(Subject, features_class, MM_Intercept, 
                MM_Hess, average_peak_magnitude, 
                average_peak_curvature)
ggpairs(peak_features_t12, columns = 3:6, ggplot2::aes(colour = features_class))+
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

## ZscoredScaled
peak_features_t12 <- hmm_features %>%
  filter(features_scale == 'ZscoredScaled', features_trials == '12') %>%
  dplyr::select(Subject, features_class, MM_Intercept, 
                MM_Hess, average_peak_magnitude, 
                average_peak_curvature)
ggpairs(peak_features_t12, columns = 3:6, ggplot2::aes(colour = features_class))+
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

########################################################################################################
################################ HMM (m = 2) features trial 1 ################################################################
################################################################################################
## Minmaxscale
hmm_features_t1 <- hmm_features %>%
  filter(features_scale == 'Minmaxscale', features_trials == '1') %>%
  dplyr::select(Subject, features_class, hmm2_mean.1, 
                hmm2_mean.2, hmm2_sd.1, hmm2_sd.2, hmm2_p1)
ggpairs(hmm_features_t1, columns = 3:7, ggplot2::aes(colour = features_class))+
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

## RawMidline
hmm_features_t1 <- hmm_features %>%
  filter(features_scale == 'RawMidline', features_trials == '1') %>%
  dplyr::select(Subject, features_class, hmm2_mean.1, 
                hmm2_mean.2, hmm2_sd.1, hmm2_sd.2, hmm2_p1)
ggpairs(hmm_features_t1, columns = 3:7, ggplot2::aes(colour = features_class))+
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

## ZscoredScaled
hmm_features_t1 <- hmm_features %>%
  filter(features_scale == 'ZscoredScaled', features_trials == '1') %>%
  dplyr::select(Subject, features_class, hmm2_mean.1, 
                hmm2_mean.2, hmm2_sd.1, hmm2_sd.2, hmm2_p1)
ggpairs(hmm_features_t1, columns = 3:6, ggplot2::aes(colour = features_class))+
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

########################################################################################################
################################ HMM (m = 2) features trial 12 ################################################################
################################################################################################
## Minmaxscale
hmm_features_t1 <- hmm_features %>%
  filter(features_scale == 'Minmaxscale', features_trials == '12') %>%
  dplyr::select(Subject, features_class, hmm2_mean.1, 
                hmm2_mean.2, hmm2_sd.1, hmm2_sd.2, hmm2_p1)
ggpairs(hmm_features_t1, columns = 3:7, ggplot2::aes(colour = features_class))+
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

## RawMidline
hmm_features_t1 <- hmm_features %>%
  filter(features_scale == 'RawMidline', features_trials == '12') %>%
  dplyr::select(Subject, features_class, hmm2_mean.1, 
                hmm2_mean.2, hmm2_sd.1, hmm2_sd.2, hmm2_p1)
ggpairs(hmm_features_t1, columns = 3:7, ggplot2::aes(colour = features_class))+
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))

## ZscoredScaled
hmm_features_t1 <- hmm_features %>%
  filter(features_scale == 'ZscoredScaled', features_trials == '12') %>%
  dplyr::select(Subject, features_class, hmm2_mean.1, 
                hmm2_mean.2, hmm2_sd.1, hmm2_sd.2, hmm2_p1)
ggpairs(hmm_features_t1, columns = 3:6, ggplot2::aes(colour = features_class))+
  theme_new()+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323"))
