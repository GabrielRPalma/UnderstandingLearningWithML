####################################################################################################
###
### File:    06_trial_coordinates_differences.r
### Purpose: Computing the differences in trials
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    03/08/23
###
####################################################################################################
# Loading the packages -----
source('00_source.r')

# Loading dataset -----
coordinate_control_data <- read_excel(here("input_data/CoordinateData/ControlCoordinatesData.xlsx"))
coordinate_experiment_data <- read_excel(here("input_data/CoordinateData/ExperientCoordinatesData.xlsx"))
OriginalCoordinateData <- rbind(coordinate_control_data, coordinate_experiment_data)

CoordinatesDataTrial1 <- read.csv(here("output_data/New_data/CoordinateDatasets/Coordinates_data_Trial1.csv"))[, -1]
CoordinatesDataTrial1$Trial <- rep('1', nrow(CoordinatesDataTrial1))

CoordinatesDataTrial12 <- read.csv(here("output_data/New_data/CoordinateDatasets/Coordinates_data_Trial12.csv"))[, -1]
CoordinatesDataTrial12$Trial <- rep('12', nrow(CoordinatesDataTrial12))

CoordinatesData <- rbind(CoordinatesDataTrial1, 
                         CoordinatesDataTrial12)
CoordinatesData$Trial <- factor(CoordinatesData$Trial)
levels(CoordinatesData$Trial) <- c('Trial 1', 'Trial 12')
OriginalCoordinateData$Trial <- factor(OriginalCoordinateData$Trial)
levels(OriginalCoordinateData$Trial) <- c('Trial 1', 'Trial 12')

EEG_data_control <- read_excel(here("input_data/FrontalThetaData/ControlFrontalThetaData.xlsx"))
EEG_data_control <- EEG_data_control %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                                Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))
EEG_data_experiment <- read_excel(here("input_data/FrontalThetaData/ExperimentFrontalThetaData.xlsx"))
EEG_data_experiment<- EEG_data_experiment %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                                     Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))

# Fitting models to check differences among Trials -----
######################################################################
########## Checking differences for total distance ###################
######################################################################

m1 <- gamlss(dis_sum ~ Trial + Class, 
       family =  'NO', data = CoordinatesData)
m2 <- gamlss(dis_sum ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m3 <- gamlss(dis_sum ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m4 <- gamlss(dis_sum ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m5 <- gamlss(dis_sum ~ Trial + Class, 
             sigma.formula = dis_sum ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m6 <- gamlss(dis_sum ~ Trial + Class, 
             sigma.formula = dis_sum ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m7 <- gamlss(dis_sum ~ Trial + Class, 
             sigma.formula = dis_sum ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m8 <- gamlss(dis_sum ~ Trial + Class, 
             sigma.formula = dis_sum ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
a <- compare_models_ic(aics = AIC(m1, m2,
                                  m3, m4, 
                                  m5, m6, 
                                  m7, m8), 
                       bics = BIC(m1, m2,
                                  m3, m4, 
                                  m5, m6, 
                                  m7, m8))
a[a$aic == min(a$aic),]
wp(m8, Trial ~ Class)

## Is there interaction for location?
complex_model <- gamlss(dis_sum ~ Trial * Class, 
                 sigma.formula = dis_sum ~ Trial + Class, 
                 family =  'GA', data = CoordinatesData)
simple_model <- gamlss(dis_sum ~ Trial + Class, 
                sigma.formula = dis_sum ~ Trial + Class, 
                family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

## Is there interaction for dispersion?
complex_model <- gamlss(dis_sum ~ Trial + Class, 
                        sigma.formula = dis_sum ~ Trial * Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(dis_sum ~ Trial + Class, 
                       sigma.formula = dis_sum ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

# Is there difference in trials?
complex_model <- gamlss(dis_sum ~ Trial + Class, 
                        sigma.formula = dis_sum ~ Trial * Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(dis_sum ~ Class, 
                       sigma.formula = dis_sum ~ Trial * Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

# Is there difference in Class?
complex_model <- gamlss(dis_sum ~ Trial+Class, 
                        sigma.formula = dis_sum ~ Trial * Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(dis_sum ~ Trial, 
                       sigma.formula = dis_sum ~ Trial * Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

## Visualisation 
CoordinatesData$Class <- factor(CoordinatesData$Class)
levels(CoordinatesData$Class) <- c("Non-learner", "Learner")
CoordinatesData %>%
  ggplot(mapping = aes(x = Trial, y = dis_sum, 
                       colour = Class)) +
  theme_new() +
  geom_boxplot() + 
  ylab('Path length (Vm)') +
  xlab('') +
  scale_color_manual(values = c("#4dbd05", "#232323"))
ggsave('Plots/Final_paper/TotalDistanceData.png', dpi = 400, width = 6, height = 4)

######################################################################
########## Checking differences for total angle shift ###################
######################################################################

m1 <- gamlss(angle_sum ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m2 <- gamlss(angle_sum ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m3 <- gamlss(angle_sum ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m4 <- gamlss(angle_sum ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m5 <- gamlss(angle_sum ~ Trial + Class, 
             sigma.formula = angle_sum ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m6 <- gamlss(angle_sum ~ Trial + Class, 
             sigma.formula = angle_sum ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m7 <- gamlss(angle_sum ~ Trial + Class, 
             sigma.formula = angle_sum ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m8 <- gamlss(angle_sum ~ Trial + Class, 
             sigma.formula = angle_sum ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
a <- compare_models_ic(aics = AIC(m1, m2,
                                  m3, m4, 
                                  m5, m6, 
                                  m7, m8), 
                       bics = BIC(m1, m2,
                                  m3, m4, 
                                  m5, m6, 
                                  m7, m8))
a[a$aic == min(a$aic),]
wp(m8, Trial ~ Class)

## Is there interaction for location?
complex_model <- gamlss(angle_sum ~ Trial * Class, 
                        sigma.formula = angle_sum ~ Trial + Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(angle_sum ~ Trial + Class, 
                       sigma.formula = angle_sum ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

## Is there interaction for dispersion?
complex_model <- gamlss(angle_sum ~ Trial + Class, 
                        sigma.formula = angle_sum ~ Trial * Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(angle_sum ~ Trial + Class, 
                       sigma.formula = angle_sum ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

# Is there difference in trials?
complex_model <- gamlss(angle_sum ~ Trial + Class, 
                        sigma.formula = angle_sum ~ Trial + Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(angle_sum ~ Class, 
                       sigma.formula = angle_sum ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

# Is there difference in Class?
complex_model <- gamlss(angle_sum ~ Trial+Class, 
                        sigma.formula = angle_sum ~ Trial + Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(angle_sum ~ Trial, 
                       sigma.formula = angle_sum ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

## Visualisation 
CoordinatesData %>%
  ggplot(mapping = aes(x = Trial, y = angle_sum, 
                       colour = Class)) +
  theme_new() +
  geom_boxplot() + 
  ylab('Total angle shift') +
  xlab('') +
  scale_color_manual(values = c("#4dbd05", "#232323"))
ggsave('Plots/Final_paper/AngleShiftData.png', dpi = 400, width = 6, height = 4)


######################################################################
########## Checking differences for average speed ###################
######################################################################

m1 <- gamlss(average_speed ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m2 <- gamlss(average_speed ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m3 <- gamlss(average_speed ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m4 <- gamlss(average_speed ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m5 <- gamlss(average_speed ~ Trial + Class, 
             sigma.formula = average_speed ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m6 <- gamlss(average_speed ~ Trial + Class, 
             sigma.formula = average_speed ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m7 <- gamlss(average_speed ~ Trial + Class, 
             sigma.formula = average_speed ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m8 <- gamlss(average_speed ~ Trial + Class, 
             sigma.formula = average_speed ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
a <- compare_models_ic(aics = AIC(m1, m2,
                                  m3, m4, 
                                  m5, m6, 
                                  m7, m8), 
                       bics = BIC(m1, m2,
                                  m3, m4, 
                                  m5, m6, 
                                  m7, m8))
a[a$aic == min(a$aic),]
wp(m8, Trial ~ Class)

## Is there interaction for location?
complex_model <- gamlss(average_speed ~ Trial * Class, 
                        sigma.formula = average_speed ~ Trial + Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(average_speed ~ Trial + Class, 
                       sigma.formula = average_speed ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

## Is there interaction for dispersion?
complex_model <- gamlss(average_speed ~ Trial + Class, 
                        sigma.formula = average_speed ~ Trial * Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(average_speed ~ Trial + Class, 
                       sigma.formula = average_speed ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

# Is there difference in trials?
complex_model <- gamlss(average_speed ~ Trial + Class, 
                        sigma.formula = average_speed ~ Trial + Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(average_speed ~ Class, 
                       sigma.formula = average_speed ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

# Is there difference in Class?
complex_model <- gamlss(average_speed ~ Trial+Class, 
                        sigma.formula = average_speed ~ Trial + Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(average_speed ~ Trial, 
                       sigma.formula = average_speed ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

## Visualisation 
CoordinatesData %>%
  ggplot(mapping = aes(x = Trial, y = average_speed, 
                       colour = Class)) +
  theme_new() +
  geom_boxplot() + 
  ylab('Average speed') +
  xlab('') +
  scale_color_manual(values = c("#4dbd05", "#232323"))
ggsave('Plots/Final_paper/AverageSpeedData.png', dpi = 400, width = 6, height = 4)

######################################################################
########## Checking differences for idle time ###################
######################################################################

m1 <- gamlss(idle_time ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m2 <- gamlss(idle_time ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m3 <- gamlss(idle_time ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m4 <- gamlss(idle_time ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m5 <- gamlss(idle_time ~ Trial + Class, 
             sigma.formula = idle_time ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m6 <- gamlss(idle_time ~ Trial + Class, 
             sigma.formula = idle_time ~ Trial + Class, 
             family =  'NO', data = CoordinatesData)
m7 <- gamlss(idle_time ~ Trial + Class, 
             sigma.formula = idle_time ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
m8 <- gamlss(idle_time ~ Trial + Class, 
             sigma.formula = idle_time ~ Trial + Class, 
             family =  'GA', data = CoordinatesData)
a <- compare_models_ic(aics = AIC(m1, m2,
                                  m3, m4, 
                                  m5, m6, 
                                  m7, m8), 
                       bics = BIC(m1, m2,
                                  m3, m4, 
                                  m5, m6, 
                                  m7, m8))
a[a$aic == min(a$aic),]
wp(m8, Trial ~ Class)

## Is there interaction for location?
complex_model <- gamlss(idle_time ~ Trial * Class, 
                        sigma.formula = idle_time ~ Trial + Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(idle_time ~ Trial + Class, 
                       sigma.formula = idle_time ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

## Is there interaction for dispersion?
complex_model <- gamlss(idle_time ~ Trial + Class, 
                        sigma.formula = idle_time ~ Trial * Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(idle_time ~ Trial + Class, 
                       sigma.formula = idle_time ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

# Is there difference in trials?
complex_model <- gamlss(idle_time ~ Trial * Class, 
                        sigma.formula = idle_time ~ Trial + Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(idle_time ~ Class, 
                       sigma.formula = idle_time ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

# Is there difference in Class?
complex_model <- gamlss(idle_time ~ Trial*Class, 
                        sigma.formula = idle_time ~ Trial + Class, 
                        family =  'GA', data = CoordinatesData)
simple_model <- gamlss(idle_time ~ Trial, 
                       sigma.formula = idle_time ~ Trial + Class, 
                       family =  'GA', data = CoordinatesData)
LR.test(simple_model, complex_model)

######## Trials #########
CoordinatesData_t1 <- CoordinatesData %>%
  filter(Trial == 'Trial 1')
complex_model <- gamlss(idle_time ~ Class, 
                        sigma.formula = idle_time ~  Class, 
                        family =  'GA', data = CoordinatesData_t1)
simple_model <- gamlss(idle_time ~ 1, 
                       sigma.formula = idle_time ~ Class, 
                       family =  'GA', data = CoordinatesData_t1)
LR.test(simple_model, complex_model)

CoordinatesData_t12 <- CoordinatesData %>%
  filter(Trial == 'Trial 12')
complex_model <- gamlss(idle_time ~ Class, 
                        sigma.formula = idle_time ~ Class, 
                        family =  'GA', data = CoordinatesData_t12)
simple_model <- gamlss(idle_time ~ 1, 
                       sigma.formula = idle_time ~ Class, 
                       family =  'GA', data = CoordinatesData_t12)
LR.test(simple_model, complex_model)

##### Group effect ######
CoordinatesData_control <- CoordinatesData %>%
  filter(Class == 'control')
complex_model <- gamlss(idle_time ~ Trial, 
                        sigma.formula = idle_time ~ Trial , 
                        family =  'GA', data = CoordinatesData_control)
simple_model <- gamlss(idle_time ~ 1, 
                       sigma.formula = idle_time ~ Trial, 
                       family =  'GA', data = CoordinatesData_control)
LR.test(simple_model, complex_model)

CoordinatesData_experiment <- CoordinatesData %>%
  filter(Class == 'experiment')

complex_model <- gamlss(idle_time ~ Trial, 
                        sigma.formula = idle_time ~ Trial, 
                        family =  'GA', data = CoordinatesData_experiment)
simple_model <- gamlss(idle_time ~ 1, 
                       sigma.formula = idle_time ~ Trial, 
                       family =  'GA', data = CoordinatesData_experiment)
LR.test(simple_model, complex_model)

## Visualisation 
CoordinatesData %>%
  ggplot(mapping = aes(x = Trial, y = idle_time, 
                       colour = Class)) +
  theme_new() +
  geom_boxplot() + 
  ylab('Idle time') +
  xlab('') +
  scale_color_manual(values = c("#4dbd05", "#232323"))
ggsave('Plots/Final_paper/IdleTimeData.png', dpi = 400, width = 6, height = 4)
############################################################################
################### Visualisation Coordinates ##########################################
############################################################################
OriginalCoordinateData %>%
  ggplot(mapping = aes(x = X, y = Y, colour = Class)) +
  geom_point() +
  theme_new()  +
  facet_wrap(~Trial) +
  scale_color_manual(values = c("#4dbd05", "#232323"))
ggsave('Plots/Final_paper/ClassesCoordinatesData.png', dpi = 400, width = 6, height = 4)

############################################################################
################### Visualisation Time series Subjects ##########################################
############################################################################
unique(EEG_data_experiment$Subject)
CTEEG002 <- EEG_data_experiment %>%
  filter(Trial == 12 &
           Subject == 'CTEEG002')
CTEEG002$X <- 1:nrow(CTEEG002)
CTEEG002 %>%
  filter(Trial == 12 &
         Subject == 'CTEEG002') %>%
  ggplot(aes(x = X, y = ZscoredScaled)) +
  theme_new() +
  geom_line(aes(colour = "#232323")) + 
  xlab('Observations') +
  ylab('Z-scored scale theta') +
  scale_color_manual(values = c("#232323"))
ggsave('Plots/Final_paper/experiment_Time_series.png', dpi = 400, width = 8, height = 5)

unique(EEG_data_control$Subject)
CTEEG033 <- EEG_data_control %>%
  filter(Trial == 12 &
           Subject == 'CTEEG033') 
CTEEG033$X <- 1:nrow(CTEEG033)
CTEEG033 %>%
  ggplot(aes(x = X, y = ZscoredScaled)) +
  theme_new() +
  geom_line(aes(colour = "#4dbd05")) + 
  xlab('Observations') +
  ylab('Z-scored scale theta') +
  scale_color_manual(values = c("#4dbd05")) +
  scale_fill_manual(values = c("#4dbd05"))
ggsave('Plots/Final_paper/control_Time_series.png', dpi = 400, width = 8, height = 5)

############################################################################
########## Visualisation Peak frequency and Hessian ##########################################
############################################################################
collected_peaks_hess_control <- obtain_peak_hess(eeg_data = 
                                                 EEG_data_control %>%
                                                 dplyr::select(Time, Subject, 
                                                               ZscoredScaled, Class, 
                                                               Trial) %>%
                                                 filter(Trial == 12 &
                                                          Subject == 'CTEEG033'), 
                                               features_class = 'Control', 
                                               features_scale = 'ZscoredScaled',
                                               features_trials = 12)$result
collected_peaks_hess_experiment <- obtain_peak_hess(eeg_data = 
                                                      EEG_data_experiment %>%
                                              dplyr::select(Time, Subject, 
                                                            ZscoredScaled, Class, 
                                                            Trial) %>%
                                              filter(Trial == 12 &
                                                       Subject == 'CTEEG002'), 
                                            features_class = 'Experiment', 
                                            features_scale = 'ZscoredScaled',
                                            features_trials = 12)$result
peak_hess_data <- rbind(collected_peaks_hess_control, 
                        collected_peaks_hess_experiment)
colnames(peak_hess_data) <- c("Subject", "peak", "hess",
                              "Class", "features_scale", 
                              "features_trials")
peak_hess_data$Class <- factor(peak_hess_data$Class)
levels(peak_hess_data$Class) <- c("Non-learner", "Learner")
peak_hess_data %>%
  ggplot(mapping = aes(x = peak, y = hess, 
                       colour = Class)) +
  geom_point() + theme_new() + 
  ylab('Curvature value') +
  xlab('Peak value') +
  scale_color_manual(values = c("#4dbd05", "#232323"))
ggsave('Plots/Final_paper/curvature_peak_plot.png', dpi = 400, width = 5, height = 5)
