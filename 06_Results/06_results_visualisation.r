####################################################################################################
###
### File:    06_results_visualisation.r
### Purpose: Creating data visualisation for the main results of the paper.
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    07/04/23
###
####################################################################################################
# Loading the results -----
source(here('06_Results/06_coordinates_results_data.r'))
source(here('06_Results/06_EEG_results_data.r'))
source(here('06_Results/06_Coordinates_Plus_EEG_results_data copy.r'))

hmm_performance <- read.csv('output_data/New_data/performance_data.csv')[,-1]
lime_parameter_importance_trial1 <- read.csv('output_data/New_data/lime_dnn_eegcoordinate_data.csv')[,-1]
lime_parameter_importance_trial12 <- read.csv('output_data/New_data/lime_dnn_eegcoordinate_data_trial_12.csv')[,-1]

# Loading datasets -----
EEG_data_control <- read_excel(here("input_data/FrontalThetaData/ControlFrontalThetaData.xlsx"))
EEG_data_control <- EEG_data_control %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                                Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))
EEG_data_control$Class <- rep('Control', nrow(EEG_data_control))
EEG_data_experiment <- read_excel(here("input_data/FrontalThetaData/ExperimentFrontalThetaData.xlsx"))
EEG_data_experiment<- EEG_data_experiment %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                                     Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))
EEG_data_experiment$Class <- rep('Experiment', nrow(EEG_data_experiment))
EEG_data <- rbind(EEG_data_control, 
                  EEG_data_experiment)
#######################################################################################################
############################ Theta waves visualisation ################################################
#######################################################################################################
CTEEG030EEG_data_Trial1 <- EEG_data %>%
                           filter(Subject == 'CTEEG030' & Trial == '1')
CTEEG030EEG_data_Trial1$X <- 1:nrow(CTEEG030EEG_data_Trial1)
CTEEG030EEG_data_Trial1$Trial <- factor(CTEEG030EEG_data_Trial1$Trial)
levels(CTEEG030EEG_data_Trial1$Trial) <- 'Trial 1'
CTEEG030EEG_data_Trial1 <- CTEEG030EEG_data_Trial1 %>% 
                    pivot_longer(cols = c(2, 6, 7))
colnames(CTEEG030EEG_data_Trial1) <- c('Time', 'Subject',  'Class',   'Trial', "X", 'Preprocessing', 'EEG')

CTEEG030EEG_data_Trial12 <- EEG_data %>%
  filter(Subject == 'CTEEG030' & Trial == '12')
CTEEG030EEG_data_Trial12$X <- 1:nrow(CTEEG030EEG_data_Trial12)
CTEEG030EEG_data_Trial12$Trial <- factor(CTEEG030EEG_data_Trial12$Trial)
levels(CTEEG030EEG_data_Trial12$Trial) <- 'Trial 12'

CTEEG030EEG_data_Trial12 <- CTEEG030EEG_data_Trial12 %>% 
  pivot_longer(cols = c(2, 6, 7))
colnames(CTEEG030EEG_data_Trial12) <- c('Time', 'Subject',  'Class',   'Trial', "X", 'Preprocessing', 'EEG')

CTEEG030EEG_data <- rbind(CTEEG030EEG_data_Trial1,
                          CTEEG030EEG_data_Trial12)
# 'EEG theta \n waves'  
CTEEG030EEG_data %>%
  ggplot(mapping = aes(x = X, y = EEG, colour = Trial)) +
  geom_line() +
  theme_new() +
  facet_wrap(~Preprocessing, ncol = 1, scales = "free") +
  scale_color_manual(values = c("#A3C4D9", "#043259", "#D9A3A3")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259", "#D9A3A3")) +
  ylab('EEG theta \n waves') +
  xlab('Indices of each observation') 
  # scale_x_continuous(
  #   breaks = CTEEG030EEG_data$X[seq(1, nrow(CTEEG030EEG_data), length.out = 10)],
  #   labels = round(CTEEG030EEG_data$Time[seq(1, nrow(CTEEG030EEG_data), length.out = 10)])
  # ) 

ggsave('Plots/EEGThetaWavesPreprocessings.png', dpi = 400, width = 6, height = 4)
CTEEG030EEG_data[CTEEG030EEG_data$Preprocessing == 'ZscoredScaled',] %>%
  ggplot(mapping = aes(x = X, y = EEG)) +
  geom_line() +
  theme_new() +
  facet_wrap(~Trial, ncol = 1, scales = "free_x") +
  scale_color_manual(values = "#043259") +
  scale_fill_manual(values = "#043259")+
  ylab('EEG theta \n waves') +
  xlab('Time in seconds')

ggsave('Plots/EEGThetaWavesRawMidline.png', dpi = 400, width = 6, height = 4)
####################################################################################
############################ Coordinates data visualisation ################################################
################################################################################################

coordinates_results[coordinates_results$Models == 'Polynomial \n SVM',]$Models <- 'Poly \n SVM'
# Creating the visualisations for the coordinates data -----
coordinates_results$Trial[coordinates_results$Trial == '12'] <- "Trial 12"
coordinates_results$Trial[coordinates_results$Trial == '1'] <- "Trial 1"
## Visualising the models performance
coordinates_results %>%
  filter(Metric == 'Accuracy') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
coordinates_results$Models <- factor(coordinates_results$Models)
levels(coordinates_results$Models) <- c("DNN", "KNN", "Ridge",
                                        "Non linear \n SVM",
                                        "Poly \n SVM", "RF")
coordinates_results$Preprocessing <- factor(coordinates_results$Preprocessing)
levels(coordinates_results$Preprocessing) <- c("Minmax scaled", 
                                               "Raw data", "Zscored scaled")
coordinates_results %>%
  filter(Metric == 'AUROC') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new() +
  theme(legend.position = 'None') +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC')+
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
ggsave('Plots/AUROCCoordinates.png', dpi = 400, width = 14, height = 8)

## Visualising the models performance
coordinates_results %>%
  filter(Metric == 'Accuracy') %>%
  group_by(Trial, Preprocessing) %>%
  summarise(mean = mean(Value), 
            se = se(Value)) %>%
  ggplot(mapping = aes(x = Preprocessing, y = mean, fill = Trial)) +
  geom_col(position = "dodge") + theme_new() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(.9)) +
  ylim(0, 1) +
  scale_color_manual(values = c("#cdfb43", "#043259")) +
  scale_fill_manual(values = c("#cdfb43", "#043259")) +
  ylab('Average accuracy')
ggsave(here('Plots/CoordinatesOverallAccuracyValues.png'), dpi = 400, width = 6, height = 4)

coordinates_results %>%
  filter(Metric == 'AUROC' &
           Models == 'RF') %>%
  group_by(Trial, Preprocessing) %>%
  summarise(mean = mean(Value), 
            se = se(Value)) %>%
  ggplot(mapping = aes(x = Preprocessing, y = mean, fill = Trial)) +
  geom_col(position = "dodge") + theme_new() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(.9)) +
  ylim(0, 1) +
  scale_color_manual(values = c("#A3C4D9", "#043259")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259")) +
  ylab('Average AUROC')
ggsave(here('Plots/CoordinatesRFAUROCValues.png'), dpi = 400, width = 6, height = 4)

####################################################################################
############################ EEG data visualisation ################################################
################################################################################################

############################# All data ##########################################################
# Accuracy ---
eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'All') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
eeg_results %>%
  filter(Metric == 'AUROC', 
         HMM_parameters == 'All') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

############################# HMM (m = 2) ##########################################################
# Accuracy ---
eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(2)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(2)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

############################# HMM (m = 3) ##########################################################
# Accuracy ---
eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(3)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
eeg_results %>%
  filter(Metric == 'AUROC', 
         HMM_parameters == 'm(3)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

############################# HMM (m = 4) ##########################################################
# Accuracy ---
eeg_results$Models <- factor(coordinates_results$Models)
levels(eeg_results$Models) <- c("DNN", "KNN", "Ridge",
                                        "Non linear \n SVM",
                                        "Poly \n SVM", "RF")

eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(4)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
eeg_results %>%
  filter(Metric == 'AUROC', 
         HMM_parameters == 'm(4)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

ggsave('Plots/AUROCEEG.png', dpi = 400, width = 14, height = 8)
############################# HMM (m = 5) ##########################################################
eeg_results[eeg_results$Trial == '1',]$Trial <- 'Trial 1'
eeg_results[eeg_results$Trial == '12',]$Trial <- 'Trial 12'
# Accuracy ---
eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(5)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
eeg_results %>%
  filter(Metric == 'AUROC', 
         HMM_parameters == 'm(5)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

############################# Comparing treatments ##########################################################
##### Accuracy -----
eeg_results %>%
  filter(Metric == 'Accuracy') %>%
  group_by(Trial, Preprocessing, HMM_parameters) %>%
  summarise(mean = mean(Value), 
            se = se(Value)) %>%
  ggplot(mapping = aes(x = Preprocessing, y = mean, fill = Trial)) +
  geom_col(position = "dodge") + theme_new() +
  facet_wrap(~HMM_parameters)+
  ylim(0, 1) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(.9)) +
  geom_text(aes(label = ifelse(mean > 0.7, "*", "")),
            y = rep(0.85, 6*5), vjust = -1, size = 6) +
  scale_color_manual(values = c("#cdfb43", "#043259")) +
  scale_fill_manual(values = c("#cdfb43", "#043259")) +
  
  ylab('Average accuracy')

ggsave(here('Plots/EEGOverallAccuracyValues.png'), dpi = 400, width = 7, height = 4)
##### AUROC -----
eeg_results %>%
  filter(Metric == 'AUROC') %>%
  group_by(Trial, Preprocessing, HMM_parameters) %>%
  filter(HMM_parameters %in% c("m(2)", "m(3)",
                               "m(4)", "m(5)")) %>%
  summarise(mean = mean(Value), 
            se = se(Value)) %>%
  ggplot(mapping = aes(x = Preprocessing, y = mean, fill = HMM_parameters)) +
  geom_col(position = "dodge") + theme_new() +
  facet_wrap(~Trial)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(.9)) +
  scale_color_manual(values = c("#D9A3A3", "#043259")) +
  scale_fill_manual(values = c("#D9A3A3", "#043259")) +
  ylim(0, 1) +

  ylab('Average auroc')
ggsave(here('Plots/EEGOverallAUROCValues.png'), dpi = 400, width = 7, height = 4)


####################################################################################
############################ Coordinates +nEEG data visualisation ################################################
################################################################################################

############################# All data ##########################################################
# Accuracy ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'All') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'AUROC', 
         HMM_parameters == 'All') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

############################# HMM (m = 2) ##########################################################
# Accuracy ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(2)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(2)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

############################# HMM (m = 3) ##########################################################
# Accuracy ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(3)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'AUROC', 
         HMM_parameters == 'm(3)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

############################# HMM (m = 4) ##########################################################
# Accuracy ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(4)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'AUROC', 
         HMM_parameters == 'm(4)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

############################# HMM (m = 5) ##########################################################
# Accuracy ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'Accuracy', 
         HMM_parameters == 'm(5)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('Accuracy') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))
# AUROC ---
coorinates_plus_eeg_results %>%
  filter(Metric == 'AUROC', 
         HMM_parameters == 'm(5)') %>%
  ggplot(mapping = aes(x = Models, y = Value, fill = "#043259")) +
  geom_col() +
  facet_wrap(Trial~Preprocessing) + 
  theme_new()  +
  geom_text(aes(label = ifelse(Value > 0.8, "*", "")),
            y = rep(0.85, 6*6), vjust = -1, size = 6) +
  ylim(0, 1) +
  ylab('AUROC') +
  theme(legend.position = 'None') +
  scale_color_manual(values = c("#043259")) +
  scale_fill_manual(values = c("#043259"))

############################# Comparing treatments ##########################################################
##### Accuracy -----
coorinates_plus_eeg_results %>%
  filter(Metric == 'Accuracy') %>%
  group_by(Trial, Preprocessing, HMM_parameters) %>%
  summarise(mean = mean(Value), 
            se = se(Value)) %>%
  ggplot(mapping = aes(x = Preprocessing, y = mean, fill = Trial)) +
  geom_col(position = "dodge") + theme_new() +
  facet_wrap(~HMM_parameters)+
  ylim(0, 1) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(.9)) +
  geom_text(aes(label = ifelse(mean > 0.7, "*", "")),
            y = rep(0.85, 6*5), vjust = -1, size = 6) +
  scale_color_manual(values = c("#cdfb43", "#043259")) +
  scale_fill_manual(values = c("#cdfb43", "#043259")) +
  
  ylab('Average accuracy')

ggsave(here('Plots/CoordinatesPlusEEGOverallAccuracyValues.png'), dpi = 400, width = 7, height = 4)
##### AUROC -----
coorinates_plus_eeg_results %>%
  filter(Metric == 'AUROC') %>%
  group_by(Trial, Preprocessing, HMM_parameters) %>%
  summarise(mean = mean(Value), 
            se = se(Value)) %>%
  ggplot(mapping = aes(x = Preprocessing, y = mean, fill = Trial)) +
  geom_col(position = "dodge") + theme_new() +
  facet_wrap(~HMM_parameters)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(.9)) +
  scale_color_manual(values = c("#D9A3A3", "#043259")) +
  scale_fill_manual(values = c("#D9A3A3", "#043259")) +
  ylim(0, 1) +
  
  ylab('Average auroc')
ggsave(here('Plots/CoordinatesPlusEEGOverallAUROCValues.png'), dpi = 400, width = 7, height = 4)

###########################################################################################
########################## HMM performance ################################################
###########################################################################################
hmm_performance[hmm_performance$performance_trials == '1',]$performance_trials <- 'Trial 1'
hmm_performance[hmm_performance$performance_trials == '12',]$performance_trials <- 'Trial 12'

hmm_performance_toplot <- hmm_performance %>%
                        pivot_longer(cols = 2:5)
colnames(hmm_performance_toplot) <- c("Subject", "Class", 
                                      "Trial", "M",
                                      "AIC" )
hmm_performance_toplot[hmm_performance_toplot$M == 'hhm_2_aics',]$M <- '2'
hmm_performance_toplot[hmm_performance_toplot$M == 'hhm_3_aics',]$M <- '3'
hmm_performance_toplot[hmm_performance_toplot$M == 'hhm_4_aics',]$M <- '4'
hmm_performance_toplot[hmm_performance_toplot$M == 'hhm_5_aics',]$M <- '5'
hmm_performance_toplot$Class <- factor(hmm_performance_toplot$Class)
levels(hmm_performance_toplot$Class) <- c("Non-learner", "Learner")
hmm_performance_toplot %>%
  ggplot(mapping = aes(x = Class, y = AIC, fill = M)) +
  geom_boxplot() +
  facet_wrap(~Trial, ncol = 1, scales = "free") +
  theme_new()+
  scale_color_manual(values = c("#4dbd05", "#232323",
                                "#1fbeb8", "#9b45a3")) +
  scale_fill_manual(values = c("#4dbd05", "#232323",
                               "#1fbeb8", "#9b45a3"))
ggsave(here('Plots/Final_paper/HMMperformance.png'), dpi = 400, width = 9, height = 7)

colnames(eeg_results) <- c("Preprocessing", "Trial",
                           "Metric", "M", "Models", 
                           "Value")
levels(eeg_results$M) <- c("All", "2", "3",
                           "4", "5")
eeg_results %>%
  filter(Metric == 'AUROC') %>%
  group_by(Trial, Preprocessing, M) %>%
  filter(M %in% c("2", "3",
                  "4", "5")) %>%
  summarise(mean = mean(Value), 
            se = se(Value)) %>%
  ggplot(mapping = aes(x = Preprocessing, y = mean, fill = M)) +
  geom_col(position = "dodge") + theme_new() +
  facet_wrap(~Trial, ncol = 1, scales = "free")+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(.9)) +
  scale_color_manual(values = c("#4dbd05", "#232323",
                                "#1fbeb8", "#9b45a3")) +
  scale_fill_manual(values = c("#4dbd05", "#232323",
                               "#1fbeb8", "#9b45a3")) +
  ylim(0, 1) +
  
  ylab('Average auroc')
ggsave(here('Plots/Final_paper/EEGOverallAUROCValues.png'), dpi = 400, width = 9, height = 7)

hmm_performance_toplot %>%
  group_by(Class, Trial, M) %>%
  summarise(average = mean(AIC), 
            standard_deviation = sd(AIC))

###########################################################################################
########################## LIME parameter importance ################################################
###########################################################################################
lime_parameter_importance_trial1$Local...Importance <- abs(lime_parameter_importance_trial1$Local...Importance)
lime_parameter_importance_trial12$Local...Importance <- abs(lime_parameter_importance_trial12$Local...Importance)
lime_parameter_importance_trial12$Trial <- 'Trial 12'
lime_parameter_importance <- rbind(lime_parameter_importance_trial1, 
                                   lime_parameter_importance_trial12)

## Trial 1
lime_parameter_importance_trial1 %>%
  filter(Class == 'Control' &
         Local...Importance >= quantile(lime_parameter_importance_trial1$Local...Importance, 0.985)) %>%
  arrange(Local...Importance)

lime_parameter_importance_trial1 %>%
  filter(Class == 'Experiment' &
           Local...Importance >= quantile(lime_parameter_importance_trial1$Local...Importance, 0.99))%>%
  arrange(Local...Importance)

## Trial 12
lime_parameter_importance_trial12 %>%
  filter(Class == 'Control' &
           Local...Importance >= quantile(lime_parameter_importance_trial12$Local...Importance, 0.98))%>%
  arrange(Local...Importance)

lime_parameter_importance_trial12 %>%
  filter(Class == 'Experiment' &
           Local...Importance >= quantile(lime_parameter_importance_trial12$Local...Importance, 0.9))%>%
  arrange(Local...Importance)

