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
EEG_data_control <- read_excel(here("input_data/FrontalThetaData/ControlFrontalThetaData.xlsx"))
EEG_data_control <- EEG_data_control %>% mutate(Scaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                               Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))
EEG_data_experiment <- read_excel(here("input_data/FrontalThetaData/ExperimentFrontalThetaData.xlsx"))
EEG_data_experiment<- EEG_data_experiment %>% mutate(Scaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                                     Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))
# Checking the peaks -----
subject_names <- EEG_data_control %>% 
                        filter(Trial == 12) %>%
                        dplyr::select(Subject) %>%
                        pull(Subject) %>%
                        unique()
## Control data
### Working with Trial 1 
EEG_data_control_trial_1 <- EEG_data_control %>% 
  filter(Trial == 1)

check_values <- NULL
index <-1 
for (subject in subject_names){
  time_series <- EEG_data_control_trial_1 %>%
    filter(Subject == subject)
  check_values[index] <- sum(diff(time_series$RawMidline) == 0)
  index <- index + 1	
}
### Working with Trial 12
EEG_data_control_trial_12 <- EEG_data_control %>% 
  filter(Trial == 12)

check_values <- NULL
index <-1 
for (subject in subject_names){
  time_series <- EEG_data_control_trial_12 %>%
    filter(Subject == subject)
  check_values[index] <- sum(diff(time_series$RawMidline) == 0)
  index <- index + 1	
}

## Experimental data
### Working with Trial 1 
EEG_data_experiment_trial_1 <- EEG_data_experiment %>% 
  filter(Trial == 1)

check_values <- NULL
index <-1 
for (subject in subject_names){
  time_series <- EEG_data_experiment_trial_1 %>%
    filter(Subject == subject)
  check_values[index] <- sum(diff(time_series$RawMidline) == 0)
  index <- index + 1	
}
### Working with Trial 12
EEG_data_experiment_trial_12 <- EEG_data_experiment %>% 
  filter(Trial == 12)

check_values <- NULL
index <-1 
for (subject in subject_names){
  time_series <- EEG_data_experiment_trial_12 %>%
    filter(Subject == subject)
  check_values[index] <- sum(diff(time_series$RawMidline) == 0)
  index <- index + 1	
}


####################################################################################################
eeg_experiment_trial_1 <- EEG_data_experiment %>% 
                            filter(Trial == 1)
collected_peaks_curvatures <- obtain_peak_hess(eeg_data = 
                                                 eeg_experiment_trial_1, 
                         condition = 'experiment', 
                         trial = '1')
subject <- 'CTEEG005'
data <- collected_peaks_curvatures$dat
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





















# 
# control_trial_1 <- EEG_data_control %>% filter(Trial == 1)
# 
# dat <- split(control_trial_1, 
#              control_trial_1$Subject)
# peak <- peak_index <- hess <- lapply(1:length(unique(control_trial_1$Subject)),
#                                      function(x) numeric(1))
# names(peak) <- names(peak_index) <- names(hess) <- names(dat)
# 
# 
# for(i in 1:25) {
#   k <- 1
#   for(j in 2:nrow(dat[[i]])) {
#     if(dat[[i]]$RawMidline[j] > dat[[i]]$RawMidline[j - 1] &
#        dat[[i]]$RawMidline[j] > dat[[i]]$RawMidline[j + 1]) {
#       peak[[i]][k] <- dat[[i]]$RawMidline[j]
#       peak_index[[i]][k] <- j
#       k <- k + 1
#     }
#   }
# }
# 
# for(i in 1:25) {
#   for(k in 1:length(peak_index[[i]])) {
#     hess[[i]][k] <- dat[[i]]$RawMidline[(peak_index[[i]] - 1)[k] : (peak_index[[i]] + 1)[k]] %>%
#       diff %>% diff
#   }
# }
# subject <- 'CTEEG036'
# EEG_data_control %>%
#    filter(Trial == 1,
#           Subject == subject) %>%
#   ggplot(aes(x = Time, y = RawMidline)) +
#   theme_bw() +
#   geom_line() +
#   facet_wrap(~ Subject, scales = "free", nrow = 5) +
#   geom_point(data = data.frame(RawMidline = peak[[subject]],
#                                Time = dat[[subject]]$Time[peak_index[[subject]]]),
#              col = 2, alpha = .4)
# 
# peak_hess <- list()
# 
# for(i in 1:25) {
#   peak_hess[[i]] <- data.frame(Subject = names(peak)[i],
#                                peak = peak[[i]],
#                                hess = hess[[i]],
#                                condition = "experiment")
# }
# 
# peak_hess <- do.call(rbind, peak_hess)
# 
# peak_hess %>%
#   ggplot(aes(x = peak * 1e12, y = hess * 1e18)) +
#   theme_bw() +
#   geom_point(alpha = .5) +
#   geom_smooth(se = FALSE, method = "lm", lwd = .5) +
#   facet_wrap(~ Subject) +
#   xlab("Peak x 10^(-12)") +
#   ylab("Curvature x 10^(-18)")
# 
# peak_hess %>%
#   group_by(Subject) %>%
#   summarise(r = cor(peak, hess)) %>%
#   ggplot(aes(y = r, x = Subject)) +
#   theme_bw() +
#   stat_identity(geom = "bar") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# fit <- lmer(peak ~ hess + (hess | Subject))
# ranef(fit) # Access hess
