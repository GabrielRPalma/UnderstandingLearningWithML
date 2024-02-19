####################################################################################################
###
### File:    00_source.R
### Purpose: Load required packages and functions used to 
###          create interactive maps.
### Authors: Gabriel Rodrigues Palma
### Date:    05/04/23
###
####################################################################################################

# packages required ---------------------------

packages <- c('GGally', # Multiple plots 
              'tidyverse', # data-wrangling + plotting
              'here', # efficient file structures
              'readxl', 
              'hnp', 
              'depmixS4',
              'lme4', 
              'MCMCglmm', 
              'plotly', 
              'fda', # Functional Data analysis
              'refund', # Functional Data analysis
              'TSclust',
              'ape', 
              'dendextend', 
              'gamlss'
              
)

install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)
if (!require("ggflags")) {
  devtools::install_github("rensa/ggflags")
  library(ggflags)
}

# Main functions ---------------------------
compare_models_ic <- function(aics, bics){
  # This function oganise a dataframe for visualising the performance 
  #of fitted models
  models_aic <- data.frame(model = rownames(aics), 
                           aic = aics$AIC)
  models_bic <- data.frame(model = rownames(bics), 
                           bic = bics$BIC)
  models_performance <- inner_join(models_aic, models_bic, 'model')
  return(models_performance)
}
se <- function(x){
  n <- length(x)
  se <- sd(x)/sqrt(n)
  return(se)
}
get_coordinate_features <- function(coordinate_data){
  
  # This function collects the data from the coordinates data
  subjects <- coordinate_data$Subject %>% unique()
  seq_times <- list()
  i <- 1
  for (subject in subjects){
    subject_coordinates <- coordinate_data %>% 
      filter(Subject == subject) %>%
      dplyr::select(X, Y, Time)
    
    seq_times[[i]] <- subject_coordinates
    i <- i + 1
  }
  
  coordinate_features <- lapply(seq_times, function(x) {
    m <- as.matrix(dist(x[,1:2]))
    d <- c(0,diag(m[-nrow(m),-1]))
    a <- atan2(x$Y[2:nrow(x)] - x$Y[1:(nrow(x)-1)],
               x$X[2:nrow(x)] - x$X[1:(nrow(x)-1)]) * 180 / pi
    a <- c(0, diff(c(0, a)))
    return(data.frame(distance = d, angle_shift = a, 
                      time = x$Time, idle_time = ifelse(d == 0, 0.25, 0)))
  })
  
  coordinate_features <- do.call(rbind, coordinate_features)
  
  final_coordinate_features <- cbind(coordinate_data, 
                                     coordinate_features)
  
  final_data <- final_coordinate_features %>% 
    group_by(Subject, Class) %>%
    summarise(dis_sum = sum(distance), 
              angle_sum = sum(abs(angle_shift)), 
              average_speed = mean(distance/time, na.rm = T), 
              idle_time = sum(idle_time))
  
  return(final_data)
}
read_excel_allsheets <- function(filename) {
  # This function reads the files from an excel sheet
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}
get_hmm_statistics <- function(data, m, response, 
                               subject_names, scalar){
  # This function extracs the HHM parameters based on the EEG data per participant
  summary_fit <- fitted_states <- aics <- list()
  i <- 1
  for(subject_name in subject_names) {
    if (response == 'RawMidline'){
      model <- depmix((RawMidline*scalar) ~ 1,
                      nstates = m,
                      family = gaussian(),
                      data = data %>%
                        filter(Subject == subject_name))  
    }
    else if(response == 'Scaled'){
      model <- depmix(Scaled ~ 1,
                      nstates = m,
                      family = gaussian(),
                      data = data %>%
                        filter(Subject == subject_name))
    }
    else{
      model <- depmix(Minmaxscale ~ 1,
                      nstates = m,
                      family = gaussian(),
                      data = data %>%
                        filter(Subject == subject_name))
    }
    
    
    set.seed(2022)
    fit <- fit(model)
    summary_fit[[i]] <- summary(fit)
    aics[[i]] <- AIC(fit)
    fitted_states[[i]] <- table(posterior(fit)$state)/length(posterior(fit)$state)
    i <- i + 1
  }
  result <- list()
  result$summaries <- summary_fit
  result$proportions <- fitted_states
  result$aics <- aics
  return(result)
}

get_hmm_data <- function(EEG_data, 
                         features_class, 
                         features_trials,
                         features_scale,
                         scalar,
                         eeg_data_subjects){
  hhm_2_statistics <- get_hmm_statistics(data = EEG_data, m = 2, 
                                         response = features_scale,
                                         subject_names = eeg_data_subjects, 
                                         scalar = scalar)
  
  hhm_2_means_sds <- do.call(rbind, lapply(hhm_2_statistics$summaries, as.vector))
  hhm_2_proportions <- do.call(rbind, hhm_2_statistics$proportions)
  hhm_2_aics <- do.call(rbind, lapply(hhm_2_statistics$aics, as.vector))
  hhm_2_aics <- data.frame(Subject = eeg_data_subjects, 
                           hhm_2_aics = hhm_2_aics)
  hh2_data <- data.frame(Subject = eeg_data_subjects, 
                         hmm2_mean = hhm_2_means_sds[,1:2], 
                         hmm2_sd = hhm_2_means_sds[,3:4],
                         hmm2_p1 = hhm_2_proportions[,1],
                         hmm2_p2 = hhm_2_proportions[,2])
  
  hhm_3_statistics <- get_hmm_statistics(data = EEG_data, m = 3, 
                                         response = features_scale,
                                         subject_names = eeg_data_subjects, 
                                         scalar = scalar)
  
  hhm_3_means_sds <- do.call(rbind, lapply(hhm_3_statistics$summaries, as.vector))
  hhm_3_proportions <- do.call(rbind, hhm_3_statistics$proportions)
  hhm_3_aics <- do.call(rbind, lapply(hhm_3_statistics$aics, as.vector))
  hhm_3_aics <- data.frame(hhm_3_aics = hhm_3_aics)
  hh3_data <- data.frame(hmm3_mean = hhm_3_means_sds[,1:3], 
                         hmm3_sd = hhm_3_means_sds[,4:6],
                         hmm3_p1 = hhm_3_proportions[,1],
                         hmm3_p2 = hhm_3_proportions[,2], 
                         hmm3_p3 = hhm_3_proportions[,3])
  
  hhm_4_statistics <- get_hmm_statistics(data = EEG_data, m = 4, 
                                         response = features_scale,
                                         subject_names = eeg_data_subjects, 
                                         scalar = scalar)
  
  hhm_4_means_sds <- do.call(rbind, lapply(hhm_4_statistics$summaries, as.vector))
  hhm_4_proportions <- do.call(rbind, hhm_4_statistics$proportions)
  hhm_4_aics <- do.call(rbind, lapply(hhm_4_statistics$aics, as.vector))
  hhm_4_aics <- data.frame(hhm_4_aics = hhm_4_aics)
  hh4_data <- data.frame(hmm4_mean = hhm_4_means_sds[,1:4], 
                         hmm4_sd = hhm_4_means_sds[,5:8],
                         hmm4_p1 = hhm_4_proportions[,1],
                         hmm4_p2 = hhm_4_proportions[,2], 
                         hmm4_p3 = hhm_4_proportions[,3],
                         hmm4_p4 = hhm_4_proportions[,4])
  hhm_5_statistics <- get_hmm_statistics(data = EEG_data, m = 5, 
                                         response = features_scale,
                                         subject_names = eeg_data_subjects, 
                                         scalar = scalar)
  hhm_5_means_sds <- do.call(rbind, lapply(hhm_5_statistics$summaries, as.vector))
  hhm_5_proportions <- do.call(rbind, hhm_5_statistics$proportions)
  hhm_5_aics <- do.call(rbind, lapply(hhm_5_statistics$aics, as.vector))
  hhm_5_aics <- data.frame(hhm_5_aics = hhm_5_aics)
  hh5_data <- data.frame(hmm5_mean = hhm_5_means_sds[,1:5], 
                         hmm5_sd = hhm_5_means_sds[,6:10],
                         hmm5_p1 = hhm_5_proportions[,1],
                         hmm5_p2 = hhm_5_proportions[,2], 
                         hmm5_p3 = hhm_5_proportions[,3],
                         hmm5_p4 = hhm_5_proportions[,4], 
                         hmm5_p5 = hhm_5_proportions[,5])
  
  features_class <- rep(features_class, nrow(hh3_data))
  features_trials <- rep(features_trials, nrow(hh3_data))
  features_scale <- rep(features_scale, nrow(hh3_data))
  performance_class <- rep(features_class, nrow(hhm_2_aics))
  performance_trials <- rep(features_trials, nrow(hhm_2_aics))
  performance_scale <- rep(features_scale, nrow(hh3_data))
  
  hmm_features <- cbind(hh2_data, hh3_data, 
                        hh4_data, hh5_data, 
                        features_class, 
                        features_trials, 
                        features_scale)
  hmm_performance <- cbind(hhm_2_aics, hhm_3_aics, 
                           hhm_4_aics, hhm_5_aics, 
                           performance_class, 
                           performance_trials, 
                           performance_scale)
  
  results <- list()
  results$hmm_features <- hmm_features
  results$hmm_performance <- hmm_performance
  return(results)
  
}


# get_hmm_performance <- function(EEG_data, 
#                          eegclass, 
#                          response,
#                          scalar,
#                          eeg_data_subjects){
#   hhm_2_statistics <- get_hmm_statistics(data = EEG_data, m = 2, 
#                                          response = response,
#                                          subject_names = eeg_data_subjects, 
#                                          scalar = scalar)
#   
#   hhm_2_means_sds <- do.call(rbind, lapply(hhm_2_statistics$summaries, as.vector))
#   hhm_2_proportions <- do.call(rbind, hhm_2_statistics$proportions)
#   hh2_data <- data.frame(Subject = eeg_data_subjects, 
#                          hmm2_mean = hhm_2_means_sds[,1:2], 
#                          hmm2_sd = hhm_2_means_sds[,3:4],
#                          hmm2_p1 = hhm_2_proportions[,1],
#                          hmm2_p2 = hhm_2_proportions[,2])
#   
#   hhm_3_statistics <- get_hmm_statistics(data = EEG_data, m = 3, 
#                                          response = response,
#                                          subject_names = eeg_data_subjects, 
#                                          scalar = scalar)
#   
#   hhm_3_means_sds <- do.call(rbind, lapply(hhm_3_statistics$summaries, as.vector))
#   hhm_3_proportions <- do.call(rbind, hhm_3_statistics$proportions)
#   hh3_data <- data.frame(hmm3_mean = hhm_3_means_sds[,1:3], 
#                          hmm3_sd = hhm_3_means_sds[,4:6],
#                          hmm3_p1 = hhm_3_proportions[,1],
#                          hmm3_p2 = hhm_3_proportions[,2], 
#                          hmm3_p3 = hhm_3_proportions[,3])
#   
#   hhm_4_statistics <- get_hmm_statistics(data = EEG_data, m = 4, 
#                                          response = response,
#                                          subject_names = eeg_data_subjects, 
#                                          scalar = scalar)
#   
#   hhm_4_means_sds <- do.call(rbind, lapply(hhm_4_statistics$summaries, as.vector))
#   hhm_4_proportions <- do.call(rbind, hhm_4_statistics$proportions)
#   hh4_data <- data.frame(hmm4_mean = hhm_4_means_sds[,1:4], 
#                          hmm4_sd = hhm_4_means_sds[,5:8],
#                          hmm4_p1 = hhm_4_proportions[,1],
#                          hmm4_p2 = hhm_4_proportions[,2], 
#                          hmm4_p3 = hhm_4_proportions[,3],
#                          hmm4_p4 = hhm_4_proportions[,4])
#   hhm_5_statistics <- get_hmm_statistics(data = EEG_data, m = 5, 
#                                          response = response,
#                                          subject_names = eeg_data_subjects, 
#                                          scalar = scalar)
#   hhm_5_means_sds <- do.call(rbind, lapply(hhm_5_statistics$summaries, as.vector))
#   hhm_5_proportions <- do.call(rbind, hhm_5_statistics$proportions)
#   hh5_data <- data.frame(hmm5_mean = hhm_5_means_sds[,1:5], 
#                          hmm5_sd = hhm_5_means_sds[,6:10],
#                          hmm5_p1 = hhm_5_proportions[,1],
#                          hmm5_p2 = hhm_5_proportions[,2], 
#                          hmm5_p3 = hhm_5_proportions[,3],
#                          hmm5_p4 = hhm_5_proportions[,4], 
#                          hmm5_p5 = hhm_5_proportions[,5])
#   class <- rep(eegclass, nrow(hh3_data))
#   hmm_features <- cbind(hh2_data, hh3_data, 
#                         hh4_data, hh5_data, class)
#   return(hmm_features)
#   
# }

## Creating the basic function for the obtaining the peaks of the time series
obtain_peak_hess <- function(eeg_data, 
                             features_class,
                             features_scale,
                             features_trials){
  # This function creates the necessary steps to obtain the peaks of the 
  # eeg time series data
  
  # Parameters:
  #   eeg_data: Dataset containing the main theta waves and time
  #   features_class: Control or Experiment
  #   features_trials: The numer of trial (1, 2, ... , 12)
  
  
  dat <- split(eeg_data, 
               eeg_data$Subject)
  n_subjects <- length(unique(eeg_data$Subject))
  peak <- peak_index <- hess <- lapply(1:n_subjects,
                                       function(x) numeric(1))
  names(peak) <- names(peak_index) <- names(hess) <- names(dat)
  
  for(i in 1:n_subjects) {
    k <- 1
    for(j in 2:nrow(dat[[i]])) {
      if(dat[[i]][[features_scale]][j] > dat[[i]][[features_scale]][j - 1] &
         dat[[i]][[features_scale]][j] > dat[[i]][[features_scale]][j + 1]) {
        peak[[i]][k] <- dat[[i]][[features_scale]][j]
        peak_index[[i]][k] <- j
        k <- k + 1
      }
    }
  }
  
  for(i in 1:n_subjects) {
    for(k in 1:length(peak_index[[i]])) {
      hess[[i]][k] <- dat[[i]][[features_scale]][(peak_index[[i]] - 1)[k] : (peak_index[[i]] + 1)[k]] %>%
        diff %>% diff
    }
  }
  
  peak_hess <- list()
  
  for(i in 1:n_subjects) {
    peak_hess[[i]] <- data.frame(Subject = names(peak)[i],
                                 peak = peak[[i]],
                                 hess = hess[[i]],
                                 features_class = features_class,
                                 features_scale = features_scale,
                                 features_trials = features_trials)
  }
  
  peak_hess <- do.call(rbind, peak_hess)
  
  res <- list()
  res$result <- peak_hess
  res$dat <-dat
  res$peak <- peak
  res$peak_index <- peak_index
  
  return(res)
}

collect_peak_parameters <- function(eeg_peak_hess_data, 
                                    features_class, 
                                    features_scale, 
                                    features_trials){
  # This function obtain the main parameters related to peaks and curvature
  peak_curvature_parameters <- eeg_peak_hess_data$result %>%
    group_by(Subject) %>%
    summarise(average_peak_magnitude = mean(peak), 
              average_peak_curvature = mean(hess))
  n_peaks <- lapply(eeg_peak_hess_data$peak, 
                    FUN = length)
  total_samples <- lapply(eeg_peak_hess_data$dat, 
                          FUN = function(x) max(x$Time) - min(x$Time))
  n_peaks <- do.call(rbind, n_peaks)
  total_samples <- do.call(rbind, total_samples)
  peaks_perseccond_data <- data.frame(Subject = rownames(n_peaks), 
                                      n_peaks = n_peaks, 
                                      total_samples = total_samples)
  rownames(peaks_perseccond_data) <- NULL
  peaks_perseccond_data <- peaks_perseccond_data %>%
    mutate(persecond = n_peaks/total_samples)
  
  ### Mixed Models data
  fit <- lmer(data = eeg_peak_hess_data$result,
              peak ~ hess + (hess | Subject))
  
  Mixed_Models_data <- data.frame(Subject = rownames(ranef(fit)$Subject), 
                                  MM_Intercept = ranef(fit)$Subject[,1], 
                                  MM_Hess = ranef(fit)$Subject[,2], 
                                  features_class = rep(features_class, nrow(peaks_perseccond_data)),
                                  features_scale = rep(features_scale, nrow(peaks_perseccond_data)),
                                  features_trials = rep(features_trials, nrow(peaks_perseccond_data)))
  peak_parameters_data <- inner_join(Mixed_Models_data,
                                     peak_curvature_parameters %>%
                                       dplyr::select(Subject, 
                                                     average_peak_magnitude,
                                                     average_peak_curvature),
                                     peaks_perseccond_data %>%
                                       dplyr::select(Subject, persecond), by = 'Subject')
  
  return(peak_parameters_data)
}
# plot settings ---------------------------
pallete = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]

theme_new <- function(base_size = 20, base_family = "Arial"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 20, colour = "grey30"),
      legend.key=element_rect(colour=NA, fill =NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks =         element_line(colour = "grey20"),
      plot.title.position = 'plot',
      legend.position = "bottom"
    )
}
