####################################################################################################
###
### File:    01_obtaining_coordinate_data.R
### Purpose: Obtaining the features related to the coordinates.
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    31/03/23
###
####################################################################################################

# Loading packages ----
source(here('00_source.r'))

# Loading data -----
coordinate_control_data <- read_excel(here("input_data/CoordinateData/ControlCoordinatesData.xlsx"))
coordinate_experiment_data <- read_excel(here("input_data/CoordinateData/ExperientCoordinatesData.xlsx"))

# Feature extraction from the coordinates ----
### Trial 1 
coordinate_control_data_trial_1 <- coordinate_control_data %>%
                                    filter(Trial == 1)
coordinate_control_data_trial_1 %>%
  ggplot(mapping = aes(x = X, y = Y, colour = Subject)) +
  geom_point() +
  theme_new()
coordinate_experiment_data_trial_1 <- coordinate_experiment_data %>%
  filter(Trial == 1)
coordinate_experiment_data_trial_1 %>%
  ggplot(mapping = aes(x = X, y = Y, colour = Subject)) +
  geom_point() +
  theme_new()

### Experiment data for trial 1 and 12
coordinate_control_data_trial_12 <- coordinate_control_data %>%
  filter(Trial == 12)
coordinate_control_data_trial_12 %>%
  ggplot(mapping = aes(x = X, y = Y, colour = Subject)) +
  geom_point() +
  theme_new()

coordinate_experiment_data_trial_12 <- coordinate_experiment_data %>%
  filter(Trial == 12)
coordinate_experiment_data_trial_12 %>%
  ggplot(mapping = aes(x = X, y = Y, colour = Subject)) +
  geom_point() +
  theme_new()

### Creating a new dataset with the main variables per subject
coordinate_control_data_trial_1 <- get_coordinate_features(
  coordinate_control_data_trial_1
)
coordinate_control_data_trial_1 %>%
  ggplot(mapping = aes(x = dis_sum, angle_sum)) +
  geom_point() +
  theme_new()


coordinate_control_data_trial_12 <- get_coordinate_features(
  coordinate_control_data_trial_12
)
coordinate_control_data_trial_12 %>%
  ggplot(mapping = aes(x = dis_sum, angle_sum)) +
  geom_point() +
  theme_new()



coordinate_experiment_data_trial_1 <- get_coordinate_features(
  coordinate_experiment_data_trial_1
)
coordinate_experiment_data_trial_1 %>%
  ggplot(mapping = aes(x = dis_sum, angle_sum)) +
  geom_point() +
  theme_new()
coordinate_experiment_data_trial_12 <- get_coordinate_features(
  coordinate_experiment_data_trial_12
)
coordinate_experiment_data_trial_12 %>%
  ggplot(mapping = aes(x = dis_sum, angle_sum)) +
  geom_point() +
  theme_new()

### Creating the final dataset -----
coordinate_trial1_data <- rbind(coordinate_control_data_trial_1, 
                                coordinate_experiment_data_trial_1)
coordinate_trial1_data[coordinate_trial1_data$Class == 'Control',]$Class <- 'control'
coordinate_trial1_data[coordinate_trial1_data$Class == 'Experiment',]$Class <- 'experiment'
raw_coordinate_trial1_data <- coordinate_trial1_data

coordinate_trial12_data <- rbind(coordinate_control_data_trial_12, 
                                coordinate_experiment_data_trial_12)
coordinate_trial12_data[coordinate_trial12_data$Class == 'Control',]$Class <- 'control'
coordinate_trial12_data[coordinate_trial12_data$Class == 'Experiment',]$Class <- 'experiment'
raw_coordinate_trial12_data <- coordinate_trial12_data

## Creating the extra pre processing coordinates datasets -----
ZscoredScaledcoordinate_trial1_data <- cbind(raw_coordinate_trial1_data[,1:2],
                                             apply(raw_coordinate_trial1_data[,3:6], MARGIN = 2, FUN = function(x){
  return((x - mean(x))/sd(x))
}))
ZscoredScaledcoordinate_trial12_data <- cbind(raw_coordinate_trial12_data[,1:2],
                                             apply(raw_coordinate_trial12_data[,3:6], MARGIN = 2, FUN = function(x){
                                               return((x - mean(x))/sd(x))
                                             }))
Minmaxscalecoordinate_trial1_data <- cbind(raw_coordinate_trial1_data[,1:2],
                                           apply(raw_coordinate_trial1_data[,3:6], MARGIN = 2, FUN = function(x){
                                             return((x - min(x))/(max(x) - min(x)))
                                           }))
Minmaxscalecoordinate_trial12_data <- cbind(raw_coordinate_trial12_data[,1:2],
                                           apply(raw_coordinate_trial12_data[,3:6], MARGIN = 2, FUN = function(x){
                                             return((x - min(x))/(max(x) - min(x)))
                                           }))

# Saving the dataset -----
sink('output_data/New_data/CoordinateDatasets/RawMidlineCoordinates_data_Trial1.csv')
write.csv(raw_coordinate_trial1_data)
sink()

sink('output_data/New_data/CoordinateDatasets/RawMidlineCoordinates_data_Trial12.csv')
write.csv(raw_coordinate_trial12_data)
sink()

sink('output_data/New_data/CoordinateDatasets/MinmaxscaleCoordinates_data_Trial1.csv')
write.csv(Minmaxscalecoordinate_trial1_data)
sink()

sink('output_data/New_data/CoordinateDatasets/MinmaxscaleCoordinates_data_Trial12.csv')
write.csv(Minmaxscalecoordinate_trial12_data)
sink()

sink('output_data/New_data/CoordinateDatasets/ZscoredScaledCoordinates_data_Trial1.csv')
write.csv(ZscoredScaledcoordinate_trial1_data)
sink()

sink('output_data/New_data/CoordinateDatasets/ZscoredScaledCoordinates_data_Trial12.csv')
write.csv(ZscoredScaledcoordinate_trial12_data)
sink()
