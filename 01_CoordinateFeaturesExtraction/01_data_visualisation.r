####################################################################################################
###
### File:    01_data_visualisation.R
### Purpose: Obtaining the features related to the coordinates.
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    04/04/23
###
####################################################################################################
# Loading packages and functions -----
source(here('00_source.r'))

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

# Visualisation
OriginalCoordinateData %>%
  ggplot(mapping = aes(x = X, y = Y, colour = Subject)) +
  geom_point() +
  theme_new() +
  facet_wrap(~Trial) + theme(legend.position = "none")
ggsave('Plots/SubjectsCoordinatesData.png', dpi = 400, width = 6, height = 4)

OriginalCoordinateData %>%
  ggplot(mapping = aes(x = X, y = Y, colour = Class)) +
  geom_point() +
  theme_new()  +
  facet_wrap(~Trial) +
  scale_color_manual(values = c("#D9A3A3", "#043259"))
ggsave('Plots/Final_paper/ClassesCoordinatesData.png', dpi = 400, width = 6, height = 4)


ggpairs(CoordinatesData %>%
          filter(Trial == 'Trial 1'), columns = 3:6, ggplot2::aes(colour = Class), columnLabels = c("Total distance", 
                                                                                         "Total angle shift", 
                                                                                         "Average speed",
                                                                                         "Idle time"))+
  theme_new() +
  scale_color_manual(values = c("#A3C4D9", "#043259")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259"))

ggsave('Plots/CoordinatesDataT1.png', dpi = 400, width = 8, height = 6)
ggpairs(CoordinatesData %>%
          filter(Trial == 'Trial 12'), columns = 3:6, ggplot2::aes(colour = Class), columnLabels = c("Total distance", 
                                                                                              "Total angle shift", 
                                                                                              "Average speed",
                                                                                              "Idle time"))+
  theme_new() +
  scale_color_manual(values = c("#A3C4D9", "#043259")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259"))
ggsave('Plots/CoordinatesDataT12.png', dpi = 400, width = 8, height = 6)

CoordinatesData %>%
  ggplot(mapping = aes(x = dis_sum, 
                       y = idle_time, colour = Class)) +
  geom_point() + theme_new() +
  ylab('Idle time') +
  xlab('Total distance') +
  facet_wrap(~Trial, scales="free") +
  scale_color_manual(values = c("#A3C4D9", "#043259")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259"))
ggsave('Plots/DistanceVSIdleTimePerTrial.png', dpi = 400, width = 6, height = 4)

## 3D plots
fig <- plot_ly(CoordinatesData %>%
                 filter(Trial == '1'), x = ~dis_sum, y = ~angle_sum, z = ~average_speed, 
               color = ~Class, colors = c('#cdfb43', '#232323'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Distance sum'),
                                   yaxis = list(title = 'Angle sum'),
                                   zaxis = list(title = 'Speed (m/s)')))

fig


fig <- plot_ly(CoordinatesData %>%
                 filter(Trial == '12'), x = ~dis_sum, y = ~angle_sum, z = ~average_speed, 
               color = ~Class, colors = c('#cdfb43', '#232323'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Distance sum'),
                                   yaxis = list(title = 'Angle sum'),
                                   zaxis = list(title = 'Speed (m/s)')))

fig
