####################################################################################################
###
### File:    07_hmm_illustration.r
### Purpose: Creating a illustration for the HMM models idea
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    10/04/23
###
####################################################################################################
# Loading packages and functions -----
source('00_source.r')

# Creating the example -----
x1 <- rnorm(50, mean = 1, sd = 0.1)
x2 <- rnorm(50, mean = 0.4, sd = 0.1)

dataset <- data.frame(Model = c(rep('Model 1', 50), 
                     rep('Model 2', 50),
                     rep('Model 1', 50),
                     rep('Model 2', 50), 
                     rep('Model 1', 50)), 
                    Value = c(x1, x2, x1, x2, x1), 
                    Time = seq(1, 100,length.out = 250))
dataset %>%
  ggplot(mapping = aes(x = Time, y = Value)) +
  geom_line() +
  geom_point(aes( colour = Model, size = 4)) +
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  theme_new() +
  ylab('Theta power') +
  theme(legend.position = 'None')

dataset %>%
  filter(Model == 'Model 2') %>%
  ggplot(mapping = aes(x = Value)) +
  geom_density(aes(fill = "#cdfb43", colour = 'cdfb43'))+
  scale_color_manual(values = c("#cdfb43", "#232323")) +
  scale_fill_manual(values = c("#cdfb43", "#232323")) +
  theme_new()+
  theme(legend.position = 'None')

dataset %>%
  filter(Model == 'Model 1') %>%
  ggplot(mapping = aes(x = Value)) +
  geom_density(aes(fill = "#232323", colour = '#232323'))+
  scale_color_manual(values = c( "#232323")) +
  scale_fill_manual(values = c( "#232323")) +
  theme_new()+
  theme(legend.position = 'None')
