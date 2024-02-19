####################################################################################################
###
### File:    03_Merging_data.r
### Purpose: Merging the coordinate features with the EEG features.
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    04/04/23
###
####################################################################################################

# Loading packages ----
source(here('00_source.r'))

# Loading data -----
MinmaxscaleTrial1 <- read.csv(here("output_data/New_data/SplitDatasets/MinmaxscaleTrial1.csv"))[, -1]
MinmaxscaleTrial12 <- read.csv(here("output_data/New_data/SplitDatasets/MinmaxscaleTrial12.csv"))[, -1]

RawMidlineTrial1 <- read.csv(here("output_data/New_data/SplitDatasets/RawMidlineTrial1.csv"))[, -1]
RawMidlineTrial12 <- read.csv(here("output_data/New_data/SplitDatasets/RawMidlineTrial12.csv"))[, -1]

ZscoredScaledTrial1 <- read.csv(here("output_data/New_data/SplitDatasets/ZscoredScaledTrial1.csv"))[, -1]
ZscoredScaledTrial12 <- read.csv(here("output_data/New_data/SplitDatasets/ZscoredScaledTrial12.csv"))[, -1]

RawMidlineCoordinatesDataTrial1 <- read.csv(here("output_data/New_data/CoordinateDatasets/RawMidlineCoordinates_data_Trial1.csv"))[, -1]
RawMidlineCoordinatesDataTrial12 <- read.csv(here("output_data/New_data/CoordinateDatasets/RawMidlineCoordinates_data_Trial12.csv"))[, -1]

ZscoredScaledCoordinatesDataTrial1 <- read.csv(here("output_data/New_data/CoordinateDatasets/ZscoredScaledCoordinates_data_Trial1.csv"))[, -1]
ZscoredScaledCoordinatesDataTrial12 <- read.csv(here("output_data/New_data/CoordinateDatasets/ZscoredScaledCoordinates_data_Trial12.csv"))[, -1]

MinmaxscaleCoordinatesDataTrial1 <- read.csv(here("output_data/New_data/CoordinateDatasets/MinmaxscaleCoordinates_data_Trial1.csv"))[, -1]
MinmaxscaleCoordinatesDataTrial12 <- read.csv(here("output_data/New_data/CoordinateDatasets/MinmaxscaleCoordinates_data_Trial12.csv"))[, -1]

# Creating new data -----
CoordinatesDataTrial1 %>%
  filter(Subject %in%
           RawMidlineTrial1$Subject) %>%
  nrow()
  
RawMidlineTrial1 %>%
  nrow()
########################################################################################################################
############################## Tria 1 ####################################################################################
########################################################################################################################
## RawMidline
RawMidlinePlusCoordinateTrial1 <- 
  inner_join(RawMidlineTrial1, RawMidlineCoordinatesDataTrial1, by = "Subject")
## Saving new datasets 
sink('output_data/New_data/CoordinatesPlusEEGData/RawMidlinePlusCoordinateTrial1.csv')
write.csv(RawMidlinePlusCoordinateTrial1 %>%
            dplyr::select(-features_class))
sink()

## ZscoredScaled
ZscoredScaledPlusCoordinateTrial1 <- 
  inner_join(ZscoredScaledTrial1, ZscoredScaledCoordinatesDataTrial1, by = "Subject")
## Saving new datasets 
sink('output_data/New_data/CoordinatesPlusEEGData/ZscoredScaledPlusCoordinateTrial1.csv')
write.csv(ZscoredScaledPlusCoordinateTrial1 %>%
            dplyr::select(-features_class))
sink()

## Minmaxscale
MinmaxscalePlusCoordinateTrial1 <- 
  inner_join(MinmaxscaleTrial1, MinmaxscaleCoordinatesDataTrial1, by = "Subject")
## Saving new datasets 
sink('output_data/New_data/CoordinatesPlusEEGData/MinmaxscalePlusCoordinateTrial1.csv')
write.csv(MinmaxscalePlusCoordinateTrial1 %>%
            dplyr::select(-features_class))
sink()

########################################################################################################################
############################## Tria 12 ####################################################################################
########################################################################################################################
## RawMidline
RawMidlinePlusCoordinateTrial12 <- 
  inner_join(RawMidlineTrial12, RawMidlineCoordinatesDataTrial12, by = "Subject")
## Saving new datasets 
sink('output_data/New_data/CoordinatesPlusEEGData/RawMidlinePlusCoordinateTrial12.csv')
write.csv(RawMidlinePlusCoordinateTrial12 %>%
            dplyr::select(-features_class))
sink()

## ZscoredScaled
ZscoredScaledPlusCoordinateTrial12 <- 
  inner_join(ZscoredScaledTrial12, ZscoredScaledCoordinatesDataTrial12, by = "Subject")
## Saving new datasets 
sink('output_data/New_data/CoordinatesPlusEEGData/ZscoredScaledPlusCoordinateTrial12.csv')
write.csv(ZscoredScaledPlusCoordinateTrial12 %>%
            dplyr::select(-features_class))
sink()

## Minmaxscale
MinmaxscalePlusCoordinateTrial12 <- 
  inner_join(MinmaxscaleTrial12, MinmaxscaleCoordinatesDataTrial12, by = "Subject")
## Saving new datasets 
sink('output_data/New_data/CoordinatesPlusEEGData/MinmaxscalePlusCoordinateTrial12.csv')
write.csv(MinmaxscalePlusCoordinateTrial12 %>%
            dplyr::select(-features_class))
sink()