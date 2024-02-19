####################################################################################################
###
### File:    08_clustering_analysis.r
### Purpose: Clustering analysis of the datasets created based on EEG data
### Authors: Gabriel Rodrigues Palma, Rafael Moral
### Date:    20/06/23
###
####################################################################################################
# Loading the results -----
EEG_data_control <- read_excel(here("input_data/FrontalThetaData/ControlFrontalThetaData.xlsx"))
EEG_data_control <- EEG_data_control %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                                Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))
EEG_data_experiment <- read_excel(here("input_data/FrontalThetaData/ExperimentFrontalThetaData.xlsx"))
EEG_data_experiment<- EEG_data_experiment %>% mutate(ZscoredScaled = (RawMidline - mean(RawMidline))/sd(RawMidline), 
                                                     Minmaxscale = (RawMidline - min(RawMidline))/(max(RawMidline) - min(RawMidline)))
EEG_data <- rbind(EEG_data_control, 
             EEG_data_experiment)

ZscoredScaledPlusCoordinateTrial1 <- read.csv('output_data/New_data/CoordinatesPlusEEGData/ZscoredScaledPlusCoordinateTrial1.csv')[,-1]
ZscoredScaledPlusCoordinateTrial1$Subject <- factor(ZscoredScaledPlusCoordinateTrial1$Subject)

ZscoredScaledPlusCoordinateTrial12 <- read.csv('output_data/New_data/CoordinatesPlusEEGData/ZscoredScaledPlusCoordinateTrial12.csv')[,-1]
ZscoredScaledPlusCoordinateTrial12$Subject <- factor(ZscoredScaledPlusCoordinateTrial12$Subject)
levels(ZscoredScaledPlusCoordinateTrial12$Subject)

ZscoredScaledPlusCoordinateTrial12 %>%
  dplyr::select(Subject, Class)
EEG_data %>%
  group_by(Class, Trial) %>%
  count()
##################################################################################################
###################### Clustering for trial 1 ############################################
##################################################################################################
cols <-  c("#A3C4D9", "#043259")
EEG_data_trial1 <- EEG_data %>%
  filter(Trial == '1') %>%
  filter((!Subject %in% c("CTEEG053", "CTEEG054", "CTEEG055",
                      "CTEEG056", "CTEEG059", "CTEEG061",
                      "CTEEG062", "CTEEG065")))
EEG_data_trial1$Subject <- factor(EEG_data_trial1$Subject)
levels(EEG_data_trial1$Subject) <- c("E02", "E03", "E04", 
                                     "E05", "E06", "E08",
                                     "E09", "E11", "E12",
                                     "E13", "E15", "E17",
                                     "E18", "E19", "E21",
                                     "E22", "E23", "E24" ,
                                     "C30", "C31", "C32",
                                     "C33", "C34", "C35",
                                     "C36", "C37", "C38",
                                     "C41", "C42", "C43",
                                     "C46", "C47", "C48",
                                     "E49", "C51", "C52",
                                     "E57", "E58", "E60")
EEG_data_trial1 %>%
  group_by(Subject) %>%
  summarise(Time = max(Time)) %>%
  pull(Time) %>%
  min

EEG_data_trial1_sync <- EEG_data_trial1 %>%
  group_by(Subject) %>%
  mutate(Time_rev = Time - max(Time)) %>%
  ungroup %>%
  filter(Time_rev > - 3.625) %>%
  mutate(Time_cat = cut(Time_rev, breaks = 200)) %>%
  group_by(Subject, Time_cat) %>%
  summarise(Time_rev = mean(Time_rev),
            RawMidline = mean(RawMidline),
            ZscoredScaled = mean(ZscoredScaled),
            Minmaxscale = mean(Minmaxscale)) %>%
  ungroup
## z-scaled plot

EEG_data_trial1_ZscoredScaled <- EEG_data_trial1_sync %>%
  dplyr::select(Subject, ZscoredScaled) %>%
  split(EEG_data_trial1_sync$Subject)

EEG_data_trial1_ZscoredScaled_wide <- do.call(cbind, 
                                           lapply(EEG_data_trial1_ZscoredScaled, function(x) x$ZscoredScaled))

tsdist <- diss(t(EEG_data_trial1_ZscoredScaled_wide), "DTWARP")

hc <- hclust(tsdist, "ward.D2")

plot(as.phylo(hc), font = 1, tip.color = c(rep(cols[1], 25), rep(cols[2], 24)),
     edge.width = .7)
dendogram1 <- as.dendrogram(hc)
# Working with the pre processed features -----
levels(ZscoredScaledPlusCoordinateTrial1$Subject) <- c("E02", "E03", "E04", 
                                                       "E05", "E06", "E08",
                                                       "E09", "E11", "E12",
                                                       "E13", "E15", "E17",
                                                       "E18", "E19", "E21",
                                                       "E22", "E23", "E24" ,
                                                       "C30", "C31", "C32",
                                                       "C33", "C34", "C35",
                                                       "C36", "C37", "C38",
                                                       "C41", "C42", "C43",
                                                       "C46", "C47", "C48",
                                                       "E49", "C51", "C52",
                                                       "E57", "E58", "E60")
ZscoredScaledPlusCoordinateTrial1_list <- ZscoredScaledPlusCoordinateTrial1 %>%
  dplyr::select("MM_Intercept", "MM_Hess",              
                "average_peak_magnitude", "average_peak_curvature", 
                "hmm5_mean.1", "hmm5_mean.2", "hmm5_mean.3",
                "hmm5_mean.4", "hmm5_mean.5", "hmm5_sd.1", 
                "hmm5_sd.2", "hmm5_sd.3", "hmm5_sd.4", 
                "hmm5_sd.5", "hmm5_p1", "hmm5_p2", "hmm5_p3",                
                "hmm5_p4", "dis_sum", "angle_sum", 
                "average_speed", "idle_time") %>%
  t()

ZscoredScaledPlusCoordinateTrial1_subjects <- ZscoredScaledPlusCoordinateTrial1 %>%
  dplyr::select('Subject') %>% t() %>% as.vector()
colnames(ZscoredScaledPlusCoordinateTrial1_list) <- ZscoredScaledPlusCoordinateTrial1_subjects
rownames(ZscoredScaledPlusCoordinateTrial1_list) <- NULL

ZscoredScaledPlusCoordinateTrial1_list <- ZscoredScaledPlusCoordinateTrial1_list[-1,]

tsdist2 <- diss(t(ZscoredScaledPlusCoordinateTrial1_list), "DTWARP")

hc2 <- hclust(tsdist2, "ward.D2")

plot(as.phylo(hc), font = 1, tip.color = c(rep(cols[1], 25), rep(cols[2], 24)),
     edge.width = .7)
dendogram2 <- as.dendrogram(hc2)

## Combining both dendograms

dendogram1_labels <- c("#043259", "#043259", "#043259",
                       "#A3C4D9", "#043259", "#A3C4D9",
                       "#043259", "#043259", "#043259",
                       "#043259", "#043259", "#A3C4D9",
                       "#A3C4D9", "#043259", "#A3C4D9",
                       "#043259", "#A3C4D9", "#A3C4D9",
                       "#043259", "#A3C4D9", "#043259",
                       "#043259", "#043259", "#043259", 
                       "#043259", "#043259", "#A3C4D9",
                       "#A3C4D9", "#043259", "#A3C4D9",
                       "#043259", "#A3C4D9", "#043259",
                       "#A3C4D9", "#A3C4D9", "#A3C4D9", 
                       "#A3C4D9", "#043259", "#A3C4D9")
dendogram2_labels <- c("#A3C4D9" ,"#043259", "#043259",
                       "#043259", "#043259", "#043259",
                       "#043259", "#043259", "#A3C4D9",
                       "#A3C4D9", "#A3C4D9", "#A3C4D9",
                       "#043259", "#043259", "#043259",
                       "#043259", "#043259", "#A3C4D9",
                       "#A3C4D9", "#A3C4D9", "#A3C4D9",
                       "#043259", "#043259", "#043259",
                       "#A3C4D9", "#043259", "#043259",
                       "#A3C4D9", "#A3C4D9", "#043259",
                       "#043259", "#043259", "#A3C4D9",
                       "#A3C4D9", "#A3C4D9", "#043259",
                       "#043259", "#A3C4D9", "#A3C4D9")
dendogram1 <- dendogram1 %>%
  set("labels_col", dendogram1_labels) %>%
  highlight_branches_lwd 
dendogram2 <- dendogram2 %>%
  set("labels_col", dendogram2_labels) %>%
  highlight_branches_lwd 
dl <- dendlist(dendogram1, dendogram2)
tanglegram(dl, sort = F, common_subtrees_color_lines = F, 
           highlight_distinct_edges  = FALSE, highlight_branches_lwd = FALSE, 
           common_subtrees_color_branches = TRUE)

