## Dissertation Intro - Figures
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

###### Setup #####
rm(list=ls())

setwd("C:/Users/steph/Documents/Courses/PhD/Papers/Intro/ANES 2020")

library(foreign)
library(dplyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(scales)

anes_2020 <- read.csv("C:/Users/steph/Documents/Courses/PhD/Papers/Intro/ANES 2020/anes_timeseries_2020_csv_20220210.csv")
anes_2020x <- anes_2020 %>% 
  select(V201228, V201549x, V201312, V202421:V202424, V202300:V202303, V202237, V202233)

#
###### Cleanup #####
anes_2020x$party_id <- anes_2020x$V201228
anes_2020x$party_id %<>%
  plyr::mapvalues(
    c("-9", "-8", "-4", "0", "1", "2", "3", "5"),
    c(NA, NA, NA, NA, "Democrat", "Republican", "Independent", NA)
  ) %>% 
  as.vector()

anes_2020x$race <- anes_2020x$V201549x
anes_2020x$race %<>%
  plyr::mapvalues(
    c("-9", "-8", "1", "2", "3", "4", "5", "6"),
    c(NA, NA, "White", "Black", "Hispanic", "Asian", "Native American", NA)
  ) %>% 
  as.vector()

anes_2020x$V201312 %<>%
  plyr::mapvalues(
    c("-9", "-8", "1", "2", "3"),
    c(NA, NA, "0", "1", "0.5") # recode higher number means welfare spending should be decreased
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202421 %<>%
  plyr::mapvalues(
    c("-9", "-8", "-7", "-6", "-5", "1", "2", "3", "4"),
    c(NA, NA, NA, NA, NA, "4", "3", "2", "1") # recode higher number means greater nationalist identity
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202422 %<>%
  plyr::mapvalues(
    c("-9", "-8", "-7", "-6", "-5", "1", "2", "3", "4"),
    c(NA, NA, NA, NA, NA, "4", "3", "2", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202423 %<>%
  plyr::mapvalues(
    c("-9", "-7", "-6", "-5", "1", "2", "3", "4"),
    c(NA, NA, NA, NA, "4", "3", "2", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202424 %<>%
  plyr::mapvalues(
    c("-9", "-8", "-7", "-6", "-5", "1", "2", "3", "4"),
    c(NA, NA, NA, NA, NA, "4", "3", "2", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202233 %<>%
  plyr::mapvalues(
    c("-9", "-8", "-7", "-6", "-5", "1", "2", "3", "4"),
    c(NA, NA, NA, NA, NA, "4", "3", "2", "1") # recoded higher value means more likely immigration takes jobs
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202237 %<>%
  plyr::mapvalues(
    c("-9", "-8", "-7", "-6", "-5", "1", "2", "3"),
    c(NA, NA, NA, NA, NA, "1", "0", "0.5") # recode higher means increase crime
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202300 %<>%
  plyr::mapvalues(
    c("-9", "-8", "-7", "-6", "-5", "1", "2", "3", "4", "5"),
    c(NA, NA, NA, NA, NA, "5","4", "3", "2", "1") # recode higher means higher racial resentment
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202301 %<>%
  plyr::mapvalues(
    c("-9", "-7", "-6", "-5", "1", "2", "3", "4", "5"),
    c(NA, NA, NA, NA, "1", "2", "3", "4", "5")
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202302 %<>%
  plyr::mapvalues(
    c("-9", "-8", "-7", "-6", "-5", "1", "2", "3", "4", "5"),
    c(NA, NA, NA, NA, NA, "1", "2", "3", "4", "5")
  ) %>% 
  as.vector() %>% 
  as.numeric()

anes_2020x$V202303 %<>%
  plyr::mapvalues(
    c("-9", "-8", "-7", "-6", "-5", "1", "2", "3", "4", "5"),
    c(NA, NA, NA, NA, NA, "5","4", "3", "2", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric()


## averages
anes_2020x$avg_nationalism <- rowMeans(anes_2020x[, c("V202421", "V202422", "V202423", "V202424")], na.rm = TRUE)
anes_2020x$racial_resent <- rowMeans(anes_2020x[, c("V202300", "V202301", "V202302", "V202303")], na.rm = TRUE)

## Rescaling
anes_2020x$avg_nationalism <- rescale(anes_2020x$avg_nationalism, to = c(0,1))
anes_2020x$racial_resent <- rescale(anes_2020x$racial_resent, to = c(0,1))

## removing any missing race and party ID data
anes_2020x <- anes_2020x %>% 
  filter(!is.na(race))
anes_2020x <- anes_2020x %>% 
  filter(!is.na(party_id))

## quick table for race breakdown
table(anes_2020x$race)

#

###### Figures #####

## Correlation between nationalist attitudes and type of immigrant threat for each race group
correlations_by_race <- anes_2020x %>%
  group_by(race) %>%
  summarise(
    Criminal = cor(avg_nationalism, V202237, use = "complete.obs"),
    Economic = cor(avg_nationalism, V202233, use = "complete.obs")
  ) %>%
  gather(key = "Threat", value = "Correlation", Criminal, Economic)

## Heatmap
ggplot(correlations_by_race, aes(x = race, y = Threat, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "aliceblue", mid = "slategray3", high = "slategray4", midpoint = 0.5, limit = c(0, 1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Between National Identity \n and Perceived Immigrant Threat by Race",
       x = "Race",
       y = "Threat Type") +
  geom_text(aes(label = round(Correlation, 3)), color = "black", size = 3)

## Racial Resentment and Welfare Spending Attitudes
## Binned Scatterplot
ggplot(anes_2020x, aes(x = racial_resent, y = V201312, color = race)) +
  stat_summary_bin(fun = mean, bins = 6, geom = "point", size = 3) + 
  stat_summary_bin(fun = mean, bins = 6, geom = "line") + 
  facet_wrap(~ race, scales = "free_x") + 
  scale_x_continuous(breaks = seq(min(anes_2020x$racial_resent, na.rm = TRUE),
                                  max(anes_2020x$racial_resent, na.rm = TRUE), length.out = 6)) +  # Custom x-axis breaks
  scale_color_manual(values = c("Asian" = "mediumorchid4", "Black" = "darkblue", "White" = "darkred", "Hispanic" = "orange", "Native American" = "darkgreen")) +
  labs(title = "Racial Resentment and Attitudes Toward Welfare Spending",
       x = "Racial Resentment (Binned)",
       y = "Decrease Welfare Spending",
       color = "Race") +
  theme_minimal()

## Racial Resentment by Race
ggplot(anes_2020x, aes(x = factor(race), y = racial_resent, fill = race)) +
  geom_violin(alpha = 0.7) +
  geom_jitter(aes(color = race), width = 0.2, alpha = 0.5, size = 1) +
  labs(title = "Racial Resentment by Race",
       x = "Race/Ethnicity",
       y = "Racial Resentment",
       fill = "Race",
       color = "Race") +
  theme_minimal()

#


###### SP Notes #####

## ANES 2020 Quick Guide
# Race V201549x (ANES CODING: 1 white, 2 black, 3 Hispanic, 4 AAPI, 5 Native American, 6 Multiracial, anything else NA)
# PID3: V201228
# Welfare spending: V201312
# Nationalism: V202421 to V202424
# RR V202300 to V202303
# Immigration: Econ threat V202233
# Immigration: Crime V202237
