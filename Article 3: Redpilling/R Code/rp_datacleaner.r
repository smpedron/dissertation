## RP Data Cleaning
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

###### Setup #####

rm(list=ls())

setwd("C:/Users/steph/Documents/Courses/PhD/Papers/RP Stuff")

library(foreign)
library(dplyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(marginaleffects)
library(emmeans)
library(survey)
library(sandwich)
library(lmtest)
library(texreg)
library(patchwork)


rp_data <- read.csv("C:/Users/steph/Documents/Courses/PhD/Papers/RP Stuff/Data/prolific_data.csv")

# saveRDS(rp_data, file = "rp_data.rds")

#
###### Cleanup #####

rp_data <- rp_data %>% 
  filter(Finished == 1) # removing those who didn't finish

rp_data <- rp_data %>% 
  filter(Duration..in.seconds. >= 180) # removing those who finished in under 3 minutes

rp_data <- rp_data %>%
  filter(citizenship == 1) # keeping only US citizens

## Dropping those who failed 2 out of 3 attention checks
rp_data$attn1_correct <- rp_data$attention_check1 == 72
rp_data$attn2_correct <- rp_data$attention_check2 == 1
rp_data$attn3_correct <- rp_data$attention_check3 == 5
rp_data$attn_total_correct <- rowSums(rp_data[, c("attn1_correct", "attn2_correct", "attn3_correct")])
rp_data$pass_attention <- rp_data$attn_total_correct >= 2
rp_data <- subset(rp_data, pass_attention == TRUE)

## Making a variable for those who failed the treatment comprehension check
## for extra analysis in appendix (i.e., subset to those who only passed for robustness check)
rp_data$exp_condition2 <- rp_data$exp_condition %>%
  plyr::mapvalues(
    c("Control", "TreatmentA-SoftSell", "TreatmentB-HardSell", "TreatmentC-ProDiversity"),
    c("4", "1", "2", "3")
  ) %>%
  as.vector() %>%
  as.numeric()

table(rp_data$exp_condition2)
table(rp_data$exp_condition)

rp_data <- rp_data %>%
  mutate(
    passed = ifelse(exp_condition2 == mcheck_closed, 1, 0))

rp_data <- rp_data %>%
  mutate(
    passed = factor(ifelse(exp_condition2 == mcheck_closed, 1, 0),
                    levels = c(0, 1),
                    labels = c("Failed", "Passed")))

# rp_data <- rp_data %>%
#   filter(exp_condition2 == mcheck_closed)


## Variable Cleanup
rp_data$race %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8"),
    c("Other", "Asian", "Black", "Hispanic", "Other", "Other", "White", "Other")
  ) %>%
  as.vector()

table(rp_data$race_commitment, rp_data$race)
table(rp_data$american_commitment, rp_data$race)

rp_data$race_commitment %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("Low", "Low", "Low", "High", "High")
  ) %>%
  as.vector()

rp_data$american_commitment %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("Low", "Low", "Low", "High", "High")
  ) %>%
  as.vector()

table(rp_data$race_commitment, rp_data$race)
table(rp_data$american_commitment, rp_data$race)

rp_data <- rp_data %>% 
  filter(race != "White" & race != "Other") # keeping only Asians, Blacks, Latinx

rp_data$age %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or older")
  ) %>%
  as.vector()

rp_data$gender %<>%
  plyr::mapvalues(
    c("0", "1"),
    c("Female", "Male")
  ) %>% 
  as.vector()

rp_data$educ2 <- rp_data$educ
rp_data$educ %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("No HS Diploma", "HS Diploma", "HS Diploma", "HS Diploma", "College Degree", "Graduate Degree", "Graduate Degree")
  ) %>% 
  as.vector() %>% 
  as.factor()
rp_data$educ <- relevel(rp_data$educ, ref = "College Degree")

rp_data$employment_type %<>%
  plyr::mapvalues(
    c("0", "1", "99"),
    c("Blue-collar", "White-collar", NA)
  ) %>% 
  as.vector() %>% 
  as.factor()
rp_data$employment_type <- relevel(rp_data$employment_type, ref = "Blue-collar")

rp_data$employment_status %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("Employed", "Employed", "Unemployed", "Unemployed", "Other", "Other", NA)
  ) %>% 
  as.vector()

rp_data$income %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("Less than $25,000", "$25,000-$49,999", "$50,000-$74,999", "$75,000-$99,999", "Over $100,000", "Over $100,000", NA)
  ) %>% 
  as.vector() %>% 
  as.factor()
rp_data$income <- relevel(rp_data$income, ref = "Less than $25,000")

rp_data$ideo5 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("Liberal", "Liberal", "Moderate", "Conservative", "Conservative")
  ) %>% 
  as.vector() %>% 
  as.factor()
rp_data$ideo5 <- relevel(rp_data$ideo5, ref = "Liberal")

rp_data$pid72 <- rp_data$pid7
rp_data$pid7 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("Democrat", "Democrat", "Democrat", "Independent", "Republican", "Republican", "Republican")
  ) %>% 
  as.vector() 

## Ethnocentrism (higher = more ethnocentrism)
rp_data <- rp_data %>%
  rowwise() %>%
  mutate(
    outgroup_therm_mean = case_when(
      race == "Black" ~ mean(c(ethnocentrism_1, ethnocentrism_2, ethnocentrism_4, ethnocentrism_5, ethnocentrism_6), na.rm = TRUE),
      race == "Asian" ~ mean(c(ethnocentrism_1, ethnocentrism_3, ethnocentrism_4, ethnocentrism_5, ethnocentrism_6), na.rm = TRUE),
      race == "Hispanic" ~ mean(c(ethnocentrism_1, ethnocentrism_2, ethnocentrism_3, ethnocentrism_4, ethnocentrism_5), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    ethnocentrism_score = 100 - outgroup_therm_mean
  ) %>%
  ungroup()

## Covariate Scales
rp_data$expert_mistrust <- rowMeans(rp_data[, c("expert_mistrust1", "expert_mistrust2")])
rp_data$authoritarianism <- rowMeans(rp_data[, c("authoritarianism1", "authoritarianism2", "authoritarianism3", "authoritarianism4")])
rp_data$egalitarianism <- rowMeans(rp_data[, c("egalitarianism1", "egalitarianism2", "egalitarianism3")])
rp_data$anti_elitism <- rowMeans(rp_data[, c("anti.elitism1", "anti.elitism2", "anti.elitism3")])

## Diversity DV Scale
rp_data$antidiversity_DV <- rowMeans(rp_data[, c("diversity1", "diversity2", "diversity3", "diversity4", "diversity5", "diversity6")], na.rm = TRUE)

## Multiculturalism DV Subscales
rp_data$mc_cultural_DV <- rowMeans(rp_data[, c("MC_cultural1", "MC_cultural2", "MC_cultural3", "MC_cultural4")])
rp_data$mc_social_DV <- rowMeans(rp_data[, c("MC_social1", "MC_social2", "MC_social3", "MC_social4")])
rp_data$mc_diversitycons_DV <- rowMeans(rp_data[, c("MC_diversitycons1", "MC_diversitycons2", "MC_diversitycons3", "MC_diversitycons4")])
rp_data$mc_equity_DV <- rowMeans(rp_data[, c("MC_equity1", "MC_equity2", "MC_equity3", "MC_equity4")])

rp_data$monoculturalism_DV <- rowMeans(rp_data[, c("mc_cultural_DV", "mc_social_DV", "mc_diversitycons_DV", "mc_equity_DV")])

## Distribution of DVs
# hist(rp_data$diversity_DV)
# hist(rp_data$monoculturalism_DV)
# hist(rp_data$mc_social_DV)
# hist(rp_data$mc_cultural_DV)
# hist(rp_data$mc_diversitycons_DV)
# hist(rp_data$mc_equity_DV)

#
