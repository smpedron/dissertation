## RP - ATE by Attention Levels
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

rp_data <- read.csv("C:/Users/steph/Documents/Courses/PhD/Papers/RP Stuff/Data/prolific_data.csv")

#
###### Cleanup #####

rp_data <- rp_data %>% 
  filter(Finished == 1) # removing those who didn't finish

rp_data <- rp_data %>% 
  filter(Duration..in.seconds. >= 180) # removing those who finished in under 3 minutes

rp_data <- rp_data %>%
  filter(citizenship == 1) # keeping only US citizens

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

rp_data$monoculturalism_DV <- rowMeans(rp_data[, c("MC_cultural1", "MC_cultural2", "MC_cultural3", "MC_cultural4")])

#
###### Attention Levels #####

## Attention checks
rp_data$attn1_correct <- rp_data$attention_check1 == 72
rp_data$attn2_correct <- rp_data$attention_check2 == 1
rp_data$attn3_correct <- rp_data$attention_check3 == 5
rp_data$attentiveness <- rowSums(rp_data[, c("attn1_correct", "attn2_correct", "attn3_correct")])

rp_data <- rp_data %>%
  mutate(attention_group = case_when(
    attentiveness == 3 ~ "High",
    attentiveness == 2 ~ "Medium",
    attentiveness <= 1 ~ "Low"
  ))


model1 <- lm(antidiversity_DV ~ exp_condition * attentiveness + race + pid7 + gender + educ + employment_status +
               income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, data = rp_data)
attention_effects <- avg_comparisons(
  model1,
  variables = list(exp_condition = "pairwise"),
  by = "attention_group"
)

ggplot(attention_effects, aes(x = attention_group, y = estimate, color = attention_group)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.5) + 
  labs(title = "ATE by Attention Levels (Anti-Diversity)", 
       x = "Attention Group", y = "ATE") +
  scale_color_manual(values = c("red", "green", "blue")) + 
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

model2 <- lm(monoculturalism_DV ~ exp_condition * attentiveness + race + pid7 + gender + educ + employment_status +
               income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, data = rp_data)
attention_effects2 <- avg_comparisons(
  model2,
  variables = list(exp_condition = "pairwise"),
  by = "attention_group"
)
ggplot(attention_effects2, aes(x = attention_group, y = estimate, color = attention_group)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.5) + 
  labs(title = "ATE by Attention Levels (Monoculturalism)", 
       x = "Attention Group", y = "ATE") +
  scale_color_manual(values = c("red", "green", "blue")) + 
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
