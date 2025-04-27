## Persuasion / Threat Checks + Mediation Analysis
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

rm(list = ls())

source("C:/Users/steph/Documents/Courses/PhD/Papers/RP Stuff/codes/rp_datacleaner.r")
library(mediation)

#$ basic
summary(lm(persuasion_check ~ exp_condition, rp_data)) # less persuaded
summary(lm(threat_check ~ exp_condition, rp_data)) # more threatened

## Mediation
rp_data$SoftSell_vs_Control <- ifelse(rp_data$exp_condition == "TreatmentA-SoftSell", 1, 
                                          ifelse(rp_data$exp_condition == "Control", 0, NA))

rp_data$HardSell_vs_Control <- ifelse(rp_data$exp_condition == "TreatmentB-HardSell", 1, 
                                          ifelse(rp_data$exp_condition == "Control", 0, NA))

rp_data$ProDiversity_vs_Control <- ifelse(rp_data$exp_condition == "TreatmentC-ProDiversity", 1, 
                                              ifelse(rp_data$exp_condition == "Control", 0, NA))



run_mediation <- function(treat_var, mediator_var, outcome_var, data) {
  mediator_model <- lm(reformulate(treat_var, response = mediator_var), data = data)
  outcome_model <- lm(reformulate(c(mediator_var, treat_var), response = outcome_var), data = data)
  
  mediate(mediator_model, outcome_model, treat = treat_var, mediator = mediator_var, sims = 500)
}


## Persuasion as mediator for Anti-diversity
mediation_SoftSell_persuasion <- run_mediation("SoftSell_vs_Control", "persuasion_check", "antidiversity_DV", rp_data)
summary(mediation_SoftSell_persuasion) 

mediation_HardSell_persuasion <- run_mediation("HardSell_vs_Control", "persuasion_check", "antidiversity_DV", rp_data)
summary(mediation_HardSell_persuasion)

mediation_ProDiversity_persuasion <- run_mediation("ProDiversity_vs_Control", "persuasion_check", "antidiversity_DV", rp_data)
summary(mediation_ProDiversity_persuasion) 


## Threat as mediator for Anti-diversity
mediation_SoftSell_threat <- run_mediation("SoftSell_vs_Control", "threat_check", "antidiversity_DV", rp_data)
summary(mediation_SoftSell_threat)

mediation_HardSell_threat <- run_mediation("HardSell_vs_Control", "threat_check", "antidiversity_DV", rp_data)
summary(mediation_HardSell_threat)

mediation_ProDiversity_threat <- run_mediation("ProDiversity_vs_Control", "threat_check", "antidiversity_DV", rp_data)
summary(mediation_ProDiversity_threat) 


## Persuasion as mediator for Monoculturalism
mediation_SoftSell_persuasion2 <- run_mediation("SoftSell_vs_Control", "persuasion_check", "monoculturalism_DV", rp_data)
summary(mediation_SoftSell_persuasion2)

mediation_HardSell_persuasion2 <- run_mediation("HardSell_vs_Control", "persuasion_check", "monoculturalism_DV", rp_data)
summary(mediation_HardSell_persuasion2)

mediation_ProDiversity_persuasion2 <- run_mediation("ProDiversity_vs_Control", "persuasion_check", "monoculturalism_DV", rp_data)
summary(mediation_ProDiversity_persuasion2)


## Threat as mediator for Monoculturalism
mediation_SoftSell_threat2 <- run_mediation("SoftSell_vs_Control", "threat_check", "monoculturalism_DV", rp_data)
summary(mediation_SoftSell_threat2)

mediation_HardSell_threat2 <- run_mediation("HardSell_vs_Control", "threat_check", "monoculturalism_DV", rp_data)
summary(mediation_HardSell_threat2)

mediation_ProDiversity_threat2 <- run_mediation("ProDiversity_vs_Control", "threat_check", "monoculturalism_DV", rp_data)
summary(mediation_ProDiversity_threat2) 
