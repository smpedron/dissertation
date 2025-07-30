## Effect size
## Stephanie Pedron (pedron.2@osu.edu)

rm(list = ls())

source("C:/Users/steph/Documents/Courses/PhD/Papers/RP Stuff/codes/rp_datacleaner.r")

library(effsize)

# Sample
control_group <- rp_data[rp_data$exp_condition == "Control", ]
treat1 <- rp_data[rp_data$exp_condition == "TreatmentA-SoftSell", ]
treat2 <- rp_data[rp_data$exp_condition == "TreatmentB-HardSell", ]
treat3 <- rp_data[rp_data$exp_condition == "TreatmentC-ProDiversity", ]

# Pairwise Cohen's d:
cohen.d(treat1$antidiversity_DV, control_group$antidiversity_DV)
cohen.d(treat2$antidiversity_DV, control_group$antidiversity_DV)
cohen.d(treat3$antidiversity_DV, control_group$antidiversity_DV)

cohen.d(treat1$monoculturalism_DV, control_group$monoculturalism_DV)
cohen.d(treat2$monoculturalism_DV, control_group$monoculturalism_DV)
cohen.d(treat3$monoculturalism_DV, control_group$monoculturalism_DV)

