## APPENDIX: SUB-GROUP EFFECTS, Voter Fraud Paper (Prolific Sample)
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

rm(list = ls())
source("C:/Users/steph/Documents/Courses/PhD/Papers/VF Stuff/R/VF_datacleaner.r")

## NOTES:
## IRR adjustment doesn't make much of a difference in the results, so opted against it for these appendix plots for ease of reporting
## Conclusion: Results are largely negligible

## Conjoint Design - since there are profile restrictions I need to incorporate in the estimation
attribute_list <- list()
attribute_list[["gender"]] <-c("Male", "Female")
attribute_list[["education"]] <- c("College degree", "High school diploma", "Less than high school")
attribute_list[["country"]] <-  c("Mexico", "USA", "China", "Germany", "Syria", "Guatemala")
attribute_list[["citizen"]] <- c("Yes", "No")
attribute_list[["englishprof"]] <- c("Fluent English", "Broken English")
attribute_list[["conviction"]] <- c("Prior conviction (not voting related)", "No prior conviction")

constraint_list <- list()
constraint_list[[1]] <- list()
constraint_list[[1]][["country"]] <- c("USA")
constraint_list[[1]][["englishprof"]] <- c("Broken English")
constraint_list[[2]] <- list()
constraint_list[[2]][["country"]] <- c("USA")
constraint_list[[2]][["citizen"]] <- c("No")

vf <- makeDesign(type='constraints', attribute.levels=attribute_list, constraints=constraint_list)

baselines <- list()
baselines$country <- "Mexico" # Can't be USA or NAN values when interacting citizen with country
baselines$englishprof <- "Fluent English"

## AMCE/ACIE Estimates
immatt_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                    immattitude:gender + immattitude:country + immattitude:education + immattitude:englishprof + 
                    immattitude:conviction + immattitude:citizen + immattitude:citizen*conviction + 
                    immattitude:country*conviction + immattitude:country*citizen + immattitude:education*citizen,
                  data = prolific_data, respondent.id = "id", respondent.varying = "immattitude", baseline = baselines, 
                  cluster = FALSE, na.ignore = TRUE, design = vf)
monitor_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                     monitor:gender + monitor:country + monitor:education + monitor:englishprof + monitor:conviction + monitor:citizen +
                     monitor:citizen*conviction + monitor:country*conviction + monitor:country*citizen + monitor:education*citizen,
                   data = prolific_data, respondent.id = "id", respondent.varying = "monitor", baseline = baselines, 
                   cluster = FALSE, na.ignore = TRUE, design = vf)
govtrust_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + govtrust:gender + 
                      govtrust:country + govtrust:education + govtrust:englishprof + govtrust:conviction + govtrust:citizen + 
                      govtrust:citizen*conviction + govtrust:country*conviction + govtrust:country*citizen + govtrust:education*citizen,
                    data = prolific_data, respondent.id = "id", respondent.varying = "govtrust", baseline = baselines, 
                    cluster = FALSE, na.ignore = TRUE, design = vf)
electrust_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + electiontrust:gender + 
                       electiontrust:country + electiontrust:education + electiontrust:englishprof + 
                       electiontrust:conviction + electiontrust:citizen + electiontrust:citizen*conviction + 
                       electiontrust:country*conviction + electiontrust:country*citizen + electiontrust:education*citizen,
                     data = prolific_data, respondent.id = "id", respondent.varying = "electiontrust", baseline = baselines, 
                     cluster = FALSE, na.ignore = TRUE, design = vf)
votesconf_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + votesconf:gender + 
                       votesconf:country + votesconf:education + votesconf:englishprof + 
                       votesconf:conviction + votesconf:citizen + votesconf:citizen*conviction + 
                       votesconf:country*conviction + votesconf:country*citizen + votesconf:education*citizen,
                     data = prolific_data, respondent.id = "id", respondent.varying = "votesconf", baseline = baselines, 
                     cluster = FALSE, na.ignore = TRUE, design = vf)
pid_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                 pid7:gender + pid7:country + pid7:education + pid7:englishprof + pid7:conviction + pid7:citizen + 
                 pid7:citizen*conviction + pid7:country*conviction + pid7:country*citizen + pid7:education*citizen,
               data = prolific_data, respondent.id = "id", respondent.varying = "pid7", baseline = baselines, 
               cluster = FALSE, na.ignore = TRUE, design = vf)
race_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                  race2:gender + race2:country + race2:education + race2:englishprof + race2:conviction + race2:citizen + 
                  race2:citizen*conviction + race2:country*conviction + race2:country*citizen + race2:education*citizen,
                data = prolific_data, respondent.id = "id", respondent.varying = "race2", baseline = baselines, 
                cluster = FALSE, na.ignore = TRUE, design = vf)
educ_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                  educ:gender + educ:country + educ:education + educ:englishprof + educ:conviction + educ:citizen + 
                  educ:citizen*conviction + educ:country*conviction + educ:country*citizen + educ:education*citizen,
                data = prolific_data, respondent.id = "id", respondent.varying = "educ", baseline = baselines, 
                cluster = FALSE, na.ignore = TRUE, design = vf)
gender_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                    respondent_gender:gender + respondent_gender:country + respondent_gender:education + respondent_gender:englishprof + 
                    respondent_gender:conviction + respondent_gender:citizen + respondent_gender:citizen*conviction + 
                    respondent_gender:country*conviction + respondent_gender:country*citizen + respondent_gender:education*citizen,
                  data = prolific_data, respondent.id = "id", respondent.varying = "respondent_gender", baseline = baselines, 
                  cluster = FALSE, na.ignore = TRUE, design = vf)
vf_wc_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                   vf_white_collar:gender + vf_white_collar:country + vf_white_collar:education + vf_white_collar:englishprof + vf_white_collar:conviction + vf_white_collar:citizen + 
                   vf_white_collar:citizen*conviction + 
                   vf_white_collar:country*conviction + vf_white_collar:country*citizen + vf_white_collar:education*citizen,
                 data = prolific_data, respondent.id = "id", respondent.varying = "vf_white_collar", baseline = baselines, 
                 cluster = FALSE, na.ignore = TRUE, design = vf)
vf_elect2020_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                          vf_elect2020:gender + vf_elect2020:country + vf_elect2020:education + vf_elect2020:englishprof + vf_elect2020:conviction + vf_elect2020:citizen + 
                          vf_elect2020:citizen*conviction + 
                          vf_elect2020:country*conviction + vf_elect2020:country*citizen + vf_elect2020:education*citizen,
                        data = prolific_data, respondent.id = "id", respondent.varying = "vf_elect2020", baseline = baselines, 
                        cluster = FALSE, na.ignore = TRUE, design = vf)
vf_problem <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                     vf_problem:gender + vf_problem:country + vf_problem:education + vf_problem:englishprof + vf_problem:conviction + vf_problem:citizen + 
                     vf_problem:citizen*conviction + vf_problem:country*conviction + vf_problem:country*citizen + vf_problem:education*citizen,
                   data = prolific_data, respondent.id = "id", respondent.varying = "vf_problem", baseline = baselines, 
                   cluster = FALSE, na.ignore = TRUE, design = vf)
vf_ethno_hisp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                       ethnocentrism3:gender + ethnocentrism3:country + ethnocentrism3:education + ethnocentrism3:englishprof + ethnocentrism3:conviction + ethnocentrism3:citizen + 
                       ethnocentrism3:citizen*conviction + ethnocentrism3:country*conviction + ethnocentrism3:country*citizen + ethnocentrism3:education*citizen,
                     data = prolific_data, respondent.id = "id", respondent.varying = "ethnocentrism3", baseline = baselines, 
                     cluster = FALSE, na.ignore = TRUE, design = vf)
vf_ethno_imm <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                     ethnocentrism5:gender + ethnocentrism5:country + ethnocentrism5:education + ethnocentrism5:englishprof + ethnocentrism5:conviction + ethnocentrism5:citizen + 
                     ethnocentrism5:citizen*conviction + ethnocentrism5:country*conviction + ethnocentrism5:country*citizen + ethnocentrism5:education*citizen,
                   data = prolific_data, respondent.id = "id", respondent.varying = "ethnocentrism5", baseline = baselines, 
                   cluster = FALSE, na.ignore = TRUE, design = vf)


## Plots
plot(immatt_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Immigration Attitudes", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(monitor_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Self-Monitoring", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(govtrust_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Trust in Government", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(electrust_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Trust in Elections", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(votesconf_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Confidence Votes are Counted Correctly", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(pid_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Party ID", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(race_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Race", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(educ_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Education", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(gender_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Gender", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(vf_wc_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Belief in VF as a White-Collar Crime", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(vf_elect2020_sp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on VF in 2020 Election", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(vf_problem, text.size = 8, main = "AMCE and ACIE Estimates Conditional on VF as a Problem", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(vf_ethno_hisp, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Ethnocentrism (Latinx)", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))
plot(vf_ethno_imm, text.size = 8, main = "AMCE and ACIE Estimates Conditional on Ethnocentrism (Immigrants)", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Country:Conviction", "Country:Citizen", "Education:Citizen"))

