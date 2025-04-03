## APPENDIX: Descriptive Statistics + Cronbach's Alpha, Voter Fraud Paper (Prolific Sample)
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

rm(list = ls())
source("C:/Users/steph/Documents/Courses/PhD/Papers/VF Stuff/R/VF_datacleaner.r")


## Additional Cleaning
descriptive_stats <- prolific_data %>% 
  distinct(id, .keep_all = TRUE)

descriptive_stats$vf_concern %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4"),
    c("Only when there is concrete evidence of widespread voter fraud", "Only in closely contested elections", 
      "Always, even when there is no evidence", "It's not a significant concern")
  ) %>% 
  as.vector() %>% 
  as.factor()

descriptive_stats$vf_solutions %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "7", "8", "9"),
    c("Stricter voter ID requirements and verification", "Secure voting systems", 
      "Educating voters about the voting process", "Non-partisan commissions that oversee elections", 
      "Providing public access to election data", "Fine individuals guilty of committing voter fraud", 
      "Incarcerate individuals guilty of committing voter fraud", "Other")
  ) %>% 
  as.vector() %>% 
  as.factor()

descriptive_stats$vf_elect2020 %<>% 
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("Not at all", "A little", "A moderate amount", "Somewhat", "A lot")
  ) %>% 
  as.vector() %>% 
  as.factor()

descriptive_stats$vf_problem %<>% 
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("Not at all", "A little", "A moderate amount", "Somewhat", "A lot")
  ) %>% 
  as.vector() %>% 
  as.factor()

table(descriptive_stats$race3)

descriptive_stats$race3 <- descriptive_stats$race
descriptive_stats$race3 %<>%
  plyr::mapvalues(
    c("African American", "American Indian", "Asian", "Native Hawaiian", "Other", "White"),
    c("African American", "Other", "Asian", "Other", "Other", "White")
  ) %>% 
  as.vector() %>% 
  as.factor()

## Descriptive stats
summary(descriptive_stats)

table(descriptive_stats$age)
table(descriptive_stats$respondent_gender)
table(descriptive_stats$educ)
table(descriptive_stats$pid7)
table(descriptive_stats$ideo5)
table(descriptive_stats$religion)
table(descriptive_stats$race)
table(descriptive_stats$ethnicity)

## Voter Fraud Questions Distribution
## Do you believe that the 2020 US presidential election was influenced by voter fraud?
descriptive_stats$vf_elect2020 <- factor(descriptive_stats$vf_elect2020, 
                                         levels = c("Not at all", "A little", "A moderate amount", "Somewhat", "A lot"))
plot1 <- ggplot(descriptive_stats, aes(y = vf_elect2020)) + geom_bar(color = "white", fill ="darkslateblue") + 
  theme_light() + labs(title = "VF: 2020 Presidential Election Influenced")

## How much of a problem do you think voter fraud is when it comes to U.S. presidential elections?
descriptive_stats$vf_problem <- factor(descriptive_stats$vf_problem, 
                                       levels = c("Not at all", "A little", "A moderate amount", "Somewhat", "A lot"))
plot2 <- ggplot(descriptive_stats, aes(y = vf_problem)) + geom_bar(color = "white", fill = "darkslateblue") + 
  theme_light() + labs(title = "VF: Problem in Presidential Elections")

## In your opinion, when does voter fraud become a concern in elections?
plot3 <- ggplot(descriptive_stats, aes(y = vf_concern)) + geom_bar(color = "white", fill = "darkslateblue") + 
  theme_light() + labs(title = "VF: Election Concern")

## Which of the following measures do you think is most likely to address voter fraud?
descriptive_stats$vf_solutions <- factor(descriptive_stats$vf_solutions, 
                                         levels =  c("Stricter voter ID requirements and verification", "Secure voting systems", 
                                                     "Educating voters about the voting process", "Non-partisan commissions that oversee elections", 
                                                     "Providing public access to election data", "Fine individuals guilty of committing voter fraud", 
                                                     "Incarcerate individuals guilty of committing voter fraud", "Other"))
plot4 <- ggplot(descriptive_stats, aes(y = vf_solutions)) + geom_bar(color = "white", fill = "darkslateblue") + 
  theme_light() + labs(title = "VF: Solutions")

## Do you consider voter fraud to be a white-collar crime? 
## (White collar crime is defined as a nonviolent crime characterized by deceit or concealment in order to obtain 
## or avoid losing money or property, or to gain a personal or business advantage.)
plot5 <- ggplot(descriptive_stats, aes(y = vf_white_collar)) + geom_bar(color = "white", fill = "darkslateblue") + 
  theme_light() + labs(title = "VF: White Collar Crime")

plot1
plot2
plot3
plot4
plot5

## Contingency Tables / Cross-tabs
## Writing function for contingency tables
cont_table <- function(x, y){
  table(x, y) %>% 
    prop.table(2) %>% # 2 col sums; change to 1 for row sums
    multiply_by(100) %>% # percentages
    round()
}

## Education and Party ID
cont_table(descriptive_stats$educ, descriptive_stats$pid7)

## VF variables
## By party ID
cont_table(descriptive_stats$vf_elect2020, descriptive_stats$pid7)
cont_table(descriptive_stats$vf_problem, descriptive_stats$pid7)
cont_table(descriptive_stats$vf_concern, descriptive_stats$pid7)
cont_table(descriptive_stats$vf_solutions, descriptive_stats$pid7)
cont_table(descriptive_stats$vf_white_collar, descriptive_stats$pid7)

## By Ideology
cont_table(descriptive_stats$vf_elect2020, descriptive_stats$ideo5)
cont_table(descriptive_stats$vf_problem, descriptive_stats$ideo5)
cont_table(descriptive_stats$vf_concern, descriptive_stats$ideo5)
cont_table(descriptive_stats$vf_solutions, descriptive_stats$ideo5)
cont_table(descriptive_stats$vf_white_collar, descriptive_stats$ideo5)

## By Education
cont_table(descriptive_stats$vf_elect2020, descriptive_stats$educ)
cont_table(descriptive_stats$vf_problem, descriptive_stats$educ)
cont_table(descriptive_stats$vf_concern, descriptive_stats$educ)
cont_table(descriptive_stats$vf_solutions, descriptive_stats$educ)
cont_table(descriptive_stats$vf_white_collar, descriptive_stats$educ)

## By Gender
cont_table(descriptive_stats$vf_elect2020, descriptive_stats$respondent_gender)
cont_table(descriptive_stats$vf_problem, descriptive_stats$respondent_gender)
cont_table(descriptive_stats$vf_concern, descriptive_stats$respondent_gender)
cont_table(descriptive_stats$vf_solutions, descriptive_stats$respondent_gender)
cont_table(descriptive_stats$vf_white_collar, descriptive_stats$respondent_gender)

## By Religion
cont_table(descriptive_stats$vf_elect2020, descriptive_stats$religion)
cont_table(descriptive_stats$vf_problem, descriptive_stats$religion)
cont_table(descriptive_stats$vf_concern, descriptive_stats$religion)
cont_table(descriptive_stats$vf_solutions, descriptive_stats$religion)
cont_table(descriptive_stats$vf_white_collar, descriptive_stats$religion)

## By Age
cont_table(descriptive_stats$vf_elect2020, descriptive_stats$age)
cont_table(descriptive_stats$vf_problem, descriptive_stats$age)
cont_table(descriptive_stats$vf_concern, descriptive_stats$age)
cont_table(descriptive_stats$vf_solutions, descriptive_stats$age)
cont_table(descriptive_stats$vf_white_collar, descriptive_stats$age)

## By Age
cont_table(descriptive_stats$vf_elect2020, descriptive_stats$age)
cont_table(descriptive_stats$vf_problem, descriptive_stats$age)
cont_table(descriptive_stats$vf_concern, descriptive_stats$age)
cont_table(descriptive_stats$vf_solutions, descriptive_stats$age)
cont_table(descriptive_stats$vf_white_collar, descriptive_stats$age)

## By Race
cont_table(descriptive_stats$vf_elect2020, descriptive_stats$race3)
cont_table(descriptive_stats$vf_problem, descriptive_stats$race3)
cont_table(descriptive_stats$vf_concern, descriptive_stats$race3)
cont_table(descriptive_stats$vf_solutions, descriptive_stats$race3)
cont_table(descriptive_stats$vf_white_collar, descriptive_stats$race3)


## Cronbach's Alpha - I average these in the datacleaner script
library(psych)

## Immigration Attitudes
immattitude1 <- data.frame(prolific_data$imm1, prolific_data$imm2, prolific_data$imm3) %>% 
  drop_na()
alpha(immattitude1) # 0.79

## Self-Monitoring
monitor1 <- data.frame(prolific_data$monitor1, prolific_data$monitor2, prolific_data$monitor3) %>% 
  drop_na()
alpha(monitor1) # 0.74

## Trust in Government
govtrust1 <- data.frame(prolific_data$govtrust1, prolific_data$govtrust2, prolific_data$govtrust3) %>% 
  drop_na()
alpha(govtrust1) # 0.78

## Trust in Elections
electiontrust1 <- data.frame(prolific_data$electiontrust1, prolific_data$electiontrust2, prolific_data$electiontrust3) %>% 
  drop_na()
alpha(electiontrust1) # 0.79 if I drop electiontrust3, otherwise it's 0.55

## Confidence that votes are counted correctly
votesconf1 <- data.frame(prolific_data$votesconf1, prolific_data$votesconf2) %>% 
  drop_na()
alpha(votesconf1) # 0.81 - votesconf2 has pull: how confident are you votes cast by absentee ballot are counted as intended?

