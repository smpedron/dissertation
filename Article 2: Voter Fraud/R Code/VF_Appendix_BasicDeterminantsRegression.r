## Determinants: Belief that the 2020 Election Influenced by Voter Fraud
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

rm(list = ls())
source("C:/Users/steph/Documents/Courses/PhD/Papers/VF Stuff/R/VF_datacleaner.r")

## Cleaning
original_data$ethnicity <- as.numeric(original_data$ethnicity)
original_data$ethnicity <- as.factor(original_data$ethnicity)

original_data$pid7 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("Democrat", "Democrat", "Democrat", "Independent", "Republican","Republican","Republican")
  ) %>%
  as.vector()

original_data$ideo5 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("1Liberal", "1Liberal", "Moderate", "Conservative","Conservative")
  ) %>%
  as.vector()

original_data$educ %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("Less than HS", "HS Diploma", "HS Diploma", "College Degree", "College Degree","College Degree","College Degree")
  ) %>%
  as.vector()

original_data$race %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("1White", "Black", "Other", "Asian", "Other","Other")
  ) %>%
  as.vector()

original_data$age %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("18-34", "18-34", "35-54", "35-54", "55 or older","55 or older")
  ) %>%
  as.vector()

original_data$christian <- original_data$religion %>% 
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
    c("1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0")
  ) %>%
  as.vector() %>% 
  as.factor()

original_data$govtrust1 <- as.numeric(original_data$govtrust1)
original_data$govtrust2 <- as.numeric(original_data$govtrust2)
original_data$govtrust3 <- as.numeric(original_data$govtrust3)

original_data$electiontrust1 <- as.numeric(original_data$electiontrust1)
original_data$electiontrust2 <- as.numeric(original_data$electiontrust2)
original_data$electiontrust3 <- as.numeric(original_data$electiontrust3)

original_data$imm1 <- as.numeric(original_data$imm1)
original_data$imm2 <- as.numeric(original_data$imm2)
original_data$imm3 <- as.numeric(original_data$imm3)

original_data$votesconf1 <- as.numeric(original_data$votesconf1)
original_data$votesconf2 <- as.numeric(original_data$votesconf2)

original_data$govtrust <- (original_data$govtrust1 + original_data$govtrust2 + original_data$govtrust3) / 3
original_data$electiontrust <- (original_data$electiontrust1 + original_data$electiontrust2 + original_data$electiontrust3) / 3
original_data$immatt <- (original_data$imm1 + original_data$imm2 + original_data$imm3) / 3
original_data$votesconf <- (original_data$votesconf1 + original_data$votesconf2) / 2

minmax <- function(x){
  (x - min(x)) /(max(x)-min(x))
} 
original_data$govtrust <- minmax(original_data$govtrust)
original_data$electiontrust <- minmax(original_data$electiontrust)
original_data$immatt <- minmax(original_data$immatt)
original_data$votesconf <- minmax(original_data$votesconf)

cor(original_data$govtrust, original_data$electiontrust) # quick correlation check


## Regressions - Basic Determinants
## DV: Do you believe that the 2020 US presidential election was influenced by voter fraud?
## (1) Not at all, A little, A moderate amount, Somewhat, (5) A lot

## Ordered logit - odds of being in higher/lower categories based on demographics
library(MASS)
model1 <- polr(factor(vf_elect2020) ~ race + ethnicity + gender + pid7 + ideo5 + educ + christian + 
                 govtrust + electiontrust + votesconf + immatt, Hess = TRUE, method = "logistic", original_data)
summary(model1)
exp(model1$coefficients)
pr <- profile(model1)
plot(pr)
AIC(model1)

## Sample Exp Interpret Government Trust: If the predictor variable increases by one unit, the odds of the outcome falling 
## into a higher category increase by a factor of COEF compared to staying in a lower category.

## grabbing p-values
t_values <- summary(model1)$coefficients[, "t value"]
p_values <- 2 * pnorm(-abs(t_values))
p_values
# 0.05: Black, Other (Race), Republican, Christian, Electiontrust, Votesconf, immatt
# 0.1: Conservative, Moderate

## Appendix Table
library(stargazer)
stargazer(model1, type = "text", 
          title = "Ordered Logistic Regression Results", 
          dep.var.labels = "Belief in 2020 Election Fraud",
          covariate.labels = c("Race: Asian", "Race: Black", "Race: Other", "Hispanic", "Male", "Party ID: Independent", 
                               "Party ID: Republican", "Ideology: Conservative", "Ideology: Moderate",
                               "HS Diploma", "Less than HS", "Christian", "Government Trust", 
                               "Election Trust", "Vote Tallying Confidence", "Immigration Attitudes"),
          out = "table.txt")

## Checking different model - OLS with RSE
library(estimatr)
model2 <- lm_robust(vf_elect2020 ~ race + ethnicity + gender + pid7 + ideo5 + educ + christian + 
                      govtrust + electiontrust + votesconf + immatt, se_type = "HC3", original_data)
summary(model2)

## Grabbing significant for easy viewing (Below 0.05)
model_summary <- summary(model2)
p_values <- model_summary$coefficients[, "Pr(>|t|)"]
significant_p_values <- p_values[p_values < 0.05]
print(significant_p_values) # Republican, less than HS, Christian, Electiontrust, Votesconf, immatt

## Below 0.1
model_summary <- summary(model2)
p_values <- model_summary$coefficients[, "Pr(>|t|)"]
significant_p_values <- p_values[p_values < 0.1]
print(significant_p_values) # Above + Black
