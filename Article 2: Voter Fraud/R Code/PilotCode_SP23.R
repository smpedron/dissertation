## R Code for Voter Fraud Paper (Experimental Design Seminar Version)
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

########################   CODING NOTES    ################################

# Does not include any code related to data collected in AU22 when I fielded a different version of the experiment (used only 5 attributes)
# The Javascript and HTML code used on Qualtrics is included in the Reduced Experiment section for easy reference.
# Cronbach's Alpha moved to Miscellaneous section. Did not remove any variables from scales made in data cleaning.

########################   SETUP   ############################
## Cleaning environment
rm(list = ls())

## Setting work directory
getwd()
setwd("C:/Users/steph/Documents/Courses/PhD/Papers/VF Stuff/R")
getwd()

## Setting seed
RNGkind("L'Ecuyer-CMRG")
.Random.seed
set.seed(1055385644)

## Loading packages: some that mask functions I typically use are further down to avoid breaking code
library(foreign)
library(dplyr)
library(magrittr)
library(tidyverse)
library(cjoint)

## Loading data
sample1 <- read.csv("C:/Users/steph/Documents/Courses/PhD/Papers/VF Stuff/R/Student Sample.csv") # student sample

## Setting aside the Spring 23 sample that I used for the seminar paper
SP23_students <- sample1[c(176:297),]

########################   REDUCED EXPERIMENT // QUALTRICS CODE  ##############

## Covariates (AKA Attributes and Levels)
gender <- as.factor(c("Male", "Female"))
education <- as.factor(c("no high school", "high school diploma", "college degree"))
english_proficiency <- as.factor( c("broken English", "fluent English"))
past_conviction <- as.factor(c("prior conviction", "no prior conviction"))
country_origin <- as.factor(c("Mexico", "India", "China", "Germany", "Afghanistan", "Russia"))
citizen <- as.factor(c("naturalized", "not natrualized"))

## Generating all possible profiles
experiment <- expand.grid(gender = gender, education = education, english_proficiency = english_proficiency, # 288 total combinations
                          past_conviction = past_conviction, country_origin = country_origin, citizen = citizen) 


## Javascript Code for Qualtrics
# /* Code Reference: https://m-graham.com/resources/conjoint%20how-to.pdf */
#   
#   /* Randomization Code 
# Notes: The randomization script was placed within the gender question on Qualtrics */
#   
#   /* Ten choice randomization of immigrant profiles */
#   
#   Qualtrics.SurveyEngine.addOnload(function(){
#     
#     /* number of choices */
#       var numChoice = 10;
#       
#       /* attributes and levels */
#         var genderArray = ["Male", "Female"];
#         var educationArray = ["no high school", "high school diploma", "college degree"];
#         var countryArray = ["Mexico", "India", "China", "Germany", "Afghanistan", "Russia"];
#         var englishprofArray = ["broken English", "fluent English"];
#         var convictionArray = ["prior conviction (not voting related)", "no prior conviction"];
#         var citizenArray = ["naturalized", "not naturalized"];
#         
#         /* shuffle */
#           function shuffle(array){
#             for (var i = array.length - 1; i > 0; i--){
#               var j = Math.floor(Math.random() * (i + 1));
#               var temp = array[i];
#               array[i] = array[j];
#               array[j] = temp;
#             }
#             return array;
#           }
#         
#         /* shuffle vector and choose first entry */
#           function shuffle_one(theArray){
#             var out = shuffle(theArray);
#             var out = out[0];
#             return(out)
#           };
#         
#         /* perform randomization and save result */
#           for (i = 1; i <= numChoice; i++){
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_gender1", shuffle_one(genderArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_gender2", shuffle_one(genderArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_education1", shuffle_one(educationArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_education2", shuffle_one(educationArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_education3", shuffle_one(educationArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country1", shuffle_one(countryArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country2", shuffle_one(countryArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country3", shuffle_one(countryArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country4", shuffle_one(countryArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country5", shuffle_one(countryArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_country6", shuffle_one(countryArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_englishprof1", shuffle_one(englishprofArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_englishprof2", shuffle_one(englishprofArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_conviction1", shuffle_one(convictionArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_conviction2", shuffle_one(convictionArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_citizen1", shuffle_one(citizenArray));
#             Qualtrics.SurveyEngine.setEmbeddedData("choice"+i+"_citizen2", shuffle_one(citizenArray));
#           }
#   });


## HTML Code for Qualtrics
# Code Reference: https://m-graham.com/resources/conjoint%20how-to.pdf
# 
# Notes: Refer to Excel file for loop and merge grid used for conjoint blocks.
#        Make sure the right Fields align with the variables in the loop & merge grid.
# 
# 
# Conjoint table presentations of immigrant profiles
# 
# <center>
# <table class = "wholetable" style="min-width:60%">
# <tr class = "toprow">
# <td></td>
# <td class="attr"><b>Immigrant 1 </b> </td>
# <td class="attr"><b>Immigrant 2 </b> </td>
# </tr>
# <tr class="normalrow">
# <td class="name">Gender</td>
# <td class="attr">${lm://Field/1}</td>
# <td class="attr">${lm://Field/2}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Education</td>
# <td class="attr">${lm://Field/3}</td>
# <td class="attr">${lm://Field/4}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Country of Origin</td>
# <td class="attr">${lm://Field/6}</td>
# <td class="attr">${lm://Field/7}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">English Proficiency</td>
# <td class="attr">${lm://Field/12}</td>
# <td class="attr">${lm://Field/13}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">Conviction Status</td>
# <td class="attr">${lm://Field/14}</td>
# <td class="attr">${lm://Field/15}</td>
# </tr>
# <tr class="normalrow">
# <td class="name">U.S. Citizenship</td>
# <td class="attr">${lm://Field/16}</td>
# <td class="attr">${lm://Field/17}</td>
# </tr>
# </table>
# </center>

########################   DATA CLEANING   ##########################

## Sample Two - SP23 Students
## Includes citizenship attribute

## Removing those that didn't complete the survey. No personal test runs. Anon links were sent to students with access issues.
SP23_students <- SP23_students %>% 
  filter(Progress == 100)

## Removing unnecessary variables (e.g. distribution info)
SP23_students <- SP23_students[, -c(1:9)]

## Making caseids
SP23_students$id <- 1:118

## Adjusting respondent gender column name for later
colnames(SP23_students) <- gsub("X", "", colnames(SP23_students))
SP23_students <- rename(SP23_students, respondent_gender = gender)

## Reshaping data
## Reshaping attributes so that every immigrant profile has its own row
sample_attributes <- SP23_students %>% 
  select(id, starts_with("choice"))

sample_attributes <- sample_attributes %>% 
  gather(variable, value, -id) %>% 
  mutate(
    Imm_Num =  gsub(".+(.$)" , "\\1 " , variable ),
    attribute = gsub(".+_|.$" , "" , variable)
  ) %>% 
  select(-variable) %>% 
  spread(attribute, value)

sample_attributes <- drop_na(sample_attributes)

## Reshaping repondents' immigrant preference so that every choice gets its own row
sample_students <- SP23_students %>% 
  select(id, contains("_select"))

sample_students <- sample_students %>% 
  gather(variable, preference, -id) %>% 
  mutate(
    choiceNum = gsub("[A-Za-z]|_.+" , "" , variable),
    preference = as.numeric(preference)
  ) %>% 
  select(-variable)

## Merging preferences and attributes
students_SP <- left_join(sample_attributes, sample_students)
students_SP$Imm_Num <- as.numeric(students_SP$Imm_Num)
students_SP$Chosen <- ifelse(students_SP$Imm_Num == students_SP$preference, 1, 0)

## Merging with the rest of the demographic data
students_SP23 <- left_join(students_SP, SP23_students, by = "id")

## Changing classes of variables
students_SP23[, 3:8] <- lapply(students_SP23[, 3:8], as.factor)
students_SP23[, 11:67] <- lapply(students_SP23[, 11:67], as.numeric)
sapply(students_SP23, class)

## Averages
students_SP23$monitor <- (students_SP23$monitor1 + students_SP23$monitor2 + students_SP23$monitor3) / 3
students_SP23$immattitude <- (students_SP23$imm1 + students_SP23$imm2 + students_SP23$imm3) / 3
students_SP23$govtrust <- (students_SP23$govtrust1 + students_SP23$govtrust2 + students_SP23$govtrust3 +
                             students_SP23$govtrust4) / 4
students_SP23$electiontrust <- (students_SP23$electiontrust1 + students_SP23$electiontrust2 + students_SP23$electiontrust3) / 3

## Get rid of unnecessary stuff
rm(sample_attributes, sample_students, SP23_students, students_SP)

## Demographic variable cleaning
## Respondent Gender
students_SP23$respondent_gender %<>%
  plyr::mapvalues(
    c("0", "1"),
    c("Female", "Male")
  ) %>%
  as.vector() %>% 
  as.factor()

## Age
students_SP23$age %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or older")
  ) %>%
  as.vector() %>% 
  as.factor()

## Race
students_SP23$race %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("White", "African American", "American Indian", "Asian", "Native Hawaiian", "Other")
  ) %>%
  as.vector() %>% 
  as.factor()

students_SP23$race2 <- students_SP23$race # easy plotting later

students_SP23$race2 %<>%
  plyr::mapvalues(
    c("White", "African American", "American Indian", "Asian", "Native Hawaiian", "Other"),
    c("White", "Non-White", "Non-White", "Non-White", "Non-White", "Non-White")
  ) %>%
  as.vector() %>% 
  as.factor()

## Ethnicity
students_SP23$ethnicity %<>%
  plyr::mapvalues(
    c("0", "1"),
    c("Not Hispanic", "Hispanic")
  ) %>%
  as.vector() %>% 
  as.factor()

## Education
students_SP23$educ %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("No high school diploma", "High school diploma", "Some college", "Undergraduate degree",
      "Undergraduate degree", "Graduate degree", "Graduate degree")
  ) %>%
  as.vector() %>% 
  as.factor()

## Marital Status
students_SP23$marstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("Not Married", "Married", "Not Married", "Not Married", "Not Married")
  ) %>%
  as.vector() %>% 
  as.factor()

## Employment Status
students_SP23$employstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8"),
    c("Employed", "Employed", "Student", "Not Employed", "Not Employed", "Employed", "Not Employed", "Not Employed")
  ) %>%
  as.vector() %>% 
  as.factor()

## Party ID
students_SP23$pid7 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("Democrat", "Democrat", "Democrat", "Independent", "Republican", "Republican", "Republican")
  ) %>%
  as.vector() %>% 
  as.factor()

## Ideology
students_SP23$ideo5 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("Liberal", "Liberal", "Moderate", "Conservative", "Conservative")
  ) %>%
  as.vector() %>% 
  as.factor()

## Religion
students_SP23$religion %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
    c("Protestant", "Roman Catholic", "Mormon", "Jewish", "Muslim", "Buddhist", "Hindu", "Atheist", 
      "Agnostic", "Other", "None")
  ) %>%
  as.vector() %>% 
  as.factor()

########################   AMCE/ACIE ESTIMATES   #########################

## Adjusting baselines
baselines <- list()
baselines$country <- "Mexico"
baselines$englishprof <- "fluent English"

## Spring 2023 Student Sample
## AMCE and ACIE
basic_2 <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen,
                data = students_SP23, respondent.id = "id", baselines = baselines)

## Hypothesis relevant ACIEs together
combo2 <- amce(Chosen ~ gender + country + education + englishprof + conviction + 
                 country*conviction + country*citizen, 
               data = students_SP23, respondent.id = "id", baselines = baselines)

baselines$conviction <- "prior conviction (not voting related)" # changing for ease of interpretation
ACIE <- amce(Chosen ~ gender + country + education + englishprof + conviction + conviction*citizen + gender*conviction,
             data = students_SP23, respondent.id = "id", baselines = baselines)
baselines$conviction <- "no prior conviction"

## Summaries
summary(basic_2)
summary(combo2)
summary(ACIE)

## Plot
plot(basic_2, main = "AMCE Estimates, Student Sample (SP23)", xlab = "Estimated AMCE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender"))

plot(combo2, main = "AMCE and ACIE Estimates, Student Sample (SP23)", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Country:Conviction", "Country:Citizen"))

plot(ACIE, main = "AMCE and ACIE Estimates, Student Sample (SP23)", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Citizen:Conviction", "Gender:Conviction"))


########################   SUB-GROUP EFFECTS   #########################

## Respondent-varying Interactions

## Spring 2023 Sample
## AMCE/ACIE Estimates
immatt_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                    immattitude:gender + immattitude:country + immattitude:education + immattitude:englishprof + 
                    immattitude:conviction + immattitude:citizen + immattitude:gender*conviction + 
                    immattitude:country*conviction + immattitude:country*citizen + immattitude:citizen*conviction,
                  data = students_SP23, respondent.id = "id", respondent.varying = "immattitude", baseline = baselines, 
                  cluster = FALSE, na.ignore = TRUE)
monitor_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                     monitor:gender + monitor:country + monitor:education + monitor:englishprof + monitor:conviction + monitor:citizen +
                     monitor:gender*conviction + monitor:country*conviction + monitor:country*citizen + monitor:citizen*conviction,
                   data = students_SP23, respondent.id = "id", respondent.varying = "monitor", baseline = baselines, 
                   cluster = FALSE, na.ignore = TRUE)
govtrust_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + govtrust:gender + 
                      govtrust:country + govtrust:education + govtrust:englishprof + govtrust:conviction + govtrust:citizen + 
                      govtrust:gender*conviction + govtrust:country*conviction + govtrust:country*citizen + govtrust:citizen*conviction,
                    data = students_SP23, respondent.id = "id", respondent.varying = "govtrust", baseline = baselines, 
                    cluster = FALSE, na.ignore = TRUE)
electrust_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + electiontrust:gender + 
                       electiontrust:country + electiontrust:education + electiontrust:englishprof + 
                       electiontrust:conviction + electiontrust:citizen + electiontrust:gender*conviction + 
                       electiontrust:country*conviction + electiontrust:country*citizen + electiontrust:citizen*conviction,
                     data = students_SP23, respondent.id = "id", respondent.varying = "electiontrust", baseline = baselines, 
                     cluster = FALSE, na.ignore = TRUE)
pid_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                 pid7:gender + pid7:country + pid7:education + pid7:englishprof + pid7:conviction + pid7:citizen + 
                 pid7:gender*conviction + pid7:country*conviction + pid7:country*citizen + pid7:citizen*conviction,
               data = students_SP23, respondent.id = "id", respondent.varying = "pid7", baseline = baselines, 
               cluster = FALSE, na.ignore = TRUE)
race_sp <- amce(Chosen ~ gender + country + education + englishprof + conviction + citizen + 
                  race2:gender + race2:country + race2:education + race2:englishprof + race2:conviction + race2:citizen + 
                  race2:gender*conviction + race2:country*conviction + race2:country*citizen + race2:citizen*conviction,
                data = students_SP23, respondent.id = "id", respondent.varying = "race2", baseline = baselines, 
                cluster = FALSE, na.ignore = TRUE)

## Plots
plot(immatt_sp, main = "AMCE and ACIE Estimates Conditional on Immigration Attitudes", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Gender:Conviction", "Country:Conviction", "Country:Citizen", "Citizen:Conviction"))
plot(monitor_sp, main = "AMCE and ACIE Estimates Conditional on Self-Monitoring", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Gender:Conviction", "Country:Conviction", "Country:Citizen", "Citizen:Conviction"))
plot(govtrust_sp, main = "AMCE and ACIE Estimates Conditional on Trust in Government", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Gender:Conviction", "Country:Conviction", "Country:Citizen", "Citizen:Conviction"))
plot(electrust_sp, main = "AMCE and ACIE Estimates Conditional on Trust in Elections", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Gender:Conviction", "Country:Conviction", "Country:Citizen", "Citizen:Conviction"))
plot(pid_sp, main = "AMCE and ACIE Estimates Conditional on Party ID", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Gender:Conviction", "Country:Conviction", "Country:Citizen", "Citizen:Conviction"))
plot(race_sp, main = "AMCE and ACIE Estimates Conditional on Race", xlab = "Estimated AMCE/ACIE", label.baseline = FALSE,
     attribute.names = c("US Citizenship", "Prior Conviction", "Country", "Education", "English Proficiency", "Gender",
                         "Gender:Conviction", "Country:Conviction", "Country:Citizen", "Citizen:Conviction"))

########################   MISCELLANEOUS  ########################

## Descriptive stats
summary(students_SP23)

## Cronbach's Alpha
library(psych)

## Immigration Attitudes
immattitude2 <- data.frame(students_SP23$imm1, students_SP23$imm2, students_SP23$imm3) %>% 
  drop_na()
alpha(immattitude2) 

## Self-Monitoring
monitor2 <- data.frame(students_SP23$monitor1, students_SP23$monitor2, students_SP23$monitor3) %>% 
  drop_na()
alpha(monitor2)

## Trust in Government
govtrust2 <- data.frame(students_SP23$govtrust1, students_SP23$govtrust2, students_SP23$govtrust3, students_SP23$govtrust4) %>% 
  drop_na()
alpha(govtrust2) 

## Trust in Elections
electiontrust2 <- data.frame(students_SP23$electiontrust1, students_SP23$electiontrust2, students_SP23$electiontrust3) %>% 
  drop_na()
alpha(electiontrust2)
