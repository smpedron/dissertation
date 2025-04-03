## Data Cleaning File, Voter Fraud Paper (Prolific Sample)
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"
## Cleaning environment
rm(list = ls())

## Setting seed
RNGkind("L'Ecuyer-CMRG")
.Random.seed
set.seed(1055385644)

## Loading packages
library(foreign)
library(dplyr)
library(magrittr)
library(tidyverse)
library(cjoint)

## Loading data
prolific <- read.csv("C:/Users/steph/Documents/Courses/PhD/Papers/VF Stuff/R/ProlificSample_Numeric.csv")


## DATA CLEANING


## Removing those that didn't complete the survey and those that took less than 4 minutes to compete
prolific$Duration..in.seconds. <- as.numeric(prolific$Duration..in.seconds.)
prolific <- prolific %>% 
  filter(Progress == 100 & Duration..in.seconds. > 240)

## Removing those that failed 2 out of 3 attention checks
## attention 1 correct is 11
## attention 2 correct is 1
## attention 3 correct is 2
prolific <- prolific %>% 
  filter(attention1 == 11 | attention2 == 1)
prolific <- prolific %>% 
  filter(attention1 == 11 | attention3 == 2)
prolific <- prolific %>% 
  filter(attention2 == 1 | attention3 == 2)

## Remove the unnecessary distribution info variables
prolific <- prolific[, -c(1:20)]

## Making case IDs
prolific$id <- 1:752

## copy of original
original_data <- prolific

## Adjusting respondent gender column name for later
colnames(prolific) <- gsub("X", "", colnames(prolific))
prolific <- rename(prolific, respondent_gender = gender)

## Reshaping data
## Reshaping attributes so that every immigrant profile has its own row
sample_attributes <- prolific %>% 
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

## Reshaping respondents' immigrant preference so that every choice gets its own row
sample_prolific <- prolific %>% 
  select(id, contains("_select"))

sample_prolific <- sample_prolific %>% 
  gather(variable, preference, -id) %>% 
  mutate(
    choiceNum = gsub("[A-Za-z]|_.+" , "" , variable),
    preference = as.numeric(preference)
  ) %>% 
  select(-variable)

## Merging preferences and attributes
prolific_merged <- left_join(sample_attributes, sample_prolific)
prolific_merged$Imm_Num <- as.numeric(prolific_merged$Imm_Num)
prolific_merged$Chosen <- ifelse(prolific_merged$Imm_Num == prolific_merged$preference, 1, 0)

## Merging with the rest of the demographic data
prolific_data <- left_join(prolific_merged, prolific, by = "id")

## Get rid of unnecessary stuff
rm(prolific, prolific_merged, sample_attributes, sample_prolific)

## Standardizing
prolific_data$englishprof <- gsub("fluent English", "Fluent English", prolific_data$englishprof, ignore.case = TRUE)

## Changing classes of variables
prolific_data[, 3:8] <- lapply(prolific_data[, 3:8], as.factor)
prolific_data[, 11:43] <- lapply(prolific_data[, 11:43], as.numeric)
prolific_data[, 46:62] <- lapply(prolific_data[, 46:62], as.numeric) # It's turning the 'Other' text response from vf_solutions into NA
prolific_data$'10_select1' <- as.numeric(prolific_data$'10_select1')
prolific_data$irr2 <- as.numeric(prolific_data$irr2)
sapply(prolific_data, class) # checking

prolific_data$monitor <- (prolific_data$monitor1 + prolific_data$monitor2 + prolific_data$monitor3) / 3
prolific_data$immattitude <- (prolific_data$imm1 + prolific_data$imm2 + prolific_data$imm3) / 3
prolific_data$govtrust <- (prolific_data$govtrust1 + prolific_data$govtrust2 + prolific_data$govtrust3) / 3
prolific_data$electiontrust <- (prolific_data$electiontrust1 + prolific_data$electiontrust2) / 2
prolific_data$votesconf <- (prolific_data$votesconf1 + prolific_data$votesconf2) / 2

## Demographic variable cleaning
## Respondent Gender
prolific_data$respondent_gender %<>%
  plyr::mapvalues(
    c("0", "1"),
    c("Female", "Male")
  ) %>%
  as.vector() %>% 
  as.factor()

## Age
prolific_data$age %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or older")
  ) %>%
  as.vector() %>% 
  as.factor()

## Race
prolific_data$race %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("White", "African American", "American Indian", "Asian", "Native Hawaiian", "Other")
  ) %>%
  as.vector() %>% 
  as.factor()

prolific_data$race2 <- prolific_data$race

prolific_data$race2 %<>%
  plyr::mapvalues(
    c("White", "African American", "American Indian", "Asian", "Native Hawaiian", "Other"),
    c("White", "Non-White", "Non-White", "Non-White", "Non-White", "Non-White")
  ) %>%
  as.vector() %>% 
  as.factor()

## Ethnicity
prolific_data$ethnicity %<>%
  plyr::mapvalues(
    c("0", "1"),
    c("Not Hispanic", "Hispanic")
  ) %>%
  as.vector() %>% 
  as.factor()

## Education
prolific_data$educ %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("No high school diploma", "High school diploma", "Some college", "Undergraduate degree",
      "Undergraduate degree", "Graduate degree", "Graduate degree")
  ) %>%
  as.vector() %>% 
  as.factor()

## Marital Status
prolific_data$marstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("Not Married", "Married", "Not Married", "Not Married", "Not Married")
  ) %>%
  as.vector() %>% 
  as.factor()

## Employment Status
prolific_data$employstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8"),
    c("Employed", "Employed", "Student", "Not Employed", "Not Employed", "Employed", "Not Employed", "Not Employed")
  ) %>%
  as.vector() %>% 
  as.factor()

## Party ID
prolific_data$pid7 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7"),
    c("Democrat", "Democrat", "Democrat", "Independent", "Republican", "Republican", "Republican")
  ) %>%
  as.vector() %>% 
  as.factor()

## Ideology
prolific_data$ideo5 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("Liberal", "Liberal", "Moderate", "Conservative", "Conservative")
  ) %>%
  as.vector() %>% 
  as.factor()

## Religion
prolific_data$religion %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
    c("Protestant", "Roman Catholic", "Mormon", "Jewish", "Muslim", "Buddhist", "Hindu", "Atheist", 
      "Agnostic", "Other", "None")
  ) %>%
  as.vector() %>% 
  as.factor()

## White Collar
prolific_data$vf_white_collar %<>%
  plyr::mapvalues(
    c("1", "0"),
    c("Yes", "No")
  ) %>%
  as.vector() %>% 
  as.factor()
