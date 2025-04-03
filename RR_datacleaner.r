## Data Cleaner, AAPI Racial Resentment Paper
## Stephanie Pedron (pedron.2@osu.edu)

########################   SETUP   ###############################
## Cleaning environment
rm(list = ls())

## Setting work directory
getwd()
setwd("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data")
getwd()

## Setting seed
set.seed(1168936982)

## Loading packages
## Some packages added after so they don't break code
library(foreign)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(magrittr)
library(tidyverse)
library(gt)
library(corrplot)

## Loading CCES Data (2010-2022; two year intervals)
## It keeps loading things in as X, so I'm individually renaming the R datasets after loading them in
CCES10 <- read.dta("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data/cces2010.dta")

load("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data/cces2012.rdata")
CCES12 <- x 

load("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data/cces2014.rdata")
CCES14 <- x

load("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data/cces2016.rdata")
CCES16 <- x

load("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data/cces2018.rdata")
CCES18 <- x
rm(x) # removing the duplicate

CCES20 <- read.csv("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data/cces2020.csv")

CCES22 <- read.csv("C:/Users/steph/Documents/Courses/PhD/Papers/RR Data/cces2022.csv")

########################   DATA CLEANING ################################
## Selecting variables
## Some of these probably unnecessary as I progress. I'll remove later.
CCES10 = select(CCES10, V208, V211, V212d, V213, V243, V246, V263, CC422a, CC422b, CC320, CC324, CC327,
                CC322_1:CC322_5, CC417a_1:CC417a_4, CC401, V214, V215, CC316)

CCES12 = select(CCES12, gender, race, educ, pid7, ideo5, faminc, immstat, marstat, CC422a, CC422b, CC320, CC324, CC327,
                CC322_1:CC322_6, CC332I, CC417a_1:CC417a_4, CC401, pew_bornagain, CC316) 
# ^Start Repeal ACA variable

CCES14 = select(CCES14, gender, race, educ, pid7, ideo5, faminc, immstat, marstat, CC422a, CC422b, CC14_320a:CC14_323_4, CC14_328,
                CC14_324_2, -CC14_322_6, CC401, CC417a_1:CC417a_4, pew_bornagain, CC14_316) 
# ^They divide policy questions, so more variables

CCES16 = select(CCES16, gender, race, educ, pid7, ideo5, faminc, immstat, marstat, CC16_422d:CC16_422f, CC16_351I, 
                Asian_origin_2:Asian_origin_14, CC16_330a:CC16_332f, -CC16_331_5, -CC16_331_6, -CC16_331_8, -CC16_331_9,
                -CC16_332e, CC16_401, CC16_417a_1:CC16_417a_4, CC16_316, pew_bornagain) 
# ^Start Asian origin variables. No racial resentment battery. No affirmative action question starting 2016.

CCES18 = select(CCES18, gender, race, educ, pid7, ideo5, faminc_new, immstat, marstat, CC18_354b_2:CC18_354b_14, CC18_422e:CC18_422h,
                CC18_320a:CC18_322c_new, CC18_327c, -CC18_321e, CC18_401, CC18_417a_6, CC18_417a_1:CC18_417a_3, pew_bornagain, CC18_316)

CCES20 = select(CCES20, gender, race, educ, pid7, ideo5, faminc_new, immstat, marstat, CC20_441a, CC20_441b, 
                CC20_330a:CC20_332f, -CC20_331c, -CC20_331e, -CC20_332e, CC20_401, CC20_430a_1:CC20_430a_3, 
                CC20_430a_6, CC20_327d, CC20_364a, pew_bornagain)

CCES22 = select(CCES22, gender4, race, educ, pid7, ideo5, faminc_new, immstat, marstat, pew_bornagain, presvote20post,
                CC22_441a, CC22_441b, CC22_330a:CC22_330c, CC22_330a:CC22_330c, CC22_331a:CC22_331c, CC22_332f, CC22_401,
                CC22_430a_1, CC22_430a_2, CC22_430a_3, CC22_430a_6, CC22_327c)


## Making year variables
CCES10$year <- 2010
CCES12$year <- 2012
CCES14$year <- 2014
CCES16$year <- 2016
CCES18$year <- 2018
CCES20$year <- 2020
CCES22$year <- 2022

## Renaming variables
## library(dplyr) used

CCES10 <- dplyr::rename(CCES10,
                 gender = V208, race = V211, pid7 = V212d, educ = V213, ideo5 = V243, faminc = V246, immstat = V263, 
                 gun_control = CC320, abortion_always_illegal = CC324, aff_act = CC327, RR_Q1 = CC422a, RR_Q2 = CC422b,
                 imm_fine = CC322_1, imm_grant_legal = CC322_2, imm_guest_workers = CC322_3, imm_bp = CC322_4,
                 imm_police_ques = CC322_5, pp_polimeeting = CC417a_1, pp_polisign = CC417a_2, pp_work = CC417a_3, 
                 pp_donate = CC417a_4, pp_vote = CC401, marstat = V214, pew_bornagain = V215, lastelect_vote = CC316)

CCES12 <- dplyr::rename(CCES12, 
                 gun_control = CC320, abortion_always_illegal = CC324, aff_act = CC327, RR_Q1 = CC422a, RR_Q2 = CC422b, 
                 imm_fine = CC322_4, imm_grant_legal = CC322_1, imm_welfare = CC322_5, imm_bp = CC322_2, 
                 imm_police_ques = CC322_3, imm_deny_cit = CC322_6, repeal_ACA = CC332I, pp_polimeeting = CC417a_1, 
                 pp_polisign = CC417a_2, pp_work = CC417a_3, pp_donate = CC417a_4, pp_vote = CC401, lastelect_vote = CC316)

CCES14 <- dplyr::rename(CCES14,
                 RR_Q1 = CC422a, RR_Q2 = CC422b, gun_bgchecks = CC14_320a, gun_namepub = CC14_320b, gun_clips = CC14_320c,
                 gun_assault = CC14_320d, gun_permits = CC14_320e, imm_grant_legal = CC14_322_1, imm_bp = CC14_322_2,
                 imm_police_ques = CC14_322_3, imm_fine = CC14_322_4, imm_deport = CC14_322_5, repeal_ACA = CC14_324_2, 
                 abortion_always_illegal = CC14_323_1, abortion_rape = CC14_323_2, abortion_20wks = CC14_323_3, pp_vote = CC401,
                 abortion_employ = CC14_323_4, aff_act = CC14_328, pp_polimeeting = CC417a_1, pp_polisign = CC417a_2, 
                 pp_work = CC417a_3, pp_donate = CC417a_4, lastelect_vote = CC14_316)

CCES16 <- dplyr::rename(CCES16,
                 RR_Proxy_Q1 = CC16_422d, RR_Proxy_Q2 = CC16_422e, RR_Proxy_Q3 = CC16_422f, gun_bgchecks = CC16_330a, 
                 gun_namepub = CC16_330b, gun_assault = CC16_330d, gun_permits = CC16_330e, imm_grant_legal = CC16_331_1, 
                 imm_bp = CC16_331_2, imm_daca = CC16_331_3, imm_fine = CC16_331_4, imm_deport = CC16_331_7,
                 abortion_always = CC16_332a, abortion_rape = CC16_332b, abortion_20wks = CC16_332c, repeal_ACA = CC16_351I,
                 abortion_employ = CC16_332d, AO_US = Asian_origin_2, AO_China = Asian_origin_3, AO_Japan = Asian_origin_4, 
                 AO_India = Asian_origin_5, AO_PH = Asian_origin_6, AO_Taiwan = Asian_origin_7, AO_Korea = Asian_origin_8, 
                 AO_Viet = Asian_origin_9, AO_Pakistan = Asian_origin_10, AO_Hmong = Asian_origin_11, 
                 AO_Cambodia = Asian_origin_12, AO_Thai = Asian_origin_13, AO_Other = Asian_origin_14, pp_vote = CC16_401,
                 pp_polimeeting = CC16_417a_1, pp_polisign = CC16_417a_2, pp_work = CC16_417a_3, pp_donate = CC16_417a_4,
                 abortion_always_illegal = CC16_332f, lastelect_vote = CC16_316)

CCES18 <- dplyr::rename(CCES18,
                 faminc = faminc_new, RR_Q1 = CC18_422e, RR_Q2 = CC18_422f, RR_Q3 = CC18_422g, RR_Q4 = CC18_422h,
                 AO_US = CC18_354b_2, AO_China = CC18_354b_3, AO_Japan = CC18_354b_4, AO_India = CC18_354b_5, 
                 AO_PH = CC18_354b_6, AO_Taiwan = CC18_354b_7, AO_Korea = CC18_354b_8, AO_Viet = CC18_354b_9, 
                 AO_Pakistan = CC18_354b_10, AO_Hmong = CC18_354b_11, AO_Cambodia = CC18_354b_12, AO_Thai = CC18_354b_13, 
                 AO_Other = CC18_354b_14, gun_bgchecks = CC18_320a, gun_assault = CC18_320c, gun_permits = CC18_320d,
                 abortion_always = CC18_321a, abortion_rape = CC18_321b, abortion_20wks = CC18_321c, pp_vote = CC18_401,
                 abortion_employ = CC18_321d, imm_bp = CC18_322a, imm_daca = CC18_322b, imm_family_lottery = CC18_322c_new,
                 repeal_ACA = CC18_327c, pp_polimeeting = CC18_417a_1, pp_polisign = CC18_417a_2, pp_work = CC18_417a_3, 
                 pp_donate = CC18_417a_6, abortion_always_illegal = CC18_321f, lastelect_vote = CC18_316)

CCES20 <- dplyr::rename(CCES20,
                 faminc = faminc_new, RR_Q1 = CC20_441a, RR_Q2 = CC20_441b, gun_namepub = CC20_330a, gun_assault = CC20_330b,
                 gun_permits = CC20_330c, imm_legal = CC20_331a, imm_bp = CC20_331b, imm_lottery = CC20_331d,
                 abortion_always = CC20_332a, abortion_rape = CC20_332b, abortion_20wks = CC20_332c, pp_vote = CC20_401,
                 abortion_employ = CC20_332d, pp_polimeeting = CC20_430a_1, pp_polisign = CC20_430a_2, pp_work = CC20_430a_3, 
                 pp_donate = CC20_430a_6, repeal_ACA = CC20_327d, abortion_always_illegal = CC20_332f, lastelect_vote = CC20_364a)

CCES22 <- dplyr::rename(CCES22, 
                 gender = gender4, RR_Q1 = CC22_441a, RR_Q2 = CC22_441b, gun_namepub = CC22_330a, gun_assault = CC22_330b,
                 gun_permits = CC22_330c, imm_legal = CC22_331a, imm_bp = CC22_331b, imm_lottery = CC22_331c,
                 abortion_always_illegal = CC22_332f, pp_vote = CC22_401, pp_polimeeting = CC22_430a_1, 
                 pp_polisign = CC22_430a_2, pp_work = CC22_430a_3, pp_donate = CC22_430a_6, repeal_ACA = CC22_327c,
                 lastelect_vote = presvote20post, faminc = faminc_new)

## Recoding variables and turning unnecessary responses into NA
## Recoding demographic variables

## Race (removing variables)
## Only keeping Whites, Blacks, Latinx, and Asians. Samples are too small for others or I can't identify them (e.g. Other).
CCES10$race %<>%
  plyr::mapvalues(
    c("White", "Black", "Hispanic", "Asian", "Native American", "Mixed", "Other", "Middle Eastern", "Skipped", "Not Asked"),
    c("White", "Black", "Latinx", "Asian", NA, NA, NA, NA, NA, NA)
  ) %>%
  as.vector()

CCES12$race %<>%
  plyr::mapvalues(
    c("White", "Black", "Hispanic", "Asian", "Native American", "Mixed", "Other", "Middle Eastern", "Skipped", "Not Asked"),
    c("White", "Black", "Latinx", "Asian", NA, NA, NA, NA, NA, NA)
  ) %>%
  as.vector()

CCES14$race %<>%
  plyr::mapvalues(
    c("White", "Black", "Hispanic", "Asian", "Native American", "Mixed", "Other", "Middle Eastern", "Skipped", "Not Asked"),
    c("White", "Black", "Latinx", "Asian", NA, NA, NA, NA, NA, NA)
  ) %>%
  as.vector()

CCES16$race %<>%
  plyr::mapvalues(
    c("White", "Black", "Hispanic", "Asian", "Native American", "Mixed", "Other", "Middle Eastern", "Skipped", "Not Asked"),
    c("White", "Black", "Latinx", "Asian", NA, NA, NA, NA, NA, NA)
  ) %>%
  as.vector()

CCES18$race %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8"),
    c("White", "Black", "Latinx", "Asian", NA, NA, NA, NA)
  ) %>%
  as.vector()

CCES20$race %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8"),
    c("White", "Black", "Latinx", "Asian", NA, NA, NA, NA)
  ) %>%
  as.vector()

CCES22$race %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8"),
    c("White", "Black", "Latinx", "Asian", NA, NA, NA, NA)
  ) %>%
  as.vector()

## Gender (1 = Female)
CCES10$gender %<>%
  plyr::mapvalues(
    c("Male", "Female", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$gender %<>%
  plyr::mapvalues(
    c("Male", "Female", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$gender %<>%
  plyr::mapvalues(
    c("Male", "Female", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$gender %<>%
  plyr::mapvalues(
    c("Male", "Female", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$gender %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("0", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$gender %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("0", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$gender %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

## Party Identification (2 = Rep; 1 = Ind; 0 = Dem)
CCES10$pid7 %<>%
  plyr::mapvalues(
    c("Strong Democrat", "Not very strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not very strong
      Republican", "Strong Republican", "Not sure", "Skipped", "Not Asked"),
    c("0", "0", "0", "1", "2", "2", "2", NA, NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$pid7 %<>%
  plyr::mapvalues(
    c("Strong Democrat", "Not very strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not very strong
      Republican", "Strong Republican", "Not sure", "Skipped", "Not Asked"),
    c("0", "0", "0", "1", "2", "2", "2", NA, NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$pid7 %<>%
  plyr::mapvalues(
    c("Strong Democrat", "Not very strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not very strong
      Republican", "Strong Republican", "Not sure", "Skipped", "Not Asked"),
    c("0", "0", "0", "1", "2", "2", "2", NA, NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$pid7 %<>%
  plyr::mapvalues(
    c("Strong Democrat", "Not very strong Democrat", "Lean Democrat", "Independent", "Lean Republican", "Not very strong
      Republican", "Strong Republican", "Not sure", "Skipped", "Not Asked"),
    c("0", "0", "0", "1", "2", "2", "2", NA, NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$pid7 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8"),
    c("0", "0", "0", "1", "2", "2", "2", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$pid7 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8"),
    c("0", "0", "0", "1", "2", "2", "2", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$pid7 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8"),
    c("0", "0", "0", "1", "2", "2", "2", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

## Education (1 = 4-year College Degree; 0 = No 4-year College Degree)
CCES10$educ %<>%
  plyr::mapvalues(
    c("No HS", "High school graduate", "Some college", "2-year", "4-year", "Post-grad", "Skipped", "Not Asked"),
    c("0", "0", "0", "0", "1", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$educ %<>%
  plyr::mapvalues(
    c("No HS", "High school graduate", "Some college", "2-year", "4-year", "Post-grad", "Skipped", "Not Asked"),
    c("0", "0", "0", "0", "1", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$educ %<>%
  plyr::mapvalues(
    c("No HS", "High school graduate", "Some college", "2-year", "4-year", "Post-grad", "Skipped", "Not Asked"),
    c("0", "0", "0", "0", "1", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$educ %<>%
  plyr::mapvalues(
    c("No HS", "High school graduate", "Some college", "2-year", "4-year", "Post-grad", "Skipped", "Not Asked"),
    c("0", "0", "0", "0", "1", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$educ %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("0", "0", "0", "0", "1", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$educ %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("0", "0", "0", "0", "1", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$educ %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("0", "0", "0", "0", "1", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

## Ideology (2 = Conservative; 1 = Moderate; 0 = Liberal)
CCES10$ideo5 %<>%
  plyr::mapvalues(
    c("Very liberal", "Liberal", "Moderate", "Conservative", "Very Conservative", "Not Sure", "Skipped", "Not Asked"),
    c("0", "0", "1", "2", "2", NA, NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$ideo5 %<>%
  plyr::mapvalues(
    c("Very liberal", "Liberal", "Moderate", "Conservative", "Very Conservative", "Not Sure"),
    c("0", "0", "1", "2", "2", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$ideo5 %<>%
  plyr::mapvalues(
    c("Very liberal", "Liberal", "Moderate", "Conservative", "Very Conservative", "Not Sure"),
    c("0", "0", "1", "2", "2", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$ideo5 %<>%
  plyr::mapvalues(
    c("Very liberal", "Liberal", "Moderate", "Conservative", "Very Conservative", "Not Sure"),
    c("0", "0", "1", "2", "2", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$ideo5 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("0", "0", "1", "2", "2", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$ideo5 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("0", "0", "1", "2", "2", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$ideo5 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("0", "0", "1", "2", "2", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

## Immigration Status (1 = Foreign Born; 0 = US Born)
## Non-citizens are excluded. I'm only interested in Americans.
CCES10$immgen <- CCES10$immstat
CCES12$immgen <- CCES12$immstat
CCES14$immgen <- CCES14$immstat
CCES16$immgen <- CCES16$immstat
CCES18$immgen <- CCES18$immstat
CCES20$immgen <- CCES20$immstat
CCES22$immgen <- CCES22$immstat

CCES10$immstat %<>%
  plyr::mapvalues(
    c("Immigrant Citizen", "Immigrant non-citizen", "First generation", "Second generation", "Third generation", "Skipped",
      "Not Asked"),
    c("1", NA, "0", "0", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$immstat %<>%
  plyr::mapvalues(
    c("Immigrant Citizen", "Immigrant non-citizen", "First generation", "Second generation", "Third generation", "Skipped",
      "Not Asked"),
    c("1", NA, "0", "0", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$immstat %<>%
  plyr::mapvalues(
    c("Immigrant Citizen", "Immigrant non-citizen", "First generation", "Second generation", "Third generation", "Skipped",
      "Not Asked"),
    c("1", NA, "0", "0", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$immstat %<>%
  plyr::mapvalues(
    c("Immigrant Citizen", "Immigrant non-citizen", "First generation", "Second generation", "Third generation", "Skipped",
      "Not Asked"),
    c("1", NA, "0", "0", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$immstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("1", NA, "0", "0", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$immstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("1", NA, "0", "0", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$immstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("1", NA, "0", "0", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

## Altering immstat for immigrant generation
library(plyr)
CCES10$immgen <- CCES10$immgen %>% 
  mapvalues(
    c("Immigrant Citizen", "Immigrant non-citizen", "First generation", "Second generation", "Third generation", "Skipped", "Not Asked"),
    c(NA, NA, "First Generation", "Second Generation", "Third Generation", NA, NA) # first is imm citizen but no gen info
  )

CCES12$immgen <- CCES12$immgen %>% 
  mapvalues(
    c("Immigrant Citizen", "Immigrant non-citizen", "First generation", "Second generation", "Third generation", "Skipped", "Not Asked"),
    c(NA, NA, "First Generation", "Second Generation", "Third Generation", NA, NA)
  )

CCES14$immgen <- CCES14$immgen %>% 
  mapvalues(
    c("Immigrant Citizen", "Immigrant non-citizen", "First generation", "Second generation", "Third generation", "Skipped", "Not Asked"),
    c(NA, NA, "First Generation", "Second Generation", "Third Generation", NA, NA) 
  )

CCES16$immgen <- CCES16$immgen %>% 
  mapvalues(
    c("Immigrant Citizen", "Immigrant non-citizen", "First generation", "Second generation", "Third generation", "Skipped", "Not Asked"),
    c(NA, NA, "First Generation", "Second Generation", "Third Generation", NA, NA) 
  )

CCES18$immgen <- CCES18$immgen %>% 
  mapvalues(
    c("1", "2", "3", "4", "5"),
    c(NA, NA, "First Generation", "Second Generation", "Third Generation") 
  )

CCES20$immgen <- CCES20$immgen %>% 
  mapvalues(
    c("1", "2", "3", "4", "5"),
    c(NA, NA, "First Generation", "Second Generation", "Third Generation") 
  )

CCES22$immgen <- CCES22$immgen %>% 
  mapvalues(
    c("1", "2", "3", "4", "5"),
    c(NA, NA, "First Generation", "Second Generation", "Third Generation") 
  )


## Family Income (0-19k = 1; 20-39k = 2; 40-59k = 3; 60-79k = 4; 80-99k = 5; 100k+ = 6)
CCES10$faminc %<>%
  plyr::mapvalues(
    c("less than $10,000", "$10,000 - $14,999", "$15,000 - $19,999", "$20,000 - $24,999", "$25,000 - $29,999",
      "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999",
      "$80,000 - $99,999", "$100,000 - $119,999", "$120,000 - $149,999", "$150,000 or more", "Prefer not to say", 
      "Skipped", "Not Asked"),
    c("1", "1", "1", "2", "2", "2", "3", "3", "4", "4", "5", "6", "6", "6", NA, NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$faminc %<>%
  plyr::mapvalues(
    c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999",
      "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999", "$100,000 - $119,999",
      "$120,000 - $149,999", "$150,000 - $199,999", "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999",
      "$500,000 or more", "$150,000 or more", "$250,000 or more", "Prefer not to say", "Skipped", "Not Asked"),
    c("1", "1", "2", "2", "3", "3", "4", "4", "5", "6", "6", "6", "6", "6", "6", "6", "6", "6", NA, NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$faminc %<>%
  plyr::mapvalues(
    c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999",
      "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999", "$100,000 - $119,999",
      "$120,000 - $149,999", "$150,000 - $199,999", "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999",
      "$500,000 or more", "$150,000 or more", "$250,000 or more", "Prefer not to say", "Skipped", "Not Asked"),
    c("1", "1", "2", "2", "3", "3", "4", "4", "5", "6", "6", "6", "6", "6", "6", "6", "6", "6", NA, NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$faminc %<>%
  plyr::mapvalues(
    c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999",
      "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999", "$100,000 - $119,999",
      "$120,000 - $149,999", "$150,000 - $199,999", "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999",
      "$500,000 or more", "$150,000 or more", "Prefer not to say", "Skipped", "Not Asked"),
    c("1", "1", "2", "2", "3", "3", "4", "4", "5", "6", "6", "6", "6", "6", "6", "6", "6", NA, NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$faminc %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "97"),
    c("1", "1", "2", "2", "3", "3", "4", "4", "5", "6", "6", "6", "6", "6", "6", "6", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$faminc %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "97"),
    c("1", "1", "2", "2", "3", "3", "4", "4", "5", "6", "6", "6", "6", "6", "6", "6", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$faminc %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "97"),
    c("1", "1", "2", "2", "3", "3", "4", "4", "5", "6", "6", "6", "6", "6", "6", "6", NA)
  ) %>%
  as.vector() %>%
  as.numeric()

## Marital Status (Married and/or Domestic Partnership = 1; Single = 0)
CCES10$marstat %<>%
  plyr::mapvalues(
    c("Married", "Separated", "Divorced", "Widowed", "Single", "Domestic partnership", "Skipped", "Not Asked"),
    c("1", "0", "0", "0", "0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES12$marstat %<>%
  plyr::mapvalues(
    c("Married", "Separated", "Divorced", "Widowed", "Single", "Domestic partnership", "Skipped", "Not Asked"),
    c("1", "0", "0", "0", "0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$marstat %<>%
  plyr::mapvalues(
    c("Married", "Separated", "Divorced", "Widowed", "Single", "Domestic partnership", "Skipped", "Not Asked"),
    c("1", "0", "0", "0", "0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$marstat %<>%
  plyr::mapvalues(
    c("Married", "Separated", "Divorced", "Widowed", "Single", "Domestic partnership", "Skipped", "Not Asked"),
    c("1", "0", "0", "0", "0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$marstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("1", "0", "0", "0", "0", "1")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$marstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("1", "0", "0", "0", "0", "1")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$marstat %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("1", "0", "0", "0", "0", "1")
  )  %>%
  as.vector() %>%
  as.numeric()

## Born Again (Yes = 1; No = 0)
CCES10$pew_bornagain %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES12$pew_bornagain %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$pew_bornagain %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$pew_bornagain %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$pew_bornagain %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$pew_bornagain %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$pew_bornagain %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

## Voted last election (Yes = 1; No = 0)
## This is different from PP Vote
CCES10$lastelect_vote %<>%
  plyr::mapvalues(
    c("No", "I usually vote, but did not in 2008", "I am not sure", "Yes. I definitely voted.", "Skipped", "Not Asked"),
    c("0", "0", "0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES12$lastelect_vote %<>%
  plyr::mapvalues(
    c("No", "I usually vote, but did not in 2008", "I am not sure", "Yes. I definitely voted.", "Skipped", "Not Asked"),
    c("0", "0", "0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$lastelect_vote %<>%
  plyr::mapvalues(
    c("No", "I usually vote but did not in 2012", "I am not sure", "Yes, I definitely voted.", "Skipped", "Not Asked"),
    c("0", "0", "0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$lastelect_vote %<>%
  plyr::mapvalues(
    c("No", "I usually vote but did not in 2012", "I am not sure", "Yes, I definitely voted.", "Skipped", "Not Asked"),
    c("0", "0", "0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$lastelect_vote %<>%
  plyr::mapvalues(
    c("1", "2", "3"),
    c("1", "0", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$lastelect_vote %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("1", "1", "1", "0", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$lastelect_vote %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("0", "0", "0", "0", "0", "1")
  )  %>%
  as.vector() %>%
  as.numeric()

## Recoding Non-Demographic Variables
## 1 is always the more conservative answer for policies, partisanship, etc.
## 1 indicates YES or MORE political participation

## Racial Resentment Battery (2016 not included)
## Racially resentful option is always coded as 1

CCES10$RR_Q1 %<>%
  plyr::mapvalues(
    c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Skipped",
      "Not Asked"),
    c("1", "1", "0", "0", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES10$RR_Q2 %<>%
  plyr::mapvalues(
    c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Skipped",
      "Not Asked"),
    c("0", "0", "0", "1", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES12$RR_Q1 %<>%
  plyr::mapvalues(
    c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Skipped",
      "Not Asked"),
    c("1", "1", "0", "0", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES12$RR_Q2 %<>%
  plyr::mapvalues(
    c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Skipped",
      "Not Asked"),
    c("0", "0", "0", "1", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$RR_Q1 %<>%
  plyr::mapvalues(
    c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Skipped",
      "Not Asked"),
    c("1", "1", "0", "0", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$RR_Q2 %<>%
  plyr::mapvalues(
    c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Skipped",
      "Not Asked"),
    c("0", "0", "0", "1", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$RR_Q1 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("1", "1", "0", "0", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$RR_Q2 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("0", "0", "0", "1", "1")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$RR_Q3 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("0", "0", "0", "1", "1")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$RR_Q4 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("1", "1", "0", "0", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$RR_Q1 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("1", "1", "0", "0", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$RR_Q2 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("0", "0", "0", "1", "1")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$RR_Q1 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("1", "1", "0", "0", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$RR_Q2 %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("0", "0", "0", "1", "1")
  )  %>%
  as.vector() %>%
  as.numeric()


## Racial Resentment Proxy Measure (2016)
## Racially resentful option is always coded as 1
CCES16$RR_Proxy_Q1 %<>%
  plyr::mapvalues(
    c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Skipped",
      "Not Asked"),
    c("0", "0", "0", "1", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$RR_Proxy_Q2 %<>%
  plyr::mapvalues(
    c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Skipped",
      "Not Asked"),
    c("1", "1", "0", "0", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$RR_Proxy_Q3 %<>%
  plyr::mapvalues(
    c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree", "Skipped",
      "Not Asked"),
    c("1", "1", "0", "0", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

## Affirmative Action (only in CCES 2010-2014)
## Oppose = 1
CCES10$aff_act %<>%
  plyr::mapvalues(
    c("Strongly support", "Somewhat support", "Somewhat oppose", "Strongly oppose", "Skipped", "Not Asked"),
    c("0", "0", "1", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES12$aff_act %<>%
  plyr::mapvalues(
    c("Strongly support", "Somewhat support", "Somewhat oppose", "Strongly oppose", "Skipped", "Not Asked"),
    c("0", "0", "1", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$aff_act %<>%
  plyr::mapvalues(
    c("Strongly support", "Somewhat support", "Somewhat oppose", "Strongly oppose", "Skipped", "Not Asked"),
    c("0", "0", "1", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

## Gun Control
## Less strict gun laws/Increase gun rights = 1
CCES10$gun_control %<>%
  plyr::mapvalues(
    c("More Strict", "Less Strict", "Kept As They Are", "Skipped", "Not Asked"),
    c("0", "1", NA, NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES12$gun_control %<>%
  plyr::mapvalues(
    c("More Strict", "Less Strict", "Kept As They Are", "Skipped", "Not Asked"),
    c("0", "1", NA, NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$gun_bgchecks %<>%
  plyr::mapvalues(
    c("For", "Against", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$gun_namepub %<>%
  plyr::mapvalues(
    c("For", "Against", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$gun_clips %<>%
  plyr::mapvalues(
    c("For", "Against", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$gun_assault %<>%
  plyr::mapvalues(
    c("For", "Against", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$gun_permits %<>%
  plyr::mapvalues(
    c("For", "Against", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$gun_bgchecks %<>%
  plyr::mapvalues(
    c("Support", "Oppose", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$gun_namepub %<>%
  plyr::mapvalues(
    c("Support", "Oppose", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$gun_assault %<>%
  plyr::mapvalues(
    c("Support", "Oppose", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$gun_permits %<>%
  plyr::mapvalues(
    c("Support", "Oppose", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$gun_bgchecks %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("0","1")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$gun_assault %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("0","1")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$gun_permits %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1","0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$gun_namepub %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1","0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$gun_assault %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("0","1")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$gun_permits %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1","0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$gun_namepub %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1","0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$gun_assault %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("0","1")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$gun_permits %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1","0")
  )  %>%
  as.vector() %>%
  as.numeric()


## Repeal Affordable Care Act (Support = 1)
CCES12$repeal_ACA %<>%
  plyr::mapvalues(
    c("Support", "Oppose", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$repeal_ACA %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$repeal_ACA %<>%
  plyr::mapvalues(
    c("For", "Against", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$repeal_ACA %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$repeal_ACA %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$repeal_ACA %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

## Abortion (Illegal = 1; Legal = 0)
CCES10$abortion_always_illegal %<>%
  plyr::mapvalues(
    c("By law, abortion should never be permitted",
      "The law should permit abortion only in case of rape, incest or when the woman's life is in danger",
      "The law should permit abortion for reasons other than rape, incest, or danger to the woman's life, 
      but only after the need for the abortion has been clearly established",
      "By law, a woman should always be able to obtain an abortion as a matter of personal choice",
      "Skipped", "Not Asked"),
    c("1", NA, NA, "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES12$abortion_always_illegal %<>%
  plyr::mapvalues(
    c("By law, abortion should never be permitted",
      "The law should permit abortion only in case of rape, incest or when the woman's life is in danger",
      "The law should permit abortion for reasons other than rape, incest, or danger to the woman's life, 
      but only after the need for the abortion has been clearly established",
      "By law, a woman should always be able to obtain an abortion as a matter of personal choice",
      "Skipped", "Not Asked"),
    c("1", NA, NA, "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES14$abortion_always_illegal %<>%
  plyr::mapvalues(
    c("For", "Against", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES16$abortion_always_illegal %<>%
  plyr::mapvalues(
    c("Support", "Oppose", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  )  %>%
  as.vector() %>%
  as.numeric()

CCES18$abortion_always_illegal %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES20$abortion_always_illegal %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

CCES22$abortion_always_illegal %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  )  %>%
  as.vector() %>%
  as.numeric()

## Immigration (More restrictive = 1)
## 2010
CCES10$imm_fine %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES10$imm_grant_legal %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES10$imm_guest_workers %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES10$imm_bp %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES10$imm_police_ques %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

## 2012
CCES12$imm_fine %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$imm_grant_legal %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$imm_bp %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$imm_police_ques %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$imm_welfare %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$imm_deny_cit %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

## 2014
CCES14$imm_fine %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$imm_grant_legal %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$imm_bp %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$imm_police_ques %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$imm_deport %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

## 2016
CCES16$imm_fine %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$imm_grant_legal %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$imm_bp %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$imm_deport %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$imm_daca %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

## 2018
CCES18$imm_bp %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$imm_daca %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("0", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$imm_family_lottery %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

## 2020
CCES20$imm_legal %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("0", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$imm_bp %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$imm_lottery %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

## 2022
CCES22$imm_legal %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("0", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$imm_bp %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$imm_lottery %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

## Political Participation (More Participation = 1)
## Voting
CCES10$pp_vote %<>%
  plyr::mapvalues(
    c("I did not vote in the election this November.", "I thought about voting this time - but didn't.",
      "I usually vote, but didn't this time.", "I attempted to vote but did not or could not.",
      "I definitely voted in the General Election on November 2.", "Skipped", "Not Asked"),
    c("0", "0", "0", "0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$pp_vote %<>%
  plyr::mapvalues(
    c("I did not vote in the election this November.", "I thought about voting this time â\u0080\u0093 but didn't.",
      "I usually vote, but didn't this time.", "I attempted to vote but did not or could not.",
      "I definitely voted in the General Election on November 6.", "Skipped", "Not Asked"),
    c("0", "0", "0", "0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$pp_vote %<>%
  plyr::mapvalues(
    c("I did not vote in the election this November.", "I thought about voting this time â\u0080\u0093 but didn't.",
      "I usually vote, but didn't this time.", "I attempted to vote but did not or could not.",
      "I definitely voted in the Midterm Election on November 4th.", "Skipped", "Not Asked"),
    c("0", "0", "0", "0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$pp_vote %<>%
  plyr::mapvalues(
    c("I did not vote in the election this November.", "I thought about voting this time - but didn't",
      "I usually vote, but didn't this time.", "I attempted to vote but did not or could not.",
      "I definitely voted in the General Election.", "Skipped", "Not Asked"),
    c("0", "0", "0", "0", "1", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$pp_vote %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("0", "0", "0", "0", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$pp_vote %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("0", "0", "0", "0", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$pp_vote %<>%
  plyr::mapvalues(
    c("1", "2", "3", "4", "5"),
    c("0", "0", "0", "0", "1")
  ) %>%
  as.vector() %>%
  as.numeric()

## Non-Voting Political Participation Variables
CCES10$pp_polimeeting %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$pp_polimeeting %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$pp_polimeeting %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$pp_polimeeting %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$pp_polimeeting %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$pp_polimeeting %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$pp_polimeeting %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES10$pp_polisign %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$pp_polisign %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$pp_polisign %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$pp_polisign %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$pp_polisign %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$pp_polisign %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$pp_polisign %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES10$pp_work %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$pp_work %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$pp_work %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$pp_work %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$pp_work %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$pp_work %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$pp_work %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES10$pp_donate %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES12$pp_donate %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES14$pp_donate %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES16$pp_donate %<>%
  plyr::mapvalues(
    c("Yes", "No", "Skipped", "Not Asked"),
    c("1", "0", NA, NA)
  ) %>%
  as.vector() %>%
  as.numeric()

CCES18$pp_donate %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES20$pp_donate %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()

CCES22$pp_donate %<>%
  plyr::mapvalues(
    c("1", "2"),
    c("1", "0")
  ) %>%
  as.vector() %>%
  as.numeric()


## Getting rid of extra variables I realized I won't need
CCES14 <- subset(CCES14, select = -c(abortion_rape, abortion_20wks, abortion_employ))
CCES16 <- subset(CCES16, select = -c(abortion_rape, abortion_20wks, abortion_employ, abortion_always))
CCES18 <- subset(CCES18, select = -c(abortion_rape, abortion_20wks, abortion_employ, abortion_always))
CCES20 <- subset(CCES20, select = -c(abortion_rape, abortion_20wks, abortion_employ, abortion_always))

## Dropping observations with NA for race
## Not imputing because it's categorical
CCES10 <- CCES10 %>% 
  drop_na(race)
CCES12 <- CCES12 %>% 
  drop_na(race)
CCES14 <- CCES14 %>% 
  drop_na(race)
CCES16 <- CCES16 %>% 
  drop_na(race)
CCES18 <- CCES18 %>% 
  drop_na(race)
CCES20 <- CCES20 %>% 
  drop_na(race)
CCES22 <- CCES22 %>% 
  drop_na(race)

########################   CRONBACH'S ALPHA   ###############################

library(psych)

## Racial Resentment
RR10 <- data.frame(CCES10$RR_Q1, CCES10$RR_Q2) %>%
  drop_na() # Making data frames for it
RR12 <- data.frame(CCES12$RR_Q1, CCES12$RR_Q2)%>%
  drop_na()
RR14 <- data.frame(CCES14$RR_Q1, CCES14$RR_Q2)%>%
  drop_na()
RR16 <- data.frame(CCES16$RR_Proxy_Q1, CCES16$RR_Proxy_Q2, CCES16$RR_Proxy_Q3)%>%
  drop_na()
RR18 <- data.frame(CCES18$RR_Q1, CCES18$RR_Q2, CCES18$RR_Q3, CCES18$RR_Q4)%>%
  drop_na()
RR20 <- data.frame(CCES20$RR_Q1, CCES20$RR_Q2)%>%
  drop_na()
RR22 <- data.frame(CCES22$RR_Q1, CCES22$RR_Q2)%>%
  drop_na()

CA_RR10 <- alpha(RR10)
CA_RR10 # 0.69

CA_RR12 <- alpha(RR12)
CA_RR12 # 0.68

CA_RR14 <- alpha(RR14)
CA_RR14 # 0.68

CA_RR16 <- alpha(RR16) # 0.31
CA_RR16 # Question 2 is ruining reliability. Removing from data
CCES16 <- subset(CCES16, select = -c(RR_Proxy_Q2)) # Remove from CCES16 data
RR16 <- data.frame(CCES16$RR_Proxy_Q1, CCES16$RR_Proxy_Q3) %>% # Remove from Cronbach's Alpha data frame
  drop_na() 
CA_RR16 <- alpha(RR16) # Run again
CA_RR16 # 0.45 (still not great, but it's what I got)

CA_RR18 <- alpha(RR18)
CA_RR18 # 0.84

CA_RR20 <- alpha(RR20)
CA_RR20 # 0.78

CA_RR22 <- alpha(RR22)
CA_RR22 # 0.75

## Immigration
i1 <- subset(CCES10, select = c(imm_fine, imm_grant_legal, imm_bp,imm_police_ques, imm_guest_workers))
alpha(i1) # 0.64
i2 <- subset(CCES12, select = c(imm_grant_legal, imm_bp, imm_fine, imm_police_ques, imm_deny_cit, imm_welfare))
alpha(i2) # 0.8
i3 <- subset(CCES14, select = c(imm_grant_legal, imm_bp, imm_fine, imm_police_ques, imm_deport))
alpha(i3) # 0.77
i4 <- subset(CCES16, select = c(imm_bp, imm_daca, imm_fine, imm_deport, imm_grant_legal))
alpha(i4) # 0.71
i5 <- subset(CCES18, select = c(imm_bp, imm_daca, imm_family_lottery))
alpha(i5) # 0.71
i6 <- subset(CCES20, select = c(imm_legal, imm_bp, imm_lottery))
alpha(i6) # 0.66
i7 <- subset(CCES22, select = c(imm_legal, imm_bp, imm_lottery))
alpha(i7) # 0.59

## Gun Control (2010 and 2012 just have one binary question)
g1 <- subset(CCES14, select = c(gun_bgchecks, gun_namepub, gun_clips, gun_permits, gun_assault))
alpha(g1) # 0.73
g2 <- subset(CCES16, select = c(gun_bgchecks, gun_namepub, gun_permits, gun_assault))
alpha(g2) # 0.59
g3 <- subset(CCES18, select = c(gun_assault, gun_permits, gun_bgchecks))
alpha(g3) # 0.63
g4 <- subset(CCES20, select = c(gun_assault, gun_permits, gun_namepub))
alpha(g4) # 0.63
g5 <- subset(CCES22, select = c(gun_assault, gun_permits, gun_namepub))
alpha(g5) # 0.67

########################   VARIABLE CONSOLIDATION   ###############################

## Consolidating Racial Resentment variables
CCES10$racialresentment <- (CCES10$RR_Q1 + CCES10$RR_Q2) / 2
CCES12$racialresentment <- (CCES12$RR_Q1 + CCES12$RR_Q2) / 2
CCES14$racialresentment <- (CCES14$RR_Q1 + CCES14$RR_Q2) / 2
CCES16$racialresentment <- (CCES16$RR_Proxy_Q1 + CCES16$RR_Proxy_Q3) / 2
CCES18$racialresentment <- (CCES18$RR_Q1 + CCES18$RR_Q2 + CCES18$RR_Q3 + CCES18$RR_Q4) / 4
CCES20$racialresentment <- (CCES20$RR_Q1 + CCES20$RR_Q2) / 2
CCES22$racialresentment <- (CCES22$RR_Q1 + CCES22$RR_Q2) / 2

## Consolidating and deleting variables
CCES10$immigration <- (CCES10$imm_fine + CCES10$imm_grant_legal + CCES10$imm_guest_workers + CCES10$imm_bp +
                         CCES10$imm_police_ques) / 5 
CCES10$poli_participation <- (CCES10$pp_work + CCES10$pp_polimeeting + CCES10$pp_polisign + CCES10$pp_donate) / 4
CCES10 <- subset(CCES10, select = -c(imm_grant_legal, imm_guest_workers, imm_bp, imm_fine, imm_police_ques, 
                                     pp_polimeeting, pp_polisign, pp_donate, pp_work, RR_Q1, RR_Q2))

CCES12$immigration <- (CCES12$imm_grant_legal + CCES12$imm_bp + CCES12$imm_police_ques + CCES12$imm_fine +
                         CCES12$imm_welfare + CCES12$imm_deny_cit) / 6
CCES12$poli_participation <- (CCES12$pp_polimeeting + CCES12$pp_polisign + CCES12$pp_work + CCES12$pp_donate) / 4
CCES12 <- subset(CCES12, select = -c(imm_grant_legal, imm_deny_cit, imm_bp, imm_fine, imm_police_ques, imm_welfare,
                                     pp_polimeeting, pp_polisign, pp_donate, pp_work, RR_Q1, RR_Q2))

CCES14$gun_control <- (CCES14$gun_bgchecks + CCES14$gun_namepub + CCES14$gun_clips + CCES14$gun_permits + CCES14$gun_assault) / 5
CCES14$immigration <- (CCES14$imm_grant_legal + CCES14$imm_bp + CCES14$imm_fine + CCES14$imm_police_ques + CCES14$imm_deport) / 5
CCES14$poli_participation <- (CCES14$pp_polimeeting + CCES14$pp_polisign + CCES14$pp_work + CCES14$pp_donate) / 4
CCES14 <- subset(CCES14, select = -c(gun_bgchecks, gun_namepub, gun_clips, gun_permits, gun_assault, imm_grant_legal,
                                     imm_bp, imm_fine, imm_police_ques, imm_deport, pp_polimeeting, pp_polisign, pp_donate,
                                     pp_work, RR_Q1, RR_Q2))

CCES16$gun_control <- (CCES16$gun_assault + CCES16$gun_permits + CCES16$gun_namepub + CCES16$gun_bgchecks) / 4
CCES16$immigration <- (CCES16$imm_bp + CCES16$imm_daca + CCES16$imm_fine + CCES16$imm_deport + 
                         CCES16$imm_grant_legal) / 5
CCES16$poli_participation <- (CCES16$pp_polimeeting + CCES16$pp_polisign + CCES16$pp_work + CCES16$pp_donate) / 4
CCES16 <- subset(CCES16, select = -c(gun_bgchecks, gun_namepub, gun_permits, gun_assault, imm_bp, imm_fine, imm_daca, 
                                     imm_deport, imm_grant_legal, pp_polimeeting, pp_polisign, pp_donate, pp_work,
                                     RR_Proxy_Q1, RR_Proxy_Q3))
CCES16 <- subset(CCES16, select = -c(AO_US, AO_China, AO_India, AO_Japan, AO_Korea, AO_PH, AO_Taiwan, AO_Viet, AO_Hmong,
                                     AO_Pakistan, AO_Thai, AO_Other, AO_Cambodia))

CCES18$gun_control <- (CCES18$gun_assault + CCES18$gun_permits + CCES18$gun_bgchecks) / 3
CCES18$immigration <- (CCES18$imm_bp + CCES18$imm_daca + CCES18$imm_family_lottery) / 3
CCES18$poli_participation <- (CCES18$pp_polimeeting + CCES18$pp_polisign + CCES18$pp_work + CCES18$pp_donate) / 4
CCES18 <- subset(CCES18, select = -c(gun_assault, gun_permits, gun_bgchecks, imm_bp, imm_daca, imm_family_lottery,
                                     pp_polimeeting, pp_polisign, pp_donate, pp_work, RR_Q1, RR_Q2, RR_Q3, RR_Q4))
CCES18 <- subset(CCES18, select = -c(AO_US, AO_China, AO_India, AO_Japan, AO_Korea, AO_PH, AO_Taiwan, AO_Viet, AO_Hmong,
                                     AO_Pakistan, AO_Thai, AO_Other, AO_Cambodia))

CCES20$gun_control <- (CCES20$gun_permits + CCES20$gun_assault + CCES20$gun_namepub) / 3
CCES20$immigration <- (CCES20$imm_legal + CCES20$imm_bp + CCES20$imm_lottery) / 3
CCES20$poli_participation <- (CCES20$pp_work + CCES20$pp_polimeeting + CCES20$pp_polisign + CCES20$pp_donate) / 4
CCES20 <- subset(CCES20, select = -c(gun_namepub, gun_permits, gun_assault, imm_legal, imm_lottery, imm_bp,
                                     pp_polimeeting, pp_polisign, pp_donate, pp_work, RR_Q1, RR_Q2))

CCES22$gun_control <- (CCES22$gun_permits + CCES22$gun_assault + CCES22$gun_namepub) / 3
CCES22$immigration <- (CCES22$imm_legal + CCES22$imm_bp + CCES22$imm_lottery) / 3
CCES22 $poli_participation <- (CCES22$pp_work + CCES22$pp_polimeeting + CCES22$pp_polisign + CCES22$pp_donate) / 4
CCES22 <- subset(CCES22, select = -c(gun_namepub, gun_permits, gun_assault, imm_legal, imm_lottery, imm_bp,
                                     pp_polimeeting, pp_polisign, pp_donate, pp_work, RR_Q1, RR_Q2))


########################   SUBSETTING DATA   ###############################

## Subsetting data for other r scripts reliant on this file

CCES10_Whites <- CCES10 %>%
  filter(race == "White")
CCES12_Whites <- CCES12 %>%
  filter(race == "White")
CCES14_Whites <- CCES14 %>%
  filter(race == "White")
CCES16_Whites <- CCES16 %>%
  filter(race == "White")
CCES18_Whites <- CCES18 %>%
  filter(race == "White")
CCES20_Whites <- CCES20 %>%
  filter(race == "White")
CCES22_Whites <- CCES22 %>%
  filter(race == "White")

CCES10_Asians <- CCES10 %>%
  filter(race == "Asian")
CCES12_Asians <- CCES12 %>%
  filter(race == "Asian")
CCES14_Asians <- CCES14 %>%
  filter(race == "Asian")
CCES16_Asians <- CCES16 %>%
  filter(race == "Asian")
CCES18_Asians <- CCES18 %>%
  filter(race == "Asian")
CCES20_Asians <- CCES20 %>%
  filter(race == "Asian")
CCES22_Asians <- CCES22 %>%
  filter(race == "Asian")

## Putting together data for plots
A <- subset(CCES10, select = c(year, race, racialresentment, immstat, immgen, pid7, ideo5))
B <- subset(CCES12, select = c(year, race, racialresentment, immstat, immgen, pid7, ideo5))
C <- subset(CCES14, select = c(year, race, racialresentment, immstat, immgen, pid7, ideo5))
D <- subset(CCES18, select = c(year, race, racialresentment, immstat, immgen, pid7, ideo5))
E <- subset(CCES20, select = c(year, race, racialresentment, immstat, immgen, pid7, ideo5))
G <- subset(CCES22, select = c(year, race, racialresentment, immstat, immgen, pid7, ideo5))

combined_data <- rbind(A, B, C, D, E, G)
rm(A, B, C, D, E, G)

