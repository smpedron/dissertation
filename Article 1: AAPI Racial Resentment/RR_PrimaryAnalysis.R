## Primary Analysis File, AAPI Racial Resentment Paper
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

rm(list = ls())

source("C:/Users/steph/Documents/Courses/PhD/Papers/RR Stuff/RR Codes/RR_datacleaner.r")

########################   ADDITIONAL CLEANING   ################################
## Can't include this in main data cleaning script because it ruins other scripts that rely on that
## Standardizing party ID, ideology, and family income
## I want them to go from 0 to 1
transform_data <- function(data) {
  minmax <- function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  # Apply minmax scaling to variables
  data$pid7 <- minmax(data$pid7)
  data$ideo5 <- minmax(data$ideo5)
  data$faminc <- minmax(data$faminc)
  
  # Making sure R treats Asian as the baseline
  data <- data %>% 
    mutate(race2 = case_when(race == "White" ~ 1,
                             race == "Asian" ~ 0))
  return(data)
}

CCES10_Asians <- transform_data(CCES10_Asians)
CCES12_Asians <- transform_data(CCES12_Asians)
CCES14_Asians <- transform_data(CCES14_Asians)
CCES16_Asians <- transform_data(CCES16_Asians)
CCES18_Asians <- transform_data(CCES18_Asians)
CCES20_Asians <- transform_data(CCES20_Asians)
CCES22_Asians <- transform_data(CCES22_Asians)

CCES10_Whites <- transform_data(CCES10_Whites)
CCES12_Whites <- transform_data(CCES12_Whites)
CCES14_Whites <- transform_data(CCES14_Whites)
CCES16_Whites <- transform_data(CCES16_Whites)
CCES18_Whites <- transform_data(CCES18_Whites)
CCES20_Whites <- transform_data(CCES20_Whites)
CCES22_Whites <- transform_data(CCES22_Whites)

## Combining data
## need to take out immgen cause it's ruining the combo in next subsection - only use it for appendix anyway
CCES10_Asians <- subset(CCES10_Asians, select = -immgen)
CCES12_Asians <- subset(CCES12_Asians, select = -immgen)
CCES14_Asians <- subset(CCES14_Asians, select = -immgen)
CCES16_Asians <- subset(CCES16_Asians, select = -immgen)
CCES18_Asians <- subset(CCES18_Asians, select = -immgen)
CCES20_Asians <- subset(CCES20_Asians, select = -immgen)
CCES22_Asians <- subset(CCES22_Asians, select = -immgen)

CCES10_Whites <- subset(CCES10_Whites, select = -immgen)
CCES12_Whites <- subset(CCES12_Whites, select = -immgen)
CCES14_Whites <- subset(CCES14_Whites, select = -immgen)
CCES16_Whites <- subset(CCES16_Whites, select = -immgen)
CCES18_Whites <- subset(CCES18_Whites, select = -immgen)
CCES20_Whites <- subset(CCES20_Whites, select = -immgen)
CCES22_Whites <- subset(CCES22_Whites, select = -immgen)

########################   REGRESSION (AND IMPUTATION)   ####################

## Prepping for imputation per year
CCES10_AW <- vctrs::vec_c(CCES10_Asians, CCES10_Whites)
CCES12_AW <- vctrs::vec_c(CCES12_Asians, CCES12_Whites)
CCES14_AW <- vctrs::vec_c(CCES14_Asians, CCES14_Whites)
CCES16_AW <- vctrs::vec_c(CCES16_Asians, CCES16_Whites)
CCES18_AW <- vctrs::vec_c(CCES18_Asians, CCES18_Whites)
CCES20_AW <- vctrs::vec_c(CCES20_Asians, CCES20_Whites)
CCES22_AW <- vctrs::vec_c(CCES22_Asians, CCES22_Whites)

## Checking where NAs for AAPI are cause regressions on regular data is dropping a lot
summary(CCES10_AW) # Immigration has way too many missing even for imputation. Trash it.
summary(CCES12_AW) 
summary(CCES14_AW)
summary(CCES16_AW) # Immigration again. Going in appendix anyway. Can just note this.
summary(CCES18_AW)
summary(CCES20_AW)
summary(CCES22_AW)

## Imputing
library(mice)
imputed_2010 <- mice(CCES10_AW, method = "pmm", print = F, seed = 11796)
imputed_2012 <- mice(CCES12_AW, method = "pmm", print = F, seed = 12495)
imputed_2014 <- mice(CCES14_AW, method = "pmm", print = F, seed = 32395)
imputed_2016 <- mice(CCES16_AW, method = "pmm", print = F, seed = 22289)
imputed_2018 <- mice(CCES18_AW, method = "pmm", print = F, seed = 40168)
imputed_2020 <- mice(CCES20_AW, method = "pmm", print = F, seed = 12598)
imputed_2022 <- mice(CCES22_AW, method = "pmm", print = F, seed = 34752)

## Distributions of original and imputed data
densityplot(imputed_2010, main = "Imputed Data Distribution (2010)")
densityplot(imputed_2012, main = "Imputed Data Distribution (2012)")
densityplot(imputed_2014, main = "Imputed Data Distribution (2014)") 
densityplot(imputed_2016, main = "Imputed Data Distribution (2016)") 
densityplot(imputed_2018, main = "Imputed Data Distribution (2018)")
densityplot(imputed_2020, main = "Imputed Data Distribution (2020)")
densityplot(imputed_2022, main = "Imputed Data Distribution (2022)")

## Regression
## Racial Resentment Predictors
rr_model10 <- with(imputed_2010, lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + 
                                      pew_bornagain + ideo5 + race))
rr_model12 <- with(imputed_2012, lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + 
                                      pew_bornagain + ideo5 + race))
rr_model14 <- with(imputed_2014, lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + 
                                      pew_bornagain + ideo5 + race))
rr_model16 <- with(imputed_2016, lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + 
                                      pew_bornagain + ideo5 + race))
rr_model18 <- with(imputed_2018, lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + 
                                      pew_bornagain + ideo5 + race))
rr_model20 <- with(imputed_2020, lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + 
                                      pew_bornagain + ideo5 + race))
rr_model22 <- with(imputed_2022, lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + 
                                      pew_bornagain + ideo5 + race))
summary(pool(rr_model10))
summary(pool(rr_model12))
summary(pool(rr_model14))
summary(pool(rr_model16)) # different measure for RR
summary(pool(rr_model18))
summary(pool(rr_model20))
summary(pool(rr_model22))


## Political Participation (Non-Voting)
pp_nonvote_model10 <- with(imputed_2010, lm(poli_participation ~ racialresentment + gender + educ + pid7 + faminc + 
                           immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                           race + race*racialresentment + race*immstat))
pp_nonvote_model12 <- with(imputed_2012, lm(poli_participation ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_nonvote_model14 <- with(imputed_2014, lm(poli_participation ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_nonvote_model16 <- with(imputed_2016, lm(poli_participation ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_nonvote_model18 <- with(imputed_2018, lm(poli_participation ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_nonvote_model20 <- with(imputed_2020, lm(poli_participation ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_nonvote_model22 <- with(imputed_2022, lm(poli_participation ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
summary(pool(pp_nonvote_model10))
summary(pool(pp_nonvote_model12))
summary(pool(pp_nonvote_model14))
summary(pool(pp_nonvote_model16)) # different measure for RR
summary(pool(pp_nonvote_model18))
summary(pool(pp_nonvote_model20))
summary(pool(pp_nonvote_model22))


## Political Participation (Voting)
pp_vote_model10 <- with(imputed_2010, lm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_vote_model12 <- with(imputed_2012, lm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_vote_model14 <- with(imputed_2014, lm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_vote_model16 <- with(imputed_2016, lm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_vote_model18 <- with(imputed_2018, lm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_vote_model20 <- with(imputed_2020, lm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
pp_vote_model22 <- with(imputed_2022, lm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                              immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                              race + race*racialresentment + race*immstat))
summary(pool(pp_vote_model10))
summary(pool(pp_vote_model12))
summary(pool(pp_vote_model14))
summary(pool(pp_vote_model16)) # different measure for RR
summary(pool(pp_vote_model18))
summary(pool(pp_vote_model20))
summary(pool(pp_vote_model22))


## Immigration (excluding 2010 due to too many NA)
immigration_model12 <- with(imputed_2012, lm(immigration ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
immigration_model14 <- with(imputed_2014, lm(immigration ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
immigration_model16 <- with(imputed_2016, lm(immigration ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat)) # note: many missing for 2016 too
immigration_model18 <- with(imputed_2018, lm(immigration ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
immigration_model20 <- with(imputed_2020, lm(immigration ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
immigration_model22 <- with(imputed_2022, lm(immigration ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
summary(pool(immigration_model12))
summary(pool(immigration_model14))
summary(pool(immigration_model16)) # different measure for RR
summary(pool(immigration_model18))
summary(pool(immigration_model20))
summary(pool(immigration_model22))


## Gun Control
gun_control_model10 <- with(imputed_2010, lm(gun_control ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
gun_control_model12 <- with(imputed_2012, lm(gun_control ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
gun_control_model14 <- with(imputed_2014, lm(gun_control ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
gun_control_model16 <- with(imputed_2016, lm(gun_control ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat)) 
gun_control_model18 <- with(imputed_2018, lm(gun_control ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
gun_control_model20 <- with(imputed_2020, lm(gun_control ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
gun_control_model22 <- with(imputed_2022, lm(gun_control ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
summary(pool(gun_control_model10))
summary(pool(gun_control_model12))
summary(pool(gun_control_model14))
summary(pool(gun_control_model16)) # different measure for RR
summary(pool(gun_control_model18))
summary(pool(gun_control_model20))
summary(pool(gun_control_model22))


## Abortion
abortion_model10 <- with(imputed_2010, lm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
abortion_model12 <- with(imputed_2012, lm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
abortion_model14 <- with(imputed_2014, lm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
abortion_model16 <- with(imputed_2016, lm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat)) 
abortion_model18 <- with(imputed_2018, lm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
abortion_model20 <- with(imputed_2020, lm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
abortion_model22 <- with(imputed_2022, lm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
summary(pool(abortion_model10))
summary(pool(abortion_model12))
summary(pool(abortion_model14))
summary(pool(abortion_model16)) # different measure for RR
summary(pool(abortion_model18))
summary(pool(abortion_model20))
summary(pool(abortion_model22))


## Affirmative Action (only 2010-2014)
aff_act_model10 <- with(imputed_2010, lm(aff_act ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat))
aff_act_model12 <- with(imputed_2012, lm(aff_act ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat))
aff_act_model14 <- with(imputed_2014, lm(aff_act ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat))
summary(pool(aff_act_model10))
summary(pool(aff_act_model12))
summary(pool(aff_act_model14))


## Affordable Care Act (no 2010)
aca_model12 <- with(imputed_2012, lm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
aca_model14 <- with(imputed_2014, lm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
aca_model16 <- with(imputed_2016, lm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat)) 
aca_model18 <- with(imputed_2018, lm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
aca_model20 <- with(imputed_2020, lm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
aca_model22 <- with(imputed_2022, lm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                               immstat + marstat + pew_bornagain + ideo5 + 
                                               race + race*racialresentment + race*immstat))
summary(pool(aca_model12))
summary(pool(aca_model14))
summary(pool(aca_model16)) # different measure for RR
summary(pool(aca_model18))
summary(pool(aca_model20))
summary(pool(aca_model22))

########################   PLOTS   ####################

library(jtools)
library(broom.mixed)

## Racial Resentment
pool1 <- pool(rr_model10)
pool2 <- pool(rr_model12)
pool3 <- pool(rr_model14)
pool4 <- pool(rr_model16)
pool5 <- pool(rr_model18)
pool6 <- pool(rr_model20)
pool7 <- pool(rr_model22)

RR_plot <- plot_summs(pool1, pool2, pool3, pool4, pool5, pool6, pool7,
                      coefs = c("White" = "raceWhite", "Foreign Born" = "immstat", "Republican" = "pid7", "Conservative" = "ideo5",
                                "Female" = "gender", "College Degree" = "educ", "Income" = "faminc", 
                                "Married" = "marstat", "Evangelical Christian" = "pew_bornagain"),
                      model.names = c("2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      robust = TRUE,
                      legend.title = "Year", point.shape = "circle", colors = "Rainbow")
RR_plot <- RR_plot + ggtitle("Racial Resentment") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

## Political Participation (Non-Voting)
pool1 <- pool(pp_nonvote_model10)
pool2 <- pool(pp_nonvote_model12)
pool3 <- pool(pp_nonvote_model14)
pool5 <- pool(pp_nonvote_model18)
pool6 <- pool(pp_nonvote_model20)
pool7 <- pool(pp_nonvote_model22)

pp_nonvote_plot <- plot_summs(pool1, pool2, pool3, pool5, pool6, pool7,
                              coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                        "White * Foreign Born" = "immstat:raceWhite"),
                              model.names = c("2010", "2012", "2014", "2018", "2020", "2022"),
                              robust = TRUE,
                              legend.title = "Year", point.shape = "circle", colors = "Rainbow")
pp_nonvote_plot <- pp_nonvote_plot + ggtitle("Political Participation (Non-Voting Activities)") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

## Political Participation (Voting)
## Robust SE cause LPM
pool1 <- pool(pp_vote_model10)
pool2 <- pool(pp_vote_model12)
pool3 <- pool(pp_vote_model14)
pool5 <- pool(pp_vote_model18)
pool6 <- pool(pp_vote_model20)
pool7 <- pool(pp_vote_model22)

pp_vote_plot <- plot_summs(pool1, pool2, pool3, pool5, pool6, pool7, robust = T,
                           coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                     "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                     "White * Foreign Born" = "immstat:raceWhite"),
                           model.names = c("2010", "2012", "2014", "2018", "2020", "2022"),
                           legend.title = "Year", point.shape = "circle", colors = "Rainbow")
pp_vote_plot <- pp_vote_plot + ggtitle("Political Participation (Voting)") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

## Immigration
pool2 <- pool(immigration_model12)
pool3 <- pool(immigration_model14)
pool5 <- pool(immigration_model18)
pool6 <- pool(immigration_model20)
pool7 <- pool(immigration_model22)

imm_plot <- plot_summs(pool2, pool3, pool5, pool6, pool7,
                           coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                     "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                     "White * Foreign Born" = "immstat:raceWhite"),
                           model.names = c("2012", "2014", "2018", "2020", "2022"),
                       robust = TRUE,
                           legend.title = "Year", point.shape = "circle", colors = "Rainbow")
imm_plot <- imm_plot + ggtitle("Restrictive Immigration") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

## Gun Control
pool1 <- pool(gun_control_model10)
pool2 <- pool(gun_control_model12)
pool3 <- pool(gun_control_model14)
pool5 <- pool(gun_control_model18)
pool6 <- pool(gun_control_model20)
pool7 <- pool(gun_control_model22)

gc_plot <- plot_summs(pool1, pool2, pool3, pool5, pool6, pool7,
                           coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                     "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                     "White * Foreign Born" = "immstat:raceWhite"),
                           model.names = c("2010", "2012", "2014", "2018", "2020", "2022"),
                      robust = TRUE,
                           legend.title = "Year", point.shape = "circle", colors = "Rainbow")
gc_plot <- gc_plot + ggtitle("Oppose Gun Control") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

## Abortion
pool1 <- pool(abortion_model10)
pool2 <- pool(abortion_model12)
pool3 <- pool(abortion_model14)
pool5 <- pool(abortion_model18)
pool6 <- pool(abortion_model20)
pool7 <- pool(abortion_model22)

abortion_plot <- plot_summs(pool1, pool2, pool3, pool5, pool6, pool7, robust = T,
                           coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                     "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                     "White * Foreign Born" = "immstat:raceWhite"),
                           model.names = c("2010", "2012", "2014", "2018", "2020", "2022"),
                           legend.title = "Year", point.shape = "circle", colors = "Rainbow")
abortion_plot <- abortion_plot + ggtitle("Prohibit Abortion") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

## Affirmative Action (only 2010-2014)
pool1 <- pool(aff_act_model10)
pool2 <- pool(aff_act_model12)
pool3 <- pool(aff_act_model14)

aff_plot <- plot_summs(pool1, pool2, pool3, robust = T,
                            coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                      "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                      "White * Foreign Born" = "immstat:raceWhite"),
                            model.names = c("2010", "2012", "2014"),
                       robust = TRUE,
                            legend.title = "Year", point.shape = "circle", colors = "Rainbow")
aff_plot <- aff_plot + ggtitle("Oppose Affirmative Action") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

## ACA
pool2 <- pool(aca_model12)
pool3 <- pool(aca_model14)
pool5 <- pool(aca_model18)
pool6 <- pool(aca_model20)
pool7 <- pool(aca_model22)

aca_plot <- plot_summs(pool2, pool3, pool5, pool6, pool7, robust = T,
                            coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                      "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                      "White * Foreign Born" = "immstat:raceWhite"),
                            model.names = c("2012", "2014", "2018", "2020", "2022"),
                       robust = TRUE,
                            legend.title = "Year", point.shape = "circle", colors = "Rainbow")
aca_plot <- aca_plot + ggtitle("Repeal ACA") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()


## Arranging plots
library(gridExtra)
grid.arrange(pp_vote_plot, pp_nonvote_plot, ncol = 1, nrow = 2)
grid.arrange(imm_plot, gc_plot, abortion_plot, aff_plot, aca_plot, ncol = 2, nrow = 3)

########################   APPENDIX   ############################

## For supplementary regressions that don't use imputed data, please refer to the "RR_SupplementaryReg_NoImpute.r" file
## In this section: GLMs and Plots, 2016 LMs

## GLM versions of imputed models with binary dependent variables
## Political Participation (Voting)
pp_vote_glm10 <- with(imputed_2010, glm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))
pp_vote_glm12 <- with(imputed_2012, glm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))
pp_vote_glm14 <- with(imputed_2014, glm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))
pp_vote_glm16 <- with(imputed_2016, glm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))
pp_vote_glm18 <- with(imputed_2018, glm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))
pp_vote_glm20 <- with(imputed_2020, glm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))
pp_vote_glm22 <- with(imputed_2022, glm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))

abortion_glm10 <- with(imputed_2010, glm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat, family = "binomial"))
abortion_glm12 <- with(imputed_2012, glm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat, family = "binomial"))
abortion_glm14 <- with(imputed_2014, glm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat, family = "binomial"))
abortion_glm16 <- with(imputed_2016, glm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat, family = "binomial"))
abortion_glm18 <- with(imputed_2018, glm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat, family = "binomial"))
abortion_glm20 <- with(imputed_2020, glm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat, family = "binomial"))
abortion_glm22 <- with(imputed_2022, glm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
                                            immstat + marstat + pew_bornagain + ideo5 + 
                                            race + race*racialresentment + race*immstat, family = "binomial"))

## Affirmative Action (only 2010-2014)
aff_act_glm10 <- with(imputed_2010, glm(aff_act ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))
aff_act_glm12 <- with(imputed_2012, glm(aff_act ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))
aff_act_glm14 <- with(imputed_2014, glm(aff_act ~ racialresentment + gender + educ + pid7 + faminc + 
                                           immstat + marstat + pew_bornagain + ideo5 + 
                                           race + race*racialresentment + race*immstat, family = "binomial"))


## Affordable Care Act (no 2010)
aca_glm12 <- with(imputed_2012, glm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                       immstat + marstat + pew_bornagain + ideo5 + 
                                       race + race*racialresentment + race*immstat, family = "binomial"))
aca_glm14 <- with(imputed_2014, glm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                       immstat + marstat + pew_bornagain + ideo5 + 
                                       race + race*racialresentment + race*immstat, family = "binomial"))
aca_glm16 <- with(imputed_2016, glm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                       immstat + marstat + pew_bornagain + ideo5 + 
                                       race + race*racialresentment + race*immstat, family = "binomial"))
aca_glm18 <- with(imputed_2018, glm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                       immstat + marstat + pew_bornagain + ideo5 + 
                                       race + race*racialresentment + race*immstat, family = "binomial"))
aca_glm20 <- with(imputed_2020, glm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                       immstat + marstat + pew_bornagain + ideo5 + 
                                       race + race*racialresentment + race*immstat, family = "binomial"))
aca_glm22 <- with(imputed_2022, glm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
                                       immstat + marstat + pew_bornagain + ideo5 + 
                                       race + race*racialresentment + race*immstat, family = "binomial"))

## Plots of GLMs
pool1 <- pool(pp_vote_glm10)
pool2 <- pool(pp_vote_glm12)
pool3 <- pool(pp_vote_glm14)
pool5 <- pool(pp_vote_glm18)
pool6 <- pool(pp_vote_glm20)
pool7 <- pool(pp_vote_glm22)

pp_vote_glmplot <- plot_summs(pool1, pool2, pool3, pool5, pool6, pool7, robust = T,
                           coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                     "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                     "White * Foreign Born" = "immstat:raceWhite"),
                           model.names = c("2010", "2012", "2014", "2018", "2020", "2022"),
                           legend.title = "Year", point.shape = "circle", colors = "Rainbow")
pp_vote_glmplot <- pp_vote_glmplot + ggtitle("Logit: Political Participation (Voting)") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

pool1 <- pool(abortion_glm10)
pool2 <- pool(abortion_glm12)
pool3 <- pool(abortion_glm14)
pool5 <- pool(abortion_glm18)
pool6 <- pool(abortion_glm20)
pool7 <- pool(abortion_glm22)

abortion_glmplot <- plot_summs(pool1, pool2, pool3, pool5, pool6, pool7, robust = T,
                            coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                      "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                      "White * Foreign Born" = "immstat:raceWhite"),
                            model.names = c("2010", "2012", "2014", "2018", "2020", "2022"),
                            legend.title = "Year", point.shape = "circle", colors = "Rainbow")
abortion_glmplot <- abortion_glmplot + ggtitle("Logit: Prohibit Abortion") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

pool1 <- pool(aff_act_glm10)
pool2 <- pool(aff_act_glm12)
pool3 <- pool(aff_act_glm14)

aff_glmplot <- plot_summs(pool1, pool2, pool3, robust = T,
                       coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                 "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                 "White * Foreign Born" = "immstat:raceWhite"),
                       model.names = c("2010", "2012", "2014"),
                       legend.title = "Year", point.shape = "circle", colors = "Rainbow")
aff_glmplot <- aff_glmplot + ggtitle("Logit: Oppose Affirmative Action") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

pool2 <- pool(aca_glm12)
pool3 <- pool(aca_glm14)
pool5 <- pool(aca_glm18)
pool6 <- pool(aca_glm20)
pool7 <- pool(aca_glm22)

aca_glmplot <- plot_summs(pool2, pool3, pool5, pool6, pool7, robust = T,
                       coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                 "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                 "White * Foreign Born" = "immstat:raceWhite"),
                       model.names = c("2012", "2014", "2018", "2020", "2022"),
                       legend.title = "Year", point.shape = "circle", colors = "Rainbow")
aca_glmplot <- aca_glmplot + ggtitle("Logit: Repeal ACA") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()


grid.arrange(abortion_glmplot, aca_glmplot, pp_vote_glmplot, aff_glmplot,
             ncol = 2, nrow = 2)


## 2016 Regression Results - Alternative measure for racial resentment
pool1 <- pool(pp_nonvote_model16)
pool2 <- pool(pp_vote_model16)
pool3 <- pool(immigration_model16)
pool4 <- pool(gun_control_model16)
pool5 <- pool(abortion_model16)
pool6 <- pool(aca_model16)

nonvote16_plot <- plot_summs(pool1, robust = T, coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                    "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                    "White * Foreign Born" = "immstat:raceWhite"), point.shape = "circle", colors = "Rainbow")
nonvote16_plot <- nonvote16_plot + ggtitle("Political Participation, Non-Voting Activities (2016)") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

vote16_plot <- plot_summs(pool2, robust = T, coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                        "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                        "White * Foreign Born" = "immstat:raceWhite"), point.shape = "circle", colors = "Rainbow")
vote16_plot <- vote16_plot + ggtitle("Political Participation, Voting (2016)") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

imm16_plot <- plot_summs(pool3, robust = T, coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                  "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                  "White * Foreign Born" = "immstat:raceWhite"), point.shape = "circle", colors = "Rainbow")
imm16_plot <- imm16_plot + ggtitle("Immigration (2016)") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

gc16_plot <- plot_summs(pool4, robust = T, coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                    "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                    "White * Foreign Born" = "immstat:raceWhite"), point.shape = "circle", colors = "Rainbow")
gc16_plot <- gc16_plot + ggtitle("Gun Control (2016)") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

abortion16_plot <- plot_summs(pool5, robust = T, coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                      "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                      "White * Foreign Born" = "immstat:raceWhite"), point.shape = "circle", colors = "Rainbow")
abortion16_plot <- abortion16_plot + ggtitle("Prohibit Abortion (2016)") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()


aca16_plot <- plot_summs(pool6, robust = T, coefs = c("Racial Resentment" = "racialresentment", "White" = "raceWhite", 
                                        "Foreign Born" = "immstat", "White*Racial Resentment" = "racialresentment:raceWhite", 
                                        "White * Foreign Born" = "immstat:raceWhite"), point.shape = "circle", colors = "Rainbow")
aca16_plot <- aca16_plot + ggtitle("Repeal ACA (2016)") + 
  labs(x = "Coefficient", y = "Variable") + 
  theme_minimal()

########################   PERSONAL NOTES #################################
## Cronbach's Alpha is before the variable consolidation phase. Located in DATA CLEANING
## Didn't use AAPI country of origin or 2010 and 2016 immigration DVs because way too many missing
## Simplified abortion variable toward the end of DATA CLEANING. Turned it binary.
## REMINDER how to check summaries per model: summary(lm_RR$mod_col[[2]])


