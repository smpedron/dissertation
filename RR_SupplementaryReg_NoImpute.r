## Appendix: Supplementary Regressions (Original Data), AAPI Racial Resentment Paper
## Stephanie Pedron (pedron.2@osu.edu)

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

########################   REGRESSION (NO IMPUTATION)  ################################

no_imputation <- vctrs::vec_c(CCES10_Asians, CCES12_Asians, CCES14_Asians, CCES16_Asians, CCES18_Asians, CCES20_Asians, CCES22_Asians,
                              CCES10_Whites, CCES12_Whites, CCES14_Whites, CCES16_Whites, CCES18_Whites, CCES20_Whites, CCES22_Whites)
no_imputation_Asians <- vctrs::vec_c(CCES10_Asians, CCES12_Asians, CCES14_Asians, CCES16_Asians, CCES18_Asians, CCES20_Asians, CCES22_Asians)

## Racial Resentment Predictors for Asians
lm_RR <- no_imputation_Asians %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    mod_col = data %>%  # Racial Resentment
      map(
        \(i)
        lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + pew_bornagain + ideo5, data = i)))

## Regression models separated by year
lm_noimpute <- no_imputation %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    mod_col0 = data %>%  # Racial Resentment
      map(
        \(i)
        lm(racialresentment ~ gender + educ + pid7 + faminc + immstat + marstat + pew_bornagain + ideo5 + race, data = i)),
    mod_col1 = data %>%  # Political Participation (Non-voting)
      map(
        \(i)
        lm(poli_participation ~ racialresentment + gender + educ + pid7 + faminc + 
             immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + 
             race + race*racialresentment + race*immstat, data = i)),
    mod_col2 = data %>% # Immigration
      map(
        \(i)
        lm(immigration ~ racialresentment + gender + educ + pid7 + faminc + 
             immstat + marstat + pew_bornagain + ideo5 + 
             race + race*racialresentment + race*immstat, data = i)),
    mod_col3 = data %>%  # Gun Control
      map(
        \(i)
        lm(gun_control ~ racialresentment + gender + educ + pid7 + faminc + 
             immstat + marstat + pew_bornagain + ideo5 + 
             race + race*racialresentment + race*immstat, data = i)),
    mod_col4 = data %>%  # Voting
      map(
        \(i)
        lm(pp_vote ~ racialresentment + gender + educ + pid7 + faminc + 
             immstat + marstat + pew_bornagain + lastelect_vote + ideo5 + race + race*racialresentment
           + race*immstat, data = i)),
    mod_col5 = data %>%  # Abortion
      map(
        \(i)
        lm(abortion_always_illegal ~ racialresentment + gender + educ + pid7 + faminc + 
             immstat + marstat + pew_bornagain + ideo5 + race + race*racialresentment
           + race*immstat, data = i))
  )

## Affirmative Action (2010-2014)
lm_AA_noimpute <- no_imputation %>% 
  group_by(year) %>% 
  filter(year != "2016" & year != "2018" & year != "2020" & year != "2022") %>% 
  nest() %>% 
  mutate(
    mod_col = data %>%
      map(
        \(i)
        lm(aff_act ~ racialresentment + gender + educ + pid7 + faminc + 
             immstat + marstat + pew_bornagain + ideo5 + race + race*racialresentment + race*immstat, data = i))
  )

## Affordable Care Act
lm_ACA_noimpute <- no_imputation %>% 
  group_by(year) %>% 
  filter(year != "2010") %>% 
  nest() %>% 
  mutate(
    mod_col = data %>%
      map(
        \(i)
        lm(repeal_ACA ~ racialresentment + gender + educ + pid7 + faminc + 
             immstat + marstat + pew_bornagain + ideo5 + race + race*racialresentment + race*immstat, data = i))
  )


########################   PLOTS   ############################
library(sjPlot)
lm_RR$mod_col %>% 
  plot_models(grid = TRUE, p.shape = TRUE, p.threshold = 0.05, colors = "Black",
              title = "Racial Resentment Predictors (Asians)", 
              m.labels = c("2010", "2012", "2014", "2016", "2018", "2020", "2022"), show.legend = F, show.values = T,
              axis.labels = c("Conservative", "Evangelical Christian", "Married", "Foreign-Born", "Income",
                              "Republican", "College Degree", "Female"), axis.title = "Coefficients",
              vline.color = "grey") + theme_sjplot()

lm_noimpute$mod_col1 %>% 
  plot_models(grid = TRUE, p.shape = TRUE, p.threshold = 0.05, colors = "Black",
              title = "Political Participation (Non-Voting Activities)",
              m.labels = c("2010", "2012", "2014", "2016", "2018", "2020", "2022"), show.legend = F, show.values = T,
              axis.labels = c("Foreign-Born * White", "Racial Resentment * White", "White", "Conservative",
                              "Voted Last Election", "Evangelical Christian", "Married", "Income", "Republican",
                              "College Degree", "Female", "Foreign-Born", "Racial Resentment"),
              axis.title = "Coefficients", vline.color = "grey") + theme_sjplot()

lm_noimpute$mod_col2 %>% 
  plot_models(grid = TRUE, p.shape = TRUE, p.threshold = 0.05, colors = "Black",
              title = "Restrictive Immigration",
              m.labels = c("2010", "2012", "2014", "2016", "2018", "2020", "2022"), show.legend = F, show.values = T,
              axis.labels = c("Foreign-Born * White", "Racial Resentment * White", "White", "Conservative",
                              "Voted Last Election", "Evangelical Christian", "Married", "Income", "Republican",
                              "College Degree", "Female", "Foreign-Born", "Racial Resentment"),
              axis.title = "Coefficients", vline.color = "grey") + theme_sjplot()

lm_noimpute$mod_col3 %>% 
  plot_models(grid = TRUE, p.shape = TRUE, p.threshold = 0.05, colors = "Black",
              title = "Oppose Gun Control",
              m.labels = c("2010", "2012", "2014", "2016", "2018", "2020", "2022"), show.legend = F, show.values = T,
              axis.labels = c("Foreign-Born * White", "Racial Resentment * White", "White", "Conservative", 
                              "Evangelical Christian", "Married", "Income", "Republican",
                              "College Degree", "Female", "Foreign-Born", "Racial Resentment"),
              axis.title = "Coefficients", vline.color = "grey") + theme_sjplot()

lm_noimpute$mod_col4 %>% 
  plot_models(grid = TRUE, p.shape = TRUE, p.threshold = 0.05, colors = "Black",
              title = "Political Participation (Voting)",
              m.labels = c("2010", "2012", "2014", "2016", "2018", "2020", "2022"), show.legend = F, show.values = T,
              axis.labels = c("Foreign-Born * White", "Racial Resentment * White", "White", "Conservative",
                              "Voted Last Election", "Evangelical Christian", "Married", "Income", "Republican",
                              "College Degree", "Female", "Foreign-Born", "Racial Resentment"),
              axis.title = "Coefficients", vline.color = "grey") + theme_sjplot()

lm_noimpute$mod_col5 %>% 
  plot_models(grid = TRUE, p.shape = TRUE, p.threshold = 0.05, colors = "Black",
              title = "Prohibit Abortion",
              m.labels = c("2010", "2012", "2014", "2016", "2018", "2020", "2022"), show.legend = F, show.values = T,
              axis.labels = c("Foreign-Born * White", "Racial Resentment * White", "White", "Conservative", 
                              "Evangelical Christian", "Married", "Income", "Republican",
                              "College Degree", "Female", "Foreign-Born", "Racial Resentment"),
              axis.title = "Coefficients", vline.color = "grey") + theme_sjplot()


lm_AA_noimpute$mod_col %>% 
  plot_models(grid = TRUE, p.shape = TRUE, p.threshold = 0.05, colors = "Black",
              title = "Oppose Affirmative Action",
              m.labels = c("2010", "2012", "2014"), show.legend = F, show.values = T,
              axis.labels = c("Foreign-Born * White", "Racial Resentment * White", "White", "Conservative", 
                              "Evangelical Christian", "Married", "Income", "Republican",
                              "College Degree", "Female", "Foreign-Born", "Racial Resentment"),
              axis.title = "Coefficients", vline.color = "grey") + theme_sjplot()

lm_ACA_noimpute$mod_col %>% 
  plot_models(grid = TRUE, p.shape = TRUE, p.threshold = 0.05, colors = "Black",
              title = "Repeal Affordable Care Act",
              m.labels = c("2012", "2014", "2016", "2018", "2020", "2022"), show.legend = F, show.values = T,
              axis.labels = c("Foreign-Born * White", "Racial Resentment * White", "White", "Conservative", 
                              "Evangelical Christian", "Married", "Income", "Republican",
                              "College Degree", "Female", "Foreign-Born", "Racial Resentment"),
              axis.title = "Coefficients", vline.color = "grey") + theme_sjplot()



