## Racial Resentment Correlation - AAPI Racial Resentment Paper
## Stephanie Pedron (pedron.2@osu.edu)

rm(list = ls())

source("C:/Users/steph/Documents/Courses/PhD/Papers/RR Stuff/RR Codes/RR_datacleaner.r")

library(gt)

## Correlation
corr_function <- function(data){
  x <- round(cor(data, use = "pairwise.complete.obs"), 2)
  y <- corrplot(x)
  return(y)
}

## CCES years used for models
## Removing non-numeric and unnecessary variables
corr_2010 <- CCES10[, c(1, 3:7, 10, 13)]
corr_2012 <- CCES12[, c(1, 3:7, 10, 13)]
corr_2014 <- CCES14[, c(1, 3:7, 10, 13)]
corr_2016 <- CCES16[, c(1, 3:13, 15:16, 18)]
corr_2018 <- CCES18[, c(1, 3:13, 15:17)] 
corr_2020 <- CCES20[, c(1, 3:13, 15:17)] 
corr_2022 <- CCES22[, c(1, 3:13, 15:17)] 

corr_2016 <- corr_2016[, -13]
corr_2018 <- corr_2018[, -13] 
corr_2020 <- corr_2020[, -13] 
corr_2022 <- corr_2022[, -13] 

corr_function(corr_2010)
corr_function(corr_2012)
corr_function(corr_2014)
corr_function(corr_2016)
corr_function(corr_2018)
corr_function(corr_2020)
corr_function(corr_2022)

## Correlation between racial resentment and partisanship for Whites and Asians
correlation_2010_Asians <- CCES10_Asians %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2010_White <- CCES10_Whites %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2012_Asians <- CCES12_Asians %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2012_White <- CCES12_Whites %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2014_Asians <- CCES14_Asians %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2014_White <- CCES14_Whites %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2016_Asians <- CCES16_Asians %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2016_White <- CCES16_Whites %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2018_Asians <- CCES18_Asians %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2018_White <- CCES18_Whites %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2020_Asians <- CCES20_Asians %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2020_White <- CCES20_Whites %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2022_Asians <- CCES22_Asians %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2022_White <- CCES22_Whites %>%
  select(racialresentment, pid7) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

# Table
cor_data <- data.frame(
  year = c(2010, 2012, 2014, 2016, 2018, 2020, 2022),
  asian_RR_PID = c(0.37, 0.34, 0.33, 0.29, 0.44, 0.49, 0.48),
  white_RR_PID = c(0.54, 0.53, 0.49, 0.48, 0.61, 0.64, 0.61)
)

## Corr table of RR and PID
cor_table <- gt(cor_data, rowname_col = "Year") %>%
  tab_header(title = md("**Correlation**"),
             subtitle = "Racial Resentment and Party ID") %>% 
  cols_label(year = "Year",
             asian_RR_PID = "Asian",
             white_RR_PID = "White") %>% 
  tab_source_note(source_note = "Data Source: Cooperative Election Study (2010-2022)") %>% 
  tab_footnote(footnote = "Racial Resentment ranges from 0 (Least resentment) to 1 (Most Resentment). 
  Party ID ranges from 0 (Democrat) to 2 (Republican). 
               2016 is a general measure of racism and not the racial resentment battery.",
               locations = cells_body(columns = year, rows = 4),
               placement = c("right"))

## Correlation between racial resentment and ideology for Whites and Asians
correlation_2010_Asians <- CCES10_Asians %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2010_White <- CCES10_Whites %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2012_Asians <- CCES12_Asians %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2012_White <- CCES12_Whites %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2014_Asians <- CCES14_Asians %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2014_White <- CCES14_Whites %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2016_Asians <- CCES16_Asians %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2016_White <- CCES16_Whites %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2018_Asians <- CCES18_Asians %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2018_White <- CCES18_Whites %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2020_Asians <- CCES20_Asians %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2020_White <- CCES20_Whites %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2022_Asians <- CCES22_Asians %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

correlation_2022_White <- CCES22_Whites %>%
  select(racialresentment, ideo5) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) %>%
  print()

## Table
cor_data2 <- data.frame(
  year = c(2010, 2012, 2014, 2016, 2018, 2020, 2022),
  asian_RR_PID = c(0.35, 0.32, 0.23, 0.20, 0.46, 0.48, 0.47),
  white_RR_PID = c(0.57, 0.54, 0.47, 0.43, 0.63, 0.64, 0.62)
)

cor_table2 <- gt(cor_data2, rowname_col = "Year") %>%
  tab_header(title = md("**Correlation**"),
             subtitle = "Racial Resentment and Ideology") %>% 
  cols_label(year = "Year",
             asian_RR_PID = "Asian",
             white_RR_PID = "White") %>% 
  tab_source_note(source_note = "Data Source: Cooperative Election Study (2010-2022)") %>% 
  tab_footnote(footnote = "Racial Resentment ranges from 0 (Least resentment) to 1 (Most Resentment).
  Ideology ranges from 0 (Liberal) to 2 (Conservative). 
               2016 is a general measure of racism and not the racial resentment battery.",
               locations = cells_body(columns = year, rows = 4),
               placement = c("right"))

rm(correlation_2010_Asians, correlation_2010_White, correlation_2012_Asians, correlation_2012_White,
   correlation_2014_Asians, correlation_2014_White, correlation_2016_Asians, correlation_2016_White,
   corretlation_2018_Asians, correlation_2018_White, correlation_2020_Asians, correlation_2020_White)