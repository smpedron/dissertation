## Appendix, Descriptive Stats and AME Figures
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

rm(list = ls())
source("C:/Users/steph/Documents/Courses/PhD/Papers/RP Stuff/codes/rp_datacleaner.r")
source("C:/Users/steph/Documents/Courses/PhD/Papers/RP Stuff/codes/rp_PrimaryAnalysis.r")

###### Descriptive Stats #####

## Descriptive Stats
summary(rp_data)

table(rp_data$exp_condition)
table(rp_data$exp_condition, rp_data$race_commitment) # uneven
table(rp_data$exp_condition, rp_data$american_commitment)


table(rp_data$income)
table(rp_data$employment_status)
table(rp_data$age)
table(rp_data$gender)
table(rp_data$educ)
table(rp_data$pid7)
table(rp_data$ideo5)
table(rp_data$race)
table(rp_data$race_commitment)
table(rp_data$american_commitment)

## Worried / Persuaded Checks
table(rp_data$exp_condition, rp_data$threat_check)
table(rp_data$exp_condition, rp_data$persuasion_check) 

check_low <- rp_data %>% 
  filter(race_commitment == "Low")
table(check_low$exp_condition, check_low$threat_check) 
table(check_low$exp_condition, check_low$persuasion_check) 

check_high <- rp_data %>% 
  filter(race_commitment == "High")
table(check_high$exp_condition, check_high$threat_check) 
table(check_high$exp_condition, check_high$persuasion_check) 

#
##### All AMEs #####

# Significance
add_sig_stars <- function(p) {
  case_when(
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
}

# Cleaning contrasts
rename_contrasts <- function(df) {
  df %>%
    mutate(
      contrast = recode(contrast,
                        "TreatmentA-SoftSell - Control" = "Soft Sell -\nControl",
                        "TreatmentB-HardSell - Control" = "Hard Sell -\nControl",
                        "TreatmentB-HardSell - TreatmentA-SoftSell" = "Hard Sell -\nSoft Sell",
                        "TreatmentC-ProDiversity - Control" = "Pro-Diversity -\nControl",
                        "TreatmentC-ProDiversity - TreatmentA-SoftSell" = "Pro-Diversity -\nSoft Sell",
                        "TreatmentC-ProDiversity - TreatmentB-HardSell" = "Pro-Diversity -\nHard Sell",
                        "mean(TreatmentA-SoftSell) - mean(Control)" = "Soft Sell -\nControl",
                        "mean(TreatmentB-HardSell) - mean(Control)" = "Hard Sell -\nControl",
                        "mean(TreatmentB-HardSell) - mean(TreatmentA-SoftSell)" = "Hard Sell -\nSoft Sell",
                        "mean(TreatmentC-ProDiversity) - mean(Control)" = "Pro-Diversity -\nControl",
                        "mean(TreatmentC-ProDiversity) - mean(TreatmentA-SoftSell)" = "Pro-Diversity -\nSoft Sell",
                        "mean(TreatmentC-ProDiversity) - mean(TreatmentB-HardSell)" = "Pro-Diversity -\nHard Sell"
      ),
      sig = add_sig_stars(p.value),
      label = paste0(scales::number(estimate, accuracy = 0.0001), sig)
    )
}

## Race Comm - all average comparisons
plot_panel2 <- function(df) {
  ggplot(df, aes(x = contrast, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(color = "black") +
    geom_text(aes(label = label), vjust = -0.5, color = "black", size = 3.5) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray40") +
    coord_flip() +
    facet_grid(panel ~ ., scales = "fixed") +
    scale_y_continuous(limits = c(-0.4, 0.4)) +
    labs(
      x = NULL,
      y = "AME"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.text.y = element_text(size = 9, hjust = 0),
      axis.text.x = element_text(size = 9),
      strip.text.y = element_text(angle = 0),
      panel.background = element_rect(fill = "grey97", color = "grey97"),
      strip.background  = element_rect(fill = "grey97", color = "grey97"),
      panel.grid = element_blank()
    )
}

main_effects <- avg_comparisons(
  lm_main_antidiv,
  variables = list(exp_condition = "pairwise")
) %>%
  mutate(panel = "Main Effects") %>%
  rename_contrasts()

subgroup_effects <- avg_comparisons(
  lm_subgroup_antidiv,
  variables = list(exp_condition = "pairwise"),
  by = "race_commitment"
) %>%
  mutate(panel = case_when(
    race_commitment == "Low" ~ "Racial Identity\nCommitment: Low",
    race_commitment == "High" ~ "Racial Identity\nCommitment: High"
  )) %>%
  rename_contrasts()

all_effects <- bind_rows(main_effects, subgroup_effects)
race_antidiv_plot_all <- plot_panel2(all_effects)

main_effects <- avg_comparisons(
  lm_main_mono,
  variables = list(exp_condition = "pairwise")
) %>%
  mutate(panel = "Main Effects") %>%
  rename_contrasts()

subgroup_effects <- avg_comparisons(
  lm_subgroup_mono,
  variables = list(exp_condition = "pairwise"),
  by = "race_commitment"
) %>%
  mutate(panel = case_when(
    race_commitment == "Low" ~ "Racial Identity\nCommitment: Low",
    race_commitment == "High" ~ "Racial Identity\nCommitment: High"
  )) %>%
  rename_contrasts()

all_effects <- bind_rows(main_effects, subgroup_effects)
race_mono_plot_all <- plot_panel2(all_effects)

race_antidiv_plot_all <- race_antidiv_plot_all + ggtitle("Anti-Diversity")
race_mono_plot_all <- race_mono_plot_all + ggtitle("Monoculturalism")
combined_race_plot_all <- race_antidiv_plot_all / race_mono_plot_all +
  plot_layout(guides = "collect") & 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.text.y = element_text(hjust = 0)
  )

combined_race_plot_all


## Amer comm - all average comparisons
main_effects <- avg_comparisons(
  lm_main_amer_antidiv,
  variables = list(exp_condition = "pairwise")
) %>%
  mutate(panel = "Main Effects") %>%
  rename_contrasts()

subgroup_effects <- avg_comparisons(
  lm_subgroup_amer_antidiv,
  variables = list(exp_condition = "pairwise"),
  by = "american_commitment"
) %>%
  mutate(panel = case_when(
    american_commitment == "Low" ~ "American Identity\nCommitment: Low",
    american_commitment == "High" ~ "American Identity\nCommitment: High"
  )) %>%
  rename_contrasts()

all_effects <- bind_rows(main_effects, subgroup_effects)
amer_antidiv_plot <- plot_panel2(all_effects)

main_effects <- avg_comparisons(
  lm_main_amer_mono,
  variables = list(exp_condition = "pairwise")
) %>%
  mutate(panel = "Main Effects") %>%
  rename_contrasts()

# Subgroup effects
subgroup_effects <- avg_comparisons(
  lm_subgroup_amer_mono,
  variables = list(exp_condition = "pairwise"),
  by = "american_commitment"
) %>%
  mutate(panel = case_when(
    american_commitment == "Low" ~ "American Identity\nCommitment: Low",
    american_commitment == "High" ~ "American Identity\nCommitment: High"
  )) %>%
  rename_contrasts()

all_effects <- bind_rows(main_effects, subgroup_effects)
amer_mono_plot <- plot_panel2(all_effects)

amer_antidiv_plot <- amer_antidiv_plot + ggtitle("Anti-Diversity")
amer_mono_plot <- amer_mono_plot + ggtitle("Monoculturalism")
combined_amer_plot <- amer_antidiv_plot / amer_mono_plot +
  plot_layout(guides = "collect") & 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.text.y = element_text(hjust = 0)
  )
combined_amer_plot

#