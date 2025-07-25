## Primary Analysis and Figures, Redpilling Paper (Prolific Sample)
## Stephanie Pedron (pedron.2@osu.edu)

## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

rm(list = ls())
source("C:/Users/steph/Documents/Courses/PhD/Papers/RP Stuff/codes/rp_datacleaner.r")


###### Data Analysis #####

## Survey Designs
racecomm_design <- svydesign(ids = ~1, data = rp_data)
amercomm_design <- svydesign(ids = ~1, data = rp_data)

## Racial Identity Commitment
## Anti-diversity DV
lm_main_antidiv <- survey::svyglm(antidiversity_DV ~ exp_condition + race_commitment + race + pid7 + gender + educ + employment_status +
                                    income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, 
                                  design = racecomm_design, family = stats::gaussian())
summary(lm_main_antidiv)

lm_main_antidiv %>% 
  marginaleffects::avg_predictions(
    variables = "exp_condition"
  ) %>% 
  broom::tidy()

lm_main_antidiv %>% 
  marginaleffects::avg_comparisons(
    variables = list(
      exp_condition = "pairwise"
    )
  )

lm_subgroup_antidiv <- survey::svyglm(antidiversity_DV ~ exp_condition * race_commitment + race + pid7 + gender + educ + employment_status +
                                        income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, 
                                      design = racecomm_design, family = stats::gaussian())
summary(lm_subgroup_antidiv)

lm_subgroup_antidiv %>% 
  marginaleffects::avg_predictions(
    variables = "exp_condition",
    by = "race_commitment"
  ) %>% 
  broom::tidy()

lm_subgroup_antidiv %>% 
  marginaleffects::avg_comparisons(
    variables = list(
      exp_condition = "pairwise"),
    by = "race_commitment"
  ) # to get soft sell - hard sell just multiply estimate by -1


## Monoculturalism DV
lm_main_mono <- survey::svyglm(monoculturalism_DV ~ exp_condition + race_commitment + race + pid7 + gender + educ + employment_status +
                                 income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, 
                               design = racecomm_design, family = stats::gaussian())
summary(lm_main_mono)

lm_main_mono %>% 
  marginaleffects::avg_predictions(
    variables = "exp_condition"
  ) %>% 
  broom::tidy()

lm_main_mono %>% 
  marginaleffects::avg_comparisons(
    variables = list(
      exp_condition = "pairwise"
    )
  )

lm_subgroup_mono <- survey::svyglm(monoculturalism_DV ~ exp_condition * race_commitment + race + pid7 + gender + educ + employment_status +
                                     income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, 
                                   design = racecomm_design, family = stats::gaussian())
summary(lm_subgroup_mono)

lm_subgroup_mono %>% 
  marginaleffects::avg_predictions(
    variables = "exp_condition",
    by = "race_commitment"
  ) %>% 
  broom::tidy()

lm_subgroup_mono %>% 
  marginaleffects::avg_comparisons(
    variables = list(
      exp_condition = "pairwise"),
    by = "race_commitment"
  ) # to get soft sell - hard sell just multiply estimate by -1


## American Identity Commitment
## Anti-diversity DV
lm_main_amer_antidiv <- survey::svyglm(antidiversity_DV ~ exp_condition + american_commitment + race + pid7 + gender + educ + employment_status +
                                         income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, 
                                       design = amercomm_design, family = stats::gaussian())
summary(lm_main_amer_antidiv)

lm_main_amer_antidiv %>% 
  marginaleffects::avg_predictions(
    variables = "exp_condition"
  ) %>% 
  broom::tidy()

lm_main_amer_antidiv %>% 
  marginaleffects::avg_comparisons(
    variables = list(
      exp_condition = "pairwise"
    )
  )

lm_subgroup_amer_antidiv <- survey::svyglm(antidiversity_DV ~ exp_condition * american_commitment + race + pid7 + gender + educ + employment_status +
                                             income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, 
                                           design = amercomm_design, family = stats::gaussian())
summary(lm_subgroup_amer_antidiv)

lm_subgroup_amer_antidiv %>% 
  marginaleffects::avg_predictions(
    variables = "exp_condition",
    by = "american_commitment"
  ) %>% 
  broom::tidy()

lm_subgroup_amer_antidiv %>% 
  marginaleffects::avg_comparisons(
    variables = list(
      exp_condition = "pairwise"),
    by = "american_commitment"
  )


## Monoculturalism DV
lm_main_amer_mono <- survey::svyglm(monoculturalism_DV ~ exp_condition + american_commitment + race + pid7 + gender + educ + employment_status +
                                      income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, 
                                    design = amercomm_design, family = stats::gaussian())
summary(lm_main_amer_mono)

lm_main_amer_mono %>% 
  marginaleffects::avg_predictions(
    variables = "exp_condition"
  ) %>% 
  broom::tidy()

lm_main_amer_mono %>% 
  marginaleffects::avg_comparisons(
    variables = list(
      exp_condition = "pairwise"
    )
  )

lm_subgroup_amer_mono <- survey::svyglm(monoculturalism_DV ~ exp_condition * american_commitment + race + pid7 + gender + educ + employment_status +
                                          income + expert_mistrust + authoritarianism + egalitarianism + anti_elitism + ethnocentrism_score, 
                                        design = amercomm_design, family = stats::gaussian())
summary(lm_subgroup_amer_mono)

lm_subgroup_amer_mono %>% 
  marginaleffects::avg_predictions(
    variables = "exp_condition",
    by = "american_commitment"
  ) %>% 
  broom::tidy()

lm_subgroup_amer_mono %>% 
  marginaleffects::avg_comparisons(
    variables = list(
      exp_condition = "pairwise"),
    by = "american_commitment"
  ) 

## H5 test by race
lm_race_antidiv <- lm(antidiversity_DV ~ exp_condition + race, data = rp_data)
summary(lm_race_antidiv)

lm_race_antidiv %>% 
  marginaleffects::avg_comparisons(
    variables = list(
      exp_condition = "pairwise"),
    by = "race"
  )

emm <- emmeans(lm_race_antidiv, ~ race)
pairs(emm, adjust = "tukey")

lm_race_mono <- lm(monoculturalism_DV ~ exp_condition + race, data = rp_data)
summary(lm_race_mono)

emm2 <- emmeans(lm_race_mono, ~ race)
pairs(emm2, adjust = "tukey")
## NOTE: Direction is supported in point estimates (black respondents score lower than others)
## But most not significant (except for monoculturalism black - hispanic)

#
##### Figures #####

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
                        "TreatmentA-SoftSell - Control" = "Soft Sell", # main effects
                        "TreatmentB-HardSell - Control" = "Hard Sell",
                        "TreatmentB-HardSell - TreatmentA-SoftSell" = "Hard Sell -\nSoft Sell",
                        "TreatmentC-ProDiversity - Control" = "Pro-Diversity",
                        "TreatmentC-ProDiversity - TreatmentA-SoftSell" = "Pro-Diversity -\nSoft Sell",
                        "TreatmentC-ProDiversity - TreatmentB-HardSell" = "Pro-Diversity -\nHard Sell",
                        "mean(TreatmentA-SoftSell) - mean(Control)" = "Soft Sell", # these are subgroup effects
                        "mean(TreatmentB-HardSell) - mean(Control)" = "Hard Sell",
                        "mean(TreatmentB-HardSell) - mean(TreatmentA-SoftSell)" = "Hard Sell -\nSoft Sell",
                        "mean(TreatmentC-ProDiversity) - mean(Control)" = "Pro-Diversity",
                        "mean(TreatmentC-ProDiversity) - mean(TreatmentA-SoftSell)" = "Pro-Diversity -\nSoft Sell",
                        "mean(TreatmentC-ProDiversity) - mean(TreatmentB-HardSell)" = "Pro-Diversity -\nHard Sell"
      ),
      sig = add_sig_stars(p.value),
      label = paste0(scales::number(estimate, accuracy = 0.0001), sig)
    )
}

## Plot function
plot_panel <- function(df) {
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
      axis.text.y = element_text(size = 11, hjust = 0),
      axis.text.x = element_text(size = 10),
      strip.text.y = element_text(angle = 0),
      panel.background = element_rect(fill = "grey97", color = "grey97"),
      strip.background  = element_rect(fill = "grey97", color = "grey97"),
      panel.grid = element_blank()
    )
}

## Main Effects Plot
race_antidiv_main <- avg_comparisons(
  lm_main_antidiv,
  variables = list(exp_condition = "pairwise")
) %>%
  mutate(panel = "Anti-Diversity") %>%
  rename_contrasts()

race_mono_main <- avg_comparisons(
  lm_main_mono,
  variables = list(exp_condition = "pairwise")
) %>%
  mutate(panel = "Monoculturalism") %>%
  rename_contrasts()

race_main_effects <- bind_rows(race_antidiv_main, race_mono_main)

race_main_effects_filtered <- race_main_effects %>%
  group_by(panel) %>%
  slice(1:4) %>%
  ungroup()

race_main_effects_filtered <- race_main_effects_filtered %>%
  group_by(panel) %>%
  mutate(
    contrast = factor(contrast, levels = {
      original <- unique(contrast)
      reordered <- c(original[3], original[-3]) 
      reordered
    })
  ) %>%
  ungroup()

race_main_plot <- plot_panel(race_main_effects_filtered)

amer_antidiv_main <- avg_comparisons(
  lm_main_amer_antidiv,
  variables = list(exp_condition = "pairwise")
) %>%
  mutate(panel = "Anti-Diversity") %>%
  rename_contrasts()

amer_mono_main <- avg_comparisons(
  lm_main_amer_mono,
  variables = list(exp_condition = "pairwise")
) %>%
  mutate(panel = "Monoculturalism") %>%
  rename_contrasts()

amer_main_effects <- bind_rows(amer_antidiv_main, amer_mono_main)

amer_main_effects_filtered <- amer_main_effects %>%
  group_by(panel) %>%
  slice(1:4) %>%
  ungroup()

amer_main_effects_filtered <- amer_main_effects_filtered %>%
  group_by(panel) %>%
  mutate(
    contrast = factor(contrast, levels = {
      original <- unique(contrast)
      reordered <- c(original[3], original[-3])
      reordered
    })
  ) %>%
  ungroup()

amer_main_plot <- plot_panel(amer_main_effects_filtered)

race_main_plot <- race_main_plot + ggtitle("Racial Identity")
amer_main_plot <- amer_main_plot + ggtitle(" American Identity")
main_effects_combined <- race_main_plot / amer_main_plot +
  plot_layout(guides = "collect") & 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.text.y = element_text(hjust = 0)
  )

main_effects_combined # main effects across identity types

## RACE COMM ANTIDIV PLOT
# Subgroup effects
subgroup_effects <- avg_comparisons(
  lm_subgroup_antidiv,
  variables = list(exp_condition = "pairwise"),
  by = "race_commitment") %>%
  mutate(panel = case_when(
    race_commitment == "Low" ~ "Racial Identity\nCommitment: Low",
    race_commitment == "High" ~ "Racial Identity\nCommitment: High"
  )) %>%  
  rename_contrasts()

# Combine
all_effects <- subgroup_effects %>%
  mutate(panel = factor(panel, levels = c("Racial Identity\nCommitment: Low", "Racial Identity\nCommitment: High")))

# Only keep estimates 3 and 4 per panel
filtered_effects <- all_effects %>%
  group_by(panel) %>%
  slice(1:4) %>%
  ungroup()

filtered_effects <- filtered_effects %>%
  group_by(panel) %>%
  mutate(
    contrast = factor(contrast, levels = {
      original <- unique(contrast)
      reordered <- c(original[3], original[-3])  
      reordered
    })
  ) %>%
  ungroup()

# Plot the data with facets
race_antidiv_plot <- plot_panel(filtered_effects)

## RACE COMM MONO PLOT
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

all_effects <- subgroup_effects %>%
  mutate(panel = factor(panel, levels = c("Racial Identity\nCommitment: Low", "Racial Identity\nCommitment: High")))
filtered_effects <- all_effects %>%
  group_by(panel) %>%
  slice(1:4) %>%
  ungroup()

filtered_effects <- filtered_effects %>%
  group_by(panel) %>%
  mutate(
    contrast = factor(contrast, levels = {
      original <- unique(contrast)
      reordered <- c(original[3], original[-3])  
      reordered
    })
  ) %>%
  ungroup()

race_mono_plot <- plot_panel(filtered_effects)


## Making one plot for race commitment
race_antidiv_plot <- race_antidiv_plot + ggtitle("Anti-Diversity")
race_mono_plot <- race_mono_plot + ggtitle("Monoculturalism")
combined_racecomm_plot <- race_antidiv_plot / race_mono_plot +
  plot_layout(guides = "collect") & 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.text.y = element_text(hjust = 0)
  )

combined_racecomm_plot



## AMER COMM ANTIDIV PLOT
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

all_effects <- subgroup_effects %>%
  mutate(panel = factor(panel, levels = c("American Identity\nCommitment: Low", "American Identity\nCommitment: High")))

filtered_effects <- all_effects %>%
  group_by(panel) %>%
  slice(1:4) %>%
  ungroup()

filtered_effects <- filtered_effects %>%
  group_by(panel) %>%
  mutate(
    contrast = factor(contrast, levels = {
      original <- unique(contrast)
      reordered <- c(original[3], original[-3])  
      reordered
    })
  ) %>%
  ungroup()

amer_antidiv_plot <- plot_panel(filtered_effects)

## AMER COMM MONO PLOT
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

all_effects <- subgroup_effects %>%
  mutate(panel = factor(panel, levels = c("American Identity\nCommitment: Low", "American Identity\nCommitment: High")))

filtered_effects <- all_effects %>%
  group_by(panel) %>%
  slice(1:4) %>%
  ungroup()

filtered_effects <- filtered_effects %>%
  group_by(panel) %>%
  mutate(
    contrast = factor(contrast, levels = {
      original <- unique(contrast)
      reordered <- c(original[3], original[-3])  
      reordered
    })
  ) %>%
  ungroup()

amer_mono_plot <- plot_panel(filtered_effects)

## Making one plot for american commitment
amer_antidiv_plot <- amer_antidiv_plot + ggtitle("Anti-Diversity")
amer_mono_plot <- amer_mono_plot + ggtitle("Monoculturalism")
combined_amer_plot <- amer_antidiv_plot / amer_mono_plot +
  plot_layout(guides = "collect") & 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.text.y = element_text(hjust = 0)
  )
combined_amer_plot


## BY RACE PLOT
# extracting pairwise comparisons
emm_antidiv <- emmeans(lm_race_antidiv, ~ race)
emm_mono <- emmeans(lm_race_mono, ~ race)

pairwise_antidiv <- pairs(emm_antidiv, adjust = "tukey")
pairwise_mono <- pairs(emm_mono, adjust = "tukey")

# CIs
pairwise_antidiv_confint <- confint(pairwise_antidiv)
pairwise_mono_confint <- confint(pairwise_mono)

# Merging CIs and comparisons
pairwise_antidiv_with_ci <- cbind(pairwise_antidiv, pairwise_antidiv_confint)
pairwise_mono_with_ci <- cbind(pairwise_mono, pairwise_mono_confint)

# Filtering (Asian-Black, Asian-Hispanic, Black-Hispanic)
desired_comparisons <- c("Asian - Black", "Asian - Hispanic", "Black - Hispanic")

colnames(pairwise_antidiv_with_ci)
colnames(pairwise_antidiv_with_ci) <- make.names(colnames(pairwise_antidiv_with_ci), unique = TRUE)
filtered_pairs_antidiv <- pairwise_antidiv_with_ci %>%
  filter(contrast %in% desired_comparisons)

colnames(pairwise_mono_with_ci)
colnames(pairwise_mono_with_ci) <- make.names(colnames(pairwise_mono_with_ci), unique = TRUE)
filtered_pairs_mono <- pairwise_mono_with_ci %>%
  filter(contrast %in% desired_comparisons)

filtered_pairs_antidiv <- filtered_pairs_antidiv %>%
  mutate(significance = case_when(
    p.value < 0.05  ~ "*",
    TRUE ~ ""  
  ))

filtered_pairs_mono <- filtered_pairs_mono %>%
  mutate(significance = case_when(
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))

plot_antidiv_race <- ggplot(filtered_pairs_antidiv, aes(x = estimate, y = contrast)) +
  geom_point(color = "black", size = 3) +  
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.1) +  
  geom_text(aes(label = paste(round(estimate, 3), significance)), 
            color = "black", size = 3, vjust = -1) + 
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  scale_x_continuous(limits = c(-0.25, 0.2)) +
  labs(title = "Anti-Diversity",
       x = "Pairwise Comparison",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 11, hjust = 0),
    axis.text.x = element_text(size = 10),
    strip.text.y = element_text(angle = 0),
    panel.background = element_rect(fill = "grey97", color = "grey97"),
    strip.background  = element_rect(fill = "grey97", color = "grey97"),
    panel.grid = element_blank()
  )

plot_mono_race <- ggplot(filtered_pairs_mono, aes(x = estimate, y = contrast)) +
  geom_point(color = "black", size = 3) + 
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.1) + 
  geom_text(aes(label = paste(round(estimate, 3), significance)), 
            color = "black", size = 3, vjust = -1) + 
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  scale_x_continuous(limits = c(-0.25, 0.2)) +
  labs(title = "Monoculturalism",
       x = "Pairwise Comparison",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 11, hjust = 0),
    axis.text.x = element_text(size = 10),
    strip.text.y = element_text(angle = 0),
    panel.background = element_rect(fill = "grey97", color = "grey97"),
    strip.background  = element_rect(fill = "grey97", color = "grey97"),
    panel.grid = element_blank()
  )

combined_race_plot <- plot_antidiv_race + plot_mono_race + plot_layout(ncol = 1)
combined_race_plot

#

