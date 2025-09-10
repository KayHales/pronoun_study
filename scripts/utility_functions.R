#####################################################
# Contextualizing Pronoun Use: Utility Functions
# 
# Authors: Ellen D.B. Riggle, Zakary Clements, Kay Hales, Paz Galupo
# Last updated: 2025-07-10
# 
# Purpose: Define reusable functions and variable label lists
# Used in: full_analysis.R
# 
#####################################################


### Load Required Packages ----
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(rstatix)
library(crayon)
library(psych)


### Summary Table Function ----
# Computes means and SDs by group
summarize_items <- function(data, group_var, item_vars) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      across(all_of(item_vars),
             list(Mean = ~ mean(.x, na.rm = TRUE),
                  SD = ~ sd(.x, na.rm = TRUE))),
      .groups = "drop"
    ) %>%
    print(width = Inf)
}


### Games-Howell Post Hoc Test ----
# Iterates over named item list and prints pairwise results
run_games_howell <- function(data, group_var, item_list) {
  for (var in names(item_list)) {
    label <- item_list[[var]]
    cat("\n", crayon::bold(label), "(", var, ")\n")
    print(games_howell_test(data, formula = as.formula(paste(var, "~", group_var)), detailed = TRUE))
  }
}


### Models for Black/White x Pronouns ----
run_pronoun_race_lmer <- function(data, outcome_var) {
  formula_str <- paste0(outcome_var, " ~ PronounSimple * BlackWhite + (1 | PronounSimple:BlackWhite)")
  model <- lmer(as.formula(formula_str), data = data)
  
  model_anova <- car::Anova(model, type = 3)
  interaction_p <- model_anova["PronounSimple:BlackWhite", "Pr(>Chisq)"]
  
  if (!is.na(interaction_p) && interaction_p < 0.05) {
    emms <- emmeans(model, ~ BlackWhite | PronounSimple)
    contrasts <- pairs(emms, adjust = "tukey")
  } else {
    emms <- NULL
    contrasts <- NULL
  }
  
  list(
    outcome = outcome_var,
    model = model,
    anova = model_anova,
    interaction_p = interaction_p,
    emmeans = emms,
    contrasts = contrasts
  )
}


### Summary of Model Results ----
# Prints interaction p-value and contrast table if applicable
summarize_lmer_model <- function(result) {
  cat("\nOutcome:", result$outcome, "\n")
  cat("  Interaction p-value:", round(result$interaction_p, 4), "\n")
  
  if (!is.null(result$contrasts)) {
    print(result$contrasts)
  } else {
    cat("  No significant interaction. Contrasts not computed.\n")
  }
}


### Variable Label Lists ----

sharing_vars <- c(
  Q19_1 = "Family",
  Q19_2 = "Partner(s)",
  Q19_3 = "Coworkers and Peers",
  Q19_4 = "Friends",
  Q19_5 = "Bosses or Teachers",
  Q19_6 = "Strangers",
  Q19_7 = "Generally"
)

comfort_vars <- c(
  ComfortSW = "Straight cis women",
  ComfortSM = "Straight cis men",
  ComfortTNBGD = "Trans and nonbinary gender-diverse people",
  ComfortLGBQ = "Cis LGBQ people",
  ComfortGeneral = "General population"
)

correcting_vars <- c(
  Q24_1 = "Straight cis women",
  Q24_2 = "Straight cis men",
  Q24_3 = "Trans and nonbinary gender-diverse people",
  Q24_4 = "Cis LGBQ people",
  Q24_5 = "General population"
)

revealing_vars <- c(
  Q22_1 = "Straight cis women",
  Q22_2 = "Straight cis men",
  Q22_3 = "Trans and nonbinary gender-diverse people",
  Q22_4 = "Cis LGBQ people",
  Q22_5 = "General population"
)

pressure_vars <- c(
  Q23_1 = "Straight cis women",
  Q23_2 = "Straight cis men",
  Q23_3 = "Trans and nonbinary gender-diverse people",
  Q23_4 = "Cis LGBQ people",
  Q23_5 = "General population"
)

concealment_vars <- c(
  Q25_1 = "Strangers",
  Q25_2 = "Cis people",
  Q25_3 = "Trans and nonbinary gender-diverse people",
  Q25_4 = "Cis LGBQ people",
  Q25_5 = "General population",
  Q25_6 = "In general"
)

hyper_vars <- c(
  HyperStranger = "Strangers",
  HyperConsRel = "Cis people in close relationships",
  HyperWorkPeer = "Coworkers or peers",
  HyperWithdrawal = "Desire to withdraw",
  HyperScan = "Scanning for safety"
)

emotion_vars <- c(
  Q20_1 = "Anxious",
  Q20_2 = "Stressed",
  Q20_3 = "Euphoric",
  Q20_4 = "Happy",
  Q20_5 = "Authentic",
  Q20_6 = "Vigilant",
  Q20_7 = "Belonging",
  Q20_8 = "Accepted",
  Q20_9 = "Wary",
  Q20_10 = "Fearful",
  Q20_11 = "Rejected",
  Q20_12 = "Uncertain",
  Q20_13 = "Affirmed",
  Q20_14 = "Annoyed"
)
