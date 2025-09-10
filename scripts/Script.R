#####################################################
# Contextualizing Pronoun Use
# 
# Authors: Ellen D.B. Riggle, Zakary Clements, Kay Hales, Paz Galupo
# 
# This script:
# - Loads and cleans survey data
# - Computes emotion composites
# - Runs ANOVAs on They+ pronoun groups
# - Computes descriptive stats + Games-Howell post hocs
# - Fits mixed models for Pronoun × Race (Black/White)
# 
# Requires: utility_functions.R in same directory
#####################################################

### Setup ----
library(tidyverse)
library(haven)
library(sjlabelled)
library(rstatix)
library(emmeans)
library(car)
library(lme4)
library(broom)
library(effectsize)
library(gt)
library(crayon)
library(psych)

source("utility_functions.R")

### Load & Clean Data ----
pronoundata <- read_spss("pronoundata.sav") %>%
  sjlabelled::as_label()

missing_summary <- pronoundata %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n()))
print(missing_summary)

### They+ ANOVA Models ----
theydata <- pronoundata %>%
  mutate(
    PronounGroup = case_when(
      Pronoun == "They" ~ "They/Them",
      Pronoun == "They/she" ~ "They/She",
      Pronoun == "They/he" ~ "They/He",
      Pronoun == "They/She/He" ~ "They/She/He",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(PronounGroup)) %>%
  mutate(PronounGroup = factor(PronounGroup, levels = c("They/Them", "They/She", "They/He", "They/She/He")))

table(theydata$PronounGroup)

outcomes <- c(
  "ComfortSW", "ComfortSM", "ComfortTNBGD", "ComfortLGBQ", "ComfortGeneral",
  paste0("Q24_", 1:5),
  paste0("Q22_", 1:5),
  paste0("Q23_", 1:5),
  paste0("Q25_", 1:6),
  "HyperStranger", "HyperConsRel", "HyperWorkPeer", "HyperWithdrawal", "HyperScan"
)

results_list <- lapply(outcomes, function(var) {
  model <- aov(as.formula(paste(var, "~ PronounGroup")), data = theydata)
  anova_summary <- broom::tidy(model)
  ss_between <- anova_summary$sumsq[1]
  ss_within <- anova_summary$sumsq[2]
  df_b <- anova_summary$df[1]
  df_w <- anova_summary$df[2]
  F_val <- anova_summary$statistic[1]
  p_val <- anova_summary$p.value[1]
  eta2 <- ss_between / (ss_between + ss_within)
  
  group_stats <- theydata %>%
    group_by(PronounGroup) %>%
    summarise(mean = mean(.data[[var]], na.rm = TRUE),
              sd = sd(.data[[var]], na.rm = TRUE),
              .groups = "drop")
  
  stats <- paste0(round(group_stats$mean, 2), " (", round(group_stats$sd, 2), ")")
  
  data.frame(
    Variable = var,
    `They/Them` = stats[1],
    `They/She` = stats[2],
    `They/He` = stats[3],
    `They/She/He` = stats[4],
    F = paste0(round(F_val, 2), " (", df_b, ", ", df_w, ")"),
    eta2 = round(eta2, 3),
    p_val = p_val
  )
})

results_full <- bind_rows(results_list) %>%
  mutate(p_adj = round(p.adjust(p_val, method = "BH"), 3)) %>%
  arrange(p_adj)

print(results_full, digits = 4)


### Descriptive Statistics & Games-Howell Tests ----
# Games-Howell is used due to unequal variances and group sizes

### Table 1: Emotions
summarize_items(pronoundata, PronounSimple, names(emotion_vars))
run_games_howell(pronoundata, "PronounSimple", emotion_vars)

### Table 3: Frequency of Sharing
summarize_items(pronoundata, PronounSimple, names(sharing_vars))
run_games_howell(pronoundata, "PronounSimple", sharing_vars)

### Table 4: Comfort and Safety
summarize_items(pronoundata, PronounSimple, names(comfort_vars))
run_games_howell(pronoundata, "PronounSimple", comfort_vars)

### Table 5: Correcting
summarize_items(pronoundata, PronounSimple, names(correcting_vars))
run_games_howell(pronoundata, "PronounSimple", correcting_vars)

### Table 6: Likelihood of Revealing
summarize_items(pronoundata, PronounSimple, names(revealing_vars))
run_games_howell(pronoundata, "PronounSimple", revealing_vars)

### Table 7: Pressure to Reveal
summarize_items(pronoundata, PronounSimple, names(pressure_vars))
run_games_howell(pronoundata, "PronounSimple", pressure_vars)

### Table 7: Concealment
summarize_items(pronoundata, PronounSimple, names(concealment_vars))
run_games_howell(pronoundata, "PronounSimple", concealment_vars)

### Table 8: Hypervigilance
summarize_items(pronoundata, PronounSimple, names(hyper_vars))
run_games_howell(pronoundata, "PronounSimple", hyper_vars)

### Emotion Composite Scoring ----
positive_items <- c("Q20_3", "Q20_4", "Q20_5", "Q20_7", "Q20_8", "Q20_13")
negative_items <- c("Q20_1", "Q20_2", "Q20_6", "Q20_9", "Q20_10", "Q20_11", "Q20_12", "Q20_14")

alpha(pronoundata[, positive_items])
pronoundata$PositiveEmotion <- rowMeans(pronoundata[, positive_items], na.rm = TRUE)

alpha(pronoundata[, negative_items])
pronoundata$NegativeEmotion <- rowMeans(pronoundata[, negative_items], na.rm = TRUE)

emotion_keys <- list(
  EmotionComposite = c(
    "-Q20_1", "-Q20_2", "Q20_3", "Q20_4", "Q20_5", "-Q20_6", "Q20_7", "Q20_8",
    "-Q20_9", "-Q20_10", "-Q20_11", "-Q20_12", "Q20_13", "-Q20_14"
  )
)

emotion_scores <- scoreItems(keys = emotion_keys, items = pronoundata, 
                             impute = "mean", missing = TRUE)
pronoundata$EmotionScore <- emotion_scores$scores[, "EmotionComposite"]

# Optional: exploratory factor analysis
# psych::fa.parallel(pronoundata[, paste0("Q20_", 1:14)])
# psych::fa(pronoundata[, paste0("Q20_", 1:14)], rotate = "none")


### Pronouns × Race (Black vs. White) ----
pronoundata_bw <- pronoundata %>%
  filter(REReduced %in% c("White", "Black")) %>%
  mutate(BlackWhite = factor(REReduced, levels = c("White", "Black")))

blackwhite_outcomes <- c(
  names(comfort_vars),
  names(correcting_vars),
  names(revealing_vars),
  names(pressure_vars),
  names(concealment_vars),
  names(hyper_vars),
  names(emotion_vars)
)

model_results_lmer <- lapply(blackwhite_outcomes, function(dv) {
  run_pronoun_race_lmer(pronoundata_bw, dv)
})
names(model_results_lmer) <- blackwhite_outcomes

invisible(lapply(model_results_lmer, summarize_lmer_model))
