## ---
##
## Script name: stevens_etal_2022_rcode.R
##
## Purpose of script: Analyze dog spatial choice data
##
## Author: Dr. Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com)
##
## Date Created: 2021-03-08
##
## Date Finalized: 2022-03-22
##
## License: All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0).
##  You are free to:
##   Share — copy and redistribute the material in any medium or format
##   Adapt — remix, transform, and build upon the material for any purpose, even commercially.
##  Under the following terms:
##   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
##   No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
##
##
## ---


# Load libraries ---------------------------------------------------------------

library(BayesFactor)
library(psych)
library(tidyverse)
library(metaBMA)
library(ggbeeswarm)
library(patchwork)
library(ggdist)
library(papaja)

library(conflicted)
conflict_prefer("filter", "dplyr")

# Define functions ---------------------------------------------------------------

# Run apa_print but include initial zeros
apa_print2 <- function(x) {
  add0 <- function(y) {
    z <- gsub(" \\.", " 0.", y)
    gsub("-\\.", "-0.", z)
  }
  apa <- apa_print(x)
  apa %>% map(add0)
}

# Calculate correlation coefficient and Bayes factor
print_correlation <- function(df, x, y, freq = TRUE) {
  df2 <- filter(df, !is.na(df[, x]) & !is.na(df[, y])) %>%  # remove rows with NAs
    select(x = x, y = y)  # select only x and y columns and rename
  corr_test <- cor.test(df2$y, df2$x)  # conduct correlation 
  r <- round(corr_test$estimate, 2)  # extract correlation coefficient
  p <- round(corr_test$p.value, 2)  # extract p-value
  bfmodel <- correlationBF(df2$y, df2$x)    # conduct Bayesian correlation
  bf <- format(extractBF(bfmodel)$bf, digits = 2)   # extract Bayes factor
  if (freq) {
    paste("N = ", nrow(df2), ", r = ", r, ", p = ", p, ", BF = ", bf, sep = "")  # create text for r and BF
  } else {
    paste("N = ", nrow(df2), ", BF = ", bf, sep = "")  # create text for r and BF
  }
}

# Plot correlations
plot_correlation <- function(df, x, y, xlab, ylab, title = TRUE, stats = FALSE, freq = freq) {
  df %>% 
    select(xvar = x, yvar = y) %>% 
    ggplot(aes(x = xvar, y = yvar)) +
    geom_point(position = position_jitter(width = 0.005, height = 0.05), alpha = 0.7) +  # plot individual points
    geom_smooth(method = "lm", formula = y ~ x, color = cb_palette_black[5]) +  # plot regression line
    ylim(0, 4.8) +
    {if (title) labs(x = xlab, y = ylab, subtitle = print_correlation(df, x, y, freq))} +
    {if (!title) labs(x = xlab, y = ylab)} +
    {if (stats) annotation_custom(grid::textGrob(print_correlation(df, x, y, freq)), xmin = -Inf, xmax = Inf, ymin = 4.7, ymax = 4.9)} +
    scale_color_manual(values = cb_palette_black[5]) + # set group color
    theme_bw() +
    theme(text=element_text(family = "arial"),
          legend.position = "none",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12)) +
    NULL
}

# Print ANOVA output
print_anova <- function(df, x, y, freq = TRUE) {
  df2 <- df %>% 
    select(x = x, y = y) %>%  # select only x and y columns and rename
    filter(!is.na(x) & !is.na(y)) %>%  # remove rows with NAs
    mutate(x = as.factor(x)) 
  if (length(levels(df2$x)) == 2) {
    anova_test <- t.test(y ~ x, data = df2)  # conduct t-test for 2 levels 
  } else if (length(levels(df2$x)) > 2) {
    anova_test <- aov(y ~ x, data = df2)  # conduct ANOVA for > 2 levels
  } else {
    stop("The independent variable requires at least two levels for 'print_anova()`.")
  }
  anova_results <- apa_print2(anova_test)$statistic
  anova_results <- gsub("\\$", "", anova_results)
  bfmodel <- anovaBF(y ~ x, data = df2, progress = FALSE)    # conduct Bayesian correlation
  bf <- format(extractBF(bfmodel)$bf, digits = 2)   # extract Bayes factor
  if (freq) {
    paste0("N = ", nrow(df2), ", ", anova_results, ", BF = ", bf)  # create text for r and BF
  } else {
    paste0("N = ", nrow(df2), ", BF = ", bf)  # create text for r and BF
  }
}

# Plot raincloud plots
plot_raincloud <- function(df, x, y, xlab, ylab, binwidth = 0.05, ymin = 0.2, ymax = 4.8, xmax = 6, title = TRUE, stats = FALSE, rotate = FALSE, freq = freq, rev = FALSE) {
  df %>% 
    select(xvar = x, yvar = y) %>% 
    filter(!is.na(xvar) & !is.na(yvar)) %>%  # remove rows with NAs
    {if (rev) mutate(., xvar = fct_rev(xvar)) else .} %>%  
    ggplot(
      aes(x = xvar, y = yvar, color = xvar, fill = xvar)
      ) +
    stat_dots(side = "left", alpha = 0.5, binwidth = binwidth, justification = 1.05) +  # plot individual points
    stat_slab(alpha = 0.5, scale = 0.6) +
    geom_boxplot(position = position_nudge(x = 0.1, y = 0), width = 0.2, outlier.shape = NA, alpha = 0.5, color = "black") +
    stat_summary(fun.data = "mean_cl_boot", position = position_nudge(x = -0.1, y = 0), color = "black") +
    {if (title) labs(x = xlab, y = ylab, subtitle = print_anova(df, x, y, freq))} +
    {if (!title) labs(x = xlab, y = ylab)} +
    {if (stats & !rotate) annotation_custom(grid::textGrob(print_anova(df, x, y, freq)), xmin = -Inf, xmax = Inf, ymin = ymax - 0.1, ymax = ymax + 0.1)} +
    {if (stats & rotate) annotation_custom(grid::textGrob(print_anova(df, x, y, freq)), ymin = -Inf, ymax = Inf, xmin = xmax + 0.5, xmax = xmax + 0.6)} +
    ylim(ymin, ymax) +
    scale_color_manual(values = cb_palette_black[c(5, 3, 1, 6, 7, 2)]) + # set group color
    scale_fill_manual(values = cb_palette_black[c(5, 3, 1, 6, 7, 2)]) + # set group color
    # labs(x = xlab, y = ylab, subtitle = print_anova(df, x, y)) +
    {if (rotate) coord_flip()} +
    theme_bw() +
    theme(text = element_text(family = "arial"),
          legend.position = "none",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12)) +
    NULL
}

# Calculate reliability (omega) for scales
calculate_reliability <- function(df, scale, type = "omega") {
  clean_data <- df %>% 
    filter(grepl(scale, survey)) %>%  # filter this scale
    select(-experiment, -survey) %>%  # remove naming column
    select_if(function(x) {!all(is.na(x))})  # remove empty columns
  if (type == "omega") {
    reliability <- psych::omega(clean_data, warnings = FALSE, plot = FALSE)
  } else if (type == "alpha") {
    reliability <- psych::alpha(clean_data)
  } else {
    stop("Incorrect reliability measure specified. Use 'omega' or 'alpha'")
  }
  return(reliability)
}

# Print Bayes factors
printbf <- function(x, digits = 2, cutoff = NULL) {
  # Check if object is numeric, BFBayesFactor, or other
  if (is.numeric(x)) {
    bf <- x
  } else if (class(x) == "BFBayesFactor") {
    bf <- extractBF(x)$bf
  } else {
    stop("Object is not numeric or of class BFBayesFactor.")
  }
  # Format Bayes factor
  if (is.null(cutoff)) {
    if (bf > 1000 | bf < 0.001) {
      bf <- typeset_scientific(sprintf("%.2e", bf))
    }
    p <- paste0("$\\mathrm{BF}_{\\textrm{10}} = ", printnum(bf), "$")
  } else {
    if (bf > cutoff) {
      p <- paste0("$\\mathrm{BF}_{\\textrm{10}} > ", cutoff, "$")
    } else if (bf <  1 / cutoff) {
      p <- paste0("$\\mathrm{BF}_{\\textrm{10}} < ", printnum(1 / cutoff, digits = digits), "$")
    } else {
      p <- paste0("$\\mathrm{BF}_{\\textrm{10}} = ", printnum(bf), "$")
    }
  }
  return(p)
}

# Print reliability data
print_reliability <- function(x, type = "omega") {
  if (type == "omega") {
    round(x$omega.tot, 2)
  } else if (type == "alpha") {
    round(x$total$raw_alpha, 2)
  } else {
    stop("Enter reliability type of 'omega' or 'delta'.")
  }
}


# Define color-blind safe colors
cb_palette_black <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")


# Import data ---------------------------------------------------

all_data <- read_csv("stevens_etal_2022_data1.csv") %>% 
  mutate(experiment = as.character(experiment),
         cognitive_ability = crt_score + numeracy_score,
         household_income = fct_relevel(household_income, "Less than $25,000", "$25,000-$49,999", "$50,000-$74,999", "$75,000-$99,999", "More than $100,000", "I would rather not say"))
item_data <- read_csv("stevens_etal_2022_data2.csv")

# CCHIL data ---------------------------------------------------
## Extract CCHIL data ---------------------------------------------------

clean_data_cchil <- all_data %>% 
  filter(experiment == 1)

item_data_cchil <- item_data %>% 
  filter(experiment == 1)

## Demographics ---------------------------------------------------

owner_gender_cchil <- table(clean_data_cchil$owner_gender)
owner_marital_cchil <- table(clean_data_cchil$owner_marital_status)
owner_otherdogs_cchil <- table(clean_data_cchil$other_dogs)
owner_income_cchil <- table(clean_data_cchil$household_income)

dog_age_cchil <- clean_data_cchil %>% 
  summarise(mean = mean(dog_age),
            sd = sd(dog_age),
            min = min(dog_age),
            max = max(dog_age))
dog_sex_cchil <- clean_data_cchil %>% 
  select(dog_sex, dog_neutered) %>% 
  unite(sex_neuter, everything())
dog_sex_table_cchil <- table(dog_sex_cchil)

## Reliability for survey measures ---------------------------------------------------

### Dog behavior (Bennett & Rolf 2007 ---------------------------------------------------
dog_behavior_disobedient_reliability_cchil <- calculate_reliability(item_data_cchil, "dog_behavior_disobedient")
dog_behavior_aggressive_reliability_cchil <- calculate_reliability(item_data_cchil, "dog_behavior_aggressive")
dog_behavior_nervous_reliability_cchil <- calculate_reliability(item_data_cchil, "dog_behavior_nervous")
dog_behavior_destructive_reliability_cchil <- calculate_reliability(item_data_cchil, "dog_behavior_destructive")
dog_behavior_excitable_reliability_cchil <- calculate_reliability(item_data_cchil, "dog_behavior_excitable")

### Dog obedience (Hiby et al. 2004 ---------------------------------------------------
dog_obedience_reliability_cchil <- calculate_reliability(item_data_cchil, "dog_obedience")

### Dog problematic behaviors (Hiby et al. 2004 ---------------------------------------------------
dog_problem_behaviors_reliability_cchil <- calculate_reliability(item_data_cchil, "dog_problem_behaviors")

### DIAS (Wright et al. 2011 ---------------------------------------------------
dias_reliability_cchil <- calculate_reliability(item_data_cchil, "dias_")
dias_behavioral_regulation_reliability_cchil <- calculate_reliability(item_data_cchil, "dias_behavioral_regulation")
dias_aggression_reliability_cchil <- calculate_reliability(item_data_cchil, "dias_aggression")
dias_responsiveness_reliability_cchil <- calculate_reliability(item_data_cchil, "dias_responsiveness")

### MDORS (Dwyer et al. 2006 ---------------------------------------------------
mdors_reliability_cchil <- calculate_reliability(item_data_cchil, "mdors")

### Owner personality (Gosling et al., 2003 ---------------------------------------------------
# Note: must use alpha because only two items per scale
owner_personality_extraversion_reliability_cchil <- calculate_reliability(item_data_cchil, "owner_personality_extraversion", type = "alpha")
owner_personality_agreeableness_reliability_cchil <- calculate_reliability(item_data_cchil, "owner_personality_agreeableness", type = "alpha")
owner_personality_conscientiousness_reliability_cchil <- calculate_reliability(item_data_cchil, "owner_personality_conscientiousness", type = "alpha")
owner_personality_stability_reliability_cchil <- calculate_reliability(item_data_cchil, "owner_personality_stability", type = "alpha")
owner_personality_openness_reliability_cchil <- calculate_reliability(item_data_cchil, "owner_personality_openness", type = "alpha")

### Cognitive Reflection Test (Frederick 2005 ---------------------------------------------------
crt_reliability_cchil <- calculate_reliability(item_data_cchil, "crt")

### Berlin Numeracy Test (Cokely et al., 2012 ---------------------------------------------------
numeracy_reliability_cchil <- calculate_reliability(item_data_cchil, "numeracy")


## DIAS ------------------------------------------------------------------
### Correlations ------------------------------------------------------------------
dias_corr_cchil <- cor.test(clean_data_cchil$max_distance, clean_data_cchil$dias_overall_score)
dias_corr_bf_cchil <- correlationBF(clean_data_cchil$max_distance, clean_data_cchil$dias_overall_score)

dias_reg_corr_cchil <- cor.test(clean_data_cchil$max_distance, clean_data_cchil$dias_behavioral_regulation_score)
dias_reg_corr_bf_cchil <- correlationBF(clean_data_cchil$max_distance, clean_data_cchil$dias_behavioral_regulation_score)

dias_agg_corr_cchil <- cor.test(clean_data_cchil$max_distance, clean_data_cchil$dias_aggression_score)
dias_agg_corr_bf_cchil <- correlationBF(clean_data_cchil$max_distance, clean_data_cchil$dias_aggression_score)

dias_resp_corr_cchil <- cor.test(clean_data_cchil$max_distance, clean_data_cchil$dias_responsiveness_score)
dias_resp_corr_bf_cchil <- correlationBF(clean_data_cchil$max_distance, clean_data_cchil$dias_responsiveness_score)

### Plots ------------------------------------------------------------------
dias_plot_cchil <- plot_correlation(clean_data_cchil, "dias_overall_score", "max_distance", "DIAS overall score", "Distance traveled (m)", title = FALSE, freq = TRUE)
dias_reg_plot_cchil <- plot_correlation(clean_data_cchil, "dias_behavioral_regulation_score", "max_distance", "DIAS behavioral regulation score", "Distance traveled (m)", title = FALSE, stats = TRUE, freq = TRUE)
dias_agg_plot_cchil <- plot_correlation(clean_data_cchil, "dias_aggression_score", "max_distance", "DIAS aggression score", "Distance traveled (m)", title = FALSE, stats = TRUE, freq = TRUE)
dias_resp_plot_cchil <- plot_correlation(clean_data_cchil, "dias_responsiveness_score", "max_distance", "DIAS responsiveness score", "Distance traveled (m)", title = FALSE, stats = TRUE, freq = TRUE)


# Kenl Inn data ---------------------------------------------------
## Extract CCHIL data ---------------------------------------------------

clean_data_kenlinn <- all_data %>% 
  filter(experiment == 2)

item_data_kenlinn <- item_data %>% 
  filter(experiment == 2)


## Demographics ---------------------------------------------------

owner_gender_kenlinn <- table(clean_data_kenlinn$owner_gender)
owner_marital_kenlinn <- table(clean_data_kenlinn$owner_marital_status)
owner_otherdogs_kenlinn <- table(clean_data_kenlinn$other_dogs)
owner_income_kenlinn <- table(clean_data_kenlinn$household_income)
dog_age_kenlinn <- clean_data_kenlinn %>% 
  summarise(mean = mean(dog_age),
            sd = sd(dog_age),
            min = min(dog_age),
            max = max(dog_age))
dog_sex_kenlinn <- clean_data_kenlinn %>% 
  select(dog_sex, dog_neutered) %>% 
  unite(sex_neuter, everything())
dog_sex_table_kenlinn <- table(dog_sex_kenlinn)


## Reliability for survey measures ---------------------------------------------------
### C-BARQ training ---------------------------------------------------
dog_obedience_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "dog_obedience")

### Dog obedience (Hiby et al. 2004)  ---------------------------------------------------
dog_obedience_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "dog_obedience")

### DIAS (Wright et al. 2011 ---------------------------------------------------
dias_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "dias_")
dias_behavioral_regulation_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "dias_behavioral_regulation")
dias_aggression_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "dias_aggression")
dias_responsiveness_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "dias_responsiveness")

### Owner personality (Gosling et al., 2003)  ---------------------------------------------------
# Note: must use alpha because only two items per scale
owner_personality_extraversion_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "owner_personality_extraversion", type = "alpha")
owner_personality_agreeableness_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "owner_personality_agreeableness", type = "alpha")
owner_personality_conscientiousness_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "owner_personality_conscientiousness", type = "alpha")
owner_personality_stability_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "owner_personality_stability", type = "alpha")
owner_personality_openness_reliability_kenlinn <- calculate_reliability(item_data_kenlinn, "owner_personality_openness", type = "alpha")


## DIAS ---------------------------------------------------
### Correlations ------------------------------------------------------------------
dias_corr_kenlinn <- cor.test(clean_data_kenlinn$max_distance, clean_data_kenlinn$dias_overall_score)
dias_corr_bf_kenlinn <- correlationBF(clean_data_kenlinn$max_distance, clean_data_kenlinn$dias_overall_score)

dias_reg_corr_kenlinn <- cor.test(clean_data_kenlinn$max_distance, clean_data_kenlinn$dias_behavioral_regulation_score)
dias_reg_corr_bf_kenlinn <- correlationBF(clean_data_kenlinn$max_distance, clean_data_kenlinn$dias_behavioral_regulation_score)

dias_agg_corr_kenlinn <- cor.test(clean_data_kenlinn$max_distance, clean_data_kenlinn$dias_aggression_score)
dias_agg_corr_bf_kenlinn <- correlationBF(clean_data_kenlinn$max_distance, clean_data_kenlinn$dias_aggression_score)

dias_resp_corr_kenlinn <- cor.test(clean_data_kenlinn$max_distance, clean_data_kenlinn$dias_responsiveness_score)
dias_resp_corr_bf_kenlinn <- correlationBF(clean_data_kenlinn$max_distance, clean_data_kenlinn$dias_responsiveness_score)

### Plots ------------------------------------------------------------------
dias_plot_kenlinn <- plot_correlation(clean_data_kenlinn, "dias_overall_score", "max_distance", "DIAS overall score", "Distance traveled (m)", title = FALSE, freq = TRUE)
dias_reg_plot_kenlinn <- plot_correlation(clean_data_kenlinn, "dias_behavioral_regulation_score", "max_distance", "DIAS behavioral regulation score", "Distance traveled (m)", title = FALSE, stats = TRUE, freq = TRUE)
dias_agg_plot_kenlinn <- plot_correlation(clean_data_kenlinn, "dias_aggression_score", "max_distance", "DIAS aggression score", "Distance traveled (m)", title = FALSE, stats = TRUE, freq = TRUE)
dias_resp_plot_kenlinn <- plot_correlation(clean_data_kenlinn, "dias_responsiveness_score", "max_distance", "DIAS responsiveness score", "Distance traveled (m)", title = FALSE, stats = TRUE, freq = TRUE)

### Combined plots ------------------------------------------------------------------
dias_plot_cchil + dias_plot_kenlinn + plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")
ggsave("figures/distance_dias_overall.png", width = 10, height = 5)

dias_reg_plot_cchil + dias_reg_plot_kenlinn + 
  dias_agg_plot_cchil + dias_agg_plot_kenlinn + 
  dias_resp_plot_cchil + dias_resp_plot_kenlinn + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")
ggsave("figures/distance_dias_subscales.png", width = 10, height = 15)


# All data ---------------------------------------------------
## Compare data sets ---------------------------------------------------

max_distance_ttest <- t.test(max_distance ~ experiment, data = all_data)
max_distance_ttest_bf <- ttestBF(formula = max_distance ~ experiment, data = all_data)
distance_study_plot <- plot_raincloud(all_data, "experiment", "max_distance", "Study", "Distance traveled (m)", freq = TRUE, title = FALSE, stats = TRUE)
dias_ttest <- t.test(dias_overall_score ~ experiment, data = all_data)
dias_ttest_bf <- ttestBF(formula = dias_overall_score ~ experiment, data = all_data)
dias_study_plot <- plot_raincloud(all_data, "experiment", "dias_overall_score", "Study", "DIAS overall score", binwidth = 0.005, ymin = 0.31, ymax = 0.72, freq = TRUE, title = FALSE, stats = TRUE)
# plot_raincloud(all_data, "experiment", "dog_weight", "Study", "Dog weight (kg)", binwidth = 0.2, ymin = 0, ymax = 80, freq = FALSE)
age_ttest <- t.test(dog_age ~ experiment, data = all_data)
age_ttest_bf <- ttestBF(formula = dog_age ~ experiment, data = all_data)
age_study_plot <- plot_raincloud(all_data, "experiment", "dog_age", "Study", "Dog age (years)", binwidth = 0.2, ymin = 0.75, ymax = 16.8, freq = TRUE, title = FALSE, stats = TRUE)


## Correlates ---------------------------------------------------
### DIAS ---------------------------------------------------
dias_corr_r <- cor(all_data$max_distance, all_data$dias_overall_score)
dias_corr_bf <- correlationBF(all_data$max_distance, all_data$dias_overall_score)
dias_reg_corr_r <- cor(all_data$max_distance, all_data$dias_behavioral_regulation_score)
dias_reg_corr_bf <- correlationBF(all_data$max_distance, all_data$dias_behavioral_regulation_score)
dias_agg_corr_r <- cor(all_data$max_distance, all_data$dias_aggression_score)
dias_agg_corr_bf <- correlationBF(all_data$max_distance, all_data$dias_aggression_score)
dias_resp_corr_r <- cor(all_data$max_distance, all_data$dias_responsiveness_score)
dias_resp_corr_bf <- correlationBF(all_data$max_distance, all_data$dias_responsiveness_score)

# Remove young dogs
age_data <- all_data %>% 
  filter(dog_age >= 2 & dog_age <= 10)
dias_age_corr_r <- cor(age_data$max_distance, age_data$dias_overall_score)
dias_age_corr_bf <- correlationBF(age_data$max_distance, age_data$dias_overall_score)

### Dog characteristics ---------------------------------------------------
dog_sex_plot <- plot_raincloud(all_data, "dog_sex", "max_distance", "Dog sex", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
# plot_raincloud(all_data, "dog_neutered", "max_distance", "Dog neuter status", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
dog_weight_plot <- plot_correlation(all_data, "dog_weight", "max_distance", "Dog weight (kg)", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
dog_age_plot <- plot_correlation(all_data, "dog_age", "max_distance", "Dog age (yrs)", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
dog_akc_plot <- plot_raincloud(all_data, "akccgc", "max_distance", "Dog CGC certified", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)

dog_sex_plot + dog_weight_plot + dog_age_plot + dog_akc_plot +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")
ggsave("figures/dog_characteristics.png", width = 10, height = 10)


### Dog behavior ---------------------------------------------------
disobedience_plot <- plot_correlation(all_data, "dog_behavior_bennett_disobedient_score", "max_distance", "Bennett disobedience score", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
aggression_plot <- plot_correlation(all_data, "dog_behavior_bennett_aggressive_score", "max_distance", "Bennett aggression score", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
destructive_plot <- plot_correlation(all_data, "dog_behavior_bennett_destructive_score", "max_distance", "Bennett destructiveness score", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
nervousness_plot <- plot_correlation(all_data, "dog_behavior_bennett_nervous_score", "max_distance", "Bennett nervousness score", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
excitable_plot <- plot_correlation(all_data, "dog_behavior_bennett_excitable_score", "max_distance", "Bennett excitability score", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
hiby_obedience_plot <- plot_correlation(all_data, "dog_obedience_hiby_score", "max_distance", "Hiby obedience score", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
training_plot <- plot_correlation(all_data, "rate_dog_trained", "max_distance", "Dog training rating", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
cbarq_training_plot <- plot_correlation(all_data, "cbarq_training_score", "max_distance", "CBARQ training score", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
sep_anxiety_plot <- plot_raincloud(all_data, "separation_anxiety_yesno", "max_distance", "Separation anxiety", "Distance traveled (m)", binwidth = 0.1, freq = FALSE, title = FALSE, stats = TRUE)

disobedience_plot + aggression_plot + destructive_plot + 
  nervousness_plot + excitable_plot + hiby_obedience_plot + 
  training_plot + cbarq_training_plot + sep_anxiety_plot +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")
ggsave("figures/dog_behavior.png", width = 10, height = 10)

### Owner characteristics ---------------------------------------------------
mdors_plot <- plot_correlation(all_data, "mdors_score", "max_distance", "MDORS score", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
extraversion_plot <- plot_correlation(all_data, "personality_extraversion_score", "max_distance", "Owner extraversion", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
aggreeable_plot <- plot_correlation(all_data, "personality_agreeableness_score", "max_distance", "Owner agreeableness", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
conscientious_plot <- plot_correlation(all_data, "personality_conscientiousness_score", "max_distance", "Owner conscientiousness", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
stability_plot <- plot_correlation(all_data, "personality_stability_score", "max_distance", "Owner stability", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
openness_plot <- plot_correlation(all_data, "personality_openness_score", "max_distance", "Owner openness", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
cognition_plot <- plot_correlation(all_data, "cognitive_ability", "max_distance", "Owner cognitive ability", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
otherdogs_plot <- plot_raincloud(all_data, "other_dogs", "max_distance", "Other dogs in household", "Distance traveled (m)", freq = FALSE, title = FALSE, stats = TRUE)
income_plot <- plot_raincloud(all_data, "household_income", "max_distance", "Household income", "Distance traveled (m)", rotate = TRUE, freq = FALSE, title = FALSE, stats = TRUE, rev = TRUE)

(mdors_plot + extraversion_plot + aggreeable_plot + 
  conscientious_plot + stability_plot + openness_plot +
  cognition_plot + otherdogs_plot) + #income_plot +
  # plot_spacer() + income_plot + plot_spacer() + 
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")
ggsave("figures/owner_characteristics.png", width = 10, height = 10)


## Study comparison --------------------------------------------------------

# Adult 1
brady_adult1_max_dist <- c(0.25, 1.5, 1.75, 1.75, 2, 2, 2.25, 2.5, 2.75, 3, 3, 3.25, 3.5, 3.5, 3.75, 4, 4.5, 4.5, 3.5, 5.75, 6.75, 7.5, 3.25, 1)
brady_adult1_dias_overall <- c(0.74, 0.53, 0.39, 0.67, 0.74, 0.42, 0.34, 0.52, 0.49, 0.6, 0.65, 0.63, 0.5, 0.49, 0.59, 0.47, 0.53, 0.49, 0.64, 0.41, 0.46, 0.45, 0.61, 0.54)
# Brady et al. ages
brady_adult1_ages <- c(24, 36, 96, 48, 36, 42, 76, 28, 96, 61, 48, 86, 42, 50, 52, 102, 26, 38, 41, 118, 108, 62, 71, 84)
mean(brady_adult1_max_dist)
sd(brady_adult1_max_dist)
mean(brady_adult1_dias_overall)
sd(brady_adult1_dias_overall)
brady_full_corr <- cor.test(brady_adult1_max_dist, brady_adult1_dias_overall)

brady_adult1_max_dist_trimmed <- brady_adult1_max_dist[-7]
brady_adult1_dias_overall_trimmed <- brady_adult1_dias_overall[-7]
brady_adult1_ages_trimmed <- brady_adult1_ages[-7]
mean(brady_adult1_max_dist_trimmed)
sd(brady_adult1_max_dist_trimmed)
mean(brady_adult1_dias_overall_trimmed)
sd(brady_adult1_dias_overall_trimmed)
brady_trimmed_corr <- cor.test(brady_adult1_max_dist_trimmed, brady_adult1_dias_overall_trimmed)


# Adult 2
brady_adult2_max_dist <- c(1, 0.75, 5, 1.5, 1.5, 0.75, 0.75, 1, 0.25, 3.75, 3.25, 0.5, 4.5)
brady_adult2_dias_overall <- c(0.56, 0.62, 0.43, 0.59, 0.49, 0.64, 0.49, 0.62, 0.49, 0.5, 0.5, 0.58, 0.43)
brady_adult2_ages <- c(108, 108, 84, 48, 36, 48, 72, 84, 60, 72, 96, 36, 48)
mean(brady_adult2_max_dist)
sd(brady_adult2_max_dist)
mean(brady_adult2_dias_overall)
sd(brady_adult2_dias_overall)
cor.test(brady_adult2_max_dist, brady_adult2_dias_overall)

brady_adult_ages <- c(brady_adult1_ages, brady_adult2_ages)

# Pup
brady_pup_max_dist <- c(2, 1.25, 0.25, 2.5, 5.25, 2.75, 7, 1.7, 3, 0, 2, 2.75, 1.5, 3.25, 5.5, 3.25, 4.75, 0, 0.25, 1.25, 0, 3, 5)
brady_pup_dias_overall <- c(0.59, 0.56, 0.59, 0.56, 0.52, 0.47, 0.61, 0.66, 0.44, 0.58, 0.46, 0.69, 0.46, 0.54, 0.53, 0.59, 0.57, 0.53, 0.59, 0.46, 0.55, 0.47, 0.52)
brady_pup_ages <- c(3,2,2,3,5,4,4,3,4,3,4,3,2,3,3,3,3,2,4,4,4,3,4)

# Compare our results to Brady et al.
brady_adult1_distance_ttest <- t.test(all_data$max_distance, brady_adult1_max_dist_trimmed)
brady_adult1_distance_ttest_bf <- ttestBF(all_data$max_distance, brady_adult1_max_dist_trimmed)

brady_adult2_distance_ttest <- t.test(all_data$max_distance, brady_adult2_max_dist)
brady_adult2_distance_ttest_bf <- ttestBF(all_data$max_distance, brady_adult2_max_dist)

brady_pup_distance_ttest <- t.test(all_data$max_distance, brady_pup_max_dist)
brady_pup_distance_ttest_bf <- ttestBF(all_data$max_distance, brady_pup_max_dist)

brady_adult1_dias_ttest <- t.test(all_data$dias_overall_score, brady_adult1_dias_overall_trimmed)
brady_adult1_dias_ttest_bf <- ttestBF(all_data$dias_overall_score, brady_adult1_dias_overall_trimmed)

brady_adult2_dias_ttest <- t.test(all_data$dias_overall_score, brady_adult2_dias_overall)
brady_adult2_dias_ttest_bf <- ttestBF(all_data$dias_overall_score, brady_adult2_dias_overall)

brady_pup_dias_ttest <- t.test(all_data$dias_overall_score, brady_pup_dias_overall)
brady_pup_dias_ttest_bf <- ttestBF(all_data$dias_overall_score, brady_pup_dias_overall)

all_max_distance <- c(clean_data_cchil$max_distance, clean_data_kenlinn$max_distance, brady_adult1_max_dist_trimmed, brady_adult2_max_dist, brady_pup_max_dist)
all_dias_overall <- c(clean_data_cchil$dias_overall_score, clean_data_kenlinn$dias_overall_score, brady_adult1_dias_overall_trimmed, brady_adult2_dias_overall, brady_pup_dias_overall)
all_age <- c(clean_data_cchil$dog_age, clean_data_kenlinn$dog_age, brady_adult1_ages_trimmed / 12, brady_adult2_ages / 12, brady_pup_ages / 12)

study_compare <- bind_cols(max_distance = all_max_distance, dias = all_dias_overall, age = all_age) %>% 
  mutate(study = c(rep(paste0("Current Study 1 (N = ", length(clean_data_cchil$max_distance), ")"), length(clean_data_cchil$max_distance)), 
                   rep(paste0("Current Study 2 (N = ", length(clean_data_kenlinn$max_distance), ")"), length(clean_data_kenlinn$max_distance)), 
                   rep(paste0("Brady et al. (2018) Lab Study 1 (N = ", length(brady_adult1_max_dist_trimmed), ")"), length(brady_adult1_max_dist_trimmed)), 
                   rep(paste0("Brady et al. (2018) Field Study 1 (N = ", length(brady_adult2_max_dist), ")"), length(brady_adult2_max_dist)),
                   rep(paste0("Brady et al. (2018) Lab Study 2 (N = ", length(brady_pup_max_dist), ")"), length(brady_pup_max_dist))),
                   .before = 1) %>% 
  mutate(study = fct_relevel(study, paste0("Current Study 2 (N = ", length(clean_data_kenlinn$max_distance), ")"), paste0("Current Study 1 (N = ", length(clean_data_cchil$max_distance), ")"), paste0("Brady et al. (2018) Lab Study 2 (N = ", length(brady_pup_max_dist), ")"), paste0("Brady et al. (2018) Field Study 1 (N = ", length(brady_adult2_max_dist), ")"), paste0("Brady et al. (2018) Lab Study 1 (N = ", length(brady_adult1_max_dist_trimmed), ")")))

max_dist_plot <- study_compare %>% 
  ggplot(aes(x = study, y = max_distance, color = study, fill = study)) +
  stat_dots(side = "left", alpha = 0.5, binwidth = 0.09, justification = 1.05) +  # plot individual points
  stat_slab(alpha = 0.5, scale = 0.6) +
  geom_boxplot(position = position_nudge(x = 0.1, y = 0), width = 0.2, outlier.shape = NA, alpha = 0.5, color = "black") +
  stat_summary(fun.data = "mean_cl_boot", position = position_nudge(x = -0.1, y = 0), color = "black") +
  scale_color_manual(values = cb_palette_black[c(5, 3, 1, 6, 7)]) + # set group color
  scale_fill_manual(values = cb_palette_black[c(5, 3, 1, 6, 7)]) + # set group color
  labs(x = "", y = "Distanced traveled (m)") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(family = "arial"),
        legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 15)) +
  ggtitle("(a)") +
  NULL

dias_plot <- study_compare %>% 
  ggplot(aes(x = study, y = dias, color = study, fill = study)) +
  stat_dots(side = "left", alpha = 0.5, binwidth = 0.005, justification = 1.05) +  # plot individual points
  stat_slab(alpha = 0.5, scale = 0.6) +
  geom_boxplot(position = position_nudge(x = 0.1, y = 0), width = 0.2, outlier.shape = NA, alpha = 0.5, color = "black") +
  stat_summary(fun.data = "mean_cl_boot", position = position_nudge(x = -0.1, y = 0), color = "black") +
  scale_color_manual(values = cb_palette_black[c(5, 3, 1, 6, 7)]) + # set group color
  scale_fill_manual(values = cb_palette_black[c(5, 3, 1, 6, 7)]) + # set group color
  labs(x = "", y = "DIAS overall score") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(family = "arial"),
        legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank()) +
  ggtitle("(b)") +
  NULL

age_plot <- study_compare %>% 
  ggplot(aes(x = study, y = age, color = study, fill = study)) +
  stat_dots(side = "left", alpha = 0.5, binwidth = 0.2, justification = 1.05) +  # plot individual points
  stat_slab(alpha = 0.5, scale = 0.6) +
  geom_boxplot(position = position_nudge(x = 0.1, y = 0), width = 0.2, outlier.shape = NA, alpha = 0.5, color = "black") +
  stat_summary(fun.data = "mean_cl_boot", position = position_nudge(x = -0.1, y = 0), color = "black") +
  scale_color_manual(values = cb_palette_black[c(5, 3, 1, 6, 7)]) + # set group color
  scale_fill_manual(values = cb_palette_black[c(5, 3, 1, 6, 7)]) + # set group color
  labs(x = "", y = "Dog age (years)") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(family = "arial"),
        legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank()) +
  ggtitle("(c)") +
  NULL

max_dist_plot + dias_plot + age_plot
ggsave("figures/study_comparison.png", width = 10, height = 4, scale = 1.2)


## Meta-analysis -----------------------------------------------------------

meta_data <- tibble(study = c("Brady et al. (2018) Lab Study 1", "Brady et al. (2018) Field Study 1", "Brady et al. (2018) Lab Study 2", "Mongillo et al. (2019)", "Current Study 1", "Current Study 2"), r = c(-0.46, -0.61, -0.053, 0.01, -0.10, 0.04), n = c(23, 13, 24, 48, nrow(clean_data_cchil), nrow(clean_data_kenlinn))) %>%
  mutate(z = transform_es(.$r, from = "r", to = "z"),
         SE = 1 / sqrt(n - 3),
         z_lower = z - qnorm(1.95 / 2) * SE,
         z_upper = z + qnorm(1.95 / 2) * SE) %>% 
  mutate(r_lower = transform_es(.$z_lower, from = "z", to = "r"),
         r_upper = transform_es(.$z_upper, from = "z", to = "r"))
(meta_z_avg <- meta_bma(y = z, SE = SE, labels = study, data = meta_data, iter = 2000, 
                            d = metaBMA::prior("cauchy", c(location = 0, scale = 0.354)), 
                            tau = metaBMA::prior("invgamma", c(shape = 1, scale = 0.075))))
z_bf <- meta_z_avg$inclusion$incl.BF
tau_bf <- (meta_z_avg$inclusion$posterior[3] + meta_z_avg$inclusion$posterior[4]) / (meta_z_avg$inclusion$posterior[1] + meta_z_avg$inclusion$posterior[2])

meta_data <- meta_data %>% 
  bind_rows(tibble(study = c("", "Averaged"), 
                   r = c(NA, as.numeric(transform_es(meta_z_avg$estimates[1], from = "z", to = "r"))), 
                   n = c(NA, sum(meta_z_avg$data$data$n)), 
                   z = c(NA, meta_z_avg$estimates[1]),
                   SE = c(NA, NA),
                   z_lower = c(NA, meta_z_avg$estimates[7]),
                   z_upper = c(NA, meta_z_avg$estimates[13]),
                   r_lower = c(NA, as.numeric(transform_es(meta_z_avg$estimates[7], from = "z", to = "r"))),
                   r_upper = c(NA, as.numeric(transform_es(meta_z_avg$estimates[13], from = "z", to = "r")))))

forestplot_bf <- meta_data %>% 
  mutate(study = fct_relevel(study, "Averaged", "", "Current Study 2", "Current Study 1", "Mongillo et al. (2019)", "Brady et al. (2018) Lab Study 2", "Brady et al. (2018) Field Study 1", "Brady et al. (2018) Lab Study 1")) %>% 
  ggplot(aes(x = study, y = r)) +
  geom_hline(yintercept = 0, color = "grey20", linetype = 2) +
  geom_pointrange(aes(ymin = r_lower, ymax = r_upper), size = c(rep(1, 7),2), shape = c(rep(21, 7), 23), fill = c("black", "black", rep("grey", 5), "black")) +
  coord_flip() +
  labs(x = "", y = "Effect") +
  ylim(-0.95, 0.45) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(),
        text = element_text(size = 20, family = "Arial"))
ggsave("figures/forestplot_bf.png", width = 9, height = 4)


## Reliability scores ------------------------------------------------------
scales <- c("Bennett and Rohlf disobedience", "Bennett and Rohlf aggression", "Bennett and Rohlf nervousness", "Bennett and Rohlf destructiveness", "Bennett and Rohlf excitability", "Hiby et al. obedience", "Hiby et al. problem behaviors", "DIAS overall", "DIAS behavioral regulation", "DIAS aggression", "DIAS responsiveness", "MDORS", "Owner extraversion*", "Owner agreeableness*", "Owner conscientiousness*", "Owner stability*", "Owner openness*", "Cognitive reflection task", "Berlin numeracy test", "CBARQ training")
study1_reliability <- c(print_reliability(dog_behavior_disobedient_reliability_cchil), 
                        print_reliability(dog_behavior_aggressive_reliability_cchil),
                        print_reliability(dog_behavior_nervous_reliability_cchil),
                        print_reliability(dog_behavior_destructive_reliability_cchil),
                        print_reliability(dog_behavior_excitable_reliability_cchil),
                        print_reliability(dog_obedience_reliability_cchil),
                        print_reliability(dog_problem_behaviors_reliability_cchil),
                        print_reliability(dias_reliability_cchil),
                        print_reliability(dias_behavioral_regulation_reliability_cchil),
                        print_reliability(dias_aggression_reliability_cchil),
                        print_reliability(dias_responsiveness_reliability_cchil),
                        print_reliability(mdors_reliability_cchil),
                        print_reliability(owner_personality_extraversion_reliability_cchil, type = "alpha"),
                        print_reliability(owner_personality_agreeableness_reliability_cchil, type = "alpha"),
                        print_reliability(owner_personality_conscientiousness_reliability_cchil, type = "alpha"),
                        print_reliability(owner_personality_stability_reliability_cchil, type = "alpha"),
                        print_reliability(owner_personality_openness_reliability_cchil, type = "alpha"),
                        print_reliability(crt_reliability_cchil),
                        print_reliability(numeracy_reliability_cchil),
                        NA
)
study2_reliability <- c(NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        print_reliability(dog_obedience_reliability_kenlinn),
                        NA,
                        print_reliability(dias_reliability_kenlinn),
                        print_reliability(dias_behavioral_regulation_reliability_kenlinn),
                        print_reliability(dias_aggression_reliability_kenlinn),
                        print_reliability(dias_responsiveness_reliability_kenlinn),
                        NA,
                        print_reliability(owner_personality_extraversion_reliability_kenlinn, type = "alpha"),
                        print_reliability(owner_personality_agreeableness_reliability_kenlinn, type = "alpha"),
                        print_reliability(owner_personality_conscientiousness_reliability_kenlinn, type = "alpha"),
                        print_reliability(owner_personality_stability_reliability_kenlinn, type = "alpha"),
                        print_reliability(owner_personality_openness_reliability_kenlinn, type = "alpha"),
                        NA,
                        NA,
                        print_reliability(dog_obedience_reliability_kenlinn)
)
reliability_table <- data.frame(scales, study1_reliability, study2_reliability)


## Demographics ------------------------------------------------------------

demo_labels <- c("", "Female", "Male", "Nonbinary", "Single", "Married", "Separated/divorced", "Widowed", "Yes", "No", "<$25,000", "$25,001-$50,000", "$50,001-$75,000", "$75,001-$100,000", ">$100,000", "Prefer not to answer")
demo_cchil <- c(
  paste("Study 1 (N=", nrow(clean_data_cchil), ")", sep = ""),
  paste(unname(owner_gender_cchil["Female"]), " (", printnum(owner_gender_cchil["Female"] / sum(owner_gender_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_gender_cchil["Male"]), " (", printnum(owner_gender_cchil["Male"] / sum(owner_gender_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_gender_cchil["Other"]), " (", printnum(owner_gender_cchil["Other"] / sum(owner_gender_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_marital_cchil["Single"]), " (", printnum(owner_marital_cchil["Single"] / sum(owner_marital_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_marital_cchil["Married"]), " (", printnum(owner_marital_cchil["Married"] / sum(owner_marital_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_marital_cchil["Separated/ divorced"]), " (", printnum(owner_marital_cchil["Separated/ divorced"] / sum(owner_marital_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_marital_cchil["Widowed"]), " (", printnum(owner_marital_cchil["Widowed"] / sum(owner_marital_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_otherdogs_cchil["Yes"]), " (", printnum(owner_otherdogs_cchil["Yes"] / sum(owner_otherdogs_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_otherdogs_cchil["No"]), " (", printnum(owner_otherdogs_cchil["No"] / sum(owner_otherdogs_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_cchil["Less than $25,000"]), " (", printnum(owner_income_cchil["Less than $25,000"] / sum(owner_income_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_cchil["$25,000-$49,999"]), " (", printnum(owner_income_cchil["$25,000-$49,999"] / sum(owner_income_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_cchil["$50,000-$74,999"]), " (", printnum(owner_income_cchil["$50,000-$74,999"] / sum(owner_income_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_cchil["$75,000-$99,999"]), " (", printnum(owner_income_cchil["$75,000-$99,999"] / sum(owner_income_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_cchil["More than $100,000"]), " (", printnum(owner_income_cchil["More than $100,000"] / sum(owner_income_cchil) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_cchil["I would rather not say"]), " (", printnum(owner_income_cchil["I would rather not say"] / sum(owner_income_cchil) * 100, digits = 1), "%)", sep = "")
)
demo_kenlinn <- c(
  paste("Study 2 (N=", nrow(clean_data_kenlinn), ")", sep = ""),
  paste(unname(owner_gender_kenlinn["Female"]), " (", printnum(owner_gender_kenlinn["Female"] / sum(owner_gender_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_gender_kenlinn["Male"]), " (", printnum(owner_gender_kenlinn["Male"] / sum(owner_gender_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_gender_kenlinn["Other"]), " (", printnum(owner_gender_kenlinn["Other"] / sum(owner_gender_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_marital_kenlinn["Single"]), " (", printnum(owner_marital_kenlinn["Single"] / sum(owner_marital_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_marital_kenlinn["Married"]), " (", printnum(owner_marital_kenlinn["Married"] / sum(owner_marital_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_marital_kenlinn["Separated/ divorced"]), " (", printnum(owner_marital_kenlinn["Separated/ divorced"] / sum(owner_marital_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_marital_kenlinn["Widowed"]), " (", printnum(owner_marital_kenlinn["Widowed"] / sum(owner_marital_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_otherdogs_kenlinn["Yes"]), " (", printnum(owner_otherdogs_kenlinn["Yes"] / sum(owner_otherdogs_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_otherdogs_kenlinn["No"]), " (", printnum(owner_otherdogs_kenlinn["No"] / sum(owner_otherdogs_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_kenlinn["Less than $25,000"]), " (", printnum(owner_income_kenlinn["Less than $25,000"] / sum(owner_income_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_kenlinn["$25,000-$49,999"]), " (", printnum(owner_income_kenlinn["$25,000-$49,999"] / sum(owner_income_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_kenlinn["$50,000-$74,999"]), " (", printnum(owner_income_kenlinn["$50,000-$74,999"] / sum(owner_income_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_kenlinn["$75,000-$99,999"]), " (", printnum(owner_income_kenlinn["$75,000-$99,999"] / sum(owner_income_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_kenlinn["More than $100,000"]), " (", printnum(owner_income_kenlinn["More than $100,000"] / sum(owner_income_kenlinn) * 100, digits = 1), "%)", sep = ""),
  paste(unname(owner_income_kenlinn["I would rather not say"]), " (", printnum(owner_income_kenlinn["I would rather not say"] / sum(owner_income_kenlinn) * 100, digits = 1), "%)", sep = "")
)
demo_table <- bind_cols(labels = demo_labels, cchil = demo_cchil, kenlinn = demo_kenlinn) %>% 
  mutate(cchil = str_replace_all(cchil, "NA", "0"),
         kenlinn = str_replace_all(kenlinn, "NA", "0"))


# save.image("dog_spatial_workspace.RData")
