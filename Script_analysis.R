# ==============================================================================
# SCRIPT 1 OF 2 — STATISTICAL ANALYSIS ONLY
# Author: Carmelo Cavallaro "Il_Nulla_Cosmico" Alias "Battle_Horse"
#
# PURPOSE:
#   Runs all statistical tests for every response variable and saves all
#   results into a single file: "analysis_results.RDS"
#   This script produces NO plots.
#   Open script_graph.R for all plotting.
#
# WORKFLOW:
#   1. Fill in STEP 0 with your dataset and variable names.
#   2. Run this script once.
#   3. Open script_grafici.R to create your plots.
# ==============================================================================


# ==============================================================================
# --- STEP 0: DATA INPUT ---
# ==============================================================================

library(dplyr)
library(emmeans)
library(car)
library(multcompView)

original_dataset   <- ____________________   # Name of the imported dataset
factor_column_name <- "_____"     # Name of the X column (Groups/Treatments)

# List ALL your response variables (Y) here:
measure_column_names <- c(
  "__________________ "
)


# ==============================================================================
# --- HELPER FUNCTION: PRETTY LABELS ---
# Converts "Peso_secco_foglie" → "Peso Secco Foglie"
# ==============================================================================

pretty_name <- function(x) {
  x <- gsub("_", " ", x)
  x <- tools::toTitleCase(x)
  return(x)
}


# ==============================================================================
# --- FOR LOOP: ANALYSES ---
# ==============================================================================

all_results <- list()

for (measure_column_name in measure_column_names) {

  cat("\n##############################################################\n")
  cat(paste0("# ANALYSING: ", toupper(measure_column_name), "\n"))
  cat("##############################################################\n\n")

  # --- DATA PREPARATION ---
  dataset <- original_dataset %>%
    rename(Treatment   = all_of(factor_column_name),
           Measurement = all_of(measure_column_name))

  dataset$Treatment <- factor(dataset$Treatment, levels = unique(dataset$Treatment))
  levels(dataset$Treatment) <- pretty_name(levels(dataset$Treatment))

  # --- ANOVA MODEL ---
  anova_model <- aov(Measurement ~ Treatment, data = dataset)

  # --- NORMALITY ---
  shapiro_result <- shapiro.test(residuals(anova_model))
  cat("--- SHAPIRO-WILK ---\n"); print(shapiro_result)

  # --- HOMOSCEDASTICITY ---
  bartlett_result <- bartlett.test(Measurement ~ Treatment, data = dataset)
  cat("\n--- BARTLETT ---\n"); print(bartlett_result)

  levene_result <- leveneTest(Measurement ~ Treatment, data = dataset)
  cat("\n--- LEVENE ---\n"); print(levene_result)

  # --- AUTOMATED DECISION ---
  p_shapiro  <- shapiro_result$p.value
  p_bartlett <- bartlett_result$p.value
  p_levene   <- levene_result$`Pr(>F)`[1]

  if (p_shapiro > 0.05) { p_homo <- p_bartlett } else { p_homo <- p_levene }

  if      (p_shapiro >  0.05 & p_homo >  0.05) { decision <- "OK"          }
  else if (p_shapiro <= 0.05 & p_homo >  0.05) { decision <- "NOT_NORMAL"  }
  else if (p_shapiro >  0.05 & p_homo <= 0.05) { decision <- "UNEQUAL_VAR" }
  else                                          { decision <- "BOTH_FAILED" }

  cat("\n==> DECISION:", decision, "\n\n")

  # --- ANOVA + POST-HOC ---
  cat("--- ANOVA ---\n"); print(summary(anova_model))

  tukey_emmeans <- emmeans(anova_model, specs = pairwise ~ Treatment, adjust = "tukey")
  cat("\n--- TUKEY ---\n"); print(summary(tukey_emmeans$contrasts))

  bonf_test <- emmeans(anova_model, specs = pairwise ~ Treatment, adjust = "bonferroni")
  cat("\n--- BONFERRONI ---\n"); print(summary(bonf_test$contrasts))

  # Welch's ANOVA — run automatically only when variances are unequal
  welch_result <- NULL
  if (decision == "UNEQUAL_VAR") {
    cat("\n--- WELCH'S ANOVA ---\n")
    welch_result <- oneway.test(Measurement ~ Treatment, data = dataset, var.equal = FALSE)
    print(welch_result)
  }

  # --- SIGNIFICANCE LETTERS ---
  tukey_raw    <- TukeyHSD(anova_model)
  letters_auto <- multcompLetters4(anova_model, tukey_raw)

  letters_df <- data.frame(
    Treatment = names(letters_auto$Treatment$Letters),
    letter    = letters_auto$Treatment$Letters
  )
  letters_df$Treatment <- factor(letters_df$Treatment,
                                 levels = levels(dataset$Treatment))

  cat("\n--- SIGNIFICANCE LETTERS ---\n"); print(letters_df)

  # --- DESCRIPTIVE STATISTICS ---
  df_summary <- dataset %>%
    group_by(Treatment) %>%
    summarise(
      mean_val = mean(Measurement, na.rm = TRUE),
      sd_val   = sd(Measurement,   na.rm = TRUE),
      n_obs    = n(),
      se_val   = sd_val / sqrt(n_obs),
      .groups  = "drop"
    ) %>%
    left_join(letters_df, by = "Treatment")

  # --- STORE EVERYTHING ---
  all_results[[measure_column_name]] <- list(
    dataset    = dataset,       # Full dataset for this variable (Treatment + Measurement)
    df_summary = df_summary,    # Means, SD, SE, significance letters — ready for plots
    letters_df = letters_df,    # Significance letters table
    decision   = decision,      # "OK" | "NOT_NORMAL" | "UNEQUAL_VAR" | "BOTH_FAILED"
    p_shapiro  = p_shapiro,     # p-value Shapiro-Wilk
    p_homo     = p_homo,        # p-value Bartlett (if normal) or Levene (if not)
    label_y    = pretty_name(measure_column_name),  # Clean Y axis label
    label_x    = pretty_name(factor_column_name)    # Clean X axis label
  )

  cat(paste0("\n>>> STORED: ", measure_column_name, " <<<\n"))

} # END FOR LOOP


# ==============================================================================
# --- SAVE RESULTS ---
# ==============================================================================

saveRDS(all_results, file = "analysis_results.RDS")

cat("\n##############################################################\n")
cat("# ALL ANALYSES COMPLETE\n")
cat("# Results saved to: analysis_results.RDS\n")
cat("# Open script_grafici.R to create your plots.\n")
cat("##############################################################\n")


# ==============================================================================
# --- ASSUMPTIONS SUMMARY TABLE ---
# Quick overview of which variables passed or failed the statistical assumptions.
# Use this to decide which variables are safe to plot as standard ANOVA results.
#
# DECISION codes:
#   OK           → All assumptions met. Standard ANOVA results are valid.
#   NOT_NORMAL   → Normality failed. Consider Kruskal-Wallis or log-transformation.
#   UNEQUAL_VAR  → Homoscedasticity failed. Welch's ANOVA was run automatically.
#   BOTH_FAILED  → Both assumptions failed. Kruskal-Wallis strongly recommended.
# ==============================================================================

cat("\n--- ASSUMPTIONS SUMMARY ---\n\n")
summary_table <- data.frame(
  Variable   = names(all_results),
  p_Shapiro  = round(sapply(all_results, function(x) x$p_shapiro), 4),
  p_Homo     = round(sapply(all_results, function(x) x$p_homo),    4),
  Decision   = sapply(all_results, function(x) x$decision)
)
print(summary_table, row.names = FALSE)
