# ==============================================================================
# SCRIPT 2 OF 2 — PLOTS
# Author: Carmelo Cavallaro "Il_Nulla_Cosmico" Alias "Battle_Horse"
#
# PURPOSE:
#   Loads the results produced by script_analisi.R and lets you create,
#   customize, and save plots for any response variable — one at a time,
#   with full control over every visual parameter.
#
# WORKFLOW:
#   1. Run script_analisi.R first (only needed once, or when data changes).
#   2. Run STEP 0 of this script to load the results.
#   3. Run STEP 1 to see which variables are available and their status.
#   4. Use plot_variable() in STEP 2 to create and customize your plots.
# ==============================================================================


# ==============================================================================
# --- STEP 0: LOAD RESULTS ---
# Run this block once at the start of every plotting session.
# ==============================================================================

library(ggplot2)
library(dplyr)

all_results <- readRDS("analysis_results.RDS")


# ==============================================================================
# --- STEP 1: SEE AVAILABLE VARIABLES AND THEIR STATISTICAL STATUS ---
#
# Run this to get a summary table before plotting.
# Use the Decision column to decide what to plot.
#
# Decision codes:
#   OK           → All assumptions met. Safe to plot as standard ANOVA.
#   NOT_NORMAL   → Normality failed. Interpret results with caution.
#   UNEQUAL_VAR  → Homoscedasticity failed. Welch's ANOVA was used.
#   BOTH_FAILED  → Both assumptions failed. Results unreliable.
# ==============================================================================

cat("=== AVAILABLE VARIABLES ===\n\n")
summary_table <- data.frame(
  Variable = names(all_results),
  Decision = sapply(all_results, function(x) x$decision)
)
print(summary_table, row.names = FALSE)
cat("\nVariable names to use in plot_variable():\n")
cat(paste0('  "', names(all_results), '"'), sep = "\n")


# ==============================================================================
# --- CORE FUNCTION: plot_variable() ---
# ==============================================================================
#
# PARAMETERS:
#
#   variable      [required] — Name of the response variable, exactly as shown
#                              in the summary table above.
#                              Example: "Peso_secco_foglie"
#
#   type          [required] — Type of plot to create. One of:
#                              "barplot" | "boxplot" | "dotplot" | "violinplot"
#
#   colors        [optional] — Character vector of HEX colors, one per group.
#                              Default: greens palette.
#                              Example: c("#E06C75", "#61AFEF", "#98C379")
#
#   title         [optional] — Custom plot title.
#                              Default: auto-generated from variable name.
#                              Example: "Effect of irrigation on dry weight"
#
#   x_label       [optional] — Custom X axis label.
#                              Default: auto-generated from factor column name.
#
#   y_label       [optional] — Custom Y axis label.
#                              Default: auto-generated from variable name.
#
#   caption       [optional] — Custom caption text shown below the plot.
#                              Default: "Different letters indicate p < 0.05 (Tukey HSD)"
#                              Set to "" to remove the caption entirely.
#
#   letters       [optional] — Character vector to override automatic significance
#                              letters. One value per group, in the same order as
#                              the groups appear on the X axis.
#                              Default: letters computed automatically by Tukey HSD.
#                              Example: c("a", "ab", "b")
#
#   show_letters  [optional] — TRUE/FALSE. Whether to show significance letters
#                              on the barplot. Default: TRUE.
#                              (Letters only apply to barplot type.)
#
#   letter_size   [optional] — Font size of significance letters. Default: 5.
#
#   bar_width     [optional] — Width of bars in barplot. Default: 0.5.
#
#   errorbar_width[optional] — Width of error bar caps. Default: 0.2.
#
#   point_size    [optional] — Size of individual points in dotplot. Default: 2.
#
#   jitter_width  [optional] — Horizontal spread of points in dotplot. Default: 0.2.
#
#   alpha         [optional] — Transparency of fill colors (0 = invisible, 1 = solid).
#                              Default: 0.7 for boxplot, 0.6 for violin, 1 for barplot.
#
#   outlier_color [optional] — Color of outlier points in boxplot. Default: "red".
#
#   x_angle       [optional] — Rotation angle of X axis tick labels. Default: 0.
#                              Use 45 when group names are long.
#
#   base_size     [optional] — Base font size for all text in the plot. Default: 12.
#
#   title_size    [optional] — Font size of the plot title. Default: 13.
#
#   theme         [optional] — ggplot2 theme to apply. One of:
#                              "classic" | "minimal" | "light" | "bw"
#                              Default: "classic" for barplot/boxplot,
#                                       "light" for dotplot,
#                                       "minimal" for violinplot.
#
#   save          [optional] — How to save the plot. One or more of:
#                              "screen" | "png" | "pdf"
#                              Default: "screen"
#                              Example: c("screen", "png")
#                              Example: c("png", "pdf")
#
#   filename      [optional] — Base name for saved files (without extension).
#                              Default: auto-generated as "variable_type".
#                              Example: "figura1_peso_secco"
#
#   width         [optional] — Width of saved file in inches. Default: 8.
#   height        [optional] — Height of saved file in inches. Default: 6.
#   dpi           [optional] — Resolution of PNG file. Default: 300.
#
#   warn_if_failed[optional] — TRUE/FALSE. If TRUE, prints a warning when the
#                              variable did not pass statistical assumptions.
#                              The plot is always produced regardless. Default: TRUE.
#
# ==============================================================================

plot_variable <- function(
    variable,
    type,
    colors         = c("#B4EEB4", "#9BCD9B", "#698B69"),
    title          = NULL,
    x_label        = NULL,
    y_label        = NULL,
    caption        = "Different letters indicate p < 0.05 (Tukey HSD)",
    letters        = NULL,
    show_letters   = TRUE,
    letter_size    = 5,
    bar_width      = 0.5,
    errorbar_width = 0.2,
    point_size     = 2,
    jitter_width   = 0.2,
    alpha          = NULL,
    outlier_color  = "red",
    x_angle        = 0,
    base_size      = 12,
    title_size     = 13,
    theme          = NULL,
    save           = "screen",
    filename       = NULL,
    width          = 8,
    height         = 6,
    dpi            = 300,
    warn_if_failed = TRUE
) {
  
  # --- VALIDATION ---
  if (!variable %in% names(all_results)) {
    stop(paste0(
      'Variable "', variable, '" not found.\n',
      'Available variables:\n  ',
      paste(names(all_results), collapse = "\n  ")
    ))
  }
  
  valid_types <- c("barplot", "boxplot", "dotplot", "violinplot")
  if (!type %in% valid_types) {
    stop(paste0('Invalid type "', type, '". Choose one of: ',
                paste(valid_types, collapse = ", ")))
  }
  
  # --- LOAD DATA FOR THIS VARIABLE ---
  res        <- all_results[[variable]]
  dataset    <- res$dataset
  df_summary <- res$df_summary
  decision   <- res$decision
  
  # --- STATISTICAL WARNING ---
  if (warn_if_failed && decision != "OK") {
    msg <- switch(decision,
                  NOT_NORMAL   = "Normality assumption failed (Shapiro-Wilk p < 0.05). Results should be interpreted with caution.",
                  UNEQUAL_VAR  = "Homoscedasticity assumption failed. Welch's ANOVA was used instead of standard ANOVA.",
                  BOTH_FAILED  = "BOTH normality and homoscedasticity assumptions failed. Consider non-parametric alternatives."
    )
    warning(paste0("\n[", variable, "] ", msg), call. = FALSE)
  }
  
  # --- LABELS ---
  label_y <- if (!is.null(y_label)) y_label else res$label_y
  label_x <- if (!is.null(x_label)) x_label else res$label_x
  label_t <- if (!is.null(title))   title    else paste("Effect of", label_x, "on", label_y)
  
  # --- SIGNIFICANCE LETTERS ---
  if (!is.null(letters)) {
    if (length(letters) != nlevels(dataset$Treatment)) {
      stop(paste0("You provided ", length(letters), " letter(s) but there are ",
                  nlevels(dataset$Treatment), " groups."))
    }
    df_summary$letter <- letters
  }
  
  # --- COLORS ---
  if (length(colors) != nlevels(dataset$Treatment)) {
    stop(paste0("You provided ", length(colors), " color(s) but there are ",
                nlevels(dataset$Treatment), " groups."))
  }
  
  # --- THEME SELECTION ---
  get_theme <- function(default) {
    t <- if (!is.null(theme)) theme else default
    switch(t,
           "classic" = theme_classic(base_size = base_size),
           "minimal" = theme_minimal(base_size = base_size),
           "light"   = theme_light(base_size   = base_size),
           "bw"      = theme_bw(base_size      = base_size),
           theme_classic(base_size = base_size)
    )
  }
  
  # --- X AXIS TEXT ---
  x_axis_theme <- theme(
    axis.text.x  = element_text(angle = x_angle,
                                hjust = if (x_angle > 0) 1 else 0.5),
    plot.title   = element_text(hjust = 0.5, face = "bold", size = title_size),
    axis.title   = element_text(face = "bold"),
    plot.margin  = margin(t = 10, r = 15, b = 10, l = 10)
  )
  
  # --- BUILD PLOT ---
  p <- switch(type,
              
              # ---- BAR PLOT ----
              "barplot" = {
                letter_offset <- 0.05 * max(df_summary$mean_val, na.rm = TRUE)
                p <- ggplot(df_summary, aes(x = Treatment, y = mean_val, fill = Treatment)) +
                  geom_bar(stat = "identity", color = "black",
                           width = bar_width, show.legend = FALSE) +
                  geom_errorbar(aes(ymin = mean_val - se_val,
                                    ymax = mean_val + se_val),
                                width = errorbar_width)
                if (show_letters) {
                  p <- p + geom_text(aes(label = letter,
                                         y = mean_val + se_val + letter_offset),
                                     vjust = -0.2, size = letter_size, fontface = "bold")
                }
                p +
                  scale_fill_manual(values = colors) +
                  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
                  labs(title = label_t, x = label_x,
                       y = paste("Mean", label_y), caption = caption) +
                  get_theme("classic") + x_axis_theme
              },
              
              # ---- BOXPLOT ----
              "boxplot" = {
                a <- if (!is.null(alpha)) alpha else 0.7
                ggplot(dataset, aes(x = Treatment, y = Measurement, fill = Treatment)) +
                  geom_boxplot(alpha = a, outlier.colour = outlier_color) +
                  scale_fill_manual(values = colors) +
                  labs(title = paste("Boxplot:", label_y), x = label_x, y = label_y) +
                  get_theme("classic") + x_axis_theme
              },
              
              # ---- DOT PLOT ----
              "dotplot" = {
                a <- if (!is.null(alpha)) alpha else 0.7
                ggplot(dataset, aes(x = Treatment, y = Measurement, color = Treatment)) +
                  geom_jitter(width = jitter_width, size = point_size, alpha = a) +
                  stat_summary(fun = mean, geom = "point",
                               shape = 18, size = point_size * 2, color = "black") +
                  scale_color_manual(values = colors) +
                  labs(title = paste("Dot Plot:", label_y), x = label_x, y = label_y) +
                  get_theme("light") + x_axis_theme
              },
              
              # ---- VIOLIN PLOT ----
              "violinplot" = {
                a <- if (!is.null(alpha)) alpha else 0.6
                ggplot(dataset, aes(x = Treatment, y = Measurement, fill = Treatment)) +
                  geom_violin(trim = FALSE, alpha = a) +
                  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
                  scale_fill_manual(values = colors) +
                  labs(title = paste("Violin Plot:", label_y), x = label_x, y = label_y) +
                  get_theme("minimal") + x_axis_theme
              }
  )
  
  # --- SAVE / DISPLAY ---
  fname <- if (!is.null(filename)) filename else paste0(variable, "_", type)
  
  if ("screen" %in% save) print(p)
  if ("png"    %in% save) {
    f <- paste0(fname, ".png")
    ggsave(f, plot = p, width = width, height = height, dpi = dpi)
    cat("Saved:", f, "\n")
  }
  if ("pdf" %in% save) {
    f <- paste0(fname, ".pdf")
    ggsave(f, plot = p, width = width, height = height)
    cat("Saved:", f, "\n")
  }
  
  invisible(p)   # Returns the plot object silently so you can still use + to modify it
}


# ==============================================================================
# --- STEP 2: CREATE YOUR PLOTS ---
# Copy, paste, and edit any of the examples below.
# ==============================================================================

# --- MINIMAL USAGE (only required parameters) ---
# plot_variable("Peso_secco_foglie", type = "barplot")

# --- CHANGE COLORS ---
# plot_variable("Peso_secco_foglie",
#               type   = "barplot",
#               colors = c("#E06C75", "#61AFEF", "#98C379"))

# --- CUSTOM TITLE AND LABELS ---
# plot_variable("Peso_secco_foglie",
#               type    = "barplot",
#               title   = "Effect of irrigation on leaf dry weight",
#               x_label = "Irrigation regime",
#               y_label = "Dry weight (g)")

# --- OVERRIDE SIGNIFICANCE LETTERS ---
# plot_variable("Peso_secco_foglie",
#               type    = "barplot",
#               letters = c("a", "ab", "b"))

# --- ROTATE X AXIS LABELS (useful for long group names) ---
# plot_variable("Peso_secco_foglie",
#               type    = "barplot",
#               x_angle = 45)

# --- CHANGE THEME ---
# plot_variable("Peso_secco_foglie",
#               type  = "barplot",
#               theme = "minimal")   # "classic" | "minimal" | "light" | "bw"

# --- SAVE AS PNG ---
# plot_variable("Peso_secco_foglie",
#               type     = "barplot",
#               save     = "png",
#               filename = "figura1_peso_secco",
#               dpi      = 300)

# --- SAVE AS BOTH PNG AND PDF ---
# plot_variable("Peso_secco_foglie",
#               type = "barplot",
#               save = c("png", "pdf"))

# --- FULL CUSTOMIZATION EXAMPLE ---
# plot_variable(
#   variable       = "Peso_secco_foglie",
#   type           = "barplot",
#   colors         = c("#E06C75", "#61AFEF", "#98C379"),
#   title          = "Effect of irrigation on leaf dry weight",
#   x_label        = "Irrigation regime",
#   y_label        = "Dry weight (g)",
#   caption        = "Means ± SE. Different letters indicate p < 0.05 (Tukey HSD)",
#   letters        = c("a", "ab", "b"),
#   letter_size    = 6,
#   bar_width      = 0.6,
#   errorbar_width = 0.15,
#   x_angle        = 45,
#   base_size      = 13,
#   title_size     = 14,
#   theme          = "classic",
#   save           = c("screen", "png", "pdf"),
#   filename       = "figura1_peso_secco_foglie",
#   width          = 10,
#   height         = 7,
#   dpi            = 300
# )

# --- FURTHER MODIFY A PLOT AFTER CREATION ---
# Since plot_variable() returns the plot invisibly, you can assign it
# and add ggplot2 layers on top for any adjustment not covered by the parameters:
#
# p <- plot_variable("Peso_secco_foglie", type = "barplot")
# p + ylim(0, 50) + theme(legend.position = "none")


# ==============================================================================
# --- YOUR PLOTS — WRITE HERE ---
# ==============================================================================
# This is the only section you need to edit.
# Copy the variable name exactly as it appears in the summary table printed
# at the top of the console when you run this script.
#
# Minimum required:  plot_variable("VariableName", type = "barplot")
# Available types:   "barplot" | "boxplot" | "dotplot" | "violinplot"
#
# Example with full customization:
#   plot_variable(
#     variable = "Altezza_al_colletto",
#     type     = "barplot",
#     colors   = c("#E06C75", "#61AFEF", "#98C379"),
#     title    = "My custom title",
#     x_label  = "Irrigation regime",
#     y_label   = "Height (cm)",
#     letters  = c("a", "ab", "b"),
#     x_angle  = 45,
#     save     = c("screen", "png"),
#     filename = "figura1"
#   )
# ==============================================================================

# ↓↓↓ START WRITING YOUR PLOTS BELOW THIS LINE ↓↓↓ #
plot_variable("NAME OF YOUR VARIABLE",  type = "barplot")
   
  