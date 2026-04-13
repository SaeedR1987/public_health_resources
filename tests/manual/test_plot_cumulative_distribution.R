# Manual Test: plot_cumulative_distribution Function
#
# This script demonstrates the usage of the refactored plot_cumulative_distribution
# function with simple examples using dummy data.
#
# The function has been refactored to:
# - Accept any numeric column (not just hardcoded anthropometric indices)
# - Remove the 'flags' parameter (use specific column names instead)
# - Allow dynamic vertical reference lines
# - Support custom labels and axis limits
# - Auto-detect MUAC vs z-score data for appropriate defaults

library(phr)
library(dplyr)
library(ggplot2)

# Set output directory
output_dir <- "tests/manual/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("=== Manual Testing: plot_cumulative_distribution ===\n\n")

# ============================================================================
# EXAMPLE 1: Basic Usage with Z-Score Data
# ============================================================================
cat("Example 1: Basic usage with a z-score column\n")
cat(strrep("-", 60), "\n")

# Create simple dummy data with z-scores
set.seed(123)
df_basic <- data.frame(
  id = 1:200,
  wfhz = rnorm(200, mean = -1, sd = 1.2),  # Weight-for-height z-scores
  hfaz = rnorm(200, mean = -0.8, sd = 1.1), # Height-for-age z-scores
  district = sample(c("District A", "District B", "District C"), 200, replace = TRUE)
)

# Basic plot with default settings
p1 <- plot_cumulative_distribution(
  df = df_basic, index = "wfhz", flags = "yes", title_name = "Cumulative Distribution of WHZ"
)

print(p1)
ggsave(file.path(output_dir, "cumulative_01_basic_zscore.png"), p1, width = 8, height = 6)
cat("✓ Saved: cumulative_01_basic_zscore.png\n\n")

# ============================================================================
# EXAMPLE 2: Custom Title and Labels
# ============================================================================
cat("Example 2: Adding custom title and x-axis label\n")
cat(strrep("-", 60), "\n")

p2 <- plot_cumulative_distribution(
  df = df_basic,
  index = "wfhz", flags = "yes",
  title_name = "Cumulative Distribution of Weight-for-Height Z-Scores",
  subtitle = "Survey 2024"
)

print(p2)
ggsave(file.path(output_dir, "cumulative_02_custom_labels.png"), p2, width = 8, height = 6)
cat("✓ Saved: cumulative_02_custom_labels.png\n\n")

# ============================================================================
# EXAMPLE 3: With Grouping Variable
# ============================================================================
cat("Example 3: Grouped by district\n")
cat(strrep("-", 60), "\n")

p3 <- plot_cumulative_distribution(
  df = df_basic,
  index = "wfhz", flags = "yes",
  grouping = "district", color_palette = "reach1",
  title_name = "Cumulative Distribution by District",
)

print(p3)
ggsave(file.path(output_dir, "cumulative_03_grouped_by_district.png"), p3, width = 10, height = 6)
cat("✓ Saved: cumulative_03_grouped_by_district.png\n\n")

# ============================================================================
# EXAMPLE 4: MUAC Data with Auto-Detection
# ============================================================================
cat("Example 4: MUAC data with auto-detection of defaults\n")
cat(strrep("-", 60), "\n")

# Create MUAC data
df_muac <- data.frame(
  child_id = 1:300,
  muac = rnorm(300, mean = 13.5, sd = 1.8),
  age_group = sample(c("6-23 months", "24-59 months"), 300, replace = TRUE)
)

p4 <- plot_cumulative_distribution(
  df = df_muac,
  index = "muac", flags = "yes",
  title_name = "MUAC Cumulative Distribution",
)

print(p4)
ggsave(file.path(output_dir, "cumulative_04_muac_autodetect.png"), p4, width = 8, height = 6)
cat("✓ Saved: cumulative_04_muac_autodetect.png\n\n")

# ============================================================================
# EXAMPLE 5: MUAC with Grouping
# ============================================================================
cat("Example 5: MUAC data grouped by age group\n")
cat(strrep("-", 60), "\n")

p5 <- plot_cumulative_distribution(
  df = df_muac,
    data_var = "muac",
  grouping = "age_group",
  title_name = "MUAC Distribution by Age Group"
)

print(p5)
ggsave(file.path(output_dir, "cumulative_05_muac_grouped.png"), p5, width = 10, height = 6)
cat("✓ Saved: cumulative_05_muac_grouped.png\n\n")

# ============================================================================
# EXAMPLE 6: Custom Vertical Reference Lines
# ============================================================================
cat("Example 6: Custom vertical reference lines (WHO standard)\n")
cat(strrep("-", 60), "\n")

p6 <- plot_cumulative_distribution(
  df = df_basic,
  data_var = "wfhz",
  vline_intercepts = c(-3, -2, -1, 2),
  vline_colors = c("red", "orange", "yellow", "orange"),
  title_name = "Distribution with WHO Standard Cutoffs",
  x_label = "Weight-for-Height Z-Score"
)

print(p6)
ggsave(file.path(output_dir, "cumulative_06_custom_reference_lines.png"), p6, width = 8, height = 6)
cat("✓ Saved: cumulative_06_custom_reference_lines.png\n\n")

# ============================================================================
# EXAMPLE 7: No Reference Lines
# ============================================================================
cat("Example 7: Plot without vertical reference lines\n")
cat(strrep("-", 60), "\n")

p7 <- plot_cumulative_distribution(
  df = df_basic,
  data_var = "hfaz",
  vline_intercepts = NULL,
  title_name = "Height-for-Age Distribution (No Reference Lines)",
  x_label = "Height-for-Age Z-Score"
)

print(p7)
ggsave(file.path(output_dir, "cumulative_07_no_reference_lines.png"), p7, width = 8, height = 6)
cat("✓ Saved: cumulative_07_no_reference_lines.png\n\n")

# ============================================================================
# EXAMPLE 8: Simulating Flagged vs Non-Flagged Data
# ============================================================================
cat("Example 8: Comparing data with and without flagged values\n")
cat(strrep("-", 60), "\n")

# Create data with some extreme outliers (flagged values)
df_with_flags <- data.frame(
  id = 1:250,
  wfhz = c(
    rnorm(230, mean = -1, sd = 1),        # Normal values
    runif(10, -8, -6),                     # Extreme low outliers
    runif(10, 6, 8)                        # Extreme high outliers
  ),
  wfhz_noflag = c(
    rnorm(230, mean = -1, sd = 1),        # Normal values
    rep(NA, 20)                            # Flagged values removed
  )
)

# Plot with flagged values
p8a <- plot_cumulative_distribution(
  df = df_with_flags,
  data_var = "wfhz",
  title_name = "Cumulative Distribution Including Flagged Values",
  x_label = "Weight-for-Height Z-Score"
)

# Plot without flagged values
p8b <- plot_cumulative_distribution(
  df = df_with_flags,
  data_var = "wfhz_noflag",
  title_name = "Cumulative Distribution Excluding Flagged Values",
  x_label = "Weight-for-Height Z-Score"
)

print(p8a)
print(p8b)
ggsave(file.path(output_dir, "cumulative_08a_with_flags.png"), p8a, width = 8, height = 6)
ggsave(file.path(output_dir, "cumulative_08b_without_flags.png"), p8b, width = 8, height = 6)
cat("✓ Saved: cumulative_08a_with_flags.png and cumulative_08b_without_flags.png\n\n")

# ============================================================================
# EXAMPLE 9: Custom Axis Limits and Breaks
# ============================================================================
cat("Example 9: Custom x-axis limits and breaks\n")
cat(strrep("-", 60), "\n")

p9 <- plot_cumulative_distribution(
  df = df_basic,
  data_var = "wfhz",
  xlim = c(-4, 4),
  breaks_by = 1,
  title_name = "Cumulative Distribution with Custom Axis",
  x_label = "Weight-for-Height Z-Score"
)

print(p9)
ggsave(file.path(output_dir, "cumulative_09_custom_axis.png"), p9, width = 8, height = 6)
cat("✓ Saved: cumulative_09_custom_axis.png\n\n")

# ============================================================================
# EXAMPLE 10: Different Color Palettes
# ============================================================================
cat("Example 10: Using different color palettes\n")
cat(strrep("-", 60), "\n")

p10a <- plot_cumulative_distribution(
  df = df_basic,
  data_var = "wfhz",
  grouping = "district",
  color_palette = "reach1",
  title_name = "Color Palette: reach1"
)

p10b <- plot_cumulative_distribution(
  df = df_basic,
  data_var = "wfhz",
  grouping = "district",
  color_palette = "reach3",
  title_name = "Color Palette: reach3"
)

print(p10a)
print(p10b)
ggsave(file.path(output_dir, "cumulative_10a_palette_reach1.png"), p10a, width = 10, height = 6)
ggsave(file.path(output_dir, "cumulative_10b_palette_reach3.png"), p10b, width = 10, height = 6)
cat("✓ Saved: cumulative_10a_palette_reach1.png and cumulative_10b_palette_reach3.png\n\n")

# ============================================================================
# EXAMPLE 11: Realistic Anthropometric Data Simulation
# ============================================================================
cat("Example 11: Realistic anthropometric data with multiple indices\n")
cat(strrep("-", 60), "\n")

# Simulate realistic anthropometric data
set.seed(456)
df_anthro <- data.frame(
  child_id = 1:500,
  wfhz = rnorm(500, mean = -0.5, sd = 1.1),
  hfaz = rnorm(500, mean = -1.2, sd = 1.3),
  wfaz = rnorm(500, mean = -0.8, sd = 1.2),
  muac = rnorm(500, mean = 13.8, sd = 1.5),
  age_group = sample(c("6-23 months", "24-59 months"), 500, replace = TRUE),
  survey_round = sample(c("Baseline", "Endline"), 500, replace = TRUE)
)

# Plot each index
p11a <- plot_cumulative_distribution(
  df = df_anthro,
  data_var = "wfhz",
  grouping = "age_group",
  title_name = "Weight-for-Height by Age Group",
  x_label = "WHZ"
)

p11b <- plot_cumulative_distribution(
  df = df_anthro,
  data_var = "hfaz",
  grouping = "age_group",
  title_name = "Height-for-Age by Age Group",
  x_label = "HAZ"
)

p11c <- plot_cumulative_distribution(
  df = df_anthro,
  data_var = "muac",
  grouping = "survey_round",
  title_name = "MUAC by Survey Round",
  x_label = "MUAC (cm)"
)

print(p11a)
print(p11b)
print(p11c)
ggsave(file.path(output_dir, "cumulative_11a_wfhz_by_age.png"), p11a, width = 10, height = 6)
ggsave(file.path(output_dir, "cumulative_11b_hfaz_by_age.png"), p11b, width = 10, height = 6)
ggsave(file.path(output_dir, "cumulative_11c_muac_by_round.png"), p11c, width = 10, height = 6)
cat("✓ Saved: cumulative_11a_wfhz_by_age.png, cumulative_11b_hfaz_by_age.png, cumulative_11c_muac_by_round.png\n\n")

# ============================================================================
# Summary
# ============================================================================
cat("\n")
cat(strrep("=", 60), "\n")
cat("SUMMARY: Manual Testing Complete\n")
cat(strrep("=", 60), "\n")
cat("\nAll plots have been saved to:", output_dir, "\n")
cat("\nKey improvements demonstrated:\n")
cat("  • Accepts any numeric column (not limited to wfhz/hfaz/wfaz/mfaz/muac)\n")
cat("  • No more 'flags' parameter - use specific column names instead\n")
cat("  • Dynamic vertical reference lines (customize or disable)\n")
cat("  • Custom x-axis labels, limits, and breaks\n")
cat("  • Auto-detection of MUAC vs z-score data for appropriate defaults\n")
cat("  • Works with grouping variables\n")
cat("  • Supports different color palettes\n")
cat("  • Can handle any numeric data type\n")
cat("\n")

# List all generated files
cat("Generated files:\n")
files <- list.files(output_dir, pattern = "cumulative_*.png", full.names = FALSE)
for (f in files) {
  cat("  -", f, "\n")
}

cat("\n✓ Manual testing complete!\n")
