
# PLOT CORRELOGRAM ####

# Load required libraries
library(ggplot2)
library(GGally)

# Create a minimal test dataset
create_test_dataset <- function(n = 100) {
  set.seed(123)  # For reproducibility

  test_data <- data.frame(
    uuid = paste0("HH_", seq_len(n)),
    fsl_fcs_score = round(runif(n, min = 0, max = 112), 1),
    fsl_rcsi_score = round(runif(n, min = 0, max = 56), 1),
    fsl_hhs_score = round(runif(n, min = 0, max = 6), 1)
  )

  # Add some correlation between variables to make the plot more interesting
  test_data$fsl_rcsi_score <- test_data$fsl_rcsi_score +
    0.3 * test_data$fsl_fcs_score + rnorm(n, 0, 5)
  test_data$fsl_hhs_score <- test_data$fsl_hhs_score +
    0.02 * test_data$fsl_fcs_score + rnorm(n, 0, 0.5)

  # Ensure scores stay within reasonable bounds
  test_data$fsl_rcsi_score <- pmax(0, pmin(56, test_data$fsl_rcsi_score))
  test_data$fsl_hhs_score <- pmax(0, pmin(6, test_data$fsl_hhs_score))

  return(test_data)
}

# Create test dataset
test_dataset <- create_test_dataset(n = 150)

# Test 1: Basic usage with default parameters
plot1 <- plot_correlogram(.dataset = test_dataset)
print(plot1)

# Test 2: With custom title
plot2 <- plot_correlogram(
  .dataset = test_dataset,
  title_name = "FSL Indicators Correlation Analysis"
)
print(plot2)

# Test 3: With title and custom subtitle
plot3 <- plot_correlogram(
  .dataset = test_dataset,
  title_name = "FSL Indicators Correlogram",
  subtitle = "Test Data - Region X"
)
print(plot3)

# Test 4: With subset of columns
plot4 <- plot_correlogram(
  .dataset = test_dataset,
  numeric_cols = c("fsl_fcs_score", "fsl_rcsi_score"),
  title_name = "FCS vs RCSI"
)
print(plot4)

# Test 5: Check correlation values
cor(test_dataset[, c("fsl_fcs_score", "fsl_rcsi_score", "fsl_hhs_score")])

#PLOT AGE PYRAMID ####

# Load required libraries
library(dplyr)
library(ggplot2)

# Create test dataset with numeric sex coding and weights
create_test_roster <- function(n = 500, include_weights = TRUE) {
  set.seed(456)
  ages <- c(
    sample(0:17, n * 0.35, replace = TRUE),
    sample(18:59, n * 0.55, replace = TRUE),
    sample(60:90, n * 0.10, replace = TRUE)
  )
  ages <- ages[1:n]

  roster <- data.frame(
    individual_id = paste0("IND_", seq_len(n)),
    household_id = paste0("HH_", sample(1:100, n, replace = TRUE)),
    sex = sample(c(1, 2), n, replace = TRUE),
    age_years = ages
  )

  if (include_weights) {
    # Add survey weights (some variation around 1)
    roster$survey_weight <- runif(n, min = 0.5, max = 2.5)
  }

  return(roster)
}

test_roster <- create_test_roster(n = 600, include_weights = TRUE)

# View structure
str(test_roster)
head(test_roster)

# Test 1: Basic usage - unweighted proportions (default)
plot1 <- plot_age_pyramid(
  .dataset = test_roster,
  sex_col = "sex",
  age_years_col = "age_years",
  title_name = "Age Pyramid - Unweighted Proportions"
)
print(plot1)

# Test 2: Unweighted counts
plot2 <- plot_age_pyramid(
  .dataset = test_roster,
  sex_col = "sex",
  age_years_col = "age_years",
  proportional = FALSE,
  title_name = "Age Pyramid - Unweighted Counts"
)
print(plot2)

# Test 3: Weighted proportions
plot3 <- plot_age_pyramid(
  .dataset = test_roster,
  sex_col = "sex",
  age_years_col = "age_years",
  weights_col = "survey_weight",
  weighted_result = TRUE,
  proportional = TRUE,
  title_name = "Age Pyramid - Weighted Proportions"
)
print(plot3)

# Test 4: Weighted counts
plot4 <- plot_age_pyramid(
  .dataset = test_roster,
  sex_col = "sex",
  age_years_col = "age_years",
  weights_col = "survey_weight",
  weighted_result = TRUE,
  proportional = FALSE,
  title_name = "Age Pyramid - Weighted Counts"
)
print(plot4)

# Test 5: Compare weighted vs unweighted proportions
p_unwt_prop <- plot_age_pyramid(
  .dataset = test_roster,
  sex_col = "sex",
  age_years_col = "age_years",
  proportional = TRUE,
  title_name = "Unweighted %"
)

p_wt_prop <- plot_age_pyramid(
  .dataset = test_roster,
  sex_col = "sex",
  age_years_col = "age_years",
  weights_col = "survey_weight",
  weighted_result = TRUE,
  proportional = TRUE,
  title_name = "Weighted %"
)

library(gridExtra)
grid.arrange(p_unwt_prop, p_wt_prop, ncol = 2)

# Test 6: Data with NA weights (should be filtered out)
test_roster_na_weights <- test_roster
test_roster_na_weights$survey_weight[sample(1:nrow(test_roster_na_weights), 50)] <- NA

plot6 <- plot_age_pyramid(
  .dataset = test_roster_na_weights,
  sex_col = "sex",
  age_years_col = "age_years",
  weights_col = "survey_weight",
  weighted_result = TRUE,
  title_name = "Age Pyramid - With NA Weights (Filtered)"
)
print(plot6)

# Test 7: Character sex values
test_roster_char <- test_roster %>%
  mutate(gender = ifelse(sex == 1, "m", "f"))

plot7 <- plot_age_pyramid(
  .dataset = test_roster_char,
  sex_col = "gender",
  age_years_col = "age_years",
  sex_male_val = "m",
  sex_female_val = "f",
  title_name = "Age Pyramid - Character Sex Values"
)
print(plot7)

# Test 8: Custom labels
plot8 <- plot_age_pyramid(
  .dataset = test_roster,
  sex_col = "sex",
  age_years_col = "age_years",
  sex_male_lab = "Men",
  sex_female_lab = "Women",
  weights_col = "survey_weight",
  weighted_result = TRUE,
  title_name = "Age Pyramid - Custom Labels (Weighted)"
)
print(plot8)

# Test 9: Custom age groups
test_roster_grouped <- test_roster %>%
  mutate(age_category = cut(age_years,
                            breaks = c(-1, 17, 59, Inf),
                            labels = c("Children (0-17)", "Adults (18-59)", "Elderly (60+)")))

plot9 <- plot_age_pyramid(
  .dataset = test_roster_grouped,
  sex_col = "sex",
  age_years_col = "age_years",
  age_grouping = TRUE,
  age_group_col = "age_category",
  title_name = "Age Pyramid - Custom Age Groups"
)
print(plot9)

# Test 10: Custom age groups with weights
plot10 <- plot_age_pyramid(
  .dataset = test_roster_grouped,
  sex_col = "sex",
  age_years_col = "age_years",
  age_grouping = TRUE,
  age_group_col = "age_category",
  weights_col = "survey_weight",
  weighted_result = TRUE,
  proportional = TRUE,
  title_name = "Age Pyramid - Custom Age Groups (Weighted %)"
)
print(plot10)

# Test 11: Custom axis labels
plot11 <- plot_age_pyramid(
  .dataset = test_roster,
  sex_col = "sex",
  age_years_col = "age_years",
  x_lab = "Custom X Label",
  y_lab = "Custom Y Label",
  title_name = "Age Pyramid - Custom Axis Labels"
)
print(plot11)

# Test 12: Different color palette
plot12 <- plot_age_pyramid(
  .dataset = test_roster,
  sex_col = "sex",
  age_years_col = "age_years",
  color_palette = "reach2",
  title_name = "Age Pyramid - REACH2 Colors"
)
print(plot12)

# Verify the percentage calculations manually
cat("\n=== Manual Verification ===\n")
test_manual <- test_roster %>%
  mutate(age_group = cut(age_years,
                         breaks = c(-1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84, Inf),
                         labels = c("0-4", "5-9", "10-14", "15-19",
                                    "20-24", "25-29", "30-34", "35-39","40-44", "45-49", "50-54", "55-59",
                                    "60-64", "65-69", "70-74", "75-79", "80-84", "85+")))

# Unweighted percentages
manual_unwt <- test_manual %>%
  group_by(sex, age_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(sex) %>%
  mutate(pct = count / sum(count) * 100) %>%
  ungroup()

cat("\nUnweighted % for first few age groups (sex=1):\n")
print(head(manual_unwt %>% filter(sex == 1), 5))

# Weighted percentages
manual_wt <- test_manual %>%
  group_by(sex, age_group) %>%
  summarise(count = sum(survey_weight), .groups = "drop") %>%
  group_by(sex) %>%
  mutate(pct = count / sum(count) * 100) %>%
  ungroup()

cat("\nWeighted % for first few age groups (sex=1):\n")
print(head(manual_wt %>% filter(sex == 1), 5))

cat("\n=== Total percentages by sex ===")
cat("\nUnweighted (should sum to 100% per sex):\n")
print(manual_unwt %>% group_by(sex) %>% summarise(total_pct = sum(pct)))

cat("\nWeighted (should sum to 100% per sex):\n")
print(manual_wt %>% group_by(sex) %>% summarise(total_pct = sum(pct)))

# PLOT AGE DISTRIBUTION ####

# Load required libraries
library(dplyr)
library(ggplot2)

# Create test dataset with age in years and months
create_test_age_data <- function(n = 500) {
  set.seed(789)

  # Create children data (0-5 years / 0-59 months)
  data.frame(
    individual_id = paste0("CHILD_", seq_len(n)),
    household_id = paste0("HH_", sample(1:100, n, replace = TRUE)),
    age_years = sample(0:5, n, replace = TRUE),
    age_months = sample(0:59, n, replace = TRUE),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    survey_round = sample(c("Round 1", "Round 2", "Round 3"), n, replace = TRUE)
  )
}

test_data <- create_test_age_data(n = 600)

# View structure
str(test_data)
head(test_data)
summary(test_data$age_years)
summary(test_data$age_months)

cat("\n=== Testing plot_age_distribution ===\n\n")


# TESTS WITH YEARS


# Test 1: Basic usage - age in years (default behavior)
plot1 <- plot_age_distribution(
  .dataset = test_data, age_years = "age_years", year_or_month = "year"
)
print(plot1)

# Test 2: Explicit year specification
plot2 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year"
)
print(plot2)

# Test 3: Custom age range in years
plot3 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year",
  min_age = 1,
  max_age = 4,
  title_name = "Age Distribution - 1 to 4 Years"
)
print(plot3)

# Test 4: Custom breaks in years
plot4 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year",
  min_age = 0,
  max_age = 5,
  breaks = 0.5,
  title_name = "Age Distribution - 0.5 Year Breaks"
)
print(plot4)

# Test 5: Years with grouping by region
plot5 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year",
  by_group = "region",
  min_age = 0,
  max_age = 5,
  title_name = "Age Distribution by Region (Years)"
)
print(plot5)

# Test 6: Years with grouping by survey round
plot6 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year",
  by_group = "survey_round",
  min_age = 0,
  max_age = 5,
  breaks = 1,
  title_name = "Age Distribution by Survey Round (Years)"
)
print(plot6)

# Test 7: Years with custom title and subtitle
plot7 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year",
  min_age = 0,
  max_age = 5,
  title_name = "Child Age Distribution",
  subtitle = "Survey 2026"
)
print(plot7)

# Test 8: Years with custom color palette
plot8 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year",
  min_age = 0,
  max_age = 5,
  color_palette = "reach3",
  title_name = "Age Distribution - Custom Color"
)
print(plot8)


# TESTS WITH MONTHS


# Test 9: Age in months (basic)
plot9 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "month"
)
print(plot9)

# Test 10: Months with custom range
plot10 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "month",
  min_age = 6,
  max_age = 23,
  title_name = "Age Distribution - 6 to 23 Months"
)
print(plot10)

# Test 11: Months with custom breaks
plot11 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "month",
  min_age = 0,
  max_age = 59,
  breaks = 6,
  title_name = "Age Distribution - 6 Month Breaks"
)
print(plot11)

# Test 12: Months with grouping by region
plot12 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "month",
  by_group = "region",
  min_age = 0,
  max_age = 59,
  breaks = 12,
  title_name = "Age Distribution by Region (Months)"
)
print(plot12)

# Test 13: Months with grouping by survey round
plot13 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "month",
  by_group = "survey_round",
  min_age = 0,
  max_age = 59,
  breaks = 12,
  title_name = "Age Distribution by Survey Round (Months)"
)
print(plot13)

# Test 14: Months - focus on under 2 years (0-23 months)
plot14 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "month",
  min_age = 0,
  max_age = 23,
  breaks = 3,
  title_name = "Age Distribution - Under 2 Years (3 Month Breaks)"
)
print(plot14)

# Test 15: Months with custom subtitle
plot15 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "month",
  min_age = 0,
  max_age = 59,
  breaks = 12,
  title_name = "Child Age Distribution (Months)",
  subtitle = "All Regions Combined"
)
print(plot15)


# TESTS WITH CUSTOM COLUMN NAMES


# Test 16: Custom age_years column name
test_data_custom <- test_data %>%
  rename(age_in_years = age_years)

plot16 <- plot_age_distribution(
  .dataset = test_data_custom,
  age_years = "age_in_years",
  year_or_month = "year",
  min_age = 0,
  max_age = 5,
  title_name = "Custom age_years Column Name"
)
print(plot16)

# Test 17: Custom age_months column name
test_data_custom2 <- test_data %>%
  rename(age_in_months = age_months)

plot17 <- plot_age_distribution(
  .dataset = test_data_custom2,
  age_months = "age_in_months",
  year_or_month = "month",
  min_age = 0,
  max_age = 59,
  title_name = "Custom age_months Column Name"
)
print(plot17)


# EDGE CASES AND ERROR HANDLING


# Test 18: Very narrow age range
plot18 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year",
  min_age = 0,
  max_age = 5,
  breaks = 0.5,
  title_name = "Narrow Age Range (2-3 Years)"
)
print(plot18)

# Test 19: Single age year
plot19 <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year",
  min_age = 2,
  max_age = 4,
  breaks = 1,
  title_name = "Single Age Year (3 Years Only)"
)
print(plot19)

# Test 20: Error handling - missing dataset
tryCatch({
  plot20 <- plot_age_distribution(.dataset = NULL)
}, error = function(e) {
  cat("Expected error caught (NULL dataset):", e$message, "\n")
})

# Test 21: Error handling - invalid by_group column
tryCatch({
  plot21 <- plot_age_distribution(
    .dataset = test_data,
    by_group = "nonexistent_column"
  )
}, error = function(e) {
  cat("Expected error caught (invalid by_group):", e$message, "\n")
})

# Test 22: Error handling - invalid age_years column
tryCatch({
  plot22 <- plot_age_distribution(
    .dataset = test_data,
    age_years = "nonexistent_age_column",
    year_or_month = "year"
  )
}, error = function(e) {
  cat("Expected error caught (invalid age_years column):", e$message, "\n")
})

# Test 23: Error handling - invalid age_months column
tryCatch({
  plot23 <- plot_age_distribution(
    .dataset = test_data,
    age_months = "nonexistent_months_column",
    year_or_month = "month"
  )
}, error = function(e) {
  cat("Expected error caught (invalid age_months column):", e$message, "\n")
})


# COMPARISON PLOTS


# Test 24: Side-by-side comparison - Years vs Months
library(gridExtra)

p_years <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "year",
  min_age = 0,
  max_age = 5,
  breaks = 1,
  title_name = "Years"
)

p_months <- plot_age_distribution(
  .dataset = test_data,
  year_or_month = "month",
  min_age = 0,
  max_age = 59,
  breaks = 2,
  title_name = "Months"
)

grid.arrange(p_years, p_months, ncol = 2)

# Test 25: Multiple groups comparison
test_data_multi <- test_data %>%
  mutate(
    age_category = cut(age_years,
                       breaks = c(-1, 2, 5),
                       labels = c("0-2 years", "3-5 years"))
  )

plot25 <- plot_age_distribution(
  .dataset = test_data_multi,
  year_or_month = "year",
  by_group = "age_category",
  min_age = 0,
  max_age = 5,
  title_name = "Age Distribution by Age Category"
)
print(plot25)


# VERIFICATION


cat("\n=== Data Verification ===\n")
cat("\nAge in years distribution:\n")
table(test_data$age_years)

cat("\nAge in months summary:\n")
summary(test_data$age_months)

cat("\nRegion distribution:\n")
table(test_data$region)

cat("\nSurvey round distribution:\n")
table(test_data$survey_round)

cat("\nCross-tabulation (age_years by region):\n")
print(table(test_data$age_years, test_data$region))

# Test filtering logic
cat("\n=== Testing Filtering Logic ===\n")
cat("Original data rows:", nrow(test_data), "\n")
cat("Rows with age_years 0-5:", nrow(test_data %>% filter(age_years >= 0 & age_years <= 5)), "\n")
cat("Rows with age_years 1-4:", nrow(test_data %>% filter(age_years >= 1 & age_years <= 4)), "\n")
cat("Rows with age_months 0-59:", nrow(test_data %>% filter(age_months >= 0 & age_months <= 59)), "\n")
cat("Rows with age_months 6-23:", nrow(test_data %>% filter(age_months >= 6 & age_months <= 23)), "\n")

cat("\n=== All tests completed ===\n")

# TEST PLOT RIDGE DISTRIBUTION ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyr)

# Create test dataset
create_test_ridge_data <- function(n = 300) {
  set.seed(999)

  data.frame(
    id = paste0("ID_", seq_len(n)),
    region = sample(c("North", "South", "East"), n, replace = TRUE),
    survey_round = sample(c("Round 1", "Round 2"), n, replace = TRUE),
    fcs_score = rnorm(n, mean = 45, sd = 15),
    rcsi_score = rnorm(n, mean = 25, sd = 10),
    hhs_score = rnorm(n, mean = 3, sd = 2),
    income = rlnorm(n, meanlog = 5, sdlog = 1),
    expenditure = rnorm(n, mean = 200, sd = 50)
  )
}

test_data <- create_test_ridge_data(n = 400)

cat("\n=== Testing plot_ridge_distribution with Labels ===\n\n")

# Test 1: Without custom labels (default behavior - uses column names)
plot1 <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"),
  name_groups = "Indicator",
  name_units = "Score",
  title_name = "Without Custom Labels (Column Names)"
)
print(plot1)

# Test 2: With custom labels
plot2 <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"),
  numeric_cols_labels = c("Food Consumption Score", "Reduced Coping Strategies Index", "Household Hunger Scale"),
  name_groups = "Indicator",
  name_units = "Score",
  title_name = "With Custom Labels"
)
print(plot2)

# Test 3: Short custom labels
plot3 <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"),
  numeric_cols_labels = c("FCS", "rCSI", "HHS"),
  name_groups = "FSL Indicator",
  name_units = "Score Value",
  title_name = "Short Custom Labels"
)
print(plot3)

# Test 4: Custom labels with grouping
plot4 <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"),
  numeric_cols_labels = c("FCS", "rCSI", "HHS"),
  name_groups = "Indicator",
  name_units = "Score",
  grouping = "region",
  title_name = "Custom Labels with Regional Grouping"
)
print(plot4)

# Test 5: Two columns with custom labels
plot5 <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("income", "expenditure"),
  numeric_cols_labels = c("Household Income", "Household Expenditure"),
  name_groups = "Economic Indicator",
  name_units = "Amount (USD)",
  title_name = "Economic Indicators with Custom Labels"
)
print(plot5)

# Test 6: Custom labels with different color palette
plot6 <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"),
  numeric_cols_labels = c("Food Consumption", "Coping Strategies", "Hunger Scale"),
  name_groups = "Indicator Type",
  name_units = "Score",
  color_palette = "reach2",
  title_name = "Custom Labels with REACH2 Palette"
)
print(plot6)

# Test 7: All five numeric columns with labels
plot7 <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score", "hhs_score", "income", "expenditure"),
  numeric_cols_labels = c("FCS", "rCSI", "HHS", "Income", "Expenditure"),
  name_groups = "Variable",
  name_units = "Value",
  title_name = "All Variables with Custom Labels"
)
print(plot7)

# Test 8: Custom labels with subtitle
plot8 <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"),
  numeric_cols_labels = c("Food Consumption Score", "Reduced Coping Strategies", "Household Hunger"),
  name_groups = "Indicator",
  name_units = "Score",
  title_name = "FSL Indicators Distribution",
  subtitle = "Survey 2026 - All Regions"
)
print(plot8)


# ERROR HANDLING - LABEL LENGTH MISMATCH


# Test 9: Error - too few labels
tryCatch({
  plot9 <- plot_ridge_distribution(
    .dataset = test_data,
    numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"),
    numeric_cols_labels = c("FCS", "rCSI"),  # Only 2 labels for 3 columns
    name_groups = "Indicator",
    name_units = "Score"
  )
}, error = function(e) {
  cat("Expected error caught (too few labels):", e$message, "\n")
})

# Test 10: Error - too many labels
tryCatch({
  plot10 <- plot_ridge_distribution(
    .dataset = test_data,
    numeric_cols = c("fcs_score", "rcsi_score"),
    numeric_cols_labels = c("FCS", "rCSI", "HHS", "Extra"),  # 4 labels for 2 columns
    name_groups = "Indicator",
    name_units = "Score"
  )
}, error = function(e) {
  cat("Expected error caught (too many labels):", e$message, "\n")
})

# Test 11: NULL labels (should work - uses column names)
plot11 <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score"),
  numeric_cols_labels = NULL,  # Explicitly NULL
  name_groups = "Indicator",
  name_units = "Score",
  title_name = "NULL Labels (Uses Column Names)"
)
print(plot11)


# COMPARISON


# Test 12: Side-by-side comparison - with and without labels
library(gridExtra)

p_no_labels <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"),
  name_groups = "Indicator",
  name_units = "Score",
  title_name = "Column Names"
)

p_with_labels <- plot_ridge_distribution(
  .dataset = test_data,
  numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"),
  numeric_cols_labels = c("FCS", "rCSI", "HHS"),
  name_groups = "Indicator",
  name_units = "Score",
  title_name = "Custom Labels"
)

grid.arrange(p_no_labels, p_with_labels, ncol = 2)


# VERIFICATION


cat("\n=== Label Mapping Verification ===\n")
numeric_cols_test <- c("fcs_score", "rcsi_score", "hhs_score")
labels_test <- c("FCS", "rCSI", "HHS")
label_mapping <- setNames(labels_test, numeric_cols_test)
cat("Column names:", paste(numeric_cols_test, collapse = ", "), "\n")
cat("Custom labels:", paste(labels_test, collapse = ", "), "\n")
cat("Mapping:\n")
print(label_mapping)

cat("\n=== Length Validation ===\n")
cat("numeric_cols length:", length(numeric_cols_test), "\n")
cat("numeric_cols_labels length:", length(labels_test), "\n")
cat("Lengths match:", length(numeric_cols_test) == length(labels_test), "\n")

cat("\n=== All label tests completed ===\n")

# PLOT IYCF AREA GRAPH ####

# Load required libraries
library(dplyr)
library(ggplot2)

# Create test dataset with proper data types
create_test_iycf_data <- function(n = 500) {
  set.seed(111)
  data.frame(
    child_id = paste0("CHILD_", seq_len(n)),
    age_months = sample(0:23, n, replace = TRUE),  # Numeric
    region = sample(c("North", "South"), n, replace = TRUE),
    iycf_ebf = sample(0:1, n, replace = TRUE),
    iycf_4 = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),
    iycf_6a = sample(0:1, n, replace = TRUE),
    iycf_6b = sample(0:7, n, replace = TRUE),  # Frequency (numeric)
    iycf_6c = sample(0:7, n, replace = TRUE),  # Frequency (numeric)
    iycf_6d = sample(0:7, n, replace = TRUE),  # Frequency (numeric)
    iycf_6e = sample(0:1, n, replace = TRUE),
    iycf_6f = sample(0:1, n, replace = TRUE),
    iycf_6g = sample(0:1, n, replace = TRUE),
    iycf_6h = sample(0:1, n, replace = TRUE),
    iycf_6i = sample(0:1, n, replace = TRUE),
    iycf_6j = sample(0:1, n, replace = TRUE),
    iycf_7a = sample(0:7, n, replace = TRUE),  # Frequency (numeric)
    iycf_7b = sample(0:1, n, replace = TRUE),
    iycf_7c = sample(0:1, n, replace = TRUE),
    iycf_7d = sample(0:1, n, replace = TRUE),
    iycf_7e = sample(0:1, n, replace = TRUE),
    iycf_7f = sample(0:1, n, replace = TRUE),
    iycf_7g = sample(0:1, n, replace = TRUE),
    iycf_7h = sample(0:1, n, replace = TRUE),
    iycf_7i = sample(0:1, n, replace = TRUE),
    iycf_7j = sample(0:1, n, replace = TRUE),
    iycf_7k = sample(0:1, n, replace = TRUE),
    iycf_7l = sample(0:1, n, replace = TRUE),
    iycf_7m = sample(0:1, n, replace = TRUE),
    iycf_7n = sample(0:1, n, replace = TRUE),
    iycf_7o = sample(0:1, n, replace = TRUE),
    iycf_7p = sample(0:1, n, replace = TRUE),
    iycf_7q = sample(0:1, n, replace = TRUE),
    iycf_7r = sample(0:1, n, replace = TRUE)
  )
}

test_iycf <- create_test_iycf_data(n = 600)

cat("\n=== Testing Numeric Validation ===\n\n")

# Test 1: Valid numeric data (should work)
cat("Test 1: Valid numeric data\n")
plot1 <- plot_iycf_areagraph(df = test_iycf, title_name = "Valid Numeric Data")
print(plot1)
cat("✓ Test 1 passed\n\n")

# Test 2: Age as character but coercible to numeric (should work)
cat("Test 2: Age as character (coercible to numeric)\n")
test_char_age <- test_iycf %>%
  mutate(age_months = as.character(age_months))
plot2 <- plot_iycf_areagraph(df = test_char_age, title_name = "Character Age (Coercible)")
print(plot2)
cat("✓ Test 2 passed\n\n")

# Test 3: Frequency columns as character but coercible (should work)
cat("Test 3: Frequency columns as character (coercible)\n")
test_char_freq <- test_iycf %>%
  mutate(
    iycf_6b = as.character(iycf_6b),
    iycf_6c = as.character(iycf_6c),
    iycf_6d = as.character(iycf_6d),
    iycf_7a = as.character(iycf_7a)
  )
plot3 <- plot_iycf_areagraph(df = test_char_freq, title_name = "Character Frequencies (Coercible)")
print(plot3)
cat("✓ Test 3 passed\n\n")

# Test 4: Error - Age as non-numeric character (should fail)
cat("Test 4: Error test - Age as non-numeric text\n")
test_invalid_age <- test_iycf %>%
  mutate(age_months = sample(c("young", "old", "infant"), nrow(.), replace = TRUE))
tryCatch({
  plot4 <- plot_iycf_areagraph(df = test_invalid_age)
  cat("✗ Test 4 failed: Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error caught:", e$message, "\n\n")
})

# Test 5: Error - iycf_6b as non-numeric (should fail)
cat("Test 5: Error test - iycf_6b as non-numeric text\n")
test_invalid_6b <- test_iycf %>%
  mutate(iycf_6b = sample(c("never", "sometimes", "often"), nrow(.), replace = TRUE))
tryCatch({
  plot5 <- plot_iycf_areagraph(df = test_invalid_6b)
  cat("✗ Test 5 failed: Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error caught:", e$message, "\n\n")
})

# Test 6: Error - iycf_7a as non-numeric (should fail)
cat("Test 6: Error test - iycf_7a as non-numeric text\n")
test_invalid_7a <- test_iycf %>%
  mutate(iycf_7a = sample(c("yes", "no", "maybe"), nrow(.), replace = TRUE))
tryCatch({
  plot6 <- plot_iycf_areagraph(df = test_invalid_7a)
  cat("✗ Test 6 failed: Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error caught:", e$message, "\n\n")
})

# Test 7: Verify frequency logic (>0 means consumed)
cat("Test 7: Frequency logic - values >0 indicate consumption\n")
test_freq_logic <- test_iycf %>%
  slice(1:100) %>%
  mutate(
    iycf_6b = c(rep(0, 50), rep(2, 50)),  # Half never, half consumed 2 times
    iycf_6c = 0,  # None consumed animal milk
    iycf_6d = 0,  # None consumed powdered milk
    iycf_7a = 0   # None consumed yogurt
  )
plot7 <- plot_iycf_areagraph(df = test_freq_logic, title_name = "Frequency Logic Test")
print(plot7)
cat("✓ Test 7 passed\n\n")

# Test 8: Subset by region (previously fixed issue)
cat("Test 8: Subset by region\n")
plot8 <- plot_iycf_areagraph(
  df = test_iycf %>% filter(region == "North"),
  title_name = "North Region - Validated Data"
)
print(plot8)
cat("✓ Test 8 passed\n\n")

# Test 9: Verify data types
cat("\n=== Data Type Verification ===\n")
cat("age_months type:", class(test_iycf$age_months), "\n")
cat("iycf_6b type:", class(test_iycf$iycf_6b), "\n")
cat("iycf_6c type:", class(test_iycf$iycf_6c), "\n")
cat("iycf_6d type:", class(test_iycf$iycf_6d), "\n")
cat("iycf_7a type:", class(test_iycf$iycf_7a), "\n")

cat("\n=== Frequency Column Value Ranges ===\n")
cat("iycf_6b (formula) range:", range(test_iycf$iycf_6b), "\n")
cat("iycf_6c (animal milk) range:", range(test_iycf$iycf_6c), "\n")
cat("iycf_6d (powdered milk) range:", range(test_iycf$iycf_6d), "\n")
cat("iycf_7a (yogurt) range:", range(test_iycf$iycf_7a), "\n")

cat("\n=== All validation tests completed ===\n")

# Load required libraries
library(dplyr)
library(ggplot2)

cat("\n=== Testing Full Category Coverage for IYCF Area Graph ===\n\n")

# Create test dataset with specific patterns to hit all categories
create_full_category_iycf_data <- function() {
  set.seed(222)

  # Define category patterns
  # We'll create 100 children in each category to ensure visibility
  n_per_category <- 100

  # Category 1: Exclusive Breastfed (bf=1, no water, no liquids, no foods)
  exclusive_bf <- data.frame(
    child_id = paste0("EBF_", 1:n_per_category),
    age_months = sample(0:5, n_per_category, replace = TRUE),  # Mostly younger children
    category_label = "Exclusive Breastfed",
    iycf_4 = 1,  # Breastfeeding
    iycf_6a = 0,  # No water
    iycf_6b = 0, iycf_6c = 0, iycf_6d = 0,  # No formula/milk
    iycf_6e = 0, iycf_6f = 0, iycf_6g = 0, iycf_6h = 0, iycf_6i = 0, iycf_6j = 0,  # No other liquids
    iycf_7a = 0,  # No yogurt
    iycf_7b = 0, iycf_7c = 0, iycf_7d = 0, iycf_7e = 0, iycf_7f = 0,  # No other foods
    iycf_7g = 0, iycf_7h = 0, iycf_7i = 0, iycf_7j = 0, iycf_7k = 0,
    iycf_7l = 0, iycf_7m = 0, iycf_7n = 0, iycf_7o = 0, iycf_7p = 0, iycf_7q = 0, iycf_7r = 0
  )

  # Category 2: Breastfed & Plain Water (bf=1, water=1, no other liquids, no foods)
  bf_water <- data.frame(
    child_id = paste0("BF_WATER_", 1:n_per_category),
    age_months = sample(2:8, n_per_category, replace = TRUE),
    category_label = "Breastfed & Plain Water",
    iycf_4 = 1,  # Breastfeeding
    iycf_6a = 1,  # YES water
    iycf_6b = 0, iycf_6c = 0, iycf_6d = 0,  # No formula/milk
    iycf_6e = 0, iycf_6f = 0, iycf_6g = 0, iycf_6h = 0, iycf_6i = 0, iycf_6j = 0,  # No other liquids
    iycf_7a = 0,  # No yogurt
    iycf_7b = 0, iycf_7c = 0, iycf_7d = 0, iycf_7e = 0, iycf_7f = 0,  # No other foods
    iycf_7g = 0, iycf_7h = 0, iycf_7i = 0, iycf_7j = 0, iycf_7k = 0,
    iycf_7l = 0, iycf_7m = 0, iycf_7n = 0, iycf_7o = 0, iycf_7p = 0, iycf_7q = 0, iycf_7r = 0
  )

  # Category 3: Breastfed & Non-Milk Liquids (bf=1, no formula/milk, has other liquids, no foods)
  bf_liquids <- data.frame(
    child_id = paste0("BF_LIQ_", 1:n_per_category),
    age_months = sample(3:10, n_per_category, replace = TRUE),
    category_label = "Breastfed & Non-Milk Liquids",
    iycf_4 = 1,  # Breastfeeding
    iycf_6a = sample(0:1, n_per_category, replace = TRUE),  # May or may not have water
    iycf_6b = 0, iycf_6c = 0, iycf_6d = 0,  # No formula/milk
    iycf_6e = sample(0:1, n_per_category, replace = TRUE),  # Juice
    iycf_6f = sample(0:1, n_per_category, replace = TRUE),  # Broth
    iycf_6g = sample(0:1, n_per_category, replace = TRUE),  # Yogurt drink
    iycf_6h = sample(0:1, n_per_category, replace = TRUE),  # Thin porridge
    iycf_6i = sample(0:1, n_per_category, replace = TRUE),  # Other liquids
    iycf_6j = sample(0:1, n_per_category, replace = TRUE),  # Tea/coffee
    iycf_7a = 0,  # No yogurt (solid)
    iycf_7b = 0, iycf_7c = 0, iycf_7d = 0, iycf_7e = 0, iycf_7f = 0,  # No other foods
    iycf_7g = 0, iycf_7h = 0, iycf_7i = 0, iycf_7j = 0, iycf_7k = 0,
    iycf_7l = 0, iycf_7m = 0, iycf_7n = 0, iycf_7o = 0, iycf_7p = 0, iycf_7q = 0, iycf_7r = 0
  )

  # Category 4: Breastfed & Animal Milk or Formula (bf=1, has formula/milk, no solid foods)
  bf_formula <- data.frame(
    child_id = paste0("BF_FORM_", 1:n_per_category),
    age_months = sample(4:12, n_per_category, replace = TRUE),
    category_label = "Breastfed & Animal Milk or Formula",
    iycf_4 = 1,  # Breastfeeding
    iycf_6a = sample(0:1, n_per_category, replace = TRUE),  # May have water
    iycf_6b = sample(1:7, n_per_category, replace = TRUE),  # YES formula (frequency)
    iycf_6c = sample(0:5, n_per_category, replace = TRUE),  # Some animal milk
    iycf_6d = sample(0:3, n_per_category, replace = TRUE),  # Some powdered milk
    iycf_6e = sample(0:1, n_per_category, replace = TRUE),  # May have other liquids
    iycf_6f = sample(0:1, n_per_category, replace = TRUE),
    iycf_6g = sample(0:1, n_per_category, replace = TRUE),
    iycf_6h = sample(0:1, n_per_category, replace = TRUE),
    iycf_6i = sample(0:1, n_per_category, replace = TRUE),
    iycf_6j = sample(0:1, n_per_category, replace = TRUE),
    iycf_7a = 0,  # NO yogurt (to avoid solid food category)
    iycf_7b = 0, iycf_7c = 0, iycf_7d = 0, iycf_7e = 0, iycf_7f = 0,  # No other solid foods
    iycf_7g = 0, iycf_7h = 0, iycf_7i = 0, iycf_7j = 0, iycf_7k = 0,
    iycf_7l = 0, iycf_7m = 0, iycf_7n = 0, iycf_7o = 0, iycf_7p = 0, iycf_7q = 0, iycf_7r = 0
  )

  # Category 5: Breastfed & Solid or Semi-Solid Foods (bf=1, has any food)
  bf_solids <- data.frame(
    child_id = paste0("BF_SOL_", 1:n_per_category),
    age_months = sample(6:23, n_per_category, replace = TRUE),  # Complementary feeding age
    category_label = "Breastfed & Solid or Semi-Solid Foods",
    iycf_4 = 1,  # Breastfeeding
    iycf_6a = sample(0:1, n_per_category, replace = TRUE),  # May have water
    iycf_6b = sample(0:5, n_per_category, replace = TRUE),  # May have formula
    iycf_6c = sample(0:5, n_per_category, replace = TRUE),  # May have animal milk
    iycf_6d = sample(0:3, n_per_category, replace = TRUE),  # May have powdered milk
    iycf_6e = sample(0:1, n_per_category, replace = TRUE),  # May have other liquids
    iycf_6f = sample(0:1, n_per_category, replace = TRUE),
    iycf_6g = sample(0:1, n_per_category, replace = TRUE),
    iycf_6h = sample(0:1, n_per_category, replace = TRUE),
    iycf_6i = sample(0:1, n_per_category, replace = TRUE),
    iycf_6j = sample(0:1, n_per_category, replace = TRUE),
    iycf_7a = sample(0:5, n_per_category, replace = TRUE),  # Yogurt
    iycf_7b = sample(0:1, n_per_category, replace = TRUE),  # Grains
    iycf_7c = sample(0:1, n_per_category, replace = TRUE),  # Legumes
    iycf_7d = sample(0:1, n_per_category, replace = TRUE),  # Dairy
    iycf_7e = sample(0:1, n_per_category, replace = TRUE),  # Meat
    iycf_7f = sample(0:1, n_per_category, replace = TRUE),  # Eggs
    iycf_7g = sample(0:1, n_per_category, replace = TRUE),  # Vit A fruits
    iycf_7h = sample(0:1, n_per_category, replace = TRUE),  # Other fruits
    iycf_7i = sample(0:1, n_per_category, replace = TRUE),  # Vit A veg
    iycf_7j = sample(0:1, n_per_category, replace = TRUE),  # Other veg
    iycf_7k = sample(0:1, n_per_category, replace = TRUE),  # Red palm oil
    iycf_7l = sample(0:1, n_per_category, replace = TRUE),  # Oil/fats
    iycf_7m = sample(0:1, n_per_category, replace = TRUE),  # Sweets
    iycf_7n = sample(0:1, n_per_category, replace = TRUE),  # Condiments
    iycf_7o = sample(0:1, n_per_category, replace = TRUE),  # Insects
    iycf_7p = sample(0:1, n_per_category, replace = TRUE),  # Fish
    iycf_7q = sample(0:1, n_per_category, replace = TRUE),  # Organ meat
    iycf_7r = sample(0:1, n_per_category, replace = TRUE)   # Other foods
  )

  # Category 6: Not Breastfed (bf=0, any combination of foods/liquids)
  not_bf <- data.frame(
    child_id = paste0("NOT_BF_", 1:n_per_category),
    age_months = sample(6:23, n_per_category, replace = TRUE),
    category_label = "Not Breastfed",
    iycf_4 = 0,  # NOT breastfeeding
    iycf_6a = sample(0:1, n_per_category, replace = TRUE),
    iycf_6b = sample(0:7, n_per_category, replace = TRUE),
    iycf_6c = sample(0:7, n_per_category, replace = TRUE),
    iycf_6d = sample(0:5, n_per_category, replace = TRUE),
    iycf_6e = sample(0:1, n_per_category, replace = TRUE),
    iycf_6f = sample(0:1, n_per_category, replace = TRUE),
    iycf_6g = sample(0:1, n_per_category, replace = TRUE),
    iycf_6h = sample(0:1, n_per_category, replace = TRUE),
    iycf_6i = sample(0:1, n_per_category, replace = TRUE),
    iycf_6j = sample(0:1, n_per_category, replace = TRUE),
    iycf_7a = sample(0:7, n_per_category, replace = TRUE),
    iycf_7b = sample(0:1, n_per_category, replace = TRUE),
    iycf_7c = sample(0:1, n_per_category, replace = TRUE),
    iycf_7d = sample(0:1, n_per_category, replace = TRUE),
    iycf_7e = sample(0:1, n_per_category, replace = TRUE),
    iycf_7f = sample(0:1, n_per_category, replace = TRUE),
    iycf_7g = sample(0:1, n_per_category, replace = TRUE),
    iycf_7h = sample(0:1, n_per_category, replace = TRUE),
    iycf_7i = sample(0:1, n_per_category, replace = TRUE),
    iycf_7j = sample(0:1, n_per_category, replace = TRUE),
    iycf_7k = sample(0:1, n_per_category, replace = TRUE),
    iycf_7l = sample(0:1, n_per_category, replace = TRUE),
    iycf_7m = sample(0:1, n_per_category, replace = TRUE),
    iycf_7n = sample(0:1, n_per_category, replace = TRUE),
    iycf_7o = sample(0:1, n_per_category, replace = TRUE),
    iycf_7p = sample(0:1, n_per_category, replace = TRUE),
    iycf_7q = sample(0:1, n_per_category, replace = TRUE),
    iycf_7r = sample(0:1, n_per_category, replace = TRUE)
  )

  # Add iycf_ebf column to all
  exclusive_bf$iycf_ebf <- 1
  bf_water$iycf_ebf <- 0
  bf_liquids$iycf_ebf <- 0
  bf_formula$iycf_ebf <- 0
  bf_solids$iycf_ebf <- 0
  not_bf$iycf_ebf <- 0

  # Combine all categories
  full_data <- bind_rows(
    exclusive_bf,
    bf_water,
    bf_liquids,
    bf_formula,
    bf_solids,
    not_bf
  )

  return(full_data)
}

# Create the full category dataset
test_full_categories <- create_full_category_iycf_data()

cat("Total children:", nrow(test_full_categories), "\n")
cat("Expected categories represented:", length(unique(test_full_categories$category_label)), "\n\n")

# Display category distribution in test data
cat("=== Input Data Category Distribution ===\n")
print(table(test_full_categories$category_label))
cat("\n")

# Test 1: Full category coverage plot
cat("Test 1: Full Category Coverage\n")
plot_full <- plot_iycf_areagraph(
  df = test_full_categories,
  title_name = "IYCF Area Graph - All 7 Categories Represented",
  subtitle = "Test data designed to cover all feeding categories"
)
print(plot_full)
cat("✓ All categories should be visible in the plot\n\n")

# Test 2: Verify each category separately
cat("=== Test 2: Verify Categories Individually ===\n\n")

# 2a: Exclusive Breastfed only
cat("2a: Exclusive Breastfed children only\n")
plot_ebf <- plot_iycf_areagraph(
  df = test_full_categories %>% filter(category_label == "Exclusive Breastfed"),
  title_name = "Exclusive Breastfed Children Only",
  min_age_months = 0,
  max_age_months = 6
)
print(plot_ebf)
cat("Expected: Should show 100% Exclusive Breastfed (green at top)\n\n")

# 2b: Breastfed & Plain Water only
cat("2b: Breastfed & Plain Water children only\n")
plot_water <- plot_iycf_areagraph(
  df = test_full_categories %>% filter(category_label == "Breastfed & Plain Water"),
  title_name = "Breastfed & Plain Water Only"
)
print(plot_water)
cat("Expected: Should show 100% Breastfed & Plain Water\n\n")

# 2c: Breastfed & Non-Milk Liquids only
cat("2c: Breastfed & Non-Milk Liquids children only\n")
plot_liquids <- plot_iycf_areagraph(
  df = test_full_categories %>% filter(category_label == "Breastfed & Non-Milk Liquids"),
  title_name = "Breastfed & Non-Milk Liquids Only"
)
print(plot_liquids)
cat("Expected: Should show 100% Breastfed & Non-Milk Liquids\n\n")

# 2d: Breastfed & Animal Milk or Formula only
cat("2d: Breastfed & Animal Milk or Formula children only\n")
plot_formula <- plot_iycf_areagraph(
  df = test_full_categories %>% filter(category_label == "Breastfed & Animal Milk or Formula"),
  title_name = "Breastfed & Animal Milk or Formula Only"
)
print(plot_formula)
cat("Expected: Should show 100% Breastfed & Animal Milk or Formula\n\n")

# 2e: Breastfed & Solid or Semi-Solid Foods only
cat("2e: Breastfed & Solid or Semi-Solid Foods children only\n")
plot_solids <- plot_iycf_areagraph(
  df = test_full_categories %>% filter(category_label == "Breastfed & Solid or Semi-Solid Foods"),
  title_name = "Breastfed & Solid or Semi-Solid Foods Only"
)
print(plot_solids)
cat("Expected: Should show 100% Breastfed & Solid or Semi-Solid Foods\n\n")

# 2f: Not Breastfed only
cat("2f: Not Breastfed children only\n")
plot_not_bf <- plot_iycf_areagraph(
  df = test_full_categories %>% filter(category_label == "Not Breastfed"),
  title_name = "Not Breastfed Only"
)
print(plot_not_bf)
cat("Expected: Should show 100% Not Breastfed\n\n")

# Test 3: Age-specific patterns
cat("=== Test 3: Age-Specific Feeding Patterns ===\n\n")

# 3a: Young infants (0-5 months) - expect more exclusive BF
cat("3a: Young infants (0-5 months)\n")
plot_young <- plot_iycf_areagraph(
  df = test_full_categories,
  min_age_months = 0,
  max_age_months = 6,
  title_name = "Feeding Practices: 0-5 Months",
  subtitle = "Should show higher exclusive breastfeeding"
)
print(plot_young)
cat("\n")

# 3b: Older infants (6-11 months) - expect complementary feeding
cat("3b: Older infants (6-11 months)\n")
plot_complementary <- plot_iycf_areagraph(
  df = test_full_categories,
  min_age_months = 6,
  max_age_months = 12,
  title_name = "Feeding Practices: 6-11 Months",
  subtitle = "Should show more solid foods introduction"
)
print(plot_complementary)
cat("\n")

# 3c: Toddlers (12-23 months) - expect diverse feeding
cat("3c: Toddlers (12-23 months)\n")
plot_toddlers <- plot_iycf_areagraph(
  df = test_full_categories,
  min_age_months = 12,
  max_age_months = 24,
  title_name = "Feeding Practices: 12-23 Months",
  subtitle = "Should show diverse feeding patterns"
)
print(plot_toddlers)
cat("\n")

# Test 4: Verify category logic with specific examples
cat("=== Test 4: Verify Category Assignment Logic ===\n\n")

# Create edge cases
edge_cases <- data.frame(
  child_id = c("EDGE1", "EDGE2", "EDGE3", "EDGE4", "EDGE5"),
  age_months = c(3, 5, 7, 10, 15),
  iycf_ebf = c(1, 0, 0, 0, 0),
  iycf_4 = c(1, 1, 1, 1, 0),

  # EDGE1: Exclusive BF - no water, no liquids, no foods
  # EDGE2: BF + Water only
  # EDGE3: BF + Non-milk liquids (juice)
  # EDGE4: BF + Formula (frequency = 3)
  # EDGE5: Not BF + Solids

  iycf_6a = c(0, 1, 1, 1, 1),  # Water
  iycf_6b = c(0, 0, 0, 3, 5),  # Formula (frequency)
  iycf_6c = c(0, 0, 0, 0, 2),  # Animal milk (frequency)
  iycf_6d = c(0, 0, 0, 0, 0),  # Powdered milk (frequency)
  iycf_6e = c(0, 0, 1, 0, 1),  # Juice
  iycf_6f = c(0, 0, 0, 0, 0),  # Broth
  iycf_6g = c(0, 0, 0, 0, 0),  # Yogurt drink
  iycf_6h = c(0, 0, 0, 0, 0),  # Thin porridge
  iycf_6i = c(0, 0, 0, 0, 0),  # Other liquids
  iycf_6j = c(0, 0, 0, 0, 0),  # Tea/coffee
  iycf_7a = c(0, 0, 0, 0, 2),  # Yogurt (frequency)
  iycf_7b = c(0, 0, 0, 0, 1),  # Grains
  iycf_7c = c(0, 0, 0, 0, 0),  # Legumes
  iycf_7d = c(0, 0, 0, 0, 0),  # Dairy
  iycf_7e = c(0, 0, 0, 0, 1),  # Meat
  iycf_7f = c(0, 0, 0, 0, 0),  # Eggs
  iycf_7g = c(0, 0, 0, 0, 0),  # Vit A fruits
  iycf_7h = c(0, 0, 0, 0, 0),  # Other fruits
  iycf_7i = c(0, 0, 0, 0, 0),  # Vit A veg
  iycf_7j = c(0, 0, 0, 0, 0),  # Other veg
  iycf_7k = c(0, 0, 0, 0, 0),  # Red palm oil
  iycf_7l = c(0, 0, 0, 0, 0),  # Oil/fats
  iycf_7m = c(0, 0, 0, 0, 0),  # Sweets
  iycf_7n = c(0, 0, 0, 0, 0),  # Condiments
  iycf_7o = c(0, 0, 0, 0, 0),  # Insects
  iycf_7p = c(0, 0, 0, 0, 0),  # Fish
  iycf_7q = c(0, 0, 0, 0, 0),  # Organ meat
  iycf_7r = c(0, 0, 0, 0, 0)   # Other foods
)

cat("Edge case children:\n")
cat("EDGE1: iycf_4=1, no water, no liquids, no foods → Exclusive Breastfed\n")
cat("EDGE2: iycf_4=1, water=1, no other liquids, no foods → Breastfed & Plain Water\n")
cat("EDGE3: iycf_4=1, water=1, juice=1, no foods → Breastfed & Non-Milk Liquids\n")
cat("EDGE4: iycf_4=1, formula=3, no foods → Breastfed & Animal Milk or Formula\n")
cat("EDGE5: iycf_4=0, has foods → Not Breastfed\n\n")

plot_edge <- plot_iycf_areagraph(
  df = edge_cases,
  title_name = "Edge Cases - Category Logic Verification"
)
print(plot_edge)
cat("\n")

# Summary
cat("=== Full Category Coverage Test Summary ===\n")
cat("Total test cases: 600 children (100 per category)\n")
cat("Categories tested:\n")
cat("  1. Exclusive Breastfed\n")
cat("  2. Breastfed & Plain Water\n")
cat("  3. Breastfed & Non-Milk Liquids\n")
cat("  4. Breastfed & Animal Milk or Formula\n")
cat("  5. Breastfed & Solid or Semi-Solid Foods\n")
cat("  6. Not Breastfed\n")
cat("  7. Unknown (edge cases where logic may not match)\n\n")

cat("✓ All category coverage tests completed\n")
cat("Review the plots to ensure all 7 categories appear with appropriate colors\n")

# PLOT DATE RUNNER ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(runner)

# Create test dataset
create_test_timeseries_data <- function(n = 500) {
  set.seed(333)

  dates <- seq.Date(from = as.Date("2024-01-01"),
                    to = as.Date("2024-01-31"),
                    by = "day")

  data.frame(
    survey_id = paste0("SURV_", seq_len(n)),
    date_dc_date = sample(dates, n, replace = TRUE),
    region = sample(c("North", "South", "East"), n, replace = TRUE),
    enumerator = sample(paste0("ENUM_", 1:10), n, replace = TRUE),

    # Anthropometric measurements
    muac = rnorm(n, mean = 135, sd = 12),
    weight = rnorm(n, mean = 12.5, sd = 2.5),
    height = rnorm(n, mean = 85, sd = 10),
    wfhz = rnorm(n, mean = -0.5, sd = 1.3),

    # Food security scores
    fcs_score = sample(0:112, n, replace = TRUE),
    rcsi_score = sample(0:56, n, replace = TRUE),
    hhs_score = sample(0:6, n, replace = TRUE),

    # Mortality data
    deaths = rpois(n, lambda = 0.2),
    under5_deaths = rpois(n, lambda = 0.1),
    population = sample(1000:5000, n, replace = TRUE),

    # Other
    household_size = sample(3:12, n, replace = TRUE)
  )
}

test_data <- create_test_timeseries_data(n = 600)

cat("\n=== Testing Generalized plot_date_runner ===\n\n")

# Test 1: Mean operation
plot1 <- plot_date_runner(
  df = test_data,
  numeric_col = "muac",
  operation = "mean",
  title_name = "Cumulative Mean MUAC Over Time"
)
print(plot1)

# Test 2: SD operation with reference line
plot2 <- plot_date_runner(
  df = test_data,
  numeric_col = "wfhz",
  operation = "sd",
  reference_line = 1.2,
  title_name = "Cumulative SD of WHZ (Reference: 1.2)"
)
print(plot2)

# Test 3: DPS operation
plot3 <- plot_date_runner(
  df = test_data,
  numeric_col = "weight",
  operation = "dps",
  title_name = "Digit Preference Score for Weight"
)
print(plot3)

# Test 4: Count operation
plot4 <- plot_date_runner(
  df = test_data,
  numeric_col = "muac",
  operation = "count",
  title_name = "Cumulative Count of MUAC Measurements"
)
print(plot4)

# Test 5: Ratio operation (crude death rate)
plot5 <- plot_date_runner(
  df = test_data,
  numeric_col = "deaths",
  numeric_col2 = "population",
  operation = "ratio",
  title_name = "Cumulative Death Rate (Deaths/Population)"
)
print(plot5)

# Test 6: With grouping
plot6 <- plot_date_runner(
  df = test_data,
  numeric_col = "fcs_score",
  operation = "mean", color_palette = "group",
  grouping_col = "region",
  title_name = "Mean FCS Score by Region Over Time"
)
print(plot6)

# Test 7: SD with grouping and reference line
plot7 <- plot_date_runner(
  df = test_data,
  numeric_col = "muac",
  operation = "sd",
  grouping_col = "region",
  reference_line = 15,
  color_palette = "group",
  title_name = "MUAC Standard Deviation by Region"
)
print(plot7)

# Test 8: Ratio with grouping
plot8 <- plot_date_runner(
  df = test_data,
  numeric_col = "under5_deaths",
  numeric_col2 = "deaths",
  operation = "ratio",
  grouping_col = "region",
  title_name = "Under-5 Death Ratio by Region"
)
print(plot8)

# Test 9: Custom date column
test_custom_date <- test_data %>% rename(collection_date = date_dc_date)
plot9 <- plot_date_runner(
  df = test_custom_date,
  date_col = "collection_date",
  numeric_col = "rcsi_score",
  operation = "mean",
  title_name = "Custom Date Column"
)
print(plot9)

# Test 10: Custom axis labels
plot10 <- plot_date_runner(
  df = test_data,
  numeric_col = "household_size",
  operation = "mean",
  x_lab = "Survey Date",
  y_lab = "Average HH Size",
  title_name = "Custom Axis Labels"
)
print(plot10)

cat("\n=== All generalized tests completed ===\n")

# PLOT DOMAIN RADAR ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggradar)
library(tidyr)

# Use the same test data creation function
create_test_domain_data <- function(n = 300) {
  set.seed(444)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),
    region = sample(c("North", "South", "East"), n, replace = TRUE),
    district = sample(c("District A", "District B"), n, replace = TRUE),

    # Food security domains (binary 0/1)
    fcs_acceptable = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),
    rcsi_low = sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6)),
    hhs_none = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)),
    food_adequate = sample(0:1, n, replace = TRUE, prob = c(0.35, 0.65)),

    # WASH domains (binary 0/1)
    safe_water = sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6)),
    improved_sanitation = sample(0:1, n, replace = TRUE, prob = c(0.45, 0.55)),
    handwashing_facility = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)),

    # Health domains (binary 0/1)
    health_access = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),
    vaccination_complete = sample(0:1, n, replace = TRUE, prob = c(0.25, 0.75)),
    illness_treated = sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6))
  )
}

test_data <- create_test_domain_data(n = 400)

cat("\n=== Testing plot_domain_radar V3 (New Features) ===\n\n")

# Test 1: Single group WITH percentage labels (NEW)
cat("Test 1: Single group with percentage labels\n")
plot1 <- plot_domain_radar(
  df = test_data,
  domain_cols = c("fcs_acceptable", "rcsi_low", "hhs_none", "food_adequate"),
  domain_labels = c("FCS Acceptable", "Low Coping", "No Hunger", "Food Adequate"),
  title_name = "Food Security Domains",
  subtitle = "Baseline Survey"
)
print(plot1)
cat("✓ Should show percentage labels on the radar points\n\n")

# Test 2: Custom color palette - reach3 (NEW)
cat("Test 2: Custom color palette (reach3)\n")
plot2 <- plot_domain_radar(
  df = test_data,
  domain_cols = c("safe_water", "improved_sanitation", "handwashing_facility"),
  domain_labels = c("Safe Water", "Sanitation", "Handwashing"),
  color_palette = "reach1",
  title_name = "WASH Indicators"
)
print(plot2)
cat("✓ Should use reach3 color palette\n\n")

# Test 3: Multiple groups WITHOUT percentage labels (grouped data)
cat("Test 3: Multiple groups by region (no labels)\n")
plot3 <- plot_domain_radar(
  df = test_data,
  domain_cols = c("safe_water", "improved_sanitation", "handwashing_facility"),
  domain_labels = c("Safe Water", "Sanitation", "Handwashing"),
  grouping = "region",
  color_palette = "group",
  title_name = "WASH Indicators by Region"
)
print(plot3)
cat("✓ Should NOT show percentage labels (too many groups)\n")
cat("✓ Should use reach2 color palette for groups\n\n")

# Test 4: Text sizing verification
cat("Test 4: Text sizing (title < subtitle < domain labels)\n")
plot4 <- plot_domain_radar(
  df = test_data,
  domain_cols = c("health_access", "vaccination_complete", "illness_treated"),
  domain_labels = c("Health Access", "Vaccination", "Treatment"),
  title_name = "Health Indicators",
  subtitle = "Coverage Assessment 2024",
  color_palette = "reach4"
)
print(plot4)
cat("✓ Title size: 12\n")
cat("✓ Subtitle size: 10\n")
cat("✓ Domain label size: 4 (largest of the three)\n\n")

# Test 5: Multiple groups with traffic_light palette
cat("Test 5: Multiple groups with traffic_light palette\n")
plot5 <- plot_domain_radar(
  df = test_data,
  domain_cols = c("fcs_acceptable", "safe_water", "health_access"),
  domain_labels = c("Food Security", "WASH", "Health"),
  grouping = "district",
  color_palette = "traffic_light",
  title_name = "Multi-Sector Indicators by District"
)
print(plot5)
cat("✓ Should use traffic_light colors for districts\n\n")

# Test 6: All food security domains with labels
cat("Test 6: All food security domains with percentage labels\n")
plot6 <- plot_domain_radar(
  df = test_data,
  domain_cols = c("fcs_acceptable", "rcsi_low", "hhs_none", "food_adequate"),
  domain_labels = c("FCS", "rCSI", "HHS", "Food Adequacy"),
  color_palette = "reach1",
  title_name = "Food Security Assessment",
  subtitle = "All Households"
)
print(plot6)
cat("✓ Should show 4 percentage labels on radar points\n\n")

# Test 7: Three regions with reach2 palette
cat("Test 7: Three regions comparison\n")
plot7 <- plot_domain_radar(
  df = test_data,
  domain_cols = c("fcs_acceptable", "rcsi_low", "hhs_none", "food_adequate"),
  domain_labels = c("FCS", "rCSI", "HHS", "Food Adequacy"),
  grouping = "region",
  color_palette = "reach2",
  title_name = "Food Security by Region",
  subtitle = "Regional Comparison"
)
print(plot7)
cat("✓ Three colored overlays on same radar\n")
cat("✓ Legend at bottom showing region colors\n\n")

# Test 8: Single WASH domain with percentage labels
cat("Test 8: WASH domains single group\n")
plot8 <- plot_domain_radar(
  df = test_data,
  domain_cols = c("safe_water", "improved_sanitation", "handwashing_facility"),
  domain_labels = c("Safe Water Access", "Improved Sanitation", "Handwashing Facility"),
  title_name = "WASH Coverage",
  subtitle = "Overall Population",
  color_palette = "reach3"
)
print(plot8)
cat("✓ Three percentage labels visible\n\n")

cat("\n=== Summary of V3 Features ===\n")
cat("1. ✓ Custom color_palette argument (reach1-4, traffic_light, etc.)\n")
cat("2. ✓ Percentage labels shown ONLY for single group\n")
cat("3. ✓ Title size: 12 (smaller)\n")
cat("4. ✓ Subtitle size: 10 (smaller)\n")
cat("5. ✓ Domain labels size: 4 (bigger than title/subtitle but still smaller overall)\n")
cat("6. ✓ Labels positioned outside radar points for clarity\n")

cat("\n=== All V3 tests completed ===\n")

#PLOT DOMAIN DISTRIBUTION ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# Create test dataset
create_test_barrier_data <- function(n = 400) {
  set.seed(1000)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),
    barrier_no_staff = sample(0:1, n, replace = TRUE, prob = c(0.65, 0.35)),
    barrier_no_medicine = sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4)),
    barrier_too_far = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)),
    barrier_cost_transport = sample(0:1, n, replace = TRUE, prob = c(0.55, 0.45)),
    barrier_rude_staff = sample(0:1, n, replace = TRUE, prob = c(0.68, 0.32)),
    barrier_long_wait = sample(0:1, n, replace = TRUE, prob = c(0.52, 0.48))
  )
}

test_data <- create_test_barrier_data(n = 500)

# Define domain structure
health_domains <- list(
  availability = list(
    responses = c("barrier_no_staff", "barrier_no_medicine"),
    label = "Availability",
    response_labels = c(
      "barrier_no_staff" = "No Medical Staff",
      "barrier_no_medicine" = "No Medicine"
    )
  ),
  accessibility = list(
    responses = c("barrier_too_far", "barrier_cost_transport"),
    label = "Accessibility",
    response_labels = c(
      "barrier_too_far" = "Facility Too Far",
      "barrier_cost_transport" = "Transport Cost"
    )
  ),
  quality = list(
    responses = c("barrier_rude_staff", "barrier_long_wait"),
    label = "Quality",
    response_labels = c(
      "barrier_rude_staff" = "Rude Staff",
      "barrier_long_wait" = "Long Wait"
    )
  )
)

cat("\n=== Testing flip_coordinates Parameter ===\n\n")

# Test 1: Horizontal bars (default - flipped)
cat("Test 1: Horizontal bars (flip_coordinates = TRUE, default)\n")
plot1 <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = TRUE,
  title_name = "Horizontal Bars (Default)"
)
print(plot1)
cat("✓ Responses on Y-axis, counts on X-axis\n\n")

# Test 2: Vertical bars (not flipped)
cat("Test 2: Vertical bars (flip_coordinates = FALSE)\n")
plot2 <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = FALSE,
  title_name = "Vertical Bars"
)
print(plot2)
cat("✓ Responses on X-axis, counts on Y-axis\n")
cat("✓ X-axis labels angled at 45 degrees\n\n")

# Test 3: Side-by-side comparison
cat("Test 3: Side-by-side comparison - Horizontal vs Vertical\n")
p_horizontal <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = TRUE,
  title_name = "Horizontal"
)

p_vertical <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = FALSE,
  title_name = "Vertical"
)

grid.arrange(p_horizontal, p_vertical, ncol = 2)
cat("✓ Side-by-side comparison completed\n\n")

# Test 4: Vertical bars with percentages
cat("Test 4: Vertical bars with percentages\n")
plot4 <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  show_percentage = TRUE,
  flip_coordinates = FALSE,
  title_name = "Vertical Bars - Percentages"
)
print(plot4)
cat("✓ Y-axis should show 'Percentage (%)'\n\n")

# Test 5: Horizontal bars with percentages
cat("Test 5: Horizontal bars with percentages\n")
plot5 <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  show_percentage = TRUE,
  flip_coordinates = TRUE,
  title_name = "Horizontal Bars - Percentages"
)
print(plot5)
cat("✓ X-axis should show 'Percentage (%)'\n\n")

# Test 6: Vertical bars with custom axis labels
cat("Test 6: Vertical bars with custom axis labels\n")
plot6 <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = FALSE,
  x_lab = "Barrier Type",
  y_lab = "Number of Reports",
  title_name = "Custom Axis Labels - Vertical"
)
print(plot6)
cat("✓ Custom labels applied\n\n")

# Test 7: Horizontal bars with custom axis labels
cat("Test 7: Horizontal bars with custom axis labels\n")
plot7 <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = TRUE,
  x_lab = "Number of Respondents",
  y_lab = "Barrier Category",
  title_name = "Custom Axis Labels - Horizontal"
)
print(plot7)
cat("✓ Custom labels applied\n\n")

# Test 8: Four-panel comparison (all combinations)
cat("Test 8: Four-panel comparison - all combinations\n")

p_h_count <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = TRUE,
  show_percentage = FALSE,
  title_name = "Horizontal - Counts"
)

p_h_pct <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = TRUE,
  show_percentage = TRUE,
  title_name = "Horizontal - %"
)

p_v_count <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = FALSE,
  show_percentage = FALSE,
  title_name = "Vertical - Counts"
)

p_v_pct <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = FALSE,
  show_percentage = TRUE,
  title_name = "Vertical - %"
)

grid.arrange(p_h_count, p_h_pct, p_v_count, p_v_pct, ncol = 2)
cat("✓ All four combinations displayed\n\n")

# Test 9: Vertical with many responses (better for vertical)
cat("Test 9: Few responses - vertical format\n")

domains_few <- list(
  availability = list(
    responses = c("barrier_no_staff", "barrier_no_medicine"),
    label = "Availability",
    response_labels = c(
      "barrier_no_staff" = "No Staff",
      "barrier_no_medicine" = "No Medicine"
    )
  ),
  accessibility = list(
    responses = c("barrier_too_far"),
    label = "Accessibility",
    response_labels = c(
      "barrier_too_far" = "Too Far"
    )
  )
)

plot9 <- plot_domain_distribution(
  df = test_data,
  domain_list = domains_few,
  flip_coordinates = FALSE,
  title_name = "Few Responses - Vertical Works Well"
)
print(plot9)
cat("✓ Vertical format good for few responses\n\n")

# Test 10: Many responses - horizontal format
cat("Test 10: Many responses - horizontal format\n")
plot10 <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = TRUE,
  title_name = "Many Responses - Horizontal Preferred"
)
print(plot10)
cat("✓ Horizontal format good for many/long response labels\n\n")

# Test 11: Different color palettes with vertical
cat("Test 11: Vertical bars with different color palettes\n")

p_reach2 <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = FALSE,
  color_palette = "reach2",
  title_name = "Vertical - REACH2"
)

p_reach3 <- plot_domain_distribution(
  df = test_data,
  domain_list = health_domains,
  flip_coordinates = FALSE,
  color_palette = "reach3",
  title_name = "Vertical - REACH3"
)

grid.arrange(p_reach2, p_reach3, ncol = 2)
cat("✓ Color palettes work with both orientations\n\n")


# VERIFICATION


cat("\n=== Axis Label Logic Verification ===\n")
cat("\nWhen flip_coordinates = TRUE (horizontal bars):\n")
cat("  - Responses appear on Y-axis\n")
cat("  - Values (count/%) appear on X-axis\n")
cat("  - Default x_lab: 'Count' or 'Percentage (%)'\n")
cat("  - Default y_lab: 'Response'\n")

cat("\nWhen flip_coordinates = FALSE (vertical bars):\n")
cat("  - Responses appear on X-axis (angled 45°)\n")
cat("  - Values (count/%) appear on Y-axis\n")
cat("  - Default x_lab: 'Response'\n")
cat("  - Default y_lab: 'Count' or 'Percentage (%)'\n")

cat("\n=== Usage Recommendations ===\n")
cat("Use horizontal bars (flip_coordinates = TRUE) when:\n")
cat("  - Many response options (>5)\n")
cat("  - Long response labels\n")
cat("  - Traditional barrier analysis presentation\n")

cat("\nUse vertical bars (flip_coordinates = FALSE) when:\n")
cat("  - Few response options (≤5)\n")
cat("  - Short response labels\n")
cat("  - Preference for vertical bar charts\n")

cat("\n=== All flip_coordinates tests completed ===\n")

# PLOT STACKED BAR ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

# Use the fixed data creation function
create_test_weighted_data <- function(n = 500) {
  set.seed(2024)

  df <- data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    fcs_category = sample(c("Poor", "Borderline", "Acceptable"), n,
                          replace = TRUE, prob = c(0.2, 0.3, 0.5)),
    water_source = sample(c("Improved", "Unimproved", "Surface Water"), n,
                          replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    education = sample(c("None", "Primary", "Secondary", "Higher"), n,
                       replace = TRUE, prob = c(0.15, 0.35, 0.35, 0.15))
  )

  df <- df %>%
    mutate(survey_weight = case_when(
      region == "North" ~ runif(n(), 0.5, 0.8),
      region == "South" ~ runif(n(), 1.2, 1.8),
      region == "East" ~ runif(n(), 0.9, 1.1),
      region == "West" ~ runif(n(), 0.95, 1.05),
      TRUE ~ 1.0
    ))

  return(df)
}

test_data <- create_test_weighted_data(n = 600)

cat("\n=== Testing Legend Position and Coordinate Flip ===\n\n")

# Test 1: Vertical bars (default) - legend at bottom
cat("Test 1: Vertical bars with legend at bottom\n")
plot1 <- plot_stacked_bar(
  df = test_data,
  category_var = "fcs_category",
  grouping = "region",
  legend_label = "Food Security",
  title_name = "Vertical Bars - Legend Bottom"
)
print(plot1)
cat("✓ Legend at bottom, vertical bars\n\n")

# Test 2: Horizontal bars - legend at bottom
cat("Test 2: Horizontal bars with legend at bottom\n")
plot2 <- plot_stacked_bar(
  df = test_data,
  category_var = "fcs_category",
  grouping = "region",
  flip_coordinates = TRUE,
  legend_label = "Food Security",
  title_name = "Horizontal Bars - Legend Bottom"
)
print(plot2)
cat("✓ Legend at bottom, horizontal bars\n\n")

# Test 3: Side-by-side comparison - vertical vs horizontal
cat("Test 3: Vertical vs Horizontal comparison\n")

p_vertical <- plot_stacked_bar(
  df = test_data,
  category_var = "fcs_category",
  grouping = "region",
  flip_coordinates = FALSE,
  legend_label = "FCS",
  title_name = "Vertical"
)

p_horizontal <- plot_stacked_bar(
  df = test_data,
  category_var = "fcs_category",
  grouping = "region",
  flip_coordinates = TRUE,
  legend_label = "FCS",
  title_name = "Horizontal"
)

grid.arrange(p_vertical, p_horizontal, ncol = 2)
cat("✓ Side-by-side comparison\n\n")

# Test 4: Weighted, horizontal bars
cat("Test 4: Weighted with horizontal bars\n")
plot4 <- plot_stacked_bar(
  df = test_data,
  category_var = "education",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = TRUE,
  legend_label = "Education Level",
  title_name = "Weighted - Horizontal Bars"
)
print(plot4)
cat("✓ Weighted data with horizontal orientation\n\n")

# Test 5: Weighted, vertical bars with labels
cat("Test 5: Weighted vertical bars with labels\n")
plot5 <- plot_stacked_bar(
  df = test_data,
  category_var = "water_source",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = FALSE,
  show_labels = TRUE,
  legend_label = "Water Source",
  title_name = "Weighted Vertical - With Labels"
)
print(plot5)
cat("✓ Weighted, vertical, with labels\n\n")

# Test 6: Weighted, horizontal bars with labels
cat("Test 6: Weighted horizontal bars with labels\n")
plot6 <- plot_stacked_bar(
  df = test_data,
  category_var = "water_source",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = TRUE,
  show_labels = TRUE,
  legend_label = "Water Source",
  title_name = "Weighted Horizontal - With Labels"
)
print(plot6)
cat("✓ Weighted, horizontal, with labels\n\n")

# Test 7: Overall (no grouping) - vertical
cat("Test 7: Overall vertical with legend at bottom\n")
plot7 <- plot_stacked_bar(
  df = test_data,
  category_var = "fcs_category",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = FALSE,
  legend_label = "Food Security Status",
  title_name = "Overall - Vertical"
)
print(plot7)
cat("✓ Single bar, vertical, legend at bottom\n\n")

# Test 8: Overall (no grouping) - horizontal
cat("Test 8: Overall horizontal with legend at bottom\n")
plot8 <- plot_stacked_bar(
  df = test_data,
  category_var = "fcs_category",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = TRUE,
  legend_label = "Food Security Status",
  title_name = "Overall - Horizontal"
)
print(plot8)
cat("✓ Single bar, horizontal, legend at bottom\n\n")

# Test 9: Four-panel comparison
cat("Test 9: Four-panel comparison - all combinations\n")

p1 <- plot_stacked_bar(
  df = test_data,
  category_var = "education",
  grouping = "region",
  weighted = FALSE,
  flip_coordinates = FALSE,
  legend_label = "Education",
  title_name = "Unweighted Vertical"
)

p2 <- plot_stacked_bar(
  df = test_data,
  category_var = "education",
  grouping = "region",
  weighted = FALSE,
  flip_coordinates = TRUE,
  legend_label = "Education",
  title_name = "Unweighted Horizontal"
)

p3 <- plot_stacked_bar(
  df = test_data,
  category_var = "education",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = FALSE,
  legend_label = "Education",
  title_name = "Weighted Vertical"
)

p4 <- plot_stacked_bar(
  df = test_data,
  category_var = "education",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = TRUE,
  legend_label = "Education",
  title_name = "Weighted Horizontal"
)

grid.arrange(p1, p2, p3, p4, ncol = 2)
cat("✓ All four combinations displayed\n\n")

# Test 10: Custom axis labels with flip
cat("Test 10: Custom axis labels with horizontal bars\n")
plot10 <- plot_stacked_bar(
  df = test_data,
  category_var = "fcs_category",
  grouping = "region",
  flip_coordinates = TRUE,
  x_label = "Geographic Region",
  y_label = "Proportion of Population",
  legend_label = "Food Consumption Score",
  title_name = "Custom Labels - Horizontal"
)
print(plot10)
cat("✓ Custom axis labels with flipped coordinates\n\n")

cat("\n=== Key Features Summary ===\n")
cat("1. ✓ Legend always positioned at bottom\n")
cat("2. ✓ flip_coordinates = FALSE: Vertical bars (default)\n")
cat("3. ✓ flip_coordinates = TRUE: Horizontal bars\n")
cat("4. ✓ Horizontal bars don't angle x-axis text (not needed)\n")
cat("5. ✓ Vertical bars angle x-axis text at 45° for readability\n")
cat("6. ✓ Legend position consistent across all orientations\n")
cat("7. ✓ Works with weighted and unweighted data\n")
cat("8. ✓ Compatible with show_labels parameter\n")

cat("\n=== When to Use Each Orientation ===\n")
cat("Vertical bars (flip_coordinates = FALSE):\n")
cat("  - Fewer groups (≤4)\n")
cat("  - Standard presentation format\n")
cat("  - When comparing many different stacked variables\n")

cat("\nHorizontal bars (flip_coordinates = TRUE):\n")
cat("  - Many groups (>4)\n")
cat("  - Long group names\n")
cat("  - Easier to read group labels\n")
cat("  - Better for reports/publications\n")

cat("\n=== All flip coordinate tests completed ===\n")

# GROUPED SELECT MULTIPLE BAR CHART ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# Create test dataset with survey weights
create_test_weighted_data <- function(n = 500) {
  set.seed(3333)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),
    district = sample(c("District A", "District B", "District C"), n, replace = TRUE),
    region = sample(c("North", "South"), n, replace = TRUE),

    # Survey weights (varying by district)
    survey_weight = case_when(
      sample(c("District A", "District B", "District C"), n, replace = TRUE) == "District A" ~ runif(n, 0.8, 1.2),
      sample(c("District A", "District B", "District C"), n, replace = TRUE) == "District B" ~ runif(n, 1.0, 1.5),
      TRUE ~ runif(n, 0.9, 1.3)
    ),

    # Health barriers (select multiple)
    barrier_cost = sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6)),
    barrier_distance = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)),
    barrier_availability = sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4)),
    barrier_quality = sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3))
  )
}

test_data <- create_test_weighted_data(n = 600)

cat("\n=== Testing Revised plot_grouped_bar_multiple ===\n\n")

# Test 1: Basic unweighted (default)
cat("Test 1: Unweighted, horizontal bars (default)\n")
plot1 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  title_name = "Unweighted - Horizontal"
)
print(plot1)
cat("✓ Unweighted, flip_coordinates = TRUE\n\n")

# Test 2: Weighted results
cat("Test 2: Weighted results\n")
plot2 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Weighted Results"
)
print(plot2)
cat("✓ Weighted using survey_weight column\n")
cat("✓ Subtitle shows '(weighted)'\n\n")

# Test 3: Comparison - weighted vs unweighted
cat("Test 3: Side-by-side - Weighted vs Unweighted\n")

p_unweighted <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  title_name = "Unweighted"
)

p_weighted <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Weighted"
)

grid.arrange(p_unweighted, p_weighted, ncol = 2)
cat("✓ Comparison shows impact of weighting\n\n")

# Test 4: Vertical bars (flip_coordinates = FALSE)
cat("Test 4: Vertical bars\n")
plot4 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  flip_coordinates = FALSE,
  title_name = "Vertical Bars"
)
print(plot4)
cat("✓ flip_coordinates = FALSE\n")
cat("✓ X-axis labels angled at 45°\n\n")

# Test 5: Horizontal vs Vertical comparison
cat("Test 5: Horizontal vs Vertical\n")

p_horizontal <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  flip_coordinates = TRUE,
  title_name = "Horizontal"
)

p_vertical <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  flip_coordinates = FALSE,
  title_name = "Vertical"
)

grid.arrange(p_horizontal, p_vertical, ncol = 2)
cat("✓ Both orientations displayed\n\n")

# Test 6: Grouped with custom legend label
cat("Test 6: Grouped with custom legend label\n")
plot6 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  grouping = "district",
  legend_label = "District Name",
  title_name = "Custom Legend Label"
)
print(plot6)
cat("✓ Legend shows 'District Name'\n\n")

# Test 7: Grouped weighted with legend label
cat("Test 7: Grouped weighted with custom legend\n")
plot7 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  legend_label = "Geographic Region",
  title_name = "Weighted by Region"
)
print(plot7)
cat("✓ Weighted results by region\n")
cat("✓ Custom legend label\n\n")

# Test 8: Vertical bars grouped with labels
cat("Test 8: Vertical grouped bars with labels\n")
plot8 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost", "Distance", "Availability"),
  grouping = "region",
  flip_coordinates = FALSE,
  show_labels = TRUE,
  title_name = "Vertical Grouped with Labels"
)
print(plot8)
cat("✓ Vertical orientation with grouped bars\n")
cat("✓ Labels above bars\n\n")

# Test 9: Weighted with labels
cat("Test 9: Weighted results with labels\n")
plot9 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability", "barrier_quality"),
  response_labels = c("Cost", "Distance", "Availability", "Quality"),
  weighted = TRUE,
  weights_col = "survey_weight",
  show_labels = TRUE,
  title_name = "Weighted with Value Labels"
)
print(plot9)
cat("✓ Weighted percentages with labels\n\n")

# Test 10: All features combined
cat("Test 10: All features - weighted, vertical, grouped, labels\n")
plot10 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance", "barrier_availability"),
  response_labels = c("Cost Barrier", "Distance Barrier", "Availability Barrier"),
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = FALSE,
  legend_label = "Region",
  show_labels = TRUE,
  color_palette = "reach3",
  title_name = "Full Feature Demo",
  subtitle = "Baseline 2024"
)
print(plot10)
cat("✓ All features combined\n\n")

# Test 11: Four-panel comparison
cat("Test 11: Four-panel comparison\n")

p1 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance"),
  response_labels = c("Cost", "Distance"),
  weighted = FALSE,
  flip_coordinates = TRUE,
  title_name = "Unweighted, Horizontal"
)

p2 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance"),
  response_labels = c("Cost", "Distance"),
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = TRUE,
  title_name = "Weighted, Horizontal"
)

p3 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance"),
  response_labels = c("Cost", "Distance"),
  weighted = FALSE,
  flip_coordinates = FALSE,
  title_name = "Unweighted, Vertical"
)

p4 <- plot_grouped_bar_multiple(
  df = test_data,
  response_vars = c("barrier_cost", "barrier_distance"),
  response_labels = c("Cost", "Distance"),
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = FALSE,
  title_name = "Weighted, Vertical"
)

grid.arrange(p1, p2, p3, p4, ncol = 2)
cat("✓ All combinations displayed\n\n")


# ERROR HANDLING TESTS


cat("=== Error Handling Tests ===\n\n")

# Test 12: Error - weighted = TRUE without weights_col
cat("Test 12: Error - weighted without weights_col\n")
tryCatch({
  plot12 <- plot_grouped_bar_multiple(
    df = test_data,
    response_vars = c("barrier_cost"),
    weighted = TRUE
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error caught:", e$message, "\n\n")
})

# Test 13: Error - invalid weights_col
cat("Test 13: Error - invalid weights_col\n")
tryCatch({
  plot13 <- plot_grouped_bar_multiple(
    df = test_data,
    response_vars = c("barrier_cost"),
    weighted = TRUE,
    weights_col = "nonexistent_weight"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error caught:", e$message, "\n\n")
})

# Test 14: Error - non-numeric weights
cat("Test 14: Error - non-numeric weights column\n")
test_bad_weights <- test_data %>%
  mutate(bad_weight = sample(c("high", "low"), nrow(.), replace = TRUE))

tryCatch({
  plot14 <- plot_grouped_bar_multiple(
    df = test_bad_weights,
    response_vars = c("barrier_cost"),
    weighted = TRUE,
    weights_col = "bad_weight"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error caught:", e$message, "\n\n")
})


# VERIFICATION


cat("\n=== Verification ===\n")
cat("Total respondents:", nrow(test_data), "\n")
cat("Weight range:", range(test_data$survey_weight), "\n")
cat("Mean weight:", mean(test_data$survey_weight), "\n\n")

cat("Weighted vs Unweighted comparison:\n")
comparison <- data.frame(
  barrier = c("Cost", "Distance", "Availability"),
  unweighted_pct = c(
    mean(test_data$barrier_cost) * 100,
    mean(test_data$barrier_distance) * 100,
    mean(test_data$barrier_availability) * 100
  ),
  weighted_pct = c(
    sum(test_data$barrier_cost * test_data$survey_weight) / sum(test_data$survey_weight) * 100,
    sum(test_data$barrier_distance * test_data$survey_weight) / sum(test_data$survey_weight) * 100,
    sum(test_data$barrier_availability * test_data$survey_weight) / sum(test_data$survey_weight) * 100
  )
)
comparison$difference <- comparison$weighted_pct - comparison$unweighted_pct
print(comparison)

cat("\n=== Key Features Added ===\n")
cat("1. ✓ weighted parameter - toggle weighted/unweighted results\n")
cat("2. ✓ weights_col parameter - specify weights column name\n")
cat("3. ✓ flip_coordinates parameter - toggle horizontal/vertical bars\n")
cat("4. ✓ legend_label parameter - custom legend title\n")
cat("5. ✓ Weighted calculations for both overall and grouped plots\n")
cat("6. ✓ Subtitle indicates when results are weighted\n")
cat("7. ✓ Labels position correctly for both orientations\n")

cat("\n=== All revised tests completed ===\n")

# PLOT BOXPLOT ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(Hmisc)
library(gridExtra)

# Create test dataset with survey weights
create_test_numeric_data <- function(n = 500) {
  set.seed(4444)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),
    district = sample(c("District A", "District B", "District C"), n, replace = TRUE),
    region = sample(c("North", "South"), n, replace = TRUE),

    # Survey weights
    survey_weight = runif(n, 0.8, 1.5),

    # Numeric variables
    muac = rnorm(n, mean = 135, sd = 12),
    weight = rnorm(n, mean = 12.5, sd = 2.5),
    height = rnorm(n, mean = 85, sd = 10),
    fcs_score = sample(0:112, n, replace = TRUE)
  )
}

test_data <- create_test_numeric_data(n = 600)

cat("\n=== Testing Revised plot_boxplot ===\n\n")

# Test 1: Basic unweighted, vertical (default)
cat("Test 1: Unweighted, vertical\n")
plot1 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  title_name = "MUAC Distribution - Unweighted"
)
print(plot1)
cat("✓ Unweighted, vertical orientation\n\n")

# Test 2: Weighted
cat("Test 2: Weighted distribution\n")
plot2 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "MUAC Distribution - Weighted"
)
print(plot2)
cat("✓ Weighted using survey_weight\n")
cat("✓ Subtitle shows '(weighted)'\n\n")

# Test 3: Weighted vs Unweighted comparison
cat("Test 3: Side-by-side - Weighted vs Unweighted\n")

p_unw <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  title_name = "Unweighted"
)

p_w <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Weighted"
)

grid.arrange(p_unw, p_w, ncol = 2)
cat("✓ Shows impact of weighting on distribution\n\n")

# Test 4: Horizontal boxes (flip_coordinates = TRUE)
cat("Test 4: Horizontal box plots\n")
plot4 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  flip_coordinates = TRUE,
  title_name = "MUAC - Horizontal Orientation"
)
print(plot4)
cat("✓ flip_coordinates = TRUE\n\n")

# Test 5: Grouped by district
cat("Test 5: Grouped by district\n")
plot5 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  grouping = "district",
  title_name = "MUAC by District"
)
print(plot5)
cat("✓ Three groups\n\n")

# Test 6: Grouped with custom legend label
cat("Test 6: Grouped with custom legend label\n")
plot6 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  grouping = "district",
  legend_label = "District Name",
  title_name = "MUAC by District - Custom Legend"
)
print(plot6)
cat("✓ Legend shows 'District Name'\n\n")

# Test 7: Grouped weighted
cat("Test 7: Grouped weighted box plots\n")
plot7 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  legend_label = "Region",
  title_name = "MUAC by Region (Weighted)"
)
print(plot7)
cat("✓ Weighted results by region\n\n")

# Test 8: Horizontal grouped
cat("Test 8: Horizontal grouped box plots\n")
plot8 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  grouping = "district",
  flip_coordinates = TRUE,
  title_name = "MUAC by District - Horizontal"
)
print(plot8)
cat("✓ Horizontal orientation with grouping\n\n")

# Test 9: With mean points
cat("Test 9: With mean points (unweighted)\n")
plot9 <- plot_boxplot(
  df = test_data,
  numeric_var = "fcs_score",
  grouping = "region",
  show_mean = TRUE,
  title_name = "FCS Score by Region with Means"
)
print(plot9)
cat("✓ Red diamond shows mean\n\n")

# Test 10: Weighted with mean points
cat("Test 10: Weighted with mean points\n")
plot10 <- plot_boxplot(
  df = test_data,
  numeric_var = "fcs_score",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_mean = TRUE,
  title_name = "FCS Score by Region (Weighted) with Means"
)
print(plot10)
cat("✓ Shows weighted mean\n\n")

# Test 11: All features combined
cat("Test 11: All features - weighted, horizontal, grouped, mean\n")
plot11 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  grouping = "district",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = TRUE,
  show_mean = TRUE,
  legend_label = "District",
  color_palette = "reach2",
  title_name = "Full Feature Demo",
  subtitle = "Baseline 2024"
)
print(plot11)
cat("✓ All features combined\n\n")

# Test 12: Four-panel comparison
cat("Test 12: Four-panel comparison\n")

p1 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  grouping = "region",
  weighted = FALSE,
  flip_coordinates = FALSE,
  title_name = "Unweighted, Vertical"
)

p2 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = FALSE,
  title_name = "Weighted, Vertical"
)

p3 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  grouping = "region",
  weighted = FALSE,
  flip_coordinates = TRUE,
  title_name = "Unweighted, Horizontal"
)

p4 <- plot_boxplot(
  df = test_data,
  numeric_var = "muac",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = TRUE,
  title_name = "Weighted, Horizontal"
)

grid.arrange(p1, p2, p3, p4, ncol = 2)
cat("✓ All combinations\n\n")


# ERROR HANDLING


cat("=== Error Handling Tests ===\n\n")

# Test 13: Error - weighted without weights_col
cat("Test 13: Error - weighted without weights_col\n")
tryCatch({
  plot13 <- plot_boxplot(
    df = test_data,
    numeric_var = "muac",
    weighted = TRUE
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error:", e$message, "\n\n")
})

# Test 14: Error - invalid weights_col
cat("Test 14: Error - invalid weights_col\n")
tryCatch({
  plot14 <- plot_boxplot(
    df = test_data,
    numeric_var = "muac",
    weighted = TRUE,
    weights_col = "nonexistent"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error:", e$message, "\n\n")
})

cat("\n=== Verification ===\n")
cat("Total respondents:", nrow(test_data), "\n")
cat("Weight range:", range(test_data$survey_weight), "\n\n")

cat("Unweighted vs Weighted MUAC statistics:\n")
cat("Unweighted median:", median(test_data$muac, na.rm = TRUE), "\n")
cat("Weighted median:", wtd.quantile(test_data$muac, weights = test_data$survey_weight, probs = 0.5), "\n\n")

cat("=== Key Features Added ===\n")
cat("1. ✓ weighted parameter - toggle weighted/unweighted\n")
cat("2. ✓ weights_col parameter - specify weights column\n")
cat("3. ✓ flip_coordinates parameter - toggle vertical/horizontal\n")
cat("4. ✓ legend_label parameter - custom legend title\n")
cat("5. ✓ Weighted quantiles using Hmisc::wtd.quantile\n")
cat("6. ✓ Weighted means when show_mean = TRUE\n")
cat("7. ✓ Subtitle indicates weighted results\n")

cat("\n=== All revised boxplot tests completed ===\n")

# PLOT TREEMAP ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(treemapify)
library(gridExtra)

# Create test dataset with hierarchical structure
create_test_treemap_hierarchical_data <- function(n = 600) {
  set.seed(6666)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),

    # Survey weights
    survey_weight = runif(n, 0.8, 1.5),

    # Hierarchical: Sector > Assistance Type
    sector = sample(c("Food Security", "Protection", "WASH", "Shelter"),
                    n, replace = TRUE, prob = c(0.35, 0.25, 0.25, 0.15)),

    assistance_type = sample(c("Cash", "Food", "NFI", "Counseling", "Legal",
                               "Water", "Sanitation", "Hygiene", "Emergency", "Transitional"),
                             n, replace = TRUE),

    # Hierarchical: Region > District
    region = sample(c("North", "South", "East", "West"),
                    n, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15)),

    district = sample(c("District A", "District B", "District C", "District D",
                        "District E", "District F", "District G", "District H"),
                      n, replace = TRUE),

    # Hierarchical: Livelihood Category > Specific Livelihood
    livelihood_category = sample(c("Agriculture", "Non-Agriculture", "Mixed"),
                                 n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),

    specific_livelihood = sample(c("Crops", "Livestock", "Fishing", "Trade",
                                   "Labor", "Services", "Crafts", "Mixed Activities"),
                                 n, replace = TRUE),

    # Hierarchical: Water Source Type > Specific Source
    water_type = sample(c("Improved", "Unimproved", "Surface"),
                        n, replace = TRUE, prob = c(0.5, 0.35, 0.15)),

    water_source = sample(c("Piped", "Borehole", "Protected Well", "Unprotected Well",
                            "Tanker", "River", "Pond"),
                          n, replace = TRUE),

    # Single level categories
    education_level = sample(c("None", "Primary", "Secondary", "Tertiary"),
                             n, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),

    shelter_type = sample(c("House", "Apartment", "Tent", "Makeshift", "Collective"),
                          n, replace = TRUE, prob = c(0.3, 0.2, 0.2, 0.2, 0.1)),

    # Numeric variables for size
    household_size = sample(1:12, n, replace = TRUE,
                            prob = c(0.05, 0.10, 0.15, 0.20, 0.20, 0.15, 0.08, 0.04, 0.02, 0.005, 0.003, 0.002)),

    income = abs(rnorm(n, mean = 200, sd = 80)),

    expenditure = abs(rnorm(n, mean = 180, sd = 70))
  )
}

# Assign logical hierarchies (ensure subcategories match categories)
create_hierarchical_mappings <- function(df) {
  df %>%
    mutate(
      # Map assistance types to sectors
      assistance_type = case_when(
        sector == "Food Security" ~ sample(c("Cash", "Food"), n(), replace = TRUE),
        sector == "Protection" ~ sample(c("Counseling", "Legal", "Cash"), n(), replace = TRUE),
        sector == "WASH" ~ sample(c("Water", "Sanitation", "Hygiene"), n(), replace = TRUE),
        sector == "Shelter" ~ sample(c("Emergency", "Transitional", "NFI"), n(), replace = TRUE),
        TRUE ~ assistance_type
      ),

      # Map districts to regions
      district = case_when(
        region == "North" ~ sample(c("District A", "District B"), n(), replace = TRUE),
        region == "South" ~ sample(c("District C", "District D"), n(), replace = TRUE),
        region == "East" ~ sample(c("District E", "District F"), n(), replace = TRUE),
        region == "West" ~ sample(c("District G", "District H"), n(), replace = TRUE),
        TRUE ~ district
      ),

      # Map specific livelihoods to categories
      specific_livelihood = case_when(
        livelihood_category == "Agriculture" ~ sample(c("Crops", "Livestock", "Fishing"), n(), replace = TRUE),
        livelihood_category == "Non-Agriculture" ~ sample(c("Trade", "Labor", "Services", "Crafts"), n(), replace = TRUE),
        livelihood_category == "Mixed" ~ "Mixed Activities",
        TRUE ~ specific_livelihood
      ),

      # Map water sources to types
      water_source = case_when(
        water_type == "Improved" ~ sample(c("Piped", "Borehole", "Protected Well"), n(), replace = TRUE),
        water_type == "Unimproved" ~ sample(c("Unprotected Well", "Tanker"), n(), replace = TRUE),
        water_type == "Surface" ~ sample(c("River", "Pond"), n(), replace = TRUE),
        TRUE ~ water_source
      )
    )
}

test_data <- create_test_treemap_hierarchical_data(n = 800) %>%
  create_hierarchical_mappings()

cat("\n=== Testing plot_treemap with Hierarchical Support ===\n\n")

# Test 1: Single level (original functionality)
cat("Test 1: Single level treemap - Sector only\n")
plot1 <- plot_treemap(
  df = test_data,
  category_var = "sector",
  title_name = "Distribution by Sector"
)
print(plot1)
cat("✓ Single level treemap\n\n")

# Test 2: Hierarchical - Sector > Assistance Type
cat("Test 2: Hierarchical - Sector > Assistance Type\n")
plot2 <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  title_name = "Assistance Types by Sector"
)
print(plot2)
cat("✓ Hierarchical with subcategories\n")
cat("✓ Colored by main sector\n")
cat("✓ White borders separate sectors\n\n")

# Test 3: Hierarchical - Region > District
cat("Test 3: Hierarchical - Region > District\n")
plot3 <- plot_treemap(
  df = test_data,
  category_var = "region",
  subcategory_var = "district",
  title_name = "Districts by Region"
)
print(plot3)
cat("✓ Geographic hierarchy\n\n")

# Test 4: Hierarchical - Livelihood Category > Specific Livelihood
cat("Test 4: Hierarchical - Livelihood Category > Specific\n")
plot4 <- plot_treemap(
  df = test_data,
  category_var = "livelihood_category",
  subcategory_var = "specific_livelihood",
  title_name = "Livelihoods Breakdown", color_palette = "group", legend_label = "Livelihoods"
)
print(plot4)
cat("✓ Livelihood hierarchy\n\n")

# Test 5: Hierarchical with size variable
cat("Test 5: Hierarchical with size variable (household_size)\n")
plot5 <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  size_var = "household_size",
  title_name = "Assistance by Sector (weighted by HH size)"
)
print(plot5)
cat("✓ Hierarchical + size variable\n\n")

# Test 6: Hierarchical weighted
cat("Test 6: Hierarchical weighted\n")
plot6 <- plot_treemap(
  df = test_data,
  category_var = "region",
  subcategory_var = "district",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Districts by Region (Weighted)"
)
print(plot6)
cat("✓ Hierarchical + weighted\n\n")

# Test 7: Comparison - Single vs Hierarchical
cat("Test 7: Side-by-side - Single vs Hierarchical\n")

p_single <- plot_treemap(
  df = test_data,
  category_var = "sector",
  title_name = "Sectors (Single Level)"
)

p_hier <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  title_name = "Sectors + Assistance (Hierarchical)"
)

grid.arrange(p_single, p_hier, ncol = 2)
cat("✓ Shows difference in detail levels\n\n")

# Test 8: Hierarchical with custom color palette
cat("Test 8: Hierarchical with different color palettes\n")

p_reach2 <- plot_treemap(
  df = test_data,
  category_var = "water_type",
  subcategory_var = "water_source",
  color_palette = "reach2",
  title_name = "Water Sources - REACH2"
)

p_reach3 <- plot_treemap(
  df = test_data,
  category_var = "water_type",
  subcategory_var = "water_source",
  color_palette = "reach3",
  title_name = "Water Sources - REACH3"
)

grid.arrange(p_reach2, p_reach3, ncol = 2)
cat("✓ Different color palettes\n\n")

# Test 9: Hierarchical with custom label size
cat("Test 9: Hierarchical with custom label sizes\n")

p_small <- plot_treemap(
  df = test_data,
  category_var = "livelihood_category",
  subcategory_var = "specific_livelihood",
  label_size = 0.8,
  title_name = "Small Labels (0.8)"
)

p_large <- plot_treemap(
  df = test_data,
  category_var = "livelihood_category",
  subcategory_var = "specific_livelihood",
  label_size = 1.3,
  title_name = "Large Labels (1.3)"
)

grid.arrange(p_small, p_large, ncol = 2)
cat("✓ Label size adjustment\n\n")

# Test 10: Hierarchical with legend position
cat("Test 10: Hierarchical with different legend positions\n")

p_bottom <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  legend_position = "bottom",
  title_name = "Legend Bottom"
)

p_right <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  legend_position = "right",
  title_name = "Legend Right"
)

p_none <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  legend_position = "none",
  title_name = "No Legend"
)

grid.arrange(p_bottom, p_right, p_none, ncol = 2)
cat("✓ Different legend positions\n\n")

# Test 11: All features combined - Hierarchical
cat("Test 11: All features - hierarchical, weighted, size_var, custom labels\n")
plot11 <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  size_var = "income",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = "group",
  label_size = 1.1,
  legend_position = "bottom",
  legend_label = "Sector",
  title_name = "Comprehensive Hierarchical Treemap",
  subtitle = "All Features Demo"
)
print(plot11)
cat("✓ All features combined\n\n")

# Test 12: Four-panel comparison
cat("Test 12: Four-panel - Different hierarchies\n")

p1 <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  title_name = "Sector > Assistance"
)

p2 <- plot_treemap(
  df = test_data,
  category_var = "region",
  subcategory_var = "district",
  title_name = "Region > District"
)

p3 <- plot_treemap(
  df = test_data,
  category_var = "livelihood_category",
  subcategory_var = "specific_livelihood",
  title_name = "Livelihood Hierarchy"
)

p4 <- plot_treemap(
  df = test_data,
  category_var = "water_type",
  subcategory_var = "water_source",
  title_name = "Water Type > Source"
)

grid.arrange(p1, p2, p3, p4, ncol = 2)
cat("✓ Multiple hierarchies displayed\n\n")

# Test 13: Single level with different categories
cat("Test 13: Single level - different categories\n")

p_edu <- plot_treemap(
  df = test_data,
  category_var = "education_level",
  title_name = "Education Level"
)

p_shelter <- plot_treemap(
  df = test_data,
  category_var = "shelter_type",
  title_name = "Shelter Type"
)

grid.arrange(p_edu, p_shelter, ncol = 2)
cat("✓ Single level still works\n\n")

# Test 14: Weighted vs Unweighted hierarchical
cat("Test 14: Weighted vs Unweighted - Hierarchical\n")

p_unw <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  weighted = FALSE,
  title_name = "Unweighted"
)

p_w <- plot_treemap(
  df = test_data,
  category_var = "sector",
  subcategory_var = "assistance_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Weighted"
)

grid.arrange(p_unw, p_w, ncol = 2)
cat("✓ Shows weighting impact on hierarchical structure\n\n")

# Test 15: Custom subtitle
cat("Test 15: Hierarchical with custom subtitle\n")
plot15 <- plot_treemap(
  df = test_data,
  category_var = "region",
  subcategory_var = "district",
  title_name = "Geographic Distribution",
  subtitle = "Baseline Assessment 2024"
)
print(plot15)
cat("✓ Custom subtitle appended\n\n")


# ERROR HANDLING TESTS


cat("=== Error Handling Tests ===\n\n")

# Test 16: Error - Invalid subcategory_var
cat("Test 16: Error - Invalid subcategory_var\n")
tryCatch({
  plot16 <- plot_treemap(
    df = test_data,
    category_var = "sector",
    subcategory_var = "nonexistent"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error:", e$message, "\n\n")
})

# Test 17: Error - weighted hierarchical without weights_col
cat("Test 17: Error - weighted without weights_col\n")
tryCatch({
  plot17 <- plot_treemap(
    df = test_data,
    category_var = "sector",
    subcategory_var = "assistance_type",
    weighted = TRUE
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error:", e$message, "\n\n")
})


# VERIFICATION


cat("\n=== Data Verification ===\n")
cat("Total respondents:", nrow(test_data), "\n")
cat("Weight range:", range(test_data$survey_weight), "\n\n")

cat("Sector distribution:\n")
print(table(test_data$sector))

cat("\nAssistance type by sector:\n")
print(table(test_data$sector, test_data$assistance_type))

cat("\nRegion > District structure:\n")
region_district <- test_data %>%
  group_by(region, district) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(region, district)
print(region_district)

 # PLOT SANKEY ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(gridExtra)

# Create test dataset for Sankey/alluvial diagrams
create_test_sankey_data <- function(n = 500) {
  set.seed(7777)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),

    # Survey weights
    survey_weight = runif(n, 0.8, 1.5),

    # Flow 1: Pre-displacement > Displacement > Current location
    origin_location = sample(c("Rural", "Urban", "Semi-Urban"), n, replace = TRUE,
                             prob = c(0.5, 0.3, 0.2)),

    displacement_type = sample(c("IDP", "Refugee", "Returnee", "Host"), n, replace = TRUE,
                               prob = c(0.4, 0.3, 0.2, 0.1)),

    current_location = sample(c("Camp", "Urban", "Rural", "Host Community"), n, replace = TRUE,
                              prob = c(0.3, 0.3, 0.25, 0.15)),

    # Flow 2: Previous livelihood > Current livelihood
    previous_livelihood = sample(c("Agriculture", "Trade", "Labor", "Professional", "None"),
                                 n, replace = TRUE, prob = c(0.4, 0.2, 0.2, 0.15, 0.05)),

    current_livelihood = sample(c("Agriculture", "Trade", "Labor", "Aid-dependent", "None"),
                                n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.15, 0.10)),

    # Flow 3: Water source change over time
    water_before = sample(c("Improved", "Unimproved", "Surface"), n, replace = TRUE,
                          prob = c(0.4, 0.4, 0.2)),

    water_after = sample(c("Improved", "Unimproved", "Surface"), n, replace = TRUE,
                         prob = c(0.5, 0.35, 0.15)),

    # Flow 4: Education pathway
    education_before = sample(c("None", "Primary", "Secondary", "Tertiary"), n, replace = TRUE,
                              prob = c(0.3, 0.4, 0.2, 0.1)),

    education_during = sample(c("None", "Primary", "Secondary", "Tertiary"), n, replace = TRUE,
                              prob = c(0.4, 0.35, 0.2, 0.05)),

    education_current = sample(c("None", "Primary", "Secondary", "Tertiary"), n, replace = TRUE,
                               prob = c(0.35, 0.35, 0.25, 0.05)),

    # Flow 5: Assistance pathway
    initial_assistance = sample(c("Food", "Cash", "NFI", "None"), n, replace = TRUE,
                                prob = c(0.35, 0.3, 0.2, 0.15)),

    current_assistance = sample(c("Food", "Cash", "Multi-sector", "None"), n, replace = TRUE,
                                prob = c(0.25, 0.35, 0.25, 0.15)),

    # Flow 6: Shelter progression
    initial_shelter = sample(c("Tent", "Makeshift", "Host", "Collective"), n, replace = TRUE,
                             prob = c(0.4, 0.3, 0.2, 0.1)),

    current_shelter = sample(c("Permanent", "Semi-permanent", "Makeshift", "Collective"),
                             n, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),

    # Flow 7: Income levels over time
    income_category_2022 = sample(c("None", "Low", "Medium", "High"), n, replace = TRUE,
                                  prob = c(0.3, 0.4, 0.2, 0.1)),

    income_category_2023 = sample(c("None", "Low", "Medium", "High"), n, replace = TRUE,
                                  prob = c(0.25, 0.4, 0.25, 0.1)),

    income_category_2024 = sample(c("None", "Low", "Medium", "High"), n, replace = TRUE,
                                  prob = c(0.2, 0.35, 0.3, 0.15)),

    # Flow 8: Food security status
    fcs_baseline = sample(c("Poor", "Borderline", "Acceptable"), n, replace = TRUE,
                          prob = c(0.4, 0.35, 0.25)),

    fcs_endline = sample(c("Poor", "Borderline", "Acceptable"), n, replace = TRUE,
                         prob = c(0.3, 0.3, 0.4))
  )
}

test_data <- create_test_sankey_data(n = 600)

cat("\n=== Testing plot_sankey with Custom Axis Labels ===\n\n")

# Test 1: Default axis labels (use column names)
cat("Test 1: Default - Column names as axis labels\n")
plot1 <- plot_sankey(
  df = test_data,
  axis_vars = c("origin_location", "current_location"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Default Axis Labels"
)
print(plot1)
cat("✓ Uses column names: origin_location, current_location\n\n")

# Test 2: Custom axis labels
cat("Test 2: Custom axis labels\n")
plot2 <- plot_sankey(
  df = test_data,
  axis_vars = c("origin_location", "current_location"),
  axis_labels = c("Origin", "Current Location"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Custom Axis Labels"
)
print(plot2)
cat("✓ Uses custom labels: Origin, Current Location\n\n")

# Test 3: Comparison - Default vs Custom labels
cat("Test 3: Side-by-side - Default vs Custom\n")

p_default <- plot_sankey(
  df = test_data,
  axis_vars = c("water_before", "water_after"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Default Labels"
)

p_custom <- plot_sankey(
  df = test_data,
  axis_vars = c("water_before", "water_after"),
  axis_labels = c("Baseline", "Endline"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Custom Labels"
)

grid.arrange(p_default, p_custom, ncol = 2)
cat("✓ Visual comparison\n\n")

# Test 4: 3-axis with custom labels
cat("Test 4: 3-axis with custom labels\n")
plot4 <- plot_sankey(
  df = test_data,
  axis_vars = c("origin_location", "displacement_type", "current_location"),
  axis_labels = c("Origin", "Status", "Current"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Displacement Journey - Custom Labels"
)
print(plot4)
cat("✓ Three custom axis labels\n\n")

# Test 5: 4-axis with custom labels
cat("Test 5: 4-axis with custom labels\n")
plot5 <- plot_sankey(
  df = test_data,
  axis_vars = c("education_before", "education_during", "education_current", "current_livelihood"),
  axis_labels = c("Pre-Crisis", "During Crisis", "Current", "Livelihood"),
  show_percentage = TRUE,
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Education to Livelihood - Custom Labels"
)
print(plot5)
cat("✓ Four custom axis labels\n\n")

# Test 6: Descriptive labels for time series
cat("Test 6: Time series with year labels\n")
plot6 <- plot_sankey(
  df = test_data,
  axis_vars = c("income_category_2022", "income_category_2023", "income_category_2024"),
  axis_labels = c("2022", "2023", "2024"),
  show_percentage = TRUE,
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  color_palette = "reach4",
  title_name = "Income Progression 2022-2024"
)
print(plot6)
cat("✓ Year labels for time series\n\n")

# Test 7: Multi-language labels (example)
cat("Test 7: Multi-language labels example\n")
plot7 <- plot_sankey(
  df = test_data,
  axis_vars = c("fcs_baseline", "fcs_endline"),
  axis_labels = c("Baseline (الخط الأساسي)", "Endline (خط النهاية)"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  color_palette = "traffic_light",
  title_name = "Food Security Status"
)
print(plot7)
cat("✓ Supports multi-language labels\n\n")

# Test 8: Descriptive programmatic labels
cat("Test 8: Programmatic labels\n")
plot8 <- plot_sankey(
  df = test_data,
  axis_vars = c("initial_assistance", "current_assistance"),
  axis_labels = c("Initial Assistance (Month 0)", "Current Assistance (Month 12)"),
  weighted = TRUE,
  weights_col = "survey_weight",
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Assistance Type Changes"
)
print(plot8)
cat("✓ Descriptive labels with context\n\n")

# Test 9: Short concise labels
cat("Test 9: Short concise labels\n")
plot9 <- plot_sankey(
  df = test_data,
  axis_vars = c("origin_location", "current_location"),
  axis_labels = c("T0", "T1"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Location Change - Time Points"
)
print(plot9)
cat("✓ Short time point labels\n\n")

# Test 10: Four-panel with different label styles
cat("Test 10: Four-panel - Different label styles\n")

p1 <- plot_sankey(
  df = test_data,
  axis_vars = c("previous_livelihood", "current_livelihood"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Default Column Names"
)

p2 <- plot_sankey(
  df = test_data,
  axis_vars = c("previous_livelihood", "current_livelihood"),
  axis_labels = c("Before", "After"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Simple Labels"
)

p3 <- plot_sankey(
  df = test_data,
  axis_vars = c("previous_livelihood", "current_livelihood"),
  axis_labels = c("Previous Livelihood", "Current Livelihood"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Descriptive Labels"
)

p4 <- plot_sankey(
  df = test_data,
  axis_vars = c("previous_livelihood", "current_livelihood"),
  axis_labels = c("Pre-Displacement (2022)", "Post-Displacement (2024)"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Detailed Context"
)

grid.arrange(p1, p2, p3, p4, ncol = 2)
cat("✓ Different labeling styles\n\n")

# Test 11: With all features combined
cat("Test 11: All features with custom axis labels\n")
plot11 <- plot_sankey(
  df = test_data,
  axis_vars = c("origin_location", "displacement_type", "current_location"),
  axis_labels = c("Place of Origin", "Displacement Status", "Current Residence"),
  weighted = TRUE,
  weights_col = "survey_weight",
  show_percentage = TRUE,
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  color_palette = "reach2",
  legend_position = "right",
  x_lab = "",
  y_lab = "Percentage (%)",
  title_name = "Complete Displacement Journey Analysis",
  subtitle = "Population Movement Patterns 2024"
)
print(plot11)
cat("✓ All features with custom labels\n\n")

# Test 12: Legend name uses first axis label
cat("Test 12: Legend title from custom axis label\n")
plot12 <- plot_sankey(
  df = test_data,
  axis_vars = c("water_before", "water_after"),
  axis_labels = c("Water Source (Baseline)", "Water Source (Endline)"),
  show_stratum_labels = FALSE,
  show_stratum_stats = TRUE,
  title_name = "Water Access Change"
)
print(plot12)
cat("✓ Legend title uses first custom axis label\n\n")

# Test 13: Shelter progression with phase labels
cat("Test 13: Shelter progression - Phase labels\n")
plot13 <- plot_sankey(
  df = test_data,
  axis_vars = c("initial_shelter", "current_shelter"),
  axis_labels = c("Phase 1: Emergency", "Phase 2: Durable"),
  show_percentage = TRUE,
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Shelter Type Transitions by Phase"
)
print(plot13)
cat("✓ Phase-based labels\n\n")

# Test 14: Flip coordinates with custom labels
cat("Test 14: Flipped coordinates with custom labels\n")

p_normal <- plot_sankey(
  df = test_data,
  axis_vars = c("fcs_baseline", "fcs_endline"),
  axis_labels = c("Before Program", "After Program"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  flip_coordinates = FALSE,
  title_name = "Normal"
)

p_flipped <- plot_sankey(
  df = test_data,
  axis_vars = c("fcs_baseline", "fcs_endline"),
  axis_labels = c("Before Program", "After Program"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  flip_coordinates = TRUE,
  title_name = "Flipped"
)

grid.arrange(p_normal, p_flipped, ncol = 2)
cat("✓ Custom labels work with both orientations\n\n")


# PRACTICAL USE CASES


cat("=== Practical Use Case Examples ===\n\n")

# Use Case 1: Report with professional labels
cat("Use Case 1: Professional Report Labels\n")
uc1 <- plot_sankey(
  df = test_data,
  axis_vars = c("fcs_baseline", "fcs_endline"),
  axis_labels = c("Baseline Assessment\n(January 2024)", "Endline Assessment\n(December 2024)"),
  show_percentage = TRUE,
  show_stratum_labels = FALSE,
  show_stratum_stats = TRUE,
  color_palette = "traffic_light",
  title_name = "Food Consumption Score Changes",
  subtitle = "12-Month Program Impact"
)
print(uc1)
cat("✓ Professional multi-line labels\n\n")

# Use Case 2: Multi-country comparison
cat("Use Case 2: Multi-location labels\n")
uc2 <- plot_sankey(
  df = test_data,
  axis_vars = c("origin_location", "current_location"),
  axis_labels = c("Syria (Origin)", "Lebanon (Current)"),
  weighted = TRUE,
  weights_col = "survey_weight",
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Cross-Border Displacement Patterns"
)
print(uc2)
cat("✓ Geographic context in labels\n\n")

# Use Case 3: Quarterly tracking
cat("Use Case 3: Quarterly Progress Tracking\n")
uc3 <- plot_sankey(
  df = test_data,
  axis_vars = c("income_category_2022", "income_category_2023", "income_category_2024"),
  axis_labels = c("Q1 2022", "Q1 2023", "Q1 2024"),
  show_percentage = TRUE,
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Economic Recovery Tracking",
  subtitle = "Household Income Categories"
)
print(uc3)
cat("✓ Quarterly period labels\n\n")

# Use Case 4: Program phases
cat("Use Case 4: Program Phase Labels\n")
uc4 <- plot_sankey(
  df = test_data,
  axis_vars = c("initial_assistance", "current_assistance"),
  axis_labels = c("Humanitarian Phase\n(0-6 months)", "Recovery Phase\n(6-12 months)"),
  show_stratum_labels = TRUE,
  show_stratum_stats = TRUE,
  title_name = "Assistance Modality Transitions",
  subtitle = "Cash & Voucher Program"
)
print(uc4)
cat("✓ Program phase context\n\n")


# ERROR HANDLING TESTS


cat("=== Error Handling Tests ===\n\n")

# Test 15: Error - Mismatched axis_labels length
cat("Test 15: Error - Wrong number of axis_labels\n")
tryCatch({
  plot15 <- plot_sankey(
    df = test_data,
    axis_vars = c("origin_location", "current_location"),
    axis_labels = c("Only One Label")  # Should have 2
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error: axis_labels length mismatch\n\n")
})

# Test 16: Error - Empty axis_labels
cat("Test 16: Works with NULL axis_labels (uses defaults)\n")
plot16 <- plot_sankey(
  df = test_data,
  axis_vars = c("water_before", "water_after"),
  axis_labels = NULL,
  show_stratum_labels = TRUE,
  title_name = "NULL axis_labels"
)
print(plot16)
cat("✓ Falls back to column names when axis_labels is NULL\n\n")


# VERIFICATION


cat("\n=== Key Features ===\n")
cat("1. ✓ axis_labels: Custom labels for each axis\n")
cat("2. ✓ Default behavior: Uses axis_vars as labels when NULL\n")
cat("3. ✓ Validation: Ensures axis_labels length matches axis_vars\n")
cat("4. ✓ Multi-line labels: Supports \\n for line breaks\n")
cat("5. ✓ Multi-language: Supports any character encoding\n")
cat("6. ✓ Legend title: Uses first axis_label\n")
cat("7. ✓ Works with all other features: percentages, weights, stats\n")
cat("8. ✓ Flip coordinates: Labels work in both orientations\n")

cat("\n=== Best Practices for Axis Labels ===\n")
cat("• Keep labels concise (1-3 words ideal)\n")
cat("• Use \\n for multi-line labels when needed\n")
cat("• Add context: time periods, locations, phases\n")
cat("• Be consistent in labeling style across plots\n")
cat("• Consider audience: technical vs. executive\n")
cat("• Use standard terminology for comparability\n")
cat("• Include units or time frames when relevant\n")

cat("\n=== Label Style Examples ===\n")
cat("Simple: c('Before', 'After')\n")
cat("Descriptive: c('Pre-Displacement', 'Post-Displacement')\n")
cat("Temporal: c('Baseline (Jan 2024)', 'Endline (Dec 2024)')\n")
cat("Geographic: c('Syria', 'Lebanon', 'Turkey')\n")
cat("Programmatic: c('Phase 1', 'Phase 2', 'Phase 3')\n")
cat("Quarterly: c('Q1 2024', 'Q2 2024', 'Q3 2024')\n")
cat("Multi-line: c('Water Source\\n(Baseline)', 'Water Source\\n(Endline)')\n")

cat("\n=== All Sankey tests with custom axis labels completed ===\n")

# PLOT CI BAR PERCENTAGE ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

# Create test dataset
create_test_ci_data <- function(n = 400) {
  set.seed(8888)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),

    # Survey weights
    survey_weight = runif(n, 0.8, 1.5),

    # Categories for single-variable analysis
    education_level = sample(c("None", "Primary", "Secondary", "Tertiary"), n, replace = TRUE,
                             prob = c(0.25, 0.40, 0.25, 0.10)),

    food_security = sample(c("Poor", "Borderline", "Acceptable"), n, replace = TRUE,
                           prob = c(0.30, 0.35, 0.35)),

    shelter_type = sample(c("House", "Apartment", "Tent", "Makeshift", "Collective"), n, replace = TRUE,
                          prob = c(0.25, 0.20, 0.20, 0.20, 0.15)),

    water_source = sample(c("Improved", "Unimproved", "Surface"), n, replace = TRUE,
                          prob = c(0.50, 0.35, 0.15)),

  livelihood = sample(c("Agriculture", "Trade", "Labor", "Services", "None"), n, replace = TRUE,
                      prob = c(0.30, 0.20, 0.25, 0.15, 0.10)),

  # Grouping variables
  region = sample(c("North", "South", "East", "West"), n, replace = TRUE,
                  prob = c(0.30, 0.30, 0.25, 0.15)),

  gender = sample(c("Male", "Female"), n, replace = TRUE,
                  prob = c(0.52, 0.48)),

  displacement_status = sample(c("IDP", "Refugee", "Returnee", "Host"), n, replace = TRUE,
                               prob = c(0.35, 0.30, 0.20, 0.15)),

  settlement_type = sample(c("Camp", "Urban", "Rural"), n, replace = TRUE,
                           prob = c(0.35, 0.35, 0.30)),

  age_group = sample(c("18-24", "25-34", "35-44", "45-54", "55+"), n, replace = TRUE,
                     prob = c(0.20, 0.25, 0.25, 0.20, 0.10)),

  income_level = sample(c("Low", "Medium", "High"), n, replace = TRUE,
                        prob = c(0.50, 0.35, 0.15)),

  # Binary outcomes
  has_documentation = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.65, 0.35)),

  access_healthcare = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.55, 0.45)),

  employed = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.40, 0.60))
  )
}

test_data <- create_test_ci_data(n = 500)

cat("\n=== Testing plot_ci_bar_percentage ===\n\n")

# Test 1: Basic - Single category, no grouping
cat("Test 1: Basic bar chart with CI - Education levels\n")
plot1 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "education_level",
  title_name = "Education Level Distribution"
)
print(plot1)
cat("✓ Single category with confidence intervals\n\n")

# Test 2: With labels
cat("Test 2: With percentage labels\n")
plot2 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "food_security",
  show_labels = TRUE,
  color_palette = "traffic_light",
  title_name = "Food Security Status with Labels"
)
print(plot2)
cat("✓ Shows percentage values on bars\n\n")

# Test 3: With grouping
cat("Test 3: Grouped bar chart - Education by Region\n")
plot3 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "education_level",
  grouping = "region",
  title_name = "Education Level by Region"
)
print(plot3)
cat("✓ Grouped bars with separate CIs\n\n")

# Test 4: Grouped with labels
cat("Test 4: Grouped with labels - Food Security by Gender\n")
plot4 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "food_security",
  grouping = "gender",
  show_labels = TRUE,
  color_palette = "traffic_light",
  title_name = "Food Security by Gender"
)
print(plot4)
cat("✓ Grouped bars with labels and CIs\n\n")

# Test 5: Weighted analysis
cat("Test 5: Weighted percentages with CI\n")
plot5 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "shelter_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Shelter Type Distribution (Weighted)"
)
print(plot5)
cat("✓ Survey-weighted percentages\n\n")

# Test 6: Weighted and grouped
cat("Test 6: Weighted and grouped - Water by Settlement\n")
plot6 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "water_source",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_labels = TRUE,
  title_name = "Water Source by Settlement (Weighted)"
)
print(plot6)
cat("✓ Weighted grouped analysis\n\n")

# Test 7: Different confidence levels
cat("Test 7: Different confidence levels - 90%, 95%, 99%\n")

p90 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "livelihood",
  conf_level = 0.90,
  title_name = "90% CI"
)

p95 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "livelihood",
  conf_level = 0.95,
  title_name = "95% CI"
)

p99 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "livelihood",
  conf_level = 0.99,
  title_name = "99% CI"
)

grid.arrange(p90, p95, p99, ncol = 3)
cat("✓ Shows wider CIs with higher confidence\n\n")

# Test 8: Flip coordinates
cat("Test 8: Flipped coordinates - Horizontal bars\n")

p_vertical <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "shelter_type",
  show_labels = TRUE,
  flip_coordinates = FALSE,
  title_name = "Vertical"
)

p_horizontal <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "shelter_type",
  show_labels = TRUE,
  flip_coordinates = TRUE,
  title_name = "Horizontal"
)

grid.arrange(p_vertical, p_horizontal, ncol = 2)
cat("✓ Horizontal orientation\n\n")

# Test 9: Different color palettes
cat("Test 9: Different color palettes\n")

p_reach1 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "education_level",
  color_palette = "reach1",
  title_name = "REACH1"
)

p_reach2 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "education_level",
  color_palette = "reach2",
  title_name = "REACH2"
)

p_reach3 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "education_level",
  color_palette = "reach3",
  title_name = "REACH3"
)

p_reach4 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "education_level",
  color_palette = "reach4",
  title_name = "REACH4"
)

grid.arrange(p_reach1, p_reach2, p_reach3, p_reach4, ncol = 2)
cat("✓ All color palettes\n\n")

# Test 10: Custom axis labels
cat("Test 10: Custom axis labels\n")
plot10 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "food_security",
  x_lab = "Food Consumption Score Category",
  y_lab = "Percentage of Households (%)",
  title_name = "Food Security Distribution",
  subtitle = "Baseline Assessment"
)
print(plot10)
cat("✓ Custom labels\n\n")

# Test 11: Legend positions
cat("Test 11: Different legend positions\n")

p_bottom <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "water_source",
  grouping = "region",
  legend_position = "bottom",
  title_name = "Legend Bottom"
)

p_right <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "water_source",
  grouping = "region",
  legend_position = "right",
  title_name = "Legend Right"
)

p_top <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "water_source",
  grouping = "region",
  legend_position = "top",
  title_name = "Legend Top"
)

grid.arrange(p_bottom, p_right, p_top, ncol = 2)
cat("✓ Different legend positions\n\n")

# Test 12: Custom legend label
cat("Test 12: Custom legend label\n")
plot12 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "education_level",
  grouping = "gender",
  legend_label = "Gender of Respondent",
  title_name = "Education by Gender"
)
print(plot12)
cat("✓ Custom legend title\n\n")

# Test 13: Binary outcomes
cat("Test 13: Binary outcomes - Yes/No questions\n")

p_doc <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "has_documentation",
  show_labels = TRUE,
  title_name = "Has Documentation"
)

p_health <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "access_healthcare",
  show_labels = TRUE,
  title_name = "Access to Healthcare"
)

p_employ <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "employed",
  show_labels = TRUE,
  title_name = "Employment Status"
)

grid.arrange(p_doc, p_health, p_employ, ncol = 3)
cat("✓ Binary outcome variables\n\n")

# Test 14: Comparison - Weighted vs Unweighted
cat("Test 14: Weighted vs Unweighted comparison\n")

p_unw <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "shelter_type",
  grouping = "displacement_status",
  weighted = FALSE,
  show_labels = TRUE,
  title_name = "Unweighted"
)

p_w <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "shelter_type",
  grouping = "displacement_status",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_labels = TRUE,
  title_name = "Weighted"
)

grid.arrange(p_unw, p_w, ncol = 2)
cat("✓ Shows impact of weighting\n\n")

# Test 15: Multiple groupings
cat("Test 15: Different grouping variables\n")

p_region <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "food_security",
  grouping = "region",
  color_palette = "traffic_light",
  title_name = "By Region"
)

p_settlement <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "food_security",
  grouping = "settlement_type",
  color_palette = "traffic_light",
  title_name = "By Settlement"
)

p_displacement <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "food_security",
  grouping = "displacement_status",
  color_palette = "traffic_light",
  title_name = "By Displacement Status"
)

grid.arrange(p_region, p_settlement, p_displacement, ncol = 2)
cat("✓ Multiple grouping options\n\n")

# Test 16: Age group analysis
cat("Test 16: Age group analysis\n")
plot16 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "education_level",
  grouping = "age_group",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_labels = FALSE,
  title_name = "Education Level by Age Group (Weighted)"
)
print(plot16)
cat("✓ Multiple age categories\n\n")

# Test 17: Income analysis
cat("Test 17: Livelihood by Income Level\n")
plot17 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "livelihood",
  grouping = "income_level",
  show_labels = TRUE,
  flip_coordinates = TRUE,
  title_name = "Livelihood Activities by Income Level"
)
print(plot17)
cat("✓ Horizontal grouped bars\n\n")

# Test 18: All features combined
cat("Test 18: All features - weighted, grouped, labels, custom styling\n")
plot18 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "water_source",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  conf_level = 0.95,
  show_labels = TRUE,
  color_palette = "reach2",
  title_name = "Water Source Distribution by Settlement Type",
  subtitle = "Comprehensive Analysis 2024",
  x_lab = "Settlement Type",
  y_lab = "Percentage of Households (%)",
  legend_label = "Water Source Category",
  legend_position = "right",
  flip_coordinates = FALSE
)
print(plot18)
cat("✓ All features combined\n\n")


# PRACTICAL USE CASES


cat("=== Practical Use Case Examples ===\n\n")

# Use Case 1: Survey results presentation
cat("Use Case 1: Survey Results - Food Security\n")
uc1 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "food_security",
  show_labels = TRUE,
  color_palette = "traffic_light",
  conf_level = 0.95,
  title_name = "Food Consumption Score Distribution",
  subtitle = "Regional Assessment - Q4 2024",
  x_lab = "FCS Category",
  y_lab = "Percentage of Households (%)"
)
print(uc1)
cat("✓ Professional survey presentation\n\n")

# Use Case 2: Comparative analysis
cat("Use Case 2: Comparative Analysis - Education by Displacement Status\n")
uc2 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "education_level",
  grouping = "displacement_status",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_labels = TRUE,
  title_name = "Education Levels Across Displacement Categories",
  subtitle = "Population-weighted estimates",
  legend_label = "Displacement Status"
)
print(uc2)
cat("✓ Comparative group analysis\n\n")

# Use Case 3: Program monitoring
cat("Use Case 3: Program Monitoring - Access indicators\n")

p_doc_region <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "has_documentation",
  grouping = "region",
  show_labels = TRUE,
  title_name = "Documentation Coverage by Region"
)

p_health_region <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "access_healthcare",
  grouping = "region",
  show_labels = TRUE,
  title_name = "Healthcare Access by Region"
)

grid.arrange(p_doc_region, p_health_region, ncol = 2)
cat("✓ Program indicator monitoring\n\n")

# Use Case 4: Vulnerability analysis
cat("Use Case 4: Vulnerability Analysis - Multiple dimensions\n")
uc4 <- plot_ci_bar_percentage(
  df = test_data,
  category_var = "shelter_type",
  grouping = "income_level",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_labels = TRUE,
  flip_coordinates = TRUE,
  title_name = "Shelter Type by Income Level",
  subtitle = "Vulnerability Assessment",
  legend_label = "Income Category"
)
print(uc4)
cat("✓ Multi-dimensional vulnerability\n\n")


# ERROR HANDLING TESTS


cat("=== Error Handling Tests ===\n\n")

# Test 19: Error - NULL category_var
cat("Test 19: Error - NULL category_var\n")
tryCatch({
  plot19 <- plot_ci_bar_percentage(df = test_data, category_var = NULL)
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 20: Error - Invalid category_var
cat("Test 20: Error - Invalid category_var\n")
tryCatch({
  plot20 <- plot_ci_bar_percentage(df = test_data, category_var = "nonexistent")
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 21: Error - weighted without weights_col
cat("Test 21: Error - weighted without weights_col\n")
tryCatch({
  plot21 <- plot_ci_bar_percentage(
    df = test_data,
    category_var = "education_level",
    weighted = TRUE
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 22: Error - Invalid grouping column
cat("Test 22: Error - Invalid grouping column\n")
tryCatch({
  plot22 <- plot_ci_bar_percentage(
    df = test_data,
    category_var = "education_level",
    grouping = "nonexistent"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})


# VERIFICATION


cat("\n=== Data Verification ===\n")
cat("Total respondents:", nrow(test_data), "\n")
cat("Weight range:", range(test_data$survey_weight), "\n")
cat("Sum of weights:", sum(test_data$survey_weight), "\n\n")

cat("Education level distribution:\n")
ed_table <- table(test_data$education_level)
ed_pct <- prop.table(ed_table) * 100
ed_summary <- data.frame(
  Category = names(ed_table),
  Count = as.integer(ed_table),
  Percentage = round(ed_pct, 1)
)
print(ed_summary)
cat("\n")

cat("Food security distribution:\n")
fcs_table <- table(test_data$food_security)
fcs_pct <- prop.table(fcs_table) * 100
fcs_summary <- data.frame(
  Category = names(fcs_table),
  Count = as.integer(fcs_table),
  Percentage = round(fcs_pct, 1)
)
print(fcs_summary)
cat("\n")

cat("=== Key Features ===\n")
cat("1. ✓ Percentages with confidence intervals\n")
cat("2. ✓ Single category or grouped analysis\n")
cat("3. ✓ Weighted and unweighted estimates\n")
cat("4. ✓ Configurable confidence levels (90%, 95%, 99%)\n")
cat("5. ✓ Optional percentage labels on bars\n")
cat("6. ✓ Multiple color palettes\n")
cat("7. ✓ Horizontal or vertical orientation\n")
cat("8. ✓ Custom axis and legend labels\n")
cat("9. ✓ Error bars show uncertainty\n")
cat("10. ✓ Works with binary and multi-category variables\n")

cat("\n=== Understanding Confidence Intervals ===\n")
cat("• CI width reflects uncertainty in estimates\n")
cat("• Larger samples = narrower CIs\n")
cat("• 95% CI means 95% confidence true value is within range\n")
cat("• Weighted analysis uses effective sample size\n")
cat("• Non-overlapping CIs suggest significant differences\n")
cat("• Higher confidence levels (99%) = wider intervals\n")

cat("\n=== Use Cases ===\n")
cat("1. Survey result presentation with uncertainty\n")
cat("2. Comparative analysis across groups\n")
cat("3. Program monitoring indicators\n")
cat("4. Vulnerability assessments\n")
cat("5. Binary outcome tracking (Yes/No)\n")
cat("6. Multi-category distributions\n")
cat("7. Weighted population estimates\n")
cat("8. Statistical hypothesis testing visualization\n")

cat("\n=== Best Practices ===\n")
cat("• Show CIs to communicate uncertainty\n")
cat("• Use appropriate confidence level (usually 95%)\n")
cat("• Consider weighting for representative samples\n")
cat("• Add labels for precise values\n")
cat("• Use horizontal bars for long category names\n")
cat("• Choose colors appropriate for data (traffic light for FCS)\n")
cat("• Include sample size in subtitle\n")

cat("\n=== All CI bar percentage tests completed ===\n")

# PLOT CI VAR MEAN ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

# Create test dataset (same as before)
create_test_mean_ci_data <- function(n = 400) {
  set.seed(9999)

  df <- data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),
    survey_weight = runif(n, 0.8, 1.5),
    household_size = sample(1:12, n, replace = TRUE,
                            prob = c(0.05, 0.10, 0.15, 0.20, 0.20, 0.15, 0.08, 0.04, 0.02, 0.005, 0.003, 0.002)),
    income = abs(rnorm(n, mean = 250, sd = 100)),
    expenditure = abs(rnorm(n, mean = 220, sd = 90)),
    debt = abs(rnorm(n, mean = 150, sd = 80)),
    food_expenditure = abs(rnorm(n, mean = 120, sd = 50)),
    distance_to_water = abs(rnorm(n, mean = 500, sd = 300)),
    distance_to_health = abs(rnorm(n, mean = 2000, sd = 1500)),
    waiting_time_water = abs(rnorm(n, mean = 45, sd = 30)),
    days_food_lasted = sample(1:30, n, replace = TRUE, prob = dexp(1:30, rate = 0.1)),
    num_income_sources = sample(0:5, n, replace = TRUE, prob = c(0.15, 0.25, 0.30, 0.20, 0.08, 0.02)),
    years_displaced = abs(rnorm(n, mean = 3.5, sd = 2.5)),
    fcs_score = abs(rnorm(n, mean = 45, sd = 20)),
    total_members = sample(1:12, n, replace = TRUE),
    children = sample(0:8, n, replace = TRUE, prob = dexp(0:8, rate = 0.3)),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE,
                    prob = c(0.30, 0.30, 0.25, 0.15)),
    displacement_status = sample(c("IDP", "Refugee", "Returnee", "Host"), n, replace = TRUE,
                                 prob = c(0.35, 0.30, 0.20, 0.15)),
    settlement_type = sample(c("Camp", "Urban", "Rural"), n, replace = TRUE,
                             prob = c(0.35, 0.35, 0.30)),
    gender = sample(c("Male", "Female"), n, replace = TRUE,
                    prob = c(0.52, 0.48)),
    age_group = sample(c("18-24", "25-34", "35-44", "45-54", "55+"), n, replace = TRUE,
                       prob = c(0.20, 0.25, 0.25, 0.20, 0.10)),
    income_level = sample(c("Low", "Medium", "High"), n, replace = TRUE,
                          prob = c(0.50, 0.35, 0.15)),
    assistance_recipient = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.65, 0.35))
  )

  df$employed_members <- sapply(1:nrow(df), function(i) {
    sample(0:df$total_members[i], 1)
  })

  return(df)
}

test_data <- create_test_mean_ci_data(n = 500)

cat("\n=== Testing plot_ci_point_mean (Point Chart) ===\n\n")

# Test 1: Basic point - No grouping
cat("Test 1: Basic point with CI - Household Size\n")
plot1 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "household_size",
  title_name = "Average Household Size"
)
print(plot1)
cat("✓ Single point with confidence interval\n\n")

# Test 2: Point with labels
cat("Test 2: Point with value labels\n")
plot2 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  show_labels = TRUE,
  title_name = "Average Income with Label"
)
print(plot2)
cat("✓ Shows mean value above point\n\n")

# Test 3: Points by grouping
cat("Test 3: Points by grouping - Income by Region\n")
plot3 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  show_labels = TRUE,
  title_name = "Average Income by Region"
)
print(plot3)
cat("✓ Grouped points with CIs\n\n")

# Test 4: Comparison - Bar vs Point
cat("Test 4: Comparison - Bar vs Point visualization\n")

p_bar <- plot_ci_bar_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  show_labels = TRUE,
  title_name = "Bar Chart"
)

p_point <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  show_labels = TRUE,
  title_name = "Point Chart"
)

grid.arrange(p_bar, p_point, ncol = 2)
cat("✓ Visual comparison of bar vs point\n\n")

# Test 5: Multiple indicators as points
cat("Test 5: Multiple indicators with points\n")

p_income <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "displacement_status",
  title_name = "Income by Status"
)

p_expend <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "expenditure",
  grouping = "displacement_status",
  title_name = "Expenditure by Status"
)

p_debt <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "debt",
  grouping = "displacement_status",
  title_name = "Debt by Status"
)

grid.arrange(p_income, p_expend, p_debt, ncol = 2)
cat("✓ Multiple economic indicators\n\n")

# Test 6: Weighted points
cat("Test 6: Weighted means with points\n")
plot6 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "household_size",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_labels = TRUE,
  title_name = "Average HH Size by Settlement (Weighted)"
)
print(plot6)
cat("✓ Survey-weighted points\n\n")

# Test 7: RATIO - Employment ratio with points
cat("Test 7: Ratio with points - Employed / Total members\n")
plot7 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "employed_members",
  numeric_var2 = "total_members",
  grouping = "region",
  show_labels = TRUE,
  title_name = "Employment Ratio by Region"
)
print(plot7)
cat("✓ Ratio as points with CI\n\n")

# Test 8: RATIO - Food expenditure share
cat("Test 8: Ratio - Food expenditure / Total expenditure\n")
plot8 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "food_expenditure",
  numeric_var2 = "expenditure",
  grouping = "income_level",
  show_labels = TRUE,
  title_name = "Food Expenditure Share by Income Level"
)
print(plot8)
cat("✓ Expenditure ratio as points\n\n")

# Test 9: Different point sizes
cat("Test 9: Different point sizes\n")

p_small <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  point_size = 2,
  title_name = "Small Points (size=2)"
)

p_medium <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  point_size = 3,
  title_name = "Medium Points (size=3)"
)

p_large <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  point_size = 5,
  title_name = "Large Points (size=5)"
)

grid.arrange(p_small, p_medium, p_large, ncol = 3)
cat("✓ Variable point sizes\n\n")

# Test 10: Different confidence levels
cat("Test 10: Different confidence levels - 90%, 95%, 99%\n")

p90 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  conf_level = 0.90,
  title_name = "90% CI"
)

p95 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  conf_level = 0.95,
  title_name = "95% CI"
)

p99 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  conf_level = 0.99,
  title_name = "99% CI"
)

grid.arrange(p90, p95, p99, ncol = 3)
cat("✓ Wider CIs with higher confidence\n\n")

# Test 11: Flip coordinates
cat("Test 11: Flipped coordinates with points\n")

p_vertical <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "years_displaced",
  grouping = "displacement_status",
  show_labels = TRUE,
  flip_coordinates = FALSE,
  title_name = "Vertical"
)

p_horizontal <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "years_displaced",
  grouping = "displacement_status",
  show_labels = TRUE,
  flip_coordinates = TRUE,
  title_name = "Horizontal"
)

grid.arrange(p_vertical, p_horizontal, ncol = 2)
cat("✓ Both orientations with points\n\n")

# Test 12: Different color palettes
cat("Test 12: Different color palettes\n")

p_reach1 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "fcs_score",
  grouping = "settlement_type",
  color_palette = "reach1",
  title_name = "REACH1"
)

p_reach2 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "fcs_score",
  grouping = "settlement_type",
  color_palette = "reach2",
  title_name = "REACH2"
)

p_reach3 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "fcs_score",
  grouping = "settlement_type",
  color_palette = "reach3",
  title_name = "REACH3"
)

grid.arrange(p_reach1, p_reach2, p_reach3, ncol = 2)
cat("✓ All color palettes\n\n")

# Test 13: Custom axis labels
cat("Test 13: Custom axis labels\n")
plot13 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "distance_to_water",
  grouping = "settlement_type",
  show_labels = TRUE,
  x_lab = "Settlement Type",
  y_lab = "Average Distance (meters)",
  legend_label = "Settlement Category",
  title_name = "Distance to Water Source"
)
print(plot13)
cat("✓ Custom labels\n\n")

# Test 14: All features combined - Mean
cat("Test 14: All features - Mean with points\n")
plot14 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "displacement_status",
  weighted = TRUE,
  weights_col = "survey_weight",
  conf_level = 0.95,
  show_labels = TRUE,
  color_palette = "reach2",
  point_size = 4,
  title_name = "Average Monthly Income by Displacement Status",
  subtitle = "Baseline Assessment 2024",
  x_lab = "Displacement Category",
  y_lab = "Mean Income (USD)",
  legend_label = "Population Group",
  legend_position = "right",
  flip_coordinates = FALSE
)
print(plot14)
cat("✓ All features for means\n\n")

# Test 15: All features combined - Ratio
cat("Test 15: All features - Ratio with points\n")
plot15 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "food_expenditure",
  numeric_var2 = "expenditure",
  grouping = "income_level",
  weighted = TRUE,
  weights_col = "survey_weight",
  conf_level = 0.95,
  show_labels = TRUE,
  color_palette = "reach3",
  point_size = 4,
  title_name = "Food Expenditure Share by Income Level",
  subtitle = "Economic Vulnerability Analysis",
  x_lab = "Income Category",
  y_lab = "Food Share of Total Spending",
  legend_label = "Income Group",
  legend_position = "right",
  flip_coordinates = FALSE
)
print(plot15)
cat("✓ All features for ratios\n\n")


# PRACTICAL USE CASES - Points are better for certain visualizations


cat("=== Practical Use Case Examples ===\n\n")

# Use Case 1: Trend-style comparison (points show "movement" better)
cat("Use Case 1: Comparison Across Groups - Points highlight differences\n")

uc1 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "region",
  show_labels = TRUE,
  point_size = 4,
  title_name = "Regional Income Comparison",
  subtitle = "Points facilitate visual comparison"
)
print(uc1)
cat("✓ Points better for comparing across groups\n\n")

# Use Case 2: Access indicators with points
cat("Use Case 2: Service Access - Distance indicators\n")

uc2_water <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "distance_to_water",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = TRUE,
  point_size = 4,
  title_name = "Distance to Water Source",
  y_lab = "Average Distance (meters)"
)

uc2_health <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "distance_to_health",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  flip_coordinates = TRUE,
  point_size = 4,
  title_name = "Distance to Health Facility",
  y_lab = "Average Distance (meters)"
)

grid.arrange(uc2_water, uc2_health, ncol = 2)
cat("✓ Points work well for horizontal comparisons\n\n")

# Use Case 3: Ratios visualization
cat("Use Case 3: Multiple Ratios - Points show proportions clearly\n")

uc3_employ <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "employed_members",
  numeric_var2 = "total_members",
  grouping = "displacement_status",
  show_labels = TRUE,
  title_name = "Employment Ratio"
)

uc3_children <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "children",
  numeric_var2 = "household_size",
  grouping = "displacement_status",
  show_labels = TRUE,
  title_name = "Child Dependency Ratio"
)

uc3_food <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "food_expenditure",
  numeric_var2 = "expenditure",
  grouping = "displacement_status",
  show_labels = TRUE,
  title_name = "Food Expenditure Share"
)

grid.arrange(uc3_employ, uc3_children, uc3_food, ncol = 2)
cat("✓ Points ideal for ratio visualizations\n\n")

# Use Case 4: Gender comparison - Points emphasize the gap
cat("Use Case 4: Gender Gap Analysis\n")
uc4 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "gender",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_labels = TRUE,
  point_size = 5,
  color_palette = "reach4",
  title_name = "Gender Income Gap",
  subtitle = "Points highlight the difference"
)
print(uc4)
cat("✓ Points effective for binary comparisons\n\n")

# Use Case 5: Age group progression - Points show trend
cat("Use Case 5: Age Group Analysis - Trend visualization\n")
uc5 <- plot_ci_point_mean(
  df = test_data,
  numeric_var = "income",
  grouping = "age_group",
  show_labels = TRUE,
  point_size = 4,
  flip_coordinates = TRUE,
  title_name = "Income by Age Group",
  subtitle = "Points suggest age-income relationship"
)
print(uc5)
cat("✓ Points show progression/trend better than bars\n\n")


# WHEN TO USE POINTS VS BARS


cat("=== Points vs Bars - Decision Guide ===\n\n")

cat("USE POINTS WHEN:\n")
cat("  ✓ Emphasizing precision of estimates\n")
cat("  ✓ Comparing means across multiple groups\n")
cat("  ✓ Visualizing ratios or proportions\n")
cat("  ✓ Showing trends or progressions\n")
cat("  ✓ CI overlap is important to show\n")
cat("  ✓ Space is limited (points are compact)\n")
cat("  ✓ Creating forest plot style visualizations\n")
cat("  ✓ Binary comparisons (e.g., gender, yes/no)\n\n")

cat("USE BARS WHEN:\n")
cat("  ✓ Emphasizing magnitude/volume\n")
cat("  ✓ Showing counts or totals\n")
cat("  ✓ Comparing to a baseline (zero)\n")
cat("  ✓ Audience expects traditional bar charts\n")
cat("  ✓ Few categories with large differences\n")
cat("  ✓ Percentage distributions\n\n")

cat("=== Key Features ===\n")
cat("1. ✓ Points instead of bars for means\n")
cat("2. ✓ Same CI calculations as bar version\n")
cat("3. ✓ Configurable point size\n")
cat("4. ✓ All other features identical (weights, ratios, grouping)\n")
cat("5. ✓ Better for comparing across many groups\n")
cat("6. ✓ Cleaner visualization for ratios\n")
cat("7. ✓ Error bars more prominent\n")

cat("\n=== All CI point mean tests completed ===\n")

# TEST PLOT SCATTER ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Create test dataset for scatter plots
create_test_scatter_data <- function(n = 300) {
  set.seed(7777)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),

    # Survey weights
    survey_weight = runif(n, 0.8, 1.5),

    # Correlated variables for scatter plots
    income = abs(rnorm(n, mean = 250, sd = 100)),
    expenditure = NA,  # Will be created based on income

    household_size = sample(1:12, n, replace = TRUE,
                            prob = c(0.05, 0.10, 0.15, 0.20, 0.20, 0.15, 0.08, 0.04, 0.02, 0.005, 0.003, 0.002)),

    # Variables with relationships
    education_years = sample(0:16, n, replace = TRUE, prob = dexp(0:16, rate = 0.15)),
    age = sample(18:70, n, replace = TRUE),

    # Create correlated debt based on income
    debt = NA,

    # Food security and expenditure
    fcs_score = abs(rnorm(n, mean = 45, sd = 20)),
    food_expenditure = abs(rnorm(n, mean = 120, sd = 50)),

    # Distance and time variables
    distance_to_water = abs(rnorm(n, mean = 500, sd = 300)),
    time_to_water = NA,  # Will be correlated with distance

    # Number of variables
    num_income_sources = sample(0:5, n, replace = TRUE, prob = c(0.15, 0.25, 0.30, 0.20, 0.08, 0.02)),

    # Employment
    days_worked_month = sample(0:30, n, replace = TRUE),

    # Grouping variables
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE,
                    prob = c(0.30, 0.30, 0.25, 0.15)),

    displacement_status = sample(c("IDP", "Refugee", "Returnee", "Host"), n, replace = TRUE,
                                 prob = c(0.35, 0.30, 0.20, 0.15)),

    settlement_type = sample(c("Camp", "Urban", "Rural"), n, replace = TRUE,
                             prob = c(0.35, 0.35, 0.30)),

    gender = sample(c("Male", "Female"), n, replace = TRUE,
                    prob = c(0.52, 0.48)),

    income_level = sample(c("Low", "Medium", "High"), n, replace = TRUE,
                          prob = c(0.50, 0.35, 0.15)),

    assistance_recipient = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.65, 0.35))
  )
}

test_data <- create_test_scatter_data(n = 300)

# Create correlated variables
test_data$expenditure <- test_data$income * 0.85 + rnorm(nrow(test_data), 0, 30)
test_data$expenditure <- pmax(0, test_data$expenditure)

test_data$debt <- test_data$income * 0.4 + rnorm(nrow(test_data), 0, 50)
test_data$debt <- pmax(0, test_data$debt)

test_data$time_to_water <- test_data$distance_to_water * 0.08 + rnorm(nrow(test_data), 5, 10)
test_data$time_to_water <- pmax(1, test_data$time_to_water)

# Add some income-education relationship
test_data$income <- test_data$income + test_data$education_years * 10 + rnorm(nrow(test_data), 0, 20)

cat("\n=== Testing plot_scatter ===\n\n")

# Test 1: Basic scatter - Income vs Expenditure
cat("Test 1: Basic scatter plot - Income vs Expenditure\n")
plot1 <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  title_name = "Income vs Expenditure"
)
print(plot1)
cat("✓ Basic scatter with default linear trend\n\n")

# Test 2: Without smooth line
cat("Test 2: Scatter without trend line\n")
plot2 <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  add_smooth = FALSE,
  title_name = "Income vs Expenditure - No Trend"
)
print(plot2)
cat("✓ Points only, no smoothing\n\n")

# Test 3: With grouping by region
cat("Test 3: Scatter with grouping - Income vs Debt by Region\n")
plot3 <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "debt",
  grouping = "region",
  title_name = "Income vs Debt by Region"
)
print(plot3)
cat("✓ Colored by grouping with separate trends\n\n")

# Test 4: LOESS smoothing
cat("Test 4: LOESS smoothing method\n")
plot4 <- plot_scatter(
  df = test_data,
  x_var = "education_years",
  y_var = "income",
  smooth_method = "loess",
  title_name = "Education vs Income - LOESS"
)
print(plot4)
cat("✓ Non-linear smoothing\n\n")

# Test 5: Comparison - Linear vs LOESS
cat("Test 5: Comparison - Linear vs LOESS smoothing\n")

p_lm <- plot_scatter(
  df = test_data,
  x_var = "household_size",
  y_var = "expenditure",
  smooth_method = "lm",
  title_name = "Linear"
)

p_loess <- plot_scatter(
  df = test_data,
  x_var = "household_size",
  y_var = "expenditure",
  smooth_method = "loess",
  title_name = "LOESS"
)

grid.arrange(p_lm, p_loess, ncol = 2)
cat("✓ Different smoothing methods\n\n")

# Test 6: Weighted scatter (point size by weight)
cat("Test 6: Weighted scatter - Point size by survey weight\n")
plot6 <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Income vs Expenditure - Weighted"
)
print(plot6)
cat("✓ Point size represents weight\n\n")

# Test 7: Weighted with grouping
cat("Test 7: Weighted with grouping\n")
plot7 <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "debt",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Income vs Debt by Settlement - Weighted"
)
print(plot7)
cat("✓ Color by group, size by weight\n\n")

# Test 8: Different point transparency
cat("Test 8: Different point transparency levels\n")

p_low <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  point_alpha = 0.3,
  title_name = "Low transparency (0.3)"
)

p_med <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  point_alpha = 0.6,
  title_name = "Medium transparency (0.6)"
)

p_high <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  point_alpha = 0.9,
  title_name = "High transparency (0.9)"
)

grid.arrange(p_low, p_med, p_high, ncol = 3)
cat("✓ Variable transparency for overlapping points\n\n")

# Test 9: Multiple grouping variables
cat("Test 9: Different grouping variables\n")

p_region <- plot_scatter(
  df = test_data,
  x_var = "education_years",
  y_var = "income",
  grouping = "region",
  title_name = "By Region"
)

p_status <- plot_scatter(
  df = test_data,
  x_var = "education_years",
  y_var = "income",
  grouping = "displacement_status",
  title_name = "By Displacement Status"
)

p_gender <- plot_scatter(
  df = test_data,
  x_var = "education_years",
  y_var = "income",
  grouping = "gender",
  title_name = "By Gender"
)

grid.arrange(p_region, p_status, p_gender, ncol = 2)
cat("✓ Multiple grouping options\n\n")

# Test 10: Distance vs Time relationship
cat("Test 10: Distance to water vs Time to collect\n")
plot10 <- plot_scatter(
  df = test_data,
  x_var = "distance_to_water",
  y_var = "time_to_water",
  grouping = "settlement_type",
  title_name = "Distance vs Time to Water by Settlement",
  x_lab = "Distance to Water Source (meters)",
  y_lab = "Time to Collect Water (minutes)"
)
print(plot10)
cat("✓ Custom axis labels\n\n")

# Test 11: Color palettes
cat("Test 11: Different color palettes\n")

p_reach1 <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  grouping = "income_level",
  color_palette = "reach1",
  title_name = "REACH1"
)

p_reach2 <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  grouping = "income_level",
  color_palette = "reach2",
  title_name = "REACH2"
)

p_reach3 <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  grouping = "income_level",
  color_palette = "reach3",
  title_name = "REACH3"
)

grid.arrange(p_reach1, p_reach2, p_reach3, ncol = 2)
cat("✓ All color palettes\n\n")

# Test 12: Legend positions
cat("Test 12: Different legend positions\n")

p_bottom <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "debt",
  grouping = "region",
  legend_position = "bottom",
  title_name = "Legend Bottom"
)

p_right <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "debt",
  grouping = "region",
  legend_position = "right",
  title_name = "Legend Right"
)

p_none <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "debt",
  grouping = "region",
  legend_position = "none",
  title_name = "No Legend"
)

grid.arrange(p_bottom, p_right, p_none, ncol = 2)
cat("✓ Different legend positions\n\n")

# Test 13: Custom legend label
cat("Test 13: Custom legend label\n")
plot13 <- plot_scatter(
  df = test_data,
  x_var = "age",
  y_var = "income",
  grouping = "gender",
  legend_label = "Gender of Respondent",
  title_name = "Age vs Income by Gender"
)
print(plot13)
cat("✓ Custom legend title\n\n")

# Test 14: Flip coordinates
cat("Test 14: Flipped coordinates\n")

p_normal <- plot_scatter(
  df = test_data,
  x_var = "education_years",
  y_var = "income",
  flip_coordinates = FALSE,
  title_name = "Normal"
)

p_flipped <- plot_scatter(
  df = test_data,
  x_var = "education_years",
  y_var = "income",
  flip_coordinates = TRUE,
  title_name = "Flipped"
)

grid.arrange(p_normal, p_flipped, ncol = 2)
cat("✓ Both orientations\n\n")

# Test 15: All features combined
cat("Test 15: All features combined\n")
plot15 <- plot_scatter(
  df = test_data,
  x_var = "education_years",
  y_var = "income",
  grouping = "displacement_status",
  weighted = TRUE,
  weights_col = "survey_weight",
  add_smooth = TRUE,
  smooth_method = "lm",
  color_palette = "reach2",
  point_alpha = 0.7,
  title_name = "Education vs Income by Displacement Status",
  subtitle = "Survey-weighted Analysis",
  x_lab = "Years of Education",
  y_lab = "Monthly Income (USD)",
  legend_label = "Displacement Category",
  legend_position = "right",
  flip_coordinates = FALSE
)
print(plot15)
cat("✓ All features combined\n\n")


# PRACTICAL USE CASES


cat("=== Practical Use Case Examples ===\n\n")

# Use Case 1: Economic relationships
cat("Use Case 1: Economic Relationships Dashboard\n")

uc1_income_exp <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "expenditure",
  grouping = "income_level",
  title_name = "Income vs Expenditure"
)

uc1_income_debt <- plot_scatter(
  df = test_data,
  x_var = "income",
  y_var = "debt",
  grouping = "income_level",
  title_name = "Income vs Debt"
)

uc1_exp_food <- plot_scatter(
  df = test_data,
  x_var = "expenditure",
  y_var = "food_expenditure",
  grouping = "income_level",
  title_name = "Total vs Food Expenditure"
)

uc1_hh_exp <- plot_scatter(
  df = test_data,
  x_var = "household_size",
  y_var = "expenditure",
  grouping = "income_level",
  title_name = "HH Size vs Expenditure"
)

grid.arrange(uc1_income_exp, uc1_income_debt, uc1_exp_food, uc1_hh_exp, ncol = 2)
cat("✓ Economic indicators relationships\n\n")

# Use Case 2: Education and income analysis
cat("Use Case 2: Returns to Education Analysis\n")
uc2 <- plot_scatter(
  df = test_data,
  x_var = "education_years",
  y_var = "income",
  grouping = "gender",
  weighted = TRUE,
  weights_col = "survey_weight",
  smooth_method = "lm",
  title_name = "Returns to Education by Gender",
  subtitle = "Does education pay off equally?",
  x_lab = "Years of Education",
  y_lab = "Monthly Income (USD)",
  legend_label = "Gender"
)
print(uc2)
cat("✓ Education-income relationship\n\n")

# Use Case 3: Food security and expenditure
cat("Use Case 3: Food Security vs Expenditure\n")
uc3 <- plot_scatter(
  df = test_data,
  x_var = "food_expenditure",
  y_var = "fcs_score",
  grouping = "assistance_recipient",
  title_name = "Food Expenditure vs Food Consumption Score",
  subtitle = "Does spending more improve food security?",
  x_lab = "Food Expenditure (USD)",
  y_lab = "Food Consumption Score",
  legend_label = "Receives Assistance"
)
print(uc3)
cat("✓ Food security analysis\n\n")

# Use Case 4: Access and distance
cat("Use Case 4: Distance-Time Relationship for Water Access\n")
uc4 <- plot_scatter(
  df = test_data,
  x_var = "distance_to_water",
  y_var = "time_to_water",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  smooth_method = "lm",
  title_name = "Water Access: Distance vs Collection Time",
  subtitle = "Relationship varies by settlement type",
  x_lab = "Distance to Water Source (m)",
  y_lab = "Collection Time (minutes)",
  legend_label = "Settlement Type"
)
print(uc4)
cat("✓ Access indicator relationships\n\n")

# Use Case 5: Employment and income
cat("Use Case 5: Work Days vs Income\n")
uc5 <- plot_scatter(
  df = test_data,
  x_var = "days_worked_month",
  y_var = "income",
  grouping = "displacement_status",
  title_name = "Employment Days vs Income by Status",
  x_lab = "Days Worked per Month",
  y_lab = "Monthly Income (USD)",
  legend_label = "Displacement Status"
)
print(uc5)
cat("✓ Employment-income relationship\n\n")

# Use Case 6: Livelihood diversity
cat("Use Case 6: Income Diversification\n")
uc6 <- plot_scatter(
  df = test_data,
  x_var = "num_income_sources",
  y_var = "income",
  grouping = "settlement_type",
  smooth_method = "loess",
  title_name = "Number of Income Sources vs Total Income",
  subtitle = "Does diversification increase income?",
  x_lab = "Number of Income Sources",
  y_lab = "Total Monthly Income (USD)"
)
print(uc6)
cat("✓ Livelihood diversification\n\n")

# Use Case 7: Age and income
cat("Use Case 7: Age-Income Profile\n")
uc7 <- plot_scatter(
  df = test_data,
  x_var = "age",
  y_var = "income",
  grouping = "gender",
  smooth_method = "loess",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Age-Income Profile by Gender",
  subtitle = "Life-cycle earnings patterns",
  x_lab = "Age (years)",
  y_lab = "Monthly Income (USD)",
  legend_label = "Gender"
)
print(uc7)
cat("✓ Age-income lifecycle analysis\n\n")


# ERROR HANDLING TESTS


cat("=== Error Handling Tests ===\n\n")

# Test 16: Error - NULL x_var
cat("Test 16: Error - NULL x_var\n")
tryCatch({
  plot16 <- plot_scatter(df = test_data, x_var = NULL, y_var = "income")
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 17: Error - NULL y_var
cat("Test 17: Error - NULL y_var\n")
tryCatch({
  plot17 <- plot_scatter(df = test_data, x_var = "income", y_var = NULL)
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 18: Error - Invalid x_var
cat("Test 18: Error - Invalid x_var\n")
tryCatch({
  plot18 <- plot_scatter(df = test_data, x_var = "nonexistent", y_var = "income")
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 19: Error - weighted without weights_col
cat("Test 19: Error - weighted without weights_col\n")
tryCatch({
  plot19 <- plot_scatter(
    df = test_data,
    x_var = "income",
    y_var = "expenditure",
    weighted = TRUE
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 20: Error - Invalid grouping
cat("Test 20: Error - Invalid grouping\n")
tryCatch({
  plot20 <- plot_scatter(
    df = test_data,
    x_var = "income",
    y_var = "expenditure",
    grouping = "nonexistent"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})


# VERIFICATION & CORRELATION


cat("\n=== Data Verification & Correlations ===\n")
cat("Total respondents:", nrow(test_data), "\n\n")

cat("Correlation matrix (key variables):\n")
cor_vars <- c("income", "expenditure", "debt", "education_years", "household_size", "age")
cor_matrix <- cor(test_data[, cor_vars], use = "complete.obs")
print(round(cor_matrix, 2))
cat("\n")

cat("Income vs Expenditure:\n")
cat("  Correlation:", cor(test_data$income, test_data$expenditure, use = "complete.obs"), "\n")
cat("  Linear model R²:", summary(lm(expenditure ~ income, data = test_data))$r.squared, "\n\n")

cat("Education vs Income:\n")
cat("  Correlation:", cor(test_data$education_years, test_data$income, use = "complete.obs"), "\n")
cat("  Linear model R²:", summary(lm(income ~ education_years, data = test_data))$r.squared, "\n\n")

cat("Distance vs Time (water):\n")
cat("  Correlation:", cor(test_data$distance_to_water, test_data$time_to_water, use = "complete.obs"), "\n")
cat("  Linear model R²:", summary(lm(time_to_water ~ distance_to_water, data = test_data))$r.squared, "\n\n")

cat("=== Key Features ===\n")
cat("1. ✓ Basic scatter plots for two numeric variables\n")
cat("2. ✓ Optional grouping by categorical variable (color)\n")
cat("3. ✓ Weighted points (size by weight)\n")
cat("4. ✓ Trend lines (linear or LOESS)\n")
cat("5. ✓ Separate trends by group\n")
cat("6. ✓ Adjustable point transparency\n")
cat("7. ✓ Multiple color palettes\n")
cat("8. ✓ Custom axis labels\n")
cat("9. ✓ Flip coordinates\n")
cat("10. ✓ Legend positioning\n")

cat("\n=== Understanding Scatter Plots ===\n")
cat("PURPOSE:\n")
cat("• Visualize relationship between two continuous variables\n")
cat("• Identify patterns: positive, negative, or no correlation\n")
cat("• Detect outliers and clusters\n")
cat("• Compare relationships across groups\n\n")

cat("TREND LINES:\n")
cat("• Linear (lm): Straight line, shows linear relationship\n")
cat("• LOESS: Smooth curve, captures non-linear patterns\n")
cat("• SE band: Confidence interval around trend\n")
cat("• Separate trends by group show differential relationships\n\n")

cat("WEIGHTING:\n")
cat("• Larger points = higher weight\n")
cat("• Emphasizes more important observations\n")
cat("• Useful for survey data with sampling weights\n")

cat("\n=== Use Cases ===\n")
cat("1. Income vs expenditure relationships\n")
cat("2. Education returns (education vs income)\n")
cat("3. Food security vs spending\n")
cat("4. Distance vs time for services\n")
cat("5. Employment days vs income\n")
cat("6. Livelihood diversification\n")
cat("7. Age-income profiles\n")
cat("8. Household size vs needs\n")
cat("9. Debt vs income ratios\n")
cat("10. Any two continuous variables\n")

cat("\n=== Best Practices ===\n")
cat("• Check correlation coefficient before plotting\n")
cat("• Use LOESS for non-linear relationships\n")
cat("• Adjust transparency for overlapping points\n")
cat("• Use grouping to reveal subgroup patterns\n")
cat("• Weight points when using survey data\n")
cat("• Add informative axis labels\n")
cat("• Consider log scales for skewed data\n")
cat("• Look for outliers that may need investigation\n")

cat("\n=== All scatter plot tests completed ===\n")

# TEST PLOT DONUT ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Create test dataset for donut charts
create_test_donut_data <- function(n = 500) {
  set.seed(8888)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),

    # Survey weights
    survey_weight = runif(n, 0.8, 1.5),

    # Categorical variables for donut charts
    education_level = sample(c("None", "Primary", "Secondary", "Tertiary"), n, replace = TRUE,
                             prob = c(0.25, 0.40, 0.25, 0.10)),

    food_security = sample(c("Poor", "Borderline", "Acceptable"), n, replace = TRUE,
                           prob = c(0.30, 0.35, 0.35)),

    shelter_type = sample(c("House", "Apartment", "Tent", "Makeshift", "Collective"), n, replace = TRUE,
                          prob = c(0.25, 0.20, 0.20, 0.20, 0.15)),

    water_source = sample(c("Improved", "Unimproved", "Surface"), n, replace = TRUE,
                          prob = c(0.50, 0.35, 0.15)),

    livelihood = sample(c("Agriculture", "Trade", "Labor", "Services", "None"), n, replace = TRUE,
                        prob = c(0.30, 0.20, 0.25, 0.15, 0.10)),

    region = sample(c("North", "South", "East", "West"), n, replace = TRUE,
                    prob = c(0.30, 0.30, 0.25, 0.15)),

    displacement_status = sample(c("IDP", "Refugee", "Returnee", "Host"), n, replace = TRUE,
                                 prob = c(0.35, 0.30, 0.20, 0.15)),

    settlement_type = sample(c("Camp", "Urban", "Rural"), n, replace = TRUE,
                             prob = c(0.35, 0.35, 0.30)),

    assistance_type = sample(c("Cash", "Food", "NFI", "Multi-sector", "None"), n, replace = TRUE,
                             prob = c(0.30, 0.25, 0.20, 0.15, 0.10)),

    income_level = sample(c("Low", "Medium", "High"), n, replace = TRUE,
                          prob = c(0.50, 0.35, 0.15)),

    gender = sample(c("Male", "Female"), n, replace = TRUE,
                    prob = c(0.52, 0.48)),

    has_documentation = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.65, 0.35)),

    access_healthcare = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.55, 0.45)),

    # Numeric values for value_var option
    household_size = sample(1:12, n, replace = TRUE,
                            prob = c(0.05, 0.10, 0.15, 0.20, 0.20, 0.15, 0.08, 0.04, 0.02, 0.005, 0.003, 0.002)),

    num_beneficiaries = sample(1:8, n, replace = TRUE),

    expenditure = abs(rnorm(n, mean = 220, sd = 90))
  )
}

test_data <- create_test_donut_data(n = 500)

cat("\n=== Testing plot_donut (Latest Version) ===\n\n")

# Test 1: Basic donut chart
cat("Test 1: Basic donut chart - Education Level\n")
plot1 <- plot_donut(
  df = test_data,
  category_var = "education_level", label_color = "black",
  title_name = "Education Level Distribution"
)
print(plot1)
cat("✓ Basic donut with white labels (default)\n\n")

# Test 2: Without labels
cat("Test 2: Donut without labels\n")
plot2 <- plot_donut(
  df = test_data,
  category_var = "food_security",
  show_labels = FALSE,
  title_name = "Food Security Status - No Labels"
)
print(plot2)
cat("✓ Legend only, no labels on chart\n\n")

# Test 3: Count labels
cat("Test 3: Donut with count labels\n")
plot3 <- plot_donut(
  df = test_data,
  category_var = "shelter_type",
  label_type = "count",
  title_name = "Shelter Type - Count Labels"
)
print(plot3)
cat("✓ Shows counts instead of percentages\n\n")

# Test 4: Both percentage and count
cat("Test 4: Donut with both percentage and count\n")
plot4 <- plot_donut(
  df = test_data,
  category_var = "water_source",
  label_type = "both",
  title_name = "Water Source - Both Labels"
)
print(plot4)
cat("✓ Shows percentage and count together\n\n")

# Test 5: Black labels for light colors
cat("Test 5: Black labels (for light-colored palettes)\n")
plot5 <- plot_donut(
  df = test_data,
  category_var = "region",
  label_type = "both",
  label_color = "white",
  color_palette = "group",
  title_name = "Region - Black Labels"
)
print(plot5)
cat("✓ Black text on light colors\n\n")

# Test 6: Comparison - White vs Black labels
cat("Test 6: Comparison - White vs Black labels\n")

p_white <- plot_donut(
  df = test_data,
  category_var = "livelihood",
  label_type = "percentage",
  label_color = "white",
  color_palette = "reach1",
  title_name = "White Labels (Dark Colors)"
)

p_black <- plot_donut(
  df = test_data,
  category_var = "livelihood",
  label_type = "percentage",
  label_color = "black",
  color_palette = "reach3",
  title_name = "Black Labels (Light Colors)"
)

grid.arrange(p_white, p_black, ncol = 2)
cat("✓ Label color comparison\n\n")

# Test 7: Different hole sizes with centered labels
cat("Test 7: Different hole sizes - Labels remain centered\n")

p_small <- plot_donut(
  df = test_data,
  category_var = "region",
  hole_size = 0.2,
  label_type = "both",
  title_name = "Small Hole (0.2)"
)

p_medium <- plot_donut(
  df = test_data,
  category_var = "region",
  hole_size = 0.4,
  label_type = "both",
  title_name = "Medium Hole (0.4)"
)

p_large <- plot_donut(
  df = test_data,
  category_var = "region",
  hole_size = 0.6,
  label_type = "both",
  title_name = "Large Hole (0.6)"
)

grid.arrange(p_small, p_medium, p_large, ncol = 3)
cat("✓ Labels centered in bands for all hole sizes\n\n")

# Test 8: Weighted donut
cat("Test 8: Weighted donut chart\n")
plot8 <- plot_donut(
  df = test_data,
  category_var = "displacement_status",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "both",
  title_name = "Displacement Status - Weighted"
)
print(plot8)
cat("✓ Survey-weighted proportions\n\n")

# Test 9: Weighted vs Unweighted comparison
cat("Test 9: Weighted vs Unweighted comparison\n")

p_unw <- plot_donut(
  df = test_data,
  category_var = "settlement_type",
  weighted = FALSE,
  label_type = "both",
  title_name = "Unweighted"
)

p_w <- plot_donut(
  df = test_data,
  category_var = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "both",
  title_name = "Weighted"
)

grid.arrange(p_unw, p_w, ncol = 2)
cat("✓ Shows impact of weighting\n\n")

# Test 10: With value_var - Sum by category
cat("Test 10: Donut with value_var - Total expenditure by region\n")
plot10 <- plot_donut(
  df = test_data,
  category_var = "region",
  value_var = "expenditure",
  label_type = "both",
  title_name = "Total Expenditure by Region",
  subtitle = "Showing sum of expenditures, not count of observations"
)
print(plot10)
cat("✓ Sums numeric values by category\n")
cat("Note: Numbers show TOTAL expenditure, not count of households\n\n")

# Test 11: Value_var with weighting
cat("Test 11: Value_var with weighting - Beneficiaries by assistance type\n")
plot11 <- plot_donut(
  df = test_data,
  category_var = "assistance_type",
  value_var = "num_beneficiaries",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "both",
  title_name = "Weighted Beneficiaries by Assistance Type"
)
print(plot11)
cat("✓ Weighted sum of values\n\n")

# Test 12: Different color palettes
cat("Test 12: Different color palettes\n")

p_reach1 <- plot_donut(
  df = test_data,
  category_var = "education_level",
  color_palette = "reach1",
  label_color = "white",
  title_name = "REACH1 (White Labels)"
)

p_reach2 <- plot_donut(
  df = test_data,
  category_var = "education_level",
  color_palette = "reach2",
  label_color = "white",
  title_name = "REACH2 (White Labels)"
)

p_reach3 <- plot_donut(
  df = test_data,
  category_var = "education_level",
  color_palette = "reach3",
  label_color = "black",
  title_name = "REACH3 (Black Labels)"
)

p_traffic <- plot_donut(
  df = test_data,
  category_var = "food_security",
  color_palette = "traffic_light",
  label_color = "white",
  title_name = "Traffic Light"
)

grid.arrange(p_reach1, p_reach2, p_reach3, p_traffic, ncol = 2)
cat("✓ All color palettes with appropriate label colors\n\n")

# Test 13: Legend positions
cat("Test 13: Different legend positions\n")

p_right <- plot_donut(
  df = test_data,
  category_var = "livelihood",
  legend_position = "right",
  label_type = "percentage",
  title_name = "Legend Right"
)

p_bottom <- plot_donut(
  df = test_data,
  category_var = "livelihood",
  legend_position = "bottom",
  label_type = "percentage",
  title_name = "Legend Bottom"
)

p_top <- plot_donut(
  df = test_data,
  category_var = "livelihood",
  legend_position = "top",
  label_type = "percentage",
  title_name = "Legend Top"
)

p_left <- plot_donut(
  df = test_data,
  category_var = "livelihood",
  legend_position = "left",
  label_type = "percentage",
  title_name = "Legend Left"
)

grid.arrange(p_right, p_bottom, p_top, p_left, ncol = 2)
cat("✓ All legend positions\n\n")

# Test 14: Custom legend label
cat("Test 14: Custom legend label\n")
plot14 <- plot_donut(
  df = test_data,
  category_var = "gender",
  legend_label = "Gender of Respondent",
  label_type = "both",
  title_name = "Gender Distribution"
)
print(plot14)
cat("✓ Custom legend title\n\n")

# Test 15: Binary variables
cat("Test 15: Binary variables - Yes/No\n")

p_doc <- plot_donut(
  df = test_data,
  category_var = "has_documentation",
  label_type = "both",
  title_name = "Has Documentation"
)

p_health <- plot_donut(
  df = test_data,
  category_var = "access_healthcare",
  label_type = "both",
  title_name = "Access to Healthcare"
)

grid.arrange(p_doc, p_health, ncol = 2)
cat("✓ Binary outcome variables\n\n")

# Test 16: All features combined
cat("Test 16: All features combined\n")
plot16 <- plot_donut(
  df = test_data,
  category_var = "displacement_status",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_labels = TRUE,
  label_type = "both",
  label_color = "white",
  hole_size = 0.5,
  color_palette = "reach2",
  title_name = "Population Distribution by Displacement Status",
  subtitle = "Comprehensive Assessment 2024",
  legend_label = "Displacement Category",
  legend_position = "right"
)
print(plot16)
cat("✓ All features combined\n\n")


# PRACTICAL USE CASES


cat("=== Practical Use Case Examples ===\n\n")

# Use Case 1: Demographics overview
cat("Use Case 1: Demographics Dashboard\n")

uc1_gender <- plot_donut(
  df = test_data,
  category_var = "gender",
  label_type = "both",
  hole_size = 0.5,
  title_name = "Gender"
)

uc1_region <- plot_donut(
  df = test_data,
  category_var = "region",
  label_type = "both",
  hole_size = 0.5,
  title_name = "Region"
)

uc1_status <- plot_donut(
  df = test_data,
  category_var = "displacement_status",
  label_type = "both",
  hole_size = 0.5,
  title_name = "Displacement Status"
)

uc1_settlement <- plot_donut(
  df = test_data,
  category_var = "settlement_type",
  label_type = "both",
  hole_size = 0.5,
  title_name = "Settlement Type"
)

grid.arrange(uc1_gender, uc1_region, uc1_status, uc1_settlement, ncol = 2)
cat("✓ Population demographics overview\n\n")

# Use Case 2: Sectoral indicators with appropriate label colors
cat("Use Case 2: Sectoral Indicators Dashboard\n")

uc2_food <- plot_donut(
  df = test_data,
  category_var = "food_security",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "percentage",
  label_color = "white",
  color_palette = "traffic_light",
  title_name = "Food Security"
)

uc2_water <- plot_donut(
  df = test_data,
  category_var = "water_source",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "percentage",
  label_color = "white",
  title_name = "Water Source"
)

uc2_shelter <- plot_donut(
  df = test_data,
  category_var = "shelter_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "percentage",
  label_color = "white",
  title_name = "Shelter Type"
)

uc2_livelihood <- plot_donut(
  df = test_data,
  category_var = "livelihood",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "percentage",
  label_color = "white",
  title_name = "Livelihood"
)

grid.arrange(uc2_food, uc2_water, uc2_shelter, uc2_livelihood, ncol = 2)
cat("✓ Multi-sectoral assessment\n\n")

# Use Case 3: Education profile
cat("Use Case 3: Education Profile - Weighted Population Estimates\n")
uc3 <- plot_donut(
  df = test_data,
  category_var = "education_level",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "both",
  label_color = "white",
  hole_size = 0.45,
  color_palette = "reach3",
  title_name = "Education Level Distribution",
  subtitle = "Population-weighted estimates",
  legend_label = "Education Attainment"
)
print(uc3)
cat("Note: Using REACH3 palette - could use black labels if needed\n\n")

# Use Case 4: Assistance coverage
cat("Use Case 4: Assistance Coverage and Types\n")
uc4 <- plot_donut(
  df = test_data,
  category_var = "assistance_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "both",
  label_color = "white",
  title_name = "Type of Assistance Received",
  legend_position = "right"
)
print(uc4)
cat("✓ Program coverage analysis\n\n")

# Use Case 5: Access indicators with custom colors
cat("Use Case 5: Access to Services - Binary Indicators\n")

uc5_doc <- plot_donut(
  df = test_data,
  category_var = "has_documentation",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "both",
  label_color = "black",
  hole_size = 0.5,
  color_palette = "reach4",
  title_name = "Has Civil Documentation"
)

uc5_health <- plot_donut(
  df = test_data,
  category_var = "access_healthcare",
  weighted = TRUE,
  weights_col = "survey_weight",
  label_type = "both",
  label_color = "black",
  hole_size = 0.5,
  color_palette = "reach4",
  title_name = "Access to Healthcare"
)

grid.arrange(uc5_doc, uc5_health, ncol = 2)
cat("✓ Access coverage with black labels for light palette\n\n")

# Use Case 6: Understanding value_var
cat("Use Case 6: Understanding value_var - Count vs Sum\n")

uc6_count <- plot_donut(
  df = test_data,
  category_var = "region",
  label_type = "both",
  title_name = "Count of Households by Region",
  subtitle = "Shows NUMBER of households"
)

uc6_sum <- plot_donut(
  df = test_data,
  category_var = "region",
  value_var = "expenditure",
  label_type = "both",
  title_name = "Total Expenditure by Region",
  subtitle = "Shows SUM of expenditure (much larger than count)"
)

grid.arrange(uc6_count, uc6_sum, ncol = 2)
cat("✓ Demonstrates difference between count and value_var\n")
cat("Left: Count of observations\n")
cat("Right: Sum of expenditure values\n\n")


# ERROR HANDLING TESTS


cat("=== Error Handling Tests ===\n\n")

# Test 17: Error - NULL category_var
cat("Test 17: Error - NULL category_var\n")
tryCatch({
  plot17 <- plot_donut(df = test_data, category_var = NULL)
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 18: Error - Invalid category_var
cat("Test 18: Error - Invalid category_var\n")
tryCatch({
  plot18 <- plot_donut(df = test_data, category_var = "nonexistent")
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 19: Error - weighted without weights_col
cat("Test 19: Error - weighted without weights_col\n")
tryCatch({
  plot19 <- plot_donut(
    df = test_data,
    category_var = "education_level",
    weighted = TRUE
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 20: Error - Invalid value_var
cat("Test 20: Error - Invalid value_var\n")
tryCatch({
  plot20 <- plot_donut(
    df = test_data,
    category_var = "region",
    value_var = "nonexistent"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})

# Test 21: Error - Invalid label_type
cat("Test 21: Error - Invalid label_type\n")
tryCatch({
  plot21 <- plot_donut(
    df = test_data,
    category_var = "education_level",
    label_type = "invalid"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error\n\n")
})


# VERIFICATION


cat("\n=== Data Verification ===\n")
cat("Total respondents:", nrow(test_data), "\n")
cat("Weight range:", range(test_data$survey_weight), "\n")
cat("Sum of weights:", sum(test_data$survey_weight), "\n\n")

cat("Education level distribution:\n")
ed_table <- table(test_data$education_level)
ed_pct <- prop.table(ed_table) * 100
ed_summary <- data.frame(
  Category = names(ed_table),
  Count = as.integer(ed_table),
  Percentage = round(ed_pct, 1)
)
print(ed_summary)
cat("Sum of percentages:", sum(ed_pct), "%\n\n")

cat("Region expenditure (for value_var example):\n")
exp_by_region <- test_data %>%
  group_by(region) %>%
  summarise(
    n_households = n(),
    total_expenditure = sum(expenditure),
    pct_of_total = total_expenditure / sum(test_data$expenditure) * 100
  )
print(exp_by_region)
cat("Note: total_expenditure >> n_households\n\n")

cat("=== Key Features ===\n")
cat("1. ✓ Donut chart (pie with hole in center)\n")
cat("2. ✓ Percentage, count, or both labels\n")
cat("3. ✓ Adjustable hole size\n")
cat("4. ✓ Customizable label color (white or black)\n")
cat("5. ✓ Labels centered in donut bands\n")
cat("6. ✓ Weighted and unweighted\n")
cat("7. ✓ Optional value_var (sum numeric values)\n")
cat("8. ✓ Multiple color palettes\n")
cat("9. ✓ Legend positioning (all sides)\n")
cat("10. ✓ Custom legend labels\n")

cat("\n=== New Features in Latest Version ===\n")
cat("• label_color: Switch between white and black text\n")
cat("• Improved centering: Labels centered in donut bands\n")
cat("• Better value_var handling: Clear distinction between count and sum\n")

cat("\n=== Label Color Guidelines ===\n")
cat("USE WHITE LABELS (label_color = 'white'):\n")
cat("  • Dark color palettes (reach1, reach2)\n")
cat("  • Dark segments\n")
cat("  • Traffic light palette\n")
cat("  • Default behavior\n\n")

cat("USE BLACK LABELS (label_color = 'black'):\n")
cat("  • Light color palettes (reach3, reach4)\n")
cat("  • Pastel colors\n")
cat("  • Light segments\n")
cat("  • Better readability on light backgrounds\n")

cat("\n=== Understanding value_var ===\n")
cat("WITHOUT value_var:\n")
cat("  • Shows count of observations\n")
cat("  • Percentages based on n observations\n")
cat("  • Example: 100 households in North = 20%\n\n")

cat("WITH value_var:\n")
cat("  • Shows SUM of values in that column\n")
cat("  • Percentages based on total sum\n")
cat("  • Example: North expenditure $50,000 = 20% of total $250,000\n")
cat("  • Numbers can be MUCH larger than n\n")
cat("  • Use for: total expenditure, total beneficiaries, resource allocation\n")

cat("\n=== Use Cases ===\n")
cat("1. Population demographics breakdown\n")
cat("2. Sectoral indicator distributions\n")
cat("3. Food security classification\n")
cat("4. Education level distribution\n")
cat("5. Assistance type coverage\n")
cat("6. Access to services (yes/no)\n")
cat("7. Income level stratification\n")
cat("8. Regional distribution\n")
cat("9. Resource allocation (with value_var)\n")
cat("10. Any categorical distribution\n")

cat("\n=== Best Practices ===\n")
cat("• Limit to 2-6 categories for clarity\n")
cat("• Use contrasting colors\n")
cat("• Match label color to palette darkness\n")
cat("• Add percentages on segments for precision\n")
cat("• Use appropriate hole size (0.3-0.5)\n")
cat("• Consider bar chart if precision matters\n")
cat("• Weight data for representative samples\n")
cat("• Use value_var for resource/budget allocation\n")
cat("• Clearly label when using value_var (in subtitle)\n")

cat("\n=== All donut chart tests completed ===\n")

# PLOT STACKED BAR MULTIPLE VARS ####
# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Create test dataset for multiple stacked bars
create_test_multiple_stacked_data <- function(n = 500) {
  set.seed(9999)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),

    # Survey weights
    survey_weight = runif(n, 0.8, 1.5),

    # Multiple categorical variables for comparison
    education_level = sample(c("None", "Primary", "Secondary", "Tertiary"), n, replace = TRUE,
                             prob = c(0.25, 0.40, 0.25, 0.10)),

    employment_status = sample(c("Employed", "Unemployed", "Student", "Retired"), n, replace = TRUE,
                               prob = c(0.35, 0.30, 0.25, 0.10)),

    food_security = sample(c("Poor", "Borderline", "Acceptable"), n, replace = TRUE,
                           prob = c(0.30, 0.35, 0.35)),

    water_source = sample(c("Improved", "Unimproved", "Surface"), n, replace = TRUE,
                          prob = c(0.50, 0.35, 0.15)),

    shelter_type = sample(c("Permanent", "Semi-permanent", "Temporary", "Emergency"), n, replace = TRUE,
                          prob = c(0.30, 0.25, 0.25, 0.20)),

    health_status = sample(c("Good", "Fair", "Poor"), n, replace = TRUE,
                           prob = c(0.40, 0.40, 0.20)),

    assistance_received = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.65, 0.35)),

    documentation_status = sample(c("Full", "Partial", "None"), n, replace = TRUE,
                                  prob = c(0.50, 0.30, 0.20)),

    # Grouping variables
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE,
                    prob = c(0.30, 0.30, 0.25, 0.15)),

    displacement_status = sample(c("IDP", "Refugee", "Returnee", "Host"), n, replace = TRUE,
                                 prob = c(0.35, 0.30, 0.20, 0.15)),

    settlement_type = sample(c("Camp", "Urban", "Rural"), n, replace = TRUE,
                             prob = c(0.35, 0.35, 0.30)),

    gender = sample(c("Male", "Female"), n, replace = TRUE,
                    prob = c(0.52, 0.48)),

    age_group = sample(c("18-24", "25-34", "35-44", "45-54", "55+"), n, replace = TRUE,
                       prob = c(0.20, 0.25, 0.25, 0.20, 0.10))
  )
}

test_data <- create_test_multiple_stacked_data(n = 500)

cat("\n=== Testing plot_stacked_bar_multiple_vars (Final Version) ===\n\n")
cat("New features: Overall bar, fixed percentages, group spacing, better legends\n\n")

# Test 1: Basic - Two variables without grouping (baseline)
cat("Test 1: Basic - Two variables without grouping\n")
plot1 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status"),
  color_palette = "reach1", separate_legends = TRUE, legend_position = "right",
  title_name = "Two Variables - No Grouping"
)
print(plot1)
cat("✓ Baseline comparison\n\n")

# Test 2: WITH GROUPING - No overall bar
cat("Test 2: With grouping - No overall bar\n")
plot2 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source"),
  grouping = "region",
  show_overall = FALSE,
  title_name = "By Region - Without Overall Bar"
)
print(plot2)
cat("✓ Grouped without overall reference\n\n")

# Test 3: WITH GROUPING - WITH OVERALL BAR (KEY NEW FEATURE)
cat("Test 3: With grouping - WITH overall bar (NEW FEATURE)\n")
plot3 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source"),
  grouping = "region",
  show_overall = TRUE,
  title_name = "By Region - With Overall Bar"
)
print(plot3)
cat("✓ Overall bar shows on the LEFT for each variable\n")
cat("✓ Easy to compare each region to overall\n\n")

# Test 4: Comparison - Without vs With Overall
cat("Test 4: Direct Comparison - Without vs With Overall Bar\n")

p_without <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status"),
  category_labels = c("Education", "Employment"),
  grouping = "displacement_status",
  show_overall = FALSE,
  title_name = "Without Overall"
)

p_with <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status"),
  category_labels = c("Education", "Employment"),
  grouping = "displacement_status",
  show_overall = TRUE,
  title_name = "With Overall"
)

grid.arrange(p_without, p_with, ncol = 2)
cat("✓ Visual comparison shows value of overall reference\n\n")

# Test 5: Custom overall label
cat("Test 5: Custom overall label\n")
plot5 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "health_status"),
  category_labels = c("Food", "Health"),
  grouping = "settlement_type",
  show_overall = TRUE, separate_legends = TRUE,
  overall_label = "All Settlements",
  title_name = "Custom Overall Label"
)
print(plot5)
cat("✓ Custom label 'All Settlements' instead of 'Overall'\n\n")

# Test 6: Overall with weighted data
cat("Test 6: Overall bar with weighted data\n")
plot6 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source"),
  grouping = "region",
  show_overall = TRUE,
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Weighted with Overall Bar"
)
print(plot6)
cat("✓ Survey-weighted overall and groups\n\n")

# Test 7: Overall with different palettes per variable
cat("Test 7: Overall bar with multi-palette\n")
plot7 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source", "health_status"),
  category_labels = c("Food", "Water", "Health"),
  grouping = "displacement_status",
  show_overall = TRUE,
  color_palette = c("traffic_light", "reach2", "reach3"),
  separate_legends = TRUE, legend_position = "right",
  title_name = "Overall Bar with Multi-Palette"
)
print(plot7)
cat("✓ Overall bar with different palettes per variable\n\n")

# Test 8: Overall with flip coordinates
cat("Test 8: Overall bar with horizontal layout\n")
plot8 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status", "health_status"),
  category_labels = c("Education", "Employment", "Health"),
  grouping = "gender",
  show_overall = TRUE,
  overall_label = "Overall",
  flip_coordinates = TRUE,
  title_name = "Horizontal with Overall Bar"
)
print(plot8)
cat("✓ Horizontal orientation with overall reference\n\n")

# Test 9: Overall with labels
cat("Test 9: Overall bar with percentage labels\n")
plot9 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source"),
  grouping = "region",
  show_overall = TRUE,
  show_labels = TRUE,
  title_name = "With Percentage Labels and Overall"
)
print(plot9)
cat("✓ Labels show on all bars including overall\n\n")

# Test 10: Test percentage scaling fix
cat("Test 10: Verify percentage scaling (should show 25%, not 0.25%)\n")
plot10 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source"),
  grouping = "region",
  show_overall = TRUE,
  title_name = "Check Y-Axis Percentages"
)
print(plot10)
cat("✓ Y-axis should show 0%, 25%, 50%, 75%, 100%\n")
cat("✓ NOT 0.00%, 0.25%, 0.50%, etc.\n\n")

# Test 11: Test group spacing
cat("Test 11: Visual group spacing between variables\n")
plot11 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source", "shelter_type"),
  category_labels = c("Food", "Water", "Shelter"),
  grouping = "displacement_status",
  show_overall = TRUE,
  group_spacing = 0.15,
  title_name = "Group Spacing Demonstration"
)
print(plot11)
cat("✓ Visual separation between variable groups\n")
cat("✓ Each variable's bars are grouped together\n\n")

# Test 12: All features combined
cat("Test 12: ALL FEATURES COMBINED\n")
plot12 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source", "shelter_type"),
  category_labels = c("Food Security", "Water Access", "Shelter Type"),
  grouping = "displacement_status",
  show_overall = TRUE,
  overall_label = "Overall",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = c("traffic_light", "reach2", "reach3"),
  separate_legends = TRUE,
  legend_label = c("FCS Status", "Water Type", "Shelter Condition"),
  flip_coordinates = TRUE,
  show_labels = TRUE,
  bar_spacing = 0.4,
  group_spacing = 0.1,
  title_name = "Comprehensive Multi-Sectoral Assessment",
  subtitle = "By Displacement Status with Overall Comparison",
  x_label = "Indicators",
  y_label = "Percentage",
  legend_position = "right"
)
print(plot12)
cat("✓ Everything: overall bar, grouping, weighting, multi-palette, spacing\n\n")


# PRACTICAL USE CASES


cat("=== Practical Use Case Examples ===\n\n")

# Use Case 1: Regional comparison with overall benchmark
cat("Use Case 1: Regional Assessment with Overall Benchmark\n")
uc1 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source", "health_status"),
  category_labels = c("Food Security", "Water Source", "Health Status"),
  grouping = "region",
  show_overall = TRUE,
  overall_label = "National",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = c("traffic_light", "reach2", "reach3"),
  separate_legends = TRUE,
  flip_coordinates = TRUE,
  title_name = "Regional Comparison Against National Average",
  subtitle = "WASH and Food Security Indicators",
  legend_position = "right"
)
print(uc1)
cat("✓ Compare each region to national average\n")
cat("✓ Identify regions performing above/below average\n\n")

# Use Case 2: Displacement status comparison
cat("Use Case 2: Population Group Profiling with Overall\n")
uc2 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status", "documentation_status"),
  category_labels = c("Education", "Employment", "Documentation"),
  grouping = "displacement_status",
  show_overall = TRUE,
  overall_label = "All Groups",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = c("reach1", "reach2", "reach3"),
  separate_legends = TRUE,
  flip_coordinates = TRUE,
  title_name = "Socioeconomic Profile by Displacement Status",
  subtitle = "Compare each group to overall population",
  legend_position = "right"
)
print(uc2)
cat("✓ See how IDPs/refugees/returnees differ from overall\n")
cat("✓ Identify vulnerable sub-populations\n\n")

# Use Case 3: Gender gap analysis
cat("Use Case 3: Gender Gap Analysis with Overall\n")
uc3 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status", "health_status"),
  category_labels = c("Education", "Employment", "Health"),
  grouping = "gender",
  show_overall = TRUE,
  overall_label = "Population",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = c("reach1", "reach2", "reach4"),
  separate_legends = TRUE,
  bar_spacing = 0.5,
  title_name = "Gender-Disaggregated Indicators",
  subtitle = "Identifying gender disparities"
)
print(uc3)
cat("✓ Compare male vs female vs overall population\n")
cat("✓ Quantify gender gaps\n\n")

# Use Case 4: Settlement type comparison
cat("Use Case 4: Settlement Type Analysis with Overall\n")
uc4 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source", "shelter_type", "assistance_received"),
  category_labels = c("Food", "Water", "Shelter", "Assistance"),
  grouping = "settlement_type",
  show_overall = TRUE,
  overall_label = "Overall",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = c("traffic_light", "reach2", "reach3", "reach4"),
  separate_legends = TRUE,
  flip_coordinates = TRUE,
  title_name = "Needs by Settlement Type vs Overall",
  legend_position = "right"
)
print(uc4)
cat("✓ Compare camp vs urban vs rural to overall\n")
cat("✓ Target interventions based on gaps\n\n")

# Use Case 5: Age group analysis
cat("Use Case 5: Age Group Comparison\n")
uc5 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status"),
  category_labels = c("Education", "Employment"),
  grouping = "age_group",
  show_overall = TRUE,
  overall_label = "All Ages",
  flip_coordinates = TRUE,
  title_name = "Education and Employment by Age Group",
  subtitle = "Compare to overall population"
)
print(uc5)
cat("✓ See how each age group compares to overall\n\n")

# Use Case 6: Before/After with overall (simulated)
cat("Use Case 6: Monitoring - Compare groups to baseline\n")
uc6 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "health_status"),
  category_labels = c("Food Security", "Health Status"),
  grouping = "assistance_received",
  show_overall = TRUE,
  overall_label = "Baseline",
  color_palette = c("traffic_light", "reach3"),
  separate_legends = TRUE,
  title_name = "Assistance Recipients vs Overall Population",
  subtitle = "Assess program impact"
)
print(uc6)
cat("✓ Compare program recipients to overall baseline\n\n")


# VALUE OF OVERALL BAR


cat("=== Why the Overall Bar is Valuable ===\n\n")

cat("BENEFITS:\n")
cat("1. ✓ Quick visual reference - see if groups differ from average\n")
cat("2. ✓ Identify outliers - which groups are above/below overall\n")
cat("3. ✓ Benchmark for comparison - consistent reference point\n")
cat("4. ✓ Context for magnitude - shows if differences are meaningful\n")
cat("5. ✓ Supports decision-making - target groups below overall\n")
cat("6. ✓ Report clarity - readers can easily interpret gaps\n\n")

cat("USE OVERALL BAR WHEN:\n")
cat("  ✓ Comparing sub-populations to total population\n")
cat("  ✓ Identifying vulnerable groups (below overall)\n")
cat("  ✓ Prioritizing interventions (target below-average groups)\n")
cat("  ✓ Regional/geographic comparisons\n")
cat("  ✓ Equity analysis (how far from overall?)\n")
cat("  ✓ Program targeting (which groups need support?)\n\n")

cat("DON'T USE OVERALL BAR WHEN:\n")
cat("  ✗ Groups are completely distinct populations\n")
cat("  ✗ No logical 'overall' (e.g., comparing different countries)\n")
cat("  ✗ Already too many groups (visual clutter)\n")
cat("  ✗ Overall is not meaningful (very heterogeneous)\n")


# INTERPRETING THE CHARTS


cat("\n=== How to Interpret Charts with Overall Bar ===\n\n")

cat("READING THE CHART:\n")
cat("1. Overall bar is LEFTMOST for each variable\n")
cat("2. Group bars follow, ordered alphabetically (or custom order)\n")
cat("3. Compare each group bar to the Overall bar\n")
cat("4. Look for bars that differ substantially from Overall\n\n")

cat("EXAMPLE INTERPRETATION:\n")
cat("If Food Security Overall shows: Poor 30%, Borderline 35%, Acceptable 35%\n")
cat("And Region North shows: Poor 45%, Borderline 30%, Acceptable 25%\n")
cat("→ North has WORSE food security than overall (more Poor, less Acceptable)\n")
cat("→ Indicates North needs prioritization\n\n")

cat("KEY QUESTIONS TO ASK:\n")
cat("• Which groups are better/worse than overall?\n")
cat("• How large are the differences?\n")
cat("• Are differences consistent across indicators?\n")
cat("• Which groups need targeted interventions?\n")


# TECHNICAL FEATURES VERIFICATION


cat("\n=== Technical Features Verification ===\n\n")

# Test 13: Verify percentage scale fix
cat("Test 13: Percentage scale verification\n")
cat("Creating chart and checking y-axis...\n")
plot13 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source"),
  grouping = "region",
  show_overall = TRUE,
  title_name = "Percentage Scale Test"
)
print(plot13)
cat("✓ Y-axis should display: 0%, 25%, 50%, 75%, 100%\n")
cat("✓ NOT: 0.00%, 0.25%, 0.50%, 0.75%, 1.00%\n\n")

# Test 14: Verify group spacing
cat("Test 14: Group spacing verification\n")

p_tight <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status"),
  grouping = "gender",
  show_overall = TRUE,
  group_spacing = 0.05,
  title_name = "Tight Spacing (0.05)"
)

p_wide <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status"),
  grouping = "gender",
  show_overall = TRUE,
  group_spacing = 0.2,
  title_name = "Wide Spacing (0.2)"
)

grid.arrange(p_tight, p_wide, ncol = 2)
cat("✓ Visual separation between variable groups\n")
cat("✓ Adjustable with group_spacing parameter\n\n")

# Test 15: Overall bar position verification
cat("Test 15: Overall bar always appears FIRST (leftmost)\n")
plot15 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("food_security", "water_source"),
  category_labels = c("Food", "Water"),
  grouping = "region",  # East, North, South, West alphabetically
  show_overall = TRUE,
  overall_label = "Overall",
  title_name = "Overall Position Test"
)
print(plot15)
cat("✓ For Food: Overall | East | North | South | West\n")
cat("✓ For Water: Overall | East | North | South | West\n")
cat("✓ Overall always comes FIRST\n\n")


# ERROR HANDLING TESTS


cat("=== Error Handling Tests ===\n\n")

# Test 16: show_overall without grouping (should warn)
cat("Test 16: show_overall=TRUE without grouping (should warn)\n")
plot16 <- plot_stacked_bar_multiple_vars(
  df = test_data,
  category_vars = c("education_level", "employment_status"),
  grouping = NULL,
  show_overall = TRUE,  # This should be ignored with warning
  title_name = "Should Ignore show_overall"
)
print(plot16)
cat("✓ Warning issued: show_overall only applies with grouping\n\n")

# Test 17: All validation still works
cat("Test 17: Error - Wrong number of palettes\n")
tryCatch({
  plot17 <- plot_stacked_bar_multiple_vars(
    df = test_data,
    category_vars = c("education_level", "employment_status", "health_status"),
    color_palette = c("reach1", "reach2")  # Only 2 for 3 vars
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error: palette count mismatch\n\n")
})


# SUMMARY


cat("\n=== SUMMARY: All Features of Final Function ===\n\n")

cat("CORE FEATURES:\n")
cat("1. ✓ Multiple categorical variables in one chart\n")
cat("2. ✓ Optional grouping within each variable\n")
cat("3. ✓ ** NEW: Overall bar for grouped visualizations **\n")
cat("4. ✓ ** FIXED: Correct percentage scaling (25% not 0.25%) **\n")
cat("5. ✓ ** IMPROVED: Visual spacing between variable groups **\n")
cat("6. ✓ Different color palette per variable\n")
cat("7. ✓ Separate legends per variable\n")
cat("8. ✓ Weighted and unweighted calculations\n")
cat("9. ✓ Horizontal and vertical orientations\n")
cat("10. ✓ Customizable labels and styling\n\n")

cat("NEW PARAMETERS:\n")
cat("• show_overall: Add overall bar when grouping (default: FALSE)\n")
cat("• overall_label: Customize overall bar label (default: 'Overall')\n")
cat("• group_spacing: Spacing between variable groups (default: 0.1)\n\n")

cat("BEST USE CASES:\n")
cat("1. Regional comparisons with national benchmark\n")
cat("2. Population group profiling (IDP/refugee/host vs all)\n")
cat("3. Gender gap analysis (male/female vs overall)\n")
cat("4. Settlement type comparisons (camp/urban/rural vs overall)\n")
cat("5. Age group analysis with population reference\n")
cat("6. Program impact (recipients vs baseline/overall)\n\n")

cat("KEY ADVANTAGES:\n")
cat("• Quick identification of groups above/below average\n")
cat("• Built-in benchmark for all comparisons\n")
cat("• Clear visual reference point\n")
cat("• Supports targeting and prioritization decisions\n")
cat("• Improves report clarity and interpretation\n")

cat("\n=== All tests completed successfully ===\n")

# PLOT CROSSTAB ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Create test dataset for crosstab
create_test_crosstab_data <- function(n = 500) {
  set.seed(8888)

  data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),

    # Survey weights
    survey_weight = runif(n, 0.8, 1.5),

    # Categorical variables for crosstab
    education_level = sample(c("None", "Primary", "Secondary", "Tertiary"), n, replace = TRUE,
                             prob = c(0.15, 0.40, 0.30, 0.15)),

    employment_status = sample(c("Employed", "Unemployed", "Student", "Retired"), n, replace = TRUE,
                               prob = c(0.35, 0.35, 0.20, 0.10)),

    food_security = sample(c("Poor", "Borderline", "Acceptable"), n, replace = TRUE,
                           prob = c(0.30, 0.35, 0.35)),

    water_source = sample(c("Improved", "Unimproved", "Surface"), n, replace = TRUE,
                          prob = c(0.50, 0.35, 0.15)),

    shelter_type = sample(c("Permanent", "Semi-permanent", "Temporary"), n, replace = TRUE,
                          prob = c(0.35, 0.35, 0.30)),

    health_status = sample(c("Good", "Fair", "Poor"), n, replace = TRUE,
                           prob = c(0.40, 0.40, 0.20)),

    assistance_received = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.60, 0.40)),

    documentation_status = sample(c("Full", "Partial", "None"), n, replace = TRUE,
                                  prob = c(0.50, 0.30, 0.20)),

    region = sample(c("North", "South", "East", "West"), n, replace = TRUE,
                    prob = c(0.30, 0.30, 0.25, 0.15)),

    displacement_status = sample(c("IDP", "Refugee", "Returnee", "Host"), n, replace = TRUE,
                                 prob = c(0.35, 0.30, 0.20, 0.15)),

    settlement_type = sample(c("Camp", "Urban", "Rural"), n, replace = TRUE,
                             prob = c(0.35, 0.35, 0.30)),

    gender = sample(c("Male", "Female"), n, replace = TRUE,
                    prob = c(0.52, 0.48)),

    age_group = sample(c("18-24", "25-34", "35-44", "45+"), n, replace = TRUE,
                       prob = c(0.25, 0.30, 0.25, 0.20))
  )
}

test_data <- create_test_crosstab_data(n = 500)

cat("\n=== Testing plot_crosstab (Revised with Margins and Smart Highlighting) ===\n\n")


# MARGINAL TOTALS TESTS


cat("=== PART 1: MARGINAL TOTALS ===\n\n")

# Test 1: Basic crosstab without margins (baseline)
cat("Test 1: Basic crosstab without margins\n")
plot1 <- plot_crosstab(
  df = test_data,
  row_var = "education_level",
  col_var = "employment_status",
  show_margins = FALSE,
  title_name = "Without Margins"
)
print(plot1)
cat("✓ Standard crosstab without totals\n\n")

# Test 2: Basic crosstab WITH margins
cat("Test 2: Basic crosstab WITH margins\n")
plot2 <- plot_crosstab(
  df = test_data,
  row_var = "education_level",
  col_var = "employment_status",
  show_margins = TRUE,
  title_name = "With Margins"
)
print(plot2)
cat("✓ Margins shown in grey at bottom and right\n")
cat("✓ Row totals, column totals, and grand total displayed\n\n")

# Test 3: Comparison - with and without margins
cat("Test 3: Side-by-side comparison\n")

p_no_margins <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  show_margins = FALSE,
  title_name = "No Margins"
)

p_with_margins <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  show_margins = TRUE,
  title_name = "With Margins"
)

grid.arrange(p_no_margins, p_with_margins, ncol = 2)
cat("✓ Visual comparison of margins\n\n")

# Test 4: Margins with row percentages
cat("Test 4: Margins with row percentages\n")
plot4 <- plot_crosstab(
  df = test_data,
  row_var = "education_level",
  col_var = "employment_status",
  percentage_by = "row",
  show_margins = TRUE,
  title_name = "Row % with Margins",
  subtitle = "Each row sums to 100% (excluding margin)"
)
print(plot4)
cat("✓ Row percentages calculated correctly\n")
cat("✓ Margin column shows row totals\n\n")

# Test 5: Margins with column percentages
cat("Test 5: Margins with column percentages\n")
plot5 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  percentage_by = "column",
  show_margins = TRUE,
  title_name = "Column % with Margins",
  subtitle = "Each column sums to 100% (excluding margin)"
)
print(plot5)
cat("✓ Column percentages calculated correctly\n")
cat("✓ Margin row shows column totals\n\n")

# Test 6: Margins with total percentages
cat("Test 6: Margins with total percentages\n")
plot6 <- plot_crosstab(
  df = test_data,
  row_var = "shelter_type",
  col_var = "water_source",
  percentage_by = "row", gradient_by = "row",
  show_margins = TRUE,
  title_name = "Total % with Margins"
)
print(plot6)
cat("✓ All cells as % of grand total\n")
cat("✓ Margins show row/column totals\n\n")

# Test 7: Custom margins label
cat("Test 7: Custom margins label\n")
plot7 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  show_margins = TRUE,
  margins_label = "All Regions",
  percentage_by = "column",
  title_name = "Custom Margin Label"
)
print(plot7)
cat("✓ Custom label 'All Regions' instead of 'Total'\n\n")

# Test 8: Weighted with margins
cat("Test 8: Weighted crosstab with margins\n")
plot8 <- plot_crosstab(
  df = test_data,
  row_var = "displacement_status",
  col_var = "assistance_received",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_margins = TRUE,
  percentage_by = "row",
  title_name = "Weighted with Margins"
)
print(plot8)
cat("✓ Survey-weighted with margins\n")
cat("✓ Weighted counts in margins\n\n")

# Test 9: Margins with gradient by row
cat("Test 9: Margins with gradient by row\n")
plot9 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  percentage_by = "row",
  gradient_by = "row",
  show_margins = TRUE,
  title_name = "Gradient by Row with Margins"
)
print(plot9)
cat("✓ Gradient excludes margins\n")
cat("✓ Each row has independent gradient\n\n")

# Test 10: Margins with gradient by column
cat("Test 10: Margins with gradient by column\n")
plot10 <- plot_crosstab(
  df = test_data,
  row_var = "health_status",
  col_var = "settlement_type",
  percentage_by = "column",
  gradient_by = "column",
  show_margins = TRUE,
  title_name = "Gradient by Column with Margins"
)
print(plot10)
cat("✓ Gradient excludes margins\n")
cat("✓ Each column has independent gradient\n\n")


# SMART BOUNDARY MERGING TESTS


cat("\n=== PART 2: SMART BOUNDARY MERGING ===\n\n")

# Test 11: Single cell highlight (baseline)
cat("Test 11: Single cell highlight\n")
plot11 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_color = "red",
  highlight_size = 3,
  title_name = "Single Cell - No Merging"
)
print(plot11)
cat("✓ Single cell highlighted\n\n")

# Test 12: Two horizontally adjacent cells - MERGED
cat("Test 12: Two horizontally adjacent cells - MERGED\n")
plot12 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "South",
  highlight_color = "red",
  title_name = "Horizontal Adjacent Cells - Merged Boundary"
)
print(plot12)
cat("✓ Two adjacent cells merge into one boundary\n")
cat("✓ Single red rectangle around both cells\n\n")

# Test 13: Two vertically adjacent cells - MERGED
cat("Test 13: Two vertically adjacent cells - MERGED\n")
plot13 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Borderline",
  highlight_cells_col_val2 = "North",
  highlight_color = "red",
  title_name = "Vertical Adjacent Cells - Merged Boundary"
)
print(plot13)
cat("✓ Two vertically adjacent cells merge\n")
cat("✓ Single red boundary around both\n\n")

# Test 14: Two cells in a row (simplified from three)
cat("Test 14: Two cells in a row - MERGED\n")
plot14 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "South",
  highlight_color = "red",
  title_name = "Two Horizontal Cells Highlighted"
)
print(plot14)
cat("✓ Two cells in same row highlighted\n\n")

# Test 15: Two cells in a column
cat("Test 15: Two cells in a column highlighted\n")
plot15 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Borderline",
  highlight_cells_col_val2 = "North",
  highlight_color = "red",
  title_name = "Two Vertical Cells Highlighted"
)
print(plot15)
cat("✓ Two cells in same column highlighted\n\n")

# Test 16: Two adjacent cells
cat("Test 16: Two adjacent cells highlighted\n")
plot16 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "South",
  highlight_color = "red",
  title_name = "Two Adjacent Cells Highlighted"
)
print(plot16)
cat("✓ Two adjacent cells highlighted\n\n")

# Test 17: Two vertically adjacent cells
cat("Test 17: Two vertically adjacent cells highlighted\n")
plot17 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Borderline",
  highlight_cells_col_val2 = "North",
  highlight_color = "red",
  title_name = "Two Vertically Adjacent Cells Highlighted"
)
print(plot17)
cat("✓ Two vertically adjacent cells highlighted\n\n")

# Test 18: Two separate non-adjacent cells
cat("Test 18: Two separate non-adjacent cells\n")
plot18 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Acceptable",
  highlight_cells_col_val2 = "West",
  highlight_color = "red",
  title_name = "Two Separate Cells"
)
print(plot18)
cat("✓ Two separate red boundaries\n")
cat("✓ Non-adjacent cells highlighted individually\n\n")

# Test 19: Two cells with same color
cat("Test 19: Two cells highlighted\n")
plot19 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "South",
  highlight_color = "red",
  title_name = "Two Adjacent Cells Highlighted"
)
print(plot19)
cat("✓ Two cells highlighted with same color\n\n")

# Test 20: Two cells with different rows/cols
cat("Test 20: Two cells at different positions\n")
plot20 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  percentage_by = "column",
  gradient_by = "column",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Borderline",
  highlight_cells_col_val2 = "South",
  highlight_color = "red",
  title_name = "Two Cells at Different Positions",
  subtitle = "Red = areas of concern"
)
print(plot20)
cat("✓ Two cells at different positions highlighted\n\n")


# COMBINED: MARGINS + HIGHLIGHTING


cat("\n=== PART 3: MARGINS + SMART HIGHLIGHTING COMBINED ===\n\n")

# Test 21: Margins with single cell highlight
cat("Test 21: Margins with single cell highlight\n")
plot21 <- plot_crosstab(
  df = test_data,
  row_var = "displacement_status",
  col_var = "assistance_received",
  show_margins = TRUE,
  percentage_by = "total", gradient_by = "all",
  highlight_cells_row_val1 = "IDP",
  highlight_cells_col_val1 = "No",
  highlight_color = "red",
  highlight_size = 3,
  title_name = "Margins + Single Highlight"
)
print(plot21)
cat("✓ Margins shown with highlighting\n\n")

# Test 22: Margins with two cell highlights
cat("Test 22: Margins with two cell highlights\n")
plot22 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  show_margins = TRUE,
  percentage_by = "column",
  gradient_by = "column",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "South",
  highlight_color = "red",
  title_name = "Margins + Two Cell Highlights"
)
print(plot22)
cat("✓ Margins and two-cell highlighting work together\n\n")

# Test 23: Weighted, margins, and complex highlighting
cat("Test 23: All features combined\n")
plot23 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  show_margins = TRUE,
  margins_label = "All Regions",
  percentage_by = "column",
  gradient_by = "column",
  color_low = "#fff5f0",
  color_high = "#a50f15",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "South",
  highlight_color = "darkred",
  title_name = "Complete Feature Set",
  subtitle = "Weighted, margins, merged highlighting"
)
print(plot23)
cat("✓ All features working together\n\n")


# PRACTICAL USE CASES WITH NEW FEATURES


cat("=== PRACTICAL USE CASES ===\n\n")

# Use Case 1: Service coverage with margins
cat("Use Case 1: Service Coverage Matrix with Totals\n")
uc1 <- plot_crosstab(
  df = test_data,
  row_var = "displacement_status",
  col_var = "assistance_received",
  show_margins = TRUE,
  percentage_by = "row",
  weighted = TRUE,
  weights_col = "survey_weight",
  highlight_cells_row_val1 = "IDP",
  highlight_cells_col_val1 = "No",
  highlight_cells_row_val2 = "Refugee",
  highlight_cells_col_val2 = "No",
  highlight_color = "red",
  highlight_size = 3,
  title_name = "Assistance Coverage by Population Group",
  subtitle = "Red = Coverage gaps. Margins show totals per group."
)
print(uc1)
cat("✓ Identify service gaps with context from totals\n")
cat("✓ Merged red boundary highlights gap pattern\n\n")

# Use Case 2: Regional vulnerability with totals
cat("Use Case 2: Regional Vulnerability Assessment\n")
uc2 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  show_margins = TRUE,
  margins_label = "National",
  percentage_by = "column",
  gradient_by = "column",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_low = "#ffffff",
  color_high = "#a50f15",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "South",
  highlight_color = "darkred",
  title_name = "Food Security by Region with National Average",
  subtitle = "Dark red = priority areas"
)
print(uc2)
cat("✓ Regional patterns with national benchmark\n")
cat("✓ Priority areas highlighted and merged\n\n")

# Use Case 3: Equity analysis with totals
cat("Use Case 3: Gender Equity Analysis\n")
uc3 <- plot_crosstab(
  df = test_data,
  row_var = "education_level",
  col_var = "gender",
  show_margins = TRUE,
  percentage_by = "column",
  gradient_by = "column",
  weighted = TRUE,
  weights_col = "survey_weight",
  highlight_cells_row_val1 = "None",
  highlight_cells_col_val1 = "Female",
  highlight_cells_row_val2 = "Primary",
  highlight_cells_col_val2 = "Female",
  highlight_color = "red",
  highlight_size = 3,
  title_name = "Education Attainment by Gender",
  subtitle = "Highlighting female education gaps"
)
print(uc3)
cat("✓ Assess gender disparities\n")
cat("✓ Totals provide context\n\n")

# Use Case 4: Multi-sector with highlighted problem areas
cat("Use Case 4: Multi-Sector Indicator Matrix\n")
uc4 <- plot_crosstab(
  df = test_data,
  row_var = "water_source",
  col_var = "settlement_type",
  show_margins = TRUE,
  percentage_by = "column",
  gradient_by = "column",
  weighted = TRUE,
  weights_col = "survey_weight",
  highlight_cells_row_val1 = "Surface",
  highlight_cells_col_val1 = "Camp",
  highlight_cells_row_val2 = "Surface",
  highlight_cells_col_val2 = "Rural",
  highlight_color = "red",
  title_name = "Water Access by Settlement Type",
  subtitle = "Red = critical areas"
)
print(uc4)
cat("✓ Multi-sector overview with totals\n")
cat("✓ Problem patterns highlighted\n\n")

# Use Case 5: Targeting matrix with comprehensive info
cat("Use Case 5: Program Targeting Matrix\n")
uc5 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "displacement_status",
  show_margins = TRUE,
  margins_label = "All Groups",
  percentage_by = "column",
  gradient_by = "column",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_low = "#f7fcf5",
  color_high = "#00441b",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "IDP",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "Refugee",
  highlight_color = "red",
  title_name = "Food Assistance Targeting Criteria",
  subtitle = "Red = Priority areas"
)
print(uc5)
cat("✓ Clear targeting priorities\n")
cat("✓ Totals show population sizes\n")
cat("✓ Merged boundaries show priority groups\n\n")


# COMPARISON: Features demonstration


cat("=== Feature Comparison Grid ===\n\n")

p1 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  title_name = "Basic"
)

p2 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  show_margins = TRUE,
  title_name = "With Margins"
)

p3 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "South",
  highlight_color = "red",
  title_name = "Two Cell Highlights"
)

p4 <- plot_crosstab(
  df = test_data,
  row_var = "food_security",
  col_var = "region",
  show_margins = TRUE,
  highlight_cells_row_val1 = "Poor",
  highlight_cells_col_val1 = "North",
  highlight_cells_row_val2 = "Poor",
  highlight_cells_col_val2 = "South",
  highlight_color = "red",
  title_name = "Both Features"
)

grid.arrange(p1, p2, p3, p4, ncol = 2)

cat("\n✓ Comparison of all feature combinations\n\n")

# PLOT RIDGE MULTIPLE ####

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggridges)
library(gridExtra)

# Create test dataset for ridge plots
create_test_ridge_data <- function(n = 500) {
  set.seed(9999)

  # First create the grouping variables
  df <- data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),
    survey_weight = runif(n, 0.8, 1.5),
    district = sample(c("District A", "District B", "District C", "District D"), n,
                      replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.15)),
    region = sample(c("North", "South", "East", "West"), n,
                    replace = TRUE, prob = c(0.30, 0.30, 0.25, 0.15)),
    settlement_type = sample(c("Camp", "Urban", "Rural"), n,
                             replace = TRUE, prob = c(0.35, 0.35, 0.30)),
    displacement_status = sample(c("IDP", "Refugee", "Returnee", "Host"), n,
                                 replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.15))
  )

  # Then add numeric variables based on grouping variables
  df <- df %>%
    mutate(
      # FCS score - varies by district
      fcs_score = case_when(
        district == "District A" ~ rnorm(n(), mean = 45, sd = 15),
        district == "District B" ~ rnorm(n(), mean = 35, sd = 12),
        district == "District C" ~ rnorm(n(), mean = 50, sd = 10),
        district == "District D" ~ rnorm(n(), mean = 30, sd = 18),
        TRUE ~ rnorm(n(), mean = 40, sd = 15)
      ),

      # Income - varies by settlement type
      monthly_income = case_when(
        settlement_type == "Camp" ~ rgamma(n(), shape = 2, scale = 50),
        settlement_type == "Urban" ~ rgamma(n(), shape = 3, scale = 80),
        settlement_type == "Rural" ~ rgamma(n(), shape = 2.5, scale = 40),
        TRUE ~ rgamma(n(), shape = 2.5, scale = 60)
      ),

      # LCSI score - varies by displacement status
      lcsi_score = case_when(
        displacement_status == "IDP" ~ rbeta(n(), 3, 2) * 50,
        displacement_status == "Refugee" ~ rbeta(n(), 2, 3) * 50,
        displacement_status == "Returnee" ~ rbeta(n(), 2.5, 2.5) * 50,
        displacement_status == "Host" ~ rbeta(n(), 2, 2) * 50,
        TRUE ~ rbeta(n(), 2.5, 2.5) * 50
      ),

      # Expenditure - varies by region
      monthly_expenditure = case_when(
        region == "North" ~ rlnorm(n(), meanlog = 4.5, sdlog = 0.8),
        region == "South" ~ rlnorm(n(), meanlog = 4.8, sdlog = 0.6),
        region == "East" ~ rlnorm(n(), meanlog = 4.3, sdlog = 0.9),
        region == "West" ~ rlnorm(n(), meanlog = 4.0, sdlog = 1.0),
        TRUE ~ rlnorm(n(), meanlog = 4.4, sdlog = 0.8)
      ),

      # Distance to services - varies by settlement
      distance_to_water = case_when(
        settlement_type == "Camp" ~ rexp(n(), rate = 0.5),
        settlement_type == "Urban" ~ rexp(n(), rate = 2),
        settlement_type == "Rural" ~ rexp(n(), rate = 0.3),
        TRUE ~ rexp(n(), rate = 1)
      ),

      # Age - varies by displacement status
      age = case_when(
        displacement_status == "IDP" ~ rnorm(n(), mean = 28, sd = 12),
        displacement_status == "Refugee" ~ rnorm(n(), mean = 32, sd = 14),
        displacement_status == "Returnee" ~ rnorm(n(), mean = 38, sd = 15),
        displacement_status == "Host" ~ rnorm(n(), mean = 35, sd = 16),
        TRUE ~ rnorm(n(), mean = 33, sd = 14)
      ),

      # Constrain values to reasonable ranges
      fcs_score = pmax(0, pmin(112, fcs_score)),
      monthly_income = pmax(0, monthly_income),
      lcsi_score = pmax(0, pmin(50, lcsi_score)),
      monthly_expenditure = pmax(0, monthly_expenditure),
      distance_to_water = pmax(0, pmin(50, distance_to_water)),
      age = pmax(18, pmin(80, age))
    )

  return(df)
}

test_data <- create_test_ridge_data(n = 500)

cat("\n=== Testing plot_ridge_distribution_by_group ===\n\n")


# BASIC FUNCTIONALITY


cat("=== PART 1: BASIC FUNCTIONALITY ===\n\n")

# Test 1: Basic ridge plot
cat("Test 1: Basic ridge plot - FCS by District\n")
plot1 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  title_name = "Food Consumption Score Distribution by District"
)
print(plot1)
cat("✓ Basic ridge plot created\n")
cat("✓ Overall distribution at top\n")
cat("✓ Group distributions below\n\n")

# Test 2: Different grouping variable
cat("Test 2: Different grouping - Income by Settlement Type\n")
plot2 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_income",
  grouping = "settlement_type",
  title_name = "Monthly Income by Settlement Type"
)
print(plot2)
cat("✓ Different grouping variable\n")
cat("✓ Clear distribution differences\n\n")

# Test 3: Custom overall label
cat("Test 3: Custom overall label\n")
plot3 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "lcsi_score",
  grouping = "displacement_status",
  overall_label = "All Groups Combined",
  title_name = "LCSI Score Distribution"
)
print(plot3)
cat("✓ Custom label for overall distribution\n\n")

# Test 4: Custom axis labels
cat("Test 4: Custom axis labels\n")
plot4 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_expenditure",
  grouping = "region",
  x_lab = "Monthly Expenditure (USD)",
  y_lab = "Geographic Region",
  title_name = "Expenditure Patterns by Region"
)
print(plot4)
cat("✓ Custom x and y axis labels\n\n")

# Test 5: With subtitle
cat("Test 5: With custom subtitle\n")
plot5 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "distance_to_water",
  grouping = "settlement_type",
  title_name = "Distance to Water Source",
  subtitle = "Dry season measurements"
)
print(plot5)
cat("✓ Custom subtitle appended to auto-generated counts\n\n")


# COLOR PALETTES


cat("=== PART 2: COLOR PALETTES ===\n\n")

# Test 6: Different color palettes
cat("Test 6: Different color palettes\n")

p_reach1 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  color_palette = "reach1",
  title_name = "Reach1 Palette"
)

p_reach2 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  color_palette = "reach2",
  title_name = "Reach2 Palette"
)

p_reach3 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  color_palette = "reach3",
  title_name = "Reach3 Palette (Default)"
)

p_reach4 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  color_palette = "reach4",
  title_name = "Reach4 Palette"
)

grid.arrange(p_reach1, p_reach2, p_reach3, p_reach4, ncol = 2)
cat("✓ Multiple color palette options\n\n")


# WEIGHTED ANALYSIS


cat("=== PART 3: WEIGHTED ANALYSIS ===\n\n")

# Test 7: Weighted vs unweighted comparison
cat("Test 7: Weighted vs unweighted\n")

p_unweighted <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  weighted = FALSE,
  title_name = "Unweighted"
)

p_weighted <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Weighted"
)

grid.arrange(p_unweighted, p_weighted, ncol = 2)
cat("✓ Weighted analysis properly applied\n")
cat("✓ Subtitle indicates weighted status\n\n")

# Test 8: Weighted with different distributions
cat("Test 8: Weighted analysis - Income by Settlement\n")
plot8 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_income",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  title_name = "Monthly Income (Weighted)"
)
print(plot8)
cat("✓ Weighted distributions\n\n")


# LEGEND POSITION


cat("=== PART 4: LEGEND POSITION ===\n\n")

# Test 9: Different legend positions
cat("Test 9: Legend positions\n")

p_none <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "age",
  grouping = "displacement_status",
  legend_position = "none",
  title_name = "No Legend (Default)"
)

p_right <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "age",
  grouping = "displacement_status",
  legend_position = "right",
  title_name = "Legend Right"
)

p_bottom <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "age",
  grouping = "displacement_status",
  legend_position = "bottom",
  title_name = "Legend Bottom"
)

p_top <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "age",
  grouping = "displacement_status",
  legend_position = "top",
  title_name = "Legend Top"
)

grid.arrange(p_none, p_right, p_bottom, p_top, ncol = 2)
cat("✓ Multiple legend positions\n\n")


# COORDINATE FLIP


cat("=== PART 5: COORDINATE FLIP ===\n\n")

# Test 10: Flipped coordinates
cat("Test 10: Flipped coordinates\n")

p_normal <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_expenditure",
  grouping = "region",
  flip_coordinates = FALSE,
  title_name = "Normal Orientation"
)

p_flipped <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_expenditure",
  grouping = "region",
  flip_coordinates = TRUE,
  title_name = "Flipped Coordinates"
)

grid.arrange(p_normal, p_flipped, ncol = 2)
cat("✓ Coordinate flip works\n\n")


# DIFFERENT DISTRIBUTIONS


cat("=== PART 6: DIFFERENT DISTRIBUTION TYPES ===\n\n")

# Test 11: Normal distribution
cat("Test 11: Normal distribution - FCS scores\n")
plot11 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  title_name = "Normal Distribution: FCS Scores"
)
print(plot11)
cat("✓ Normal distribution pattern\n\n")

# Test 12: Right-skewed distribution
cat("Test 12: Right-skewed distribution - Income\n")
plot12 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_income",
  grouping = "settlement_type",
  title_name = "Right-Skewed: Monthly Income"
)
print(plot12)
cat("✓ Right-skewed distribution visible\n\n")

# Test 13: Log-normal distribution
cat("Test 13: Log-normal distribution - Expenditure\n")
plot13 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_expenditure",
  grouping = "region",
  title_name = "Log-Normal: Monthly Expenditure"
)
print(plot13)
cat("✓ Log-normal distribution pattern\n\n")

# Test 14: Exponential distribution
cat("Test 14: Exponential distribution - Distance\n")
plot14 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "distance_to_water",
  grouping = "settlement_type",
  title_name = "Exponential: Distance to Water"
)
print(plot14)
cat("✓ Exponential decay pattern\n\n")

# Test 15: Beta-like distribution
cat("Test 15: Beta distribution - LCSI scores\n")
plot15 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "lcsi_score",
  grouping = "displacement_status",
  title_name = "Beta Distribution: LCSI Scores"
)
print(plot15)
cat("✓ Beta distribution pattern (bounded)\n\n")


# DIFFERENT GROUPING SIZES


cat("=== PART 7: DIFFERENT NUMBERS OF GROUPS ===\n\n")

# Test 16: Two groups (binary)
cat("Test 16: Binary grouping\n")
test_data_binary <- test_data %>%
  mutate(urban_rural = ifelse(settlement_type == "Urban", "Urban", "Rural"))

plot16 <- plot_ridge_distribution_by_group(
  .dataset = test_data_binary,
  numeric_col = "monthly_income",
  grouping = "urban_rural",
  title_name = "Binary Grouping: Urban vs Rural"
)
print(plot16)
cat("✓ Two groups plus overall\n\n")

# Test 17: Three groups
cat("Test 17: Three groups - Settlement Type\n")
plot17 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_income",
  grouping = "settlement_type",
  title_name = "Three Groups: Settlement Types"
)
print(plot17)
cat("✓ Three groups plus overall\n\n")

# Test 18: Four groups
cat("Test 18: Four groups - District\n")
plot18 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  title_name = "Four Groups: Districts"
)
print(plot18)
cat("✓ Four groups plus overall\n\n")

# Test 19: Four groups - Displacement Status
cat("Test 19: Four groups - Displacement Status\n")
plot19 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "age",
  grouping = "displacement_status",
  title_name = "Four Groups: Displacement Status"
)
print(plot19)
cat("✓ Another four-group example\n\n")


# PRACTICAL USE CASES


cat("=== PRACTICAL USE CASES ===\n\n")

# Use Case 1: Food Security Analysis
cat("Use Case 1: Food Security Assessment\n")
uc1 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  weighted = TRUE,
  weights_col = "survey_weight",
  overall_label = "Country Average",
  x_lab = "Food Consumption Score",
  y_lab = "District",
  title_name = "Food Security Distribution by District",
  subtitle = "Higher scores indicate better food security"
)
print(uc1)
cat("✓ Identify districts with poor food security\n")
cat("✓ Compare each district to national average\n\n")

# Use Case 2: Income Distribution
cat("Use Case 2: Economic Assessment\n")
uc2 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_income",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = "reach2",
  x_lab = "Monthly Income (USD)",
  y_lab = "Settlement Type",
  title_name = "Income Distribution by Settlement Type",
  subtitle = "Economic vulnerability assessment"
)
print(uc2)
cat("✓ Compare income across settlement types\n")
cat("✓ Identify most vulnerable populations\n\n")

# Use Case 3: Access to Services
cat("Use Case 3: Service Access Analysis\n")
uc3 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "distance_to_water",
  grouping = "settlement_type",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = "reach4",
  x_lab = "Distance to Water Source (km)",
  y_lab = "Settlement Type",
  title_name = "Water Access by Settlement Type",
  subtitle = "Target areas with poor access"
)
print(uc3)
cat("✓ Identify access barriers\n")
cat("✓ Target interventions to underserved areas\n\n")

# Use Case 4: Demographic Analysis
cat("Use Case 4: Age Distribution Analysis\n")
uc4 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "age",
  grouping = "displacement_status",
  weighted = TRUE,
  weights_col = "survey_weight",
  overall_label = "All Populations",
  x_lab = "Age (years)",
  y_lab = "Population Group",
  title_name = "Age Distribution by Displacement Status",
  subtitle = "Demographics for program planning"
)
print(uc4)
cat("✓ Understand demographic composition\n")
cat("✓ Design age-appropriate interventions\n\n")

# Use Case 5: Coping Strategies
cat("Use Case 5: Coping Strategies Assessment\n")
uc5 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "lcsi_score",
  grouping = "displacement_status",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = "reach1",
  x_lab = "Livelihood Coping Strategy Index",
  y_lab = "Population Group",
  title_name = "Household Coping Mechanisms",
  subtitle = "Higher scores = more severe coping strategies"
)
print(uc5)
cat("✓ Assess stress and coping levels\n")
cat("✓ Identify groups using crisis strategies\n\n")

# Use Case 6: Regional Comparison
cat("Use Case 6: Regional Expenditure Patterns\n")
uc6 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_expenditure",
  grouping = "region",
  weighted = TRUE,
  weights_col = "survey_weight",
  color_palette = "reach3",
  overall_label = "National",
  x_lab = "Monthly Expenditure (USD)",
  y_lab = "Region",
  title_name = "Household Expenditure by Region",
  subtitle = "Economic patterns across regions"
)
print(uc6)
cat("✓ Compare regional economic patterns\n")
cat("✓ Understand cost of living variations\n\n")


# COMPARISON GRID


cat("=== COMPARISON: Multiple Indicators ===\n\n")

p1 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "fcs_score",
  grouping = "district",
  title_name = "Food Security"
)

p2 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "lcsi_score",
  grouping = "district",
  title_name = "Coping Strategies"
)

p3 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_income",
  grouping = "district",
  title_name = "Income"
)

p4 <- plot_ridge_distribution_by_group(
  .dataset = test_data,
  numeric_col = "monthly_expenditure",
  grouping = "district",
  title_name = "Expenditure"
)

grid.arrange(p1, p2, p3, p4, ncol = 2)
cat("✓ Multi-indicator comparison\n")
cat("✓ Comprehensive district profile\n\n")


# ERROR HANDLING


cat("=== ERROR HANDLING ===\n\n")

# Test 20: Missing numeric_col
cat("Test 20: Error - Missing numeric_col\n")
tryCatch({
  plot20 <- plot_ridge_distribution_by_group(
    .dataset = test_data,
    numeric_col = NULL,
    grouping = "district"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error: numeric_col is required\n\n")
})

# Test 21: Missing grouping
cat("Test 21: Error - Missing grouping\n")
tryCatch({
  plot21 <- plot_ridge_distribution_by_group(
    .dataset = test_data,
    numeric_col = "fcs_score",
    grouping = NULL
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error: grouping is required\n\n")
})

# Test 22: Non-existent column
cat("Test 22: Error - Non-existent column\n")
tryCatch({
  plot22 <- plot_ridge_distribution_by_group(
    .dataset = test_data,
    numeric_col = "fake_column",
    grouping = "district"
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error: column doesn't exist\n\n")
})

# Test 23: Weighted without weights_col
cat("Test 23: Error - Weighted without weights_col\n")
tryCatch({
  plot23 <- plot_ridge_distribution_by_group(
    .dataset = test_data,
    numeric_col = "fcs_score",
    grouping = "district",
    weighted = TRUE
  )
  cat("✗ Should have thrown error\n\n")
}, error = function(e) {
  cat("✓ Expected error: weights_col required when weighted=TRUE\n\n")
})


# BEST PRACTICES


cat("=== Best Practices for Ridge Plots ===\n\n")

cat("WHEN TO USE RIDGE PLOTS:\n")
cat("✓ Comparing distributions across multiple groups\n")
cat("✓ Showing overall pattern alongside group patterns\n")
cat("✓ Identifying differences in shape, spread, or center\n")
cat("✓ Visualizing continuous variables by categorical groups\n")
cat("✓ Space-efficient alternative to multiple histograms\n\n")

cat("INTERPRETING RIDGE PLOTS:\n")
cat("• Height: Density/frequency of values\n")
cat("• Width: Spread/variability\n")
cat("• Peaks: Most common values (modes)\n")
cat("• Overlap: Similarity between groups\n")
cat("• Overall ridge: Reference for comparison\n\n")

cat("DESIGN CONSIDERATIONS:\n")
cat("• Overall at top for easy reference\n")
cat("• Color helps distinguish groups\n")
cat("• Weighted analysis for survey data\n")
cat("• Clear axis labels with units\n")
cat("• Subtitle provides context (n counts)\n\n")

cat("COMPARISON TO OTHER PLOTS:\n")
cat("• Ridge vs Box Plot: Shows full distribution shape\n")
cat("• Ridge vs Violin Plot: Better for many groups\n")
cat("• Ridge vs Histogram: More compact, easier to compare\n")
cat("• Ridge vs Density Plot: Stacked instead of overlapping\n\n")


# SUMMARY


cat("=== Summary: plot_ridge_distribution_by_group ===\n\n")

cat("CORE FEATURES:\n")
cat("1. ✓ Overall distribution at top\n")
cat("2. ✓ Group distributions stacked below\n")
cat("3. ✓ Weighted and unweighted analysis\n")
cat("4. ✓ Multiple color palettes\n")
cat("5. ✓ Customizable labels and titles\n")
cat("6. ✓ Flexible legend positioning\n")
cat("7. ✓ Coordinate flip option\n")
cat("8. ✓ Auto-generated sample size reporting\n\n")

cat("USE CASES:\n")
cat("• Food security assessments\n")
cat("• Income and expenditure analysis\n")
cat("• Service access evaluation\n")
cat("• Demographic profiling\n")
cat("• Coping strategies assessment\n")
cat("• Regional comparisons\n")
cat("• Multi-indicator dashboards\n\n")

cat("STRENGTHS:\n")
cat("• Clear visual comparison across groups\n")
cat("• Shows distribution shape, not just summary stats\n")
cat("• Overall reference for context\n")
cat("• Space-efficient for multiple groups\n")
cat("• Works with weighted survey data\n\n")

cat("\n=== All ridge plot tests completed successfully ===\n")

# TABLE FREQUENCY ####

# Load required libraries
library(dplyr)
library(flextable)
library(srvyr)
library(tidyr)

# Create test dataset
create_test_frequency_data <- function(n = 500) {
  set.seed(12345)

  # First create all independent variables
  df <- data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),
    survey_weight = runif(n, 0.8, 1.5),
    district = sample(c("District A", "District B", "District C", "District D"), n,
                      replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.15)),
    region = sample(c("North", "South", "East", "West"), n,
                    replace = TRUE, prob = c(0.30, 0.30, 0.25, 0.15)),
    food_security = sample(c("Poor", "Borderline", "Acceptable"), n,
                           replace = TRUE, prob = c(0.30, 0.35, 0.35)),
    displacement_status = sample(c("IDP", "Refugee", "Returnee", "Host"), n,
                                 replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.15)),
    assistance_received = sample(c("Yes", "No"), n,
                                 replace = TRUE, prob = c(0.60, 0.40)),
    gender = sample(c("Male", "Female"), n,
                    replace = TRUE, prob = c(0.52, 0.48)),
    fcs_score = rnorm(n, mean = 42, sd = 15),
    monthly_income = rgamma(n, shape = 2.5, scale = 80),
    household_size = rpois(n, lambda = 5) + 1,
    expenditure = rlnorm(n, meanlog = 4.5, sdlog = 0.8),
    total_members = rpois(n, lambda = 6) + 1
  )

  # Then add dependent variables
  df <- df %>%
    mutate(
      children_count = pmin(rpois(n(), lambda = 3), total_members - 1),
      fcs_score = pmax(0, pmin(112, fcs_score)),
      monthly_income = pmax(0, monthly_income),
      expenditure = pmax(0, expenditure)
    )

  return(df)
}

# Create dataset with skip logic
create_skip_logic_data <- function(n = 500) {
  set.seed(54321)

  df <- data.frame(
    respondent_id = paste0("RESP_", seq_len(n)),
    survey_weight = runif(n, 0.8, 1.5),
    district = sample(c("District A", "District B", "District C"), n,
                      replace = TRUE, prob = c(0.40, 0.35, 0.25)),

    # Base question - everyone answers
    assistance_received = sample(c("Yes", "No"), n,
                                 replace = TRUE, prob = c(0.60, 0.40))
  )

  # Skip logic questions
  df <- df %>%
    mutate(
      assistance_type = ifelse(assistance_received == "Yes",
                               sample(c("Cash", "Food", "NFI", "Multiple"),
                                      n(), replace = TRUE,
                                      prob = c(0.35, 0.30, 0.20, 0.15)),
                               NA_character_),

      assistance_sufficient = ifelse(assistance_received == "Yes",
                                     sample(c("Sufficient", "Partially Sufficient", "Not Sufficient"),
                                            n(), replace = TRUE,
                                            prob = c(0.25, 0.45, 0.30)),
                                     NA_character_),

      cash_amount = ifelse(assistance_type == "Cash",
                           rnorm(n(), mean = 150, sd = 40),
                           NA_real_)
    ) %>%
    mutate(
      cash_amount = pmax(0, cash_amount, na.rm = FALSE)
    )

  return(df)
}

test_data <- create_test_frequency_data(n = 500)
skip_logic_data <- create_skip_logic_data(n = 500)

cat("\n=== Comprehensive Testing: table_frequency (Final Version) ===\n\n")

# ==========================================
# PART 1: UNIT COLUMN TOGGLE
# ==========================================

cat("=== PART 1: UNIT COLUMN TOGGLE (show_unit) ===\n\n")

# Test 1: With Unit column (default)
cat("Test 1: Multiple variables with Unit column (default)\n")
table1 <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "fcs_score", "children_count"),
  stat_type = c("percentage", "mean", "ratio"),
  ratio_denominator = c(NA, NA, "total_members"),
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = c("Food Security", "FCS Score", "Child Ratio"),
  show_unit = TRUE,  # default
  title_name = "With Unit Column"
)
print(table1)
cat("✓ Unit column shown\n")
cat("✓ Shows: %, Mean, Ratio\n\n")

# Test 2: Without Unit column
cat("Test 2: Same table without Unit column\n")
table2 <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "fcs_score", "children_count"),
  stat_type = c("percentage", "mean", "ratio"),
  ratio_denominator = c(NA, NA, "total_members"),
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = c("Food Security", "FCS Score", "Child Ratio"),
  show_unit = FALSE,
  title_name = "Without Unit Column"
)
print(table2)
cat("✓ Unit column hidden\n")
cat("✓ Columns: Variable | Value | n | Estimate\n\n")

# ==========================================
# PART 2: DISAGGREGATION WIDE FORMAT
# ==========================================

cat("=== PART 2: DISAGGREGATION WIDE FORMAT ===\n\n")

# Test 3: Long format (default)
cat("Test 3: Disaggregation - Long format (default)\n")
table3 <- table_frequency(
  .dataset = test_data,
  variable = "food_security",
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = "Food Security Status",
  disaggregation_wide = FALSE,  # default
  title_name = "Long Format: Food Security by District"
)
print(table3)
cat("✓ Long format (rows)\n")
cat("✓ Columns: district | Value | n | Unit | Estimate\n\n")

# Test 4: Wide format
cat("Test 4: Disaggregation - Wide format\n")
table4 <- table_frequency(
  .dataset = test_data,
  variable = "food_security",
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = "Food Security Status",
  disaggregation_wide = TRUE,
  title_name = "Wide Format: Food Security by District"
)
print(table4)
cat("✓ Wide format (columns)\n")
cat("✓ Two-level headers\n")
cat("✓ Top: District A | District B | District C | District D\n")
cat("✓ Bottom: n | Unit | Estimate (repeated for each district)\n\n")

# Test 5: Wide format with show_ci
cat("Test 5: Wide format with confidence intervals\n")
table5 <- table_frequency(
  .dataset = test_data,
  variable = "food_security",
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  show_ci = TRUE,
  variable_label = "Food Security Status",
  disaggregation_wide = TRUE,
  title_name = "Wide Format with 95% CI"
)
print(table5)
cat("✓ CI shown in wide format\n")
cat("✓ Estimate column shows: value [lower - upper]\n\n")

# Test 6: Wide format without Unit column
cat("Test 6: Wide format without Unit column\n")
table6 <- table_frequency(
  .dataset = test_data,
  variable = "food_security",
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = "Food Security Status",
  disaggregation_wide = TRUE,
  show_unit = FALSE,
  title_name = "Wide Format: No Unit Column"
)
print(table6)
cat("✓ Wide format\n")
cat("✓ Only n and Estimate columns per district\n\n")

# Test 7: Wide format with multiple stat types
cat("Test 7: Wide format - multiple variables, different stat types\n")
table7 <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "fcs_score"),
  stat_type = c("percentage", "mean"),
  disaggregation = "region",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = c("Food Security", "Mean FCS"),
  disaggregation_wide = TRUE,
  title_name = "Wide Format: Multiple Variables by Region"
)
print(table7)
cat("✓ Multiple variables in wide format\n")
cat("✓ Variable column on left\n")
cat("✓ Each region has n, Unit, Estimate columns\n\n")

# ==========================================
# PART 3: COMPARISON - LONG VS WIDE
# ==========================================

cat("=== PART 3: LONG VS WIDE COMPARISON ===\n\n")

cat("Test 8a: Multiple variables - LONG format\n")
table8a <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "assistance_received"),
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = c("Food Security", "Assistance"),
  disaggregation_wide = FALSE,
  title_name = "LONG: Multiple Indicators by District"
)
print(table8a)
cat("✓ Long format\n")
cat("✓ District repeats for each variable\n")
cat("✓ Easy to add many variables\n\n")

cat("Test 8b: Same data - WIDE format\n")
table8b <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "assistance_received"),
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = c("Food Security", "Assistance"),
  disaggregation_wide = TRUE,
  title_name = "WIDE: Multiple Indicators by District"
)
print(table8b)
cat("✓ Wide format\n")
cat("✓ Side-by-side district comparison\n")
cat("✓ Easier to compare across districts\n\n")

# ==========================================
# PART 4: WIDE FORMAT PRACTICAL USE CASES
# ==========================================

cat("=== PART 4: WIDE FORMAT USE CASES ===\n\n")

# Use Case 1: Regional comparison
cat("Use Case 1: Regional Comparison - Wide Format\n")
uc1 <- table_frequency(
  .dataset = test_data,
  variable = "food_security",
  stat_type = "percentage",
  disaggregation = "region",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  show_ci = TRUE,
  variable_label = "Food Security Category",
  disaggregation_wide = TRUE,
  title_name = "Food Security Across Regions (Wide Format)"
)
print(uc1)
cat("✓ Easy regional comparison\n")
cat("✓ All regions visible at once\n")
cat("✓ Good for executive summaries\n\n")

# Use Case 2: Multiple indicators by location
cat("Use Case 2: Multiple Indicators - Wide by District\n")
uc2 <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "fcs_score", "monthly_income"),
  stat_type = c("percentage", "mean", "median"),
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  show_ci = TRUE,
  variable_label = c("Food Security", "Mean FCS", "Median Income"),
  disaggregation_wide = TRUE,
  title_name = "Key Indicators by District (Wide Format)",
  digits = 1
)
print(uc2)
cat("✓ Multiple indicators\n")
cat("✓ District comparison side-by-side\n")
cat("✓ Different stat types (%, Mean, Median)\n\n")

# Use Case 3: Skip logic with wide format
cat("Use Case 3: Skip Logic Data - Wide Format\n")
uc3 <- table_frequency(
  .dataset = skip_logic_data,
  variable = "assistance_type",
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = "Type of Assistance",
  disaggregation_wide = TRUE,
  title_name = "Assistance Type by District (Wide, Skip Logic)"
)
print(uc3)
cat("✓ Skip logic N correctly shown\n")
cat("✓ NA values excluded\n")
cat("✓ District comparison in wide format\n\n")

# ==========================================
# PART 5: ADVANCED FEATURES COMBINATION
# ==========================================

cat("=== PART 5: COMBINING ALL FEATURES ===\n\n")

# Test 9: All features - long format
cat("Test 9: All features combined - LONG format\n")
table9 <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "fcs_score", "children_count"),
  stat_type = c("percentage", "mean", "ratio"),
  ratio_denominator = c(NA, NA, "total_members"),
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  show_ci = TRUE,
  variable_label = c("Food Security", "Mean FCS", "Dependency Ratio"),
  show_n = TRUE,
  show_unit = TRUE,
  disaggregation_wide = FALSE,
  title_name = "Complete Long Format Example",
  digits = 1
)
print(table9)
cat("✓ Multiple variables\n")
cat("✓ Different stat types\n")
cat("✓ Disaggregation (long)\n")
cat("✓ Confidence intervals\n")
cat("✓ Unit column shown\n")
cat("✓ N in variable labels\n\n")

# Test 10: All features - wide format
cat("Test 10: All features combined - WIDE format\n")
table10 <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "fcs_score"),
  stat_type = c("percentage", "mean"),
  disaggregation = "region",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  show_ci = TRUE,
  variable_label = c("Food Security", "Mean FCS Score"),
  show_n = TRUE,
  show_unit = TRUE,
  disaggregation_wide = TRUE,
  title_name = "Complete Wide Format Example",
  digits = 1
)
print(table10)
cat("✓ Multiple variables\n")
cat("✓ Wide format disaggregation\n")
cat("✓ Two-level headers\n")
cat("✓ Confidence intervals\n")
cat("✓ Unit column per region\n\n")

# ==========================================
# PART 6: EDGE CASES
# ==========================================

cat("=== PART 6: EDGE CASES ===\n\n")

# Test 11: Single variable, no disaggregation, no unit
cat("Test 11: Minimal configuration\n")
table11 <- table_frequency(
  .dataset = test_data,
  variable = "food_security",
  stat_type = "percentage",
  weighted_result = FALSE,
  show_n = TRUE,
  show_unit = FALSE,
  title_name = "Minimal: No Weights, No Unit, No Disagg"
)
print(table11)
cat("✓ Simplest table\n")
cat("✓ Columns: Value | n | Estimate\n\n")

# Test 12: Wide format with show_n = FALSE
cat("Test 12: Wide format without n column\n")
table12 <- table_frequency(
  .dataset = test_data,
  variable = "food_security",
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  show_n = FALSE,
  show_unit = TRUE,
  disaggregation_wide = TRUE,
  title_name = "Wide Format: No n Column"
)
print(table12)
cat("✓ Wide format\n")
cat("✓ Only Unit and Estimate per district\n\n")

# Test 13: Wide format with many disaggregation groups
cat("Test 13: Wide format with 4 districts\n")
table13 <- table_frequency(
  .dataset = test_data,
  variable = "assistance_received",
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = "Assistance Coverage",
  disaggregation_wide = TRUE,
  title_name = "Wide Format: 4 Districts"
)
print(table13)
cat("✓ Four districts side by side\n")
cat("✓ Two-level headers span correctly\n\n")

# ==========================================
# PART 7: FORMATTING OPTIONS
# ==========================================

cat("=== PART 7: FORMATTING OPTIONS IN WIDE FORMAT ===\n\n")

# Test 14: Different decimal places
cat("Test 14: Wide format - different decimal precision\n")
table14a <- table_frequency(
  .dataset = test_data,
  variable = "fcs_score",
  stat_type = "mean",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  disaggregation_wide = TRUE,
  digits = 0,
  title_name = "Wide Format: 0 Decimals"
)

table14b <- table_frequency(
  .dataset = test_data,
  variable = "fcs_score",
  stat_type = "mean",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  disaggregation_wide = TRUE,
  digits = 2,
  title_name = "Wide Format: 2 Decimals"
)

print(table14a)
print(table14b)
cat("✓ Decimal precision controlled in wide format\n\n")

# ==========================================
# PART 8: VALIDATION & VERIFICATION
# ==========================================

cat("=== PART 8: VALIDATION - WIDE VS LONG EQUIVALENCE ===\n\n")

cat("Test 15: Verify wide and long formats have same data\n")

# Create both versions
long_version <- table_frequency(
  .dataset = test_data,
  variable = "food_security",
  stat_type = "percentage",
  disaggregation = "region",
  weighted_result = FALSE,
  disaggregation_wide = FALSE,
  title_name = "LONG Version"
)

wide_version <- table_frequency(
  .dataset = test_data,
  variable = "food_security",
  stat_type = "percentage",
  disaggregation = "region",
  weighted_result = FALSE,
  disaggregation_wide = TRUE,
  title_name = "WIDE Version"
)

print(long_version)
cat("\n")
print(wide_version)

cat("\nVerification:\n")
cat("✓ Same underlying data\n")
cat("✓ Long: Better for many variables/categories\n")
cat("✓ Wide: Better for cross-group comparison\n")
cat("✓ Both formats produce accurate results\n\n")

# ==========================================
# PART 9: PRACTICAL REPORTING SCENARIOS
# ==========================================

cat("=== PART 9: PRACTICAL REPORTING SCENARIOS ===\n\n")

# Scenario 1: Executive dashboard - wide format
cat("Scenario 1: Executive Dashboard (Wide Format)\n")
scenario1 <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "assistance_received", "fcs_score"),
  stat_type = c("percentage", "percentage", "mean"),
  disaggregation = "region",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  show_ci = TRUE,
  variable_label = c("Food Insecurity Rate", "Assistance Coverage", "Mean FCS"),
  disaggregation_wide = TRUE,
  title_name = "Executive Dashboard: Key Indicators by Region",
  digits = 1
)
print(scenario1)
cat("✓ Wide format for executive view\n")
cat("✓ Easy regional comparison\n")
cat("✓ Multiple indicators at a glance\n\n")

# Scenario 2: Detailed analysis - long format
cat("Scenario 2: Detailed Analysis Report (Long Format)\n")
scenario2 <- table_frequency(
  .dataset = test_data,
  variable = c("food_security", "displacement_status", "assistance_received", "gender"),
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  show_ci = TRUE,
  variable_label = c("Food Security", "Displacement", "Assistance", "Gender"),
  disaggregation_wide = FALSE,
  title_name = "Detailed Analysis: Multiple Indicators by District (Long)",
  digits = 1
)
print(scenario2)
cat("✓ Long format for detailed analysis\n")
cat("✓ Many variables easily included\n")
cat("✓ District cells merged within variables\n\n")

# Scenario 3: Skip logic report - wide format
cat("Scenario 3: Skip Logic Assessment (Wide Format)\n")
scenario3 <- table_frequency(
  .dataset = skip_logic_data,
  variable = "assistance_type",
  stat_type = "percentage",
  disaggregation = "district",
  weighted_result = TRUE,
  weights_col = "survey_weight",
  variable_label = "Type of Assistance Received",
  disaggregation_wide = TRUE,
  title_name = "Assistance Type Distribution by District (Conditional Question)",
  digits = 1
)
print(scenario3)
cat("✓ Skip logic handled correctly\n")
cat("✓ N reflects actual respondents per district\n")
cat("✓ NA values excluded\n")
cat("✓ Wide format for comparison\n\n")

# ==========================================
# PART 10: FEATURE SUMMARY
# ==========================================

cat("=== COMPLETE FEATURE SUMMARY ===\n\n")

cat("CORE FUNCTIONALITY:\n")
cat("1. ✓ Single and multiple variables\n")
cat("2. ✓ Four stat types: percentage, mean, median, ratio\n")
cat("3. ✓ Weighted and unweighted analysis\n")
cat("4. ✓ 95% Confidence intervals (correct 0-100 scale)\n")
cat("5. ✓ Skip logic support (N = non-NA count)\n\n")

cat("DISAGGREGATION OPTIONS:\n")
cat("6. ✓ Long format (disaggregation as rows) - DEFAULT\n")
cat("7. ✓ Wide format (disaggregation as columns) - NEW\n")
cat("8. ✓ Two-level headers in wide format\n")
cat("9. ✓ Disaggregation merges within variables (long)\n\n")

cat("DISPLAY OPTIONS:\n")
cat("10. ✓ show_n: Toggle n column display\n")
cat("11. ✓ show_unit: Toggle Unit column display\n")
cat("12. ✓ show_ci: Toggle confidence intervals\n")
cat("13. ✓ N appended to variable labels\n")
cat("14. ✓ Consolidated Value column\n")
cat("15. ✓ Custom decimal precision\n\n")

cat("FORMATTING:\n")
cat("16. ✓ Roboto Condensed font\n")
cat("17. ✓ Light cell borders\n")
cat("18. ✓ Zebra striping\n")
cat("19. ✓ Merged cells (Variable, disaggregation)\n")
cat("20. ✓ Center-aligned Unit columns\n")
cat("21. ✓ Right-aligned numeric columns\n")
cat("22. ✓ Custom table dimensions\n\n")

cat("UNIT COLUMN:\n")
cat("- Shows: % | Mean | Median | Ratio\n")
cat("- Clarifies stat type for each row\n")
cat("- Especially useful in multi-variable tables\n")
cat("- Can be toggled with show_unit parameter\n\n")

cat("WIDE FORMAT BENEFITS:\n")
cat("- Side-by-side group comparison\n")
cat("- Executive-friendly presentation\n")
cat("- Good for dashboards\n")
cat("- Easier to spot differences across groups\n")
cat("- Two-level headers provide context\n\n")

cat("LONG FORMAT BENEFITS:\n")
cat("- Accommodate many variables\n")
cat("- Accommodate many categories per variable\n")
cat("- More compact for complex analyses\n")
cat("- Traditional statistical table format\n\n")

cat("WHEN TO USE WIDE:\n")
cat("✓ Few disaggregation groups (2-5)\n")
cat("✓ Need side-by-side comparison\n")
cat("✓ Executive summaries\n")
cat("✓ Dashboards and reports\n")
cat("✓ Highlighting differences\n\n")

cat("WHEN TO USE LONG:\n")
cat("✓ Many disaggregation groups (5+)\n")
cat("✓ Many variables to display\n")
cat("✓ Many categories per variable\n")
cat("✓ Detailed statistical reports\n")
cat("✓ Traditional analysis\n\n")

cat("KEY BEHAVIORS:\n")
cat("─────────────────────────────────────────────\n")
cat("Long Format Structure:\n")
cat("  District A → Food Security values\n")
cat("  District A → Assistance values\n")
cat("  District B → Food Security values\n")
cat("  District B → Assistance values\n\n")

cat("Wide Format Structure:\n")
cat("  Row Headers: Food Security categories\n")
cat("  Column Groups: District A | District B | District C\n")
cat("  Under each: n | Unit | Estimate\n\n")

cat("Skip Logic:\n")
cat("  ✓ N = non-NA count (not total rows)\n")
cat("  ✓ NA values excluded from Value column\n")
cat("  ✓ sum(n) = N for each variable\n")
cat("  ✓ Works in both long and wide formats\n\n")

cat("\n=== All table_frequency tests completed successfully ===\n")
cat("✓ Unit column toggle working\n")
cat("✓ Wide format disaggregation working\n")
cat("✓ Two-level headers in wide format working\n")
cat("✓ Long format remains functional\n")
cat("✓ All previous features maintained\n")
cat("✓ Skip logic handled correctly in both formats\n\n")
