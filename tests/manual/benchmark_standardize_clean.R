#!/usr/bin/env Rscript

#' Manual Performance Benchmark: Data vs. DataOptimized
#' (standardize() / clean() workflow)
#'
#' @description
#' Standalone, dependency-light benchmark that runs the SAME dummy datasets
#' through both:
#'   * the original `HouseholdData` (backed by `Data`, `R/class_data.R`)
#'   * the optimized copy `HouseholdDataOptimized` (backed by
#'     `DataOptimized`, `R/class_data_optimized.R`)
#'
#' and times `validate()`, `standardize()`, and `clean()` (including
#' cleaning-log application) for each, so the two implementations can be
#' compared directly in a single run -- no need to check out different
#' commits or save separate "before"/"after" CSVs.
#'
#' Usage:
#'   Rscript tests/manual/benchmark_standardize_clean.R
#'   # or from an interactive session:
#'   source("tests/manual/benchmark_standardize_clean.R")
#'
#' Output:
#'   tests/manual/output/benchmark_standardize_clean_<timestamp>.csv
#'   A "latest" copy is also written to:
#'   tests/manual/output/benchmark_standardize_clean_latest.csv
#'
#' Each row is one (dataset size x implementation) combination, so the CSV
#' can be pivoted/filtered by `implementation` to compare "original" vs.
#' "optimized" directly, or a wide `speedup_*` column-set is also printed
#' to the console for a quick look.

rm(list = ls())

devtools::load_all()
source("dev/generate_household_samples.R")

# CONFIGURATION ####

# Dataset sizes to benchmark. Increase the largest value(s) to make any
# super-linear (e.g. O(n^2)) behavior more visible.
sample_sizes <- c(200, 1000, 4000, 8000)

# Fraction of rows to include in the synthetic cleaning log, to exercise
# clean() -> private$..apply_cleaning_changes().
cleaning_log_fraction <- 0.10

# Number of timing repetitions per stage/size (median is reported).
n_reps <- 3

# The two implementations to compare. `class_generator` is the R6 generator
# object, `label` is used in the output table.
implementations <- list(
  list(label = "original", class_generator = HouseholdData),
  list(label = "optimized", class_generator = HouseholdDataOptimized)
)

# WHERE TO WRITE RESULTS ####

output_dir <- "tests/manual/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path(
  output_dir,
  paste0("benchmark_standardize_clean_", timestamp, ".csv")
)
latest_file <- file.path(
  output_dir,
  "benchmark_standardize_clean_latest.csv"
)

# HELPERS ####

#' Time an expression, returning elapsed seconds (wall clock).
time_it <- function(expr) {
  t0 <- Sys.time()
  force(expr)
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

#' Run `expr_fn()` `n_reps` times and return the median elapsed time in seconds.
median_time <- function(expr_fn, n_reps) {
  times <- vapply(seq_len(n_reps), function(i) time_it(expr_fn()), numeric(1))
  stats::median(times)
}

#' Build a synthetic cleaning log touching `fraction` of rows across a
#' handful of columns, to exercise clean()'s cleaning-log application.
make_cleaning_log_df <- function(df, uuid_col, fraction) {
  candidate_cols <- setdiff(names(df), uuid_col)
  # Prefer character/numeric columns for simple, safely-coercible edits
  candidate_cols <- candidate_cols[vapply(
    candidate_cols,
    function(cn) is.character(df[[cn]]) || is.numeric(df[[cn]]),
    logical(1)
  )]
  if (length(candidate_cols) == 0) {
    return(NULL)
  }

  n <- nrow(df)
  n_edits <- max(1L, floor(n * fraction))
  edit_rows <- sample.int(n, n_edits, replace = FALSE)
  edit_cols <- sample(candidate_cols, n_edits, replace = TRUE)

  uuids <- as.character(df[[uuid_col]][edit_rows])
  old_vals <- mapply(
    function(r, cn) as.character(df[[cn]][r]),
    edit_rows,
    edit_cols
  )
  new_vals <- mapply(
    function(cn, ov) {
      if (is.numeric(df[[cn]])) {
        as.character(suppressWarnings(as.numeric(ov)) + 1)
      } else {
        paste0(ov, "_edited")
      }
    },
    edit_cols,
    old_vals
  )

  data.frame(
    uuid = uuids,
    enum_id = "benchmark_enum",
    device_id = "benchmark_device",
    question.name = edit_cols,
    issue = "benchmark synthetic edit",
    feedback = "benchmark synthetic edit",
    changed = "yes",
    old.value = old_vals,
    new.value = new_vals,
    stringsAsFactors = FALSE
  )
}

#' Run the validate/standardize/clean benchmark for one implementation on
#' one dummy dataset, returning a single-row data.frame of timings.
benchmark_one <- function(
  class_generator,
  label,
  n,
  hh_df,
  cleaning_log_fraction,
  n_reps
) {
  dataset_name <- paste0("BenchmarkHH_", label, "_", n)

  # --- validate() ---
  hh <- class_generator$new(data = hh_df, dataset_name = dataset_name)
  t_validate <- median_time(function() hh$validate(), n_reps)

  # --- standardize() ---
  # standardize() is not safe to re-run identically many times on the exact
  # same object, so rebuild a fresh object for each repetition to keep the
  # timing honest.
  t_standardize <- median_time(
    function() {
      obj <- class_generator$new(data = hh_df, dataset_name = dataset_name)
      obj$validate()
      obj$standardize()
    },
    n_reps
  )

  # --- clean() (including cleaning log application) ---
  uuid_col <- tryCatch(hh$get(field = "..uuid"), error = function(e) "uuid")
  if (is.null(uuid_col) || !uuid_col %in% names(hh_df)) {
    uuid_col <- "uuid"
  }

  t_clean <- median_time(
    function() {
      obj <- class_generator$new(data = hh_df, dataset_name = dataset_name)
      obj$validate()
      obj$standardize()

      std_df <- obj$get_data(stage = "standardized")
      log_df <- make_cleaning_log_df(std_df, uuid_col, cleaning_log_fraction)
      if (!is.null(log_df)) {
        obj$set(
          field = "cleaning_log",
          value = CleaningLog$new(log_df = log_df)
        )
      }

      obj$clean()
    },
    n_reps
  )

  data.frame(
    implementation = label,
    n_rows = n,
    n_cleaning_log_rows = max(1L, floor(n * cleaning_log_fraction)),
    n_reps = n_reps,
    validate_sec = t_validate,
    standardize_sec = t_standardize,
    clean_sec = t_clean,
    total_sec = t_validate + t_standardize + t_clean,
    stringsAsFactors = FALSE
  )
}

# BENCHMARK LOOP ####

results <- list()

for (n in sample_sizes) {
  message(sprintf("\n=== Benchmarking n = %d rows ===", n))

  # Generate the dummy dataset ONCE per size and reuse it for both
  # implementations, so the comparison is on identical input data.
  hh_df <- generate_household_dataset(n = n)

  for (impl in implementations) {
    res <- benchmark_one(
      class_generator = impl$class_generator,
      label = impl$label,
      n = n,
      hh_df = hh_df,
      cleaning_log_fraction = cleaning_log_fraction,
      n_reps = n_reps
    )
    results[[length(results) + 1L]] <- res

    message(sprintf(
      "[%s] validate: %.4fs | standardize: %.4fs | clean: %.4fs",
      impl$label,
      res$validate_sec,
      res$standardize_sec,
      res$clean_sec
    ))
  }
}

results_df <- do.call(rbind, results)

# Rows-per-second per stage: useful to spot super-linear (O(n^2)+) scaling --
# a healthy vectorized implementation should keep this roughly flat/increasing
# as n grows, while an O(n^2) loop will show it dropping sharply.
results_df$validate_rows_per_sec <- results_df$n_rows / results_df$validate_sec
results_df$standardize_rows_per_sec <- results_df$n_rows /
  results_df$standardize_sec
results_df$clean_rows_per_sec <- results_df$n_rows / results_df$clean_sec

cat("\n\n==== BENCHMARK SUMMARY (long format: one row per size x implementation) ====\n")
print(results_df, row.names = FALSE)

# WIDE COMPARISON TABLE: original vs. optimized side-by-side, with speedup
# ratios (original_sec / optimized_sec; > 1 means optimized is faster).
orig <- results_df[results_df$implementation == "original", ]
opt <- results_df[results_df$implementation == "optimized", ]
comparison_df <- merge(
  orig[, c(
    "n_rows",
    "validate_sec",
    "standardize_sec",
    "clean_sec",
    "total_sec"
  )],
  opt[, c(
    "n_rows",
    "validate_sec",
    "standardize_sec",
    "clean_sec",
    "total_sec"
  )],
  by = "n_rows",
  suffixes = c("_original", "_optimized")
)
comparison_df$speedup_validate <- comparison_df$validate_sec_original /
  comparison_df$validate_sec_optimized
comparison_df$speedup_standardize <- comparison_df$standardize_sec_original /
  comparison_df$standardize_sec_optimized
comparison_df$speedup_clean <- comparison_df$clean_sec_original /
  comparison_df$clean_sec_optimized
comparison_df$speedup_total <- comparison_df$total_sec_original /
  comparison_df$total_sec_optimized

cat("\n\n==== ORIGINAL vs. OPTIMIZED (speedup > 1 means optimized is faster) ====\n")
print(
  comparison_df[, c(
    "n_rows",
    "speedup_validate",
    "speedup_standardize",
    "speedup_clean",
    "speedup_total"
  )],
  row.names = FALSE
)

write.csv(results_df, output_file, row.names = FALSE)
write.csv(results_df, latest_file, row.names = FALSE)

cat(sprintf("\nResults written to:\n  %s\n  %s\n", output_file, latest_file))
cat(
  "\nEach run already contains both 'original' and 'optimized' timings for\n",
  "the same dummy data, so no separate before/after files are needed. Re-run\n",
  "this script any time to get a fresh comparison (e.g. after further\n",
  "changes to R/class_data_optimized.R).\n"
)
