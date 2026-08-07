# Create a mock add_ function
add_better_column <- function(.dataset) {
  # Add a column with different values
  .dataset$better_col <- c(
    "new_val_1",
    "new_val_2",
    "new_val_1",
    "new_val_2",
    "new_val_1"
  )
  return(.dataset)
}

# Create a mock add_ function that adds a column
add_test_indicator_1 <- function(.dataset) {
  .dataset$indicator_1 <- rep("value_a", nrow(.dataset))
  return(.dataset)
}

# Create a mock add_ function that depends on indicator_1
add_test_indicator_2 <- function(.dataset, dep_col) {
  # This function checks if dep_col exists (should be mapped from indicator_1)
  if (!is.null(dep_col) && dep_col %in% names(.dataset)) {
    .dataset$indicator_2 <- paste0("depends_on_", .dataset[[dep_col]])
  } else {
    .dataset$indicator_2 <- "no_dependency"
  }
  return(.dataset)
}

# Create a mock add_ function that adds a preferred column
add_preferred_column <- function(.dataset) {
  # Add a column with the most preferred name
  .dataset$preferred_name <- .dataset$less_preferred_name
  return(.dataset)
}
