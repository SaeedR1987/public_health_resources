# Data Cleaning Process

## Overview

The cleaning process is the final transformation step in the public_health_resources data pipeline. It applies validated corrections and deletions to standardized data, producing analysis-ready clean data. This document provides a comprehensive overview of the cleaning workflow, including cleaning log generation, deletion log generation, and the clean operation.

## Purpose

The cleaning process:
- Applies quality-driven corrections to data values
- Removes records marked for deletion
- Generates audit trails of all changes
- Produces final clean datasets ready for analysis
- Maintains data integrity and traceability

## The Cleaning Pipeline

The cleaning workflow consists of three main components working together:

```
Standardized Data → Quality Checks → Generate Logs → Review & Edit → Apply Clean → Clean Data
```

---

## Cleaning Workflow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    STANDARDIZED DATA                             │
│              (with quality check flags)                          │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                    RUN QUALITY CHECKS                            │
│          (creates data_quality_flags table)                      │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│               GENERATE CLEANING LOG                              │
├─────────────────────────────────────────────────────────────────┤
│  • Process quality flags (flag_*)                                │
│  • Process "other" response columns                              │
│  • Create entries with old values                                │
│  • Mark entries for review                                       │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│              GENERATE DELETION LOG                               │
├─────────────────────────────────────────────────────────────────┤
│  • Process critical flags (duplicates, etc.)                     │
│  • Create deletion entries                                       │
│  • Mark records for removal                                      │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│              MANUAL REVIEW & EDITING                             │
│   (Export logs → Review in Excel → Import updated logs)          │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                      CLEAN METHOD                                │
├─────────────────────────────────────────────────────────────────┤
│  1. Validate data                                                │
│  2. Copy standardized_data to clean_data                         │
│  3. Validate & apply cleaning log                                │
│  4. Validate & apply deletion log                                │
│  5. Set cleaned flag                                             │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                      CLEAN DATA                                  │
│            (ready for analysis & export)                         │
└─────────────────────────────────────────────────────────────────┘
```

---

## Component 1: Generate Cleaning Log

### Purpose

Automatically creates a cleaning log with entries for all detected data quality issues, ready for human review and correction.

### Method: `generate_cleaning_log()`

**Parameters**:
- `stage`: Data stage to use ("standardized" or "clean"), default: "standardized"
- `overwrite`: If TRUE, clears existing log before adding entries, default: FALSE

**Prerequisites**:
- `run_quality_checks()` must have been executed
- `data_quality_flags` must exist

### What Gets Logged

#### 1. Quality Check Flags

For each quality flag where `flag_value = 1`:

**Type Coercion Flags** (e.g., `flag_age_type`):
- Automatically marked as `changed = "yes"` (safe coercion failure)
- Indicates value needs correction or removal

**Dependency Check Flags** (e.g., `flag_fever_temp_check`):
- Uses `action` field from dependency_schema
- If `action = "flag_autoclean"`, marked as `changed = "yes"`
- Otherwise, marked as `changed = "no"` for review

**Multiple Variables per Flag**:
- If flag has `variables` field in dependency_schema, creates one entry per variable
- Example: A dependency checking fever/temperature/medication creates 3 log entries
- Uses `variable_map` to resolve canonical names to actual column names

#### 2. "Other" Response Columns

For each "other" text response detected:

**Main "Other" Column**:
- Creates entry with `issue = "other_response"`
- Includes the actual text response in feedback
- Marked as `changed = "no"` (informational)

**Linked Columns**:
- Creates additional entries for each parent/related column
- Uses `issue = "has_other_response"`
- Links back to the other text for context

### Cleaning Log Structure

Each entry contains:

| Column | Description | Example |
|--------|-------------|---------|
| uuid | Record identifier | "subm_001" |
| enum_id | Enumerator ID (from variable_map) | "enum_12" |
| device_id | Device ID (from variable_map) | "tablet_03" |
| question.name | Actual column name in dataset | "person_age" |
| issue | Type of issue flagged | "flag_age_range" |
| feedback | Human-readable description | "Auto-flagged by quality check: flag_age_range" |
| changed | Whether value should change | "yes" or "no" |
| old.value | Current value in dataset | "150" |
| new.value | Corrected value (initially NA) | "" |

### Variable Name Resolution

The cleaning log uses `resolve_column()` to ensure proper column naming:

```r
# Dependency schema uses canonical names
dependencies = list(
  flag_fever_temp = list(
    variables = c("fever", "temperature"),
    condition_if = "fever == 'yes'",
    then = "!is.na(temperature)"
  )
)

# Variable map links canonical → dataset names
variable_map = list(
  fever = "q7_fever_status",
  temperature = "q8_body_temp"
)

# Cleaning log entries use actual dataset column names:
# - question.name = "q7_fever_status"
# - question.name = "q8_body_temp"
```

**Why This Matters**:
- Ensures cleaning log references actual columns in the dataset
- Enables direct editing in tools like Excel
- Maintains traceability between schema and data

### Example Usage

```r
# After standardization
data_obj$standardize()

# Quality checks run automatically during standardization
# Flags are in data_obj$data_quality_flags

# Generate cleaning log from quality flags
data_obj$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

# View the log
View(data_obj$cleaning_log$log_df)

# Export for review
cleaning_table <- data_obj$cleaning_log$export()
write.csv(cleaning_table, "cleaning_log.csv", row.names = FALSE)

# After manual review and editing, import back
edited_log <- read.csv("cleaning_log_edited.csv")
data_obj$import_cleaning_log(edited_log, mode = "replace")
```

### Output Summary

After generation, the method reports:
- Number of entries created from quality flags
- Number of entries created from "other" columns
- Total entries added to the cleaning log

---

## Component 2: Generate Deletion Log

### Purpose

Identifies records with critical data quality issues that warrant complete removal from the dataset.

### Method: `generate_deletion_log()`

**Parameters**:
- `stage`: Data stage to use ("standardized" or "clean"), default: "standardized"
- `critical_flags`: Vector of flag column names warranting deletion, default: NULL (uses defaults)
- `overwrite`: If TRUE, clears existing log before adding entries, default: FALSE

**Prerequisites**:
- `run_quality_checks()` must have been executed
- `data_quality_flags` must exist

### Default Critical Flags

If `critical_flags` is not specified, the following defaults are used:

1. **Duplicate UUIDs**: `flag_[uuid_col]_unique`
   - Records with duplicate submission IDs
   - Indicates data collection or merging errors

**Rationale**: Duplicate UUIDs compromise data integrity and make records untrustworthy for analysis.

### Custom Critical Flags

You can specify additional flags for deletion:

```r
# Define custom critical flags
critical_issues <- c(
  "flag_consent_required",        # Missing consent
  "flag_uuid_unique",              # Duplicate UUIDs
  "flag_gps_coordinate_invalid"    # Invalid location data
)

data_obj$generate_deletion_log(
  stage = "standardized",
  critical_flags = critical_issues,
  overwrite = TRUE
)
```

### Deletion Log Structure

Each entry contains:

| Column | Description | Example |
|--------|-------------|---------|
| uuid | Record identifier | "subm_001" |
| enum_id | Enumerator ID | NA (not tracked for deletions) |
| device_id | Device ID | NA (not tracked for deletions) |
| issue | Critical issue type | "flag_uuid_unique" |
| feedback | Reason for deletion | "Critical quality issue: flag_uuid_unique" |

### Conservative Approach

**By Design**: Deletion is conservative and requires explicit flagging.

- Only specified critical flags trigger deletion
- Default behavior deletes only duplicate UUIDs
- All other issues go to cleaning log for review and correction
- Manual review can add additional deletions

**Philosophy**: Prefer correction over deletion when possible.

### Example Usage

```r
# Generate deletion log with defaults (duplicates only)
data_obj$generate_deletion_log(stage = "standardized")

# View deletion candidates
View(data_obj$deletion_log$log_df)

# Export for review
deletion_table <- data_obj$deletion_log$export()
write.csv(deletion_table, "deletion_log.csv", row.names = FALSE)

# Add manual deletions or remove false positives
# Then import back
edited_deletions <- read.csv("deletion_log_edited.csv")
data_obj$import_deletion_log(edited_deletions, mode = "replace")
```

### Output Summary

After generation, the method reports:
- Number of deletion entries created from critical flags
- Which flags were processed
- Warnings for flags not found in quality flags

---

## Component 3: Clean Method

### Purpose

Applies validated cleaning log corrections and deletion log removals to produce final clean data.

### Method: `clean()`

**Parameters**: None

**Prerequisites**:
- Data must be validated (`self$validated = TRUE`)
- Ideally, data should be standardized first
- Cleaning and deletion logs should be reviewed and finalized

### Cleaning Process Steps

#### 1. Validation Check

```r
self$validate()

if (!self$validated) {
  # ERROR: Cannot proceed without valid data
}
```

**Purpose**: Ensure data integrity before cleaning operations.

#### 2. Baseline Clean Data

```r
self$clean_data <- self$standardized_data
```

**Fallback Logic**:
- Prefers `standardized_data` if available
- Falls back to `raw_data` if standardization was skipped (with warning)
- Ensures cleaning always has a baseline to work from

#### 3. Apply Cleaning Log

**Process**:

a. **Validate Cleaning Log**
```r
self$cleaning_log$validate()
self$cleaning_log$post_validate(self, stage = "clean")
```

b. **Apply Changes**
- Iterates through each entry in `cleaning_log$log_df`
- Finds matching row by UUID
- Updates column value with `new.value`
- Only applies if `new.value` is not empty/NA

**Change Application Logic**:
```r
for (each entry in cleaning_log) {
  row_index <- find_row_by_uuid(entry$uuid)
  column <- entry$question.name
  new_value <- entry$new.value
  
  if (row_exists && column_exists) {
    clean_data[row_index, column] <- new_value
  }
}
```

**What Gets Changed**:
- Only entries with non-empty `new.value` are applied
- Empty/NA `new.value` entries are informational only
- Changes are applied in order they appear in the log

#### 4. Apply Deletion Log

**Process**:

a. **Validate Deletion Log**
```r
self$deletion_log$validate()
self$deletion_log$post_validate(self)
```

b. **Remove Records**
- Extracts UUIDs from `deletion_log$log_df`
- Removes all rows with matching UUIDs from `clean_data`

**Deletion Logic**:
```r
delete_ids <- deletion_log$log_df$uuid
clean_data <- clean_data[!clean_data$uuid %in% delete_ids, ]
```

**Impact**:
- Records are completely removed from the dataset
- No trace in clean_data (audit trail is in deletion_log)
- Row count of clean_data will be less than standardized_data

#### 5. Finalization

```r
self$cleaned <- TRUE
self$update_metadata()
```

**Result**:
- `clean_data` contains the final cleaned dataset
- `cleaned` flag is set to TRUE
- Metadata is updated with cleaning status and timestamps

### Error Handling

**Validation Failure**:
- If validation fails, cleaning aborts with error
- Must resolve validation issues first

**Missing Standardized Data**:
- Issues warning but attempts to proceed with raw_data
- Not recommended - always standardize before cleaning

**Log Validation Errors**:
- Cleaning/deletion logs must pass their own validation
- Invalid log entries are skipped with warnings

### Example Usage

```r
# Complete cleaning workflow
data_obj <- Data$new(data = survey_data, dataset_name = "Survey", uuid = "id")
data_obj$set_variable_schema(schema)
data_obj$map_schema_vars()

# 1. Standardize
data_obj$standardize()

# 2. Generate logs (automatic quality checks run during standardize)
data_obj$generate_cleaning_log(stage = "standardized", overwrite = TRUE)
data_obj$generate_deletion_log(stage = "standardized", overwrite = TRUE)

# 3. Review and edit logs (manual process)
# Export → Review in Excel → Import back
cleaning_df <- data_obj$cleaning_log$export()
write.csv(cleaning_df, "for_review.csv")
# ... manual review ...
reviewed <- read.csv("reviewed.csv")
data_obj$import_cleaning_log(reviewed, mode = "replace")

# 4. Apply cleaning
data_obj$clean()

# 5. Access clean data
clean_df <- data_obj$get_data("clean")
View(clean_df)

# 6. Export
data_obj$export_data(stage = "clean", format = "csv", file_path = "clean_survey.csv")
```

---

## Log Import and Export

### Exporting Logs

```r
# Export cleaning log to data frame
cleaning_table <- data_obj$cleaning_log$export()

# Export deletion log to data frame  
deletion_table <- data_obj$deletion_log$export()

# Save to CSV for review
write.csv(cleaning_table, "cleaning_log.csv", row.names = FALSE)
write.csv(deletion_table, "deletion_log.csv", row.names = FALSE)
```

### Reviewing Logs

**Cleaning Log Review**:
1. Open CSV in Excel or similar tool
2. Review each entry's `old.value` and `issue`
3. Fill in `new.value` for corrections (leave blank if no change needed)
4. Optionally adjust `changed` field
5. Save reviewed CSV

**Deletion Log Review**:
1. Open CSV in Excel or similar tool
2. Review each deletion candidate
3. Remove rows that should NOT be deleted
4. Add rows for additional deletions if needed
5. Save reviewed CSV

### Importing Reviewed Logs

```r
# Import reviewed cleaning log
reviewed_cleaning <- read.csv("cleaning_log_reviewed.csv")
data_obj$import_cleaning_log(reviewed_cleaning, mode = "replace")

# Import reviewed deletion log
reviewed_deletion <- read.csv("deletion_log_reviewed.csv")  
data_obj$import_deletion_log(reviewed_deletion, mode = "replace")

# Modes:
# - "replace": Clears existing log and uses imported version
# - "append": Adds to existing log (useful for incremental updates)
```

---

## Integration with Variable/Value Maps

### Variable Map in Cleaning

**Column Name Resolution**:
- Cleaning log uses actual dataset column names
- Resolution happens via `variable_map` during log generation
- Ensures logs reference real columns that can be edited

**Example**:
```r
# Schema uses canonical name
dependency_schema = list(
  flag_age_range = list(
    variables = c("age"),  # canonical name
    condition_if = "age >= 0",
    then = "age <= 120"
  )
)

# Variable map provides actual column name
variable_map = list(age = "respondent_age")

# Cleaning log entry uses actual name
# question.name = "respondent_age"  ✓
# NOT question.name = "age"         ✗
```

### Value Map in Cleaning

**Categorical Corrections**:
- Cleaning log can standardize categorical values
- Uses value_map to identify valid replacement values

**Example**:
```r
# Value map defines standard values
value_map = list(
  sex = list(
    male = c("male", "m", "homme"),
    female = c("female", "f", "femme")
  )
)

# Cleaning log can correct variants
# old.value = "homme" → new.value = "male"
# old.value = "m" → new.value = "male"
```

---

## Best Practices

### For Package Users

1. **Always standardize first**: Run `standardize()` before generating logs
2. **Review all logs**: Don't blindly apply auto-generated logs
3. **Export and review externally**: Use Excel/CSV for easier review
4. **Document decisions**: Add notes in feedback column about why changes were made
5. **Keep original logs**: Save auto-generated logs before editing
6. **Validate after cleaning**: Run `validate(stage = "clean")` after cleaning

### For Package Developers

1. **Define clear critical flags**: Be conservative about what triggers deletion
2. **Use dependency schemas**: Leverage `variables` field for multi-variable flags
3. **Set appropriate actions**: Use `action = "flag_autoclean"` sparingly
4. **Test log generation**: Verify logs contain expected entries
5. **Handle edge cases**: Test with missing columns, NA values, etc.

### For Data Managers

1. **Establish review workflows**: Define who reviews and approves logs
2. **Create review guidelines**: Document criteria for corrections vs deletions
3. **Track log versions**: Save dated copies of logs throughout review
4. **Document major decisions**: Note unusual corrections in the feedback field
5. **Maintain audit trails**: Keep all versions of logs for accountability

---

## Audit Trail and Traceability

### What Gets Tracked

**Cleaning Log**:
- Every proposed change (even if not applied)
- Old and new values
- Reason for flagging (issue)
- Human feedback/notes
- Whether change was applied

**Deletion Log**:
- Every deleted record
- UUID of deleted record
- Reason for deletion
- Issue that triggered deletion

**Metadata**:
- Cleaning timestamps
- Number of records in each stage
- Number of entries in each log

### Accessing Audit Information

```r
# Review what was changed
View(data_obj$cleaning_log$log_df)

# Review what was deleted
View(data_obj$deletion_log$log_df)

# Check counts
data_obj$metadata$cleaning_log_n  # entries in cleaning log
data_obj$metadata$deletion_log_n  # entries in deletion log

# Compare record counts
nrow(data_obj$standardized_data)  # before cleaning
nrow(data_obj$clean_data)          # after cleaning
```

---

## Common Scenarios

### Scenario 1: Simple Type Corrections

```r
# Age entered as "25 years" instead of 25
# Quality check flags: flag_age_type = 1

# Generate log
data_obj$generate_cleaning_log()

# Log entry created:
# uuid = "sub_001", question.name = "age", 
# old.value = "25 years", new.value = ""
# issue = "flag_age_type", changed = "yes"

# Edit log to correct
# new.value = "25"

# Apply
data_obj$clean()
# Result: age column now has 25 (numeric)
```

### Scenario 2: Dependency Violation

```r
# Fever = "yes" but temperature is missing
# Quality check flags: flag_fever_temp = 1

# Generate log (creates entries for both variables)
data_obj$generate_cleaning_log()

# Log entries:
# 1. uuid = "sub_001", question.name = "fever_status", 
#    old.value = "yes", changed = "no"
# 2. uuid = "sub_001", question.name = "temperature",
#    old.value = "", changed = "no"

# Review and correct in log:
# Entry 2: new.value = "38.5"

# Apply
data_obj$clean()
# Result: temperature now has 38.5
```

### Scenario 3: Duplicate Records

```r
# Two submissions with same UUID
# Quality check flags: flag_uuid_unique = 1 (for both)

# Generate deletion log
data_obj$generate_deletion_log()

# Review which duplicate to keep
# Remove one entry from deletion log (keep the better quality record)

# Apply
data_obj$clean()
# Result: Only one record remains
```

### Scenario 4: "Other" Responses

```r
# Question: "What is your occupation?"
# Response: "other" + free text "Astronaut"

# Generate log
data_obj$generate_cleaning_log()

# Log entries:
# 1. question.name = "occupation_other_text"
#    old.value = "Astronaut", issue = "other_response"
# 2. question.name = "occupation"
#    old.value = "other", issue = "has_other_response"

# After review, recode if appropriate:
# Entry 2: new.value = "professional" (if astronaut fits that category)

# Apply
data_obj$clean()
```

---

## Technical Implementation

**File**: `R/class_data.R`

**Methods**:
- `generate_cleaning_log()` - Lines ~1793-2021
- `generate_deletion_log()` - Lines ~2033-2107
- `clean()` - Lines ~734-829

**Helper Methods**:
- `get_flag_action_from_schema()` - Determines if flag requires auto-cleaning
- `get_flag_variables_from_schema()` - Extracts variables from dependency checks
- `.apply_cleaning_changes()` - Internal method that applies log entries

**Supporting Classes**:
- `CleaningLog` - R6 class for managing cleaning entries
- `DeletionLog` - R6 class for managing deletion entries

---

## Related Documentation

- **Validation**: See `docs/validation_process.md` for validation workflow
- **Standardization**: See `docs/standardization_process.md` for the pipeline
- **Quality Checks**: See `docs/dependency_schema_enhancements.md` for quality rules
- **Cleaning Enhancements**: See `docs/cleaning_log_enhancements.md` for technical details
- **Variable Mapping**: See `docs/variable_value_mapping_guide.md` for mapping system

---

## Summary

The cleaning process is the final quality assurance step that transforms standardized data into analysis-ready clean data. By leveraging automatically generated logs, manual review workflows, and robust audit trails, it ensures that data corrections and deletions are:

- **Systematic**: Based on quality check results
- **Traceable**: Every change is logged
- **Reviewable**: Human oversight before application
- **Reversible**: Logs can be edited and re-applied
- **Auditable**: Complete history of changes

The integration with variable and value maps ensures that cleaning operations reference actual dataset columns and can leverage canonical value definitions, making the cleaning process both powerful and maintainable.
