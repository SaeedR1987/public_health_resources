# Log Classes Overview

## Purpose

The Log class hierarchy provides R6 classes for structured logging and tracking of data operations, particularly data cleaning and deletion activities. These classes maintain audit trails and support reproducible data management workflows.

## Base Class: Log

The `Log` class is the base class that defines common logging functionality. It provides:

- Structured log entry creation
- Log storage and retrieval
- Log export capabilities
- Log filtering and querying
- Timestamp management
- User action tracking

**Location**: `R/class_log.R`

## Relationship to Data Classes

Log objects are **owned by Data objects** in a composition relationship:

### 1. Owned by Data Objects

Every Data object has two nested Log objects that belong uniquely to it:

- **`cleaning_log`** (CleaningLog): Owned by the Data object
- **`deletion_log`** (DeletionLog): Owned by the Data object

These logs are **created during Data object initialization** and persist with the Data object throughout its lifecycle.

**Example:**
```r
# Data object creates and owns log objects automatically
data <- HouseholdData$new(data = df, uuid = "uuid")

# Logs are nested within the Data object
data$cleaning_log  # CleaningLog object owned by this Data instance
data$deletion_log  # DeletionLog object owned by this Data instance

# Each log is named after its parent Data object
data$cleaning_log$log_name  # "household_data_CleaningLog"
data$deletion_log$log_name  # "household_data_DeletionLog"
```

### 2. Composition (Not Aggregation)

The relationship is **composition**, not aggregation:
- Logs are created when the Data object is created
- Logs cannot exist independently of their parent Data object
- Logs are specific to their parent Data object's data
- Logs lifecycle is tied to the Data object lifecycle

### 3. Used in Data Cleaning Workflow

Logs are populated and applied within the Data object's workflow:

```
Data Object initialization
    ↓
creates → CleaningLog (nested)
creates → DeletionLog (nested)
    ↓
Data standardization
    ↓
Quality checks identify issues
    ↓
generate_cleaning_log() → populates CleaningLog
    ↓
clean() method → applies CleaningLog and DeletionLog
    ↓
Data Object (clean stage) with audit trail in logs
```

**Complete Example:**
```r
# 1. Data object creates nested logs on initialization
data <- HouseholdData$new(data = df, uuid = "uuid")
# data$cleaning_log and data$deletion_log now exist

# 2. Standardize and check quality
data$standardize()
quality <- data$generate_data_quality(stage = "standardized")
quality$run_quality_checks()

# 3. Generate cleaning log based on quality flags
data$generate_cleaning_log(stage = "standardized")
# This populates data$cleaning_log with cleaning actions

# 4. Manually add deletion entries if needed
data$deletion_log$add_deletion(
  uuid = "id_to_delete",
  reason = "Duplicate record"
)

# 5. Apply logs to clean data
data$clean()
# Uses data$cleaning_log to correct values
# Uses data$deletion_log to remove records

# 6. Export logs for documentation
data$cleaning_log$export_csv("cleaning_log.csv")
data$deletion_log$export_csv("deletion_log.csv")

# 7. Import logs from external sources if needed
data$import_cleaning_log(external_cleaning_df, mode = "append")
data$import_deletion_log(external_deletion_df, mode = "append")
```

### 4. Key Ownership Properties

| Property | Value |
|----------|-------|
| **Created by** | Data object during initialization |
| **Owned by** | Data object (composition) |
| **Lifespan** | Same as parent Data object |
| **Access** | Via `data$cleaning_log` and `data$deletion_log` |
| **Purpose** | Track modifications to parent's data |
| **Uniqueness** | Each Data object has its own unique log instances |

### 5. Differences from Quality/Analysis Objects

| Aspect | Log Objects | Quality/Analysis Objects |
|--------|------------|--------------------------|
| **Relationship** | Owned (composition) | Generated (association) |
| **Creation** | At Data initialization | On-demand via generate methods |
| **Lifespan** | Same as Data object | Can exist independently |
| **Purpose** | Track changes to data | Assess or analyze data |
| **Persistence** | Nested in Data object | Separate objects with parent reference |

See [Data Classes Overview](../Data_Classes/data_classes_overview.md) for more details on how Data objects own and use Log objects.

## Class Hierarchy

```
Log (Base)
├── CleaningLog
├── DeletionLog
└── QuantDataAnalysisPlanLog
```

## Subclasses

### CleaningLog

Specialized class for tracking data cleaning operations.

**Purpose**: Maintain a detailed record of all data cleaning actions, enabling reproducibility and audit trails.

**Key Features**:
- Record cleaning operations with before/after values
- Track cleaning rules applied
- Link to data quality flags
- Support cleaning decision documentation
- Enable cleaning action replay
- Generate cleaning reports

**Typical Use Cases**:
- Documenting value corrections
- Recording variable transformations
- Tracking outlier treatment
- Maintaining data modification history
- Supporting data quality audits

**Location**: `R/class_cleaning_log.R`

---

### DeletionLog

Specialized class for tracking data deletion operations.

**Purpose**: Maintain a record of deleted records and the reasons for deletion.

**Key Features**:
- Record deleted records with full context
- Document deletion reasons and criteria
- Track deletion timestamps and users
- Enable deletion review and validation
- Support undelete operations (if original data preserved)
- Generate deletion reports

**Typical Use Cases**:
- Documenting record exclusions
- Tracking duplicate removal
- Recording consent withdrawals
- Maintaining deletion audit trail
- Supporting data protection compliance

**Location**: `R/class_deletion_log.R`

---

### QuantDataAnalysisPlanLog

Specialized class for managing quantitative data analysis plans.

**Purpose**: Maintain structured analysis plans with schema enforcement and validation for quantitative data analysis workflows.

**Key Features**:
- Enforce column structure from `phr_analysis_plan_template.csv`
- Validate indicator calculation types (prop, mean, median, ratio)
- Validate multiplier values are positive
- Track planned analyses with proper type safety
- Support reproducible analytical workflows
- Enable export/import of analysis plans

**Required Columns**:
- `indicator_name`: Name/label of the indicator
- `calculation`: Type of calculation (prop, mean, median, ratio)
- `var_name`: Primary variable name in the dataset
- `denom_var`: Denominator variable (for ratios)
- `disaggregation`: Disaggregation variable(s)
- `multiplier`: Multiplier for the calculation (numeric, must be positive)
- `indicator_unit`: Unit of measurement for the indicator

**Typical Use Cases**:
- Defining data analysis plans (DAPs) for quantitative analysis
- Validating analysis plan structure before execution
- Tracking planned indicators and calculations
- Ensuring type safety for analysis configurations
- Supporting reproducible analysis workflows

**Location**: `R/class_quant_data_analysis_plan_log.R`

**Usage Example**:
```r
# Create analysis plan log
analysis_plan <- QuantDataAnalysisPlanLog$new()

# Add indicators to the plan
analysis_plan$add_indicator(
  indicator_name = "Crude Death Rate",
  calculation = "ratio",
  var_name = "deaths",
  denom_var = "person_time",
  multiplier = 10000,
  indicator_unit = "deaths per 10,000 person-days"
)

# Validate the plan
issues <- analysis_plan$validate()
if (analysis_plan$validated) {
  message("Analysis plan is valid")
}

# Export the plan
analysis_plan$export(file = "analysis_plan.csv", format = "csv")
```

**Integration with QuantDataAnalysis and DataAnalytics**:

The `QuantDataAnalysisPlanLog` is owned by both `QuantDataAnalysis` objects (legacy) and `DataAnalytics` objects (unified):

```r
# Legacy: QuantDataAnalysis creates a QuantDataAnalysisPlanLog on initialization
qda <- QuantDataAnalysis$new()

# Access the analysis plan log
qda$data_analysis_plan  # This is a QuantDataAnalysisPlanLog object

# Add indicators through the QuantDataAnalysis interface
qda$add_indicator_dap(
  indicator_name = "Prevalence",
  calculation = "prop",
  var_name = "indicator_var",
  multiplier = 100,
  indicator_unit = "%"
)

# Access the underlying data frame
qda$data_analysis_plan$log_df  # The actual data frame with indicators

# Unified: DataAnalytics also owns a QuantDataAnalysisPlanLog
analytics <- data$generate_data_analytics(stage = "clean")
analytics$data_analysis_plan         # QuantDataAnalysisPlanLog object
analytics$data_analysis_plan$log_df  # Underlying data frame
```

## Common Functionality

All Log classes inherit these core capabilities from the base `Log` class:

### 1. Initialization

```r
# Create cleaning log
cleaning_log <- CleaningLog$new()

# Create deletion log
deletion_log <- DeletionLog$new()
```

### 2. Adding Log Entries

```r
# Add a log entry
log$add_entry(
  uuid = "respondent_id_123",
  variable = "age",
  old_value = "999",
  new_value = "45",
  reason = "Corrected data entry error",
  timestamp = Sys.time(),
  user = "analyst_name"
)
```

### 3. Querying Logs

```r
# Get all entries
all_entries <- log$get_entries()

# Filter by UUID
uuid_entries <- log$get_entries_by_uuid("respondent_id_123")

# Filter by variable
var_entries <- log$get_entries_by_variable("age")

# Filter by date range
recent_entries <- log$get_entries_by_date(
  start_date = "2024-01-01",
  end_date = "2024-12-31"
)
```

### 4. Exporting Logs

```r
# Export to CSV
log$export_csv(file = "cleaning_log.csv")

# Export to Excel
log$export_excel(file = "cleaning_log.xlsx")

# Export to JSON
log$export_json(file = "cleaning_log.json")
```

### 5. Log Statistics

```r
# Get summary statistics
summary <- log$get_summary()

# Count entries by variable
var_counts <- log$count_by_variable()

# Count entries by reason
reason_counts <- log$count_by_reason()
```

## CleaningLog Specific Features

### Log Entry Structure

A cleaning log entry typically includes:

```r
cleaning_entry <- list(
  uuid = "respondent_id_123",        # Record identifier
  variable = "age",                  # Variable name
  old_value = "999",                 # Original value
  new_value = "45",                  # Corrected value
  reason = "Data entry error",       # Reason for change
  rule = "age_validation_rule_01",   # Rule that triggered cleaning
  quality_flag = "ERROR",            # Associated quality flag
  timestamp = "2024-01-15 10:30:00", # When change was made
  user = "analyst_name",             # Who made the change
  approved = TRUE,                   # Whether change was approved
  comment = "Confirmed with supervisor" # Additional notes
)
```

### Applying Cleaning Actions

```r
# Apply cleaning actions from log to data
cleaned_data <- cleaning_log$apply_to_data(
  data = original_data,
  uuid_column = "uuid"
)
```

### Generating Cleaning Reports

```r
# Generate summary report
report <- cleaning_log$generate_report()

# Generate detailed cleaning history for a record
history <- cleaning_log$get_cleaning_history(uuid = "respondent_id_123")
```

## DeletionLog Specific Features

### Deletion Entry Structure

A deletion log entry typically includes:

```r
deletion_entry <- list(
  uuid = "respondent_id_456",           # Record identifier
  deletion_reason = "Duplicate entry",   # Reason for deletion
  deletion_criteria = "same_name_age",   # Criteria used
  original_record = list(...),           # Full original record
  timestamp = "2024-01-15 11:00:00",    # When deleted
  user = "analyst_name",                 # Who deleted
  approved = TRUE,                       # Whether deletion was approved
  comment = "Confirmed duplicate",       # Additional notes
  reversible = TRUE                      # Whether can be undeleted
)
```

### Tracking Deleted Records

```r
# Add deletion entry
deletion_log$add_deletion(
  uuid = "respondent_id_456",
  reason = "Duplicate entry",
  criteria = "same UUID and interview date",
  original_record = record_data
)

# Get all deleted records
deleted <- deletion_log$get_deleted_records()

# Check if a UUID was deleted
is_deleted <- deletion_log$is_deleted(uuid = "respondent_id_456")
```

### Deletion Statistics

```r
# Count deletions by reason
reason_summary <- deletion_log$count_by_reason()

# Get deletion timeline
timeline <- deletion_log$get_deletion_timeline()
```

## Usage Patterns

### Basic Cleaning Log Workflow

```r
# 1. Create cleaning log
cleaning_log <- CleaningLog$new()

# 2. Identify issues through quality checks
quality <- DataQuality$new(data = data)
quality$run_quality_checks()
flags <- quality$get_flagged_records()

# 3. Review and document cleaning decisions
for (flag in flags) {
  # Review flag and determine action
  if (action_needed) {
    cleaning_log$add_entry(
      uuid = flag$uuid,
      variable = flag$variable,
      old_value = flag$current_value,
      new_value = corrected_value,
      reason = flag$issue,
      rule = flag$rule_id
    )
  }
}

# 4. Apply cleaning actions
cleaned_data <- cleaning_log$apply_to_data(data$standardized_data)

# 5. Export log for documentation
cleaning_log$export_excel(file = "cleaning_log.xlsx")
```

### Basic Deletion Log Workflow

```r
# 1. Create deletion log
deletion_log <- DeletionLog$new()

# 2. Identify duplicates or records to exclude
duplicates <- data$find_duplicates(by = c("name", "age", "interview_date"))

# 3. Document deletions
for (dup in duplicates) {
  deletion_log$add_deletion(
    uuid = dup$uuid,
    reason = "Duplicate record",
    criteria = "Same name, age, and interview date",
    original_record = dup
  )
}

# 4. Remove duplicates from data
clean_data <- data[!data$uuid %in% deletion_log$get_deleted_uuids(), ]

# 5. Export deletion log
deletion_log$export_excel(file = "deletion_log.xlsx")
```

### Combined Logging Workflow

```r
# Use both logs together
cleaning_log <- CleaningLog$new()
deletion_log <- DeletionLog$new()

# Process data
data <- HouseholdData$new(data = df, uuid = "uuid")
data$standardize()

# Run quality checks
quality <- DataQuality$new(data = data)
quality$run_quality_checks()

# Handle flagged records
flags <- quality$get_flagged_records()
for (flag in flags) {
  if (should_clean(flag)) {
    # Log cleaning action
    cleaning_log$add_entry(...)
  } else if (should_delete(flag)) {
    # Log deletion action
    deletion_log$add_deletion(...)
  }
}

# Apply changes
cleaned_data <- cleaning_log$apply_to_data(data$standardized_data)
final_data <- cleaned_data[!cleaned_data$uuid %in% deletion_log$get_deleted_uuids(), ]

# Export both logs
cleaning_log$export_excel(file = "cleaning_log.xlsx")
deletion_log$export_excel(file = "deletion_log.xlsx")
```

## Best Practices

### 1. Document All Changes

Always log every data modification, no matter how small:
```r
# Good
cleaning_log$add_entry(uuid = "id1", variable = "age", 
                       old_value = "45", new_value = "46",
                       reason = "Verified with supervisor")

# Bad - making undocumented changes
data$age[data$uuid == "id1"] <- 46
```

### 2. Use Descriptive Reasons

Provide clear, actionable reasons for changes:
```r
# Good
reason = "Survey team confirmed 999 is missing value code, not actual age"

# Bad
reason = "Fixed"
```

### 3. Include Context

Add relevant context in comments:
```r
cleaning_log$add_entry(
  ...,
  comment = "Contacted respondent by phone on 2024-01-15, confirmed correct age is 45"
)
```

### 4. Export Regularly

Export logs regularly for backup and sharing:
```r
# Export after each cleaning session
cleaning_log$export_excel(file = paste0("cleaning_log_", Sys.Date(), ".xlsx"))
```

### 5. Review Before Applying

Review log entries before applying changes:
```r
# Review entries
summary <- cleaning_log$get_summary()
print(summary)

# Apply only after review
cleaned_data <- cleaning_log$apply_to_data(data)
```

## Key Design Principles

1. **Traceability**: Every change is tracked with full context
2. **Reproducibility**: Logs can be used to replay changes
3. **Auditability**: Complete audit trail for data management
4. **Transparency**: Clear documentation of data decisions
5. **Reversibility**: Original values preserved for potential rollback

## Related Documentation

- [Data Classes Overview](../Data_Classes/data_classes_overview.md) - Data objects that own Log objects
- [Analytics Classes Overview](../Analytics_Classes/analytics_classes_overview.md) - Unified analytics objects that own QuantDataAnalysisPlanLog
- [Analysis Classes Overview](../Analysis_Classes/analysis_classes_overview.md) - Legacy analysis objects that own QuantDataAnalysisPlanLog
- [Quality Classes Overview](../Quality_Classes/quality_classes_overview.md) - Quality checks that generate log entries
- [Cleaning Process](../../Processes/cleaning/cleaning_process.md) - Cleaning workflows using logs
- [Error Handling](../../Processes/error_handling/error_handling.md) - Error handling patterns
