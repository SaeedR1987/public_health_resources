# Schema Enhancements - Examples and Documentation

## Overview

This document provides examples for the new schema enhancements:
1. Question Type Field (`question_types`)
2. Select Multiple Expansion
3. "Other" Column Identification (`is_other`, `other_column_link`)

## 1. Question Type Field

The `question_types` field in the schema identifies the type of question from xlsform/odk:
- `select_one`: Single choice question
- `select_multiple`: Multiple choice question (will be expanded into dummy columns)
- `text`: Text input
- `integer`: Integer input
- `date`: Date input
- etc.

### Example Schema

```r
schema <- list(
  required = c("uuid"),
  types = list(
    uuid = "character",
    water_source = "character",
    livelihood = "character",
    age = "numeric"
  ),
  question_types = list(
    water_source = "select_one",
    livelihood = "select_multiple",
    age = "integer"
  )
)
```

## 2. Select Multiple Expansion

When a column is marked as `select_multiple` in the schema, it will be automatically
expanded into dummy columns during the `standardize()` method.

### Input Data

```r
df <- data.frame(
  uuid = c("id_1", "id_2", "id_3"),
  livelihood = c("farming fishing", "trading", "farming trading fishing")
)
```

### After Standardization

The `livelihood` column will be expanded into:
- `livelihood.farming` (1 if present, 0 if not)
- `livelihood.fishing` (1 if present, 0 if not)
- `livelihood.trading` (1 if present, 0 if not)

```r
d <- Data$new(data = df, uuid = "uuid", dataset_name = "TestData")
d$set_variable_schema(schema)
d$standardize()

# Result:
# uuid   livelihood                  livelihood.farming  livelihood.fishing  livelihood.trading
# id_1   farming fishing            1                   1                   0
# id_2   trading                    0                   0                   1
# id_3   farming trading fishing    1                   1                   1
```

### Key Features:
- Space-separated values are split and converted to dummy variables
- Order doesn't matter: "farming fishing" and "fishing farming" produce same result
- NA and empty values are handled correctly
- Original column is preserved

## 3. "Other" Column Identification

The `is_other` and `other_column_link` fields identify columns that capture open-ended
text because standard response options didn't capture the respondent's answer.

### Schema Definition

```r
schema <- list(
  required = c("uuid"),
  types = list(
    uuid = "character",
    water_source = "character",
    water_source_other = "character"
  ),
  question_types = list(
    water_source = "select_one",
    water_source_other = "text"
  ),
  is_other = list(
    water_source_other = TRUE
  ),
  other_column_link = list(
    water_source_other = c("water_source")
  )
)
```

### During Standardization

Schema-identified "other" columns are automatically added to `self$other_columns`:

```r
d <- Data$new(data = df, uuid = "uuid", dataset_name = "TestData")
d$set_variable_schema(schema)
d$standardize()

# d$other_columns will include "water_source_other"
```

### Cleaning Log Generation

When `generate_cleaning_log()` is called, two entries are created for each record
with an "other" value:

```r
df <- data.frame(
  uuid = c("id_1", "id_2", "id_3"),
  water_source = c("well", "other", "river"),
  water_source_other = c("", "mountain spring", "")
)

d <- Data$new(data = df, uuid = "uuid", dataset_name = "TestData")
d$set_variable_schema(schema)
d$standardize()
d$generate_cleaning_log()

# For id_2, two cleaning log entries are created:
# 1. question.name = "water_source_other", issue = "other_text_response"
#    old.value = "mountain spring"
# 2. question.name = "water_source", issue = "has_other_response"
#    old.value = "other"
```

### Naming Convention Inference

If `other_column_link` is not specified in the schema, the system will try to infer
the main column from naming conventions:

```r
# Column "income_source_other" will automatically link to "income_source"
# if both columns exist in the dataset
```

### Key Features:
- Both schema-identified and automatically detected "other" columns are handled
- Cleaning log entries help data cleaners identify and review "other" responses
- Flexible linking: can specify explicit link or rely on naming convention
- Multiple "other" columns can link to the same main column

## 4. Complete Example

```r
library(public_health_resources)

# Create sample survey data
df <- data.frame(
  uuid = paste0("hh_", 1:5),
  water_source = c("well", "river", "other", "tap", "other"),
  water_source_other = c("", "", "spring", "", "rainwater"),
  livelihood = c("farming", "farming fishing", "trading", "fishing trading", "farming"),
  age = c(30, 45, 25, 50, 35)
)

# Define schema with new features
schema <- list(
  required = c("uuid"),
  types = list(
    uuid = "character",
    water_source = "character",
    water_source_other = "character",
    livelihood = "character",
    age = "numeric"
  ),
  question_types = list(
    water_source = "select_one",
    water_source_other = "text",
    livelihood = "select_multiple",
    age = "integer"
  ),
  is_other = list(
    water_source_other = TRUE
  ),
  other_column_link = list(
    water_source_other = c("water_source")
  )
)

# Create Data object and process
d <- Data$new(data = df, uuid = "uuid", dataset_name = "SurveyData")
d$set_variable_schema(schema)

# Standardize: expands select_multiple, identifies "other" columns
d$standardize()

# Check results
names(d$standardized_data)
# Includes: uuid, water_source, water_source_other, livelihood, age,
#           livelihood.farming, livelihood.fishing, livelihood.trading

d$other_columns
# Includes: "water_source_other"

# Generate cleaning log for "other" responses
d$data_quality_flags <- data.frame(uuid = df$uuid)  # Empty flags
d$generate_cleaning_log(stage = "standardized")

# Review cleaning log
d$cleaning_log$log_df
# Will have entries for hh_3 and hh_5 (rows with "other" text)
```

## 5. Schema Table Format

When exporting/importing schemas as tables, the new fields appear as columns:

| variable | type | question_type | is_other | other_column_link |
|----------|------|---------------|----------|-------------------|
| uuid | character | NA | NA | NA |
| water_source | character | select_one | NA | NA |
| water_source_other | character | text | TRUE | water_source |
| livelihood | character | select_multiple | NA | NA |
| age | numeric | integer | NA | NA |

## 6. Migration Notes

For existing code:
- All new fields are optional
- Schemas without these fields continue to work unchanged
- Backward compatible with existing schemas
- The `other_columns` field continues to auto-detect "other" columns using heuristics
- Schema-identified "other" columns are added to the auto-detected list
