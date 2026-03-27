# Household Schema Sample Datasets and Test Script

This directory contains sample datasets and a test script for validating the Household Data schema workflow in the iphRa application.

## Files

### Sample Datasets

1. **household_data_good_sample.csv** (500 records)
   - Fully compliant dataset that passes all schema validations
   - All required fields present and valid
   - Data types match schema expectations
   - Values within allowed ranges and enumerations
   - Dependencies correctly satisfied
   - Suitable for testing successful workflow execution

2. **household_data_bad_sample.csv** (500 records)
   - Dataset with intentional schema violations
   - Approximately 300+ violations across different types:
     * Missing required fields (~50 violations)
     * Invalid GPS coordinates (~25 violations)
     * Negative/zero weights (~25 violations)
     * Invalid age ranges (~20 violations)
     * End dates before start dates (~25 violations)
     * Invalid consent values (~15 violations)
     * Out-of-range numeric values (~40 violations)
     * Invalid categorical values (~20 violations)
     * Duplicate priority needs (~35 violations)
     * Dependency logic violations (~40 violations)
     * Duplicate UUIDs (~10 violations)
   - Suitable for testing error detection, cleaning logs, and deletion logs

### Test Script

**test_household_workflow.R**
- Comprehensive R script for testing the HouseholdData class workflow
- Tests both good and bad datasets
- Demonstrates the complete data pipeline:
  1. Load CSV data
  2. Create HouseholdData objects
  3. Validate data against schema
  4. Standardize data types and values
  5. Generate cleaning and deletion logs
  6. Run post-validation checks
  7. Export results

## Usage

### Running the Test Script

From the iphRa project root directory:

```r
# Option 1: Source the script in R
source("tests/manual/test_household_workflow.R")

# Option 2: Run from command line
Rscript tests/manual/test_household_workflow.R
```

### Output

The test script creates output files in `tests/manual/output/`:
- `good_household_standardized.csv` - Standardized version of good dataset
- `good_household_cleaning_log.csv` - Cleaning log for good dataset (if any)
- `bad_household_processed.csv` - Processed bad dataset
- `bad_household_quality_flags.csv` - Data quality flags from bad dataset
- `bad_household_cleaning_log.csv` - Cleaning operations required
- `bad_household_deletion_log.csv` - Records marked for deletion

## Schema Coverage

The sample datasets cover key schema elements defined in `household_schema_template.xlsx`:

### Required Fields
- uuid, hh_id, consent, start, end, deviceid

### Optional Core Fields
- enum_id, gps_lat, gps_lon, gps_precision, weight, stratum, cluster_id
- admin1, admin2, respondent_age, respondent_sex, num_members, hh_size

### Categorical Variables with Allowed Values
- consent (yes/no)
- residency_status (host/idp/idp_returnee/refugee)
- hohh_status (single/married/divorced/widowed)
- respondent_sex (m/f)
- fsl_fcs_cat (Poor/Borderline/Acceptable)
- fsl_hhs_cat (None/Moderate/Severe)

### Numeric Variables with Ranges
- respondent_age (18-120)
- num_members, hh_size (1-30)
- gps_lat (-90 to 90)
- gps_lon (-180 to 180)
- gps_precision (0-10)
- weight (>0)
- fsl_fcs_score (0-112)
- fsl_hhs_score (0-6)

### Dependency Rules Tested
- consent='yes' requires respondent_age >= 18
- end date must be after start date
- dod_idp_returnee='yes' requires residency_status != 'host'
- date_dod_idp_returnee must be before start date
- Priority needs must be different from each other
- cluster_id requires stratum
- Weights must be positive

## Generating New Samples

To regenerate the sample datasets:

```bash
# From project root
python3 dev/generate_samples.py
```

This will create fresh sample CSVs in the `resources/` directory.

## Schema Structure

The household schema template defines:
- **201 variables** with type and validation rules
- **196 dependency rules** for cross-field validation
- Support for:
  - Required vs optional fields
  - Data types (character, numeric, date)
  - Allowed value lists
  - Range constraints
  - Pattern matching
  - Unique constraints
  - Conditional dependencies (if-then logic)
  - Question types (for ODK/XLSForm compatibility)
  - "Other" field linking

## Notes

- The datasets use realistic but synthetic data
- All dates are in YYYY-MM-DD format
- Missing values are represented as empty strings or NA
- The bad dataset is designed to trigger multiple validation pathways
- Both datasets have the same structure but different data quality

## For Developers

When extending the schema or adding new validation rules:

1. Update `household_schema_template.xlsx` with new variables/rules
2. Regenerate sample datasets to include new fields
3. Update the test script to verify new validations
4. Run the full test suite to ensure compatibility

## References

- Schema template: `resources/household_schema_template.xlsx`
- HouseholdData class: `R/class_data_household.R`
- Schema utilities: `R/utils_data_class.R`
- Data class tests: `tests/testthat/test-data_household_class.R`
