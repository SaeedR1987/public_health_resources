# Data Classes Overview

## Purpose

The Data class hierarchy provides R6 classes for managing, validating, standardizing, and transforming survey datasets in the public_health_resources package. These classes form the foundation for all data operations in the system.

## Base Class: Data

The `Data` class is the abstract base class that defines common functionality for all dataset types. It provides:

- Data validation and schema enforcement
- Variable standardization and type coercion
- Select multiple expansion
- Value mapping and harmonization
- Date validation and formatting
- UUID management
- Schema management (variable, dependency, indicator, quality)
- "Other" column tracking and handling
- Nested Log objects for tracking changes
- Generation methods for Quality and Analysis objects

**Location**: `R/class_data.R`

## Interrelations with Other Classes

Data classes serve as the central hub that creates and manages other object types in the public_health_resources ecosystem:

### 1. Owns Log Objects

Each Data object has two nested Log objects that belong uniquely to it:

- **`cleaning_log`** (CleaningLog): Tracks all data cleaning operations applied to the dataset
  - Created during Data object initialization
  - Accessed via `data$cleaning_log`
  - Used in the `clean()` method to apply recorded changes
  - See [Log Classes Overview](../Log_Classes/log_classes_overview.md) for details

- **`deletion_log`** (DeletionLog): Tracks records marked for deletion
  - Created during Data object initialization  
  - Accessed via `data$deletion_log`
  - Used in the `clean()` method to remove flagged records
  - See [Log Classes Overview](../Log_Classes/log_classes_overview.md) for details

**Example:**
```r
# Data object creates and owns log objects
data <- HouseholdData$new(data = df, uuid = "uuid")

# Access nested logs
data$cleaning_log  # CleaningLog object
data$deletion_log  # DeletionLog object

# Logs are populated during cleaning
data$generate_cleaning_log()  # Populates cleaning_log
data$clean()  # Applies cleaning_log and deletion_log
```

### 2. Generates Quality Objects

Data objects create DataQuality objects for quality assessment:

- **`generate_data_quality()`** method creates domain-specific DataQuality objects
- The Quality object receives:
  - Reference to parent Data object (`parent_data_object`)
  - Data at specified stage (standardized or clean)
  - Data hash for version tracking
  - Variable and value maps from parent
- Quality objects are linked to their parent Data object
- See [Quality Classes Overview](../Quality_Classes/quality_classes_overview.md) for details

**Example:**
```r
# Data object generates Quality object
data <- HouseholdData$new(data = df, uuid = "uuid")
data$standardize()

# Generate quality object (inherits variable_map, value_map, data_hash)
quality <- data$generate_data_quality(stage = "standardized")

# Quality object references parent
quality$parent_data_object  # Points back to data object
```

### 3. Generates Analysis Objects

Data objects create QuantDataAnalysis objects for statistical analysis:

- **`generate_data_analysis()`** method creates domain-specific Analysis objects
- The Analysis object receives:
  - Reference to parent Data object (`parent_data_object`)
  - Data at specified stage (standardized or clean)
  - Data hash for version tracking
  - Variable and value maps from parent
  - Survey design object (for weighted analysis)
  - Data Analysis Plan (DAP) configuration
- Analysis objects store their own DAP as a field
- See [Analysis Classes Overview](../Analysis_Classes/analysis_classes_overview.md) for details

**Example:**
```r
# Data object generates Analysis object
data <- HouseholdData$new(data = df, uuid = "uuid")
data$standardize()

# Generate analysis object with optional DAP
analysis <- data$generate_data_analysis(
  stage = "standardized",
  analysis_config = my_dap
)

# Analysis object references parent and has its own DAP
analysis$parent_data_object  # Points back to data object
analysis$data_analysis_plan  # Stores the DAP as a field
```

### 4. Workflow Integration

The typical workflow shows how Data objects orchestrate other classes:

```r
# 1. Initialize Data object (creates nested Logs)
data <- HouseholdData$new(data = df, uuid = "uuid")

# 2. Standardize data
data$standardize()

# 3. Generate Quality object from Data
quality <- data$generate_data_quality(stage = "standardized")
quality$run_quality_checks()

# 4. Generate cleaning log based on quality flags
data$generate_cleaning_log(stage = "standardized")

# 5. Apply cleaning (uses nested cleaning_log and deletion_log)
data$clean()

# 6. Generate Analysis object from cleaned Data
analysis <- data$generate_data_analysis(stage = "clean")
analysis$calculate_all_indicators()
```

This architecture ensures:
- **Single source of truth**: Data object owns the data and metadata
- **Bidirectional links**: Quality/Analysis objects can access parent Data
- **Audit trail**: Log objects track all modifications
- **Reproducibility**: Data hash tracking across linked objects

## Class Hierarchy

```
Data (Base)
├── HouseholdData
├── IndividualData
│   ├── WomenIndividualData
│   ├── HealthIndividualData
│   ├── DeathIndividualData
│   └── NutritionIndividualData
├── WaterContainerData
└── MUACDataset
```

## Subclasses

### HouseholdData

Specialized class for household-level survey data.

**Purpose**: Manage household demographic, economic, and contextual data.

**Key Features**:
- Household member roster management
- Household-level indicator calculations
- Relationship tracking between household members

**Location**: `R/class_data_household.R`

---

### IndividualData

Base class for individual-level survey data.

**Purpose**: Manage data collected at the individual respondent level.

**Key Features**:
- Individual-level validation
- Person-specific standardization
- Link to household data when applicable

**Location**: `R/class_data_individual.R`

---

### WomenIndividualData

Specialized class for women's health and reproductive health data.

**Purpose**: Manage data specific to women's health surveys (e.g., maternal health, family planning).

**Key Features**:
- Women-specific validation rules
- Reproductive health indicators
- Pregnancy and birth history management

**Location**: `R/class_data_individual_women.R`

---

### HealthIndividualData

Specialized class for health services and morbidity data.

**Purpose**: Manage individual health status, illness episodes, and healthcare access data.

**Key Features**:
- Morbidity tracking
- Healthcare utilization indicators
- Treatment-seeking behavior analysis

**Location**: `R/class_data_individual_health.R`

---

### DeathIndividualData

Specialized class for mortality and death data.

**Purpose**: Manage death records and mortality-related information.

**Key Features**:
- Death cause tracking
- Mortality rate calculations
- Death date validation

**Location**: `R/class_data_individual_death.R`

---

### NutritionIndividualData

Specialized class for nutrition and anthropometry data.

**Purpose**: Manage nutritional status measurements and dietary data.

**Key Features**:
- Anthropometric measurements (weight, height, MUAC)
- Nutrition indicators (WHZ, HAZ, WAZ, MUAC)
- Dietary diversity and food consumption

**Location**: `R/class_data_individual_nutrition.R`

---

### WaterContainerData

Specialized class for water container testing data.

**Purpose**: Manage water quality testing data at the container level.

**Key Features**:
- Water quality test results
- Container type tracking
- Water source linkage

**Location**: `R/class_data_individual_water_container.R`

---

### MUACDataset

Specialized class for MUAC (Mid-Upper Arm Circumference) screening data.

**Purpose**: Manage rapid MUAC screening data for malnutrition detection.

**Key Features**:
- MUAC measurement validation
- Age-based MUAC classification
- Rapid screening workflows

**Location**: `R/data_muac_class.R`

## Common Functionality

All Data classes inherit these core capabilities from the base `Data` class:

### 1. Initialization

```r
data <- HouseholdData$new(
  data = df,
  uuid = "uuid_column_name"
)
```

### 2. Schema Management

```r
# Set variable schema
data$set_variable_schema(schema)

# Set dependency schema
data$set_dependency_schema(dep_schema)

# Set indicator schema
data$set_indicator_schema(ind_schema)

# Set quality schema
data$set_quality_schema(qual_schema)
```

### 3. Standardization

```r
# Standardize data (type coercion, value mapping, expansion)
data$standardize()

# Access standardized data
standardized_df <- data$standardized_data
```

### 4. Validation

```r
# Validate against schema
data$validate()

# Check required columns
data$check_required_columns()
```

### 5. Indicator Calculation

```r
# Add specific indicators
data$add_fcs()  # Food Consumption Score
data$add_hdds()  # Household Dietary Diversity Score

# Or use indicator schema for automatic calculation
data$calculate_indicators()
```

## Usage Patterns

### Basic Workflow

```r
# 1. Load data
df <- read.csv("survey_data.csv")

# 2. Create data object
data <- HouseholdData$new(data = df, uuid = "uuid")

# 3. Set schema
data$set_variable_schema(my_schema)

# 4. Standardize
data$standardize()

# 5. Validate
data$validate()

# 6. Calculate indicators
data$calculate_indicators()

# 7. Access results
clean_data <- data$standardized_data
```

### Advanced Features

```r
# Handle select_multiple expansion
data$expand_select_multiple("skills")

# Track "other" columns
other_cols <- data$other_columns

# Get validation errors
errors <- data$validation_errors

# Get schema information
schema_info <- data$get_schema_info()
```

## Key Design Principles

1. **Inheritance**: Subclasses extend base functionality with domain-specific features
2. **Schema-driven**: All operations are guided by configurable schemas
3. **Immutability**: Original data is preserved; transformations create new datasets
4. **Validation**: Built-in validation at every step to ensure data quality
5. **Flexibility**: Support for various data formats, question types, and workflows

## Related Documentation

- [Quality Classes Overview](../Quality_Classes/quality_classes_overview.md) - Quality objects generated by Data objects
- [Analysis Classes Overview](../Analysis_Classes/analysis_classes_overview.md) - Analysis objects generated by Data objects
- [Log Classes Overview](../Log_Classes/log_classes_overview.md) - Log objects owned by Data objects
- [Error Handling](../../Processes/error_handling/error_handling.md) - Error handling patterns
- [Validation Process](../../Processes/validation/validation_process.md) - Validation details
- [Standardization Process](../../Processes/standardization/standardization_process.md) - Standardization workflows
- [Schema Overview](../schema_overview.md) - Schema structure details
