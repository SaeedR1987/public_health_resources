# iphRa Documentation

Welcome to the iphRa package documentation. This documentation is organized into two main sections: **Data Structures** and **Processes**.

## Documentation Structure

### Data_Structures/

Documentation for the R6 classes that form the foundation of the iphRa package.

#### Data_Classes/
- **[data_classes_overview.md](Data_Structures/Data_Classes/data_classes_overview.md)** - Overview of the Data class hierarchy
  - Base `Data` class
  - `HouseholdData`, `IndividualData` and subclasses
  - `WomenIndividualData`, `HealthIndividualData`, `DeathIndividualData`, `NutritionIndividualData`
  - `WaterContainerData`, `MUACDataset`
  - Relationship to Log, Quality, and Analysis objects

#### Quality_Classes/
- **[quality_classes_overview.md](Data_Structures/Quality_Classes/quality_classes_overview.md)** - Overview of the DataQuality class hierarchy
  - Base `DataQuality` class
  - `HealthDataQuality`, `DemographicsDataQuality`, `MortalityDataQuality`
  - `WASHDataQuality`, `AnthropometricDataQuality`
  - `FSLDataQuality`, `IYCFDataQuality`
  - Relationship to parent Data objects

#### Analysis_Classes/
- **[analysis_classes_overview.md](Data_Structures/Analysis_Classes/analysis_classes_overview.md)** - Overview of the QuantDataAnalysis class hierarchy
  - Base `QuantDataAnalysis` class
  - `HealthAnalysis`, `DemographicsAnalysis`, `MortalityAnalysis`
  - `WASHAnalysis`, `NutritionAnalysis`, `QuantDataAnalysisFSL`
  - Relationship to parent Data objects and DAP ownership

#### Log_Classes/
- **[log_classes_overview.md](Data_Structures/Log_Classes/log_classes_overview.md)** - Overview of the Log class hierarchy
  - Base `Log` class
  - `CleaningLog` - Track data cleaning operations
  - `DeletionLog` - Track data deletion operations
  - Ownership by Data objects (composition relationship)

#### Schema Documentation
- **[schema_overview.md](Data_Structures/schema_overview.md)** - Comprehensive overview of all schema types
- **[indicator_schema.md](Data_Structures/indicator_schema.md)** - Indicator schema structure and usage
- **[variable_schema_harmonization.md](Data_Structures/variable_schema_harmonization.md)** - Variable schema harmonization
- **[variable_value_mapping_guide.md](Data_Structures/variable_value_mapping_guide.md)** - Guide to value mapping
- **[schema_enhancements_examples.md](Data_Structures/schema_enhancements_examples.md)** - Schema enhancement examples
- **[dependency_schema_migration.md](Data_Structures/dependency_schema_migration.md)** - Dependency schema migration guide
- **[na_handling_in_dependency_rules.md](Data_Structures/na_handling_in_dependency_rules.md)** - NA handling in dependencies

### Processes/

Documentation for the major processes and workflows in the iphRa package.

#### error_handling/
- **[error_handling.md](Processes/error_handling/error_handling.md)** - Error handling patterns and practices

#### validation/
- **[validation_process.md](Processes/validation/validation_process.md)** - Data validation workflows

#### standardization/
- **[standardization_process.md](Processes/standardization/standardization_process.md)** - Data standardization process

#### cleaning/
- **[cleaning_process.md](Processes/cleaning/cleaning_process.md)** - Data cleaning workflows

#### quality_checks/
- **[quality_variable_mapping.md](Processes/quality_checks/quality_variable_mapping.md)** - Quality variable mapping

#### analysis/
- **[analysis_process.md](Processes/analysis/analysis_process.md)** - Statistical analysis workflows

### General Documentation

- **[naming_conventions.md](naming_conventions.md)** - Naming conventions used throughout the codebase

## Quick Start Guide

### 1. Understanding Data Structures

Start with the overview documents for each class hierarchy:
1. Read [Data_Classes/data_classes_overview.md](Data_Structures/Data_Classes/data_classes_overview.md) to understand data management
2. Read [Quality_Classes/quality_classes_overview.md](Data_Structures/Quality_Classes/quality_classes_overview.md) to understand quality assessment
3. Read [Analysis_Classes/analysis_classes_overview.md](Data_Structures/Analysis_Classes/analysis_classes_overview.md) to understand analysis capabilities
4. Read [Log_Classes/log_classes_overview.md](Data_Structures/Log_Classes/log_classes_overview.md) to understand logging

### 2. Understanding Schemas

Schemas are central to iphRa's functionality:
1. Read [schema_overview.md](Data_Structures/schema_overview.md) for a comprehensive introduction
2. Read [indicator_schema.md](Data_Structures/indicator_schema.md) for indicator calculations
3. Read [variable_value_mapping_guide.md](Data_Structures/variable_value_mapping_guide.md) for value mapping

### 3. Understanding Processes

Learn the key workflows:
1. [Validation](Processes/validation/validation_process.md) - How to validate survey data
2. [Standardization](Processes/standardization/standardization_process.md) - How to standardize data
3. [Cleaning](Processes/cleaning/cleaning_process.md) - How to clean data
4. [Quality Checks](Processes/quality_checks/quality_variable_mapping.md) - How to assess data quality
5. [Analysis](Processes/analysis/analysis_process.md) - How to analyze data

## Common Workflows

### Basic Data Processing Workflow

```r
# 1. Load and create data object
data <- HouseholdData$new(data = df, uuid = "uuid")

# 2. Set schema
data$set_variable_schema(schema)

# 3. Standardize
data$standardize()

# 4. Validate
data$validate()

# 5. Quality checks
quality <- DataQuality$new(data = data)
quality$run_quality_checks()

# 6. Clean data (using logs)
cleaning_log <- CleaningLog$new()
# ... add cleaning actions ...
cleaned_data <- cleaning_log$apply_to_data(data$standardized_data)

# 7. Analysis
analysis <- HealthAnalysis$new(data = data)
results <- analysis$calculate_all_indicators()
```

### Schema-Driven Workflow

```r
# Use schemas to automate most steps
data <- HouseholdData$new(data = df, uuid = "uuid")
data$set_variable_schema(variable_schema)
data$set_dependency_schema(dependency_schema)
data$set_indicator_schema(indicator_schema)
data$set_quality_schema(quality_schema)

# Automated processing
data$standardize()
data$validate()
data$calculate_indicators()

# Quality assessment
quality <- DataQuality$new(data = data)
quality$run_quality_checks()

# Analysis
analysis <- DemographicsAnalysis$new(data = data)
results <- analysis$calculate_all_indicators()
```

## Key Concepts

### R6 Classes
iphRa uses R6 object-oriented programming with inheritance hierarchies for Data, Quality, Analysis, and Log classes.

### Schemas
Schemas define the structure, validation rules, and transformations for survey data. Four main types:
- **Variable Schema**: Define variables, types, and validation rules
- **Dependency Schema**: Define logical consistency rules
- **Indicator Schema**: Define indicators to calculate
- **Quality Schema**: Define quality checks and thresholds

### Standardization
The process of transforming raw survey data into a consistent format with:
- Type coercion
- Value mapping
- Select multiple expansion
- Date formatting
- Missing value handling

### Quality Assessment
Systematic checking of data for:
- Plausibility issues
- Outliers
- Missing data patterns
- Logical inconsistencies
- Statistical anomalies

## Naming Conventions

The package uses specific naming conventions throughout. See [naming_conventions.md](naming_conventions.md) for details on:
- Dummy columns use **period (`.`)** separator: `skills.reading`, `skills.other`
- Text columns use **underscore (`_`)** separator: `skills_other_text`
- Standard variable names use **underscore (`_`)**: `household_id`, `respondent_age`
- R6 classes use **PascalCase**: `Data`, `HouseholdData`, `DataQuality`
- Functions use **snake_case**: `set_variable_schema()`, `standardize()`

## Additional Resources

For package-level documentation, see:
- Function reference: Use `?function_name` in R
- Package vignettes: `browseVignettes("iphRa")`
- GitHub repository: https://github.com/SaeedR1987/iphRa

## Contributing to Documentation

When adding new documentation:
1. Place class documentation in the appropriate `Data_Structures/` subfolder
2. Place process documentation in the appropriate `Processes/` subfolder
3. Include clear examples and use cases
4. Follow the existing documentation structure and style
5. Update this README if adding new major sections

## Getting Help

If you need help:
1. Check the relevant overview document for your class/process
2. Review the schema documentation if working with schemas
3. Look at examples in the process documentation
4. Check the function documentation with `?function_name`
5. Open an issue on GitHub

---

**Last Updated**: 2026-02-10
