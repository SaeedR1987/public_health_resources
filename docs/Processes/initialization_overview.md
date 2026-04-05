# Object Initialization Overview

## Purpose

This document provides an overview of initialization processes for the core R6 classes in the public_health_resources package. Understanding these initialization steps is crucial for working with Data, Log, Quality, and Analysis objects, as they establish the foundational state and relationships between objects.

Object initialization in public_health_resources serves several critical functions:
- Validates input parameters and data structures
- Establishes required fields and metadata
- Creates relationships between parent and child objects
- Loads schema definitions and templates
- Sets up tracking mechanisms (logs, validation flags)
- Ensures objects are ready for downstream operations

## Class-Specific Initialization Documentation

For detailed initialization steps for each object type, see:

- **[Data Object Initialization](data_initialization.md)** - Base class for all data structures
- **[Log Object Initialization](log_initialization.md)** - Base class for logging mechanisms (CleaningLog, DeletionLog)
- **[DataQuality Object Initialization](quality_initialization.md)** - Quality assessment for Data objects
- **[QuantDataAnalysis Object Initialization](analysis_initialization.md)** - Quantitative data analysis framework

## Common Initialization Patterns

### Metadata Tracking
All objects initialize metadata with:
- Creation timestamp (`Sys.time()`)
- Object identification information
- State tracking for operations

### Validation Flags
Most objects include validation flags:
- Initially set to FALSE
- Updated to TRUE when validation succeeds
- Used to control workflow progression

### Parent-Child Relationships
Quality and Analysis objects:
- Link to parent Data objects
- Access parent's mappings and schema
- Coordinate operations with parent state

### Schema Loading
Quality and Analysis objects:
- Attempt to load templates from package resources
- Provide fallback to empty schemas
- Allow custom schema configuration after initialization

### Template-Based Initialization
Many objects use templates from `system.file("resources", ...)`:
- Quality checks template
- Analysis schema template
- Data analysis plan template
- Ensures consistency across applications

## Initialization Workflow

```
┌─────────────────────────────────────────────────────────┐
│                   Data Object Created                    │
│  • Validates input data frame                           │
│  • Stores raw data                                      │
│  • Creates CleaningLog and DeletionLog                  │
│  • Initializes mapping structures                       │
└────────────────────┬────────────────────────────────────┘
                     │
                     ├─────────────────────────────────────┐
                     │                                     │
                     ▼                                     ▼
         ┌───────────────────────┐          ┌────────────────────────┐
         │ DataQuality Created   │          │ QuantDataAnalysis      │
         │  • Links to parent    │          │  • Links to parent     │
         │  • Loads QC schema    │          │  • Loads analysis      │
         │  • Prepares results   │          │    schema and DAP      │
         └───────────────────────┘          └────────────────────────┘
```

## General Initialization Pattern

Each object type follows a consistent pattern:

1. **Validate inputs** - Ensure parameters meet requirements
2. **Store core data** - Save essential references and data
3. **Initialize metadata** - Track creation and state information
4. **Load schemas/templates** - Set up configuration from resources
5. **Create child objects** - Establish related objects (logs, etc.)
6. **Set up containers** - Prepare structures for results and tracking
7. **Set validation flags** - Initialize state tracking variables

## Best Practices Summary

### For Data Objects
- Always provide explicit UUID (required parameter)
- Set schema early using `set_variable_schema()`
- Map variables with `map_schema_vars()`
- Validate before operations

### For Log Objects
- Let Data objects create logs automatically
- Use provided methods for adding entries
- Trust automatic column enforcement

### For Quality Objects
- Link to parent Data object when possible
- Use standardized data for quality checks
- Extend quality_schema for custom checks
- Review metadata for sanity checks

### For Analysis Objects
- Always link to parent Data object
- Include survey_design for weighted analysis
- Provide or customize data_analysis_plan
- Use clean data stage when available

## Troubleshooting Guide

### Common Issues Across All Objects

**Object creation fails**
- Check that all required parameters are provided
- Verify data types match expected types
- Review error messages for specific guidance

**Template loading warnings**
- Normal if package resources not fully installed
- Objects still created with empty schemas
- Provide custom schemas as needed

### Parent-Child Linkage Issues

**Parent object reference errors**
- Ensure parent object is created first
- Verify parent object is of correct type
- Check that parent object is not NULL

**Missing mappings or schema**
- Set up variable_map and value_map in parent
- Configure schema in parent before creating child
- Use `map_schema_vars()` to populate mappings

## Related Documentation

- **Data Structures**: See `docs/Data_Structures/` for class architecture
- **Validation Process**: See `docs/Processes/validation/validation_process.md`
- **Quality Checks**: See `docs/Processes/quality_checks/` for quality assessment
- **Analysis Process**: See `docs/Processes/analysis/analysis_process.md`
- **Naming Conventions**: See `docs/naming_conventions.md`

## Summary

Understanding object initialization is crucial for effective use of the public_health_resources package. Each object type has specific initialization requirements documented in detail in the class-specific pages linked above. All objects follow common patterns for metadata tracking, validation flags, and schema loading, ensuring consistency across the package.

By following initialization best practices and understanding the parent-child relationships between objects, you can build robust data processing workflows that leverage the full capabilities of the public_health_resources package.
