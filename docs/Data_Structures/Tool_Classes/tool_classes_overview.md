# Tool Classes Overview

## Purpose

The `Tool` class hierarchy provides R6 classes for managing **XLSForm** data collection instruments used with Kobo Toolbox, ODK, and other mobile data collection platforms.

An XLSForm consists of up to three sheets:

| Sheet | Purpose |
|-------|---------|
| `survey` | Form structure — questions, groups, repeat groups |
| `choices` | Choice lists for select-one and select-multiple questions |
| `settings` | Form-level settings (title, language, version) |

Each `Tool` object maintains both a **master** (template) copy and a **modified** (filtered/customised) copy of the survey and choices sheets.

## Base Class: Tool

The `Tool` class provides the foundation for all tool types.  It provides:

- Loading and storing XLSForm data (master and modified sheets)
- Filtering the survey sheet by modules or indicators
- Filtering choices to match the modified survey
- Safely updating specific choice lists with new values
- Changing the default language in settings
- Validating the modified survey against available choices
- Validating structure according to the XLSForm specification

**Location**: `R/class_tool.R`

## Relationship to Protocol

`Tool` objects are used by the `Protocol` class to manage data collection instruments for an assessment:

```r
protocol <- Protocol$new(
  assessment_title = "Emergency Nutrition Survey",
  country_name = "Kenya",
  month_year = "March 2026"
)

# Add tools to the protocol
protocol$add_tools(tool_type = "household")
protocol$add_tools(tool_type = "key_informant", tool_name = "Health KII")
```

See [Protocol Overview](../Protocol_Class/protocol_overview.md) for details.

## Class Hierarchy

```
Tool (Base)
├── HouseholdTool
├── KeyInformantTool
└── ObservationTool
```

## Subclasses

### HouseholdTool

Tool for household survey XLSForms.

**Purpose**: Manage household-level surveys that include household-level questions, roster/repeat groups for household members, and sectoral question modules (FSL, WASH, Health, etc.).

**Default template**: Loads from the bundled `iphra_tool_v2.xlsx` template in `inst/resources/`.

**Location**: `R/class_household_tool.R`

```r
tool <- HouseholdTool$new(name = "My Household Survey")
```

---

### KeyInformantTool

Tool for Key Informant Interview (KII) XLSForms.

**Purpose**: Manage KII instruments used for community-level interviews, service provider interviews, market vendor interviews, and sector-specific expert interviews.

**Additional field**: `kii_type` — classification of the KII (e.g., `"community"`, `"health"`, `"fsl"`, `"wash"`, `"general"`).

**Default template**: Loads from the bundled `iphra_kii_tool_dummy.xlsx` template in `inst/resources/`.

**Location**: `R/class_key_informant_tool.R`

```r
tool <- KeyInformantTool$new(name = "Community KII", kii_type = "health")
```

---

### ObservationTool

Tool for observation/checklist XLSForms.

**Purpose**: Manage observation checklists used for water point assessments, latrine observations, health facility observations, market observations, and community observations.

**Additional field**: `observation_type` — classification of the observation (e.g., `"water_point"`, `"latrine"`, `"general"`).

**Default template**: Loads from the bundled `iphra_observation_tool_dummy.xlsx` template in `inst/resources/`.

**Location**: `R/class_observation_tool.R`

```r
tool <- ObservationTool$new(name = "Water Point Observation", observation_type = "water_point")
```

## Common Functionality

All `Tool` subclasses inherit the following from the base `Tool` class:

### Loading XLSForm data

```r
# Load from a file path
tool$load_xlsform("path/to/my_tool.xlsx")

# Or supply sheets at initialisation
tool <- HouseholdTool$new(
  survey  = my_survey_df,
  choices = my_choices_df
)
```

### Accessing sheets

```r
tool$master_survey    # Master (template) survey sheet
tool$master_choices   # Master (template) choices sheet
tool$survey           # Modified survey sheet
tool$choices          # Modified choices sheet
tool$settings         # Settings sheet
```

### Filtering by module or indicators

```r
# Keep only questions belonging to selected modules
tool$filter_survey_by_modules(c("fsl", "wash"))

# Sync choices to the filtered survey
tool$filter_choices_to_survey()
```

### Updating choice lists

```r
# Replace a specific choice list with new values
tool$update_choice_list(
  list_name = "admin1_list",
  new_choices = data.frame(
    list_name = "admin1_list",
    name      = c("nairobi", "mombasa"),
    label     = c("Nairobi", "Mombasa")
  )
)
```

### Changing language

```r
tool$set_default_language("French")
```

### Validation

```r
# Validate choices coverage
tool$validate_choices()

# Validate XLSForm structure
tool$validate_structure()
```

### Export

```r
tool$export_xlsform("output_tool.xlsx")
```

## Key Design Principles

1. **Master vs. modified**: The master copy is never overwritten; all modifications apply to a separate copy, allowing non-destructive filtering
2. **Default templates**: Each subclass ships with a bundled template, so tools can be created without any external file
3. **XLSForm compatible**: Outputs are valid XLSForm files that can be uploaded directly to Kobo Toolbox or ODK Central

## Related Documentation

- [Protocol Overview](../Protocol_Class/protocol_overview.md) — `Protocol` class that manages `Tool` objects
- [Data Classes Overview](../Data_Classes/data_classes_overview.md) — Data classes for processing collected survey data
