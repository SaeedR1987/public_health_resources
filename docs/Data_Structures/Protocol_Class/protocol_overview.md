# Protocol Class Overview

## Purpose

The `Protocol` class is a top-level R6 class that manages the **complete assessment design pipeline** for a public health survey.  It coordinates:

1. **Objective setting** — primary and secondary research objectives
2. **Sample size and strata** — per-stratum sample size parameters
3. **Sampling frame and sample drawing** — loading a frame and drawing a sample using multiple methods
4. **Tool management** — attaching `Tool` objects to the protocol
5. **Indicator selection** — recording which indicators will be collected

The `Protocol` class also performs self-validation and maintains an `issues` list that flags inconsistencies between its components (e.g., strata present in the sample table but missing from the sampling frame).

**Location**: `R/class_protocol.R`

## Initialisation

```r
protocol <- Protocol$new(
  assessment_title = "Emergency Nutrition and Food Security Survey",
  country_name     = "Somalia",
  month_year       = "April 2026"
)
```

### Fields set on initialisation

| Field | Type | Description |
|-------|------|-------------|
| `metadata` | list | Assessment title, country, date, version |
| `primary_objectives` | list | Primary research objectives |
| `secondary_objectives` | list | Secondary research objectives |
| `sample_table` | data frame | One row per stratum with sample size parameters |
| `sampling_frame` | data frame | Population units for sampling |
| `drawn_sample` | list | Drawn sample with method and seed metadata |
| `tools` | list | `Tool` objects attached to this protocol |
| `selected_indicators` | list | Indicators selected for data collection |
| `issues` | list | Validation issues and discrepancies |

## Workflow

```
Protocol$new()
    ↓
set_primary_objectives() / set_secondary_objectives()
    ↓
add_stratum() (one call per stratum)
    ↓
set_sampling_frame()
    ↓
draw_sample()
    ↓
add_tools() (one call per tool type)
    ↓
select_indicators()
    ↓
export_protocol()
```

## Key Methods

### Setting objectives

Objectives must be provided as a list of named lists, each containing `objective_id`, `objective_text`, and `sector`:

```r
protocol$set_primary_objectives(list(
  list(
    objective_id   = "obj_1",
    objective_text = "Estimate GAM prevalence among children 6–59 months",
    sector         = "nutrition"
  ),
  list(
    objective_id   = "obj_2",
    objective_text = "Estimate CDR among the surveyed population",
    sector         = "mortality"
  )
))

protocol$set_secondary_objectives(list(
  list(
    objective_id   = "sec_1",
    objective_text = "Describe FCS distribution among households",
    sector         = "fsl"
  )
))
```

### Adding strata and calculating sample sizes

```r
protocol$add_stratum(
  stratum_id         = "region_a",
  stratum_name       = "Region A",
  population_size    = 50000,
  design_effect      = 1.5,
  precision          = 0.05,
  confidence_level   = 0.95,
  allocation_method  = "proportional"
)
```

Each call appends a row to `sample_table`.  The `sample_size` column is populated when `calculate_sample_sizes()` is called (or can be set directly).

### Setting the sampling frame

The sampling frame must contain at minimum the columns `id`, `stratum`, and `population_size`:

```r
protocol$set_sampling_frame(sampling_frame_df)
```

### Drawing a sample

```r
protocol$draw_sample(
  method       = "pps_cluster",  # "srs", "proportional", "pps_cluster", "rlc", "systematic"
  seed         = 42,
  cluster_size = 12
)
```

Supported methods:

| Method | Description |
|--------|-------------|
| `"srs"` | Simple random sampling |
| `"proportional"` | Proportional allocation |
| `"pps_cluster"` | Probability-proportional-to-size cluster sampling |
| `"rlc"` | Random Location Cluster (PPS, fixed cluster size of 3) |
| `"systematic"` | Systematic sampling |

### Adding tools

```r
# Household survey tool (loads bundled iphra_tool_v2.xlsx template)
protocol$add_tools(tool_type = "household")

# Key informant interview tool
protocol$add_tools(tool_type = "key_informant", tool_name = "Community KII")

# Observation checklist
protocol$add_tools(tool_type = "observation", tool_name = "Water Point Obs")
```

Valid `tool_type` values: `"household"`, `"key_informant"`, `"observation"`, `"generic"`.

Each call creates the appropriate `Tool` subclass object and appends it to `protocol$tools`.

### Reviewing issues

The `Protocol` class validates itself after every mutating method call:

```r
protocol$get_issues()
# Returns a named list of issue descriptions, e.g.:
# $strata_missing_in_frame
# [1] "Strata in sample table but not in frame: region_c"
```

### Summaries and export

```r
# Quick summary
protocol$get_protocol_summary()

# Full export as a list
exported <- protocol$export_protocol()
```

## Complete Example

```r
library(phr)

# 1. Create protocol
protocol <- Protocol$new(
  assessment_title = "Emergency Survey",
  country_name     = "Kenya",
  month_year       = "April 2026"
)

# 2. Set objectives
protocol$set_primary_objectives(list(
  list(objective_id = "o1", objective_text = "Estimate GAM", sector = "nutrition")
))

# 3. Add strata
protocol$add_stratum("nairobi", "Nairobi", population_size = 200000, design_effect = 1.5)
protocol$add_stratum("mombasa", "Mombasa", population_size = 80000,  design_effect = 1.2)

# 4. Set sampling frame and draw sample
protocol$set_sampling_frame(my_frame_df)
protocol$draw_sample(method = "pps_cluster", seed = 99, cluster_size = 12)

# 5. Add tools
protocol$add_tools("household")
protocol$add_tools("key_informant", tool_name = "Health Facility KII")

# 6. Check for issues
issues <- protocol$get_issues()
if (length(issues) == 0) message("No issues found.")

# 7. Export
protocol_data <- protocol$export_protocol()
```

## Key Design Principles

1. **Self-validating** — `check_issues()` runs automatically after every mutating method, populating `issues`
2. **Composable** — `Tool` objects are created and managed by the protocol, creating a clean ownership model
3. **Reproducible** — the drawn sample stores the random seed and method for full reproducibility
4. **Method chaining** — all mutating methods return `invisible(self)` to support chaining

## Related Documentation

- [Tool Classes Overview](../Tool_Classes/tool_classes_overview.md) — `Tool` objects managed by the protocol
- [Data Classes Overview](../Data_Classes/data_classes_overview.md) — Data classes for processing collected survey data
- [Analytics Classes Overview](../Analytics_Classes/analytics_classes_overview.md) — Analytics objects for post-collection analysis
