# Confidence Interval Method Decision Tree

This document describes how `phr_pick_ci_method()` selects the confidence
interval (CI) method used by the survey calculation functions
(`phr_calc_survey_prop_single`, `phr_calc_survey_categorical_single`, and
`phr_calc_survey_ratio_single`).

---

## Proportion / Categorical variables

```
Is the variable a ratio (numerator / denominator)?
│
└── NO (proportion or categorical)
     │
     ├── Is the variable numeric (continuous)?
     │    └── YES → mean-wald
     │             (Wald CI around the survey-weighted mean)
     │
     └── NO (binary 0/1 or categorical)
          │
          ├── Is p̂ < 0.05 OR p̂ > 0.95?  (extreme / rare proportion)
          │    └── YES → wilson
          │             (Wilson Score Interval via effective sample size n_eff;
          │              avoids CI bounds outside [0, 1] near extreme proportions)
          │
          ├── Is n < 30  OR  n_eff < 10  OR  DEFF > 5  OR  high_design_complexity?
          │    └── YES → design-logit
          │             (logit-transformed CI via survey::svyciprop(method="logit");
          │              appropriate for small / complex survey designs)
          │
          └── DEFAULT → design-wald
                        (Wald CI via survey::svyciprop(method="mean");
                         used when sample is adequate and proportion is not extreme)
```

---

## Ratio variables

```
Is the variable a ratio (numerator / denominator)?
│
└── YES
     │
     ├── Is n_unweighted < 10?
     │    └── YES → design-meanbased
     │             (simple unweighted mean ratio; no CI produced)
     │
     ├── Is n < 30  OR  n_eff < 10  OR  DEFF > 5?
     │    └── YES → design-replicate
     │             (bootstrap replicate-weight variance via survey::as.svrepdesign;
     │              falls back to design-meanbased if replicate design cannot be created)
     │
     └── DEFAULT → design-taylor
                   (Taylor-linearised variance via survey::svyratio;
                    falls back to design-meanbased if variance estimation fails)
```

---

## Lower CI floor

Regardless of which method is selected, **`lower_ci` is floored at 0** after
computation. A negative lower bound is statistically inadmissible for
proportions and for ratios representing counts or non-negative quantities.

---

## Summary table

| Method            | Applies to    | When selected                                          |
|-------------------|---------------|--------------------------------------------------------|
| `wilson`          | proportion    | p̂ < 0.05 or p̂ > 0.95                                 |
| `design-logit`    | proportion    | n < 30, n_eff < 10, or DEFF > 5 (non-extreme p̂)      |
| `design-wald`     | proportion    | default (adequate sample, non-extreme p̂)              |
| `mean-wald`       | numeric mean  | always (numeric continuous variables)                  |
| `design-taylor`   | ratio         | default                                                |
| `design-replicate`| ratio         | small / complex design (n < 30, n_eff < 10, DEFF > 5) |
| `design-meanbased`| ratio         | very small sample (n < 10) or other methods fail       |
