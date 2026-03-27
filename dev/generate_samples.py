#!/usr/bin/env python3
"""
Generate good and bad sample datasets for household schema testing
Based on household_schema_template.xlsx
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import random

random.seed(42)
np.random.seed(42)

n = 500

print(f"Generating {n} sample records...")

# ===== GOOD DATASET =====
print("\n1. Creating GOOD dataset...")

# Core required
uuid = [f"HH-{i:05d}" for i in range(1, n+1)]
hh_id = [f"HHID-{i:05d}" for i in range(1, n+1)]
consent = ["yes"] * n

# Dates
base_date = datetime(2024, 1, 1)
start = [(base_date + timedelta(days=i*0.7) + timedelta(days=random.randint(0, 10))).strftime("%Y-%m-%d") for i in range(n)]
end = [(datetime.strptime(start[i], "%Y-%m-%d") + timedelta(days=random.randint(1, 5))).strftime("%Y-%m-%d") for i in range(n)]

deviceid = [f"device-{random.randint(1000, 9999)}" for _ in range(n)]
enum_id = [f"ENUM-{random.randint(1, 50)}" for _ in range(n)]

# GPS - valid ranges
gps_lat = list(np.random.uniform(-90, 90, n))
gps_lon = list(np.random.uniform(-180, 180, n))
gps_precision = list(np.random.uniform(0, 10, n))

# Survey design
weight = list(np.random.uniform(0.5, 3.0, n))
stratum = [random.choice(["Urban", "Rural", "Camp"]) for _ in range(n)]
cluster_id = [f"CLU-{random.randint(1, 100)}" for _ in range(n)]
site = [f"Site{random.randint(1, 20)}" for _ in range(n)]

# Admin
admin1 = [f"Admin1_{random.choice('ABCDE')}" for _ in range(n)]
admin2 = [f"Admin2_{random.randint(1, 15)}" for _ in range(n)]

# Respondent
respondent_age = [random.randint(18, 80) for _ in range(n)]
respondent_sex = [random.choice(["m", "f"]) for _ in range(n)]
num_members = [random.randint(1, 15) for _ in range(n)]
hh_size = num_members.copy()

# HH status
hohh_status = [random.choice(["single", "married", "divorced", "widowed"]) for _ in range(n)]
residency_status = [random.choice(["host", "idp", "idp_returnee", "refugee"]) for _ in range(n)]

# Displacement - only for non-host
dod_idp_returnee = [random.choice(["yes", "no"]) if res != "host" else None for res in residency_status]
date_dod_idp_returnee = [(datetime.strptime(start[i], "%Y-%m-%d") - timedelta(days=random.randint(30, 365))).strftime("%Y-%m-%d") 
                          if dod_idp_returnee[i] == "yes" else None for i in range(n)]

# Priority needs - ensure different
need_options = ["drinking_water", "food", "shelter_materials", "healthcare", "wash_nfi"]
first_priority_need = [random.choice(need_options) for _ in range(n)]

second_priority_need = []
for i in range(n):
    opts = [x for x in need_options if x != first_priority_need[i]]
    second_priority_need.append(opts[0] if opts else None)

# Movement
left_yn = [random.choice(["yes", "no"]) for _ in range(n)]
num_left = [random.randint(0, 5) if left_yn[i] == "yes" else 0 for i in range(n)]

# FSL - simple valid ranges
fsl_fcs_cereal = [random.randint(0, 7) for _ in range(n)]
fsl_fcs_legumes = [random.randint(0, 7) for _ in range(n)]
fsl_fcs_score = [min(c*2 + l*3, 112) for c, l in zip(fsl_fcs_cereal, fsl_fcs_legumes)]
fsl_fcs_cat = ["Poor" if s <= 28 else "Borderline" if s <= 42 else "Acceptable" for s in fsl_fcs_score]

fsl_hhs_score = [random.randint(0, 6) for _ in range(n)]
fsl_hhs_cat = ["None" if s == 0 else "Moderate" if s <= 3 else "Severe" for s in fsl_hhs_score]

good_data = pd.DataFrame({
    'uuid': uuid,
    'enum_id': enum_id,
    'hh_id': hh_id,
    'gps_lat': gps_lat,
    'gps_lon': gps_lon,
    'gps_precision': gps_precision,
    'weight': weight,
    'stratum': stratum,
    'cluster_id': cluster_id,
    'site': site,
    'admin1': admin1,
    'admin2': admin2,
    'consent': consent,
    'respondent_age': respondent_age,
    'respondent_sex': respondent_sex,
    'num_members': num_members,
    'start': start,
    'end': end,
    'deviceid': deviceid,
    'hh_size': hh_size,
    'hohh_status': hohh_status,
    'residency_status': residency_status,
    'dod_idp_returnee': dod_idp_returnee,
    'date_dod_idp_returnee': date_dod_idp_returnee,
    'first_priority_need': first_priority_need,
    'second_priority_need': second_priority_need,
    'left_yn': left_yn,
    'num_left': num_left,
    'fsl_fcs_cereal': fsl_fcs_cereal,
    'fsl_fcs_legumes': fsl_fcs_legumes,
    'fsl_fcs_score': fsl_fcs_score,
    'fsl_fcs_cat': fsl_fcs_cat,
    'fsl_hhs_score': fsl_hhs_score,
    'fsl_hhs_cat': fsl_hhs_cat
})

print(f"   {len(good_data)} records, {len(good_data.columns)} columns")

# Save
good_data.to_csv('resources/household_data_good_sample.csv', index=False, na_rep='')
print("   Saved: resources/household_data_good_sample.csv")

# ===== BAD DATASET =====
print("\n2. Creating BAD dataset with violations...")

bad_data = good_data.copy()

# Add intentional errors
n_errors = 0

# 1. Missing required fields (10%)
idx = random.sample(range(n), min(50, n//10))
bad_data.loc[idx[:10], 'uuid'] = None
bad_data.loc[idx[10:20], 'consent'] = None
bad_data.loc[idx[20:30], 'start'] = None
bad_data.loc[idx[30:40], 'deviceid'] = None
bad_data.loc[idx[40:50], 'hh_id'] = None
n_errors += 50

# 2. Invalid GPS (5%)
idx = random.sample(range(n), min(25, n//20))
bad_data.loc[idx[:5], 'gps_lat'] = np.random.uniform(-200, -91, 5)
bad_data.loc[idx[5:10], 'gps_lat'] = np.random.uniform(91, 200, 5)
bad_data.loc[idx[10:15], 'gps_lon'] = np.random.uniform(-300, -181, 5)
bad_data.loc[idx[15:20], 'gps_lon'] = np.random.uniform(181, 300, 5)
bad_data.loc[idx[20:25], 'gps_precision'] = np.random.uniform(11, 50, 5)
n_errors += 25

# 3. Negative/zero weights (5%)
idx = random.sample(range(n), min(25, n//20))
bad_data.loc[idx[:10], 'weight'] = np.random.uniform(-5, -0.001, 10)
bad_data.loc[idx[10:15], 'weight'] = 0
bad_data.loc[idx[15:25], 'weight'] = None
n_errors += 25

# 4. Invalid age (5%)
idx = random.sample(range(n), min(20, n//25))
bad_data.loc[idx[:10], 'respondent_age'] = np.random.randint(1, 18, 10)
bad_data.loc[idx[10:20], 'respondent_age'] = np.random.randint(121, 150, 10)
n_errors += 20

# 5. End before start (5%)
idx = random.sample(range(n), min(25, n//20))
for i in idx:
    if pd.notna(bad_data.loc[i, 'start']):
        start_date = datetime.strptime(bad_data.loc[i, 'start'], "%Y-%m-%d")
        bad_data.loc[i, 'end'] = (start_date - timedelta(days=random.randint(1, 10))).strftime("%Y-%m-%d")
n_errors += 25

# 6. Invalid consent (3%)
idx = random.sample(range(n), min(15, n//33))
bad_data.loc[idx, 'consent'] = random.choice(["maybe", "unknown", "pending"])
n_errors += 15

# 7. Out of range numbers (8%)
idx = random.sample(range(n), min(40, n//12))
bad_data.loc[idx[:10], 'num_members'] = np.random.randint(31, 50, 10)
bad_data.loc[idx[10:20], 'hh_size'] = np.random.randint(31, 50, 10)
bad_data.loc[idx[20:30], 'fsl_fcs_score'] = np.random.randint(113, 200, 10)
bad_data.loc[idx[30:40], 'fsl_hhs_score'] = np.random.randint(7, 15, 10)
n_errors += 40

# 8. Invalid categories (5%)
idx = random.sample(range(n), min(20, n//25))
bad_data.loc[idx[:5], 'residency_status'] = "invalid_status"
bad_data.loc[idx[5:10], 'hohh_status'] = "unknown"
bad_data.loc[idx[10:15], 'fsl_fcs_cat'] = "Very Poor"
bad_data.loc[idx[15:20], 'respondent_sex'] = "other"
n_errors += 20

# 9. Duplicate priority needs (7%)
idx = random.sample(range(n), min(35, n//14))
for i in idx:
    bad_data.loc[i, 'second_priority_need'] = bad_data.loc[i, 'first_priority_need']
n_errors += 35

# 10. Dependency violations (8%)
idx = random.sample(range(n), min(40, n//12))
# dod_idp_returnee == 'yes' but residency == 'host'
for i in idx[:20]:
    bad_data.loc[i, 'residency_status'] = "host"
    bad_data.loc[i, 'dod_idp_returnee'] = "yes"
# Consent but underage
for i in idx[20:40]:
    bad_data.loc[i, 'consent'] = "yes"
    bad_data.loc[i, 'respondent_age'] = random.randint(10, 17)
n_errors += 40

# 11. Duplicate UUIDs (2%)
idx = random.sample(range(n), min(10, n//50))
for i in idx:
    bad_data.loc[i, 'uuid'] = bad_data.loc[random.randint(0, n-1), 'uuid']
n_errors += 10

print(f"   Introduced approximately {n_errors} violations across records")
print(f"   {len(bad_data)} records, {len(bad_data.columns)} columns")

# Save
bad_data.to_csv('resources/household_data_bad_sample.csv', index=False, na_rep='')
print("   Saved: resources/household_data_bad_sample.csv")

print("\n✓ Sample datasets generated successfully!")
print(f"\nColumns in datasets: {list(good_data.columns)}")
